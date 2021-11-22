library(data.table)
library(future)
library(future.apply)
library(gamlr)
library(glmnet)
library(ggplot2)
library(janitor)
library(Matrix)
library(SparseM)
library(ranger)
library(recipes)
library(sandwich)
library(tidyverse)
library(viridis)

# Helper functions
source(here::here("source/analysis/dml/helper-functions.R"))

# Bare-bones functions for DML LASSO --------------------------------------

# Define function that runs k-fold validated lasso
lasso <- function(x,
                  y,
                  nfolds = 5,
                  family,
                  test_x = NULL,
                  free_idx,
                  stand = FALSE) {
  set.seed(202103)
  model <- cv.gamlr(
    x = x,
    y = y,
    nfold = nfolds,
    family = family,
    free = free_idx,
    standardize = stand
  ) # Specifies variables to keep
  # Predict values using lambda value that minimizes cv error
  if (!is.null(test_x)) {
    if (family == "binomial") {
      values <- predict(model, test_x, select = "1se", type = "response")
    } else {
      values <- predict(model, test_x, select = "1se")
    }
  }
  # Uncomment below to return both values and selected variables
  # Generate tibble for non-zero coefficients
  non_zero_coef <- tibble()
  # Get non-zero coefficienst
  c <- as.matrix(coef(model, select = "1se"))
  non_zero_coef <- bind_rows(
    non_zero_coef,
    tryCatch(
      tibble(
        selected_variables =
          names(c[which(c != 0), ]),
        selected_coefs =
          c[which(c != 0), ]
      ),
      error = function(e) {
        tibble(selected_variables = "None")
      }
    )
  )
  # Return predicted values
  out <- list(
    coefficients = non_zero_coef,
    model = model
  )
  if (!is.null(test_x)) {
    out <- append(out, list(values = as.vector(values)))
  }
  return(out)
}

# Random forest w/ grid-search possibility
# Grid search ranger
rf <- function(form,
               df,
               probability,
               predict_df = NULL,
               mtry = NULL,
               node_size = NULL,
               num_trees = NULL,
               error_type = "OOB",
               verbose = FALSE) {
  # Error checking
  assert(
    is.null(error_type) | error_type == "OOB",
    "Argument 'error_type' must be either NULL or 'OOB'"
  )
  # Split out data by train and test set
  x <- df %>% select(-!!formula_lhs(form))
  y <- df %>% pull(formula_lhs(form))
  if (!is.null(predict_df)) {
    predict_x <- predict_df %>% select(-!!formula_lhs(form))
    predict_y <- predict_df %>% pull(formula_lhs(form))
  }
  # Run models across tuning grid if specified
  # If not, run model with default values
  if (!is.null(mtry) | !is.null(node_size) | !is.null(num_trees)) {
    if (is.null(mtry)) {
      mtry <- floor(sqrt(ncol(x)))
    }
    if (is.null(node_size)) {
      node_size <- ifelse(probability == TRUE, 1, 5)
    }
    if (is.null(num_trees)) {
      num_trees <- 1000
    }
    # Define grid of hyper parameters
    grid <- t(expand.grid(
      mtry = mtry,
      node_size = node_size,
      num_trees = num_trees
    )) %>%
      as.data.frame()
    # Run a random forest for every row in grid
    set.seed(202103)
    models <- future_lapply(
      grid,
      function(i) {
        model <- ranger(y ~ .,
                        data = x,
                        probability = probability,
                        mtry = i[1],
                        min.node.size = i[2],
                        num.trees = i[3],
                        verbose = FALSE
        )
        error <- model$prediction.error
        return(error)
      }
    )
    # Append the respective OOB rate to each row of the grid
    grid_errors <- as_tibble(t(grid)) %>%
      bind_cols(error = unlist(models)) %>%
      arrange(error)
    # Get the hyper parameters that generated minimum OOB error
    hyper_params <- as_tibble(t(grid))[which.min(unlist(models)), ]
    # Run model with hyper params defined above
    final_model <- ranger(y ~ .,
                          data = x,
                          probability = probability,
                          mtry = hyper_params$mtry,
                          min.node.size = hyper_params$node_size,
                          num.trees = hyper_params$num_trees
    )
  } else {
    if (verbose == TRUE) {
      cat(
        "At least one of mtry, node_size, and num_trees are null,",
        "so using default ranger values\n"
      )
    }
    # If hyper parameters aren't defined, run model with default values
    final_model <- ranger(y ~ .,
                          data = x,
                          probability = probability
    )
  }
  # If a dataframe is provided, generate predictions
  if (!is.null(predict_df)) {
    values <- if (is.factor(y)) {
      predict(final_model, predict_x)$predictions[, 2]
    } else {
      predict(final_model, predict_x)$predictions
    }
  }
  # List of outs
  out <- list(model = final_model)
  if (!is.null(predict_df)) out <- append(out, list(values = as.vector(values)))
  if (!is.null(mtry) & !is.null(node_size) & !is.null(num_trees)) {
    out <- append(
      out,
      list(
        grid = grid_errors,
        which_min = hyper_params
      )
    )
  }
  return(out)
}

dml_first_stage_lasso <- function(x,
                                  y,
                                  d,
                                  y_family,
                                  d_family,
                                  nfold = 5,
                                  formula,
                                  free_vars,
                                  verbose = TRUE) {
  # Create model matrix. Removes constant columns and duplicates.
  x <- sparse.model.matrix(formula, data = x)
  x <- x[, unique(summary(x)$j)][, -1]
  if (verbose) {
    cat(
      "\rDesign matrix has",
      dim(x)[1],
      "rows and",
      dim(x)[2],
      "columns\n"
    )
  }
  # These following lines are stolen from Victor C.
  nobs <- nrow(x)
  foldid <- rep.int(1:nfold, times = ceiling(nobs / nfold))[sample.int(nobs)]
  I <- split(1:nobs, foldid)
  ytil <- ypreds <- dtil <- dpreds <- rep(NA, nobs)
  thetas <- list()
  # Identify which columns in the matrix should never be penalized
  free_idx <- if (is.null(free_vars)) {
    NULL
  } else {
    grep(paste(free_vars, collapse = "|"), colnames(x))
  }
  if (verbose == TRUE) {
    if (!is.null(free_vars)) {
      cat(
        "\rRunning model without penalizing the following variables:\n",
        colnames(x)[free_idx],
        "\n"
      )
    }
  }
  # Initialize dataframes for returning values AND selected predictors
  y_predictors <- d_predictors <- tibble()
  # The following loop builds residuals over the folds
  for (b in 1:length(I)) {
    # Following four lines subsets each component of data
    x_sub <- x[-I[[b]], ]
    y_sub <- y[-I[[b]]]
    d_sub <- d[-I[[b]]]
    test_x <- x[I[[b]], ] # Held-out Xs to predict values
    # Residualize
    y_model <- lasso(
      x = x_sub,
      y = y_sub,
      family = y_family,
      test_x = test_x,
      free_idx = free_idx,
      stand = TRUE
    )
    d_model <- lasso(
      x = x_sub,
      y = d_sub,
      family = d_family,
      test_x = test_x,
      free_idx = free_idx,
      stand = TRUE
    )
    # Return predicted values
    yhat <- y_model$values
    dhat <- d_model$values
    # Return selected LASSO variables
    d_predictors <- bind_rows(d_predictors, d_model$coefficients)
    y_predictors <- bind_rows(y_predictors, y_model$coefficients)
    # Get predicted values
    ypreds[I[[b]]] <- yhat
    dpreds[I[[b]]] <- dhat
    # Calculate Residuals
    ytil[I[[b]]] <- (as_numeric(y[I[[b]]]) - yhat)
    dtil[I[[b]]] <- (as_numeric(d[I[[b]]]) - dhat)
    # Calculate theta for DML1 calculation
    theta <- list(
      theta = coef(summary(lm(ytil[I[[b]]] ~ dtil[I[[b]]])))[, 1][2],
      se = coef(summary(lm(ytil[I[[b]]] ~ dtil[I[[b]]])))[, 2][2],
      ytil = ytil[I[[b]]],
      dtil = dtil[I[[b]]]
    )
    thetas <- append(thetas, list("theta" = theta))
    if (verbose == TRUE) {
      cat("\r--- Finished fold", b, "---\n")
    }
  }
  # List of error-checking
  assert(sum(is.na(ytil)) == 0)
  assert(sum(is.na(dtil)) == 0)
  assert(sum(is.na(ypreds)) == 0)
  assert(sum(is.na(dpreds)) == 0)
  assert(sum(is.na(thetas)) == 0, message = "Vector 'thetas' has NA values")
  assert(length(ytil) == nobs)
  # Calculate precision metrics
  outcomes <- tibble::tibble(
    y_truth = y,
    y_pred = ypreds,
    d_truth = d,
    `0` = 1 - dpreds,
    d_pred = dpreds,
    y_resid = ytil,
    d_resid = dtil
  )
  rsquared <- yardstick::rsq(
    data = outcomes,
    y_truth,
    y_pred
  )$`.estimate`
  RMSE <- yardstick::rmse(
    data = outcomes,
    y_truth,
    y_pred
  )$`.estimate`
  auc <- auc_roc(preds = outcomes$d_pred,
                 actuals = as_numeric(outcomes$d_truth))
  # Plot histogram of propensity scores
  prop_scores_hist <- outcomes %>%
    rename("Treatment" = d_truth) %>%
    ggplot(aes(x = d_pred, fill = Treatment)) +
    geom_histogram(bins = 100) +
    facet_wrap(~Treatment, nrow = 2) +
    labs(
      x = "Propensity Scores",
      y = "Frequency",
      title = "Histogram of Propensity Scores"
    ) +
    theme(
      panel.background = element_blank(),
      panel.grid.major.x = element_line(color = "gray95"),
      legend.key = element_rect(fill = "white"),
      axis.text = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      plot.title = element_text(
        hjust = 0.5,
        face = "bold"
      )
    )
  # Plot Actual y values vs Predicted y values
  actual_vs_pred <- outcomes %>%
    rename("Treatment" = d_truth) %>%
    ggplot(aes(
      x = y_truth,
      y = y_pred,
      color = Treatment
    )) +
    geom_point(alpha = 0.5) +
    facet_wrap(~Treatment, nrow = 2) +
    labs(
      x = "Y Truth",
      y = "Y Predicted",
      title = "Actual Y values vs Predicted Y values"
    ) +
    geom_abline(
      slope = 1,
      intercept = 0
    ) +
    theme(
      panel.background = element_blank(),
      panel.grid.major.x = element_line(color = "gray95"),
      legend.key = element_rect(fill = "white"),
      axis.text = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      plot.title = element_text(
        hjust = 0.5,
        face = "bold"
      )
    )
  # Plot histogram of Outcome residuals
  outcome_resid_hist <- outcomes %>%
    rename("Treatment" = d_truth) %>%
    ggplot(aes(x = y_resid, fill = Treatment)) +
    geom_histogram(bins = 100) +
    facet_wrap(~Treatment, nrow = 2) +
    labs(
      x = "Outcome Residuals",
      y = "Frequency",
      title = "Histogram of Outcome Residuals"
    ) +
    theme(
      panel.background = element_blank(),
      panel.grid.major.x = element_line(color = "gray95"),
      legend.key = element_rect(fill = "white"),
      axis.text = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      plot.title = element_text(
        hjust = 0.5,
        face = "bold"
      )
    )
  # Plot histogram of Treatment residuals
  treat_resid_hist <- outcomes %>%
    rename("Treatment" = d_truth) %>%
    ggplot(aes(x = d_resid, fill = Treatment)) +
    geom_histogram(bins = 100) +
    facet_wrap(~Treatment, nrow = 2) +
    labs(
      x = "Treatment Residuals",
      y = "Frequency",
      title = "Histogram of Treatment Residuals"
    ) +
    theme(
      panel.background = element_blank(),
      panel.grid.major.x = element_line(color = "gray95"),
      legend.key = element_rect(fill = "white"),
      axis.text = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      plot.title = element_text(
        hjust = 0.5,
        face = "bold"
      )
    )
  # Return residuals and predicted values
  out <- list(
    y_predicted_values = ypreds,
    d_predicted_values = dpreds,
    y_residuals = ytil,
    d_residuals = dtil,
    y_actual = outcomes$y_truth,
    d_actual = outcomes$d_truth,
    R2 = rsquared,
    RMSE = RMSE,
    AUC = auc,
    d_propensity_hist = prop_scores_hist,
    y_actual_vs_pred = actual_vs_pred,
    y_resid_hist = outcome_resid_hist,
    d_resid_hist = treat_resid_hist,
    y_selected_vars = suppressMessages(
      y_predictors %>%
        group_by(selected_variables) %>%
        summarise(
          n = n(),
          coef = mean(selected_coefs)
        ) %>%
        ungroup()
    ),
    d_selected_vars = suppressMessages(
      d_predictors %>%
        group_by(selected_variables) %>%
        summarise(
          n = n(),
          coef = mean(selected_coefs)
        ) %>%
        ungroup()
    ),
    theta_estimates = thetas
  )
  return(out)
}

# Random forest first
dml_first_stage_rf <- function(x,
                               y,
                               d,
                               verbose = TRUE,
                               nfold = 5,
                               mtry,
                               num_trees,
                               node_size,
                               error_type = "OOB") {
  x <- bind_cols(x, y = y, d = d)
  # These following lines are stolen from Victor C.
  nobs <- nrow(x)
  foldid <- rep.int(1:nfold, times = ceiling(nobs / nfold))[sample.int(nobs)]
  I <- split(1:nobs, foldid)
  ytil <- ypreds <- dtil <- dpreds <- rep(NA, nobs)
  thetas <- list()
  yprob <- ifelse(is.factor(y), TRUE, FALSE)
  dprob <- ifelse(is.factor(d), TRUE, FALSE)
  # The following loop builds residuals over the folds
  for (b in 1:length(I)) {
    # Following four lines subsets each component of data
    x_sub <- x[-I[[b]], ]
    test_x <- x[I[[b]], ] # Held-out Xs to predict values
    # Residualize
    y_model <- suppressMessages(
      rf(
        form = y ~ .,
        df = x_sub %>% select(-d),
        probability = yprob,
        mtry = mtry,
        num_trees = num_trees,
        node_size = node_size,
        predict_df = test_x %>% select(-d),
        error_type = error_type,
        verbose = FALSE
      )
    )
    d_model <- suppressMessages(
      rf(
        form = d ~ .,
        df = x_sub %>% select(-y),
        probability = dprob,
        mtry = mtry,
        num_trees = num_trees,
        node_size = node_size,
        predict_df = test_x %>% select(-y),
        error_type = error_type,
        verbose = FALSE
      )
    )
    # Return predicted values
    yhat <- y_model$values
    dhat <- d_model$values
    # Get predicted values
    ypreds[I[[b]]] <- yhat
    dpreds[I[[b]]] <- dhat
    # Calculate Residuals
    ytil[I[[b]]] <- (as_numeric(test_x$y) - yhat)
    dtil[I[[b]]] <- (as_numeric(test_x$d) - dhat)
    # Calculate theta for DML1 calculation
    theta <- list(
      theta = coef(summary(lm(ytil[I[[b]]] ~ dtil[I[[b]]])))[, 1][2],
      se = coef(summary(lm(ytil[I[[b]]] ~ dtil[I[[b]]])))[, 2][2],
      ytil = ytil[I[[b]]],
      dtil = dtil[I[[b]]]
    )
    thetas <- append(thetas, list("theta" = theta))
    if (verbose == TRUE) {
      cat("--- Finished fold", b, "---\n")
    }
  }
  # List of error-checking
  assert(sum(is.na(ytil)) == 0)
  assert(sum(is.na(dtil)) == 0)
  assert(sum(is.na(ypreds)) == 0)
  assert(sum(is.na(dpreds)) == 0)
  assert(sum(is.na(thetas)) == 0, message = "Vector 'thetas' has NA values")
  assert(length(ytil) == nobs)
  # Calculate precision metrics
  outcomes <- tibble::tibble(
    y_truth = y,
    y_pred = ypreds,
    d_truth = d,
    `0` = 1 - dpreds,
    d_pred = dpreds,
    y_resid = ytil,
    d_resid = dtil
  )
  rsquared <- yardstick::rsq(
    data = outcomes,
    y_truth,
    y_pred
  )$`.estimate`
  RMSE <- yardstick::rmse(
    data = outcomes,
    y_truth,
    y_pred
  )$`.estimate`
  auc <- auc_roc(preds = outcomes$d_pred,
                 actuals = as_numeric(outcomes$d_truth))
  # Plot histogram of propensity scores
  prop_scores_hist <- outcomes %>%
    rename("Treatment" = d_truth) %>%
    ggplot(aes(x = d_pred, fill = Treatment)) +
    geom_histogram(bins = 100) +
    facet_wrap(~Treatment, nrow = 2) +
    labs(
      x = "Propensity Scores",
      y = "Frequency",
      title = "Histogram of Propensity Scores"
    ) +
    theme(
      panel.background = element_blank(),
      panel.grid.major.x = element_line(color = "gray95"),
      legend.key = element_rect(fill = "white"),
      axis.text = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      plot.title = element_text(
        hjust = 0.5,
        face = "bold"
      )
    )
  # Plot Actual y values vs Predicted y values
  actual_vs_pred <- outcomes %>%
    rename("Treatment" = d_truth) %>%
    ggplot(aes(
      x = y_truth,
      y = y_pred,
      color = Treatment
    )) +
    geom_point(alpha = 0.5) +
    facet_wrap(~Treatment, nrow = 2) +
    labs(
      x = "Y Truth",
      y = "Y Predicted",
      title = "Actual Y values vs Predicted Y values"
    ) +
    geom_abline(
      slope = 1,
      intercept = 0
    ) +
    theme(
      panel.background = element_blank(),
      panel.grid.major.x = element_line(color = "gray95"),
      legend.key = element_rect(fill = "white"),
      axis.text = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      plot.title = element_text(
        hjust = 0.5,
        face = "bold"
      )
    )
  # Plot histogram of Outcome residuals
  outcome_resid_hist <- outcomes %>%
    rename("Treatment" = d_truth) %>%
    ggplot(aes(x = y_resid, fill = Treatment)) +
    geom_histogram(bins = 100) +
    facet_wrap(~Treatment, nrow = 2) +
    labs(
      x = "Outcome Residuals",
      y = "Frequency",
      title = "Histogram of Outcome Residuals"
    ) +
    theme(
      panel.background = element_blank(),
      panel.grid.major.x = element_line(color = "gray95"),
      legend.key = element_rect(fill = "white"),
      axis.text = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      plot.title = element_text(
        hjust = 0.5,
        face = "bold"
      )
    )
  # Plot histogram of Treatment residuals
  treat_resid_hist <- outcomes %>%
    rename("Treatment" = d_truth) %>%
    ggplot(aes(x = d_resid, fill = Treatment)) +
    geom_histogram(bins = 100) +
    facet_wrap(~Treatment, nrow = 2) +
    labs(
      x = "Treatment Residuals",
      y = "Frequency",
      title = "Histogram of Treatment Residuals"
    ) +
    theme(
      panel.background = element_blank(),
      panel.grid.major.x = element_line(color = "gray95"),
      legend.key = element_rect(fill = "white"),
      axis.text = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      plot.title = element_text(
        hjust = 0.5,
        face = "bold"
      )
    )
  # Return residuals and predicted values
  out <- out <- list(
    y_predicted_values = ypreds,
    d_predicted_values = dpreds,
    y_residuals = ytil,
    d_residuals = dtil,
    y_actual = outcomes$y_truth,
    d_actual = outcomes$d_truth,
    R2 = rsquared,
    RMSE = RMSE,
    AUC = auc,
    d_propensity_hist = prop_scores_hist,
    y_actual_vs_pred = actual_vs_pred,
    y_resid_hist = outcome_resid_hist,
    d_resid_hist = treat_resid_hist,
    theta_estimates = thetas
  )
  return(out)
}

# Estimates ATE and SE by regressing outcome residuals on treatment residuals
dml_second_stage <- function(y_resid,
                             d_resid) {
  final_lm <- lm(y_resid ~ d_resid)
  coef.est <- coef(final_lm)[2] # Extract coefficient
  se <- sqrt(vcovHC(final_lm)[2, 2]) # Record standard error
  cat(sprintf("\ncoef (se) = %g (%g)\n", coef.est, se))
  out <- list(
    coefficient = as.vector(coef.est),
    se = as.vector(se)
  )
  return(out)
}

# This function facilitates running second-stage DML with LASSO
dml_second_stage_lasso <- function(formula,
                                   df,
                                   nfolds,
                                   lambda = "1se",
                                   alpha = .05,
                                   standardize = TRUE,
                                   post.lasso = FALSE) {
  # Get sparse matrix
  x <- sparse.model.matrix(formula, data = df)
  x <- x[, unique(summary(x)$j)][, -1]
  # Print design matrix info
  cat(
    "\rDesign matrix has",
    dim(x)[1],
    "rows and",
    dim(x)[2],
    "columns\n"
  )
  # Run lasso
  set.seed(202103)
  final_lasso <- cv.gamlr(
    x = x,
    y = df %>%
      pull(formula_lhs(formula)),
    nfold = nfolds,
    standardize = standardize
  )
  # Get non-zero coefficienst
  c <- as.matrix(coef(final_lasso, select = lambda))
  selected <- setNames(
    c %>%
      as.data.frame() %>%
      tibble::rownames_to_column() %>%
      as_tibble(),
    c("variables", "coef")
  ) %>%
    filter(coef != 0)
  if (post.lasso) {
    x_lm <- x[, selected$variables[-1]] %>%
      as.matrix() %>%
      as_tibble()
    ols <- tryCatch(
      estimatr::lm_robust(df %>% pull(formula_lhs(formula)) ~ ., data = x_lm),
      error = function(e) {
        cat("\rPost-LASSO estimator threw an error :(\t\t\t\n")
        return(NULL)
      }
    )
    if (is.null(ols)) {
      return(list(
        "lasso_selected" = selected,
        "lasso_model" = final_lasso
      ))
    }
    ols_out <- broom::tidy(ols, conf.int = TRUE, conf.level = 1 - alpha)
  }
  if (post.lasso) {
    return(list(
      "lasso_selected" = selected,
      "lasso_model" = final_lasso,
      "ols_model" = ols,
      "ols_coefficients" = ols_out
    ))
  } else {
    return(list(
      "lasso_selected" = selected,
      "lasso_model" = final_lasso
    ))
  }
}

# This function facilitates running second-stage DML with Ridge
dml_second_stage_ridge <- function(formula,
                                   df,
                                   nfolds,
                                   lambda = "lambda.1se",
                                   alpha = .05,
                                   standardize = TRUE) {
  # Get sparse matrix
  x <- sparse.model.matrix(formula, data = df)
  x <- x[, unique(summary(x)$j)][, -1]
  # Design matrix info
  cat(
    "\rDesign matrix has",
    dim(x)[1],
    "rows and",
    dim(x)[2],
    "columns\n"
  )
  # Run ridge
  set.seed(202103)
  final_ridge <- cv.glmnet(
    x = x,
    y = df %>%
      pull(formula_lhs(formula)),
    nfolds = nfolds,
    alpha = 0,
    standardize = standardize
  )
  # Get coefficients
  c <- as.matrix(coef(final_ridge, s = lambda))
  selected <- setNames(
    c %>%
      as.data.frame() %>%
      tibble::rownames_to_column() %>%
      as_tibble(),
    c("variables", "coef")
  )
  return(list(
    "ridge_selected" = selected,
    "ridge_model" = final_ridge
  ))
}

# Runs bootstrapped hte_ridge to get coefficients and conf.
# intervals for them.
dml_second_stage_ridge_boot <- function(formula,
                                        df,
                                        nfolds,
                                        lambda = "lambda.min",
                                        alpha = .05,
                                        nboot = 1000,
                                        standardize = TRUE,
                                        strata) {
  # Get sparse matrix
  x <- sparse.model.matrix(formula, data = df)
  x <- x[, unique(summary(x)$j)][, -1]
  # Design matrix info
  cat(
    "\rDesign matrix has",
    dim(x)[1],
    "rows and",
    dim(x)[2],
    "columns\n"
  )
  y <- df %>% pull(formula_lhs(formula))
  # Define function that runs ridge on bootstrap sample
  set.seed(202103)
  glmnet_call <- function(idx) {
    # idx <- sort(sample(1:nrow(x), replace = TRUE))
    boot_mod <- cv.glmnet(
      x = x[idx, ],
      y = y[idx],
      alpha = 0,
      nfolds = 5,
      standardize = standardize
    )
    c <- as.matrix(coef(boot_mod, s = lambda))
    selected <- setNames(
      c %>%
        as.data.frame() %>%
        tibble::rownames_to_column() %>%
        as_tibble(),
      c("variables", "coef")
    )
    return(selected)
  }
  # Get Coefficients
  initial_model <- glmnet_call(1:nrow(x))
  # Run ridge regression across each bootstrap sample
  boot_models <- future_lapply(1:nboot, function(i) {
    idx <- sample(1:nrow(x), nrow(x), replace = TRUE)
    glmnet_call(idx)
  }, future.seed = TRUE)
  # Merge all n dataframes of ridge estimates
  # Every row is n estimates of a variable's coefficient
  boot_results <- boot_models %>%
    reduce(left_join, by = "variables") %>%
    mutate(across(-variables,
                  ~ .x - initial_model$coef))
  # Convert results into a matrix and calculate row quantiles
  # based on the user-specified value of alpha. Creates UB and LB
  LB <- alpha / 2
  UB <- 1 - LB
  row_quantiles <- boot_results %>%
    select(-variables) %>%
    as.matrix() %>%
    apply(1,
          quantile,
          c(1 - LB, 1 - UB),
          na.rm = TRUE
    ) %>%
    t() %>%
    as_tibble() %>%
    mutate(across(everything(),
                  ~ initial_model$coef - .x)) %>%
    `names<-`(paste0(100*c(LB, UB), "%"))
  # Combine Bootstrapped confidence intervals with the estimated coefficients
  full_results <- bind_cols(
    row_quantiles,
    coef = initial_model$coef,
    "variables" = boot_results$variables
  ) %>%
    `[`(, c(4, 1, 3, 2))
  return(full_results)
}