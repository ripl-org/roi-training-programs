library(data.table)
library(future)
library(future.apply)
library(Matrix)
library(tidyverse)

# Helper Functions --------------------------------------------------------

auc_roc <- function(preds, actuals, returnDT=FALSE){
  # Calculate area under the ROC curve
  # If returnDT = TRUE, a data.table is returned
  
  #--------------------------------------------------
  # Hack to pass 'no visible binding for global variable' notes from R CMD check
  
  Pred <- NULL
  Actual <- NULL
  CumulativeFPR <- NULL
  CountFalse <- NULL
  CumulativeTPR <- NULL
  CountTrue <- NULL
  AdditionalArea <- NULL
  CumulativeArea <- NULL
  
  #--------------------------------------------------
  
  # Check if every prediction is identical and if so, return 0.5
  if(length(unique(preds)) == 1L) return(0.5)
  
  # Convert actuals to numeric if it's an ordered factor
  if(is(actuals, "factor")){
    if(is.ordered(actuals) & length(levels(actuals)) == 2) actuals <- as.numeric(actuals) - 1 else stop("actuals is type factor, but is unordered. Make it an ordered factor.")
  }
  
  dt <- data.table(Pred=preds, Actual=actuals*1L)
  setorder(dt, -Pred)
  
  dt <- dt[ , {
    CountTrue = sum(Actual)
    list(CountFalse=.N - CountTrue, CountTrue=CountTrue)
  }, by=Pred]
  
  # Calculate the CumulativeFalsePositiveRate and CumulativeTruePositiveRate
  dt[, CumulativeFPR := cumsum(CountFalse)/sum(CountFalse)]
  dt[, CumulativeTPR := cumsum(CountTrue)/sum(CountTrue)]
  
  # Calculate AUC ROC
  dt[, AdditionalArea := c(head(CumulativeFPR, 1) * head(CumulativeTPR, 1)/2,
                           (tail(CumulativeFPR, -1) - head(CumulativeFPR, -1)) * (head(CumulativeTPR, -1) + (tail(CumulativeTPR, -1) - head(CumulativeTPR, -1))/2))]
  dt[, CumulativeArea := cumsum(AdditionalArea)]
  
  # Return the desired result
  if(returnDT) return(dt[]) else return(tail(dt$CumulativeArea, 1))
}

# Generic as.numeric function that works with factors
as_numeric <- function(var) {
  var <- as.numeric(as.character(var))
  return(var)
}

# Make assertion with error statement
assert <- function(condition,
                   message = NULL) {
  if(!condition) {
    if(is.null(message)) {
      stop(FALSE, call. = FALSE)
    } else {
      stop(message, call. = FALSE)
    }
  }
}

# Pull the lhs of a function (can be in character format)
formula_lhs <- function(form) {
  assert(is_formula(form),
         "Object doesn't appear to be a valid formula.")
  lhs <- trimws(form[2])
  return(lhs)
}

# Pull the rhs of a function including the '~'
formula_rhs <- function(form) {
  assert(is_formula(form),
         "Object doesn't appear to be a valid formula.")
  rhs <- paste("~", form[3])
  return(rhs)
}

# Check if a vector (or factor) is binary (or all logical)
is_binary <- function(var) {
  ux <- unique(var)
  if (length(ux) != 2) {
    return(FALSE)
  } else {
    if (!all(var %in% c(0, 1))) {
      message(
        "The input has only two unique values, but they are not in {0, 1}"
      )
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
}

# Check if object is sparse Matrix
is_sparseMatrix <- function(dat) {
  return(is(dat, "sparseMatrix"))
}

# Find modal value of a vector
mode <- function(x,
                 na.rm = TRUE,
                 all.modes = FALSE) {
  if (any(!is.na(x))) {
    if(na.rm == TRUE) {
      ux <- unique(x)[!is.na(unique(x))]
    } else {
      ux <- unique(x)
    }
  } else {
    ux <- NA
  }
  modes <- which(tabulate(match(x, ux)) == max(tabulate(match(x, ux))))
  if (all.modes == FALSE) {
    return(ux[modes[[1]]])
  } else {
    return(ux[modes])
  }
}

# Function that sets NA as factor reference level
na_ref <- function(var) {
  var <- factor(var,
                exclude = NULL,
                levels = c(NA,
                           unique(var)[!is.na(unique(var))]))
  var <- droplevels(var)
  return(var)
}

# Removes duplicates from dataframes or matrices
remove_duplicates <- function(dat, 
                              return.sparse = FALSE){
  is_df <- is.data.frame(dat)
  is_sM <- is_sparseMatrix(dat)
  if(!is_sM) dat <- as.matrix(dat)
  dup.cols <- as.vector(duplicated.matrix(t(dat)))
  dat <- dat[, !dup.cols]
  if(return.sparse == FALSE & !is_sM) {
    if(is_df) {
      return(as_tibble(dat))
    } else {
      return(dat)
    }
  } else {
    return(Matrix(dat, sparse = TRUE))
  }
}

# Shuffle vector, matrix, or dataframe
shuffle <- function(dat) {
  is_df  <- is.data.frame(dat)
  is_mat <- is.matrix(dat)
  if(is_df | is_mat) {
    return(dat[sample(1:nrow(dat)), ])
  } else {
    return(dat[sample(1:length(dat))])
  }
}

# Set factor with sorted ascending unique values as levels. Specify base level.
sort_factor <- function(var,
                        base.level = NULL) {
  if(!is.null(base.level)) {
    var <- factor(var,
                  levels = c(
                    base.level,
                    sort(unique(var))[which(sort(unique(var)) != base.level)]
                  ))
    return(var)
  } else {
    var <- factor(var,
                  levels = sort(unique(var)))
    return(var)
  }
}

# Check sparsity of each column of a matrix or dataframe
sparsity <- function(dat,
                     count.na.zero = FALSE) {
  sparsity <- apply(dat, 2, function(i) {
    nnzero(i, na.counted = !count.na.zero)/nrow(dat)
  }) %>%
    enframe() %>%
    rename(variable     = name,
           perc_nonzero = value)
  return(sparsity)
}

# Standardize a vector, dataframe, or matrix
# Doesn't standardize factors, character cols, or binary variables
standardize <- function(var) {
  # Function that scales any non-factor/character vector
  stdz <- function(x){
    if(!is.factor(x) & !is.character(x) & !is_binary(x)) {
      x <- as.vector(scale(x))
    }
    return(x)
  }
  # Checks for data.frame and matrix classes
  if(is.data.frame(var)) {
    var <- lapply(var, stdz) %>%
      bind_cols()
  } else if (is.matrix(var)) {
    var <- apply(var, 2, stdz)
  } else {
    var <- stdz(var)
  }
  return(var)
}

# Define function that creates one stratified bootstrap sample
strat_sample <- function(df, strat) {
  # Append a column of ids
  df <- df %>% mutate(id = 1:nrow(df)) %>% select(`strat`, id)
  # Get stratified bootstrap sample
  ids <- df %>% 
    group_by(get(strat)) %>%
    mutate(N = n()) %>%
    sample_n(N, replace = TRUE) %>%
    ungroup() %>%
    pull(id) %>%
    sort()
  return(ids)
}

# Define function that creates n stratified bootstrap samples
strat_sample_boot <- function(df,
                              strat,
                              nboot,
                              parallel = FALSE) {
  # Create a named vector for each bootstrap sample
  boot_list <- paste0("Sample", 1:nboot)
  # Create samples in parallel or not
  if(parallel == FALSE) {
    # Create n samples using strat_sample function
    samples <- lapply(boot_list, function(i) {
      return(strat_sample(df = df, strat = strat))
    })
  } else {
    # Initialize parallel processing
    plan(multiprocess)
    # Create n samples using strat_sample function
    samples <- future_lapply(boot_list, function(i) {
      return(strat_sample(df = df, strat = strat))
    })
  }
  # Return list of n bootstrap samples with names
  names(samples) <- boot_list
  return(samples)
}

# Create train, test, and validation split
train_test_validate <- function(y,
                                train.p,
                                test.p) {
  rand_idx <- shuffle(1:length(y))
  train_idx <- floor(train.p*length(y))
  test_idx <- floor(test.p*length(y)) + train_idx
  train <- rand_idx[1:train_idx]
  test <- rand_idx[(train_idx + 1):test_idx]
  validate <- rand_idx[(test_idx + 1):length(y)]
  assert(sum(c(train, test, validate) == rand_idx) == length(y))
  return(list(train = sort(train),
              test = sort(test),
              validate = sort(validate)))
}
