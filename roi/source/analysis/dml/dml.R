# Packages ----------------------------------------------------------------

library(dplyr)
library(fst)
library(readr)

# Load Helper and DML Functions -------------------------------------------

source(here::here("source/analysis/dml/helper-functions.R"))
source(here::here("source/analysis/dml/dml-functions.R"))

# Set seed ----------------------------------------------------------------

set.seed(123)

# Load Data ---------------------------------------------------------------

rjri_analysis <- read_fst(here::here("scratch/derived/rjri-analysis-panel.fst")) %>%
  lapply(as.character) %>%
  bind_cols() %>%
  as_tibble()

# Treatment obs
treat <- rjri_analysis %>%
  filter(treatment == "Treatment")

# Down sample control group to 20,000
ctrl <- rjri_analysis %>%
  filter(treatment == "Control") %>%
  shuffle() %>%
  head(20000)

# Combine treatment and control
rjri_analysis <- bind_rows(treat,
                      ctrl) %>%
  shuffle() %>%
  select(sirad_id,
         training_program,
         partnership,
         treatment,
         yyq,
         wages_prev4:wages_next4,
         contains("ern_prev"),
         ern,
         contains("naics_prev"),
         naics,
         contains("ui_flag_prev"),
         ui_flag,
         everything()) %>%
  mutate(across(contains("wages"), as.numeric),
         across(contains("wages"), ~ replace(.x, is.na(.x), 0)),
         missing_wages = case_when(wages == 0 ~ 1,
                                   TRUE ~ 0),
         missing_wages_prev1 = case_when(wages_prev1 == 0 ~ 1,
                                         TRUE ~ 0),
         missing_wages_prev2 = case_when(wages_prev2 == 0 ~ 1,
                                         TRUE ~ 0),
         missing_wages_prev3 = case_when(wages_prev3 == 0 ~ 1,
                                         TRUE ~ 0),
         missing_wages_prev4 = case_when(wages_prev4 == 0 ~ 1,
                                         TRUE ~ 0),
         across(contains("naics"), na_ref),
         across(contains("ern"), na_ref),
         across(contains("ui_flag"), na_ref),
         across(contains("missing_wages"), as.factor),
         yyq = as.factor(yyq),
         partnership = na_ref(partnership),
         training_program = na_ref(training_program),
         outcome_wages = (wages_next1 + wages_next2 + wages_next3 + wages_next4)/4,
         treatment = as.factor(case_when(treatment == "Control" ~ 0,
                                         TRUE ~ 1)))

# Pull off demographics
rjri_analysis_dems <- rjri_analysis %>%
  select(sirad_id,
         training_program,
         partnership,
         gender:education,
         wages_prev4:wages_next4)

# Remove demographics from panel
rjri_analysis <- rjri_analysis %>%
  select(-c(gender:education),
         -contains("_next"))

# Estimate RJRI Training Program returns ----------------------------------

rjri_first_stage <- dml_first_stage_rf(x = rjri_analysis %>% select(-c(sirad_id,
                                                                       treatment,
                                                                       outcome_wages,
                                                                       training_program,
                                                                       partnership)),
                                       y = rjri_analysis$outcome_wages,
                                       d = rjri_analysis$treatment,
                                       mtry = NULL,
                                       num_trees = NULL,
                                       node_size = NULL,
                                       error_type = "OOB",
                                       verbose = TRUE,
                                       nfold = 2)
# Simple ATE
rjri_overall_ate <- broom::tidy(lm(
  y_residuals ~ d_residuals,
  data = bind_cols("y_residuals" = rjri_first_stage$y_residuals,
                   "d_residuals" = rjri_first_stage$d_residuals)
))

# Second stage with bootstrapped ridge

# HTE for programs
rjri_second_stage_panel <- rjri_analysis %>%
  select(training_program,
         partnership) %>%
  bind_cols("y_residuals" = rjri_first_stage$y_residuals,
            "d_residuals" = rjri_first_stage$d_residuals)

plan(multisession, workers = availableCores() - 1)

rjri_second_stage_ridge_activity <- dml_second_stage_ridge_boot(
  formula = y_residuals ~ d_residuals + d_residuals:training_program,
  df = rjri_second_stage_panel, 
  nfolds = 5, 
  nboot = 1000, 
  standardize = FALSE
)

rjri_second_stage_ridge_partnership <- dml_second_stage_ridge_boot(
  formula = y_residuals ~ d_residuals + d_residuals:partnership,
  df = rjri_second_stage_panel, 
  nfolds = 5, 
  nboot = 1000, 
  standardize = FALSE
)

# Estimate WIOA Training Program returns ----------------------------------

wioa_analysis <- read_fst(here::here("scratch/derived/wioa-wp-analysis-panel.fst")) %>%
  lapply(as.character) %>%
  bind_cols() %>%
  as_tibble() %>%
  shuffle() %>%
  select(sirad_id,
         treatment,
         yyq,
         wages_prev4:wages_next4,
         contains("ern_prev"),
         ern,
         contains("naics_prev"),
         naics,
         everything()) %>%
  mutate(across(contains("wages"), as.numeric),
         across(contains("wages"), ~ replace(.x, is.na(.x), 0)),
         missing_wages = case_when(wages == 0 ~ 1,
                                   TRUE ~ 0),
         missing_wages_prev1 = case_when(wages_prev1 == 0 ~ 1,
                                         TRUE ~ 0),
         missing_wages_prev2 = case_when(wages_prev2 == 0 ~ 1,
                                         TRUE ~ 0),
         missing_wages_prev3 = case_when(wages_prev3 == 0 ~ 1,
                                         TRUE ~ 0),
         missing_wages_prev4 = case_when(wages_prev4 == 0 ~ 1,
                                         TRUE ~ 0),
         across(contains("naics"), na_ref),
         across(contains("ern"), na_ref),
         across(contains("missing_wages"), as.factor),
         across(gender:education, na_ref),
         yyq = as.factor(yyq),
         outcome_wages = (wages_next1 + wages_next2 + wages_next3 + wages_next4)/4,
         treatment = as.factor(case_when(treatment == "Control" ~ 0,
                                         TRUE ~ 1))) %>%
  select(-contains("_next"))

wioa_first_stage <- dml_first_stage_rf(x = wioa_analysis %>% select(-c(sirad_id,
                                                                       treatment,
                                                                       outcome_wages)),
                                       y = wioa_analysis$outcome_wages,
                                       d = wioa_analysis$treatment,
                                       mtry = NULL,
                                       num_trees = NULL,
                                       node_size = NULL,
                                       error_type = "OOB",
                                       verbose = TRUE,
                                       nfold = 2)
# Simple ATE
wioa_overall_ate <- broom::tidy(lm(
  y_residuals ~ d_residuals,
  data = bind_cols("y_residuals" = wioa_first_stage$y_residuals,
                   "d_residuals" = wioa_first_stage$d_residuals)
))

# Extract CATEs -----------------------------------------------------------

# CATES from second-stage Ridge
rjri_cates_activity <- rjri_second_stage_ridge_activity %>%
  filter(str_detect(variables, ":")) %>%
  mutate(
    variables = str_remove_all(variables, "d_residuals:training_program"),
    `treatment_2.5%` = rjri_second_stage_ridge_activity %>% 
      filter(variables == "d_residuals") %>% 
      pull(`2.5%`),
    treatment = rjri_second_stage_ridge_activity %>% 
      filter(variables == "d_residuals") %>% 
      pull(coef),
    `treatment_97.5%` = rjri_second_stage_ridge_activity %>% 
      filter(variables == "d_residuals") %>% 
      pull(`97.5%`),
    cate_low = `2.5%` + `treatment_2.5%`,
    cate = coef + treatment,
    cate_hi = `97.5%` + `treatment_97.5%`
  ) %>%
  rename("program" = "variables") %>%
  select(program, cate_low, cate, cate_hi)

rjri_cates_partnership <- rjri_second_stage_ridge_partnership %>%
  filter(str_detect(variables, ":")) %>%
  mutate(
    variables = str_remove_all(variables, "d_residuals:partnership"),
    `treatment_2.5%` = rjri_second_stage_ridge_partnership %>% 
      filter(variables == "d_residuals") %>% 
      pull(`2.5%`),
    treatment = rjri_second_stage_ridge_partnership %>% 
      filter(variables == "d_residuals") %>% 
      pull(coef),
    `treatment_97.5%` = rjri_second_stage_ridge_partnership %>% 
      filter(variables == "d_residuals") %>% 
      pull(`97.5%`),
    cate_low = `2.5%` + `treatment_2.5%`,
    cate = coef + treatment,
    cate_hi = `97.5%` + `treatment_97.5%`
  ) %>%
  rename("partnership" = "variables") %>%
  select(partnership, cate_low, cate, cate_hi)

# Output Results ----------------------------------------------------------

# Analysis panel demographics
write_csv(
  rjri_analysis_dems,
  here::here("scratch/derived/rjri-analysis-panel-demographics.csv")
)

# First-Stage results
write_csv(
  tibble("RMSE" = rjri_first_stage$RMSE,
         "R2" = rjri_first_stage$R2,
         "AUC" = rjri_first_stage$AUC,
         "ATE1" = rjri_overall_ate$estimate[[2]],
         "ATE2" = mean(
           unlist(lapply(rjri_first_stage$theta_estimates, function(i) i$theta))
         )),
  here::here("results/dml/rjri-first-stage-metrics.csv")
)

# Propensity Score plot
ggsave(
  plot = rjri_first_stage$d_propensity_hist,
  filename = here::here("results/dml/rjri-treatment-propensity-scores.png"),
  width = 10,
  height = 6
)

# Actual vs. Predicted Outcome plot
ggsave(
  plot = rjri_first_stage$y_actual_vs_pred,
  filename = here::here("results/dml/rjri-outcome-actual-vs-predicted.png"),
  width = 10,
  height = 6
)

# Outcome Residual Plots
ggsave(
  plot = rjri_first_stage$y_resid_hist,
  filename = here::here("results/dml/rjri-outcome-residuals.png"),
  width = 8,
  height = 8
)

# Ridge CATES - Activity
write_csv(
  rjri_cates_activity,
  here::here("results/dml/rjri-ridge-cates-activity.csv")
)

# Ridge CATES - Partnership
write_csv(
  rjri_cates_partnership,
  here::here("results/dml/rjri-ridge-cates-partnership.csv")
)

# WIOA overall ATE
write_csv(
  wioa_overall_ate,
  here::here("results/dml/wioa-overall-ate.csv")
)

# WIOA overall ATE
write_csv(
  wioa_overall_ate,
  here::here("results/dml/wioa-overall-ate.csv")
)