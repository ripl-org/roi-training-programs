# Packages ----------------------------------------------------------------

library(dplyr)
library(fst)
library(lubridate)
library(readr)
library(stringr)

# Load Data ---------------------------------------------------------------

### Wage Data ###

wage <- read_fst(here::here("scratch/derived/wage-individual-lagged.fst"))

### Enrollee Data

# Training Program enrollee table
person <- read_csv(here::here("scratch/derived/rjri-enrollees.csv"),
                   guess_max = 100000) %>%
  mutate(yyq = as.numeric(yyq)) %>%
  filter(yyq %>% between(161, 184))

# WIOA/WIA table
wioa_wp <- read_csv(here::here("scratch/derived/wioa-wp-enrollees.csv"),
                    guess_max = 100000) %>%
  mutate(yyq = as.numeric(yyq)) %>%
  filter(yyq %>% between(161, 184))

### Merge Wage and Enrollee Data ###

person <- person %>%
  left_join(wage %>%
              mutate(across(c(sirad_id, yyq), as.numeric)),
            by = c("sirad_id", "yyq")) %>%
  mutate(across(contains("wages"), ~ replace(.x, .x == 0, NA)),
         across(everything(), ~ replace(.x, .x == "U", NA)),
         across(everything(), ~ replace(.x, .x == "Unknown", NA))) %>%
  rowwise() %>%
  mutate(n_missing_wages_prev = sum(is.na(c_across(starts_with("wages_prev")))),
         n_missing_wages_post = sum(is.na(c_across(starts_with("wages_next"))))) %>%
  ungroup() %>%
  filter(n_missing_wages_prev < 4,
         n_missing_wages_post < 4) %>%
  select(-c(n_missing_wages_prev,
            n_missing_wages_post)) %>%
  mutate(treatment = "Treatment")
cat("Rows:", nrow(person), "\n")

# Wage and wIOA data
wioa_wp <- wioa_wp %>%
  left_join(wage %>%
              mutate(across(c(sirad_id, yyq), as.numeric)),
            by = c("sirad_id", "yyq")) %>%
  mutate(across(contains("wages"), ~ replace(.x, .x == 0, NA)),
         across(everything(), ~ replace(.x, .x == "U", NA)),
         across(everything(), ~ replace(.x, .x == "Unknown", NA))) %>%
  rowwise() %>%
  mutate(n_missing_wages_prev = sum(is.na(c_across(starts_with("wages_prev")))),
         n_missing_wages_post = sum(is.na(c_across(starts_with("wages_next"))))) %>%
  ungroup() %>%
  filter(n_missing_wages_prev < 4,
         n_missing_wages_post < 4) %>%
  select(-c(n_missing_wages_prev,
            n_missing_wages_post))

# Construct control group -------------------------------------------------

# Don't keep any overlap with person table and restrict to 2016-2018
# Drop any observations where the individual is missing all previous or post wages
wage_control <- wage %>%
  filter(!sirad_id %in% person$sirad_id,
         yyq %>% between(161, 184),
         !(is.na(wages_prev4) &
             is.na(wages_prev3) &
             is.na(wages_prev2) &
             is.na(wages_prev1)),
         !(is.na(wages_next4) &
             is.na(wages_next3) &
             is.na(wages_next2) &
             is.na(wages_next1))) %>%
  mutate(treatment = "Control")

# Combined Person and Wage Control group
analysis_panel <- bind_rows(person %>%
                              mutate(sirad_id = as.character(sirad_id)),
                            wage_control) %>%
  mutate(training_program = trimws(
    gsub("(Cohort.*)|Cohort.*|cohort.*|Phase.*|(Phase.*)|Cohorts.*|
                 Cohorts.*|Addendum.*|(Addendum.*)|\\(.*\\)|.\\..|
                 Program|:.*|Certificate Program|Specialized|
                 to Medical.*|CF.*|PILOT.*|Providence.*|Westerly|ADVANCE|
                 EMERGE.*| -|Machine|Accelerate.*|Elevate.*|Expose.*|
                 Incubate.*|\\&.*|Incubate.*|Immersive",
         "",
         training_program)
  ),
  training_program = case_when(
    startsWith(training_program, "CNA") ~ "CNA Training",
    startsWith(training_program, "Community Health") ~ 
      "Community Health Worker Training",
    startsWith(training_program, "Behavioral Health") ~ 
      "Behavioral Health",
    startsWith(training_program, "Composites") ~ 
      "composites Pre-Apprenticeship Program",
    startsWith(training_program, "Design Industry") ~ "Design Industry",
    startsWith(training_program, "Fast Track") ~ 
      "Fast Track to Manufacturing CNC",
    startsWith(training_program, "Industry-Driven") ~ 
      "Industry-Driven Incumbent Worker Training",
    startsWith(training_program, "OSHA") ~ 
      "Occupational Safety and Health Administration",
    startsWith(training_program, "Occupational Safety") ~ 
      "Occupational Safety and Health Administration",
    startsWith(training_program, "RI Makers") ~ "RI Makers",
    startsWith(training_program, "Weld") ~ "Weld to Work",
    training_program == "Health" ~ "Health and Wellness Accelerator",
    startsWith(training_program, "SAMI") ~ "SAMI",
    startsWith(training_program, "LaunchCode") ~ "LaunchCode",
    startsWith(training_program, "Tiffany") ~ "Tiffany & Co.",
    TRUE ~ training_program
  ))

# Output Data -------------------------------------------------------------

cat("Writing output\n")
write_fst(
  analysis_panel, 
  here::here("scratch/derived/rjri-analysis-panel.fst")
)

write_fst(
  wioa_wp,
  here::here("scratch/derived/wioa-wp-analysis-panel.fst")
)