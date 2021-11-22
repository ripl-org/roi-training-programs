# Packages ----------------------------------------------------------------

library(fst)
library(dplyr)
library(dtplyr)
library(lubridate)
library(tidyverse)

# Global Variables --------------------------------------------------------

matchfile <- here::here("scratch/analysis/matching/matches-rjri.fst")
matchfile_wioa <- here::here("scratch/analysis/matching/matches-wioa.fst")

# Data Cleaning -----------------------------------------------------------

## RJRI ##

# Matches
match <- read_fst(matchfile)

cat("Pivoting match to long format and replacing missing outcome with 0\n")
match <- match %>%
         rename(`-4`=paste0("wages", "_prev4"),
                `-3`=paste0("wages", "_prev3"),
                `-2`=paste0("wages", "_prev2"),
                `-1`=paste0("wages", "_prev1"),
                 `0`="wages",
                 `1`=paste0("wages", "_next1"),
                 `2`=paste0("wages", "_next2"),
                 `3`=paste0("wages", "_next3"),
                 `4`=paste0("wages", "_next4")) %>%
         pivot_longer(cols=`-4`:`4`,
                      names_to="relative_yyq",
                      values_to="outcome") %>%
         mutate(relative_yyq=as.integer(relative_yyq),
                outcome=ifelse(is.na(outcome), 0, outcome),
                probability=as.integer(outcome > 0))

cat("Summarizing control observations 1:1 with training observations\n")
match1to1 <- bind_rows(match %>% 
                           lazy_dt() %>%
                           filter(treatment=="Control") %>%
                           group_by(treatment_id,
                                    yyq,
                                    program,
                                    relative_yyq,
                                    treatment) %>%
                           summarise(outcome=mean(outcome), 
                                     probability=mean(probability)) %>%
                           ungroup() %>%
                           as_tibble(),
                       match %>% filter(treatment=="Treatment")) %>%
             select(treatment_id:probability)

cat("Adding an 'All-RJRI' program")
match1to1 <- match1to1 %>%
    bind_rows(match1to1 %>%
                  mutate(program = "RJRI")) %>%
    mutate(yyq=12*floor(yyq/10) + 3*(yyq %% 10) + 3*relative_yyq) %>% # Convert YYQ to months for easier calculations
    group_by(relative_yyq, program, treatment) %>%
    mutate(mean_outcome = mean(outcome)) %>%
    ungroup()

cat("Residualizing outcome by YYQ fixed effects\n")
resids <- residuals(lm(outcome ~ factor(yyq), data = match1to1))
match1to1 <- match1to1 %>%
    mutate(residualized_outcome = resids,
           adj_residualized_outcome = residualized_outcome + mean_outcome)

# Calculating averages by Control and Treatment across programs
match1to1 <- match1to1 %>%
    group_by(relative_yyq, treatment, program) %>%
    summarise(adj_outcome = mean(adj_residualized_outcome),
              mean_outcome = mean(outcome)) %>%
    ungroup() %>%
    arrange(program, relative_yyq, treatment)

## WIOA/WP ##

# Matches
match_wioa <- read_fst(matchfile_wioa)

cat("Pivoting match to long format and replacing missing outcome with 0\n")
match_wioa <- match_wioa %>%
    rename(`-4`=paste0("wages", "_prev4"),
           `-3`=paste0("wages", "_prev3"),
           `-2`=paste0("wages", "_prev2"),
           `-1`=paste0("wages", "_prev1"),
           `0`="wages",
           `1`=paste0("wages", "_next1"),
           `2`=paste0("wages", "_next2"),
           `3`=paste0("wages", "_next3"),
           `4`=paste0("wages", "_next4")) %>%
    pivot_longer(cols=`-4`:`4`,
                 names_to="relative_yyq",
                 values_to="outcome") %>%
    mutate(relative_yyq=as.integer(relative_yyq),
           outcome=ifelse(is.na(outcome), 0, outcome),
           probability=as.integer(outcome > 0))

cat("Summarizing control observations 1:1 with training observations\n")
match1to1_wioa <- bind_rows(match_wioa %>% 
                                lazy_dt() %>%
                                filter(treatment=="Control") %>%
                                group_by(treatment_id,
                                         yyq,
                                         program,
                                         relative_yyq,
                                         treatment) %>%
                                summarise(outcome=mean(outcome), 
                                          probability=mean(probability)) %>%
                                ungroup() %>%
                                as_tibble(),
                            match_wioa %>% filter(treatment=="Treatment")) %>%
    select(treatment_id:probability)

cat("Adding an 'All-WIOA' program")
match1to1_wioa <- match1to1_wioa %>%
    mutate(yyq=12*floor(yyq/10) + 3*(yyq %% 10) + 3*relative_yyq) %>% # Convert YYQ to months for easier calculations
    group_by(relative_yyq, program, treatment) %>%
    mutate(mean_outcome = mean(outcome)) %>%
    ungroup()

cat("Residualizing outcome by YYQ fixed effects\n")
resids <- residuals(lm(outcome ~ factor(yyq), data = match1to1_wioa))
match1to1_wioa <- match1to1_wioa %>%
    mutate(residualized_outcome = resids,
           adj_residualized_outcome = residualized_outcome + mean_outcome)

# Calculating averages by Control and Treatment across programs
match1to1_wioa <- match1to1_wioa %>%
    group_by(relative_yyq, treatment, program) %>%
    summarise(adj_outcome = mean(adj_residualized_outcome),
              mean_outcome = mean(outcome)) %>%
    ungroup() %>%
    arrange(program, relative_yyq, treatment)

# Output Data -------------------------------------------------------------

write_csv(
    match1to1,
    here::here("scratch/analysis/matching/plot-match_rjri.csv")
)

write_csv(
    match1to1_wioa,
    here::here("scratch/analysis/matching/plot-match_wioa.csv")
)
