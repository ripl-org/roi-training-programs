# Packages ----------------------------------------------------------------

library(fst)
library(future.apply)
library(lubridate)
library(dplyr)
library(readr)

# Global Variables --------------------------------------------------------

# Set seed
set.seed(123)

# Initialize parallel session
options(future.globals.maxSize = Inf)
plan(multisession, workers = future::availableCores() - 1)

# Wage radius to match on
WAGE_RADIUS <- 500
out_dir <- here::here("scratch/analysis/matching")

# Load Data ---------------------------------------------------------------

### RJRI ###

# Load Full analysis panel
analysis <- read_fst(here::here("scratch/derived/rjri-analysis-panel.fst"))

# Split into Treatment and Control groups
treatment <- analysis %>% filter(treatment == "Treatment")
control <- analysis %>% filter(treatment == "Control")

# Split control by yyq
control <- group_by(control, yyq)
# Get distinct yyqs
control_keys <- group_keys(control)$yyq
# Split into list
control_yyqs <- group_split(control)

### WIOA ###

wioa_wp <- read_fst(here::here("scratch/derived/wioa-wp-analysis-panel.fst")) %>%
    mutate(training_program = "WIOA")

# Split into Treatment and Control groups
treatment_wioa <- wioa_wp %>% filter(treatment == "Treatment")
control_wioa <- wioa_wp %>% filter(treatment == "Control")

# Split control by yyq
control_wioa <- group_by(control_wioa, yyq)
# Get distinct yyqs
control_keys_wioa <- group_keys(control_wioa)$yyq
# Split into list
control_yyqs_wioa <- group_split(control_wioa)

# Compute Matches ---------------------------------------------------------

## For RJRI ##

cat("Identifying matches for each Training SIRAD ID\n")
matches_rjri <- future_lapply(1:nrow(treatment), function(i)
{

    self_sirad  <- treatment$sirad_id[i]
    program     <- treatment$training_program[i]
    treat_yyq   <- treatment$yyq[i]
    wages       <- treatment$wages[i]
    wages1      <- treatment$wages_prev1[i]
    wages2      <- treatment$wages_prev2[i]
    wages3      <- treatment$wages_prev3[i]
    wages4      <- treatment$wages_prev4[i]
    naics1      <- treatment$naics_prev1[i]
    naics2      <- treatment$naics_prev2[i]
    naics3      <- treatment$naics_prev3[i]
    naics4      <- treatment$naics_prev4[i]

    yyq_idx <- which(control_keys == treat_yyq)
    if (!length(yyq_idx) == 1) stop("YYQ has length longer than 1")
    ctrl <- control_yyqs[[yyq_idx]]

    idx <- which(
        ((is.na(ctrl$wages_prev1) & is.na(wages1)) |
             abs(ctrl$wages_prev1 - wages1) <= WAGE_RADIUS
        ) &
            # ((is.na(ctrl$wages) & is.na(wages)) |
            #      abs(ctrl$wages - wages) <= WAGE_RADIUS
            # ) &
            ((is.na(ctrl$wages_prev2) & is.na(wages2)) |
                 abs(ctrl$wages_prev2 - wages2) <= WAGE_RADIUS
            ) &
            ((is.na(ctrl$wages_prev3) & is.na(wages3)) |
                 abs(ctrl$wages_prev3 - wages3) <= WAGE_RADIUS
            ) &
            ((is.na(ctrl$wages_prev4) & is.na(wages4)) |
                 abs(ctrl$wages_prev4 - wages4) <= WAGE_RADIUS
            )
    )
    # Create output dataframe
    treat_out <- treatment[i, ] %>%
        mutate(treatment_id = self_sirad,
               program = program,
               control_id = NA,
               treatment = "Treatment") %>%
        # rename_with(.cols = c(contains("wages"), contains("naics")),
        #             .fn = ~ paste0(.x, "_treatment")) %>%
        select(treatment_id,
               yyq,
               program,
               control_id,
               contains("wages"), 
               contains("naics"),
               treatment)
    ctrl <- ctrl[idx,] %>% 
        mutate(treatment_id=self_sirad,
               program=program,
               treatment = "Control") %>%
        rename(control_id=sirad_id) 
    if (nrow(ctrl) > 25) ctrl <- ctrl[sort(sample(1:nrow(ctrl), 25)), ]
    ctrl <- ctrl %>%
        # rename_with(.cols = c(contains("wages"), contains("naics")), 
        #             .fn = ~ paste0(.x, "_control")) %>%
        bind_rows(treat_out) %>%
        select(treatment_id, 
               yyq,
               program,
               control_id,
               contains("wages"),
               contains("naics"),
               treatment)
    if (all(ctrl$treatment == "Treatment")) {
        return(tibble())
    }
    return(ctrl)
}, future.seed = TRUE) %>%
    bind_rows()

perc_match <- 100*round(nrow(distinct(matches_rjri %>%
                                          filter(treatment == "Control"), 
                                      treatment_id, 
                                      yyq))/nrow(distinct(treatment, sirad_id, yyq)),
                        digits = 2)
cat("- Percentage of Matches Found for RJRI:", perc_match, "\n")

cat("Writing output\n")
write_fst(matches_rjri, paste0(out_dir, "/matches-rjri.fst"))
cat("Done.\n")

rm(matches_rjri)

## WIOA/WP ##

cat("Identifying matches for each Training SIRAD ID\n")
matches_wioa <- future_lapply(1:nrow(treatment_wioa), function(i)
{
    
    self_sirad  <- treatment_wioa$sirad_id[i]
    program     <- treatment_wioa$training_program[i]
    treat_yyq   <- treatment_wioa$yyq[i]
    wages       <- treatment_wioa$wages[i]
    wages1      <- treatment_wioa$wages_prev1[i]
    wages2      <- treatment_wioa$wages_prev2[i]
    wages3      <- treatment_wioa$wages_prev3[i]
    wages4      <- treatment_wioa$wages_prev4[i]
    naics1      <- treatment_wioa$naics_prev1[i]
    naics2      <- treatment_wioa$naics_prev2[i]
    naics3      <- treatment_wioa$naics_prev3[i]
    naics4      <- treatment_wioa$naics_prev4[i]
    
    yyq_idx <- which(control_keys_wioa == treat_yyq)
    if (!length(yyq_idx) == 1) stop("YYQ has length longer than 1")
    ctrl <- control_yyqs_wioa[[yyq_idx]]
    
    idx <- which(
        ((is.na(ctrl$wages_prev1) & is.na(wages1)) |
             abs(ctrl$wages_prev1 - wages1) <= WAGE_RADIUS
        ) &
            # ((is.na(ctrl$wages) & is.na(wages)) |
            #      abs(ctrl$wages - wages) <= WAGE_RADIUS
            # ) &
            ((is.na(ctrl$wages_prev2) & is.na(wages2)) |
                 abs(ctrl$wages_prev2 - wages2) <= WAGE_RADIUS
            ) &
        ((is.na(ctrl$wages_prev3) & is.na(wages3)) |
             abs(ctrl$wages_prev3 - wages3) <= 1500
        ) &
            ((is.na(ctrl$wages_prev4) & is.na(wages4)) |
                 abs(ctrl$wages_prev4 - wages4) <= 1500
            )
    )
    # Create output dataframe
    treat_out <- treatment_wioa[i, ] %>%
        mutate(treatment_id = self_sirad,
               program = program,
               control_id = NA,
               treatment = "Treatment") %>%
        # rename_with(.cols = c(contains("wages"), contains("naics")),
        #             .fn = ~ paste0(.x, "_treatment")) %>%
        select(treatment_id,
               yyq,
               program,
               control_id,
               contains("wages"), 
               contains("naics"),
               treatment)
    ctrl <- ctrl[idx,] %>% 
        mutate(treatment_id=self_sirad,
               program=program,
               treatment = "Control") %>%
        rename(control_id=sirad_id) %>%
        # rename_with(.cols = c(contains("wages"), contains("naics")), 
        #             .fn = ~ paste0(.x, "_control")) %>%
        bind_rows(treat_out) %>%
        select(treatment_id, 
               yyq,
               program,
               control_id,
               contains("wages"),
               contains("naics"),
               treatment)
    if (all(ctrl$treatment == "Treatment")) {
        return(tibble())
    }
    return(ctrl)
}, future.seed = TRUE) %>%
    bind_rows()

perc_match <- 100*round(nrow(distinct(matches_wioa %>%
                                          filter(treatment == "Control"), 
                                      treatment_id, 
                                      yyq))/nrow(distinct(treatment_wioa, sirad_id, yyq)),
                        digits = 2)
cat("- Percentage of Matches Found for WIOA:", perc_match, "\n")

# Merge Matches back to Wages ---------------------------------------------

cat("Writing output\n")
write_fst(matches_wioa, paste0(out_dir, "/matches-wioa.fst"))
cat("Done.\n")
