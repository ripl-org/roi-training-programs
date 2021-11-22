# Packages ----------------------------------------------------------------

library(data.table)
library(fst)
library(dplyr)
library(future.apply)
library(dtplyr)
library(tidyr)
library(purrr)

# Global Variables --------------------------------------------------------

wagefile <- here::here("scratch/derived/wage-individual.fst")
uifile   <- here::here("scratch/derived/ui.fst")
outdir   <- here::here("scratch/derived")
plan(multisession, workers = availableCores() - 1)

# Functions ---------------------------------------------------------------

# Function that will lead and lag the wage and UI tables
lead_lag <- function(dat.subset,
                     dat.full,
                     lag.quarters = 4,
                     lead.quarters = 4) {
    # Make sure necessary columns exist
    if (!all(c("sirad_id", "yyq") %in% names(dat.subset))) {
        stop("One or both of 'sirad_id' and 'yyq' are missing", call. = FALSE)
    }
    # Make sure necessary columns exist
    if (!all(c("sirad_id", "yyq") %in% names(dat.full))) {
        stop("One or both of 'sirad_id' and 'yyq' are missing", call. = FALSE)
    }
    
    ### Run for Lags ###
    if (!is.null(lag.quarters)) {
        # Create numeric vector for prev quarters
        quarters_prev <- 1:lag.quarters
        # Create df list
        df_list <- list()
        # For every quarter, create lead and lag column
        lapply(quarters_prev, function(i) {
            df <- dat.subset %>%
                lazy_dt() %>%
                mutate(merge_col_prev = yyq - 3 * i) %>%
                left_join(dat.full %>%
                              rename_with(
                                  .cols = -c(sirad_id, yyq),
                                  .fn = ~ paste0(.x, "_prev", i)
                              ),
                          by = c("sirad_id",
                                 "merge_col_prev" = "yyq"
                          )
                ) %>%
                as_tibble() %>%
                select(-merge_col_prev)
            df_list <<- append(df_list, list(df))
            cat("\rLagged by", i, "quarter(s)\n")
        })
        # Get common names
        common_names <- lapply(df_list, names) %>% reduce(.f = intersect)
        common_names <- common_names[!common_names %in% c("sirad_id", "yyq")]
        # Remove these names from additional dfs
        df_list_addtl <- lapply(2:length(df_list), function(i) {
            df_list[[i]] %>%
                select(-all_of(common_names))
        })
        df_list <- append(df_list_addtl, list(df_list[[1]]), after = 0)
        # Combine in one df
        cat("\rMerging lagged columns\n")
        dat_prev <- df_list %>% reduce(left_join, by = c("sirad_id", "yyq"))
    } else {
        dat_prev <- tibble()
    }
    
    ### Run for leads ###
    
    if (!is.null(lead.quarters)) {
        # Create numeric vector for prev quarters
        quarters_next <- 1:lead.quarters
        # Create df list
        df_list <- list()
        # For every quarter, create lead and lag column
        lapply(quarters_next, function(i) {
            df <- dat.subset %>%
                lazy_dt() %>%
                mutate(merge_col_next = yyq + 3 * i) %>%
                left_join(dat.full %>%
                              rename_with(
                                  .cols = -c(sirad_id, yyq),
                                  .fn = ~ paste0(.x, "_next", i)
                              ),
                          by = c("sirad_id",
                                 "merge_col_next" = "yyq"
                          )
                ) %>%
                as_tibble() %>%
                select(-merge_col_next)
            df_list <<- append(df_list, list(df))
            cat("\rLead by", i, "quarter(s)\n")
        })
        # Get common names
        common_names <- lapply(df_list, names) %>% reduce(.f = intersect)
        common_names <- common_names[!common_names %in% c("sirad_id", "yyq")]
        # Remove these names from additional dfs
        df_list_addtl <- lapply(2:length(df_list), function(i) {
            df_list[[i]] %>%
                select(-all_of(common_names))
        })
        df_list <- append(df_list_addtl, list(df_list[[1]]), after = 0)
        # Combine in one df
        cat("\rMerging leading columns\n")
        dat_next <- df_list %>% reduce(left_join, by = c("sirad_id", "yyq"))
    } else {
        dat_next <- tibble()
    }
    
    ### Combine lead and lagged together
    
    if (!is.null(lag.quarters) & !is.null(lead.quarters)) {
        # Get common names
        common_names <- lapply(list(dat_prev, dat_next), names) %>%
            reduce(.f = intersect)
        dat <- inner_join(dat_prev, dat_next, by = common_names)
        # Ensure dimensions of dat are correct
        if (!nrow(dat) == nrow(dat_prev)) stop()
    } else {
        if (!is.null(lag.quarters)) {
            dat <- dat_prev
        } else {
            dat <- dat_next
        }
    }
    cat("\rDone!\n")
    return(dat)
}

# Clean Data --------------------------------------------------------------

cat("Loading UI table\n")
ui <- read_fst(uifile, 
               columns = c("sirad_id", "claim_yyq")) %>%
    mutate(ui_flag = 1)

cat("Loading WageIndividual\n")
wage <- read_fst(wagefile, 
                 columns=c("sirad_id", 
                           "yyq", 
                           "wages", 
                           "naics4", 
                           "ern"))
cat("- Raw rows:", nrow(wage), "\n")

# Get full list of sirad_ids
wage_ids <- sort(unique(wage$sirad_id))
# Full list of YYQs
wage_yyqs <- sort(unique(wage$yyq))

# Create full list of Wages that SHOULD exist
cat("Creating full table of wage observations that SHOULD exist\n")
wage_full <- future_lapply(wage_ids, function(i) {
    tibble("sirad_id" = i,
           "yyq" = wage_yyqs)
}, future.seed = TRUE) %>% bind_rows()

# Outer join to get full set of observations
wage <- wage_full %>%
    full_join(wage, by = c("sirad_id", "yyq")) %>%
    mutate(across(c(sirad_id, yyq), as.character))
    left_join(ui %>%
                distinct(sirad_id, claim_yyq, .keep_all = TRUE), 
              by = c("sirad_id", "yyq" = "claim_yyq")) %>%
    mutate(ui_flag = case_when(ui_flag == 1 ~ 1,
                               TRUE ~ 0))

# Remove wage_full and garbage collect
rm(wage_full)
gc()

cat("Converting NAICS to factor and WAGES to integer\n")
wage <- wage %>%
        lazy_dt() %>%
        group_by(ern) %>%
        mutate(n_ern=n_distinct(sirad_id)) %>%
        ungroup() %>%
        mutate(wages=as.integer(wages),
               ern=case_when(n_ern > 20 ~ ern,
                             TRUE       ~ "CENSORED"),
               n_ern=case_when(is.na(ern) ~ NA_integer_,
                               TRUE ~ n_ern)) %>%
        as_tibble() %>%
        arrange(sirad_id, yyq)

cat("Top-coding WAGES\n")
p <- quantile(wage$wages, 0.995, na.rm = TRUE)
wage <- filter(wage, 
               wages < p | is.na(wages))
cat("- 99.5th percentile:", p, "\n")

cat("Converting YYQ to months\n")
wage <- mutate(wage,
               yyq = as.numeric(yyq),
               yyq=as.integer(12*floor(yyq/10) + 3*((yyq %% 10) - 1))) # Convert YYQ to months since 2000,
                                                                       # to simplify lag calculations
cat("Select only necessary columns\n")
wage <- wage %>%
        rename(naics = naics4) %>%
        select(sirad_id, wages, yyq, naics, ern, ui_flag)

cat("Generating lags\n")
wage <- lead_lag(dat.subset = wage,
                 dat.full = wage,
                 lag.quarters = 4,
                 lead.quarters = 4)
cat("- Rows:", nrow(wage), "\n")

cat("Converting YYQ back to year/quarter\n")
wage <- mutate(wage,
               yyq=as.integer(10*floor(yyq/12) + floor((yyq %% 12) / 3) + 1))

cat("Reordering final columns\n")
wage <- select(wage,
               sirad_id, yyq,
               wages_prev4, wages_prev3, wages_prev2, wages_prev1, wages, wages_next1, wages_next2, wages_next3, wages_next4,
               ern_prev4, ern_prev3, ern_prev2, ern_prev1, ern, ern_next1, ern_next2, ern_next3, ern_next4,
               naics_prev4, naics_prev3, naics_prev2, naics_prev1, naics, naics_next1, naics_next2, naics_next3, naics_next4,
               ui_flag_prev4, ui_flag_prev3, ui_flag_prev2, ui_flag_prev1, ui_flag, ui_flag_next1, ui_flag_next2, ui_flag_next3, ui_flag_next4)

cat("Writing output\n")
write_fst(wage, paste0(outdir, "/wage-individual-lagged.fst"))

cat("Done.\n")
