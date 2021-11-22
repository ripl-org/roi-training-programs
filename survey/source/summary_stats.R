# Load required packages -----------------
require(here)
require(tidyverse)

# Load survey data
survey_df <- read_delim(here("data", "survey_data.txt"), delim="|")

# Survey descriptives
survey_df <- survey_df %>%
    mutate(
        Female = case_when(
            S1 == 1 ~ 1,
            TRUE ~ 0
        ),
        Age = 2020 - S2ac2,
        Region = as.factor(region4),
        `White, Non-Hispanic` = case_when(
            S5r1 == 1 & S4 != 2 ~ 1,
            TRUE ~ 0
        ),
        Black = case_when(
            S5r2 == 1 ~ 1,
            TRUE ~ 0
        ),
        Asian = case_when(
            S5r3 == 1 ~ 1,
            TRUE ~ 0
        ),
        Hispanic = case_when(
            S4 == 2 ~ 1,
            TRUE ~ 0
        ),
        `Employed (full-time)` = case_when(
            S8 == 1 ~ 1,
            TRUE ~ 0
        ),
        `Unemployed, looking for work` = case_when(
            S8 %in% c(3, 4, 5) ~ 1,
            TRUE ~ 0
        ),
        Income = as.factor(S9),
        `Ever enrolled in job training` = case_when(
            Q10 == 1 ~ 1,
            TRUE ~ 0
        )
    )

sum_table <- compareGroups::compareGroups(
    ~ Female +
    Age +
    Region +
    `White, Non-Hispanic` +
    Black +
    Asian +
    Hispanic +
    `Employed (full-time)` +
    `Unemployed, looking for work` +
    Income +
    `Ever enrolled in job training`,
    data = survey_df) %>% compareGroups::createTable()

compareGroups::export2xls(sum_table, here("output", "summary_stats.xlsx"))
