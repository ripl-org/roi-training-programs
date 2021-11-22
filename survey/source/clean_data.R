# Load required packages -----------------
require(here)
require(tidyverse)
require(readxl)
require(haven)
require(sjPlot)

# Load Data ------------------------------
conjoint_df <- read_excel(here("data", "ORD-461143-V3R5_Final_Excel_Completes_042120.xlsx"))

# Save data dictionary
conjoint_df %>% sjPlot::view_df(
  use.viewer = FALSE, show.frq = TRUE, show.prc = TRUE, show.na = TRUE,
  file = here("output", "conjoint_data_dictionary.html")
)

# Survey Data ----------------------

survey_df <- conjoint_df %>%
                  select(
                            -psid, -Country, -status,
                            # Remove Conjoint 1
                            -starts_with("hCon1Attr"),
                            -starts_with("Q25_"),
                            -starts_with("Q26_"),
                            -starts_with("Q27_"),
                            -starts_with("Q28_"),
                            -starts_with("Q29_"),
                            # Remove Conjoint 2
                            -starts_with("hCon2Attr"),
                            -starts_with("Q30_"),
                            -starts_with("Q31_"),
                            -starts_with("Q32_"),
                            -starts_with("Q33_"),
                            -starts_with("Q34_"),
                            # Remove Conjoint 3
                            -starts_with("hCon3Attr"), 
                            -starts_with("Q35_"),
                            -starts_with("Q36_"),
                            -starts_with("Q37_"),
                            -starts_with("Q38_"),
                            -starts_with("Q39_"),
                            # Remove Conjoint 4
                            -starts_with("hCon4Attr"), 
                            -starts_with("Q40_"),
                            -starts_with("Q41_"),
                            -starts_with("Q42_"),
                            -starts_with("Q43_"),
                            -starts_with("Q44_"),
                            # Remove Conjoint 5
                            -starts_with("hCon5Attr"), 
                            -starts_with("Q45_"),
                            -starts_with("Q46_"),
                            -starts_with("Q47_"),
                            -starts_with("Q48_"),
                            -starts_with("Q49_"),
                            # Remove Conjoint 6
                            -starts_with("hCon6Attr"), 
                            -starts_with("Q50_"),
                            -starts_with("Q51_"),
                            -starts_with("Q52_"),
                            -starts_with("Q53_"),
                            -starts_with("Q54_"),
                            # Remove Conjoint 7
                            -starts_with("hCon7Attr"), 
                            -starts_with("Q55_"),
                            -starts_with("Q56_"),
                            -starts_with("Q57_"),
                            -starts_with("Q58_"),
                            -starts_with("Q59_"),
                            # Remove Timer variables
                            -ends_with("_Timer"),
                            # Additional Qs to remove
                            -Q62, -LOI, -dTrack,
                            -starts_with("hQfirst"),
                            -hEarning,
                            -starts_with("hValues"),
                            -starts_with("C2orderr"),
                            -starts_with("C3orderr"),
                            -starts_with("C4orderr"),
                            -starts_with("C5orderr"),
                            -starts_with("C7orderr"),
                            -Q24r1,
                            -ends_with("OrdRecord"),
                            -starts_with("dTermr")
                            )

# Save survey responses
write_delim(survey_df, here("data", "survey_data.txt"),  delim = "|")
write_dta(survey_df, here("data", "survey_data.dta"))

# Save data dictionary
survey_df %>% sjPlot::view_df(
  use.viewer = FALSE, show.frq = TRUE, show.prc = TRUE, show.na = TRUE,
  file = here("output", "survey_data_dictionary.html")
)

# Conjoint Table 1 ------------------------------

cj1 <- conjoint_df %>%
  select(
    uuid, starts_with("hCon1Attr1"),
    starts_with("Q25_"),
  ) %>%
  rename_at(
    .vars = vars(starts_with("hCon1Attr1_")),
    .funs = ~ sub("hCon1Attr1_", "tab", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("r1", "r1_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("r2", "r2_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("r3", "r3_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("Q25_")),
    .funs = ~ sub("Q25_", "tab", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("tab1c", "tab1rank_c", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("tab2c", "tab2rank_c", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("tab3c", "tab3rank_c", .))

cj1_reflect <- conjoint_df %>%
  select(
    uuid,
    starts_with("Q26_"),
    starts_with("Q27_"),
    starts_with("Q28_"),
    starts_with("Q29_"),
  ) %>%
  pivot_longer(
    -uuid,
    names_to = c(".value", "tab"),
    names_sep = "_",
    values_drop_na = TRUE
  ) %>%
  rename(enroll = Q26) %>%
  rename(enroll_freetext = Q27) %>%
  rename(earnmore = Q28) %>%
  rename(earnmore_freetext = Q29) %>%
  mutate(cj = 1) %>%
  select(uuid, tab, cj, everything())

cj1_long <- cj1 %>%
  pivot_longer(
    -uuid,
    names_to = c(".value", "c"),
    names_sep = "_",
    values_drop_na = TRUE
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab1")),
    .funs = ~ sub("tab1", "tab1_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab2")),
    .funs = ~ sub("tab2", "tab2_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab3")),
    .funs = ~ sub("tab3", "tab3_", .)
  ) %>%
  pivot_longer(starts_with("tab"),
    names_to = c("tab", ".value"),
    names_sep = "_",
    values_drop_na = TRUE
  ) %>%
  rename(prof = c) %>%
  select(uuid, tab, prof, everything()) %>%
  mutate_at(vars(r1, r2, r3), as.factor) %>%
  mutate(selected = if_else(rank == 1, 1, 0)) %>%
  rename(cost = r1) %>%
  mutate(cost = fct_relevel(
    cost,
    "$0", "$100", "$250", "$500",
    "$1,000", "$2,000", "$3,000",
    "$4,000", "$5,000", "$6,000",
    "$7,000", "$8,000", "$9,000",
    "$10,000", "$15,000", "$20,000"
  )) %>%
  rename(duration = r2) %>%
  mutate(duration = fct_relevel(
    duration,
    "2 weeks", "4 weeks", "6 weeks",
    "8 weeks", "10 weeks", "12 weeks"
  )) %>%
  rename(traveltime = r3) %>%
  mutate(traveltime = fct_relevel(
    traveltime,
    "10 minutes", "20 minutes",
    "30 minutes", "40 minutes",
    "50 minutes", "60 minutes"
  )) %>%
  mutate(cj = 1) %>%
  arrange(uuid, tab, prof)

# Conjoint Table 2 ------------------------------

cj2 <- conjoint_df %>%
  select(
    uuid,
    starts_with("hCon2Attr1"),
    starts_with("Q30_"),
  ) %>%
  rename_at(
    .vars = vars(starts_with("hCon2Attr1_")),
    .funs = ~ sub("hCon2Attr1_", "tab", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("r1", "r1_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("r2", "r2_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("r3", "r3_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("r4", "r4_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("Q30_")),
    .funs = ~ sub("Q30_", "tab", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("tab1c", "tab1rank_c", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("tab2c", "tab2rank_c", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("tab3c", "tab3rank_c", .)
  )

cj2_reflect <- conjoint_df %>%
  select(
    uuid,
    starts_with("Q31_"),
    starts_with("Q32_"),
    starts_with("Q33_"),
    starts_with("Q34_"),
  ) %>%
  pivot_longer(
    -uuid,
    names_to = c(".value", "tab"),
    names_sep = "_",
    values_drop_na = TRUE
  ) %>%
  rename(enroll = Q31) %>%
  rename(enroll_freetext = Q32) %>%
  rename(earnmore = Q33) %>%
  rename(earnmore_freetext = Q34) %>%
  mutate(cj = 2) %>%
  select(uuid, tab, cj, everything())

cj2_long <- cj2 %>%
  pivot_longer(
    -uuid,
    names_to = c(".value", "c"),
    names_sep = "_",
    values_drop_na = TRUE
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab1")),
    .funs = ~ sub("tab1", "tab1_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab2")),
    .funs = ~ sub("tab2", "tab2_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab3")),
    .funs = ~ sub("tab3", "tab3_", .)
  ) %>%
  pivot_longer(starts_with("tab"),
    names_to = c("tab", ".value"),
    names_sep = "_",
    values_drop_na = TRUE
  ) %>%
  rename(prof = c) %>%
  select(uuid, tab, prof, everything()) %>%
  mutate_at(vars(r1, r2, r3, r4), as.factor) %>%
  mutate(selected = if_else(rank == 1, 1, 0)) %>%
  rename(cost = r1) %>%
  mutate(cost = fct_relevel(
    cost,
    "$0", "$100", "$250", "$500",
    "$1,000", "$2,000", "$3,000",
    "$4,000", "$5,000", "$6,000",
    "$7,000", "$8,000", "$9,000",
    "$10,000", "$15,000", "$20,000"
  )) %>%
  rename(duration = r2) %>%
  mutate(duration = fct_relevel(
    duration,
    "2 weeks", "4 weeks", "6 weeks",
    "8 weeks", "10 weeks", "12 weeks"
  )) %>%
  rename(traveltime = r3) %>%
  mutate(traveltime = fct_relevel(
    traveltime,
    "10 minutes", "20 minutes",
    "30 minutes", "40 minutes",
    "50 minutes", "60 minutes"
  )) %>%
  rename(completionrate = r4) %>%
  mutate(completionrate = fct_relevel(
    completionrate, "25%", "30%", "50%",
    "65%", "75%", "90%"
  )) %>%
  mutate(cj = 2) %>%
  arrange(uuid, tab, prof)


# Conjoint Table 3 ------------------------------

cj3 <- conjoint_df %>%
  select(
    uuid,
    starts_with("hCon3Attr1"),
    starts_with("Q35_")
  ) %>%
  rename_at(
    .vars = vars(starts_with("hCon3Attr1_")),
    .funs = ~ sub("hCon3Attr1_", "tab", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("r1", "r1_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("r2", "r2_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("r3", "r3_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("r4", "r4_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("r5", "r5_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("Q35_")),
    .funs = ~ sub("Q35_", "tab", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("tab1c", "tab1rank_c", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("tab2c", "tab2rank_c", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("tab3c", "tab3rank_c", .)
  )

cj3_reflect <- conjoint_df %>%
  select(
    uuid,
    starts_with("Q36_"),
    starts_with("Q37_"),
    starts_with("Q38_"),
    starts_with("Q39_"),
  ) %>%
  pivot_longer(
    -uuid,
    names_to = c(".value", "tab"),
    names_sep = "_",
    values_drop_na = TRUE
  ) %>%
  rename(enroll = Q36) %>%
  rename(enroll_freetext = Q37) %>%
  rename(earnmore = Q38) %>%
  rename(earnmore_freetext = Q39) %>%
  mutate(cj = 3) %>%
  select(uuid, tab, cj, everything())

cj3_long <- cj3 %>%
  pivot_longer(
    -uuid,
    names_to = c(".value", "c"),
    names_sep = "_",
    values_drop_na = TRUE
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab1")),
    .funs = ~ sub("tab1", "tab1_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab2")),
    .funs = ~ sub("tab2", "tab2_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab3")),
    .funs = ~ sub("tab3", "tab3_", .)
  ) %>%
  pivot_longer(starts_with("tab"),
    names_to = c("tab", ".value"),
    names_sep = "_",
    values_drop_na = TRUE
  ) %>%
  rename(prof = c) %>%
  select(uuid, tab, prof, everything()) %>%
  mutate_at(vars(r1, r2, r3, r4, r5), as.factor) %>%
  mutate(selected = if_else(rank == 1, 1, 0)) %>%
  rename(cost = r1) %>%
  mutate(cost = fct_relevel(
    cost,
    "$0", "$100", "$250", "$500",
    "$1,000", "$2,000", "$3,000",
    "$4,000", "$5,000", "$6,000",
    "$7,000", "$8,000", "$9,000",
    "$10,000", "$15,000", "$20,000"
  )) %>%
  rename(duration = r2) %>%
  mutate(duration = fct_relevel(
    duration,
    "2 weeks", "4 weeks", "6 weeks",
    "8 weeks", "10 weeks", "12 weeks"
  )) %>%
  rename(traveltime = r3) %>%
  mutate(traveltime = fct_relevel(
    traveltime,
    "10 minutes", "20 minutes",
    "30 minutes", "40 minutes",
    "50 minutes", "60 minutes"
  )) %>%
  rename(completionrate = r4) %>%
  mutate(completionrate = fct_relevel(
    completionrate, "25%", "30%", "50%",
    "65%", "75%", "90%"
  )) %>%
  mutate(earn_ref = as.factor(case_when(
    str_detect(r5, " per year") ~ "Year",
    str_detect(r5, " per month") ~ "Month",
    str_detect(r5, " per week") ~ "Week",
    TRUE ~ as.character(r5)
  ))) %>%
  mutate(earn_ref = fct_relevel(earn_ref, "Week", "Month", "Year")) %>%
  mutate(r5 = str_remove(r5, " per year")) %>%
  mutate(r5 = str_remove(r5, " per month")) %>%
  mutate(r5 = str_remove(r5, " per week")) %>% 
  mutate(r5 = as.numeric(str_trim(str_squish(str_remove_all(r5, ","))))) %>%
  rename(earn_posttrain = r5) %>%
  mutate(cj = 3) %>%
  arrange(uuid, tab, prof)

# Conjoint Table 4 ------------------------------

cj4 <- conjoint_df %>%
  select(
    uuid,
    starts_with("hCon4Attr1"),
    starts_with("Q40_")
  ) %>%
  rename_at(
    .vars = vars(starts_with("hCon4Attr1_")),
    .funs = ~ sub("hCon4Attr1_", "tab", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("r1", "r1_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("r2", "r2_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("r3", "r3_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("r4", "r4_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("r5", "r5_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("Q40_")),
    .funs = ~ sub("Q40_", "tab", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("tab1c", "tab1rank_c", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("tab2c", "tab2rank_c", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("tab3c", "tab3rank_c", .)
  )

cj4_reflect <- conjoint_df %>%
  select(
    uuid,
    starts_with("Q41_"),
    starts_with("Q42_"),
    starts_with("Q43_"),
    starts_with("Q44_")
  ) %>%
  pivot_longer(
    -uuid,
    names_to = c(".value", "tab"),
    names_sep = "_",
    values_drop_na = TRUE
  ) %>%
  rename(enroll = Q41) %>%
  rename(enroll_freetext = Q42) %>%
  rename(earnmore = Q43) %>%
  rename(earnmore_freetext = Q44) %>%
  mutate(cj = 4) %>%
  select(uuid, tab, cj, everything())


cj4_long <- cj4 %>%
  pivot_longer(
    -uuid,
    names_to = c(".value", "c"),
    names_sep = "_",
    values_drop_na = TRUE
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab1")),
    .funs = ~ sub("tab1", "tab1_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab2")),
    .funs = ~ sub("tab2", "tab2_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab3")),
    .funs = ~ sub("tab3", "tab3_", .)
  ) %>%
  pivot_longer(starts_with("tab"),
    names_to = c("tab", ".value"),
    names_sep = "_",
    values_drop_na = TRUE
  ) %>%
  rename(prof = c) %>%
  select(uuid, tab, prof, everything()) %>%
  mutate_at(vars(r1, r2, r3, r4, r5), as.factor) %>%
  mutate(selected = if_else(rank == 1, 1, 0)) %>%
  rename(cost = r1) %>%
  mutate(cost = fct_relevel(
    cost,
    "$0", "$100", "$250", "$500",
    "$1,000", "$2,000", "$3,000",
    "$4,000", "$5,000", "$6,000",
    "$7,000", "$8,000", "$9,000",
    "$10,000", "$15,000", "$20,000"
  )) %>%
  rename(duration = r2) %>%
  mutate(duration = fct_relevel(
    duration,
    "2 weeks", "4 weeks", "6 weeks",
    "8 weeks", "10 weeks", "12 weeks"
  )) %>%
  rename(traveltime = r3) %>%
  mutate(traveltime = fct_relevel(
    traveltime,
    "10 minutes", "20 minutes",
    "30 minutes", "40 minutes",
    "50 minutes", "60 minutes"
  )) %>%
  mutate(earn_ref = as.factor(case_when(
    str_detect(r4, " per year") ~ "Year",
    str_detect(r4, " per month") ~ "Month",
    str_detect(r4, " per week") ~ "Week",
    TRUE ~ as.character(r4)
  ))) %>%
  mutate(earn_ref = fct_relevel(earn_ref, "Week", "Month", "Year")) %>%
  mutate(r4 = str_remove(r4, " per year")) %>%
  mutate(r4 = str_remove(r4, " per month")) %>%
  mutate(r4 = str_remove(r4, " per week")) %>%
  mutate(r4 = as.numeric(str_trim(str_squish(str_remove_all(r4, ","))))) %>%
  rename(earn_posttrain = r4) %>%
  mutate(r5 = str_remove(r5, " per year")) %>%
  mutate(r5 = str_remove(r5, " per month")) %>%
  mutate(r5 = str_remove(r5, " per week")) %>%
  mutate(r5 = as.numeric(str_trim(str_squish(str_remove_all(r5, ","))))) %>%
  rename(earn_notrain = r5) %>%
  mutate(cj = 4) %>%
  arrange(uuid, tab, prof)

# Conjoint Table 5 ------------------------------

cj5 <- conjoint_df %>%
  select(
    uuid,
    starts_with("hCon5Attr1"),
    starts_with("Q45_")
  ) %>%
  rename_at(
    .vars = vars(starts_with("hCon5Attr1_")),
    .funs = ~ sub("hCon5Attr1_", "tab", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("r1", "r1_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("r2", "r2_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("r3", "r3_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("r4", "r4_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("Q45_")),
    .funs = ~ sub("Q45_", "tab", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("tab1c", "tab1rank_c", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("tab2c", "tab2rank_c", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs =  ~ sub("tab3c", "tab3rank_c", .)
  )

cj5_reflect <- conjoint_df %>%
  select(
    uuid,
    starts_with("Q46_"),
    starts_with("Q47_"),
    starts_with("Q48_"),
    starts_with("Q49_")
  ) %>%
  pivot_longer(
    -uuid,
    names_to = c(".value", "tab"),
    names_sep = "_",
    values_drop_na = TRUE
  ) %>%
  rename(enroll = Q46) %>%
  rename(enroll_freetext = Q47) %>%
  rename(earnmore = Q48) %>%
  rename(earnmore_freetext = Q49) %>%
  mutate(cj = 5) %>%
  select(uuid, tab, cj, everything())


cj5_long <- cj5 %>%
  pivot_longer(
    -uuid,
    names_to = c(".value", "c"),
    names_sep = "_",
    values_drop_na = TRUE
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab1")),
    .funs = ~ sub("tab1", "tab1_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab2")),
    .funs = ~ sub("tab2", "tab2_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab3")),
    .funs = ~ sub("tab3", "tab3_", .)
  ) %>%
  pivot_longer(starts_with("tab"),
    names_to = c("tab", ".value"),
    names_sep = "_",
    values_drop_na = TRUE
  ) %>%
  rename(prof = c) %>%
  select(uuid, tab, prof, everything()) %>%
  mutate_at(vars(r1, r2, r3, r4), as.factor) %>%
  mutate(selected = if_else(rank == 1, 1, 0)) %>%
  rename(cost = r1) %>%
  mutate(cost = fct_relevel(
    cost,
    "$0", "$100", "$250", "$500",
    "$1,000", "$2,000", "$3,000",
    "$4,000", "$5,000", "$6,000",
    "$7,000", "$8,000", "$9,000",
    "$10,000", "$15,000", "$20,000"
  )) %>%
  rename(duration = r2) %>%
  mutate(duration = fct_relevel(
    duration,
    "2 weeks", "4 weeks", "6 weeks",
    "8 weeks", "10 weeks", "12 weeks"
  )) %>%
  rename(traveltime = r3) %>%
  mutate(traveltime = fct_relevel(
    traveltime,
    "10 minutes", "20 minutes",
    "30 minutes", "40 minutes",
    "50 minutes", "60 minutes"
  )) %>%
  mutate(earn_ref = as.factor(case_when(
    str_detect(r4, " per year") ~ "Year",
    str_detect(r4, " per month") ~ "Month",
    str_detect(r4, " per week") ~ "Week",
    TRUE ~ as.character(r4)
  ))) %>%
  mutate(earn_ref = fct_relevel(earn_ref, "Week", "Month", "Year")) %>%
  mutate(r4 = str_remove(r4, "<b>")) %>%
  mutate(r4 = str_remove(r4, "</b>")) %>%
  mutate(r4 = str_remove(r4, " per year")) %>%
  mutate(r4 = str_remove(r4, " per month")) %>%
  mutate(r4 = str_remove(r4, " per week")) %>%
  rename(earn_increase = r4) %>%
  mutate(earn_increase = fct_relevel(
    earn_increase, "5% less",
    "0% more", "5% more",
    "10% more",
    "20% more", "30% more"
  )) %>%
  mutate(cj = 5) %>%
  arrange(uuid, tab, prof)

# Conjoint table 6 was dropped from the soft launch

# Conjoint Table 7 ------------------------------

cj7 <- conjoint_df %>%
  select(
    uuid,
    starts_with("hCon7Attr1"),
    starts_with("Q55_")
  ) %>%
  rename_at(
    .vars = vars(starts_with("hCon7Attr1_")),
    .funs = ~ sub("hCon7Attr1_", "tab", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("r1", "r1_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("r2", "r2_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("r3", "r3_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("r4", "r4_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("r5", "r5_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("Q55_")),
    .funs = ~ sub("Q55_", "tab", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("tab1c", "tab1rank_c", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("tab2c", "tab2rank_c", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab")),
    .funs = ~ sub("tab3c", "tab3rank_c", .)
  )

cj7_reflect <- conjoint_df %>%
  select(
    uuid,
    starts_with("Q56_"),
    starts_with("Q57_"),
    starts_with("Q58_"),
    starts_with("Q59_"),
  ) %>%
  pivot_longer(
    -uuid,
    names_to = c(".value", "tab"),
    names_sep = "_",
    values_drop_na = TRUE
  ) %>%
  rename(enroll = Q56) %>%
  rename(enroll_freetext = Q57) %>%
  rename(earnmore = Q58) %>%
  rename(earnmore_freetext = Q59) %>%
  mutate(cj = 7) %>%
  select(uuid, cj, tab, everything())

cj7_long <- cj7 %>%
  pivot_longer(
    -uuid,
    names_to = c(".value", "c"),
    names_sep = "_",
    values_drop_na = TRUE
  )  %>% 
  rename_at(
    .vars = vars(starts_with("tab1")),
    .funs = ~ sub("tab1", "tab1_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab2")),
    .funs = ~ sub("tab2", "tab2_", .)
  ) %>%
  rename_at(
    .vars = vars(starts_with("tab3")),
    .funs = ~ sub("tab3", "tab3_", .)
  ) %>%
  pivot_longer(starts_with("tab"),
    names_to = c("tab", ".value"),
    names_sep = "_",
    values_drop_na = TRUE
  ) %>%
  rename(prof = c) %>%
  select(uuid, tab, prof, everything()) %>%
  mutate_at(vars(r1, r2, r3, r4, r5), as.factor) %>%
  mutate(selected = if_else(rank == 1, 1, 0)) %>%
  rename(cost = r1) %>%
  mutate(cost = fct_relevel(
    cost,
    "$0", "$100", "$250", "$500",
    "$1,000", "$2,000", "$3,000",
    "$4,000", "$5,000", "$6,000",
    "$7,000", "$8,000", "$9,000",
    "$10,000", "$15,000", "$20,000"
  )) %>%
  rename(duration = r2) %>%
  mutate(duration = fct_relevel(
    duration,
    "2 weeks", "4 weeks", "6 weeks",
    "8 weeks", "10 weeks", "12 weeks"
  )) %>%
  rename(traveltime = r3) %>%
  mutate(traveltime = fct_relevel(
    traveltime,
    "10 minutes", "20 minutes",
    "30 minutes", "40 minutes",
    "50 minutes", "60 minutes"
  )) %>%
  mutate(earn_ref = as.factor(case_when(
    str_detect(r4, " per year") ~ "Year",
    str_detect(r4, " per month") ~ "Month",
    str_detect(r4, " per week") ~ "Week",
    TRUE ~ as.character(r4)
  ))) %>%
  mutate(earn_ref = fct_relevel(earn_ref, "Week", "Month", "Year")) %>%
  mutate(r4 = str_remove(r4, " per year")) %>%
  mutate(r4 = str_remove(r4, " per month")) %>%
  mutate(r4 = str_remove(r4, " per week")) %>%
  mutate(r4 = as.numeric(str_trim(str_squish(str_remove_all(r4, ","))))) %>%
  rename(earn_posttrain = r4) %>%
  rename(unemployment = r5) %>%
  mutate(unemployment = fct_relevel(
    unemployment, "0%", "5%", "10%",
    "15%", "20%", "25%"
  )) %>%
  mutate(cj = 7) %>%
  arrange(uuid, cj, tab, prof)


# Append all data frames for one dataset ------

cj_long <- bind_rows(
  cj1_long,
  cj2_long,
  cj3_long,
  cj4_long,
  cj5_long,
  cj7_long
) %>%
  mutate(
    tab = as.factor(str_remove(as.character(tab), "tab")),
    cost_int = as.numeric(str_trim(str_squish(str_replace_all(cost, c("\\$" = "", "," = ""))))) / 100,
    duration_int = as.numeric(str_trim(str_squish(str_remove(duration, "weeks")))),
    traveltime_int = as.numeric(str_trim(str_squish(str_remove(traveltime, "minutes")))),
    completionrate_int = as.numeric(str_trim(str_squish(str_remove(completionrate, "\\%")))),
    earn_posttrain_int = earn_posttrain,
    earn_posttrain = as.factor(earn_posttrain),
    earn_notrain_int = earn_notrain,
    earn_notrain = as.factor(earn_notrain),
    earn_increase_int = as.numeric(str_trim(str_squish(str_replace_all(earn_increase, c("% less" = "", "% more" = ""))))),
    unemployment_int = as.numeric(str_trim(str_squish(str_remove(unemployment, "%"))))) %>% 
  select(uuid, cj, tab, prof, rank, selected, everything()) %>%
  arrange(uuid, cj, tab, prof)

write_delim(cj_long, here("data", "conjoint_tables_long.txt"),  delim = "|")
write_dta(cj_long, here("data", "conjoint_tables_long.dta"), version = 15)

# Save data dictionary
cj_long %>% sjPlot::view_df(
  use.viewer = FALSE, show.frq = TRUE, show.prc = TRUE, show.na = TRUE,
  file = here("output", "conjoint_dictionary.html")
)

# Respondent reflection (text) to conjoint options
cj_reflect <- bind_rows(
  cj1_reflect,
  cj2_reflect,
  cj3_reflect,
  cj4_reflect,
  cj5_reflect,
  cj7_reflect
) %>%
  select(uuid, cj, tab, everything()) %>%
  arrange(uuid, cj, tab)

write_delim(cj_reflect, here("data", "conjoint_reflection_long.txt"),  delim = "|")
write_dta(cj_reflect, here("data", "conjoint_reflection_long.dta"), version = 15)

# Save data dictionary
cj_reflect %>% sjPlot::view_df(
  use.viewer = FALSE, show.frq = TRUE, show.prc = TRUE, show.na = TRUE,
  file = here("output", "conjoint_reflection_dictionary.html")
)


# Order of conjoint tabel elements
cj_order <- conjoint_df %>% 
    select(uuid, ends_with("OrdRecord"))

write_delim(cj_order, here("data", "conjoint_order.txt"),  delim = "|")
write_dta(cj_order, here("data", "conjoint_order.dta"), version = 15)

# Save data dictionary
cj_order %>% sjPlot::view_df(
  use.viewer = FALSE, show.frq = TRUE, show.prc = TRUE, show.na = TRUE,
  show.string.values = TRUE,
  file = here("output", "conjoint_order_data_dictionary.html")
)
