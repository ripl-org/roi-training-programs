library(data.table)
library(dtplyr)
library(dplyr)
library(lubridate)
library(readr)
library(stringr)
library(tidyr)

# Load Data ---------------------------------------------------------------

# Wagner Peyser ID file
wp_id <- read_csv(here::here("scratch/derived/wagner-peyser-id.csv"))

# Wagner Peyser Data
wp <- read_delim(paste0(dataPath, "/WagnerPeyser.txt"), 
                 delim = "|", 
                 guess_max = 100000) %>%
      rename(state_id        = `Individual Identifier`,
             gender          = Gender,
             disability      = `Individual with a Disability`,
             native_american = `Race: American Indian or Alaskan Native`,
             asian           = `Race: Asian`,
             black           = `Race: Black or African American`,
             pacific         = `Race: Native Hawaiian or other Pacific Islander`,
             white           = `Race: White`,
             multiracial     = `More Than One Race`,
             hispanic_latino = `Ethnicity Hispanic/Latino`,
             veteran         = `Veteran Status`,
             emp_at_part     = `Employment Status at Participation`,
             education       = `Highest School Grade Completed`,
             intensive_date  = `Most Recent Date Received Intensive Services`,
             training_date   = `Most Recent Date Placed in Federal Training`,
             exit_date       = `Date of Exit`) %>%
      select(state_id,
             gender,
             disability,
             native_american,
             asian,
             black,
             pacific,
             white,
             multiracial,
             hispanic_latino,
             veteran,
             emp_at_part,
             education,
             intensive_date, 
             training_date,
             exit_date) %>%
       mutate(native_american = case_when(native_american == "1" ~ "Y",
                                          TRUE                   ~ "N"),
              asian           = case_when(asian == "1" ~ "Y",
                                          TRUE         ~ "N"),
              black           = case_when(black == "1" ~ "Y",
                                          TRUE         ~ "N"),
              pacific         = case_when(pacific == "1" ~ "Y",
                                          TRUE           ~ "N"),
              white           = case_when(white == "1" ~ "Y",
                                          TRUE         ~ "N"),
              multiracial     = case_when(multiracial == "1" ~ "Y",
                                          TRUE               ~ "N"),
              gender          = case_when(gender == "1" ~ "M",
                                          gender == "2" ~ "F",
                                          TRUE          ~ "U"),
              disability      = case_when(disability == "1" ~ "Y",
                                          disability == "2" ~ "N",
                                          TRUE              ~ "U"),
              hispanic_latino = case_when(hispanic_latino == "1" ~ "Y",
                                          hispanic_latino == "2" ~ "N",
                                          TRUE                   ~ "U"),
              veteran         = case_when(veteran == "1" ~ "Y",
                                          veteran == "2" ~ "N",
                                          TRUE           ~ "U"),
              emp_at_part     = case_when(emp_at_part == "1" ~ "Employed",
                                          emp_at_part == "2" ~ "Terminated",
                                          emp_at_part == "3" ~ "Unemployed",
                                          TRUE               ~ "Unknown"),
              education       = case_when(education == "00"                            ~ "No education",
                                          education %in% str_pad(1:12, 2, "left", "0") ~ "Less Than HS",
                                          education %in% c("87", "88")                 ~ "HS Equivalent",
                                          education == "89"                            ~ "Certificate of Attendance Completion",
                                          education == "90"                            ~ "Post-Secondary Degree or Certification",
                                          education == "91"                            ~ "Associates Degree",
                                          education %in% c("13", "14", "15")           ~ "Some College",
                                          education == "16"                            ~ "Bachelors Degree",
                                          education == "17"                            ~ "Advanced Degree",
                                          TRUE                                         ~ "Unknown"))
cat("Rows:", n_wp <- nrow(wp), "\n")

cat("Grouping Wagner-Peyser at the individual level\n")
wp <- wp %>%
      filter(!is.na(state_id)) %>% # remove 13 obs with missing state ids
      mutate(intensive_date = ymd(intensive_date),
             training_date  = ymd(training_date),
             exit_date      = ymd(exit_date)) %>%
      group_by(state_id) %>%
      mutate(treat_control   = case_when(any(!is.na(intensive_date)) ~ 1,
                                         TRUE                        ~ 0),
             treatment       = case_when(treat_control == 1 &
                                         any(!is.na(training_date)) ~ "Treatment",
                                         treat_control == 1         ~ "Control"),        
             veteran         = case_when(any(veteran == "Y") ~ "Y",
                                         any(veteran == "N") ~ "N",
                                         TRUE                ~ "U"),
             disability      = case_when(any(disability == "Y") ~ "Y",
                                         any(disability == "N") ~ "N",
                                         TRUE                   ~ "U"),
             gender          = case_when(any(gender == "M") ~ "M",
                                         any(gender == "F") ~ "F",
                                         TRUE               ~ "U"),
             hispanic_latino = case_when(any(hispanic_latino == "Y") ~ "Y",
                                         any(hispanic_latino == "N") ~ "N",
                                         TRUE                        ~ "U")) %>%
      filter(treat_control == 1) %>%
      mutate(max_training_date = case_when(any(treatment == "Treatment") ~ as.Date(max(training_date, na.rm = TRUE), origin = "1970-01-01"),
                                               TRUE                      ~ as.Date(NA))) %>%
      filter(!is.na(intensive_date) &
                (intensive_date <= max_training_date |
                is.na(max_training_date))) %>%
      mutate(max_intensive_date = as.Date(max(intensive_date), origin = "1970-01-01"),
             max_exit_date      = case_when(any(!is.na(exit_date)) ~ as.Date(max(exit_date, na.rm = TRUE), origin = "1970-01-01"),
                                            TRUE                   ~ as.Date(NA))) %>%
      filter(intensive_date == max_intensive_date) %>%
      ungroup() %>%
      mutate(program = "WP") %>%
      distinct(state_id, intensive_date, .keep_all = TRUE)

cat("Set common variable names\n")
wp <- wp %>%
      mutate(training_comp_date = max_exit_date,
             training_date      = max_training_date,
             max_training_date  = NULL,
             max_exit_date      = NULL,
             max_intensive_date = NULL) %>%
      inner_join(wp_id, by = "state_id") %>%
      select(sirad_id, everything(), -state_id)
cat("Rows:", nrow(wp), "\n")
cat("Rows lost by join:", n_wp - nrow(wp), "\n")

cat("Loading WIA\n")
wia <- read_delim(paste0(dataPath, "/WIASRD.txt"), 
                  delim = "|", 
                  guess_max = 100000) %>%
       rename(state_id           = `100 - Unique Individual Identifier`,
              veteran            = `300 - Veteran Status`,
              intensive_date     = `1200 - Date of First Intensive Service`,
              training_date      = `1208 - Date Entered Training 1`,
              training_comp_date = `1212 - Date Completed, or Withdrew from, Training \\1`,
              gender             = `201 - Gender`,
              disability         = `202 - Individual with a Disability`,
              native_american    = `205 - American Indian / Alaskan Native`,
              asian              = `206 - Asian`,
              black              = `207 - Black / African American`,
              pacific            = `208 - Native Hawaiian / Other Pacific Islander`,
              white              = `209 - White`,
              hispanic_latino    = `204 - Ethnicity Hispanic / Latino`,
              emp_at_part        = `400 - Employment Status at Participation`,
              education          = `410 - Highest School Grade Completed`,
              participation_date = `900 - Date of Participation/Date of First Case Management & Reemployment Service`,
              exit_date          = `901 - Date of Exit`) %>%
       select(state_id,
              gender,
              disability,
              native_american,
              asian,
              black,
              pacific,
              white,
              hispanic_latino,
              veteran,
              emp_at_part,
              education,
              intensive_date, 
              training_date,
              exit_date,
              training_comp_date) %>%
       mutate(multiracial = (as.numeric(replace_na(native_american, 0)) +
                as.numeric(replace_na(asian, 0)) +
                as.numeric(replace_na(black, 0)) +
                as.numeric(replace_na(pacific, 0)) +
                as.numeric(replace_na(white, 0))) > 1,
              multiracial = case_when(multiracial == TRUE ~ "Y",
                                          TRUE            ~ "N"),
              native_american = case_when(native_american == "1" ~ "Y",
                                          native_american == "0" ~ "N",
                                          TRUE                   ~ "U"),
              asian           = case_when(asian == "1" ~ "Y",
                                          asian == "0" ~ "N",
                                          TRUE         ~ "U"),
              black           = case_when(black == "1" ~ "Y",
                                          black == "0" ~ "N",
                                          TRUE         ~ "U"),
              pacific         = case_when(pacific == "1" ~ "Y",
                                          pacific == "0" ~ "N",
                                          TRUE           ~ "U"),
              white           = case_when(white == "1" ~ "Y",
                                          white == "0" ~ "N",
                                          TRUE         ~ "U"),
              gender          = case_when(gender == "1" ~ "M",
                                          gender == "2" ~ "F",
                                          TRUE          ~ "U"),
              disability      = case_when(disability == "1" ~ "Y",
                                          disability == "0" ~ "N",
                                          TRUE              ~ "U"),
              hispanic_latino = case_when(hispanic_latino == "1" ~ "Y",
                                          hispanic_latino == "2" ~ "N",
                                          TRUE                   ~ "U"),
              veteran         = case_when(veteran == "1" ~ "Y",
                                          veteran == "2" ~ "N",
                                          TRUE           ~ "U"),
              emp_at_part     = case_when(emp_at_part == "1" ~ "Employed",
                                          emp_at_part == "2" ~ "Terminated",
                                          emp_at_part == "0" ~ "Unemployed",
                                          TRUE               ~ "Unknown"),
              education       = case_when(education == "00"                            ~ "No education",
                                          education %in% str_pad(1:12, 2, "left", "0") ~ "Less Than HS",
                                          education %in% c("87", "88")                 ~ "HS Equivalent",
                                          education == "89"                            ~ "Certificate of Attendance Completion",
                                          education == "90"                            ~ "Post-Secondary Degree or Certification",
                                          education == "91"                            ~ "Associates Degree",
                                          education %in% c("13", "14", "15")           ~ "Some College",
                                          education == "16"                            ~ "Bachelors Degree",
                                          education == "17"                            ~ "Advanced Degree",
                                          TRUE                                         ~ "Unknown"))

cat("Grouping WIA at the individual level\n")
wia <- wia %>%
       filter(!is.na(state_id)) %>%
       mutate(intensive_date     = ymd(intensive_date),
              training_date      = ymd(training_date),
              training_comp_date = ymd(training_comp_date),
              exit_date          = ymd(exit_date)) %>%
       group_by(state_id) %>%
       mutate(treat_control   = case_when(any(!is.na(intensive_date)) ~ 1,
                                          TRUE                        ~ 0),
              treatment       = case_when(treat_control == 1 &
                                          any(!is.na(training_date)) ~ "Treatment",
                                          treat_control == 1         ~ "Control"),
              veteran         = case_when(any(veteran == "Y") ~ "Y",
                                          any(veteran == "N") ~ "N",
                                          TRUE                ~ "U"),
              disability      = case_when(any(disability == "Y") ~ "Y",
                                          any(disability == "N") ~ "N",
                                          TRUE                   ~ "U"),
              gender          = case_when(any(gender == "M") ~ "M",
                                          any(gender == "F") ~ "F",
                                          TRUE               ~ "U"),
              hispanic_latino = case_when(any(hispanic_latino == "Y") ~ "Y",
                                          any(hispanic_latino == "N") ~ "N",
                                          TRUE                        ~ "U")) %>%
       filter(treat_control == 1) %>%
       mutate(max_training_date      = case_when(any(treatment == "Treatment") ~ as.Date(max(training_date, na.rm = TRUE), origin = "1970-01-01"),
                                                 TRUE                          ~ as.Date(NA)),
              max_training_comp_date = case_when(any(treatment == "Treatment") ~ as.Date(max(training_comp_date, na.rm = TRUE), origin = "1970-01-01"),
                                                 TRUE                          ~ as.Date(NA))) %>%
       filter(!is.na(intensive_date) &
                (intensive_date <= max_training_date |
                is.na(max_training_date))) %>%
       mutate(max_intensive_date = as.Date(max(intensive_date), origin = "1970-01-01")) %>%
       filter(intensive_date == max_intensive_date) %>%
       ungroup() %>%
       mutate(program = "WIA") %>%
       distinct(state_id, intensive_date, .keep_all = TRUE)
cat("Rows:", n_wia <- nrow(wia), "\n")

cat("Set common variable names\n")
wia <- wia %>%
       mutate(training_comp_date     = max_training_comp_date,
              training_date          = max_training_date,
              max_training_date      = NULL,
              max_training_comp_date = NULL,
              max_intensive_date     = NULL) %>%
       inner_join(wp_id, by = "state_id") %>%
       select(sirad_id, everything(), -state_id)
cat("Rows:", nrow(wia), "\n")
cat("Rows lost by join:", n_wia - nrow(wia), "\n")

cat("Loading WIOA\n")
wioa <- read_delim(paste0(dataPath, "/WIOA.txt"), 
                   delim = "|", 
                   guess_max = 100000) %>%
        rename(state_id       = STATEID,
               program_name   = PROVNAME1) %>%
        mutate(intensive      = as.integer(startsWith(COL_ACTCODE, "2")),
               training       = as.integer(startsWith(COL_ACTCODE, "3")),
               intensive_date = case_when(intensive == 1 ~ ymd(COL_ACTBEGINDATE),
                                          TRUE           ~ as.Date(NA)),
               training_date  = case_when(training == 1 ~ ymd(COL_ACTBEGINDATE),
                                          TRUE          ~ as.Date(NA))) %>%
        mutate_at("program_name", ~ replace(., . == "Go-Live Generic Provider", NA)) %>%
        select(state_id, intensive_date, training_date, program_name)
cat("Rows:", nrow(wioa), "\n")

### Cleaning up here ###

cat("Grouping WIOA at the individual level\n")
wioa <- wioa %>%
        filter(!is.na(state_id)) %>%
        group_by(state_id) %>%
        mutate(treat_control = case_when(any(!is.na(intensive_date)) ~ 1,
                                         TRUE                        ~ 0),
               treatment     = case_when(treat_control == 1 &
                                         any(!is.na(training_date)) ~ "Treatment",
                                         treat_control == 1         ~ "Control")) %>%
        filter(treat_control == 1) %>%
        mutate(max_training_date = case_when(!all(is.na(training_date))  ~ as.Date(max(training_date, na.rm = TRUE), origin = "1970-01-01"),
                                             TRUE                        ~ as.Date(NA)),
               max_program_name  = case_when(training_date == max_training_date ~ program_name,
                                             TRUE ~ NA_character_)) %>%
        fill(max_program_name, .direction = "updown") %>%
        filter(!is.na(intensive_date) &
                 (intensive_date <= max_training_date |
                 is.na(max_training_date))) %>%
        mutate(max_intensive_date = as.Date(max(intensive_date), origin = "1970-01-01")) %>%
        filter(intensive_date == max_intensive_date) %>%
        ungroup() %>%
        mutate(program = "WIOA") %>%
        distinct(state_id, intensive_date, .keep_all = TRUE)
cat("Rows:", n_wioa <- nrow(wioa), "\n")

cat("Set common variable names\n")
wioa <- wioa %>%
        mutate(training_date          = max_training_date,
               program_name           = max_program_name,
               max_training_date      = NULL,
               max_intensive_date     = NULL,
               max_program_name       = NULL) %>%
        inner_join(wp_id, by = "state_id") %>%
        select(sirad_id, everything(), -state_id)
cat("Rows:", nrow(wioa), "\n")
cat("Rows lost by join:", n_wioa - nrow(wioa), "\n")
cat("Number of distinct Treatment Individuals:",
    nrow(wp %>% 
         filter(treatment == "Treatment", 
                !sirad_id %in% c(wia$sirad_id, wioa$sirad_id)) %>% 
         distinct(sirad_id)) +
      nrow(wia %>% 
           filter(treatment == "Treatment",
                  !sirad_id %in% wioa$sirad_id) %>% 
           distinct(sirad_id)) +
      nrow(wioa %>% filter(treatment == "Treatment") %>% 
           distinct(sirad_id)), "\n")

cat("Joining demographic features for WIOA")
dems_wp <- wp %>% 
  select(sirad_id, 
         gender,
         disability, 
         native_american, 
         asian, 
         black, 
         pacific, 
         white, 
         multiracial, 
         hispanic_latino, 
         veteran, 
         emp_at_part, 
         education
        )
dems_wia <- wia %>% 
  rename(gender_wia           = gender, 
         disability_wia       = disability, 
         native_american_wia  = native_american, 
         asian_wia            = asian, 
         black_wia            = black, 
         pacific_wia          = pacific, 
         white_wia            = white, 
         multiracial_wia      = multiracial, 
         hispanic_latino_wia  = hispanic_latino, 
         veteran_wia          = veteran, 
         emp_at_part_wia      = emp_at_part, 
         education_wia        = education) %>% 
  select(sirad_id, ends_with("wia"))

wioa <- left_join(wioa, dems_wp, by = "sirad_id")
na_wp <- length(which(is.na(wioa$gender)))/nrow(wioa) #Using gender to measure NAs,
                                                      #number will be the same if you were 
                                                      #any other demographic var. 

wioa <- left_join(wioa, dems_wia, by = "sirad_id")
wioa <- wioa %>% 
  mutate(gender           = coalesce(gender, gender_wia), 
         disability       = coalesce(disability, disability_wia), 
         native_american  = coalesce(native_american, native_american_wia), 
         asian            = coalesce(asian, asian_wia), 
         black            = coalesce(black, black_wia), 
         white            = coalesce(white, white_wia), 
         pacific          = coalesce(pacific, pacific_wia), 
         multiracial      = coalesce(multiracial, multiracial_wia), 
         hispanic_latino  = coalesce(hispanic_latino, hispanic_latino_wia), 
         veteran          = coalesce(veteran, veteran_wia), 
         emp_at_part      = coalesce(emp_at_part, emp_at_part_wia), 
         education        = coalesce(education, education_wia)
        ) %>% 
  select(-ends_with("wia")) 
cat("% NAs WP - ", na_wp, "%/n")
cat("% NAs WIA - ", length(which(is.na(wioa$gender)))/nrow(wioa), "%/n")

wioa <- wioa %>% 
  mutate_at(vars(8:19), replace_na, "U") #Replacing NAs with "U" to 
                                         #maintain consistency throughout 
                                         #the derived table. 

cat("Merge WP, WIA, and WIOA and coalesce columns\n")
wp_wia_wioa <- bind_rows(wp %>% filter(!sirad_id %in% c(wia$sirad_id, wioa$sirad_id)), 
                         wia %>% filter(!sirad_id %in% wioa$sirad_id),
                         wioa) %>%
               rename(training_program = program_name) %>%
               mutate(yyq = case_when(treatment == "Treatment" ~ paste0(str_sub(year(training_date), 3, 4),
                                                                        quarter(training_date)),
                                      treatment == "Control" ~ paste0(str_sub(year(intensive_date), 3, 4),
                                                                      quarter(intensive_date)))) %>%
               select(-c(intensive_date, 
                         training_date,
                         exit_date, 
                         treat_control,
                         training_program,
                         program,
                         training_comp_date,
                         disability,
                         emp_at_part,
                         veteran))
cat("Rows:", nrow(wp_wia_wioa), "\n")
cat("Number of Treatment Individuals after binding columns:", 
    nrow(wp_wia_wioa %>% filter(treatment == "Treatment")), "\n")

cat("Loading Real Jobs\n")
real <- read_delim(paste0(dataPath, "/REAL.txt"), 
                   delim = "|", 
                   guess_max = 100000) %>%
        janitor::clean_names() %>%
        rename("training_program"   = "activity_name",
               "partnership"        = "partnership_name",
               "training_date"      = "target_start_date",
               "training_comp_date" = "target_end_date",
               "program"            = "program_name",
               "gender"             = "participant_gender",
               "race"               = "participant_race",
               "hispanic_latino"    = "participant_ethnicity",
               "education"          = "participant_education") %>%
        mutate(gender          = case_when(gender == "FEMALE" ~ "F",
                                           gender == "MALE"   ~ "M",
                                           TRUE               ~ "U"),
               white           = case_when(race == "WHITE" ~ "Y",
                                           TRUE            ~ "N"),
               black           = case_when(race == "BLACK" ~ "Y",
                                           TRUE            ~ "N"),
               pacific         = case_when(race == "ISLANDER" ~ "Y",
                                           TRUE               ~ "N"),
               asian           = case_when(race == "ASIAN" ~ "Y",
                                           TRUE            ~ "N"),
               native_american = case_when(race == "INDIGENOUS" ~ "Y",
                                           TRUE                 ~ "N"),
               multiracial     = case_when(str_detect(race, ",") ~ "Y",
                                           TRUE                  ~ "N"),
               education       = case_when(education == "NO_HIGHSCHOOL"           ~ "Less Than HS",
                                           education %in% c("GED", "HIGH_SCHOOL") ~ "HS Equivalent",
                                           education == "VOCATIONAL"              ~ "Post-Secondary Degree or Certification",
                                           education == "ASSOCIATE"               ~ "Associates Degree",
                                           education == "SOME_COLLEGE"            ~ "Some College",
                                           education == "BACHELORS"               ~ "Bachelors Degree",
                                           education %in% c("MASTERS", "PHD")     ~ "Advanced Degree",
                                           TRUE                                   ~ "Unknown"),
               hispanic_latino = case_when(hispanic_latino == "NOT-LATINO" ~ "N",
                                           hispanic_latino == "LATINO"     ~ "Y",
                                           TRUE                            ~ "U")) %>%
        select(training_program,
               partnership,
               training_date,
               training_comp_date,
               program,
               gender,
               white,
               black,
               pacific,
               asian,
               native_american,
               multiracial,
               hispanic_latino,
               education,
               employri_id)
cat("Rows:", nrow(real), "\n")
cat("Distinct SIRAD IDs:", 
    n_real <- nrow(distinct(real %>% filter(!is.na(training_date)))), 
    "\n")

cat("Loading Real_ID table\n")
real_id <- read_delim(paste0(dataPath, "/Real_ID.txt"),
                      "|",
                      guess_max = 100000) %>%
  janitor::clean_names() %>%
  select(-record_id)
cat("- Rows:", nrow(real), "\n")

cat("Merging Real and Real_ID\n")
real <- inner_join(
  real,
  real_id,
  by = c("employri_id" = "employri_user_id")
)
cat("Number of rows lost in join:", n_real - nrow(real), "\n")


cat("Group RJRI by individual\n")
rjri <- real %>%
        filter(sirad_id != "0", program == "REAL_JOBS") %>%
        mutate(program = "RJRI") %>%
        group_by(sirad_id) %>%
        mutate(treatment = case_when(any(!is.na(training_date)) ~ "Treatment")) %>%
        mutate(max_training_date      = case_when(!all(is.na(training_date))      ~ as.Date(max(training_date, na.rm = TRUE), origin = "1970-01-01"),
                                                  TRUE                            ~ as.Date(NA)),
               max_training_comp_date = case_when(!all(is.na(training_comp_date)) ~ as.Date(max(training_comp_date, na.rm = TRUE), origin = "1970-01-01"),
                                                  TRUE                            ~ as.Date(NA))) %>%
        filter(training_date == max_training_date) %>%
        ungroup() %>%
        mutate(program = "RJRI") %>%
        distinct(sirad_id, training_date, .keep_all = TRUE)

cat("Set common variable names\n")
rjri <- rjri %>%
        mutate(training_date          = max_training_date,
               training_comp_date     = max_training_comp_date,
               max_training_date      = NULL,
               max_training_comp_date = NULL,
               yyq = paste0(str_sub(year(training_date), 3, 4),
                            quarter(training_date))) %>%
        select(-c(training_date,
                  training_comp_date,
                  program,
                  employri_id,
                  treatment))

# cat("Bind RJRI and RPRI to WP, WIA, WIOA\n")
# all_programs <- bind_rows(wp_wia_wioa %>% 
#                             lapply(as.character) %>% 
#                             bind_cols() %>%
#                             filter(!sirad_id %in% rjri$sirad_id),
#                           rjri %>% lapply(as.character) %>% bind_cols())
# cat("Rows:", nrow(all_programs), "\n")

cat("Writing output\n")
write_csv(filter(wp_wia_wioa, !sirad_id %in% rjri$sirad_id),
          here::here("scratch/derived/wioa-wp-enrollees.csv"))
write_csv(rjri, here::here("scratch/derived/rjri-enrollees.csv"))
cat("Done.\n")