library(dplyr)
library(readr)

# Load Data ---------------------------------------------------------------

wp_id <- read_delim(paste0(dataPath, "/WagnerPeyser_ID.txt"),
                    delim = "|",
                    guess_max = 100000) %>%
  rename("state_id" = "STATE ID",
         "user_id" = "USER ID") %>%
  select(sirad_id,
         state_id,
         user_id) %>%
  filter(sirad_id != 0) %>%
  mutate(program = "Wagner Peyser")

# Get full list of IDs
id <- distinct(wp_id,
               sirad_id, 
               state_id)

# Write Output
write_csv(id, here::here("scratch/derived/wagner-peyser-id.csv"))
