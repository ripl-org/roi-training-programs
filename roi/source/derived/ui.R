# Packages ----------------------------------------------------------------

library(dplyr)
library(fst)
library(lubridate)
library(readr)
library(stringr)

# Global Variables --------------------------------------------------------

infiles <- list.files(dataPath, full.names = TRUE)[
  str_detect(list.files(dataPath), 
             "UI.txt|UI_[0-9][0-9][0-9].txt")
]
outdir  <- here::here("scratch/derived")

# Data Cleaning -----------------------------------------------------------

ui <- bind_rows(lapply(infiles, function(f) {
  read_delim(f, 
             delim = "|",
             col_types = cols(.default = "c"))
})) %>%
  janitor::clean_names() %>%
  distinct() %>%
  mutate(claim_date_clean = ymd(claim_date),
         effective_date_clean = ymd(effective_date),
         claim_yyq = paste0(str_sub(year(claim_date_clean), 3, 4), 
                            quarter(claim_date_clean)),
         effective_yyq = paste0(str_sub(year(effective_date_clean), 3, 4), 
                                quarter(effective_date_clean)))

write_fst(ui, paste0(outdir, "/ui.fst"))