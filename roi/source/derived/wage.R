# Packages ----------------------------------------------------------------

library(readr)
library(dplyr)
library(fst)

# Global Variables --------------------------------------------------------

infiles <- list.files(dataPath, full.names = TRUE)[
  which(startsWith(list.files(dataPath), "Wage"))
]
outdir  <- here::here("scratch/derived")

# Data Cleaning -----------------------------------------------------------

wage <- bind_rows(lapply(infiles, function(f) {
  read_delim(f, "|", col_types="iiicc") %>%
    janitor::clean_names()
})) %>%
  distinct() %>%
  select(-record_id) %>%
  mutate(wages=as.integer(substr(wages, 1, 10)))

cat("Removing duplicate SIRAD_ID/ERN/YYQ records (for SIRAD_ID != 0)\n")
wage <- bind_rows(filter(wage, sirad_id > 0) %>% 
                    distinct(sirad_id, ern, yyq, .keep_all=TRUE),
                  filter(wage, sirad_id == 0))
cat("- Deduplicated rows:", nrow(wage), "\n")
cat("- Fraction with zero SIRAD_ID:", sum(wage$sirad_id == 0) / nrow(wage), "\n")

cat("Writing output\n")
write_fst(wage, paste0(outdir, "/wage.fst"))

cat("Done.\n")
