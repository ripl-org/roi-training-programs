# Packages ----------------------------------------------------------------

library(data.table)
library(fst)
library(dplyr)
library(dtplyr)
library(tidyr)

# Global Variables --------------------------------------------------------

wagefile <- here::here("scratch/derived/wage.fst")
empfile  <- here::here("scratch/derived/employer.fst")
outdir   <- here::here("scratch/derived")

# Clean Data --------------------------------------------------------------

cat("Loading Wage\n")
wage <- read_fst(wagefile)
cat("- Raw rows:", nrow(wage), "\n")

cat("Filtering on SIRAD != 0 and between YYQ 151 and 194\n")
wage <- filter(wage, 
               sirad_id != 0,
               yyq %>% between(151, 194))
cat("- Rows:", nrow(wage), "\n")

cat("Loading Employer\n")
employer <- read_fst(empfile, 
                     columns=c("ern", 
                               "yyq",
                               "naics4_modal", 
                               "naics2_modal",
                               "industry_modal", 
                               "employee_count"))
cat("- Raw rows:", nrow(employer), "\n")

cat("Joining Employer to Wage\n")
wage <- left_join(wage, employer, by=c("ern", "yyq"))

cat("Sorting on SIRAD_ID/YYQ/WAGES\n")
wage <- arrange(wage, sirad_id, yyq, wages)

cat("Grouping by SIRAD_ID/YYQ\n")
wage <- wage %>%
        lazy_dt() %>%
        group_by(sirad_id, yyq) %>%
        summarise(wages=sum(wages),
                  ern=last(ern),
                  naics4=last(naics4_modal),
                  naics2=last(naics2_modal)) %>%
        as_tibble()

cat("Writing output\n")
write_fst(wage, paste0(outdir, "/wage-individual.fst"))

cat("Done.\n")
