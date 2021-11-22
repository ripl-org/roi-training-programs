# Packages ----------------------------------------------------------------

library(readr)
library(data.table)
library(dplyr)
library(dtplyr)
library(fst)

# Global Variables --------------------------------------------------------

# Filepaths for employer files
infiles <- list.files(dataPath, full.names = TRUE)[
  which(startsWith(list.files(dataPath), "Employer"))
]
# Set output filepath
outdir  <- here::here("scratch/derived")

# Functions ---------------------------------------------------------------

# Mode function (returns a scalar)
mode <- function(x,
                 na.rm = TRUE,
                 all.modes = FALSE) {
  if (any(!is.na(x))) {
    if(na.rm == TRUE) {
      ux <- unique(x)[!is.na(unique(x))]
    } else {
      ux <- unique(x)
    }
  } else {
    ux <- NA
  }
  modes <- which(tabulate(match(x, ux)) == max(tabulate(match(x, ux))))
  if (all.modes == FALSE) {
    return(ux[modes[[1]]])
  } else {
    return(ux[modes])
  }
}

# Clean Data --------------------------------------------------------------

employer <- bind_rows(lapply(infiles, function(f) {
  read_delim(f, "|", col_types="cicicici") %>%
    janitor::clean_names()
})) %>%
  distinct() %>%
  select(-record_id) %>%
  distinct(ern, yyq, .keep_all=TRUE)

cat("Replacing 0 and 99 NAICS codes with NA\n")
cat("- NAs before:", sum(is.na(employer$naics4)), "\n")
employer <- mutate(employer, naics4=ifelse(naics4 == "9999" | startsWith(naics4, "0"), NA, naics4))
cat("- NAs after:", sum(is.na(employer$naics4)), "\n")

cat("Generating NAICS and converting factors\n")
employer <- mutate(employer,
                   naics2=as.factor(substr(naics4, 1, 2)),
                   naics4=as.factor(naics4))

cat("Generating modal NAICS2 and NAICS4\n")
industry <- employer %>%
            group_by(ern) %>%
            summarise(naics2_modal=mode(naics2),
                      naics4_modal=mode(naics4))
employer <- left_join(employer, industry, by="ern")

cat("Generating normalized industry\n")
employer <- mutate(employer,
                   industry_modal=case_when(naics2_modal == "11" ~ "Agriculture/Mining",
                                            naics2_modal == "21" ~ "Agriculture/Mining",
                                            naics2_modal == "22" ~ "Utilities",
                                            naics2_modal == "23" ~ "Construction",
                                            naics2_modal == "31" ~ "Manufacturing",
                                            naics2_modal == "32" ~ "Manufacturing",
                                            naics2_modal == "33" ~ "Manufacturing",
                                            naics2_modal == "42" ~ "Wholesale",
                                            naics2_modal == "44" ~ "Retail",
                                            naics2_modal == "45" ~ "Retail",
                                            naics2_modal == "48" ~ "Transportation",
                                            naics2_modal == "49" ~ "Transportation",
                                            naics2_modal == "51" ~ "Information",
                                            naics2_modal == "52" ~ "Finance",
                                            naics2_modal == "53" ~ "Real Estate",
                                            naics2_modal == "54" ~ "Professional",
                                            naics2_modal == "55" ~ "Management",
                                            naics2_modal == "56" ~ "Administrative",
                                            naics2_modal == "61" ~ "Education",
                                            naics2_modal == "62" ~ "Health",
                                            naics2_modal == "71" ~ "Arts/Entertainment",
                                            naics2_modal == "72" ~ "Food/Hospitality",
                                            naics2_modal == "81" ~ "Service",
                                            TRUE                 ~ "Unknown"))

cat("Renaming and reordering columns\n")
employer <- rename(employer,
                   blockgroup=employer_blkgrp) %>%
            select(ern, 
                   yyq, 
                   naics2, 
                   naics4,
                   naics2_modal,
                   naics4_modal, 
                   industry_modal,
                   employee_count,
                   blockgroup)

cat("Writing output\n")
write_fst(employer, paste0(outdir, "/employer.fst"))

cat("Done.\n")
