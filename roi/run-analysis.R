# Get environment variable that defines where flat files are
dataPath <- Sys.getenv("DATA_PATH")

# Create directory structure ----------------------------------------------

source(here::here("source/derived/create-directories.R"))

# Create Derived Tables ---------------------------------------------------

# Create Employer table
source(here::here("source/derived/employer.R"))

# Create UI table
source(here::here("source/derived/ui.R"))

# Create Wage Derived table
source(here::here("source/derived/wage.R"))
# Create Individual level Wage derived table
source(here::here("source/derived/wage-individual.R"))
# Create Lagged individual wage table
source(here::here("source/derived/wage-individual-lag.R"))


# Create Training Program Enrollee Tables ---------------------------------

# WIOA, and WP ID table
source(here::here("source/derived/wagner-peyser-id.R"))
# Full Training Program Enrollee Table
source(here::here("source/derived/training-program-enrollees.R"))
# Merge Training Program Enrollees with Wages
source(here::here("source/derived/training-program-enrollees-wage.R"))

# Run Matching Analysis ---------------------------------------------------

# Run matching estimator with $1000 wage band
source(here::here("source/analysis/matching/match.R"))
# Create table for making plotting event-study plots conducive
source(here::here("source/analysis/matching/plot-match.R"))

# Run DML Analysis --------------------------------------------------------

source(here::here("source/analysis/dml/dml.R"))
# shiny::runApp("results/matching/analysis-dashboard.R")