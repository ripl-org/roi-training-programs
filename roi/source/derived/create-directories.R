# This script will create all necessary directories for RJRI Output

# Create scratch directory structure
if (!dir.exists(here::here("scratch"))) {
  dir.create(here::here("scratch"))
}
if (!dir.exists(here::here("scratch/derived"))) {
  dir.create(here::here("scratch/derived"))
}
if (!dir.exists(here::here("scratch/analysis"))) {
  dir.create(here::here("scratch/analysis"))
}
if (!dir.exists(here::here("scratch/analysis/dml"))) {
  dir.create(here::here("scratch/analysis/dml"))
}
if (!dir.exists(here::here("scratch/analysis/matching"))) {
  dir.create(here::here("scratch/analysis/matching"))
}

# Create results directory structure
if (!dir.exists(here::here("results"))) {
  dir.create(here::here("results"))
}
if (!dir.exists(here::here("results/dml"))) {
  dir.create(here::here("results/dml"))
}
if (!dir.exists(here::here("results/matching"))) {
  dir.create(here::here("results/matching"))
}