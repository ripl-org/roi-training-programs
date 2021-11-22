# Survey analysis

Analysis code from the conjoint experiment for labor training preferences presented in the manuscript:

Angell M, Gold S, Hastings JS, *et al*. 2021. Estimating Value-added Returns to Labor Training Programs with Causal Machine Learning. *OSF Preprints*: thg23. doi:[https://doi.org/10.31219/osf.io/thg23](10.31219/osf.io/thg23)

## Data

The original survey results file `ORD-461143-V3R5_Final_Excel_Completes_042120.xlsx` may be requested from the survey provider, [Dynata](https://www.dynata.com/company/contact/). Place in the `data` subdirectory.

## Setup

### R

Code tested with Microsoft R Open version 4.0.2 on Windows 10 with the following packages installed:

    install.packages(c("here", "tidyverse", "haven", "sjPlot", "broom", "hrbrthemes", "compareGroups", "ggthemes", "readxl"))

### Stata

Code tested with Stata MP 16.1 on Windows 10 with the following packages installed:

    ssc install blindschemes
    ssc install estout

Change the working directory to the this `survey` subdirectory of the repo.

## Order of analysis files to run

1. `source/clean_data.R` - constructs analysis files
2. `source/summary_stats.R` - descriptive statistics for survey respondents
3. `source/conjoint_avg_elasticities.do` - conjoint choice models for full sample
4. `source/conjoint_avg_elasticities_women.do` - conjoint choice models for female sample
5. `source/conjoint_avg_elasticities_men.do` - conjoint choice models for male sample
6. `source/conjoint_avg_elasticities_below10k_hs.do` - conjoint choice models for sample less than $10k income and high school degree or less education
7. `source/conjoint_analysis.R` - descriptive analysis of conjoint design
