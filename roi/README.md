# Causal estimates of return-on-investment to labor training programs

Analysis code for estimating value-added returns to Real Jobs RI labor training program, as presented in the manuscript:

Angell M, Gold S, Hastings JS, *et al*. 2021. Estimating Value-added Returns to Labor Training Programs with Causal Machine Learning. *OSF Preprints*: thg23. doi:[https://doi.org/10.31219/osf.io/thg23](10.31219/osf.io/thg23)

## Data

Data are available from the [Rhode Island Department of Labor and Training](https://dlt.ri.gov/) (RIDLT) under a data use agreement.

## Setup

After executing a data use agreement with RIDLT, the data are available to approved users in RIDLT's Research Data Lake. For more information on the architecture and use of that system, see:

Howison M, Angell M, Hicklen MS, Hastings JS. 2021. Protecting Sensitive Data with Secure Data Enclaves. *OSF Preprints*: jmd7t. doi:[https://doi.org/10.31219/osf.io/jmd7t](10.31219/osf.io/jmd7t)

## Run

Set the environment variable `DATA_PATH` to the path to the input flatfiles in the RIDLT Research Data Lake.

The analysis can be automatically run with `Rscript run-analysis.R`.
