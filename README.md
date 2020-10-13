# Code and data for "Estimating the global reduction in transmission and rise in detection capacity of the novel coronavirus SARS-CoV-2 in early 2020"

This is the code and data used to generate the results in the paper **Estimating the global reduction in transmission and rise in detection capacity of the novel coronavirus SARS-CoV-2 in early 2020** by Antoine Belloir and François Blanquart, accessible at https://www.medrxiv.org/content/10.1101/2020.09.10.20192120v2


## *attempt_renewal_deaths_v4.R*

This the main code that performs the inference given the region (described by an index between 1 and 79 given in argument). It loads functions in *functions_renewal_deaths_v4.R*. The likelihood function *loglik_Rcpp* calls a small piece of C code *simulate.cpp* that performs fast simulations of the renewal equation.

## *functions_renewal_deaths_v4.R*

It contains the likelihood functions used for inference and other functions (for optimisation, MCMC, etc.). It also loads the epidemiological data and cleans it.

## *data/*
Contains the data. All data sources are detailed in the paper

### Epidemiological data:

*ECDC_data.csv* European data
*jh.csv* cleaned John Hopkins data
*italy.csv* Italian data
*chiffres-cles_France_opencovid19-fr.csv* French data
*covidtracking_USA_daily.csv* USA data

### Other data:

*Lockdown_dates - Sheet1.csv* lockdown dates
*Verity_IFR.csv* age-specific infection fatality ratio
*age_structure_all_clean.csv* age structure by region
*mobilities.csv* and *mobilities_data.csv* Google mobility data
*owid-covid-data.csv* test data
*states_abbreviation.csv* USA states abbreviations



 

