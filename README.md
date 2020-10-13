# Description of code and data

This is the code and data used to generate the results in the paper **Estimating the global reduction in transmission and rise in detection capacity of the novel coronavirus SARS-CoV-2 in early 2020** by Antoine Belloir and François Blanquart, accessible at https://www.medrxiv.org/content/10.1101/2020.09.10.20192120v2

## *data/*
Contains the data. All data sources are detailed in the manuscript.

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


## *attempt_renewal_deaths_v4.R*

This the main code that performs the inference given the region (described by an index between 1 and 79 given in argument). It loads functions in *functions_renewal_deaths_v4.R*. The likelihood function *loglik_Rcpp* calls a small piece of C code *simulate.cpp* that performs fast simulations of the renewal equation. For the region, this code returns a compressed .RData file containing all information on the parameter inference.

## *functions_renewal_deaths_v4.R*

It contains the likelihood functions used for inference and other functions (for optimisation, MCMC, etc.). It also loads the epidemiological data and cleans it.

## *analyse_results.R* and *analyse_results_functions.R*

Code to analyse the results of the inference from the 79 regions. For each region it loads the .RData file (produced by *attempt_renewal_deaths_v4.R*) and creates a table *all_pars.csv* containing all results. It also creates various figures.

## *all_pars.csv*

Table containing all results for each region. Each line is a region.

## *figureS7_merged.pdf*

Figure showing the fit of three models for each region. The first model inferred parameters based on daily deaths count only and therefore did not infer the probability of detection. The second model inferred parameters based on joint daily case counts and deaths and inferred a sharp reduction in transmissibility (one value pre- and one value post- lockdown). The third model inferred parameters based on joint daily case counts and deaths and inferred a smooth reduction in transmissibility (sigmoid).

The inset shows the inferred relative transmissibility (red curve) and detection probability (black curve).

Confidence intervals are generated by re-simulating the model under parameters sampled from the MCMC run.

## *validate_Austria_NY.R*

Code to produce estimates of the attack rate under the best inferred parameters, for each region where seroprevalence surveys are available (Figure 1 of the paper).

## *figureS2_sharp_smooth_reduction_comparison.pdf*

Figure showing the inferred reduction in transmissibility under the sharp reduction in transmissibility and smooth reduction in transmissibility, for those countries where the fit of both models was similar. In other countries, the smooth reduction model was a better fit.

## *clean_John_Hopkins.R*

Code to clean the John Hopkins data






 

