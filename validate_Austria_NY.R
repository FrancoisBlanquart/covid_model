rm(list = ls())
get_best_mcmc <- function(mm){
  #print(max(mm$all.lik))
  return(mm$chain[which.max(mm$all.lik), ])
}
get_sample_mcmc <- function(mm, myn = 200){
  n <- length(mm$all.lik)
  tokeep <- round(0.2*n):n
  mysamp <- sample(tokeep, size = myn, replace = F)
  return(mm$chain[mysamp, ])
}
get_frac <- function(par, betafun, type, dd){
  # par = parameters
  # betafun = functional form for transmission
  # type = PCR tests or serological
  # dd = date
  mypred <- loglik_Rcpp(par = par, epidata = sub_tab, datelockdown = datelockdown, tmax = tmax, main = mycountry, with_detected = T, use_tests = F, total_pop = mytotal_pop, betafun = betafun, return_what = 2, draw_plot = F)
  if(type == "test") return(0.75 * sum(mypred$sim_incidence[match(dd + positive_interval, mypred$dates_over5_cases)]) / mytotal_pop)
  if(type == "sero") return(sum(mypred$sim_incidence[1:match(dd - 13, mypred$dates_over5_cases)]) / mytotal_pop) # median time to seroconversion 13 days Long et al Nature Medicin
}

library(Rcpp)
if(getwd() == "/projet/extern/save/fblanquart/lockdown") masterdir <- "" else masterdir <- "~/ownCloud/coronavirus/lockdown/"
source(paste0(masterdir, "functions_renewal_deaths_v4.R"))
codefile <- paste0(masterdir, "simulate.cpp")
sourceCpp(codefile)  # simulations in Rcpp

# https://docs.google.com/spreadsheets/d/17Tf1Ln9VuE5ovpnhLRBJH-33L5KRaiB3NhvaiF3hWC0/edit#gid=0

positive_interval <- -20:-4 # positivity interval REF  https://www.acpjournals.org/doi/10.7326/M20-1495

get_fractionpositive <- function(dd, cc, type, positive_interval = -20:-4, with_CI = F){
  print(paste0("loading ", cc, " in global environment..."))
  load(paste0("~/ownCloud/coronavirus/lockdown/August2020/", cc, ".RData"), envir = .GlobalEnv)
  sourceCpp(codefile)  # simulations in Rcpp
  date_measure <- as.numeric(strftime(dd, format = "%j"))
  
  # get predictions from simulation results:
  mypar1 <- get_best_mcmc(mcmc_sample1)
  mypar2 <- get_best_mcmc(mcmc_sample2)
  frac1 <- get_frac(mypar1, 1, type, date_measure)
  frac2 <- get_frac(mypar2, 2, type, date_measure)
  
  if(!with_CI){
    return(c(frac1, frac2))
  } else {
    # compute CI:
    samp1 <- get_sample_mcmc(mm = mcmc_sample1)
    truc1 <- apply(samp1, 1, function(parvec) get_frac(parvec, 1, type, date_measure))
    samp2 <- get_sample_mcmc(mm = mcmc_sample2)
    truc2 <- apply(samp2, 1, function(parvec) get_frac(parvec, 2, type, date_measure))
    
    return(c(frac1, frac2, quantile(truc1, c(0.025, 0.975)), quantile(truc2, c(0.025, 0.975))))
  }
}

tab <- data.frame(country = c("Austria", "Brazil", "New York_US", "Netherlands", "United_Kingdom", "Denmark", "Spain", "Belgium", "Belgium", "Czechia"))
tab$dates <- c("2020-04-05", "2020-05-17", "2020-04-23", "2020-04-08", "2020-05-02", "2020-04-11", "2020-04-30", "2020-03-30", "2020-04-14", "2020-04-26")
tab$type <- c("test", "sero", "sero", "sero", "test", "sero", "sero", "sero", "sero", "sero")
tab$reference <- c(
  "https://unctad.org/en/PublicationsLibrary/CSTD_COVID19_c06_Austria_en.pdf",
  "https://www.medrxiv.org/content/10.1101/2020.05.30.20117531v1.full.pdf",
  "https://www.governor.ny.gov/news/amid-ongoing-covid-19-pandemic-governor-cuomo-announces-results-completed-antibody-testing",
)
tab$N <- c(1544, 25025, 15103, 7361, 7087, 9496, 60983, 3000, 1300, 26549)
tab$p <- c(0.33, 1.4, 12.3, 2.7, 0.24, 1.7, 5, 2.1, 4.1, 0.40)/100
tab$p_lower <- tab$p - 1.96 * sqrt(tab$p * (1 - tab$p) / tab$N)
tab$p_upper <- tab$p + 1.96 * sqrt(tab$p * (1 - tab$p) / tab$N)

tab <- tab[tab$country != "United_Kingdom", ]

tab$model1 <- NA
tab$model2 <- NA
tab$model1_lower <- NA
tab$model2_lower <- NA
tab$model1_upper <- NA
tab$model2_upper <- NA

for(i in 1:nrow(tab)){
  truc <- get_fractionpositive(dd = tab$dates[i], cc = tab$country[i], type = tab$type[i], with_CI = T)
  tab$model1[i] <- truc[1]
  tab$model2[i] <- truc[2]
  tab$model1_lower[i] <- truc[3]
  tab$model1_upper[i] <- truc[4]
  tab$model2_lower[i] <- truc[5]
  tab$model2_upper[i] <- truc[6]
}

pdf(file = "~/ownCloud/coronavirus/lockdown/validation_positivity.pdf", width = 4 * 1, height = 4 * 1)
par(mar = c(4,4,1,1), las = 1)
plot(NULL, pch = 20, xlim = c(0., 0.15), ylim = c(0, 0.15), xlab = "attack rate predicted by the model", ylab = "attack rate in serosurveys")
abline(0,1, col = "gray")
points(tab$model2, tab$p, pch = 20)
segments(x0 = tab$model2, y0 = tab$p_lower, x1 = tab$model2, y1 = tab$p_upper)
segments(x0 = tab$model2_lower, y0 = tab$p, x1 = tab$model2_upper,  y1 = tab$p)
xlabels <- tab$model2 + 0.002
ylabels <- tab$p - 0.002
#xlabels[c(1, 9)] <- tab$model2[c(1, 9)] + c(-0.003, -0.005)
#ylabels[c(1, 9)] <- tab$p[c(1, 9)] + c(-0.003, 0.003)
yoffsets <- c(0, 0, -0.005, 0, 0, 0.006, 0, -0.003, 0.005)
text(xlabels, ylabels + yoffsets,
     labels = tab$country, cex = 0.5, adj = 0)
dev.off()

# AUSTRIA has 0.33% [0.12-0.76] tested positive on April 4th-5th (Apr 1-6) (N = 1544)
# https://unctad.org/en/PublicationsLibrary/CSTD_COVID19_c06_Austria_en.pdf
get_fractionpositive(dd = "2020-04-05", cc = "Austria", type = "test")

# BRAZIL HAS  1.4% seroprevalence from 14-21 May (N = 25,025) 
# https://www.medrxiv.org/content/10.1101/2020.05.30.20117531v1.full.pdf
get_fractionpositive(dd = "2020-05-03", cc = "Brazil", type = "sero")

# NY STATE has 12.3% seroprevalent as of (20/04/2020 to 0.1/05/2020) (N = 15,103)
# https://www.bloomberg.com/news/articles/2020-04-23/new-york-finds-virus-marker-in-13-9-suggesting-wide-spread
# https://www.governor.ny.gov/news/amid-ongoing-covid-19-pandemic-governor-cuomo-announces-results-completed-antibody-testing
get_fractionpositive(dd = "2020-04-23", cc = "New York_US", type = "sero")

# Netherlands 2.7% seroprevalent as of 1 to 15 April 2020 ( = 7361)
# https://www.researchsquare.com/article/rs-25862/v1
get_fractionpositive(dd = "2020-04-08", cc = "Netherlands", type = "sero")

# England
# https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/bulletins/coronaviruscovid19infectionsurvey/england10may2020
# 0.24% of the population in England tested positive for COVID-19 between 26 April and 8 May 2020 (N = 7087)
# let's take 2 May as midpoint
get_fractionpositive(dd = "2020-05-02", cc = "United_Kingdom", type = "test")
                     
# Denmark blood donors April 6 to 17, seroprevalence of 1.7% (N = 9496)
# https://www.medrxiv.org/content/10.1101/2020.04.24.20075291v1.full.pdf
get_fractionpositive(dd = "2020-04-11", cc = "Denmark", type = "sero")
get_fractionpositive(dd = "2020-04-06", cc = "Denmark", type = "sero")
get_fractionpositive(dd = "2020-04-17", cc = "Denmark", type = "sero")


# Spain 5% beginning of May, serology (N = 60,983 individuals)
# https://www.aa.com.tr/en/europe/study-5-of-spanish-population-has-had-coronavirus/1839965
# https://www.mscbs.gob.es/gabinete/notasPrensa.do?id=4914
get_fractionpositive(dd = "2020-04-30", cc = "Spain", type = "sero")

# Belgium blood donors serology (Flanders). 4.1% (N=3000), 14 April / 2.1% (N = 1300), 30 March
#https://www.vrt.be/vrtnws/en/2020/04/25/4-3-of-belgians-have-antibodies-against-coronavirus0/
#https://covid-19.sciensano.be/sites/default/files/Covid19/COVID-19_Weekly%20report_20200529%20-%20NL_0.pdf

get_fractionpositive(dd = "2020-04-14", cc = "Belgium", type = "sero")
get_fractionpositive(dd = "2020-03-30", cc = "Belgium", type = "sero")
get_fractionpositive(dd = "2020-05-15", cc = "Belgium", type = "sero")

# Wuhan China 10% April 3 to 15 (N = 1400) https://onlinelibrary.wiley.com/doi/epdf/10.1002/jmv.25904
# problem for Wuhan we do not have the beginning of the epidemic so we can't say total prevalence as of March

# Czech Republic 0.4% April 23 to May 1st (N = 26,549) https://koronavirus.mzcr.cz/infekce-covid-19-prosla-ceskou-populaci-velmi-mirne-podobne-jako-v-okolnich-zemich/
get_fractionpositive(dd = "2020-04-26", cc = "Czechia", type = "sero")

# England week 17 (around 26 May) around 5% positive https://www.gov.uk/government/publications/national-covid-19-surveillance-reports/sero-surveillance-of-covid-19
get_fractionpositive(dd = "2020-04-26", cc = "United_Kingdom", type = "sero")



