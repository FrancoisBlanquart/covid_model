rm(list = ls())
source("~/ownCloud/coronavirus/lockdown/analyse_results_functions.R")
testdata <- read.csv("~/ownCloud/coronavirus/lockdown/data/owid-covid-data.csv")
library(Rcpp)

# HERE WE TAKE THE .RDATA FILE RESULTING FROM INFERENCE AND SUMMARISE RESULTS IN A SINGLE TABLE

f <- list.files("~/ownCloud/coronavirus/lockdown/August2020/")
f <- f[grepl(pattern = ".RData", x = f)]
nf <- length(f)
all_pars <- data.frame(matrix(NA, nrow = nf, ncol = (3 + 8 + 10) * 3 + 3*2))
namespars0 <- c("Rpre_0", "Rpost_0", "size_nbinom_0")
namespars1 <- c("Rpre_1", "Rpost_1", "pmin_1", "pmax_1", "kp_1", "tmidpointp_1", "size_nbinom_deaths_1", "size_nbinom_det_1")
namespars2 <- c("Rpre_2", "Rpost_2", "tmidpointR_2", "kR_2", "pmin_2", "pmax_2", "kp_2", "tmidpointp_2", "size_nbinom_deaths_2", "size_nbinom_det_2")

nn <- c(namespars0, namespars1, namespars2)
names(all_pars) <- c(paste0(nn, "_MCMC"), paste0(nn, "_lower"), paste0(nn, "_upper"), c("Rratio0_lower", "Rratio1_lower", "Rratio2_lower", "Rratio0_upper", "Rratio1_upper", "Rratio2_upper"))

all_pars$pdetected_1_lastdate <- NA
all_pars$pdetected_2_lastdate <- NA
all_pars$pdetected_1_lastdate_lower <- NA
all_pars$pdetected_2_lastdate_lower <- NA
all_pars$pdetected_1_lastdate_median <- NA
all_pars$pdetected_2_lastdate_median <- NA
all_pars$pdetected_1_lastdate_upper <- NA
all_pars$pdetected_2_lastdate_upper <- NA

all_pars$pdetected_1_firstdate <- NA
all_pars$pdetected_2_firstdate <- NA
all_pars$pdetected_1_firstdate_lower <- NA
all_pars$pdetected_2_firstdate_lower <- NA
all_pars$pdetected_1_firstdate_median <- NA
all_pars$pdetected_2_firstdate_median <- NA
all_pars$pdetected_1_firstdate_upper <- NA
all_pars$pdetected_2_firstdate_upper <- NA

all_pars$R_2_firstdate <- NA
all_pars$R_2_firstdate_lower <- NA
all_pars$R_2_firstdate_median <- NA
all_pars$R_2_firstdate_upper <- NA
all_pars$R_2_lastdate <- NA
all_pars$R_2_lastdate_lower <- NA
all_pars$R_2_lastdate_median <- NA
all_pars$R_2_lastdate_upper <- NA

all_pars$country <- NA
all_pars$datelockdown <- NA
all_pars$lastdate <- NA
all_pars$date5deaths <- NA
all_pars$daily_deaths_over50 <- NA
all_pars$firstdate <- NA
all_pars$ifr <- NA
all_pars$total_pop <- NA
# all_pars$lik0 <- NA
# all_pars$lik1 <- NA
# all_pars$lik2 <- NA
all_pars$lik0_MCMC <- NA
all_pars$lik1_MCMC <- NA
all_pars$lik2_MCMC <- NA
all_pars$best_model <- NA
all_pars$total_infected0 <- NA
all_pars$total_infected1 <- NA
all_pars$total_infected2 <- NA
all_pars$ntests_lastdate <- NA

all_pars$total_infected0_lower <- NA
all_pars$total_infected1_lower <- NA
all_pars$total_infected2_lower <- NA
all_pars$total_infected0_median <- NA
all_pars$total_infected1_median <- NA
all_pars$total_infected2_median <- NA
all_pars$total_infected0_upper <- NA
all_pars$total_infected1_upper <- NA
all_pars$total_infected2_upper <- NA

all_pars$total_death_data <- NA
all_pars$total_death0 <- NA
all_pars$total_death1 <- NA
all_pars$total_death2 <- NA
all_pars$total_death0_lower <- NA
all_pars$total_death1_lower <- NA
all_pars$total_death2_lower <- NA
all_pars$total_death0_median <- NA
all_pars$total_death1_median <- NA
all_pars$total_death2_median <- NA
all_pars$total_death0_upper <- NA
all_pars$total_death1_upper <- NA
all_pars$total_death2_upper <- NA

# deaths without detection and without distanciation
all_pars$total_death1_nodetection <- NA
all_pars$total_death1_nodetection_lower <- NA
all_pars$total_death1_nodetection_median <- NA
all_pars$total_death1_nodetection_upper <- NA
all_pars$total_death1_nodistanciation <- NA
all_pars$total_death1_nodistanciation_lower <- NA
all_pars$total_death1_nodistanciation_median <- NA
all_pars$total_death1_nodistanciation_upper <- NA

all_pars$total_death2_nodetection <- NA
all_pars$total_death2_nodetection_lower <- NA
all_pars$total_death2_nodetection_median <- NA
all_pars$total_death2_nodetection_upper <- NA
all_pars$total_death2_nodistanciation <- NA
all_pars$total_death2_nodistanciation_lower <- NA
all_pars$total_death2_nodistanciation_median <- NA
all_pars$total_death2_nodistanciation_upper <- NA

all_pars$delta0 <- NA
all_pars$delta1 <- NA
all_pars$delta2 <- NA
all_pars$delta0_lower <- NA
all_pars$delta1_lower <- NA
all_pars$delta2_lower <- NA
all_pars$delta0_median <- NA
all_pars$delta1_median <- NA
all_pars$delta2_median <- NA
all_pars$delta0_upper <- NA
all_pars$delta1_upper <- NA
all_pars$delta2_upper <- NA

for(ii in 1:nf){

  print(ii)
  
  myfile <- paste0("~/ownCloud/coronavirus/lockdown/August2020/", f[ii])
  load(myfile)
  
  if(!exists("col1_rgb")){
    col1_rgb <- as.vector(col2rgb(col1))/256
    col2_rgb <- as.vector(col2rgb(col2))/256
    col3_rgb <- as.vector(col2rgb(col3))/256
  }

  all_pars$country[ii] <- mycountry
  all_pars[ii, "datelockdown"] <- datelockdown
  all_pars$total_pop[ii] <- mytotal_pop
  idx_data <- sub_tab$date[which(sub_tab$cases >= 5 & sub_tab$deaths >= 0)]
  lastdate <- max(idx_data, na.rm = T)
  firstdate <- min(idx_data, na.rm = T)
  
  stopifnot(firstdate == min(tmp0$dates_over5_cases))
  stopifnot(lastdate == max(tmp0$dates_over5_cases))
  stopifnot(firstdate == min(tmp1$dates_over5_cases))
  stopifnot(lastdate == max(tmp1$dates_over5_cases))
  stopifnot(firstdate == min(tmp2$dates_over5_cases))
  stopifnot(lastdate == max(tmp2$dates_over5_cases))
  stopifnot(lastdate == 129 | (mycountry == "Hubei_China" & lastdate == 73) | (mycountry == "Ecuador" & lastdate == 128) | (mycountry == "Mexico" & lastdate == 130) | (mycountry == "Minnesota_US" & lastdate == 134) | (mycountry == "Serbia" & lastdate == 134) | (mycountry == "South_Africa" & lastdate == 130)) # now it is 129 except for Hubei and Ecuador
  
  all_pars$lastdate[ii] <- lastdate
  all_pars$ifr[ii] <- ifr
  all_pars$date5deaths[ii] <- date5deaths
  all_pars$firstdate[ii] <- firstdate
  all_pars$total_death_data[ii] <- sum(sub_tab$deaths[1:match(lastdate, sub_tab$date)], na.rm = T) # all deaths till last date?
  
  # add test data if country had large enough death incidence (> 50 daily)
  if(any(sub_tab$deaths[which(sub_tab$cases >= 5 & sub_tab$deaths >= 0)] > 50)) all_pars$daily_deaths_over50[ii] <- TRUE
    
  if(!"ntests" %in% names(sub_tab)){ # complete test data if not yet in sub_tab
    idx_in_test <- which(testdata$ld_country == mycountry)
    if(length(idx_in_test) > 0){
      sub_testdata <- testdata[idx_in_test, ]
      sub_tab$ntests <- NA
      sub_tab$ntests <- sub_testdata$new_tests[match(sub_tab$date, sub_testdata$date)]
      if(sum(!is.na(sub_tab$ntests)) <= 5) sub_tab$ntests <- NULL # nullify that column if less than 5 datapoints
    } else {
      sub_tab$ntests <- NULL # nullify that column if not present in dasta
    }
  }
  
  # number of tests in week before last date (there are day-of-the-week effects in the data)
  if("ntests" %in% names(sub_tab)){
    # select series of tests over 7 last days
    ntest_series <- sub_tab$ntests[match(lastdate-7, sub_tab$date):match(lastdate, sub_tab$date)]
    npositive_series <- sub_tab$cases[match(lastdate-7, sub_tab$date):match(lastdate, sub_tab$date)]
    toselect <- which(ntest_series > 0 & ntest_series != npositive_series) # those where positive number of tests and negative tests have been reported
    n_test_series <- length(toselect) # number of valid tests
    if(n_test_series > 0){
      all_pars$ntests_lastdate[ii] <- sum(ntest_series) / n_test_series #sub_tab$ntests[match(lastdate, sub_tab$date)]
    } else {
      stop()
    }
    # 
    # if(all_pars$ntests_lastdate[ii] < 0 | all_pars$ntests_lastdate[ii] == sub_tab$cases[match(lastdate, sub_tab$date)]) all_pars$ntests_lastdate[ii] <- sub_tab$ntests[match(lastdate-1, sub_tab$date)] # take day before if < 0 or negative not reported
    # if(all_pars$ntests_lastdate[ii] < 0 | all_pars$ntests_lastdate[ii] == sub_tab$cases[match(lastdate, sub_tab$date)]) stop()
  }
    
  ################### GET ML ESTIMATES ###################
  
  all_pars$lik0_MCMC[ii] <- -max(mcmc_sample0$all.lik)
  all_pars$lik1_MCMC[ii] <- -max(mcmc_sample1$all.lik)
  all_pars$lik2_MCMC[ii] <- -max(mcmc_sample2$all.lik)
  
  if(2 * (all_pars$lik2_MCMC[ii] - all_pars$lik1_MCMC[ii]) + 2 * 2 <  -4) all_pars$best_model[ii] <- 2 else all_pars$best_model[ii] <- 1
  
  mcmc_pars0 <- get_best_mcmc(mcmc_sample0)
  mcmc_pars1 <- get_best_mcmc(mcmc_sample1)
  mcmc_pars2 <- get_best_mcmc(mcmc_sample2)
  
  all_pars[ii , paste0(nn, "_MCMC")] <- c(mcmc_pars0 , mcmc_pars1, mcmc_pars2)
  
  ################### GET CI ON PARAMETERS ###################
  
  # FIRST DRAW A SAMPLE OF PARAMETERS ONLY
  
  mcmc_subsample0 <- get_parsample(mcmc_sample0, myn = 10000)
  mcmc_subsample1 <- get_parsample(mcmc_sample1, myn = 10000)
  mcmc_subsample2 <- get_parsample(mcmc_sample2, myn = 10000)
  
  ci0 <- get_ci(mcmc_subsample0)
  ci1 <- get_ci(mcmc_subsample1)
  ci2 <- get_ci(mcmc_subsample2)
  
  #all_pars[ii, nn] <- c(opt0_0$pars, opt0_1$pars, opt0_2$pars)
  all_pars[ii, paste0(nn, "_lower")] <- c(ci0["2.5%",], ci1["2.5%",], ci2["2.5%",])
  all_pars[ii, paste0(nn, "_upper")] <- c(ci0["97.5%",], ci1["97.5%",], ci2["97.5%",])
  
  all_pars[ii, c("Rratio0_lower", "Rratio1_lower", "Rratio2_lower", "Rratio0_upper", "Rratio1_upper", "Rratio2_upper")] <- c(
    get_ci_ratio(mcmc_subsample0)[c("2.5%", "97.5%")], get_ci_ratio(mcmc_subsample1)[c("2.5%", "97.5%")], get_ci_ratio(mcmc_subsample2)[c("2.5%", "97.5%")]
  )
  
  ################### RESIMULATE AND GET CI ON VARIOUS QUANTITIES ###################
  
  # FOR NOW NEED TO RELOAD SIMULATE.CPP AND DRAW_PLOT_FUN BECAUSE DIFFERENT VERSION THAN IN.RDATA
  sourceCpp("~/ownCloud/coronavirus/lockdown/simulate.cpp")
  #source("~/ownCloud/coronavirus/lockdown/analyse_results_functions.R")
  if(getwd() == "/projet/extern/save/fblanquart/lockdown") masterdir <- "" else masterdir <- "~/ownCloud/coronavirus/lockdown/"
  source(paste0(masterdir, "functions_renewal_deaths_v4.R"))
  
  # get confidence intervals on model and at the same time draw a figure: 
  pdf(file = gsub(pattern = ".RData", replacement = ".pdf" , x = myfile), width = 4 * 2, height = 3 * 2)
    par(mar = c(4,4,1,1))
    # BEST PARAMETER PREDICTIONS:
    # MODEL 0
    delta0 <- loglik_Rcpp(par = mcmc_pars0, epidata = sub_tab, datelockdown = datelockdown, tmax = tmax, with_detected = F, use_tests = F, total_pop = mytotal_pop, betafun = 1, return_what = 1, draw_plot = F)$delta
    myattach(get_sims(pars = mcmc_pars0, parsample = mcmc_subsample0, with_detected = F, delta = delta0, betafun = 1))
    pred0 <- pred_bis
    all_pars[ii, c("total_infected0", "total_infected0_lower", "total_infected1_median", "total_infected0_upper")] <- c(sum(pred_bis$all_incidence[1:(lastdate+delta0)]), ci_total_infected)
    all_pars[ii, c("total_death0", "total_death0_lower", "total_death0_median", "total_death0_upper")] <- c(total_death, ci_total_deaths)
    all_pars[ii, "delta0"] <- delta0
    all_pars[ii, c("delta0_lower", "delta0_median", "delta0_upper")] <- ci_delta
    
    # MODEL 1
    delta1 <- loglik_Rcpp(par = mcmc_pars1, epidata = sub_tab, datelockdown = datelockdown, tmax = tmax, with_detected = T, use_tests = F, total_pop = mytotal_pop, betafun = 1, return_what = 1, draw_plot = F)$delta
    myattach(get_sims(pars = mcmc_pars1, parsample = mcmc_subsample1, with_detected = T, delta = delta1, betafun = 1))
    pred1 <- pred_bis
    all_pars[ii, c("total_infected1", "total_infected1_lower", "total_infected1_median", "total_infected1_upper")] <- c(sum(pred_bis$all_incidence[1:(lastdate+delta1)]), ci_total_infected)
    all_pars[ii, c("total_death1", "total_death1_lower", "total_death1_median", "total_death1_upper")] <- c(total_death, ci_total_deaths)
    all_pars[ii, c("tmidpointp_1_lower", "tmidpointp_1_median", "tmidpointp_1_upper")] <- ci_tmidpoint # have that in data time
    all_pars[ii, "tmidpointp1_MCMC"] <- mcmc_pars1[which(namespars1 == "tmidpointp_1")] - delta1 # convert that in data time
    all_pars[ii, "delta1"] <- delta1
    all_pars[ii, c("delta1_lower", "delta1_median", "delta1_upper")] <- ci_delta
    all_pars[ii, c("pdetected_1_firstdate_lower", "pdetected_1_firstdate_median", "pdetected_1_firstdate_upper")] <- ci_pdetected_first
    all_pars[ii, c("pdetected_1_lastdate_lower", "pdetected_1_lastdate_median", "pdetected_1_lastdate_upper")] <- ci_pdetected_last
    
    # MODEL 2
    delta2 <- loglik_Rcpp(par = mcmc_pars2, epidata = sub_tab, datelockdown = datelockdown, tmax = tmax, with_detected = T, use_tests = F, total_pop = mytotal_pop, betafun = 2, return_what = 1, draw_plot = F)$delta
    myattach(get_sims(pars = mcmc_pars2, parsample = mcmc_subsample2, with_detected = T, delta = delta2, betafun = 2))
    pred2 <- pred_bis
    all_pars[ii, c("total_infected2", "total_infected2_lower", "total_infected2_median", "total_infected2_upper")] <- c(sum(pred_bis$all_incidence[1:(lastdate+delta2)]), ci_total_infected)
    all_pars[ii, c("total_death2", "total_death2_lower", "total_death2_median", "total_death2_upper")] <- c(total_death, ci_total_deaths)
    all_pars[ii, "delta2"] <- delta2
    all_pars[ii, c("tmidpointR_2_lower", "tmidpointR_2_median", "tmidpointR_2_upper")] <- ci_tmidpointR_2 # have that in data time
    all_pars[ii, c("tmidpointp_2_lower", "tmidpointp_2_median", "tmidpointp_2_upper")] <- ci_tmidpoint # have that in data time
    all_pars[ii, "tmidpointR_2_MCMC"] <- mcmc_pars2[which(namespars2 == "tmidpointR_2")] - delta2 # convert that in data time
    all_pars[ii, "tmidpointp_2_MCMC"] <- mcmc_pars2[which(namespars2 == "tmidpointp_2")] - delta2 # convert that in data time
    all_pars[ii, c("delta2_lower", "delta2_median", "delta2_upper")] <- ci_delta
    all_pars[ii, c("pdetected_2_firstdate_lower", "pdetected_2_firstdate_median", "pdetected_2_firstdate_upper")] <- ci_pdetected_first
    all_pars[ii, c("pdetected_2_lastdate_lower",  "pdetected_2_lastdate_median",  "pdetected_2_lastdate_upper")] <- ci_pdetected_last
    all_pars[ii, c("R_2_firstdate_lower", "R_2_firstdate_median", "R_2_firstdate_upper", "R_2_lastdate_lower", "R_2_lastdate_median", "R_2_lastdate_upper")] <- c(ci_Rfirst, ci_Rlast)
  dev.off()

  # get p detected at 5deaths and at lastdate
  all_pars[ii, c("pdetected_1_firstdate", "pdetected_1_lastdate", "pdetected_2_firstdate", "pdetected_2_lastdate")] <- c(pdetected_smooth(tt = c(firstdate, lastdate) + delta1, par = mcmc_pars1, betafun = 1), pdetected_smooth(tt = c(firstdate, lastdate) + delta2, par = mcmc_pars2, betafun = 2))
  
  # and R at firstdate and lastdate
  all_pars[ii, c("R_2_firstdate", "R_2_lastdate")] <- mcmc_pars2[1] * relativeR_smooth(tt = c(firstdate, lastdate) + delta2, par = mcmc_pars2)
  
  # scenarios without interventions:
  # model 1
  myattach(get_sims(pars = mcmc_pars1, parsample = mcmc_subsample1, with_detected = T, delta = delta1, betafun = 1, parzero = 3))
  all_pars[ii, c("total_death1_nodetection", "total_death1_nodetection_lower", "total_death1_nodetection_median", "total_death1_nodetection_upper")] <- c(total_death, ci_total_deaths)
  
  myattach(get_sims(pars = mcmc_pars1, parsample = mcmc_subsample1, with_detected = T, delta = delta1, betafun = 1, nochangeR = TRUE))
  all_pars[ii, c("total_death1_nodistanciation", "total_death1_nodistanciation_lower", "total_death1_nodistanciation_median", "total_death1_nodistanciation_upper")] <-   c(total_death, ci_total_deaths)
  
  # model 2
  # TODO NOT CHANGE THE DELTA WHEN INFERRING DEATHS WITHOUT LOCKDOWN???
  myattach(get_sims(pars = mcmc_pars2, parsample = mcmc_subsample2, with_detected = T, delta = delta2, betafun = 2, parzero = 5))
  all_pars[ii, c("total_death2_nodetection", "total_death2_nodetection_lower", "total_death2_nodetection_median", "total_death2_nodetection_upper")] <- c(total_death, ci_total_deaths)
  
  myattach(get_sims(pars = mcmc_pars2, parsample = mcmc_subsample2, with_detected = T, delta = delta2, betafun = 2, nochangeR = TRUE))
  all_pars[ii, c("total_death2_nodistanciation", "total_death2_nodistanciation_lower", "total_death2_nodistanciation_median", "total_death2_nodistanciation_upper")] <-   c(total_death, ci_total_deaths)

  
  # save file for tests analysis if enough daily deaths for c_t to be well inferred
  if(any(sub_tab$deaths[which(sub_tab$cases >= 5 & sub_tab$deaths >= 0)] > 50) & "ntests" %in% names(sub_tab)){
    mytestfile <- paste0("~/ownCloud/coronavirus/lockdown/August2020/tests/", gsub(pattern = ".RData", replacement = "_test.RData", f[ii]))
    save(file = mytestfile, list = c("mytotal_pop", "mycountry", "sub_tab", "firstdate", "lastdate", "pred2", "delta2"))
  }
}

all_pars$deltaAIC <- 2*(all_pars$lik1_MCMC - all_pars$lik2_MCMC) - 2 * 2
table(all_pars$deltaAIC > 4, all_pars$best_model)
all_pars$deaths_averted <-      all_pars$total_death2_nodistanciation - all_pars$total_death_data
all_pars$deaths_averted_lower <- all_pars$total_death2_nodistanciation_lower - all_pars$total_death_data
all_pars$deaths_averted_median <- all_pars$total_death2_nodistanciation_median - all_pars$total_death_data
all_pars$deaths_averted_upper <- all_pars$total_death2_nodistanciation_upper - all_pars$total_death_data

# ALL TIMES ARE IN DATA TIME NOW!
lastdate <- 129
print("ALL TIMES ARE IN DATA TIME REFERENTIAL NOW!!!!")

######################################################           MAP ALL COUNTRIES            ######################################################

stopifnot(all(ld$country==all_pars$country))
all_pars$continent <- NA
all_pars$continent[grepl(pattern = "_US", x = all_pars$country) | grepl(pattern = "_Canada", x = all_pars$country) | all_pars$country == "Mexico"  | all_pars$country == "Canada"] <- "North America"
all_pars$continent[all_pars$country %in% c("Austria", "Belgium", "France", "Germany", "Czechia", "Denmark", "Hungary", "Ireland" , "Italy", "Netherlands", "Norway", "Romania", "Russia", "Serbia", "Spain",  "Sweden", "Switzerland", "Ukraine", "United_Kingdom", "Poland", "Portugal")] <- "Europe + Russia"
all_pars$continent[all_pars$country %in% c("Bangladesh", "Hubei_China", "India",  "Indonesia", "Pakistan", "Philippines")] <- "Asia"
all_pars$continent[all_pars$country %in% c("Argentina", "Brazil", "Chile", "Colombia", "Ecuador", "Dominican_Republic", "Peru")] <- "South / Central America"
all_pars$continent[all_pars$country %in% c("Algeria", "Egypt", "Israel", "Iran",  "Morocco", "Turkey")] <- "North Africa / Middle East"
all_pars$continent[all_pars$country == "South_Africa"] <- "South Africa"

all_pars$pretty_country <- all_pars$country
all_pars$pretty_country <- gsub(pattern = "_US", replacement = ", US", x = all_pars$pretty_country)
all_pars$pretty_country <- gsub(pattern = "_Canada", replacement = ", Canada", x = all_pars$pretty_country)
all_pars$pretty_country[all_pars$pretty_country=="South_Africa"] <- "South Africa"
all_pars$pretty_country[all_pars$pretty_country=="Dominican_Republic"] <- "Dominican Republic"
all_pars$pretty_country[all_pars$pretty_country=="Hubei_China"] <- "Hubei, China"
all_pars$pretty_country[all_pars$pretty_country=="United_Kingdom"] <- "United Kingdom"

all_pars$matching_country <- ld$matching_country

library(rworldmap)
colset <- RColorBrewer::brewer.pal(n = 12, name = "Paired")[seq(2, 12, 2)]
colset_light <- RColorBrewer::brewer.pal(n = 12, name = "Paired")[seq(1, 11, 2)]
names(colset) <- sort(unique(all_pars$continent))
names(colset_light) <- sort(unique(all_pars$continent))
# Get the world map
worldMap <- getMap()
all_pars$worldmapcountry <- all_pars$country
all_pars$worldmapcountry[all_pars$worldmapcountry=="South_Africa"] <- "South Africa"
all_pars$worldmapcountry[grepl(pattern = "_US", all_pars$worldmapcountry)] <- "United States"
all_pars$worldmapcountry[grepl(pattern = "_Canada", all_pars$worldmapcountry)] <- "Canada"
all_pars$worldmapcountry[all_pars$worldmapcountry=="Czechia"] <- "Czech Rep."
all_pars$worldmapcountry[all_pars$worldmapcountry=="Dominican_Republic"] <- "Dominican Rep."
all_pars$worldmapcountry[all_pars$worldmapcountry=="Hubei_China"] <- "China"
all_pars$worldmapcountry[all_pars$worldmapcountry=="United_Kingdom"] <- "United Kingdom"

stopifnot(all(all_pars$worldmapcountry %in% worldMap$NAME))


tmp <- all_pars[, c("worldmapcountry", "continent")]
names(tmp)[2] <- "mycontinent"
mymap <- joinCountryData2Map(tmp, joinCode = "NAME", nameJoinColumn = "worldmapcountry")
mymap <- subset(mymap, continent != "Antarctica")
pdf("~/ownCloud/coronavirus/lockdown/August2020/map.pdf", width = 4 * 2, height = 3 * 2)
mapCountryData(mymap, nameColumnToPlot="mycontinent", catMethod = "categorical",
               missingCountryCol = gray(.8), colourPalette = colset, addLegend = F)
dev.off()

subs <- sapply(unique(all_pars$continent), function(cc) which(all_pars$continent == cc))

######################################################           ANALYSES AND PLOTS           ######################################################



# high / low IFR
all_pars$country[which(all_pars$ifr < 0.004)]
all_pars$ifr[which(all_pars$ifr < 0.004)]

all_pars$country[which(all_pars$ifr > 0.012)]
all_pars$ifr[which(all_pars$ifr > 0.012)]

# When SMOOTH / SHARP REDUCTION IN TRANSMISSION FAVOURED
table(all_pars$deltaAIC > 4, all_pars$best_model)

# mean transmissibility pre-post lockdown
mean(all_pars$R_2_firstdate) # 3.8
range(all_pars$R_2_firstdate)
mean(all_pars$R_2_lastdate)  # 0.96

# mean proba of detection pre-post
mean(all_pars$pdetected_2_firstdate) # 4%
mean(all_pars$pdetected_2_lastdate)  # 27%
table(all_pars$pdetected_2_lastdate > 0.5) # 9 regions above 50%

# WHICH LOCKDOWN TYPE DOES NOT AFFECT CHANGE IN R
stopifnot(all(ld$country==all_pars$country))
table(ld$which_date)
ld$country[ld$which_date=="national_distancing_startdate"]
ld$country[ld$which_date=="regional_lockdown_startdate"]
summary(lm(all_pars$Rpost_1_MCMC ~ ld$which_date))
anova(lm(all_pars$R_2_lastdate ~ ld$which_date))
summary(lm(all_pars$Rpost_2_MCMC/all_pars$Rpre_2_MCMC ~ ld$which_date))
summary(lm(all_pars$Rpost_1_MCMC/all_pars$Rpre_1_MCMC ~ ld$which_date))

# PDETECTION BELOW 0.5
table(all_pars$pdetected_2_lastdate < 0.5)

# PREVALENCE
range(all_pars$total_infected2/all_pars$total_pop, na.rm = T) * 100
all_pars$country[which.min(all_pars$total_infected2/all_pars$total_pop)]
(all_pars$total_infected2/all_pars$total_pop)[all_pars$country %in% c("Bangladesh", "India")]
all_pars$country[which.max(all_pars$total_infected2/all_pars$total_pop)]
hist(all_pars$total_infected2/all_pars$total_pop, breaks = 50)
hist(all_pars$total_infected2/all_pars$total_pop, breaks = 50)
# prevalence smaller than 2%
sum(all_pars$total_infected2/all_pars$total_pop < 0.02, na.rm = T)
View(cbind(all_pars$country, all_pars$total_infected2/all_pars$total_pop))

# DEATHS AVERTED
sum(all_pars$deaths_averted)/1e6
all_pars$deaths_averted[all_pars$country=="Brazil"]/1e6
all_pars$deaths_averted[all_pars$country=="France"]/1e6
all_pars$deaths_averted[all_pars$country=="Germany"]/1e6
all_pars$deaths_averted[all_pars$country=="United_Kingdom"]/1e6
all_pars$deaths_averted[all_pars$country=="Spain"]/1e6
all_pars$deaths_averted[all_pars$country=="Italy"]/1e6
all_pars$deaths_averted[all_pars$country=="Mexico"]/1e6

# COMPARISON WITH RUSSELL ET AL
for(cc in c("Denmark", "United_Kingdom", "Austria", "Belgium", "France")){
  print(cc)
  print(
    all_pars[all_pars$country==cc, c("firstdate", "pdetected_2_firstdate", "pdetected_2_lastdate")]
  )
}

all_pars[all_pars$country=="United_Kingdom", c("firstdate", "pdetected_2_firstdate", "pdetected_2_lastdate")]
all_pars[all_pars$country=="United_Kingdom", c("firstdate", "pdetected_2_firstdate", "pdetected_2_lastdate")]

# WHICH MODEL, SHARP or SMOOTH, IS FAVORED
par(mar = c(4,4,1,1))
figformat <- c(4, 3)

pdf("~/ownCloud/coronavirus/lockdown/August2020/correlation_midpoint_lockdown_date.pdf", width = 4 * 1.2, height = 4 * 1.2)
par(mar= c(4,4,1,1))
plot(NULL, las = 1, pch = 20, xlim = c(20, 100), ylim = c(20, 100), ylab = "midpoint date", xlab = "date social distancing")
for(cc in unique(all_pars$continent)) segments(x0 = all_pars$datelockdown[subs[[cc]]], x1 = all_pars$datelockdown[subs[[cc]]], y0 = all_pars$tmidpointR_2_lower[subs[[cc]]], y1 = all_pars$tmidpointR_2_upper[subs[[cc]]], col = colset_light[cc])
for(cc in unique(all_pars$continent)) points(all_pars$datelockdown[subs[[cc]]], all_pars$tmidpointR_2_MCMC[subs[[cc]]], las = 1, pch = 20,  col = colset[cc])
abline(0,1)
dev.off()


# R as a function of time for the regions where the two models do not have significant difference:
table(all_pars$deltaAIC < 4)

pdf("~/ownCloud/coronavirus/lockdown/August2020/sharp_smooth_reduction_comparison.pdf", width = 4 * 1.2 * 2, height = 4 * 1.2 * 2)
par(mar = c(4,4,1,1), mfrow = c(2,2))
x <- seq(0, 250, 1)
for(i in 1:nf) {
  if(signif(all_pars$deltaAIC[i], 2) < 4){
    plot(NULL, xlim = c(0, 183), ylim = c(0, 10), ylab = expression(R[t]), xlab = "date", las = 1, axes = F, main = paste(all_pars$pretty_country[i], ", AIC difference =", signif(all_pars$deltaAIC[i], 2))) #, " date lockdown:", all_pars$datelockdown[i], sep = "  "))
    all_dates_num <- c(1, 32, 61, 92, 122, 153, 183)
    all_dates <- c("01/01", "01/02", "01/03", "01/04", "01/05", "01/06", "01/07")
    axis(side = 1, at = all_dates_num, labels = all_dates)
    axis(side = 2, at = seq(0, 10, 2), las = 1)
    points(all_pars$firstdate[i]:all_pars$lastdate[i] , all_pars[i, "Rpre_2_MCMC"] * relativeR_smooth(tt = all_pars$firstdate[i]:all_pars$lastdate[i], par = unlist(c(all_pars[i, c("Rpre_2_MCMC" , "Rpost_2_MCMC", "tmidpointR_2_MCMC", "kR_2_MCMC")]))), type = "l", lwd = 3, col = colset_light[all_pars$continent][i])
    points(all_pars$firstdate[i]:all_pars$lastdate[i], all_pars[i, "Rpre_1_MCMC"] * relativeR_smooth(tt =  all_pars$firstdate[i]:all_pars$lastdate[i], par = unlist(c(all_pars[i, c("Rpre_1_MCMC" , "Rpost_1_MCMC", "datelockdown")], 10))), type = "l", lwd = 3, col = colset_light[all_pars$continent][i])
  }
}
dev.off()

# timescale of reduction
hist(1/all_pars$kR_2_MCMC, breaks = 50)


# COMPARE R PRE-POST LOCKDOWN IN MODEL 1 and 2
pdf("~/ownCloud/coronavirus/lockdown/August2020/R_pre_post_model1.pdf", width = 4 * 1.2, height = 4 * 1.2)
par(mar = c(4,4,1,1))
plot(NULL, xlim = c(1, 10), ylim = c(0, 2), xlab = "R pre-social distancing", ylab = "R post-social distancing")
for(cc in unique(all_pars$continent)){
  segments(x0 = all_pars$Rpre_1_MCMC[subs[[cc]]], x1 = all_pars$Rpre_1_MCMC[subs[[cc]]], y0 = all_pars$Rpost_1_lower[subs[[cc]]], y1 = all_pars$Rpost_1_upper[subs[[cc]]], col = colset_light[cc])
  segments(x0 = all_pars$Rpre_1_lower[subs[[cc]]], x1 = all_pars$Rpre_1_upper[subs[[cc]]], y0 = all_pars$Rpost_1_MCMC[subs[[cc]]], y1 = all_pars$Rpost_1_MCMC[subs[[cc]]], col = colset_light[cc])
}
for(cc in unique(all_pars$continent))points(all_pars$Rpre_1_MCMC[subs[[cc]]], all_pars$Rpost_1_MCMC[subs[[cc]]], pch = 20, type = "p", col = colset[cc])
#abline(0,1)
abline(1, 0, lty = 2)
dev.off()

pdf("~/ownCloud/coronavirus/lockdown/August2020/R_pre_post_model2.pdf", width = 4 * 1.2, height = 4 * 1.2)
par(mar = c(4,4,1,1))
plot(NULL, xlim = c(1, 10), ylim = c(0, 2), xlab = "R pre-social distancing", ylab = "R post-social distancing")
for(cc in unique(all_pars$continent)){
  segments(x0 = all_pars$R_2_firstdate[subs[[cc]]], x1 = all_pars$R_2_firstdate[subs[[cc]]], y0 = all_pars$R_2_lastdate_lower[subs[[cc]]], y1 = all_pars$R_2_lastdate_upper[subs[[cc]]], col = colset_light[cc])
  segments(x0 = all_pars$R_2_firstdate_lower[subs[[cc]]], x1 = all_pars$R_2_firstdate_upper[subs[[cc]]], y0 = all_pars$R_2_lastdate[subs[[cc]]], y1 = all_pars$R_2_lastdate[subs[[cc]]], col = colset_light[cc])
}
for(cc in unique(all_pars$continent))points(all_pars$R_2_firstdate[subs[[cc]]], all_pars$R_2_lastdate[subs[[cc]]], pch = 20, type = "p", col = colset[cc])
#abline(0,1)
abline(1, 0, lty = 2)
dev.off()

# COMPARE PDETECTION PRE-POST LOCKDOWN
pdf("~/ownCloud/coronavirus/lockdown/August2020/pdetection_pre_post_model1.pdf", width = 4 * 1.2, height = 4 * 1.2)
par(mar = c(4,4,1,1))
plot(NULL, xlim = c(0, 0.1), ylim = c(0, 1), xlab = "detection first date", ylab = "detection last date")
for(cc in unique(all_pars$continent)){
  segments(x0 = all_pars$pdetected_1_firstdate[subs[[cc]]], x1 = all_pars$pdetected_1_firstdate[subs[[cc]]], y0 = all_pars$pdetected_1_lastdate_lower[subs[[cc]]], y1 = all_pars$pdetected_1_lastdate_upper[subs[[cc]]], col = colset_light[cc])
  segments(x0 = all_pars$pdetected_1_firstdate_lower[subs[[cc]]], x1 = all_pars$pdetected_1_firstdate_upper[subs[[cc]]], y0 = all_pars$pdetected_1_lastdate[subs[[cc]]], y1 = all_pars$pdetected_1_lastdate[subs[[cc]]], col = colset_light[cc])
}
for(cc in unique(all_pars$continent))points(all_pars$pdetected_1_firstdate[subs[[cc]]], all_pars$pdetected_1_lastdate[subs[[cc]]], pch = 20, type = "p", col = colset[cc])
dev.off()

pdf("~/ownCloud/coronavirus/lockdown/August2020/pdetection_pre_post_model2.pdf", width = 4 * 1.2, height = 4 * 1.2)
par(mar = c(4,4,1,1))
plot(NULL, xlim = c(0, 0.1), ylim = c(0, 1), xlab = "detection first date", ylab = "detection last date")
for(cc in unique(all_pars$continent)){
  segments(x0 = all_pars$pdetected_2_firstdate[subs[[cc]]], x1 = all_pars$pdetected_2_firstdate[subs[[cc]]], y0 = all_pars$pdetected_2_lastdate_lower[subs[[cc]]], y1 = all_pars$pdetected_2_lastdate_upper[subs[[cc]]], col = colset_light[cc])
  segments(x0 = all_pars$pdetected_2_firstdate_lower[subs[[cc]]], x1 = all_pars$pdetected_2_firstdate_upper[subs[[cc]]], y0 = all_pars$pdetected_2_lastdate[subs[[cc]]], y1 = all_pars$pdetected_2_lastdate[subs[[cc]]], col = colset_light[cc])
}
for(cc in unique(all_pars$continent))points(all_pars$pdetected_2_firstdate[subs[[cc]]], all_pars$pdetected_2_lastdate[subs[[cc]]], pch = 20, type = "p", col = colset[cc])
dev.off()

# correlation between R post model 1 and model 2

pdf("~/ownCloud/coronavirus/lockdown/August2020/R_pre_post_model1_model2.pdf", width = 4 * 1.2, height = 4 * 1.2)
par(mar = c(4,4,1,1))
# first date:
plot(NULL, xlim = c(0, 10), ylim = c(0, 10), xlab = "initial R model 1", ylab = "initial R model 2")
for(cc in unique(all_pars$continent)){
  segments(x0 = all_pars$Rpre_1_MCMC[subs[[cc]]], x1 = all_pars$Rpre_1_MCMC[subs[[cc]]], y0 = all_pars$R_2_firstdate_lower[subs[[cc]]], y1 = all_pars$R_2_firstdate_upper[subs[[cc]]], col = colset_light[cc])
  segments(x0 = all_pars$Rpre_1_lower[subs[[cc]]], x1 = all_pars$Rpre_1_upper[subs[[cc]]], y0 = all_pars$R_2_firstdate[subs[[cc]]], y1 = all_pars$R_2_firstdate[subs[[cc]]], col = colset_light[cc])
}
for(cc in unique(all_pars$continent))points(all_pars$Rpre_1_MCMC[subs[[cc]]], all_pars$R_2_firstdate[subs[[cc]]], pch = 20, type = "p", col = colset[cc])
abline(0,1)

plot(NULL, xlim = c(0, 2), ylim = c(0, 2), xlab = "final R model 1", ylab = "final R model 2")
for(cc in unique(all_pars$continent)){
  segments(x0 = all_pars$Rpost_1_MCMC[subs[[cc]]], x1 = all_pars$Rpost_1_MCMC[subs[[cc]]], y0 = all_pars$R_2_lastdate_lower[subs[[cc]]], y1 = all_pars$R_2_lastdate_upper[subs[[cc]]], col = colset_light[cc])
  segments(x0 = all_pars$Rpost_1_lower[subs[[cc]]], x1 = all_pars$Rpost_1_upper[subs[[cc]]], y0 = all_pars$R_2_lastdate[subs[[cc]]], y1 = all_pars$R_2_lastdate[subs[[cc]]], col = colset_light[cc])
}
for(cc in unique(all_pars$continent))points(all_pars$Rpost_1_MCMC[subs[[cc]]], all_pars$R_2_lastdate[subs[[cc]]], pch = 20, type = "p", col = colset[cc])
abline(0,1)
dev.off()

# correlation between pdetected model 1 and model 2

pdf("~/ownCloud/coronavirus/lockdown/August2020/pdetection_pre_post_model1_model2.pdf", width = 4 * 1.2, height = 4 * 1.2)
par(mar = c(4,4,1,1))
# first date:
plot(NULL, xlim = c(0, 0.1), ylim = c(0, 0.1), xlab = "detection first date model 1", ylab = "detection first date model 2")
for(cc in unique(all_pars$continent)){
  segments(x0 = all_pars$pdetected_1_firstdate[subs[[cc]]], x1 = all_pars$pdetected_1_firstdate[subs[[cc]]], y0 = all_pars$pdetected_2_firstdate_lower[subs[[cc]]], y1 = all_pars$pdetected_2_firstdate_upper[subs[[cc]]], col = colset_light[cc])
  segments(x0 = all_pars$pdetected_1_firstdate_lower[subs[[cc]]], x1 = all_pars$pdetected_1_firstdate_upper[subs[[cc]]], y0 = all_pars$pdetected_2_firstdate[subs[[cc]]], y1 = all_pars$pdetected_2_firstdate[subs[[cc]]], col = colset_light[cc])
}
for(cc in unique(all_pars$continent))points(all_pars$pdetected_1_firstdate[subs[[cc]]], all_pars$pdetected_2_firstdate[subs[[cc]]], pch = 20, type = "p", col = colset[cc])
abline(0,1)

# last date
plot(NULL, xlim = c(0, 1), ylim = c(0, 1), xlab = "detection last date model 1", ylab = "detection last date model 2")
for(cc in unique(all_pars$continent)){
  segments(x0 = all_pars$pdetected_1_lastdate[subs[[cc]]], x1 = all_pars$pdetected_1_lastdate[subs[[cc]]], y0 = all_pars$pdetected_2_lastdate_lower[subs[[cc]]], y1 = all_pars$pdetected_2_lastdate_upper[subs[[cc]]], col = colset_light[cc])
  segments(x0 = all_pars$pdetected_1_lastdate_lower[subs[[cc]]], x1 = all_pars$pdetected_1_lastdate_upper[subs[[cc]]], y0 = all_pars$pdetected_2_lastdate[subs[[cc]]], y1 = all_pars$pdetected_2_lastdate[subs[[cc]]], col = colset_light[cc])
}
for(cc in unique(all_pars$continent))points(all_pars$pdetected_1_lastdate[subs[[cc]]], all_pars$pdetected_2_lastdate[subs[[cc]]], pch = 20, type = "p", col = colset[cc])
abline(0,1)
dev.off()

all_pars$country[which(all_pars$pdetected_1_lastdate  < 0.05)]
all_pars$country[which(all_pars$pdetected_2_lastdate  < 0.05)]

all_pars$country[which(all_pars$pdetected_1_lastdate  > 0.5)]
all_pars$country[which(all_pars$pdetected_2_lastdate  > 0.5)]

# REDUCTION IN TRANSMISSION CAUSED BY REDUCTION IN TRANSMISSIBILITY, DETECTION, IMMUNITY
# mean reduction in transmission when detection
detect_factor <- 1-sum(sapply(1:max_age, function(td) tit[td] * sum(si[1:td])))
colset2 <- RColorBrewer::brewer.pal(n = 3, name = "Set1")
colset2 <- sapply(colset2, col2rgb)/256
colset2 <- apply(colset2, 2, function(vec) rgb(vec[1], vec[2], vec[3], alpha = 0.5))
pdf("~/ownCloud/coronavirus/lockdown/August2020/reductions_transmission_3factors.pdf", width = 4 * 1.2, height = 3 * 1.2)
breaks <- seq(0, 10, 0.1)
par(mar= c(4,4,1,1))
hist(1 - all_pars$Rpost_2_MCMC / all_pars$Rpre_2_MCMC, breaks = breaks, freq = F, xlim = c(0, 1), col = colset2[2], ylim = c(0, 10), ylab = "density", main = NULL, xlab = "reduction in transmission", las = 1)
hist(detect_factor * all_pars$pdetected_2_lastdate, breaks = breaks, freq = F, xlim = c(0, 1), add = T, col = colset2[1])
hist(all_pars$total_infected2 / all_pars$total_pop, breaks = breaks, freq = F, xlim = c(0, 1), add = T, col = colset2[3])
legend("topright", legend = c("reduced transmissibility", "case isolation", "immunity"), fill = colset2[c(2,1,3)], bty = "n")
dev.off()

# PROBABILITY OF DETECTION AS A FUNCTION OF TOTAL NUMBER OF TESTS / TOTAL POP
pdf("~/ownCloud/coronavirus/lockdown/August2020/pdetection_tests_per_inhabitants.pdf", width = 4 * 1.2, height = 3 * 1.2)
par(mar = c(4,4,1,1))
plot(NULL, pch = 20, las = 1, xlab = "daily tests per capita", ylab = "final probability of detection", xlim = c(0, 0.003), ylim = c(0, 1))
all_pars$tests_per_inhabitant <- all_pars$ntests_lastdate/all_pars$total_pop
sub <- which(!is.na(all_pars$tests_per_inhabitant))
sub2 <- which(!is.na(all_pars$tests_per_inhabitant) & all_pars$daily_deaths_over50)
sub3 <- which(!is.na(all_pars$tests_per_inhabitant) & !grepl("_US", all_pars$country))
w <- 1/(all_pars$pdetected_2_lastdate_upper[sub] - all_pars$pdetected_2_lastdate_lower[sub]) # weights
summary(lm0_tests <- lm(pdetected_2_lastdate ~ tests_per_inhabitant, data = all_pars[sub, ], weights = w))
summary(lm0_2 <- lm(pdetected_2_lastdate ~ tests_per_inhabitant, data = all_pars[sub2, ]))
summary(lm0_3 <- lm(pdetected_2_lastdate ~ tests_per_inhabitant, data = all_pars[sub3, ]))
xx <- seq(0, 0.003, 0.0001)
conf_interval <- predict(object = lm0_tests, newdata = data.frame(tests_per_inhabitant = xx), interval = "confidence", level = 0.95)
polygon(x = c(xx, rev(xx)), y = c(conf_interval[,2], rev(conf_interval[,3])), border = NA, col = "lightgray")
abline(lm0_tests$coefficients[1], lm0_tests$coefficients[2], lwd = 3)
#abline(lm0_2$coefficients[1], lm0_2$coefficients[2], lwd = 3, col = "red")
#abline(lm0_3$coefficients[1], lm0_3$coefficients[2], lwd = 3, col = "blue")
for(cc in unique(all_pars$continent)){
  x <- subs[[cc]]
  segments(x0 = (all_pars$ntests_lastdate/all_pars$total_pop)[x], y0 = all_pars$pdetected_2_lastdate_lower[x], x1 = (all_pars$ntests_lastdate/all_pars$total_pop)[x], y1 = all_pars$pdetected_2_lastdate_upper[x], col = colset_light[cc])
  points((all_pars$ntests_lastdate/all_pars$total_pop)[x], all_pars$pdetected_2_lastdate[x], pch = 20, las = 1, xlab = "tests per inhabitant", ylab = "final probability of detection", col = colset[cc])
}
#text((all_pars$ntests_lastdate/all_pars$total_pop)[sub], all_pars$pdetected_2_lastdate[sub], all_pars$country[sub])
dev.off()

summary(lm0_tests)
length(fitted(lm0_tests))

# ORDERED NUMBER OF DEATHS AVERTED
pdf("~/ownCloud/coronavirus/lockdown/August2020/ordered_deaths_2.pdf", width = 8 * 1.2, height = 4 * 1.2)
plot(NULL, xlim =  c(0, 80), ylim = c(2, 6), ylab = "log10 deaths averted", axes = F, xlab = "")
abline(h = 3, col = "gray")
abline(h = 4, col = "gray")
abline(h = 5, col = "gray")
par(mar = c(0,4,1,0))
axis(side = 2, at = seq(2, 6, 1), las = 1)
oo <- order(all_pars$deaths_averted)
for(cc in unique(all_pars$continent)){
  x <- subs[[cc]]
  xo <- match(x, oo)
  segments(x0 = xo, x1 = xo, y0 = log10(all_pars$deaths_averted_lower[x]), y1 = log10(all_pars$deaths_averted_upper[x]), col = colset_light[cc])
  points(xo, log10(all_pars$deaths_averted[x]), pch = 20, col = colset[cc])
}
text(x = 1:nf, y = 2, labels = all_pars$pretty_country[oo], srt = -45, cex = 0.5, adj = 0) #, col = colset[all_pars$continent][oo])
dev.off()

# ORDERED R AND ORDER P

pdf("~/ownCloud/coronavirus/lockdown/August2020/ordered_R_2.pdf", width = 8 * 1.2, height = 4 * 1.2)
plot(NULL, xlim =  c(0, 80), ylim = c(-1, 2), ylab = "R final", axes = F, xlab = "")
par(mar = c(0,4,1,0))
axis(side = 2, at = seq(0, 2, 0.5), las = 1)
oo <- order(all_pars$Rpost_2_MCMC)
for(cc in unique(all_pars$continent)){
  x <- subs[[cc]]
  xo <- match(x, oo)
  segments(x0 = xo, x1 = xo, y0 = all_pars$Rpost_2_lower[x], y1 = all_pars$Rpost_2_upper[x], col = colset_light[cc])
  points(xo, all_pars$Rpost_2_MCMC[x], pch = 20, col = colset[cc])
}
abline(h = 1, lty  = 2)
text(x = 1:nf, y = -0.5, labels = all_pars$pretty_country[oo], srt = -45, cex = 0.5, adj = 0) #, col = colset[all_pars$continent][oo])
dev.off()

pdf("~/ownCloud/coronavirus/lockdown/August2020/ordered_p_final_1.pdf", width = 8 * 1.2, height = 4 * 1.2)
plot(NULL, xlim =  c(0, 80), ylim = c(-0.5, 1), ylab = "c final", axes = F, xlab = "", las = 1)
par(mar = c(0,4,1,0))
axis(side = 2, at = seq(0, 1, 0.2), las = 1)
oo <- order(all_pars$pdetected_1_lastdate)
for(cc in unique(all_pars$continent)){
  x <- subs[[cc]]
  xo <- match(x, oo)
  segments(x0 = xo, x1 = xo, y0 = all_pars$pdetected_1_lastdate_lower[x], y1 = all_pars$pdetected_1_lastdate_upper[x], col = colset_light[cc])
  points(xo, all_pars$pdetected_1_lastdate[x], pch = 20, col = colset[cc])
}
abline(h = 0, lty  = 1)
abline(h = 1, lty  = 1)
text(x = 1:nf, y = -0.1, labels = all_pars$pretty_country[oo], srt = -45, cex = 0.5, adj = 0, col = colset[all_pars$continent][oo])
dev.off()

pdf("~/ownCloud/coronavirus/lockdown/August2020/ordered_p_final_2.pdf", width = 8 * 1.2, height = 4 * 1.2)
plot(NULL, xlim =  c(0, 80), ylim = c(-0.5, 1), ylab = "c final", axes = F, xlab = "", las = 1)
par(mar = c(0,4,1,0))
axis(side = 2, at = seq(0, 1, 0.2), las = 1)
oo <- order(all_pars$pdetected_2_lastdate)
for(cc in unique(all_pars$continent)){
  x <- subs[[cc]]
  xo <- match(x, oo)
  segments(x0 = xo, x1 = xo, y0 = all_pars$pdetected_2_lastdate_lower[x], y1 = all_pars$pdetected_2_lastdate_upper[x], col = colset_light[cc])
  points(xo, all_pars$pdetected_2_lastdate[x], pch = 20, col = colset[cc])
}
abline(h = 0, lty  = 1)
abline(h = 1, lty  = 1)
text(x = 1:nf, y = -0.1, labels = all_pars$pretty_country[oo], srt = -45, cex = 0.5, adj = 0) #, col = colset[all_pars$continent][oo])
dev.off()


# R AND p THROUGH TIME

pdf("~/ownCloud/coronavirus/lockdown/August2020/R_and_p_through_time_2.pdf", width = 4 * 1.2, height = 3 * 1.2)

# proba of detection as a function of time:
par(mar = c(4,4,1,1))
plot(NULL, xlim = c(0, 130), ylim = c(0, 1), ylab = expression(c[t]), xlab = "date", las = 1, axes = F)
all_dates_num <- c(1, 32, 61, 92, 122, 153, 183)
all_dates <- c("01/01", "01/02", "01/03", "01/04", "01/05", "01/06", "01/07")
axis(side = 1, at = all_dates_num, labels = all_dates)
axis(side = 2, at = seq(0, 1, 0.2), las = 1)

x <- seq(0, 129, 1)
y <- list()
ymean <- rep(0, lastdate - (61 + 14) + 1) # vector to store the mean from 015/03 to lastdate
count <- rep(0, lastdate - (61 + 14) + 1)
for(i in 1:nf){
  myx <- all_pars$firstdate[i]:all_pars$lastdate[i]
  y[[i]] <- pdetected_smooth(tt = myx, par = unlist(all_pars[i, c("Rpre_2_MCMC", "Rpost_2_MCMC", "tmidpointR_2_MCMC", "kR_2_MCMC", "pmin_2_MCMC", "pmax_2_MCMC", "kp_2_MCMC", "tmidpointp_2_MCMC")]), betafun = 2)
  points(myx, y[[i]], type = "l", lwd = 3, col = colset_light[all_pars$continent][i])
  idx_in_y <- match(myx, 75:lastdate); idx_in_y <- idx_in_y[!is.na(idx_in_y)]
  ymean[idx_in_y] <- ymean[idx_in_y] + y[[i]][which(myx %in% 75:lastdate)]
  count[idx_in_y] <- count[idx_in_y] + 1
}
ymean <- ymean/count
cat("overall mean p detected: ", range(ymean), "\n")
points(75:lastdate, ymean, type = "l", lwd = 3, col = "black")

# R as a function of time:
par(mar = c(4,4,1,1))
plot(NULL, xlim = c(0, 130), ylim = c(0, 10), ylab = expression(R[t]), xlab = "date", las = 1, axes = F)
all_dates_num <- c(1, 32, 61, 92, 122, 153, 183)
all_dates <- c("01/01", "01/02", "01/03", "01/04", "01/05", "01/06", "01/07")
axis(side = 1, at = all_dates_num, labels = all_dates)
axis(side = 2, at = seq(0, 10, 2), las = 1)

x <- seq(0, 130, 1)
y <- list()
ymean <- rep(0, lastdate - 75 + 1) # vector to store the mean from 15/03 to lastdate
count <- rep(0, lastdate - 75 + 1)
for(i in 1:nf) {
  myx <- all_pars$firstdate[i]:all_pars$lastdate[i]
  y[[i]] <- all_pars[i, c("Rpre_2_MCMC")] * relativeR_smooth(tt = myx, par = unlist(all_pars[i, c("Rpre_2_MCMC", "Rpost_2_MCMC", "tmidpointR_2_MCMC", "kR_2_MCMC")]))
  points(myx, y[[i]], type = "l", lwd = 3, col = colset_light[all_pars$continent][i])
  idx_in_y <- match(myx, 75:lastdate); idx_in_y <- idx_in_y[!is.na(idx_in_y)]
  ymean[idx_in_y] <- ymean[idx_in_y] + y[[i]][which(myx %in% 75:lastdate)]
  count[idx_in_y] <- count[idx_in_y] + 1
}
ymean <- ymean/count
cat("overall mean R: ", range(ymean), "\n")
points(75:lastdate, ymean, type = "l", lwd = 3, col = "black")
abline(h = 1, lty = 2)

dev.off()

write.csv(x = all_pars, file = "~/ownCloud/coronavirus/lockdown/August2020/all_pars.csv", row.names = F)

#####################                                                MOBILITY INFORMATION                                 ##################### 

# define ratios pre-post lockdown:
all_pars$ratio_2_MCMC <- all_pars$R_2_lastdate / alls_pars$R_2_firstdate
all_pars$ratio_1_MCMC <- all_pars$Rpost_1_MCMC / all_pars$Rpre_1_MCMC
get_lm <- function(xvec, yvec, cols, collight){
  lm0 <- lm(yvec ~ xvec)
  co <- lm0$coefficients
  xx <- seq(min(xvec, na.rm = T), max(xvec, na.rm = T), 1)
  yy <- co[1] + co[2] * xx
  conf_interval <- predict(lm0, newdata=data.frame(xvec=xx), interval="confidence", level = 0.95)
  collight <- col2rgb(collight)/256
  polygon(x = c(xx, rev(xx)), y = c(conf_interval[,2], rev(conf_interval[,3])), border = NA, col = rgb(collight[1], collight[2], collight[3], alpha = 0.2))
  points(xx, yy, col = cols, type = "l", lwd = 3)

  return(lm0)
}
mob <- read.csv("~/ownCloud/coronavirus/lockdown/data/mobilities.csv")
mobdata <- read.csv("~/ownCloud/coronavirus/lockdown/data/mobilities_data.csv")

# get dates of mobility info:
mob_dates <- sapply(names(mobdata), function(nn) if(grepl(pattern = "X", nn)) return(as.numeric(gsub(pattern = "X", replacement = "", nn))) else return(NA))
mob_dates <- mob_dates[!is.na(mob_dates)]

#mob <- mob[match(all_pars$country, mob$country),]
stopifnot(all(mob$country == all_pars$country))

sub <- grep(pattern = "_US", x = all_pars$country) # all_pars$continent == "North America" #grep(pattern = "_US", x = all_pars$country)
sub2 <- all_pars$continent == "Europe + Russia" # c("Austria", "Belgium", "Czechia",  "Denmark", "France", "Germany", "Hungary", "Ireland", "Italy", "Netherlands", "Norway",        
                                # "Poland", "Portugal", "Romania", "Russia", "Serbia", "Spain", "Sweden", "Switzerland", "Ukraine", "United_Kingdom")
sub3 <- all_pars$continent == "Asia"
sub4 <- all_pars$continent == "South / Central America"

yvar <- "ratio_1_MCMC" # it is working better with model 1; I think that is because first R is not inferred as well
xlims = list(c(-90, -20), c(7, 32), c(-60, 0), c(-100, 0), c(-100, 100))
ylims <- c(0, 1)
xvars <- c("y1_retail", "y1_residential", "y1_grocery", "y1_transit", "y1_parks")
mains <- c("retail", "residential", "grocery", "transit", "parks")

par(mfrow = c(2, 2))
for(k in 1:4){
  xvar <- xvars[k]
  xlim <- xlims[[k]]
  print(xvar)
  par(mar = c(4, 4, 2, 1))
  plot(NULL, ylim = ylims, xlim = xlim, ylab = "R post / R pre", xlab = "change in mobility", col = colset["North America"], las = 1, main = mains[k])
  
  lm_US <- get_lm(xvec = mob[sub, xvar], yvec = all_pars[sub, yvar], cols = colset["North America"], collight = colset_light["North America"])
  lm_EU <- get_lm(xvec = mob[sub2, xvar], yvec = all_pars[sub2, yvar], cols = colset["Europe + Russia"], collight = colset["Europe + Russia"])
  #lm_Asia <- get_lm(xvec = mob[sub3, xvar], yvec = all_pars[sub3, yvar], cols = colset["Asia"], collight = colset["Asia"])
  #lm_SA <- get_lm(xvec = mob[sub4, xvar], yvec = all_pars[sub4, yvar], cols = colset["South / Central America"], collight = colset["South / Central America"])
  
  print(summary(lm_US))
  print(summary(lm_EU))
  
  points(mob[sub, xvar], all_pars[sub, yvar], pch = 20, las  = 1, ylim = c(0, 0.8), xlim = xlim, ylab = "R post / R pre", xlab = "change in mobility", col = colset["North America"])
  text(mob[sub, xvar], all_pars[sub, yvar] + 0.005, all_pars$pretty_country[sub], cex = 0.8, col = colset["North America"])
  
  points(mob[sub2, xvar], all_pars[sub2, yvar], pch = 20, col = colset["Europe + Russia"])
  text(mob[sub2, xvar], all_pars[sub2, yvar] + 0.005, all_pars[sub2, "pretty_country"], col = colset["Europe + Russia"], cex = 0.8)
  
  # points(mob[sub3, xvar], all_pars[sub3, yvar], pch = 20, col = colset["Asia"])
  # text(mob[sub3, xvar], all_pars[sub3, yvar] + 0.005, all_pars[sub3, "pretty_country"], col = colset["Asia"], cex = 0.8)
  # 
  # points(mob[sub4, xvar], all_pars[sub4, yvar], pch = 20, col = colset["South / Central America"])
  # text(mob[sub4, xvar], all_pars[sub4, yvar] + 0.005, all_pars[sub4, "pretty_country"], col = colset["South / Central America"], cex = 0.8)
  # 
}



all_pars$country[which(all_pars$Rpre_2_MCMC > 6.5 & all_pars$Rpre_1_MCMC < 2.1)]
all_pars$country[which(all_pars$R_2_firstdate > 6 & all_pars$Rpre_1_MCMC < 2.1)]


# maybe that's because some of the states do not have much info prior to lockdown, so what if we select those with good info prior to lockdown
all_pars$datelockdown - all_pars$firstdate > 10 # actually no




# create a table mob_R with mobility info and estimated transmissibility by country and date
mob_R <- as.data.frame(expand.grid(mob_dates, all_pars$matching_country)) # data.frame(matrix(NA, nrow = nf * length(mob_dates), ncol = 6 + 1))
names(mob_R) <- c("date", "country")                    
mob_R$mob_grocery <- NA
mob_R$mob_parks <- NA
mob_R$mob_residential <- NA
mob_R$mob_retail <- NA
mob_R$mob_transit <- NA
mob_R$mob_work <- NA
mob_R$R <- NA

for(i in 1:nf){
  for(j in c("grocery", "parks", "residential", "retail", "transit", "work")){
    idx_mob <- which(mobdata$country==all_pars$matching_country[i] & mobdata$type == j)
    if(length(idx_mob) > 0){
      relativemob <- unlist(mobdata[idx_mob, paste0("X", mob_dates)])
      mob_R[which(mob_R$country == all_pars$matching_country[i]), paste0("mob_", j)] <- relativemob
      mob_R[which(mob_R$country == all_pars$matching_country[i]), "R"] <- unname(sapply(mob_dates, function(tt) relativeR_smooth(tt, par = unlist(all_pars[i, c("Rpre_2_MCMC", "Rpost_2_MCMC", "tmidpointR_2_MCMC", "kR_2_MCMC")]))) - 1)
    }
  }
}


View(mob_R)
mob_R$R <- signif(mob_R$R, 3)
library(lme4)

sub <- !is.na(mob_R$mob_grocery) & !is.na(mob_R$mob_parks) & !is.na(mob_R$mob_residential) & !is.na(mob_R$mob_retail) & !is.na(mob_R$mob_transit) & !is.na(mob_R$mob_work) & !is.na(mob_R$R)

# coutries not included in the mobility analysis
ld$matching_country[!ld$matching_country %in% mob_R[sub, "country"]]

summary(lm(R ~ mob_grocery + mob_parks + mob_residential + mob_retail + mob_transit + mob_work, data = mob_R[sub, ]))
# full random-effect model
summary(lm00 <- lmer(R ~ mob_transit + (mob_transit|country), data = mob_R[sub, ]))
summary(lm0 <- lmer(R ~ 0 + mob_grocery + mob_parks + mob_residential + mob_retail + mob_transit + mob_work + (mob_grocery|country) + (mob_parks|country) + (mob_residential|country) + (mob_retail|country) + (mob_transit|country) + (mob_work|country), data = mob_R[sub, ]))
summary(lm1 <- lmer(R ~ mob_grocery + mob_parks + mob_residential + mob_retail + mob_transit + mob_work + (mob_grocery|country) + (mob_parks|country) + (mob_residential|country) + (mob_retail|country) + (mob_transit|country) + (mob_work|country), data = mob_R[sub, ]))
AIC(lm0, lm1)
Rpre <- predict(lm1)
meanR <- mean(mob_R$R[sub])
1 - sum((mob_R$R[sub] - Rpre)^2) / sum((mob_R$R[sub] - meanR)^2)

# model simplifications:
summary(lm2 <- lmer(R ~ mob_grocery + mob_parks + mob_residential + mob_retail + mob_transit +            (mob_grocery|country) + (mob_parks|country) + (mob_residential|country) + (mob_retail|country) + (mob_transit|country)                     , data = mob_R[sub, ]))
summary(lm3 <- lmer(R ~ mob_grocery + mob_parks + mob_residential +              mob_transit + mob_work + (mob_grocery|country) + (mob_parks|country) + (mob_residential|country) +                        (mob_transit|country) + (mob_work|country), data = mob_R[sub, ]))
summary(lm4 <- lmer(R ~               mob_parks + mob_residential + mob_retail + mob_transit + mob_work +                         (mob_parks|country) + (mob_residential|country) + (mob_retail|country) + (mob_transit|country) + (mob_work|country), data = mob_R[sub, ]))
summary(lm5 <- lmer(R ~ mob_grocery +             mob_residential + mob_retail + mob_transit + mob_work + (mob_grocery|country)                       + (mob_residential|country) + (mob_retail|country) + (mob_transit|country) + (mob_work|country), data = mob_R[sub, ]))
summary(lm6 <- lmer(R ~ mob_grocery + mob_parks + mob_residential + mob_retail +               mob_work + (mob_grocery|country) + (mob_parks|country) + (mob_residential|country) + (mob_retail|country)                         + (mob_work|country), data = mob_R[sub, ]))
summary(lm7 <- lmer(R ~ mob_grocery + mob_parks +                 + mob_retail + mob_transit + mob_work + (mob_grocery|country) + (mob_parks|country) +                             (mob_retail|country) + (mob_transit|country) + (mob_work|country), data = mob_R[sub, ]))


ranef(lm1)
fixef(lm1)


# the more complex model is always favoured
# anova(lm0, lm1)
# anova(lm1, lm2)
# anova(lm1, lm3)
# anova(lm1, lm4)
# anova(lm1, lm5)
# anova(lm1, lm6)
# anova(lm1, lm7)


# add R_t predicted from mobility data
mob_R$pred1 <- NA
mob_R$pred1[sub] <- predict(object = lm1)
plot(mob_R$pred1[sub], mob_R$R[sub], pch = 20)
abline(0, 1, col = "red", lwd = 3)

SStot <- sum((mob_R$R[sub] - mean(mob_R$R[sub]))^2)
SSres1 <- sum((mob_R$R[sub] - mob_R$pred1[sub])^2)
Rsquared1 <- 1 - SSres1/SStot

# fit separately in US and EU
sub_US <- !is.na(mob_R$mob_grocery) & !is.na(mob_R$mob_parks) & !is.na(mob_R$mob_residential) & !is.na(mob_R$mob_retail) & !is.na(mob_R$mob_transit) & !is.na(mob_R$mob_work) & !is.na(mob_R$R) & mob_R$country %in% state.name
summary(lm1_US <- lmer(R ~ mob_grocery + mob_parks + mob_residential + mob_retail + mob_transit + mob_work + (mob_grocery|country) + (mob_parks|country) + (mob_residential|country) + (mob_retail|country) + (mob_transit|country) + (mob_work|country), data = mob_R[sub_US, ]))

sub_EU <- !is.na(mob_R$mob_grocery) & !is.na(mob_R$mob_parks) & !is.na(mob_R$mob_residential) & !is.na(mob_R$mob_retail) & !is.na(mob_R$mob_transit) & !is.na(mob_R$mob_work) & !is.na(mob_R$R) & mob_R$country %in% c("Austria", "Belgium", "Czechia", "Denmark", "France", "Germany", "Hungary", "Ireland", "Italy", "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Russia", "Serbia", "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom")
summary(lm1_EU <- lmer(R ~ mob_grocery + mob_parks + mob_residential + mob_retail + mob_transit + mob_work + (mob_grocery|country) + (mob_parks|country) + (mob_residential|country) + (mob_retail|country) + (mob_transit|country) + (mob_work|country), data = mob_R[sub_EU, ]))

sub_SA <- !is.na(mob_R$mob_grocery) & !is.na(mob_R$mob_parks) & !is.na(mob_R$mob_residential) & !is.na(mob_R$mob_retail) & !is.na(mob_R$mob_transit) & !is.na(mob_R$mob_work) & !is.na(mob_R$R) & mob_R$country %in% c("Argentina", "Brazil", "Chile", "Colombia", "Dominican_Republic", "Ecuador", "Peru")
summary(lm1_SA <- lmer(R ~ mob_grocery + mob_parks + mob_residential + mob_retail + mob_transit + mob_work + (mob_grocery|country) + (mob_parks|country) + (mob_residential|country) + (mob_retail|country) + (mob_transit|country) + (mob_work|country), data = mob_R[sub_SA, ]))

if(!file.exists("~/ownCloud/coronavirus/lockdown/August2020/lm_mobility_confidence_intervals.csv")){
  parnames <- c("mob_grocery", "mob_parks", "mob_transit", "mob_retail", "mob_residential", "mob_work")
  lm_ci1    <- confint.merMod(lm1, parm = parnames, level = 0.95, method="profile")
  lm_ci1_EU <- confint.merMod(lm1_EU, parm = parnames, level = 0.95, method="boot")
  lm_ci1_US <- confint.merMod(lm1_US, parm = parnames, level = 0.95, method="profile")
  #lm_ci1_SA <- confint.merMod(lm1_SA, parm = parnames, level = 0.95, method="profile")
  
  # create a table of confidence intervals
  get_char_int <- function(mylm, myci){
    paste0(round(fixef(mylm)[mypar], 2), " [", round(myci[mypar, "2.5 %"], 2), " ; ", round(myci[mypar, "97.5 %"], 2), "]")
  }
  mat_res <- matrix(NA, nrow = 3, ncol = length(parnames))
  for(mypar in parnames){
    mat_res[1, which(parnames==mypar)] <- get_char_int(lm1, lm_ci1)
    mat_res[2, which(parnames==mypar)] <- get_char_int(lm1_EU, lm_ci1_EU)
    mat_res[3, which(parnames==mypar)] <- get_char_int(lm1_US, lm_ci1_US)
  }
  colnames(mat_res) <- parnames
  mat_res <- data.frame(mat_res)
  mat_res$model <- c("full", "EU", "US")
  
  write.csv(x = mat_res, file = "~/ownCloud/coronavirus/lockdown/August2020/lm_mobility_confidence_intervals.csv", row.names = F)
  
}


fixef(lm1)[c("mob_grocery", "mob_parks", "mob_residential", "mob_retail", "mob_transit", "mob_work")]
fixef(lm1_US)[c("mob_grocery", "mob_parks", "mob_residential", "mob_retail", "mob_transit", "mob_work")]
fixef(lm1_EU)[c("mob_grocery", "mob_parks", "mob_residential", "mob_retail", "mob_transit", "mob_work")]


fixef(lm00)
fixef(lm1)
fixef(lm1_EU)
fixef(lm1_US)
fixef(lm1_SA)

plot(fixef(lm1_EU), fixef(lm1_US), pch = 20)
abline(h = 0)
abline(v = 0)


# plotting inferred R in blue together with R predicted from mobility data in red
for(cc in unique(mob_R$country)){
  sub <- which(mob_R$country == cc)
  plot(mob_R$date[sub], mob_R$R[sub], lwd= 3, col ="blue", type = "l", ylim = c(-1, 0.1), main = cc)
  points(mob_R$date[sub], mob_R$pred1[sub], lwd= 3, col ="red", type = "l")
}


# in general date lockdown correlates better than inflexion point transmission with inflexion point of mobility curve
where <- "work"
summary(lm(all_pars[,"tmidpointR_2_MCMC"] ~ mob[, paste0("c_", where)]))
summary(lm(all_pars[,"datelockdown"] ~ mob[, paste0("c_", where)]))


