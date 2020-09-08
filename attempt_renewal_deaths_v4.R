idx_country = as.numeric(commandArgs(trailingOnly=TRUE)[1])
#if(is.na(idx_country)) idx_country <- 48

if(getwd() == "/save_home/fblanquart/lockdown") masterdir <- "" else masterdir <- "~/ownCloud/coronavirus/lockdown/"
if(is.null(masterdir)) masterdir <- ""
source(paste0(masterdir, "functions_renewal_deaths_v4.R"))
library(numDeriv)
library(MASS)
library(utils)

# my attempt at coding the renewal equation and the resulting number of deaths:

max_age <- 60 # following infections up to age max_age

# 1. discretise the distribution of the serial interval (2 options here!)
# for the discretisation: the events that may happen in day 0 (in small numbers), we add to day 1; day i is the density integrated from i - 0.5 to i + 0.5
#si <- pweibull(1.5:(max_age+0.5), shape = 2.826, scale = 5.665) - pweibull(0.5:(max_age-0.5), shape = 2.826, scale = 5.665); si[1] <- si[1] + pweibull(0.5, shape = 2.826, scale = 5.665) # best parameters from Ferretti et al. Science 2020
mean_si <- 1.77296
sd_si <- 0.588135
si <- plnorm(1.5:(max_age+0.5), meanlog = mean_si, sdlog = sd_si) - plnorm(0.5:(max_age-0.5), meanlog = mean_si, sdlog = sd_si); si[1] <- si[1] + plnorm(0.5,  meanlog = mean_si, sdlog = sd_si) # lognormal with mean 7, sd 4.5 (as in Wu et al; as inferred from Ma et al by Antoine with lower sd; longer than Ferretti et al; ).
stopifnot(abs(sum(si)-1) < 1e-4)
#sum(si * 1:max_age)
#sqrt(sum(si * (1:max_age)^2) - sum(si * 1:max_age)^2)

# mean and median of weibull
5.665 * gamma(1 + 1/2.826)
5.665 * log(2)^(1/2.826)

# 2. discretise the distribution of time from infection to symptom onset
tio <- plnorm(1.5:(max_age+0.5), meanlog = 1.51804, sdlog = 0.471594) - plnorm(0.5:(max_age-0.5), meanlog = 1.51804, sdlog = 0.471594); tio[1] <- tio[1] + plnorm(0.5, meanlog = 1.525, sdlog = 0.6288) # Lauer et al Annals of Internal Medicine; or Linton et al (meanlog = 1.525, sdlog = 0.6288)
if(sum(tio) > 0.999) tio <- tio / sum(tio) else stop()

# 3. discretise the distribution of time from onset to death (2 options here! Wu et al. and Salje et al.)
tod <- pgamma(1.5:(max_age+0.5), shape = 5, rate = 1/4) - pgamma(0.5:(max_age-0.5), shape = 5, rate = 1/4); tod[1] <- tod[1] + pgamma(0.5, shape = 5, rate = 1/4)  # Wu et al Nature Medicine
if(sum(tod) > 0.999) tod <- tod / sum(tod) else stop()

# 3.5 time from infection to death is the convolution of the two
tid <- convolve(tio, rev(tod), type = "o")[1:max_age]
if(sum(tid) > 0.99) tid <- tid / sum(tid) else stop()

# 3.7 discretise the distribution of time from onset to test and deduce infection to test
tot <- pgamma(1.5:(max_age+0.5), shape = 0.69, rate = 0.31) - pgamma(0.5:(max_age-0.5), shape = 0.69, rate = 0.31); tot[1] <-tot[1] + pgamma(0.5, shape = 0.69, rate = 0.31)  # numerised data from Lauer et al.

tit <- convolve(tio, rev(tot), type = "o")[1:max_age]
if(sum(tit) > 0.99) tit <- tit / sum(tit) else stop()

# pdf("~/Dropbox (Personal)/COVIDsmile/inference_R0_before_after_highCFR.pdf", paper = "a4", width = 0, height = 0)
# par(mfrow = c(3, 1), las = 1, mar = c(4,4,1,1))

######## LOOP ON COUNTRY ########

#tmp <- ddply(ecdc, .(countriesAndTerritories), summarise, max.incidence.death = max(deaths))
#list1 <- as.character(tmp[which(tmp$max.incidence.death >= 10), "countriesAndTerritories"])

#tmp <- ddply(jh, .(Province_Country), summarise, max.incidence.death = max(deaths, na.rm = T))
#list2 <- as.character(tmp[which(tmp$max.incidence.death >= 10), "Province_Country"])
#list2 <- unname(sapply(list2, function(x){
#   y <- strsplit(x, "_")[[1]]
#   if(y[1] == y[2]) return(y[1]) else return(x)
# }))
# sort(unique(c(list1, list2)))
# write.csv(x = sort(c(list1, list2)), file = "~/ownCloud/coronavirus/lockdown_dates.csv", row.names = F)
# 
# tmp <- ddply(jh, .(Province_Country), summarise, max.incidence.death = max(deaths, na.rm = T))
# 

# NOTE: SWEDEN, THE NETHERLANDS, ICELAND DO NOT HAVE A LOCKDOWN. THE CHOSEN DATE OF LOCKDOWN IS ARBITRARY (ICELAND = BAN OF > 20)
#pdf("~/ownCloud/coronavirus/figures_cases_V2.pdf", width = 8 * 2, height = 6)
#par(mar = c(4,4,1,1), mfrow = c(1, 2))
#all_res_country <- c()
#for(idx_country in 1:nrow(ld)){
print("getting data for the country...")
print(idx_country)

  mycountry <- as.character(ld$country[idx_country])
  mydataset <- ld$preferred_data[idx_country]
  my_use_tests  <- ld$use_tests[idx_country]
  
  print(mycountry)
  # compute IFR for that country
  ifr <- sum(age[which(age$country == mycountry), c("age_0", "age_10", "age_20", "age_30", "age_40", "age_50", "age_60", "age_70", "age_80")] * all_ifr$IFR) / age[which(age$country == mycountry), "total"] / 100 # can be adjusted with age distribution + age-dependent CFR in Wu et al. or Verity et al.
  mytotal_pop <- age[which(age$country == mycountry), "total"]
  
  if(mydataset == "ecdc"){
    # when retrieving data from ECDC:
    sub_tab <- ecdc[which(ecdc$countriesAndTerritories == mycountry),]
  }
  if(mydataset == "jh"){
    # when retrieving data from JH:
    sub_tab <- jh[which(jh$Province_Country == mycountry),]
  }
  if(mydataset == "fr"){
    # when retrieving data from JH:
    sub_tab <- fr
  }
  if(mydataset == "us"){
    # when retrieving data from JH:
    sub_tab <- us[which(paste0(us$state, "_US") == mycountry), c("date", "cases", "deaths", "ntests")]
  }
  if(mydataset == "it") sub_tab <- it
  
  sub_tab <- sub_tab[sub_tab$date <= 129 , ] # stop at day 129 # as.numeric(strftime(Sys.time(), format = "%j")) - 5, ]
 
  if(!all(sub_tab$cases[!is.na(sub_tab$cases)] >= 0)){
    cat(sum(which(sub_tab$cases < 0), na.rm = T), " cases set to 0\ndates", sub_tab$date[which(sub_tab$cases < 0)], "\nvalue", sub_tab$cases[which(sub_tab$cases < 0)])
    sub_tab$cases[sub_tab$cases < 0] <- 0
  }
  if(!all(sub_tab$deaths[!is.na(sub_tab$deaths)] >= 0)){
    cat(length(which(sub_tab$deaths < 0)), " deaths set to 0\ndates", sub_tab$date[which(sub_tab$deaths < 0)], "\nvalue", sub_tab$deaths[which(sub_tab$deaths < 0)])
    sub_tab$deaths[sub_tab$deaths < 0] <- 0
  }
  stopifnot(all(sub_tab$deaths[!is.na(sub_tab$deaths)] >= 0))
  
  # then order by date and get first date with at least 5 deaths
  sub_tab <- sub_tab[order(sub_tab$date), ]
  date5deaths <- get_date_5deaths(epidata = sub_tab) #sub_tab$date[which(sub_tab$deaths >= 10)][1]
  if(is.na(date5deaths)){
    date5deaths <- get_date_5deaths(epidata = sub_tab, simple_option = T)
  }
  if(is.na(date5deaths)) stop("5 deaths not reached according to smooth spline")
  datelockdown <- ld$date_lockdown[which(ld$country==mycountry)] # date of lockdown
  idx_data <- sub_tab$date[which(sub_tab$cases >= 5 & sub_tab$deaths >= 0)]
  lastdate <- max(idx_data, na.rm = T)
  firstdate <- min(idx_data, na.rm = T)

  # set tmax
  if(mycountry == "Hubei_China"){
    tmax <- 220
    sub_tab <- sub_tab[sub_tab$date <= 106, ] # cut at 15th April the Hubei data, afterward reports become irregular
  } else {
    tmax <- 200
  }
  
  # R0 initial guess
  # rinitialguess <- (1/10) * log(sum(sub_tab$cases[which(sub_tab$date <= firstdate+10)], na.rm = T) / sum(sub_tab$cases[which(sub_tab$date <= firstdate)], na.rm = T))
  # rinitialguess2 <- (1/10) * log(sum(sub_tab$cases[which(sub_tab$date <= date5deaths+10)], na.rm = T) / sum(sub_tab$cases[which(sub_tab$date <= date5deaths)], na.rm = T))
  # R0initialguess <- c(1 /  sum(si * exp(-c(rinitialguess * 1:max_age))), 1 /  sum(si * exp(-c(rinitialguess2 * 1:max_age))))
  # R0initialguess <- max(R0initialguess)
  # 
  # if(is.na(R0initialguess) | is.infinite(R0initialguess) | R0initialguess < 1 | R0initialguess > 10) R0initialguess <- 3
  # 
  # optimising likelihood and plot
  
  # lower_vec <- c(1.5, 0.1, 0, 0, 0.00001,  0, 0)
  # upper_vec <- c(5, 5, 100, 100, 0.99, 1, 200)
  # npar <- 7
  # init_fun_0 <- function() runif(n = 3, min = lower_vec[1:3], max = upper_vec[1:3])
  # init_fun_1 <- function() runif(n = npar, min = lower_vec, max = upper_vec)
  # init_fun_2 <- function() runif(n = npar-1, min = lower_vec[1:(npar-1)], max = upper_vec[1:(npar-1)]) # with tests
  # 
  # FITTING JUST DEATHS & STEP FUNCTION FOR BETA:
  # R01 = par[0]; // in C referential for vectors
  # R02 = par[1]; // in C referential for vectors
  # size_nbinom_deaths <- par[3]
  lower_vec0 <- c(1.1, 0.1, 0)
  upper_vec0 <- c(10, 5, 100)
  npar0 <- 3
  init_fun_0 <- function() runif(n = npar0, min = lower_vec0, max = upper_vec0)
  opt0_0 <- optim.fun.repeated(n.repeats = 20, lik.fun = loglik_Rcpp, init.fun = init_fun_0, lower = lower_vec0, upper = upper_vec0, epidata = sub_tab, datelockdown = datelockdown, tmax = tmax, verbose = T, with_detected = F, use_tests = F, total_pop = mytotal_pop, betafun = 1)
  tmp0 <- loglik_Rcpp(par = opt0_0$pars, epidata = sub_tab, datelockdown = datelockdown, tmax, main = mycountry, with_detected = F, use_tests = F, total_pop = mytotal_pop, betafun = 1, return_what = 2, draw_plot = T)
  
  # NOW FITTING DEATHS AND CONFIRMED WITH TRANSMISSION = STEP FUNCTION
  # R01 = par[0]; // in C referential for vectors
  # R02 = par[1];
  # pdetectedmin = par[2];
  # pdetectedmax = par[3];
  # ktimecoeff = par[4];
  # tmidpoint = par[5];

  #init_fun_1 <- function() c(opt0_0$pars, runif(n = npar - 3, min = lower_vec[4:7], max = upper_vec[4:7]))
  lower_vec1 <- c(1.1, 0.1,  0.0001, 0.0001,      0.0001, 0,       0, 0)
  upper_vec1 <- c(10, 5,    0.001, 0.99,          5, 200,          100, 100)
  npar1 <- 8
  init_fun_1 <- function() runif(n = npar1, min = lower_vec1, max = upper_vec1)

  opt0_1 <- NULL
  while(is.null(opt0_1)){
    tryCatch(
      opt0_1 <- optim.fun.repeated(n.repeats = 50, lik.fun = loglik_Rcpp, init.fun = init_fun_1, lower = lower_vec1, upper = upper_vec1, epidata = sub_tab, datelockdown = datelockdown, tmax = tmax, verbose = T, with_detected = T, use_tests = F, total_pop = mytotal_pop, betafun = 1)
      , error = function(e){
        print(e)
      }
    )
  }
  tmp1 <- loglik_Rcpp(par = opt0_1$pars, epidata = sub_tab, datelockdown = datelockdown, tmax = tmax, main = mycountry, with_detected = T, use_tests = F, total_pop = mytotal_pop, betafun = 1, return_what = 2, draw_plot = T)
  
  
  # NOW FITTING DEATHS AND CONFIRMED WITH TRANSMISSION = SIGMOID DECLINING FUNCTION
  # R01 <- par[0]; // in C referential for vectors
  # R02 <- par[1];
  # tR0 = par[2];
  # kR0 = par[3];
  # pdtectedmin <- par[4]
  # pdetectedmax <- par[5]
  # ktimecoeff <- par[6]
  # tmidpoint <- par[7]
  # size_nbinom_deaths <- par[8]
  # size_nbinom_detected <- par[9]
  

  lower_vec2 <- c(1.1, 0.1, 0, 0.0001,       0.0001, 0.0001, 0.0001, 0,      0, 0)
  upper_vec2 <- c(10, 5, 200, 2,             0.001, 1,         5, 200,        100, 100)
  npar2 <- 10
  init_fun_2 <- function() c(min(opt0_1$pars[1], 8), opt0_1$pars[2], datelockdown + tmp1$delta - 1, runif(1, 0.001, 1), opt0_1$pars[3:8])  # start from close to the step function inferred previously
  #init_fun_2 <- function() runif(n = npar2, min = lower_vec2, max = upper_vec2)
  
  opt0_2 <- NULL
  while(is.null(opt0_2)){
    tryCatch(
      opt0_2 <- optim.fun.repeated(n.repeats = 50, lik.fun = loglik_Rcpp, init.fun = init_fun_2, lower = lower_vec2, upper = upper_vec2, epidata = sub_tab, datelockdown = datelockdown, tmax = tmax, verbose = T, with_detected = T, use_tests = F, total_pop = mytotal_pop, betafun = 2)
      , error = function(e){
        print(e)
      }
    )
  }
  tmp2 <- loglik_Rcpp(par = opt0_2$pars, epidata = sub_tab, datelockdown = datelockdown, tmax = tmax, main = mycountry, with_detected = T, use_tests = F, total_pop = mytotal_pop, betafun = 2, return_what = 2, draw_plot = T)

  # Iceland Gudbjartsson et al NEJM 2020 SARS-CoV-2 Iceland 0.6% test positive on April 1th-4th
  # France Salje et al re-estimate IFR at 0.5%; point of comparison is distribution of severity in Diamond princess
  # NYC ~ 20% seroprevalence apparently, 14% state wise -> very consistent with IFR 1%, sum(tmp1$sim_incidence)/20e6 is 14%!
  # note Austria has 0.33% [0.12-0.76] tested positive on April 4th-5th (Apr 1-6); check proba detection as a function of time -> 20 days according to FRENCH COVID
  # California Santa Clara Bendavid et al medRxiv 2020 antibody *seroprevalence* in this county 2.81% on April 1st (note problem with false positive)
  
  #################################################################
  
  # COMPUTE CI WITH MCMC
  if(T){
    max_iter <- 1000000
    proposal_cov_matrix0 <- diag(0.005 * (upper_vec0 - lower_vec0)^2)
    mcmc_sample0 <- run_MCMC_MH(start.value = opt0_0$pars, N.iter = max_iter, proposal.cov = proposal_cov_matrix0, posterior.fun = negativeloglik,
                             npar = npar0, lower = lower_vec0, upper = upper_vec0, verbose = T,
                             # arguments of the likelihood function:
                             epidata = sub_tab, datelockdown = datelockdown, tmax, main = mycountry, with_detected = F, use_tests = F, total_pop = mytotal_pop, betafun = 1
                             )
    #plot(mcmc_sample$all.lik, type = "l", ylim = c(-1000,-500))
    #ci_mcmc <- apply(mcmc_sample$chain[(0.2 * max_iter):max_iter,], 2, function(x) quantile(x, c(0.025, 0.975)))
    #all_res_country <- rbind(all_res_country, c(opt0_1$lik, opt0_1$pars, ci_mcmc[1,], ci_mcmc[2,]))
    
    proposal_cov_matrix1 <- diag(0.005 * (upper_vec1 - lower_vec1)^2)
    mcmc_sample1 <- run_MCMC_MH(start.value = opt0_1$pars, N.iter = max_iter, proposal.cov = proposal_cov_matrix1, posterior.fun = negativeloglik,
                                npar = npar1, lower = lower_vec1, upper = upper_vec1, verbose = T,
                                # arguments of the likelihood function:
                                epidata = sub_tab, datelockdown = datelockdown, tmax, main = mycountry, with_detected = T, use_tests = F, total_pop = mytotal_pop, betafun = 1
    )
    
    proposal_cov_matrix2 <- diag(0.001 * (upper_vec2 - lower_vec2)^2)
    mcmc_sample2 <- run_MCMC_MH(start.value = opt0_2$pars, N.iter = max_iter, proposal.cov = proposal_cov_matrix2, posterior.fun = negativeloglik,
                                npar = npar2, lower = lower_vec2, upper = upper_vec2, verbose = T,
                                # arguments of the likelihood function:
                                epidata = sub_tab, datelockdown = datelockdown, tmax, main = mycountry, with_detected = T, use_tests = F, total_pop = mytotal_pop, betafun = 2
    )
    
  }
  save(file = paste0(masterdir, mycountry, ".RData"), list = c(
                                                               "age", "all_ifr", "ecdc", "fr", "jh", "it", "us", "ld", "sub_tab",
                                                               "col1", "col2", "col3", "date5deaths", "datelockdown", "draw_plot_fun", "get_date_5deaths",
                                                               "ld", "loglik_Rcpp", "masterdir", "max_age", "mycountry",
                                                               "mydataset", "mytotal_pop", "negativeloglik", "nmkb", "npar0", "npar1", "npar2", "opt0_0", "opt0_1", "opt0_2", "optim.fun.repeated",
                                                               "ifr", "pdetected_smooth", "relativeR_smooth", "run_MCMC_MH", "simulate_Rcpp",
                                                               "si", "tid", "tio", "tit", "tod", "tot", "tmax", "tmp0", "tmp1", "tmp2", "tod", "tot",
                                                               "lower_vec0", "lower_vec1", "lower_vec2", "upper_vec0", "upper_vec1", "upper_vec2", "init_fun_0", "init_fun_1", "init_fun_2",
                                                               "proposal_cov_matrix0", "mcmc_sample0", "proposal_cov_matrix1", "mcmc_sample1", "proposal_cov_matrix2", "mcmc_sample2"
                                                               )
       )
#}
#dev.off()

#write.csv(x = all_res_country, file = "~/ownCloud/coronavirus/all_res_country.csv", row.names = F)
# dev.off()



