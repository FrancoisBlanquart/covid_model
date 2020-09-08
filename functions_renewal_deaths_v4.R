# list of files needed:
#if(getwd() == "/projet/extern/save/fblanquart/lockdown") masterdir <- "" else masterdir <- "~/ownCloud/coronavirus/lockdown/"
jhfile <- paste0(masterdir, "data/jh.csv")
jhcleaning <- paste0(masterdir, "clean_John_Hopkins.R")
ldfile <- paste0(masterdir, "data/Lockdown_dates - Sheet1.csv") #data/lockdown_dates.txt
agestructurefile <- paste0(masterdir, "data/age_structure_all_clean.csv")
ifrfile <- paste0(masterdir, "data/Verity_IFR.csv")
ecdcfile <- paste0(masterdir, "data/ECDC_data.csv")
frfile <- paste0(masterdir, "data/chiffres-cles_France_opencovid19-fr.csv") # from https://github.com/opencovid19-fr/data
usfile <- paste0(masterdir, "data/covidtracking_USA_daily.csv")
itfile <- paste0(masterdir, "data/italy.csv")
abbfile <- paste0(masterdir, "data/states_abbreviation.csv")
codefile <- paste0(masterdir, "simulate.cpp")

# UPDATE THE DATA:
if(data_update <- FALSE){
  
  # ECDC:
  tmp <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")
  write.csv(tmp, ecdcfile, row.names = F)
  
  # FRENCH:
  tmp <- read.csv("https://raw.githubusercontent.com/opencovid19-fr/data/master/dist/chiffres-cles.csv")
  write.csv(tmp, frfile, row.names = F)
  
  # JH:
  download.file("https://github.com/CSSEGISandData/COVID-19/archive/master.zip", destfile = paste0(masterdir, "data/master.zip"))
  unzip(zipfile = paste0(masterdir, "data/master.zip"), exdir = paste0(masterdir, "data"))
  source(jhcleaning)
  file.remove(paste0(masterdir, "data/master.zip"))
  
  # AMERICAN:
  tmp <- read.csv("https://covidtracking.com/api/v1/states/daily.csv") # https://covidtracking.com/api
  write.csv(x = tmp, file = usfile, row.names = F)
  
  # ITALIAN:
  tmp <- read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv")
  write.csv(tmp, itfile, row.names = F)

}

colset <- RColorBrewer::brewer.pal(n = 3, name = "Set1")
col1 <- colset[1]
col2 <- colset[2]
col3 <- colset[3]

stopifnot(all(file.exists(c(ldfile, agestructurefile, ifrfile, ecdcfile, jhfile, frfile, usfile, codefile))))

myintegrate <- function(f, tmin, tmax, deltat, param){ # my function to simply integrate a function with parameters "param"
  if(tmin == tmax) return(0)
  f <- match.fun(f)
  tmp <- sapply(seq(tmin, tmax, deltat), f, param = param)
  n <- length(tmp)
  weights <- rep(2, n); weights[1] <- 1; weights[n] <- 1
  return(sum(0.5 * (tmax-tmin)/(n-1) * weights * tmp))
}
process_date_ecdc <- function(dd){ # sorry for terrible coding
  if(dd == "31/12/2019") return("0")
  if(grepl(pattern = "\\/", x = dd)){
    dd <- strsplit(as.character(dd), split = "\\/")[[1]]
  } else {
    if(grepl(pattern = "-", x = dd)){
      dd <- rev(strsplit(as.character(dd), split = "-")[[1]])
    }
  }
  if(dd[3]!="2020")stop("date in 2020 only")
  if(dd[2] == "01") return(as.numeric(dd[1]))
  if(dd[2] == "02") return(31 + as.numeric(dd[1]))
  if(dd[2] == "03") return(31 + 29 + as.numeric(dd[1]))
  if(dd[2] == "04") return(31 + 29 + 31 + as.numeric(dd[1]))
  if(dd[2] == "05") return(31 + 29 + 31 + 30 + as.numeric(dd[1]))
  if(dd[2] == "06") return(31 + 29 + 31 + 30 + 31 + as.numeric(dd[1]))
  stop("Jan, Feb, Mar, April, May, June 2020 only")
}
process_date_usa <- function(dd){
  dd <- as.character(dd)
  day <- substr(dd, 7, 8)
  month <- substr(dd, 5, 6)
  year <- substr(dd, 1, 4)
  #return(paste(year, month, day, sep = "-"))
  if(dd == "20191231") return("0")
  if(year!="2020")stop("date in 2020 only")
  if(month == "01") return(as.numeric(day))
  if(month == "02") return(31 + as.numeric(day))
  if(month == "03") return(31 + 29 + as.numeric(day))
  if(month == "04") return(31 + 29 + 31 + as.numeric(day))
  if(month == "05") return(31 + 29 + 31 + 30 + as.numeric(day))
  if(month == "06") return(31 + 29 + 31 + 30 + 31 + as.numeric(day))
  stop("Jan, Feb, Mar, April, May, June 2020 only")
}
get_date_5deaths <- function(epidata, simple_option = F){
  # get date at which 5 deaths are reached, simply using data or using a smooth spline fit to data
  if(simple_option) return(epidata$date[which(epidata$deaths >= 5)][1]) # simple option
  # else fit smooth spline
  #plot(epidata$date, log10(epidata$deaths), type = "o", pch= 20)
  spl <- smooth.spline(x = epidata$date[which(epidata$deaths>0)], y = log10(epidata$deaths)[which(epidata$deaths>0)], df = 5)
  #lines(spl$x, spl$y, col=rgb(red = 1, green = 0, blue = 0, alpha = 0.5), lwd = 4)
  return(spl$x[which(spl$y > log10(5))[1]]) # return first time at 5 deaths
}
library(Rcpp)
sourceCpp(codefile)  # simulations in Rcpp

# t0 <- Sys.time()
# tmax <- 100
# truc <- simulate_Rcpp(max_age = length(si), tmax = tmax,
#                       R01 = 3, R02 = 0.7, simtlockdown = 100, ifr = ifr, pdetectedmax = 0.05, ktimecoeff = 0.01, tmidpoint = 30,
#                       si = si, tid = tid, tit = tit,
#                       only_get_5deaths = F)
# 
# Sys.time() - t0

#R0_step <- function(tt, tlockdown, par) if(tt < tlockdown) par[1] else par[2] # time-varying
pdetected_smooth <- function(tt, par, betafun){
  if(betafun == 1) return(par[3] + (par[4] - par[3]) / (1 +  exp(- par[5] * (tt - par[6])))) # note the tmidpoint is in the time of simulations
  if(betafun == 2) return(par[5] + (par[6] - par[5]) / (1 +  exp(- par[7] * (tt - par[8]))))
}
relativeR_smooth <- function(tt, par){ # plot the relative beta function
  R01 <- par[1]
  R02 <- par[2]
  tR0 <- par[3]
  kR0 <- par[4]
  return((1 + (R02/R01 - 1) / (1 + exp(- kR0 * (tt - tR0))))) # beta relative to baseline
}
loglik_Rcpp <- function(par, epidata, datelockdown, tmax, main = NULL, with_detected, use_tests, total_pop, betafun,
                        return_what = 0, draw_plot = F){
  
  stopifnot(all(c("date", "deaths", "cases") %in% colnames(epidata)))
  
  #######           first run to find time to 5 deaths when starting from 1 infected, WITH THOSE PARAMETERS, without lockdown ####### 
  
  simt5deaths <- simulate_Rcpp(max_age = max_age, tmax = tmax, simtlockdown = tmax, ifr = ifr, total_pop = total_pop,
                                par, betafun, # parameters
                                si = si, tid = tid, tit = tit,
                                only_get_5deaths = T, with_detected = with_detected)$simt5
  
  if(is.null(simt5deaths)) return(10^9) #stop("5 deaths not reached for those parameters")
  deltasimdata <- simt5deaths - date5deaths
  
  # now re-run once more to see if the lockdown at the right shifted date, we still get 5 deaths
  simt5deaths <- simulate_Rcpp(max_age = max_age, tmax = tmax, simtlockdown = datelockdown + deltasimdata, ifr = ifr, total_pop = total_pop,
                                par, betafun, # parameters
                                si = si, tid = tid, tit = tit,
                                only_get_5deaths = T, with_detected = with_detected)$simt5
  if(is.null(simt5deaths)) return(10^9) # do not consider that parameter if 5 deaths is not reached after all
  deltasimdata <- simt5deaths - date5deaths
  
  if(return_what == 1) return(list(delta = deltasimdata)) # return just delta between simulations and data
  
  #######                 now run with the lockdown date corresponding to what occurred in that country             ####### 
  
  # cat("deltasim = ", deltasimdata, "\n")
  # cat("simtlockdown = ", datelockdown + deltasimdata, "\n")
  tmp <- simulate_Rcpp(max_age = max_age, tmax = tmax, simtlockdown = datelockdown + deltasimdata, ifr = ifr, total_pop = total_pop,
                       par = par, betafun,
                       si = si, tid = tid, tit = tit,
                       only_get_5deaths = F, with_detected = with_detected)
  
  #######                                                now draw plot if necesary                                  ####### 
  
  if(draw_plot){
    draw_plot_fun(tmax = tmax, simtlockdown = datelockdown + deltasimdata, deltasimdata = deltasimdata, par = par,
                  with_detected = with_detected, use_tests = use_tests, all_incidence = tmp$all_incidence, all_deaths_incidence = tmp$all_deaths_incidence, all_detected_incidence = tmp$all_detected_incidence, betafun = betafun, main = main)
    points(epidata$date, log10(epidata$deaths), pch = 4, col = col2)
    points(epidata$date[which(epidata$deaths==0)], rep(-1, sum(epidata$deaths==0, na.rm = T)), pch = 4, col = col2) # plot 0 deaths at -1
    points(epidata$date, log10(epidata$cases), pch = 4, col = col3)
    points(epidata$date[which(epidata$cases==0)], rep(-1, sum(epidata$cases==0, na.rm = T)), pch = 4, col = col3) # plot 0 deaths at -1
  }

  #######                                           now define a likelihood based on deaths                         #######
  
  if(betafun == 1){
    if(with_detected){ # use cases as well
      size_nbinom_deaths <- par[7]
      size_nbinom_detected <- par[8]
    } else { # use only deaths
      size_nbinom_deaths <- par[3]
    }
  }
  if(betafun == 2){
    if(with_detected){
      size_nbinom_deaths <- par[9]
      size_nbinom_detected <- par[10]
    } else {
      size_nbinom_deaths <- par[3]
    }
  }
  idx_data <- which(epidata$cases >= 5 & epidata$deaths >= 0)
  idx_data <- seq(min(idx_data), max(idx_data)) # KEEP THE RANGE OF DATES WHEN CASES >= 5
  
  dates_over5_cases <- epidata$date[idx_data]
  
  
  if(max(dates_over5_cases + deltasimdata) > length(tmp$all_deaths_incidence)) { print(paste("simulations not long enough to span the period in data, returning lik = 10^9")); return(10^9) }
  if(any(dates_over5_cases + deltasimdata < 1)) {
    #print(paste("period in data implies need to go < 1 day in simulations"))
    if(any(dates_over5_cases + deltasimdata < 1)){ # THIS TO BE CHANGED
      print(paste("period in data implies need to go < day 1 in simulations, stopping"))
      return(10^9)
    }
    toselect <- dates_over5_cases >= 1 - deltasimdata
    dates_over5_cases <- dates_over5_cases[toselect]
    idx_data <- idx_data[toselect]
  }
  
  data_deaths <- epidata$deaths[idx_data]
  sim_deaths <- tmp$all_deaths_incidence[dates_over5_cases + deltasimdata]
  sim_incidence <- tmp$all_incidence[dates_over5_cases + deltasimdata]
  sim_presenting <- tmp$all_presenting[dates_over5_cases + deltasimdata]

  stopifnot(length(data_deaths) == length(sim_deaths))
  mylik <- sum(dnbinom(x = data_deaths, size = size_nbinom_deaths, mu = sim_deaths, log = T))
  if(is.na(mylik) | is.infinite(mylik)) mylik <- -10^9
  
  #######                                       now define a likelihood with detected cases                         #######
  
  if(with_detected){ # compute likelihood including detected cases
    
    data_detected <- epidata$cases[idx_data]
    sim_detected <- tmp$all_detected_incidence[dates_over5_cases + deltasimdata]
    
    mylik2 <- sum(dnbinom(x = data_detected, size = size_nbinom_detected, mu = sim_detected, log = T))
    if(is.na(mylik2) | is.infinite(mylik2)) mylik2 <- -10^9 # set to -10^9 when NA or infinite likelihood
    if(return_what == 2){
      return(list(lik = - mylik - mylik2, delta = deltasimdata, lik_deaths = mylik, lik_cases = mylik2, dates_over5_cases = dates_over5_cases, sim_deaths = sim_deaths, sim_detected = sim_detected, sim_incidence = sim_incidence, sim_presenting = sim_presenting))
    } else {
      #print(par)
      #print(mylik)
      return(- mylik - mylik2)
    }
  } else {
    if(return_what == 2){ # return a bunch of results
      return(list(lik = - mylik, delta = deltasimdata, dates_over5_cases = dates_over5_cases, sim_deaths = sim_deaths, sim_incidence = sim_incidence, sim_presenting = sim_presenting))
    }
    if(return_what == 0){ # return just likelihood
      return(- mylik)
    }
  }
  stop("return_what argument mis-specified")
}
#negativeloglik <- function(x, ...) -loglik(x, ...)
negativeloglik <- function(x, ...) -loglik_Rcpp(par = x, ...)
draw_plot_fun <- function(tmax, simtlockdown, deltasimdata, par, with_detected, use_tests, all_incidence, all_deaths_incidence, all_detected_incidence, ci_inc = NULL, ci_deaths = NULL, ci_det = NULL, betafun, main = NULL){
  ts <- (1:tmax)
  
  #abline(0, 0.14) # growth rate 0.14
  
  if(!with_detected & betafun == 1) main <- paste(mycountry, "model 0 (deaths)", sep = " | ")
  if(with_detected & betafun == 1) main <- paste(mycountry, "model 1 (sharp decline in R)", sep = " | ")
  if(with_detected & betafun == 2) main <- paste(mycountry, "model 2 (smooth decline in R)", sep = " | ")
  plot(NULL, xlim = c(0, tmax), ylim = c(-2.5, 6), pch = 20, ylab = "log10 incidence", xlab = "date in 2020", axes = F, main = main)
  all_dates_num <- c(1, 32, 61, 92, 122, 153, 183)
  all_dates <- c("01/01", "01/02", "01/03", "01/04", "01/05", "01/06", "01/07")
  axis(side = 1, at = all_dates_num, labels = all_dates)
  axis(side = 2, at = seq(-1, 6), labels = c("none", seq(0, 6)), las = 1)
  
  
  # range of simulations to plot:
  sel_inc <- which(all_incidence >= 1)
  sel_deaths <- which(all_deaths_incidence >= 1)
  sel_detected <- which(all_detected_incidence >= 1)
  
  # do not extrapolate beyond last date in data + 2 weeks
  sel_inc <- sel_inc[which(ts[sel_inc] - deltasimdata <= lastdate + 14)]
  sel_deaths <- sel_deaths[which(ts[sel_deaths] - deltasimdata <= lastdate + 14)]
  sel_detected <- sel_detected[which(ts[sel_detected] - deltasimdata <= lastdate + 14)]
  
  if(exists("ci_inc")) if(!is.null(ci_inc)){
    polygon(c(ts[sel_inc] - deltasimdata, rev(ts[sel_inc] - deltasimdata)), c(sapply(log10(ci_inc["2.5%", sel_inc]), function(x) max(x,-1)), rev(log10(ci_inc["97.5%", sel_inc]))), col = rgb(col1_rgb[1], col1_rgb[2], col1_rgb[3], 0.2), border = NA)
  }
  if(exists("ci_deaths")) if(!is.null(ci_deaths)){
    polygon(c(ts[sel_deaths] - deltasimdata, rev(ts[sel_deaths] - deltasimdata)), c(sapply(log10(ci_deaths["2.5%", sel_deaths]), function(x) max(x, -1)), rev(log10(ci_deaths["97.5%", sel_deaths]))), col = rgb(col2_rgb[1], col2_rgb[2], col2_rgb[3], 0.2), border = NA)
  }
  if(exists("ci_det")) if(!is.null(ci_det)){
    polygon(c(ts[sel_detected] - deltasimdata, rev(ts[sel_detected] - deltasimdata)), c(sapply(log10(ci_det["2.5%", sel_detected]), function(x) max(x, -1)), rev(log10(ci_det["97.5%", sel_detected]))), col = rgb(col3_rgb[1], col3_rgb[2], col3_rgb[3], 0.2), border = NA)
  }
  
  
  points(ts[sel_inc] - deltasimdata, log10(all_incidence)[sel_inc], pch = 20, cex= 0.5, col = col1) # shift all simulations to match data-time
  points(ts[sel_deaths] - deltasimdata, log10(all_deaths_incidence)[sel_deaths], pch = 20, col = col2, cex =0.5)
  points(ts[sel_detected] - deltasimdata, log10(all_detected_incidence)[sel_detected], pch = 20, col = col3, cex =0.5)
  
  #abline(1,0)
  #arrows(x0 = simt5, y0 = -2, x1 = simt10, y1 = -1, lwd = 3)
  arrows(x0 = simtlockdown - deltasimdata, y0 = -2, x1 = simtlockdown - deltasimdata, y1 = -1, lwd = 3)
  #text(x = simt5, y = -2.3, label = "5 deaths", cex = 0.8)
  #text(x = simtlockdown - deltasimdata, y = -2.3, label = "lockdown", cex = 0.8)
  #arrows(x0 = tlockdown + deltat + 28, y0 = -2, x1 = tlockdown + deltat + 28, y1 = -1, lwd = 3)
  
  if(with_detected) legend("topright", pch = c(20, 20, 20, 8, 8), col = c(col1, col3, col2, col3, col2), legend = c("infections-model", "detected-model", "deaths-model", "detected-data", "deaths-data"), bty = "n")
  if(!with_detected) legend("topright", pch = c(20, 20, 8, 8), col = c(col1, col2, col3, col2), legend = c("infections-model", "deaths-model", "detected-data", "deaths-data"), bty = "n")
  
  if(betafun == 0 | betafun == 1){
    Rpre <- signif(par[1], 2)
    Rpost <- signif(par[2], 2)
  } else {
    Rpre <- signif(par[1] * relativeR_smooth(firstdate + deltasimdata, par), 2)
    Rpost <- signif(par[1] * relativeR_smooth(lastdate + deltasimdata, par), 2)
  }
  legendtext1 <- bquote(R[pre] ~"="~ .(Rpre))
  legendtext2 <- bquote(R[post] ~"="~ .(Rpost))
  text(x = 175, y = 3.5, labels = legendtext1)
  text(x = 175, y = 3, labels = legendtext2)
  
  # add small figure for the probability detection
  if(with_detected){
    old_par <- par()
    par(new = TRUE, mar=c(0,0,2,0), fig = c(0.66,1,0.15,0.4)) # overlay existing plot
    maxday_toplot <- 122
    x <- seq(firstdate + deltasimdata, lastdate + deltasimdata, 1) # x is in simulation referential
    
    # here pdetected_smooth takes argument in "simulations referential" and we want to plot in data referential
    plot((x - deltasimdata), sapply(x, pdetected_smooth, par, betafun), type = "l", axes = F, main = "", lwd = 3, cex = 0.7, xlim = c(0, tmax), ylim = c(0, 1))
    if(betafun == 2){
      points((x - deltasimdata), sapply(x, relativeR_smooth, par), type = "l", col = "red", lwd = 3)
    }
    if(betafun == 1) points((x - deltasimdata), sapply(x, relativeR_smooth, c(par[1], par[2], datelockdown + deltasimdata, 10)), type = "l", col = "red", lwd = 3) # step function (k coefficient set to 10)
    axis(side = 1, at = c(1, 32, 61, 92, 122, 153), labels = c("", "", "", "", "", ""))
    axis(side = 2, at = c(0, 0.5, 1), las = 1)
    
    text(20, 0.85, labels = paste("rel.", expression(R[t])), col = "red")
    text(10, 0.1, labels = expression(c[t]))
    #par(new = old_par$new, mar = old_par$mar, fig = old_par$fig)
    suppressWarnings(par(old_par))
  }
}

optim.fun.repeated <- function(n.repeats, lik.fun, init.fun, verbose = F, lower = NULL, upper = NULL, ...){
  # a function to MINIMISE properly the negative log-likelihood function
  # using repeated use of function optim
  generate.init.fun <- match.fun(init.fun)
  lik.fun <- match.fun(lik.fun)
  all.opt <- list()
  for(ii in 1:n.repeats){
    init <- generate.init.fun()
    
    if(length(init)==1){ # one parameter function, use optimise
      if(is.null(upper) | is.null(lower)){ # if unspecified bounds for parameters, use (0,1)
        interval <- c(0, 1)
      } else {
        interval <- c(lower[1], upper[1])
      }
      all.opt[[ii]] <- optimise(lik.fun, interval = interval, ...)
      names(all.opt[[ii]]) <- c("par", "value")
      all.opt[[ii]]$message <- "Successful convergence"
    } else { # otherwise use nmk
      
        if(is.null(upper) | is.null(lower)){ # if unspecified bounds for parameters, use nmk
          all.opt[[ii]] <- nmk(init, lik.fun, ...)
          flag <- F
        } else {
          all.opt[[ii]] <- NA
          while(is.na(all.opt[[ii]])){
            init <- generate.init.fun()
            tryCatch(
              all.opt[[ii]] <- nmkb(init, lik.fun, lower = lower, upper = upper, ...),
              error = function(e){
                print(e)
              }
            )
          }
        }
    }
    
    if(verbose)print(ii)
  }
  if(verbose) cat(sum(unlist(lapply(all.opt, function(x)x$message == "Successful convergence"))), "optimisations converge\n")
  all.ll <- unlist(lapply(all.opt, function(x)x$value))
  min(all.ll) -> minll.pow
  if(verbose) cat(length(which(all.ll < minll.pow + 1 & all.ll > minll.pow)), " optim within 1 log lik of minimum likelihood\n")
  output <- list(minll.pow, all.opt[[which.min(all.ll)]]$par)
  names(output) <- c("lik", "pars")
  return(output)
}
run_MCMC_MH <- function(start.value, N.iter, proposal.cov, posterior.fun, npar, lower, upper, verbose = T, ...){ # same as run_MCMC but tuning the proposal distribution
  # THIS IS THE LATEST AND MOST CORRECT VERSION OF THE MCMC ALGO
  # Metropolis-Hastings algorithm (requires symmetric proposal distribution -> multivariate Gaussian OK)
  init.proposal.cov <- proposal.cov
  factor.cov <- 10
  
  posterior.fun <- match.fun(posterior.fun)
  stopifnot(dim(proposal.cov) == c(npar, npar))
  
  chain = array(dim = c(N.iter + 1, npar))
  all.lik <- rep(NA, N.iter + 1)
  chain[1,] = start.value
  all.lik[1] <- posterior.fun(start.value, ...)
  
  for (i in 1:N.iter){
    if(i / 1000 == floor(i / 1000)){
      if(verbose) print(i)
      acceptance <- 1 - mean(duplicated(chain[(i - 1000 + 1):i, ])) # assess acceptance rate
      if(verbose) cat("acceptance rate in last 1000 generations: ", acceptance, "\n")
      if(verbose) cat("likelihood: ", all.lik[i], "\n")
      df.mcmc <- data.frame(chain[1:i, ])
      df.mcmc <- df.mcmc[!duplicated(df.mcmc), ]    # unique values
      if(nrow(df.mcmc) == 1) {
        print("no new parameter accepted; reducing the proposal distribution")
        proposal.cov <- init.proposal.cov / factor.cov
      } else {
        if(npar > 1){
          proposal.cov <- cov(df.mcmc)                  # update proposal cov
        } else {
          proposal.cov <- var(df.mcmc)
        }
      }
      if(verbose) print("updated proposal covariance:")
      if(verbose) print(proposal.cov / factor.cov)
    }
    # draw new parameters
    if(npar > 1){
      proposal <- chain[i,] + mvrnorm(1, mu = rep(0, npar), Sigma = proposal.cov / factor.cov) # multivariate normal distribution
    } else {
      proposal <- chain[i,] + rnorm(1, mean = 0, sd = sqrt(proposal.cov)) # univariate normal
    }
    # NOTE HERE WE CANNOT SIMPLY DRAW NEW PROPOSAL UNTIL WE FIND SUITABLE ONE - THIS WOULD BIAS THE POSTERIOR SAMPLE https://darrenjw.wordpress.com/2012/06/04/metropolis-hastings-mcmc-when-the-proposal-and-target-have-differing-support/
    # see also https://stats.stackexchange.com/questions/73885/mcmc-on-a-bounded-parameter-space
    
    if(all(proposal > lower & proposal < upper)) {
      all.lik[i + 1] <- posterior.fun(proposal, ...)
    } else {
      all.lik[i + 1] <- NA # set to NA if unfeasible value
    }
    
    if(!is.na(all.lik[i+1])) probab <- exp(all.lik[i + 1] - all.lik[i]) else probab <- 0 # and set acceptance proba to 0 if NA...
    
    if(runif(1) < probab){
      chain[i+1,] <- proposal
    }else{
      chain[i+1,] <- chain[i,]
      all.lik[i + 1] <- all.lik[i]
    }
  }
  return(list(chain = chain, all.lik = all.lik))
}
nmkb <- function (par, fn, lower = -Inf, upper = Inf, control = list(), mytol = 1e-2, ...)  # from dfoptim package
{
  #print(par)
  ctrl <- list(tol = mytol, maxfeval = min(5000, max(1500, 
                                                     20 * length(par)^2)), regsimp = TRUE, maximize = FALSE, 
               restarts.max = 3, trace = FALSE)
  namc <- match.arg(names(control), choices = names(ctrl), 
                    several.ok = TRUE)
  if (!all(namc %in% names(ctrl))) 
    stop("unknown names in control: ", namc[!(namc %in% names(ctrl))])
  if (!is.null(names(control))) 
    ctrl[namc] <- control
  ftol <- ctrl$tol
  maxfeval <- ctrl$maxfeval
  regsimp <- ctrl$regsimp
  restarts.max <- ctrl$restarts.max
  maximize <- ctrl$maximize
  trace <- ctrl$trace
  n <- length(par)
  g <- function(x) {
    gx <- x
    gx[c1] <- atanh(2 * (x[c1] - lower[c1])/(upper[c1] - 
                                               lower[c1]) - 1)
    gx[c3] <- log(x[c3] - lower[c3])
    gx[c4] <- log(upper[c4] - x[c4])
    gx
  }
  ginv <- function(x) {
    gix <- x
    gix[c1] <- lower[c1] + (upper[c1] - lower[c1])/2 * (1 + 
                                                          tanh(x[c1]))
    gix[c3] <- lower[c3] + exp(x[c3])
    gix[c4] <- upper[c4] - exp(x[c4])
    gix
  }
  if (length(lower) == 1) 
    lower <- rep(lower, n)
  if (length(upper) == 1) 
    upper <- rep(upper, n)
  if (any(c(par <= lower, upper <= par))) 
    stop("Infeasible starting values!", call. = FALSE)
  low.finite <- is.finite(lower)
  upp.finite <- is.finite(upper)
  c1 <- low.finite & upp.finite
  c2 <- !(low.finite | upp.finite)
  c3 <- !(c1 | c2) & low.finite
  c4 <- !(c1 | c2) & upp.finite
  if (all(c2)) 
    stop("Use `nmk()' for unconstrained optimization!", call. = FALSE)
  if (maximize) 
    fnmb <- function(par) -fn(ginv(par), ...)
  else fnmb <- function(par) fn(ginv(par), ...)
  x0 <- g(par)
  if (n == 1) 
    stop(call. = FALSE, "Use `optimize' for univariate optimization")
  if (n > 30) 
    warning("Nelder-Mead should not be used for high-dimensional optimization")
  V <- cbind(rep(0, n), diag(n))
  f <- rep(0, n + 1)
  f[1] <- fnmb(x0)
  V[, 1] <- x0
  scale <- max(1, sqrt(sum(x0^2)))
  if (regsimp) {
    alpha <- scale/(n * sqrt(2)) * c(sqrt(n + 1) + n - 1, 
                                     sqrt(n + 1) - 1)
    V[, -1] <- (x0 + alpha[2])
    diag(V[, -1]) <- x0[1:n] + alpha[1]
    for (j in 2:ncol(V)) f[j] <- fnmb(V[, j])
  }
  else {
    V[, -1] <- x0 + scale * V[, -1]
    for (j in 2:ncol(V)) f[j] <- fnmb(V[, j])
  }
  f[is.nan(f)] <- Inf
  nf <- n + 1
  ord <- order(f)
  f <- f[ord]
  V <- V[, ord]
  rho <- 1
  gamma <- 0.5
  chi <- 2
  sigma <- 0.5
  conv <- 1
  oshrink <- 1
  restarts <- 0
  orth <- 0
  dist <- f[n + 1] - f[1]
  v <- V[, -1] - V[, 1]
  delf <- f[-1] - f[1]
  diam <- sqrt(colSums(v^2))
  sgrad <- c(solve(t(v), delf))
  alpha <- 1e-04 * max(diam)/sqrt(sum(sgrad^2))
  simplex.size <- sum(abs(V[, -1] - V[, 1]))/max(1, sum(abs(V[, 
                                                              1])))
  itc <- 0
  conv <- 0
  message <- "Succesful convergence"
  while (nf < maxfeval & restarts < restarts.max & dist > ftol & 
         simplex.size > 1e-06) {
    fbc <- mean(f)
    happy <- 0
    itc <- itc + 1
    xbar <- rowMeans(V[, 1:n])
    xr <- (1 + rho) * xbar - rho * V[, n + 1]
    fr <- fnmb(xr)
    nf <- nf + 1
    if (is.nan(fr)) 
      fr <- Inf
    if (fr >= f[1] & fr < f[n]) {
      happy <- 1
      xnew <- xr
      fnew <- fr
    }
    else if (fr < f[1]) {
      xe <- (1 + rho * chi) * xbar - rho * chi * V[, n + 
                                                     1]
      fe <- fnmb(xe)
      if (is.nan(fe)) 
        fe <- Inf
      nf <- nf + 1
      if (fe < fr) {
        xnew <- xe
        fnew <- fe
        happy <- 1
      }
      else {
        xnew <- xr
        fnew <- fr
        happy <- 1
      }
    }
    else if (fr >= f[n] & fr < f[n + 1]) {
      xc <- (1 + rho * gamma) * xbar - rho * gamma * V[, 
                                                       n + 1]
      fc <- fnmb(xc)
      if (is.nan(fc)) 
        fc <- Inf
      nf <- nf + 1
      if (fc <= fr) {
        xnew <- xc
        fnew <- fc
        happy <- 1
      }
    }
    else if (fr >= f[n + 1]) {
      xc <- (1 - gamma) * xbar + gamma * V[, n + 1]
      fc <- fnmb(xc)
      if (is.nan(fc)) 
        fc <- Inf
      nf <- nf + 1
      if (fc < f[n + 1]) {
        xnew <- xc
        fnew <- fc
        happy <- 1
      }
    }
    if (happy == 1 & oshrink == 1) {
      fbt <- mean(c(f[1:n], fnew))
      delfb <- fbt - fbc
      armtst <- alpha * sum(sgrad^2)
      if (delfb > -armtst/n) {
        if (trace) 
          cat("Trouble - restarting: \n")
        restarts <- restarts + 1
        orth <- 1
        diams <- min(diam)
        sx <- sign(0.5 * sign(sgrad))
        happy <- 0
        V[, -1] <- V[, 1]
        diag(V[, -1]) <- diag(V[, -1]) - diams * sx[1:n]
      }
    }
    if (happy == 1) {
      V[, n + 1] <- xnew
      f[n + 1] <- fnew
      ord <- order(f)
      V <- V[, ord]
      f <- f[ord]
    }
    else if (happy == 0 & restarts < restarts.max) {
      if (orth == 0) 
        orth <- 1
      V[, -1] <- V[, 1] - sigma * (V[, -1] - V[, 1])
      for (j in 2:ncol(V)) f[j] <- fnmb(V[, j])
      nf <- nf + n
      ord <- order(f)
      V <- V[, ord]
      f <- f[ord]
    }
    v <- V[, -1] - V[, 1]
    delf <- f[-1] - f[1]
    diam <- sqrt(colSums(v^2))
    simplex.size <- sum(abs(v))/max(1, sum(abs(V[, 1])))
    f[is.nan(f)] <- Inf
    dist <- f[n + 1] - f[1]
    sgrad <- c(solve(t(v), delf))
    if (trace & !(itc%%2)) 
      cat("iter: ", itc, "\n", "value: ", f[1], "\n")
  }
  if (dist <= ftol | simplex.size <= 1e-06) {
    conv <- 0
    message <- "Successful convergence"
  }
  else if (nf >= maxfeval) {
    conv <- 1
    message <- "Maximum number of fevals exceeded"
  }
  else if (restarts >= restarts.max) {
    conv <- 2
    message <- "Stagnation in Nelder-Mead"
  }
  return(list(par = ginv(V[, 1]), value = f[1] * (-1)^maximize, 
              feval = nf, restarts = restarts, convergence = conv, 
              message = message))
}
################################ download and clean ECDC data & John Hopkins data ################################
print("loading lockdown data...")
# LOCKDOWN DATES DATA:
# ld <- read.csv(ldfile)
# ld$country <- as.character(ld$country)
# ld$preferred_data <- as.character(ld$preferred_data)

ld <- read.csv(ldfile, na.strings = NA)
ld$date_lockdown <- as.character(ld$national_lockdown_startdate)
ld$date_lockdown[ld$date_lockdown == ""] <- as.character(ld$national_distancing_startdate[ld$date_lockdown == ""])
ld$date_lockdown[ld$date_lockdown == ""] <- as.character(ld$regional_lockdown_startdate[ld$date_lockdown == ""])
ld$date_lockdown[ld$country=="Turkey"] <- as.character(ld$national_distancing_startdate[ld$country=="Turkey"])

ld$preferred_data <- "ecdc"
ld$preferred_data[grepl(pattern = "_US", x = ld$country)] <- "us"
ld$preferred_data[grepl(pattern = "_Canada", x = ld$country)] <- "jh"
ld$preferred_data[ld$country == "South Korea_South Korea"] <- "jh"
ld$preferred_data[ld$country == "Hubei_China"] <- "jh"
ld$preferred_data[ld$country == "UK_UK"] <- "jh"
ld$preferred_data[ld$country == "France"] <- "fr"
ld$preferred_data[ld$country == "Italy"] <- "it"
ld <- ld[ld$country != "United_States_of_America",]
ld <- ld[ld$country != "Japan",]
ld <- ld[ld$country != "South Korea_South Korea",]
ld <- ld[ld$country != "UK_UK",]

type_of_date <- c("national_lockdown_startdate", "regional_lockdown_startdate", "national_distancing_startdate")
ld$which_date <- sapply(1:nrow(ld), function(i) type_of_date[ld[i, type_of_date] == ld$date_lockdown[i]])

ld$date_lockdown <- sapply(ld$date_lockdown, process_date_ecdc)

ld$matching_country <- sapply(ld$country, function(x){ # a country to match mobility data
  x <- as.character(x)
  if(!grepl(pattern = "_", x)) return(x) else {
    if(grepl(pattern = "_US", x)){
      x <- gsub(pattern = "_US", replacement = "", x); return(x)
    }
    if(grepl(pattern = "_Canada", x)){
      x <- gsub(pattern = "_Canada", replacement = "", x); return(x)
    }
    if(grepl(pattern = "_South Korea", x)) return("South Korea")
    if(grepl(pattern = "UK_UK", x)) return("United Kingdom")
    if(grepl(pattern = "Hubei_China", x)) return("China")
    return(gsub(pattern = "_", replacement = " ", x))
  }
})
ld$is_state <- grepl(pattern = "_US", ld$country) | grepl(pattern = "_Canada", ld$country)

# AGE STRUCTURE FILE AND IFR FILE
print("loading age data...")
age <- read.csv(agestructurefile)
all_ifr <- read.csv(ifrfile)

# CLEAN ECDC DATA:
ecdc <- read.csv(ecdcfile, na.strings = "", fileEncoding = "UTF-8-BOM") #
#write.csv(x = ecdc, file = "~/ownCloud/coronavirus/lockdown/data/ECDC_data.csv", row.names = F)
ecdc$date <- as.numeric(sapply(ecdc$dateRep, process_date_ecdc)) - 1 # ECDC is updated early in the morning, John Hopkins late at night
ecdc <- ecdc[with(ecdc, order(countriesAndTerritories, date)),]

# CLEAN FRENCH DATA
print("loading French data...")
fr <- read.csv(frfile)
fr <- fr[fr$granularite == "pays",]
#View(fr[fr$maille_nom=="Mayotte",])
#plot(fr$date[fr$granularite=="pays"], a$deces[fr$granularite == "pays"], type = "p")

fr <- fr[, c("date", "deces", "cas_confirmes")]
fr <- fr[!duplicated(fr), ]
fr <- fr[!is.na(fr$deces), ]
fr <- fr[!is.na(fr$cas_confirmes),]
fr$date2 <-as.numeric(strftime(x = fr$date, format = "%j"))

# for each duplicated date, take the first entry only
dates_duplicated <- fr$date2[which(duplicated(fr$date2))]
for(dd in dates_duplicated){
  idx <- which(fr$date2 == dd)
  if(length(idx) != 2) stop("date duplicated more than once")
  fr[idx[1], ] <- rep(NA, ncol(fr))
}
fr <- fr[!is.na(fr$date), ]
n <- nrow(fr)
fr$deaths <- NA
fr$deaths[2:n] <- fr$deces[2:n]- fr$deces[1:(n-1)]
fr$cases <- NA
fr$cases[2:n] <- fr$cas_confirmes[2:n]- fr$cas_confirmes[1:(n-1)]
fr$date <- NULL
fr <- fr[c("date2", "cases", "deaths")]
names(fr) <- c("date", "cases", "deaths")

# CLEAN JOHN HOPKIN DATA
#source(jhcleaning) # + John Hopkins data
print("loading JH data...")
jh <- read.csv(jhfile)
colnames(jh)[which(colnames(jh) == "death_incidence")] <- "deaths"
colnames(jh)[which(colnames(jh) == "incidence")] <- "cases"

# CLEAN USA DATA
print("loading US data...")
us <- read.csv(usfile)
us$date <- sapply(as.character(us$date), process_date_usa)
colnames(us)[which(colnames(us) == "deathIncrease")] <- "deaths"
colnames(us)[which(colnames(us) == "positiveIncrease")] <- "cases"
colnames(us)[which(colnames(us) == "totalTestResultsIncrease")] <- "ntests"
abb <- read.csv(abbfile)
us$state <- abb$state[match(us$state, abb$abbreviation)]
us <- us[with(us, order(state, date)),]

# CLEAN ITALIAN DATA
print("loading IT data...")
it <- read.csv(itfile)
it$date <- as.numeric(strftime(x = substr(x = it$data, start = 1, stop = 10), format = "%j"))
n <- nrow(it)
it$deaths <- NA
it$deaths[2:n] <- it$deceduti[2:n]- it$deceduti[1:(n-1)]
it$cases <- it$nuovi_positivi
it$ntests <- NA
it$ntests[2:n] <- it$tamponi[2:n] - it$tamponi[1:(n-1)]
it <- it[ , c("date", "cases", "deaths", "ntests")]

# test agreement among data sources
# truc <- jh[which(jh$Province_Country == "France_France"),]
# truc2 <- ecdc[which(ecdc$countriesAndTerritories == "France"), ]
# plot(truc$cases[which(truc$date %in% 31:104)], truc2$cases[which(truc2$date %in% 31:104)])
# abline(0, 1)
# plot(truc$deaths[which(truc$date %in% 31:104)], truc2$deaths[which(truc2$date %in% 31:104)])
# abline(0, 1)

# LOOK AT DATA IN LINEAR SCALE
# for(mycountry in list2[1:9]){
#   sub_tab <- jh[which(jh$Province_Country == mycountry),]
#   tmax <- 200
#   plot(NULL, xlim = c(0, tmax), ylim = c(-2.5, 6), pch = 20, ylab = "log10 incidence", xlab = "date in 2020", axes = F, main = mycountry)
#   all_dates_num <- c(1, 32, 61, 92, 122, 153, 183)
#   all_dates <- c("01/01", "01/02", "01/03", "01/04", "01/05", "01/06", "01/07")
#   axis(side = 1, at = all_dates_num, labels = all_dates)
#   axis(side = 2, at = seq(-2, 6), las = 1)
#   points(sub_tab$date, log10(sub_tab$deaths), pch = 8, col = col2, type = "o", cex = 0.5)
#   points(sub_tab$date, log10(sub_tab$cases), pch = 8, col = col3, cex = 0.5)
#   abline(h = 1, lty = 2)
#   
#   plot(NULL, xlim = range(sub_tab$date), ylim = c(0, 1.1 * max(sub_tab$deaths, na.rm = T)), pch = 20, ylab = "incidence deaths", xlab = "date in 2020", axes = F, main = mycountry)
#   all_dates_num <- c(1, 32, 61, 92, 122, 153, 183)
#   all_dates <- c("01/01", "01/02", "01/03", "01/04", "01/05", "01/06", "01/07")
#   axis(side = 1, at = all_dates_num, labels = all_dates)
#   axis(side = 2, at = seq(0, 1000, 10), las = 1)
#   points(sub_tab$date, (sub_tab$deaths), pch = 8, col = col2, type = "o", cex = 0.5)
#   abline(h = 1, lty = 2)
#   
#   plot(NULL, xlim = range(sub_tab$date), ylim = c(0, 1.1 * max(sub_tab$cases, na.rm = T)), pch = 20, ylab = "incidence cases", xlab = "date in 2020", axes = F, main = mycountry)
#   all_dates_num <- c(1, 32, 61, 92, 122, 153, 183)
#   all_dates <- c("01/01", "01/02", "01/03", "01/04", "01/05", "01/06", "01/07")
#   axis(side = 1, at = all_dates_num, labels = all_dates)
#   axis(side = 2, at = seq(0, 10000, 100), las = 1)
#   points(sub_tab$date, (sub_tab$cases), pch = 8, col = col3, type = "o", cex = 0.5)
#   abline(h = 1, lty = 2)
# }

