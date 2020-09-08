#analyse_results_functions.R
get_parsample <- function(mm, myn = 5000){
  n <- length(mm$all.lik)
  tokeep <- round(0.2*n):n
  if(myn <= length(tokeep)){
    mysamp <- sample(tokeep, size = myn, replace = F) # sample of the MCMC run
  } else {
    mysamp <- tokeep
  }
  return(mm$chain[mysamp, ])
}
get_ci <- function(parsample){ # get CIs from MCMC object
  return(apply(parsample, 2, function(x) quantile(x, c(0.025, 0.5, 0.975))))
}
get_ci_ratio <- function(parsample){ # get CI for the ratio of Rs
  return(quantile(parsample[,2]/parsample[,1], c(0.025, 0.5, 0.975)))
}
get_best_mcmc <- function(mm, another_one = F){
  if(!another_one) return(mm$chain[which.max(mm$all.lik), ])
  idx <- which(mm$all.lik > max(mm$all.lik) - 1) # parameter sets close to
  my_idx <- sample(x = idx, size = 1)
  return(mm$chain[my_idx, ])
}
get_median_mcmc <- function(mm){
  n <- length(mm$all.lik)
  tokeep <- round(0.2*n):n
  apply(mm$chain[tokeep, ], 2, median) -> medians
  apply(mm$chain[tokeep, ], 2, sd) -> sds
  apply(mm$chain[tokeep, ], 1, function(x) sum(x - medians)^2) -> dist
  idx <- which.min(dist)
  return(mm$chain[tokeep,][idx,])
}
myattach <- function(mylist){
  stopifnot(is.list(mylist))
  mynames <- names(mylist)
  cat("attaching ", mynames, "\n")
  n <- length(mylist)
  stopifnot(length(mynames) == n)
  for(i in 1:n) assign(x = mynames[i], value = mylist[[i]], envir = .GlobalEnv)
}
plot_data <- function(){
  points(sub_tab$date, log10(sub_tab$deaths), pch = 4, col = col2)
  points(sub_tab$date[which(sub_tab$deaths==0)], rep(-1, sum(sub_tab$deaths==0, na.rm = T)), pch = 4, col = col2) # plot 0 deaths at -1
  points(sub_tab$date, log10(sub_tab$cases), pch = 4, col = col3)
  points(sub_tab$date[which(sub_tab$cases==0)], rep(-1, sum(sub_tab$cases==0, na.rm = T)), pch = 4, col = col3) # plot 0 deaths at -1
}
# draw_plot_fun <- function(tmax, simtlockdown, deltasimdata, par, with_detected, use_tests, all_incidence, all_deaths_incidence, all_detected_incidence, ci_inc = NULL, ci_deaths = NULL, ci_det = NULL, betafun, main = NULL){
#   ts <- (1:tmax)
#   
#   #abline(0, 0.14) # growth rate 0.14
#   
#   if(!with_detected & betafun == 1) main <- paste(mycountry, "model 0 (just deaths)", sep = " | ")
#   if(with_detected & betafun == 1) main <- paste(mycountry, "model 1 (sharp decline in R)", sep = " | ")
#   if(with_detected & betafun == 2) main <- paste(mycountry, "model 2 (smooth decline in R)", sep = " | ")
#   plot(NULL, xlim = c(0, tmax), ylim = c(-2.5, 6), pch = 20, ylab = "log10 incidence", xlab = "date in 2020", axes = F, main = main)
#   all_dates_num <- c(1, 32, 61, 92, 122, 153, 183)
#   all_dates <- c("01/01", "01/02", "01/03", "01/04", "01/05", "01/06", "01/07")
#   axis(side = 1, at = all_dates_num, labels = all_dates)
#   axis(side = 2, at = seq(-1, 6), labels = c("none", seq(0, 6)), las = 1)
#  
#   
#   # range of simulations to plot:
#   sel_inc <- which(all_incidence >= 1)
#   sel_deaths <- which(all_deaths_incidence >= 1)
#   sel_detected <- which(all_detected_incidence >= 1)
#   
#   # do not extrapolate beyond last date in data + 2 weeks
#   sel_inc <- sel_inc[which(ts[sel_inc] - deltasimdata <= lastdate + 14)]
#   sel_deaths <- sel_deaths[which(ts[sel_deaths] - deltasimdata <= lastdate + 14)]
#   sel_detected <- sel_detected[which(ts[sel_detected] - deltasimdata <= lastdate + 14)]
#   
#   if(!is.null(ci_inc)){
#     polygon(c(ts[sel_inc] - deltasimdata, rev(ts[sel_inc] - deltasimdata)), c(sapply(log10(ci_inc["2.5%", sel_inc]), function(x) max(x,-1)), rev(log10(ci_inc["97.5%", sel_inc]))), col = rgb(col1_rgb[1], col1_rgb[2], col1_rgb[3], 0.2), border = NA)
#   }
#   if(!is.null(ci_deaths)){
#     polygon(c(ts[sel_deaths] - deltasimdata, rev(ts[sel_deaths] - deltasimdata)), c(sapply(log10(ci_deaths["2.5%", sel_deaths]), function(x) max(x, -1)), rev(log10(ci_deaths["97.5%", sel_deaths]))), col = rgb(col2_rgb[1], col2_rgb[2], col2_rgb[3], 0.2), border = NA)
#   }
#   if(!is.null(ci_det)){
#     polygon(c(ts[sel_detected] - deltasimdata, rev(ts[sel_detected] - deltasimdata)), c(sapply(log10(ci_det["2.5%", sel_detected]), function(x) max(x, -1)), rev(log10(ci_det["97.5%", sel_detected]))), col = rgb(col3_rgb[1], col3_rgb[2], col3_rgb[3], 0.2), border = NA)
#   }
#   
#   
#   points(ts[sel_inc] - deltasimdata, log10(all_incidence)[sel_inc], pch = 20, cex= 0.5, col = col1) # shift all simulations to match data-time
#   points(ts[sel_deaths] - deltasimdata, log10(all_deaths_incidence)[sel_deaths], pch = 20, col = col2, cex =0.5)
#   points(ts[sel_detected] - deltasimdata, log10(all_detected_incidence)[sel_detected], pch = 20, col = col3, cex =0.5)
#   
#   #abline(1,0)
#   #arrows(x0 = simt5, y0 = -2, x1 = simt10, y1 = -1, lwd = 3)
#   arrows(x0 = simtlockdown - deltasimdata, y0 = -2, x1 = simtlockdown - deltasimdata, y1 = -1, lwd = 3)
#   #text(x = simt5, y = -2.3, label = "5 deaths", cex = 0.8)
#   #text(x = simtlockdown - deltasimdata, y = -2.3, label = "lockdown", cex = 0.8)
#   #arrows(x0 = tlockdown + deltat + 28, y0 = -2, x1 = tlockdown + deltat + 28, y1 = -1, lwd = 3)
#   
#   if(with_detected) legend("topright", pch = c(20, 20, 20, 8, 8), col = c(col1, col3, col2, col3, col2), legend = c("infections-model", "detected-model", "deaths-model", "detected-data", "deaths-data"), bty = "n")
#   if(!with_detected) legend("topright", pch = c(20, 20, 8, 8), col = c(col1, col2, col3, col2), legend = c("infections-model", "deaths-model", "detected-data", "deaths-data"), bty = "n")
#   
#   if(betafun == 0 | betafun == 1){
#     Rpre <- signif(par[1], 2)
#     Rpost <- signif(par[2], 2)
#   } else {
#     Rpre <- signif(par[1] * relativeR_smooth(firstdate + deltasimdata, par), 2)
#     Rpost <- signif(par[1] * relativeR_smooth(lastdate + deltasimdata, par), 2)
#   }
#   legendtext1 <- bquote(R[pre] ~"="~ .(Rpre))
#   legendtext2 <- bquote(R[post] ~"="~ .(Rpost))
#   text(x = 175, y = 3.5, labels = legendtext1)
#   text(x = 175, y = 3, labels = legendtext2)
#   
#   # add small figure for the probability detection
#   if(with_detected){
#     old_par <- par()
#     par(new = TRUE, mar=c(0,0,2,0), fig = c(0.66,1,0.15,0.4)) # overlay existing plot
#     maxday_toplot <- 122
#     x <- seq(firstdate + deltasimdata, lastdate + deltasimdata, 1) # x is in simulation referential
#     
#     # here pdetected_smooth takes argument in "simulations referential" and we want to plot in data referential
#     plot((x - deltasimdata), sapply(x, pdetected_smooth, par, betafun), type = "l", axes = F, main = "", lwd = 3, cex = 0.7, xlim = c(0, tmax), ylim = c(0, 1))
#     if(betafun == 2){
#       points((x - deltasimdata), sapply(x, relativeR_smooth, par), type = "l", col = "red", lwd = 3)
#     }
#     if(betafun == 1) points((x - deltasimdata), sapply(x, relativeR_smooth, c(par[1], par[2], datelockdown + deltasimdata, 10)), type = "l", col = "red", lwd = 3) # step function (k coefficient set to 10)
#     axis(side = 1, at = c(1, 32, 61, 92, 122, 153), labels = c("", "", "", "", "", ""))
#     axis(side = 2, at = c(0, 0.5, 1), las = 1)
#     
#     text(20, 0.85, labels = paste("rel.", expression(R[t])), col = "red")
#     text(10, 0.1, labels = expression(c[t]))
#     #par(new = old_par$new, mar = old_par$mar, fig = old_par$fig)
#     suppressWarnings(par(old_par))
#   }
# }

get_MCMC_sim_ci <- function(parsample, deltas, with_detected, betafun){
  
  myn <- nrow(parsample)
  stopifnot(length(deltas) == myn)
  
  ci_death <- matrix(NA, nrow = myn, ncol = tmax)
  ci_inc <- matrix(NA, nrow = myn, ncol = tmax)
  ci_det <- matrix(NA, nrow = myn, ncol = tmax)
  ci_total_infected <- rep(NA, myn)
  ci_total_deaths <- rep(NA, myn)
  ci_tmidpointR_2 <- rep(NA, myn)
  ci_tmidpoint <- rep(NA, myn)
  ci_delta <- rep(NA, myn)
  ci_pdetected_first <- rep(NA, myn)
  ci_pdetected_last <- rep(NA, myn)
  ci_Rfirst <- rep(NA, myn)
  ci_Rlast <- rep(NA, myn)
  
  for(k in 1:myn){
    if(!is.na(deltas[k])){
      # re-simulate
      truc_2 <- simulate_Rcpp(max_age = max_age, tmax = tmax, simtlockdown = datelockdown + deltas[k], ifr = ifr, total_pop = mytotal_pop,
                              par = parsample[k, ], betafun = betafun,
                              si = si, tid = tid, tit = tit,
                              only_get_5deaths = F, with_detected = with_detected)
      ci_death[k, ] <- truc_2$all_deaths_incidence
      ci_inc[k, ] <- truc_2$all_incidence
      ci_det[k, ] <- truc_2$all_detected_incidence
      ci_total_infected[k] <- sum(truc_2$all_incidence[1:(lastdate + deltas[k])], na.rm = T) # total infected at last date
      ci_total_deaths[k] <- sum(truc_2$all_deaths_incidence[1:(lastdate + deltas[k])], na.rm = T) # total deaths at last date (convert last date data to days in sim)
      if(betafun == 2) ci_tmidpointR_2[k] <- parsample[k, 3] - deltas[k] # (convert days in sim in data date)
      if(with_detected & betafun == 1) ci_tmidpoint[k] <- parsample[k, 5] - deltas[k]
      if(with_detected & betafun == 2) ci_tmidpoint[k] <- parsample[k, 7] - deltas[k]
      if(with_detected){
        ci_pdetected_first[k] <- pdetected_smooth(tt = firstdate + deltas[k], par = parsample[k, ], betafun = betafun)
        ci_pdetected_last[k] <- pdetected_smooth(tt = lastdate + deltas[k], par = parsample[k, ], betafun = betafun)
      }
      if(betafun == 2){
        mytmp <- parsample[k, 1] * relativeR_smooth(tt = c(firstdate, lastdate) + deltas[k], par = parsample[k, ])
        ci_Rfirst[k] <-  mytmp[1]
        ci_Rlast[k] <-  mytmp[2]
      }
      ci_delta[k] <- deltas[k]
    }
  }
  ci_inc <- apply(ci_inc, 2, quantile, c(0.025, 0.5, 0.975), na.rm = T)
  ci_det <- apply(ci_det, 2, quantile, c(0.025, 0.5, 0.975), na.rm = T)
  ci_death <- apply(ci_death, 2, quantile, c(0.025, 0.5, 0.975), na.rm = T)
  ci_total_infected <- quantile(ci_total_infected, c(0.025, 0.5, 0.975), na.rm = T)
  ci_total_deaths <- quantile(ci_total_deaths, c(0.025, 0.5, 0.975), na.rm = T)
  if(betafun == 2){
    ci_tmidpointR_2 <- quantile(ci_tmidpointR_2, c(0.025, 0.5, 0.975), na.rm =T) 
    ci_Rfirst <- quantile(ci_Rfirst, c(0.025, 0.5, 0.975), na.rm = T)
    ci_Rlast <- quantile(ci_Rlast, c(0.025, 0.5, 0.975), na.rm = T)
  } else {
    ci_tmidpointR_2 <- c(NA, NA)
    ci_Rfirst <- c(NA, NA)
    ci_Rlast <- c(NA, NA)
  }
  if(with_detected){
    ci_tmidpoint <- quantile(ci_tmidpoint, c(0.025, 0.5, 0.975), na.rm = T)
    ci_pdetected_first <- quantile(ci_pdetected_first, c(0.025, 0.5, 0.975), na.rm = T)
    ci_pdetected_last <- quantile(ci_pdetected_last, c(0.025, 0.5, 0.975), na.rm = T)
  } else {
    ci_tmidpoint <- c(NA, NA)
    ci_pdetected_first <- c(NA, NA)
    ci_pdetected_last <- c(NA, NA)
  }
  ci_delta <- quantile(ci_delta, c(0.025, 0.5, 0.975), na.rm = T)
  return(list(ci_death = ci_death, ci_inc = ci_inc, ci_det = ci_det, ci_total_infected = ci_total_infected, ci_total_deaths = ci_total_deaths, ci_tmidpointR_2 = ci_tmidpointR_2, ci_tmidpoint = ci_tmidpoint, ci_delta = ci_delta, ci_Rfirst = ci_Rfirst, ci_Rlast = ci_Rlast, ci_pdetected_first = ci_pdetected_first, ci_pdetected_last = ci_pdetected_last))
}
get_sims <- function(pars, parsample, with_detected, delta, betafun, parzero = NULL, nochangeR = NULL){
  
  # get deltas for the parameter sample
  deltas <- apply(parsample, 1, function(parvec){
    tmp <- loglik_Rcpp(par = parvec, epidata = sub_tab, datelockdown = datelockdown, tmax = tmax, with_detected = with_detected, use_tests = F, total_pop = mytotal_pop, betafun = betafun, return_what = 1, draw_plot = F)
    if(tmp == 10^9) return(NA) else return(tmp$delta)
  }) # get the delta between simulations and data for this parameter set
  
  # alter the parameter for the counterfactual simulations:
  if(!is.null(parzero)){ # this to fix some parameters at 0
    pars[parzero] <- 0
    parsample[, parzero] <- 0
  }
  if(!is.null(nochangeR)){ # this to fix R post to R pre
    pars[2] <- pars[1]
    parsample[, 2] <- parsample[, 1]
  }
  
  pred_bis <- simulate_Rcpp(max_age = max_age, tmax = tmax, simtlockdown = datelockdown + delta, ifr = ifr, total_pop = mytotal_pop,
                           par = pars, betafun = betafun,
                           si = si, tid = tid, tit = tit,
                           only_get_5deaths = F, with_detected = with_detected)
  
  # CONFIDENCE INTERVALS ON TOTAL INFECTED, AND SIMULATIONS
  tmp <- get_MCMC_sim_ci(parsample = parsample, deltas = deltas, with_detected = with_detected, betafun = betafun)
  
  draw_plot_fun(tmax = tmax, simtlockdown = datelockdown + delta, deltasimdata = delta, par = pars, with_detected = with_detected, use_tests = F, 
                all_incidence = pred_bis$all_incidence, all_deaths_incidence = pred_bis$all_deaths_incidence, all_detected_incidence = pred_bis$all_detected_incidence, ci_death = tmp$ci_death, ci_inc = tmp$ci_inc, ci_det = tmp$ci_det, betafun = betafun)
  plot_data()
  return(list(pred_bis = pred_bis, total_death = sum(pred_bis$all_deaths_incidence[1:(lastdate+delta)], na.rm = T), ci_total_infected = tmp$ci_total_infected, ci_total_deaths = tmp$ci_total_deaths, ci_tmidpointR_2 = tmp$ci_tmidpointR_2, ci_tmidpoint = tmp$ci_tmidpoint, ci_delta = tmp$ci_delta, ci_Rfirst = tmp$ci_Rfirst, ci_Rlast = tmp$ci_Rlast, ci_pdetected_first = tmp$ci_pdetected_first, ci_pdetected_last = tmp$ci_pdetected_last))
}

