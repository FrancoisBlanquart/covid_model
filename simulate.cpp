#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
  
Rcpp::List simulate_Rcpp(
  int max_age, int tmax, int simtlockdown, double ifr, double total_pop,
  NumericVector par, int betafun, // parameters and type of transmission function
  NumericVector si, NumericVector tid, NumericVector tit,
  bool only_get_5deaths, bool with_detected){

    Rcpp::NumericVector infected_byage(max_age);     // create vector of undetected infected by age of infection
    Rcpp::NumericVector all_infected_byage(max_age); // create vector of all infected by age of infection
    Rcpp::NumericVector new_deaths_byage(max_age);   // create vector of newly dead by age of infection

    Rcpp::NumericVector all_deaths_incidence(tmax);    // vector of deaths 
    Rcpp::NumericVector all_detected_incidence(tmax);  // vector of detected
    Rcpp::NumericVector all_incidence(tmax);           // vector of incidence
    Rcpp::NumericVector all_presenting(tmax);          // vector of people that may present to healthcare

    // by default the NumericVector are filled with 0s

    int i, t;
    double new_infections, new_deaths, new_detected, new_presenting, ni_detected, ni_deaths;

    infected_byage[0] = 1.; // initialise with 1 infected of age 0
    all_infected_byage[0] = 1.;
    double total_infected = 1.;
    
    double R01, R02, tR0, kR0, pdetectedmin, pdetectedmax, ktimecoeff, tmidpoint;

    // define parameter values depending on which mode we run the function on:
    if(betafun == 1){ // step function
       R01 = par[0];
       R02 = par[1];
       if(with_detected){
          pdetectedmin = par[2];
          pdetectedmax = par[3];
          ktimecoeff = par[4];
          tmidpoint = par[5];
       } else {
          pdetectedmin = 0.;
          pdetectedmax = 0.;
          ktimecoeff = 0.;
          tmidpoint = 0.;
       }  
    }
    if(betafun == 2){ // smooth function
       R01 = par[0];
       R02 = par[1];
       tR0 = par[2];
       kR0 = par[3];
       if(with_detected){
         pdetectedmin = par[4];
         pdetectedmax = par[5];
         ktimecoeff = par[6];
         tmidpoint = par[7];
       } else {
          pdetectedmin = 0.;
          pdetectedmax = 0.;
          ktimecoeff = 0.;
          tmidpoint = 0.;
       }
    }
    
    // printf("R01 %f", R01);
    for(t = 1; t <= tmax; t++){

      // 1. compute new infections
      new_infections = 0;
      new_deaths = 0;
      new_detected = 0;
      new_presenting = 0;

      for(i = 0; i < max_age; i++){ // for each age of infection
        
        // infections:
        if(betafun == 1){ // step function
          if(t < simtlockdown){
            new_infections += (total_pop - total_infected) / total_pop * R01 * si[i] * infected_byage[i];
          } else {
            new_infections += (total_pop - total_infected) / total_pop * R02 * si[i] * infected_byage[i];
          }
        }
        if(betafun == 2){ // smooth function
          new_infections += (total_pop - total_infected) / total_pop * (R01 + (R02 - R01) / (1 + exp(- kR0 * (t - tR0)))) * si[i] * infected_byage[i];
        }
        
        // dead:
        ni_deaths = ifr * tid[i] * all_infected_byage[i]; // both detected and undetected infections die at same rate
        new_deaths += ni_deaths;
        if(only_get_5deaths && new_deaths >= 5) return(List::create(Named("simt5") = t));
        
        // detected:
        ni_detected = (pdetectedmin + (pdetectedmax - pdetectedmin) / (1. +  exp(- ktimecoeff * (t - tmidpoint)))) * tit[i] * infected_byage[i];
        new_detected += ni_detected;
        new_presenting += tit[i] * infected_byage[i];

        infected_byage[i] -= ni_detected; // remove detected from infected with age of infection i
        // infected_byage[i] -= ni_deaths; // remove dead from infected with age of infection i -> we neglect this now as deaths are rare and occur after a long time

      } // end of loop on age
      
      // 2. update infected_byage accordingly
      for(i = max_age - 1; i > 0; i--){
        infected_byage[i] = infected_byage[i-1]; // increase age of infection
        all_infected_byage[i] = all_infected_byage[i-1];
      }
      infected_byage[0] = new_infections;       // update new infections at age of infection 0
      all_infected_byage[0] = new_infections;   // update new infections at age of infection 0
      total_infected += new_infections;         // update total infected
      
      // 3. record in vectors
      all_deaths_incidence[t-1] = new_deaths;
      all_presenting[t-1] = new_presenting;
      all_detected_incidence[t-1] = new_detected;
      all_incidence[t-1] = new_infections;

    } // end of loop on time

    // return results:
    return(List::create(
    Named("infected_byage") = infected_byage,
    Named("all_infected_byage") = all_infected_byage,
    Named("all_deaths_incidence") = all_deaths_incidence,
    Named("all_detected_incidence") = all_detected_incidence,
    Named("all_presenting") = all_presenting,
    Named("all_incidence") = all_incidence,
    Named("total_infected") = total_infected
    ));
}

