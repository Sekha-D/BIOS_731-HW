library(tidyverse)
library(tictoc)

source(here::here("source", "Functions.R"))

B=100
nt=100
alpha=0.05
nsim=0.95*(1 - 0.95)/(0.01)^2

n = c(10, 50, 500)
beta_true = c(0, 0.5, 2)
sigma2_true = c(2, log(2))

param_grid = expand.grid(n = n,
                     n_sim = nsim,
                     beta_true = beta_true,
                     sigma2_true = sigma2_true)

no_scenario=nrow(param_grid)


beta_est<-rep(NA,no_scenario)
bias<- rep(NA,no_scenario)
wald_coverage<-rep(NA,no_scenario)
boot_perc_coverage<-rep(NA,no_scenario)
boot_t_coverage<-rep(NA,no_scenario)

for (j in 1:no_scenario){

params = param_grid[j,]




###############################################################
## start simulation code
###############################################################

# generate a random seed for each simulated dataset
seed = floor(runif(nsim, 1, 10000))
results = matrix(NA,ncol=14,nrow=nsim)

for(i in 1:nsim){
  
  set.seed(seed[i])
  
  ####################
  # simulate data
  simdata = get_simdata(n = params$n,
                        beta_treat = params$beta_true,
                        sigma2 = params$sigma2_true)
  
  ####################
  # apply method(s)
  fit = fit_model(simdata)
  
  ####################
  # calculate estimates
  tic()
  estimates = get_estimates(model_fit = fit,
                            true_beta = params$beta_true)

  estimates = c(estimates,true_beta = params$beta_true, n = params$n,sigma2_true = params$sigma2_true)
  
  time_stamp1 = toc(quiet =TRUE)
  wald_time=time_stamp1$toc - time_stamp1$tic
  
  coef=estimates[1]
  
  tic()
  boot_perc<-boot_nonpara_perc_residuals(sample=simdata,beta_est=coef,B=B,alpha=alpha)
  
  time_stamp2 = toc(quiet =TRUE)
  boot_perc_time=time_stamp2$toc - time_stamp2$tic
  
  estimates<- c(estimates,boot_perc)
  
  tic()
  boot_t<-boot_nonpara_t_residuals(sample=simdata,beta_est=coef,B=B,nt=nt,alpha=alpha)
  time_stamp3 = toc(quiet =TRUE)
  boot_t_time=time_stamp3$toc - time_stamp3$tic
  
  estimates<- c(estimates,boot_t,wald_time,boot_perc_time,boot_t_time)
  
  results[i,] = estimates
  
}

####################
beta_est[j]<-mean(results[,1])
bias[j]<- params$beta_true-beta_est

wald_coverage[j]<-mean((params$beta_true >= results[,3] & params$beta_true <= results[,4]))

boot_perc_coverage[j]<-mean((params$beta_true >= results[,8] & params$beta_true <= results[,9]))

boot_t_coverage[j]<-mean((params$beta_true >= results[,10] & params$beta_true <= results[,11]))

mean_wald_time[j]<-mean(results[,12])
mean_bootperc_time[j]<-mean(results[,13])
mean_boott_time[j]<-mean(results[,14])
}

# save results
# note that I am saving results outside of the for loop. For slow simulations,
# you may want to save each iteration separately
filename = paste0("scenario_", scenario, ".RDA")
save(results,
     file = here::here("results", filename))