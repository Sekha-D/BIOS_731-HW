################# load libraries ########################
library(tidyverse)
library(tictoc)
library(doParallel)
library(foreach)


#import the functions ##############################
source(here::here("source", "Functions.R"))
source(here::here("Simulations","run_scenario.R"))

# set parameter values 
B=500 #no of bootstrap samples
nt=500 # number of inner bootstrap samples
alpha=0.05  # alpha percentage
nsim=0.95*(1 - 0.95)/(0.01)^2  #nsim calculations

# create the parameter grid for the 18 scenarios ############
n = c(10, 50, 500)
beta_true = c(0, 0.5, 2)
sigma2_true = c(2, log(2))

param_grid = expand.grid(n = n,
                     beta_true = beta_true,
                     sigma2_true = sigma2_true)

no_scenario=nrow(param_grid) # 18 scenarios



# Set up parallel backend with (8 cores in my personal mac, 6 cores in university lap )
num_cores = detectCores() -2
cl = makeCluster(num_cores)
registerDoParallel(cl)

# Run the 18 scenarios using parallelization #################
summary_table = foreach(i =1:no_scenario, .combine = rbind) %dopar% {
  params = param_grid[i,] 
  run_scenario(params,nsim=nsim,B=B,nt=nt,alpha=alpha)
}

############ save the summary results ###########################
save(summary_table,file = here::here("Results", "summary_table.RDA"))

#load(file = here::here("results", "summary_table.RDA"))







