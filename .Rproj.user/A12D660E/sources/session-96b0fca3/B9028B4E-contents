
#################### This function here allows the user to run 1 scenario given nsim,B, nt and alpha #####

run_scenario<- function (params,nsim,B,nt,alpha=0.05){
  
  "Results matrix will save parameters, beta_hat, sd_beta_hat, Walds lower and upper bounds, bootstrap percentile 
  and t quantiles, computaional times for each of the number of simulations.
  At the end of the function, the results matrix will be saved in results folder as a .RDA file
  "
  results = matrix(NA,ncol=15,nrow=nsim)
  
  set.seed(3333)
  for(i in 1:nsim){
    
    
    
    ####################
    # simulate data
    simdata = get_simdata(n = params$n,
                          beta_treat = params$beta_true,
                          sigma2 = params$sigma2_true)
    
    " when n=10, X can be be all 1s or 0s by chance. Avoid such samples because
    their regression model is not full rank"
    while (var(simdata$X)==0) {
      simdata = get_simdata(n = params$n,
                            beta_treat = params$beta_true,
                            sigma2 = params$sigma2_true)
    }
    
    ####################
    # apply method
    
    fit = fit_model(simdata) # fit the regression method and get Walds method results
    
    ####################
    # calculate estimates
    tictoc::tic() # start first timer to calculate CI for WALDS
    estimates = get_estimates(model_fit = fit)
    
    estimates = c(estimates,true_beta = params$beta_true, n = params$n,sigma2_true = params$sigma2_true)
    
    time_stamp1 = tictoc::toc(quiet =TRUE) # stop first timer to for WALDS
    wald_time=time_stamp1$toc - time_stamp1$tic # elapsed time
    
    coef=estimates[1]
    
    tictoc::tic() # start second timer to calculate CI for bootstrap percentile
    # get estimates from bootstrap percentile function
    boot_perc<-boot_nonpara_perc_residuals(sample=simdata,beta_est=coef,B=B,alpha=alpha)
    time_stamp2 = tictoc::toc(quiet =TRUE)# stop second timer
    boot_perc_time=time_stamp2$toc - time_stamp2$tic

    estimates<- c(estimates,boot_perc)

    tictoc::tic() # start third timer to calculate CI for bootstrap t
    # get estimates from bootstrap t function
    boot_t<-boot_nonpara_t_residuals(sample=simdata,beta_est=coef,B=B,nt=nt,alpha=alpha)
    time_stamp3 = tictoc::toc(quiet =TRUE)# stop time
    boot_t_time=time_stamp3$toc - time_stamp3$tic

    estimates<- c(estimates,boot_t,wald_time,boot_perc_time,boot_t_time)

    results[i,] = estimates
    
  }
  
  ####################  save results with params as filename ################
  
  filename = paste0("scenario_", paste(params,collapse = "_"), ".RDA")
  save(results,file = here::here("Results", filename))
  
  #calculate bias
  beta_est<-mean(results[,1])
  bias<- params$beta_true-beta_est
  
  # calculate wald coverage
  wald_coverage<-mean((params$beta_true >= results[,3] & params$beta_true <= results[,4]))
  
  # calculate bootstrap percentile coverage
  boot_perc_coverage<-mean((params$beta_true >= results[,9] & params$beta_true <= results[,10]))

  #calculate bootstrap t coverage
  boot_t_coverage<-mean((params$beta_true >= results[,11] & params$beta_true <= results[,12]))

  #calculate the sum of elapsed time across all nsim
  sum_wald_time<-sum(results[,13])
  sum_bootperc_time<-sum(results[,14])
  sum_boott_time<-sum(results[,15])
  
  #return the above calculated values. For a given scenario we return bias, coverage and times
  return(cbind(params,beta_est,bias,wald_coverage,boot_perc_coverage,boot_t_coverage, sum_wald_time,
           sum_bootperc_time,sum_boott_time))
  
}
