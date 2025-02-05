#################### Function to simulate data according to n, beta_treat and sigma2 ##############

get_simdata= function (n, beta_treat, sigma2){
  if (sigma2==2){ # generate data under normal
    
    X<- rbinom(n, 1, prob = 0.5)
    epsi<- rnorm(n,sqrt(sigma2))
    Y<-1+ beta_treat*X+epsi #is beta0 1?
    
  }
  
  else{ # generate data under lognormal
    
    X<- rbinom(n, 1, prob = 0.5)
    epsi<- rlnorm(n, meanlog = 0, sdlog = sqrt(exp(sigma2)))
    Y<-1+ beta_treat*X+epsi
  }
  return(as.data.frame(cbind(Y,X)))
  
}


############################### Function to fit the simple linear regression model #############################
fit_model= function(simdata){
  
  model_fit=lm(Y~X, data=simdata)
  
  return(model_fit)
}


################################# Function to return the necessary estimated #################

get_estimates= function(model_fit ){
  #estimates dataframe
  summary_model=summary(model_fit) 
  #CI for the estimates
  confid_int=confint(model_fit)
  
  estimates=cbind(coef=summary_model$coefficients[2,1],sd=summary_model$coefficients[2,2],
                  lower=confid_int[2,1],upper=confid_int[2,2])
  # estimates return beta_hat, sd of beta_hat, lower CI and upper CI respectively
  return(estimates)
  
}


########################## function to conduct bootstrap percentile method ##########################


boot_nonpara_perc_residuals=function(sample,beta_est,B,alpha=0.05){
  # calculate the residuals from X Y data
  residuals=sample$Y-sample$X*beta_est
  n=nrow(sample)
  
  nonboot_est = rep(0, B)
  for (i in 1:B) {   
    # sample residuals with replacement
    nboot.res = sample(residuals, n, replace=TRUE)
    # calculate new y_b as y_b=x*beta_hat+ epsilon_b 
    nboot.data=data.frame(Y_b=sample$X*beta_est+nboot.res,X=sample$X)
    # fit the model for y_c and X 
    model_fit=lm(Y_b~X, data=nboot.data)
    #save the coefficient estimate
    nonboot_est[i] = summary(model_fit)$coefficients[2,1]
    
  } 
  
  # we aren't calculating bias for this. If we want to, then nonboot_mean=mean(nonboot_est)
  nonboot_sd = sd(nonboot_est) # save the sd of bootstrap sample
  # calculate the 95% percentile from the bootstrap sample
  boot_perc=quantile(nonboot_est, probs = c(alpha/2, 1-(alpha/2)))
  
  return(c(nonboot_sd,boot_perc))
  
}

########################## function to conduct bootstrap t method ##########################

# similar as boot_nonpara_perc_residuals function
boot_nonpara_t_residuals=function(sample,beta_est,B,nt,alpha=0.05){
  residuals=sample$Y-sample$X*beta_est
  n=nrow(sample)
  nonboot_est = tstar= rep(0, B) 
  for (i in 1:B) {   
    nboot.res = sample(residuals, n, replace=TRUE)
    nboot.data=data.frame(Y_b=sample$X*beta_est+nboot.res,X=sample$X)
    model_fit=lm(Y_b~X, data=nboot.data)
    nonboot_est[i] = summary(model_fit)$coefficients[2,1]
    
    nonboot_est2 = rep(NA, nt) # 2nd round of for loops
    for(k in 1:nt){
      # sample from the nboot.res
      ystar_k = sample(nboot.res, size = n, replace = TRUE)
      #calculate y_b2= x*beta_est + ystart_k
      nboot.data2=data.frame(Y_b2=sample$X*beta_est+ystar_k,X=sample$X)
      model_fit2=lm(Y_b2~X, data=nboot.data2)
      nonboot_est2[k] = summary(model_fit2)$coefficients[2,1]
    }
    
    # calculate tstar
    se_star = sd(nonboot_est2)
    tstar[i] = (nonboot_est[i] - beta_est)/se_star
    
    
  }
  
  # bootstrap t interval
  
  t_quants = quantile(tstar, probs = c(alpha/2, 1-(alpha/2)))
  se_beta_hat = sd(nonboot_est)
  # lower CI
  lower=beta_est - t_quants[2] *se_beta_hat
  # upper CI
  upper=beta_est - t_quants[1] *se_beta_hat
  return(c(lower,upper))
}


  
  
