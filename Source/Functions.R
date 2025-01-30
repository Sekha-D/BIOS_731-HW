get_simdata= function (n, beta_treat, sigma2){
  if (sigma2==2){
    
    X<- rbinom(n, 1, prob = 0.5)
    epsi<- rnorm(n,sqrt(sigma2))
    Y<-1+ beta_treat*X+epsi #is beta0 1?
  }
  
  else{
    X<- rbinom(n, 1, prob = 0.5)
    epsi<- rlnorm(n, meanlog = 0, sdlog = sqrt(exp(sigma2)))
    Y<-1+ beta_treat*X+epsi
    
  }
  return(as.data.frame(cbind(Y,X)))
  
}

fit_model= function(simdata){
  model_fit=lm(Y~X, data=simdata)
  
  return(model_fit)
}

get_estimates= function(model_fit , true_beta ){
  summary_model=summary(model_fit)
  confid_int=confint(model_fit)
  estimates=cbind(coef=summary_model$coefficients[2,1],sd=summary_model$coefficients[2,2],
                  lower=confid_int[2,1],upper=confid_int[2,2])
  
  return(estimates)
  
}

boot_nonpara_perc_pairs=function(sample,B,alpha){
 n=nrow(sample)
nonboot_est = rep(0, B) 
for (i in 1:B) {   
  nboot.data = sample[sample(c(1:n), n, replace=TRUE),   ]
  model_fit=lm(Y~X, data=nboot.data)
  nonboot_est[i] = summary(model_fit)$coefficients[2,1]
  
} 

quantile(nonboot_est, probs = c(alpha/2, 1-(alpha/2)))

}

boot_nonpara_perc_residuals=function(sample,beta_est,B,alpha=0.05){
  residuals=sample$Y-sample$X*beta_est
  n=nrow(sample)
  nonboot_est = rep(0, B) 
  for (i in 1:B) {   
    nboot.res = sample(residuals, n, replace=TRUE)
    nboot.data=data.frame(Y_b=sample$X*beta_est+nboot.res,X=sample$X)
    model_fit=lm(Y_b~X, data=nboot.data)
    nonboot_est[i] = summary(model_fit)$coefficients[2,1]
    
  } 
  
  boot_perc=quantile(nonboot_est, probs = c(alpha/2, 1-(alpha/2)))
  
  return(boot_perc)
  
}

boot_nonpara_t_residuals=function(sample,beta_est,B,nt,alpha=0.05){
  residuals=sample$Y-sample$X*beta_est
  n=nrow(sample)
  nonboot_est = tstar= rep(0, B) 
  for (i in 1:B) {   
    nboot.res = sample(residuals, n, replace=TRUE)
    nboot.data=data.frame(Y_b=sample$X*beta_est+nboot.res,X=sample$X)
    model_fit=lm(Y_b~X, data=nboot.data)
    nonboot_est[i] = summary(model_fit)$coefficients[2,1]
    
    nonboot_est2 = rep(NA, nt)
    for(k in 1:nt){
      ystar_k = sample(nboot.res, size = n, replace = TRUE)
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


  
  
