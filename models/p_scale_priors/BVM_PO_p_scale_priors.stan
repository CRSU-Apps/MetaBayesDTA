// Bivariate MA model of Reitsma et al. 

data { 
    // user inputs priors for Se/Sp directly on the probability scale (aka p scale)
    real<lower=0, upper=1> MA_prior_sens_lower95; 
    real<lower=0, upper=1> MA_prior_sens_upper95; 
    real<lower=0, upper=1> MA_prior_spec_lower95; 
    real<lower=0, upper=1> MA_prior_spec_upper95; 
    // scale priors stay inputted on logit scale 
    real<lower=0> MA_prior_SD_sens_sd; 
    real<lower=0> MA_prior_SD_spec_sd; 
}



parameters { 
    vector[2] mu;  
    cholesky_factor_corr[2] L_Omega; 
    vector<lower=0>[2] sigma; 
}

transformed parameters { 
    real MA_prior_mean_sens_mu; 
    real MA_prior_mean_sens_sd; 
    real MA_prior_mean_spec_mu; 
    real MA_prior_mean_spec_sd; 
    
    // convert interval priors to logit
     MA_prior_mean_sens_mu = ( logit(MA_prior_sens_upper95) + logit(MA_prior_sens_lower95) ) / 2 ;
     MA_prior_mean_sens_sd = ( logit(MA_prior_sens_upper95) - logit(MA_prior_sens_lower95) ) / (2*1.96) ;
     MA_prior_mean_spec_mu = ( logit(MA_prior_spec_upper95) + logit(MA_prior_spec_lower95) )/ 2 ;
     MA_prior_mean_spec_sd = ( logit(MA_prior_spec_upper95) - logit(MA_prior_spec_lower95) )/ (2*1.96) ;


  }

model {

    //Prior Model
       mu[1] ~ normal(MA_prior_mean_sens_mu, MA_prior_mean_sens_sd); 
       mu[2] ~ normal(MA_prior_mean_spec_mu, MA_prior_mean_spec_sd); 

       sigma[1] ~ normal(0, MA_prior_SD_sens_sd);
       sigma[2] ~ normal(0, MA_prior_SD_spec_sd);
       L_Omega ~ lkj_corr_cholesky(2);
       
}

generated quantities { 
     real Se; 
     real Sp;  
     real lSe; 
     real lSp; 
     corr_matrix[2] Omega; 
     matrix[2,2] Sigma; 
 
  Omega = multiply_lower_tri_self_transpose(L_Omega); // between-study correlation matrix
  Sigma = quad_form_diag(Omega, sigma); // between-study var-cov matrix


  Se = inv_logit(mu[1]); 
  Sp = inv_logit(mu[2]); 
  lSe = mu[1];
  lSp = mu[2];


}
