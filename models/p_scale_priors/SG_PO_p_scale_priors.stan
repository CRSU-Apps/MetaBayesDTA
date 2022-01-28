// Bivariate MA model of Reitsma et al. 

data { 
    int n_studies; 
    vector[101] roc_points_sp;
    int Cov[n_studies]; // univariable meta-reg covariate ( 2 <= length <= n_studies )
    int num_levels; // the number of categories / values in the univariable meta-reg covariate Z
    int Cov_level[num_levels]; // categories / values in the univariable meta-reg covariate Z
    int ind_meta_reg; // indicator for whether meta-reg covariate is on both Se and Sp (1), just on Se (2) or just on Sp (3)
    real<lower=0, upper=1> SG_prior_sens_lower95;
    real<lower=0, upper=1> SG_prior_sens_upper95;
    real<lower=0, upper=1> SG_prior_spec_lower95;
    real<lower=0, upper=1> SG_prior_spec_upper95;
    real<lower=0> SG_prior_SD_sens_sd;
    real<lower=0> SG_prior_SD_spec_sd;
    int<lower=0,upper=1> holdout[n_studies]; //index whether the observation should be used (for K-fold CV)
}

parameters { 
    matrix[num_levels, 2] mu_pre; 
    cholesky_factor_corr[2] L_Omega[num_levels]; 
    vector<lower=0>[2] sigma[num_levels]; 
    vector[2] z[n_studies]; 
}

transformed parameters {
    matrix[num_levels, 2] mu; 
    real SG_prior_mean_sens_mu;
    real SG_prior_mean_sens_sd;
    real SG_prior_mean_spec_mu;
    real SG_prior_mean_spec_sd;
    
   // convert interval priors to logit
     SG_prior_mean_sens_mu = (logit( SG_prior_sens_upper95 ) + logit( SG_prior_sens_lower95 ) ) /2;
     SG_prior_mean_sens_sd = (logit( SG_prior_sens_upper95 ) - logit( SG_prior_sens_lower95 ) ) /(2*1.96);
     SG_prior_mean_spec_mu = (logit( SG_prior_spec_upper95 ) + logit( SG_prior_spec_lower95 ) ) /2;
     SG_prior_mean_spec_sd = (logit( SG_prior_spec_upper95 ) - logit( SG_prior_spec_lower95 ) ) /(2*1.96);
     

  for (n in 1:n_studies) {
    if (ind_meta_reg == 1) 
      mu[Cov[n], 1] = mu_pre[Cov[n],1];
      mu[Cov[n], 2] = mu_pre[Cov[n],2];
    if (ind_meta_reg == 2) {
      mu[Cov[n], 1] = mu_pre[Cov[n],1];
      mu[Cov[n], 2] = mu_pre[1,1];
    }
    if (ind_meta_reg == 3) {
      mu[Cov[n], 1] = mu_pre[1, 2];
      mu[Cov[n], 2] = mu_pre[Cov[n],2];
    }

    }
}

model {
    //Prior Model
       mu_pre[,1] ~ normal(SG_prior_mean_sens_mu, SG_prior_mean_sens_sd); 
       mu_pre[,2] ~ normal(SG_prior_mean_spec_mu, SG_prior_mean_spec_sd); 

  for (n in 1:n_studies) {
       sigma[Cov[n], 1] ~ normal(0, SG_prior_SD_sens_sd);
       sigma[Cov[n], 2] ~ normal(0, SG_prior_SD_spec_sd);
       L_Omega[Cov[n], ] ~ lkj_corr_cholesky(2); 
  }

}

generated quantities { 
     vector[num_levels] Se; 
     vector[num_levels] Sp;  
     vector[num_levels] lSe; 
     vector[num_levels] lSp; 
     corr_matrix[2] Omega[num_levels]; 
     matrix[2,2] Sigma[num_levels]; 
     vector[n_studies] se;
     vector[n_studies] sp;

  for (n in 1:n_studies) {
   Omega[Cov[n], ] = multiply_lower_tri_self_transpose(L_Omega[Cov[n], ]); // between-study correlation matrix
   Sigma[Cov[n], ] = quad_form_diag(Omega[Cov[n], ], sigma[Cov[n], ]); // between-study var-cov matrix
  }
  

 // Summary estimates for each value of covariate
 for (i in 1:num_levels) { 
  Se[i] = inv_logit(mu[Cov_level[i], 1]); 
  Sp[i] = inv_logit(mu[Cov_level[i], 2]); 
  lSe[i] = mu[Cov_level[i], 1];
  lSp[i] = mu[Cov_level[i], 2];
 }
}
