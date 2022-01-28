// Bivariate MA model of Reitsma et al. 

data { 
    int n_studies; 
    vector[101] roc_points_sp;
    vector[101] cts_cov_points;
    vector[n_studies] Z_Cov; // univariable meta-reg covariate ( 2 <= length <= n_studies )
    int ind_meta_reg; // indicator for whether meta-reg covariate is on both Se and Sp (1), just on Se (2) or just on Sp (3)
    real MRcts_prior_mean_sens_mu;
    real MRcts_prior_mean_sens_sd;
    real MRcts_prior_mean_spec_mu;
    real MRcts_prior_mean_spec_sd;
    real MRcts_prior_SD_sens_sd;
    real MRcts_prior_SD_spec_sd;
    real MRcts_prior_coeff_sens_mean;
    real MRcts_prior_coeff_sens_sd;
    real MRcts_prior_coeff_spec_mean;
    real MRcts_prior_coeff_spec_sd;
    real input_cov; // covariate value of interest inputted
    int<lower=0,upper=1> holdout[n_studies]; //index whether the observation should be used (for K-fold CV)
}

parameters { 
    vector[2] mu; 
    vector[2] coeff;
    cholesky_factor_corr[2] L_Omega; 
    vector<lower=0>[2] sigma; 
    vector[2] z[n_studies];
}


model {
    //Prior Model
       mu[1] ~ normal(MRcts_prior_mean_sens_mu, MRcts_prior_mean_sens_sd); 
       mu[2] ~ normal(MRcts_prior_mean_spec_mu, MRcts_prior_mean_spec_sd); 

       coeff[1] ~ normal(MRcts_prior_coeff_sens_mean, MRcts_prior_coeff_sens_sd);
       coeff[2] ~ normal(MRcts_prior_coeff_spec_mean, MRcts_prior_coeff_spec_sd);

       sigma[1] ~ normal(0, MRcts_prior_SD_sens_sd);
       sigma[2] ~ normal(0, MRcts_prior_SD_spec_sd);

       L_Omega ~ lkj_corr_cholesky(2);

}

generated quantities { 
     real Se_at_cov_input;
     real Sp_at_cov_input;
     real lSe_at_cov_input;
     real lSp_at_cov_input;
     real DOR_at_cov_input;
     real LRp_at_cov_input;
     real LRn_at_cov_input;
     vector[101] Se; 
     vector[101] Sp;  
     vector[101] lSe; 
     vector[101] lSp; 
     corr_matrix[2] Omega; 
     matrix[2,2] Sigma; 

  Omega = multiply_lower_tri_self_transpose(L_Omega); // between-study correlation matrix
  Sigma = quad_form_diag(Omega, sigma); // between-study var-cov matrix


  // summary Se and Sp for meta-regression (within range of min and max observed point)
 for (i in 1:101) { 
  if (ind_meta_reg == 1) 
      Se[i] =  inv_logit(mu[1] + coeff[1]*cts_cov_points[i]);
      Sp[i] =  inv_logit(mu[2] + coeff[2]*cts_cov_points[i]);
    if (ind_meta_reg == 2) {
      Se[i] =  inv_logit(mu[1] + coeff[1]*cts_cov_points[i]);
      Sp[i] =  inv_logit(mu[2]);
    }
    if (ind_meta_reg == 3) {
      Se[i] =  inv_logit(mu[1]);
      Sp[i] =  inv_logit(mu[2] + coeff[2]*cts_cov_points[i]);
    }
  }

  // summary Se and Sp for meta-regression (at covariate value inputted)
   Se_at_cov_input =  inv_logit(mu[1] + coeff[1]*input_cov);
   Sp_at_cov_input =  inv_logit(mu[2] + coeff[2]*input_cov);
   lSe_at_cov_input = mu[1] + coeff[1]*input_cov;
   lSp_at_cov_input = mu[2] + coeff[2]*input_cov;
   DOR_at_cov_input = (Se_at_cov_input*Sp_at_cov_input) / ((1-Se_at_cov_input)*(1-Sp_at_cov_input));
   LRp_at_cov_input = Se_at_cov_input/(1-Sp_at_cov_input);
   LRn_at_cov_input = (1-Se_at_cov_input)/Sp_at_cov_input;



}

