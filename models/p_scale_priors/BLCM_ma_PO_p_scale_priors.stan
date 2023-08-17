data {
        int<lower=1> num_ref;  // total Number of ref tests 
      	int<lower=0> n_studies;	// total Number of studies
        int Ref[n_studies]; // indicator of ref. test 
     // priors for index test (fixed as 1 index test)
        real<lower=0, upper=1> LCM_prior_sens_index_lower95;
        real<lower=0, upper=1> LCM_prior_sens_index_upper95;
        real<lower=0, upper=1> LCM_prior_spec_index_lower95;
        real<lower=0, upper=1> LCM_prior_spec_index_upper95;
        real<lower=0> LCM_prior_SD_sens_index_sd;
        real<lower=0> LCM_prior_SD_spec_index_sd;
     // priors for reference (dynamic as 1+ reference tests)
        vector<lower=0, upper=1>[num_ref] LCM_prior_sens_ref_lower95;
        vector<lower=0, upper=1>[num_ref] LCM_prior_sens_ref_upper95;
        vector<lower=0, upper=1>[num_ref] LCM_prior_spec_ref_lower95;
        vector<lower=0, upper=1>[num_ref] LCM_prior_spec_ref_upper95;
     // static priors for reference (as between-study SD is assumed the same between different reference tests)
        real<lower=0> LCM_prior_SD_sens_ref_sd;
        real<lower=0> LCM_prior_SD_spec_ref_sd;
     // prev priors
        real LCM_prior_prev_a;
        real LCM_prior_prev_b;

}

parameters {
    real<lower = 0, upper = 1> p;
    vector[2] index_logit_mu; 
    vector[2] ref_logit_mu[num_ref];
    vector<lower=0>[2] sigma_ref;
    vector<lower=0>[2] sigma_index;
    cholesky_factor_corr[2] L_Omega_ref; 
    cholesky_factor_corr[2] L_Omega_index; 
}

transformed parameters { 
        real LCM_prior_mean_sens_index_mu;
        real LCM_prior_mean_sens_index_sd;
        real LCM_prior_mean_spec_index_mu;
        real LCM_prior_mean_spec_index_sd;
        vector[num_ref] LCM_prior_mean_sens_ref_mu;
        vector[num_ref] LCM_prior_mean_sens_ref_sd;
        vector[num_ref] LCM_prior_mean_spec_ref_mu;
        vector[num_ref] LCM_prior_mean_spec_ref_sd;
        
 // convert interval priors to logit
     // for index test
     LCM_prior_mean_sens_index_mu = ( logit( LCM_prior_sens_index_upper95 ) + logit(LCM_prior_sens_index_lower95 ) ) /2;
     LCM_prior_mean_sens_index_sd = ( logit( LCM_prior_sens_index_upper95 ) - logit(LCM_prior_sens_index_lower95 ) ) /(2*1.96);
     LCM_prior_mean_spec_index_mu = ( logit( LCM_prior_spec_index_upper95 ) + logit(LCM_prior_spec_index_lower95 ) ) /2;
     LCM_prior_mean_spec_index_sd = ( logit( LCM_prior_spec_index_upper95 ) - logit(LCM_prior_spec_index_lower95 ) ) /(2*1.96);
     
    // for ref tests
     LCM_prior_mean_sens_ref_mu = ( logit( LCM_prior_sens_ref_upper95 ) + logit( LCM_prior_sens_ref_lower95 ) ) /2;
     LCM_prior_mean_sens_ref_sd = ( logit( LCM_prior_sens_ref_upper95 ) - logit( LCM_prior_sens_ref_lower95 ) ) /(2*1.96);
     LCM_prior_mean_spec_ref_mu = ( logit( LCM_prior_spec_ref_upper95 ) + logit( LCM_prior_spec_ref_lower95 ) ) /2;
     LCM_prior_mean_spec_ref_sd = ( logit( LCM_prior_spec_ref_upper95 ) - logit( LCM_prior_spec_ref_lower95 ) ) /(2*1.96);
  }

model {
       // prior model
     	index_logit_mu[1]  ~ normal(LCM_prior_mean_sens_index_mu, LCM_prior_mean_sens_index_sd);
     	index_logit_mu[2]  ~ normal(LCM_prior_mean_spec_index_mu, LCM_prior_mean_spec_index_sd);

		ref_logit_mu[, 1] ~ normal(LCM_prior_mean_sens_ref_mu, LCM_prior_mean_sens_ref_sd);		
		ref_logit_mu[, 2] ~ normal(LCM_prior_mean_spec_ref_mu, LCM_prior_mean_spec_ref_sd);	

        L_Omega_ref   ~ lkj_corr_cholesky(2);
        L_Omega_index ~ lkj_corr_cholesky(2);

	    sigma_ref[1]    ~ normal(0, LCM_prior_SD_sens_ref_sd);
	    sigma_ref[2]    ~ normal(0, LCM_prior_SD_spec_ref_sd);

	    sigma_index[1]    ~ normal(0, LCM_prior_SD_sens_index_sd);
	    sigma_index[2]    ~ normal(0, LCM_prior_SD_spec_index_sd);

        p ~ beta(LCM_prior_prev_a, LCM_prior_prev_b);
	    
}

generated quantities {
        vector[num_ref+1] Se; 
        vector[num_ref+1] Sp; 
        corr_matrix[2] Omega_ref; 
        matrix[2,2] Sigma_ref; 
        corr_matrix[2] Omega_index; 
        matrix[2,2] Sigma_index; 
        real Se_index; 
        real Sp_index; 
        vector[num_ref] Se_ref; 
        vector[num_ref] Sp_ref; 

   Omega_ref = multiply_lower_tri_self_transpose(L_Omega_ref); // between-study correlation matrix
   Sigma_ref = quad_form_diag( Omega_ref, sigma_ref ); // between-study var-cov matrix

   Omega_index = multiply_lower_tri_self_transpose(L_Omega_index); // between-study correlation matrix
   Sigma_index = quad_form_diag( Omega_index, sigma_index ); // between-study var-cov matrix


      // overall test accuracy estimates, for each test
 for (s in 1:n_studies) { 
          Se[Ref[s]] =    inv_logit(ref_logit_mu[Ref[s],1]);
          Sp[Ref[s]] =    inv_logit(ref_logit_mu[Ref[s],2]);
 }
          Se[num_ref+1] =    inv_logit(index_logit_mu[1]);
          Sp[num_ref+1] =    inv_logit(index_logit_mu[2]);
         
 for (s in 1:n_studies) { 
          Se_ref[Ref[s]] = Se[Ref[s]];
          Sp_ref[Ref[s]] = Sp[Ref[s]];
 }

          Se_index = Se[num_ref+1];
          Sp_index = Sp[num_ref+1];

}
