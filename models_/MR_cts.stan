// Bivariate MA model of Reitsma et al. 

data { 
    int n_studies; 
    int TP[n_studies];
    int FN[n_studies];
    int FP[n_studies];
    int TN[n_studies];
    vector[101] roc_points_sp;
    vector[101] cts_cov_points;
    real centering_value; // value to centre the cts. covariate at (default is mean of observed data) 
    vector[n_studies] Z_Cov; // meta-reg covariate ( 2 <= length <= n_studies )
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

transformed data { 
    int pos[n_studies];
    int neg[n_studies];
    vector[n_studies] Z_Cov_centered; // centered meta-reg covariate
    
    Z_Cov_centered = Z_Cov - centering_value; // centered meta-reg covariate

  for (n in 1:n_studies) { 
    pos[n] = TP[n] + FN[n];
    neg[n] = TN[n] + FP[n];
  }
}
 
parameters { 
    vector[2] mu; 
    vector[2] coeff;
    cholesky_factor_corr[2] L_Omega; 
    vector<lower=0>[2] sigma; 
    vector[2] z[n_studies];
}

transformed parameters {
    matrix[n_studies, 2] mu_lp; 
    vector[2] logit_pi[n_studies];
    vector[2] log_lik[n_studies]; // log_lik for each study 

  for (n in 1:n_studies) {
    if (ind_meta_reg == 1) 
      mu_lp[n,1] = mu[1] + coeff[1]*Z_Cov_centered[n];
      mu_lp[n,2] = mu[2] + coeff[2]*Z_Cov_centered[n];
    if (ind_meta_reg == 2) {
      mu_lp[n,1] = mu[1] + coeff[1]*Z_Cov_centered[n];
      mu_lp[n,2] = mu[2];
    }
    if (ind_meta_reg == 3) {
      mu_lp[n,1] = mu[1];
      mu_lp[n,2] = mu[2] + coeff[2]*Z_Cov_centered[n];
    }

     logit_pi[n, ] =   to_vector(mu_lp[n, ]) + diag_pre_multiply(sigma, L_Omega) * z[n, ];  

  // Pointwise Likelihood 
    log_lik[n, 1] =  binomial_logit_lpmf(TP[n]  | pos[n], logit_pi[n, 1]);
    log_lik[n, 2] =  binomial_logit_lpmf(TN[n]  | neg[n], logit_pi[n, 2]);
    }
}

model {
    //Prior Model
       mu[1] ~ normal(MRcts_prior_mean_sens_mu, MRcts_prior_mean_sens_sd); 
       mu[2] ~ normal(MRcts_prior_mean_spec_mu, MRcts_prior_mean_spec_sd); 

       coeff[1] ~ normal(MRcts_prior_coeff_sens_mean, MRcts_prior_coeff_sens_sd);
       coeff[2] ~ normal(MRcts_prior_coeff_spec_mean, MRcts_prior_coeff_spec_sd);

  for (j in 1:2) 
       z[,j]  ~ std_normal();

       sigma[1] ~ normal(0, MRcts_prior_SD_sens_sd);
       sigma[2] ~ normal(0, MRcts_prior_SD_spec_sd);

       L_Omega ~ lkj_corr_cholesky(2);

  // Likelihood Model
    for (n in 1:n_studies) {
      if (holdout[n] == 0) {
        target += log_lik[n, 1];
        target += log_lik[n, 2];
      }
    }
}

generated quantities { 
     real Se_at_cov_input;
     real Sp_at_cov_input;
     real lSe_at_cov_input;
     real lSp_at_cov_input;
     real DOR_at_cov_input;
     real LRp_at_cov_input;
     real LRn_at_cov_input;
     vector[2] pred_at_cov_input; 
     real Se_pred_at_cov_input; 
     real Sp_pred_at_cov_input;  
     real lSe_pred_at_cov_input; 
     real lSp_pred_at_cov_input; 
     real beta_at_cov_input; 
     real theta_at_cov_input; 
     real lambda_at_cov_input;
     real sigma_sq_theta_at_cov_input;
     real sigma_sq_alpha_at_cov_input;
     vector[101] Se; 
     vector[101] Sp;  
     vector[101] lSe; 
     vector[101] lSp; 
     corr_matrix[2] Omega; 
     matrix[2,2] Sigma; 
     real beta; 
     real sigma_sq_theta;
     real sigma_sq_alpha;
     vector[n_studies] se;
     vector[n_studies] sp;
     vector[2] var_pw[n_studies];
     vector[2*n_studies] var_pw2;
     vector[2*n_studies] inv_n;
     matrix[2*n_studies, 2*n_studies] B;
     matrix[2*n_studies, 2*n_studies] BI;
     matrix[2*n_studies, 2*n_studies] Z;
     matrix[2*n_studies, 2*n_studies] A;
     matrix[2, 2] G_one;
     int d[n_studies];
     int nd[n_studies];
     vector[101] roc_points_tpr;
     vector[101] roc_points_fpr;
     vector[101] logit_se_points;

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
   pred_at_cov_input = multi_normal_rng( mu + coeff*input_cov, Sigma );
   lSe_pred_at_cov_input = pred_at_cov_input[1];
   lSp_pred_at_cov_input = pred_at_cov_input[2];
   Se_pred_at_cov_input = inv_logit(pred_at_cov_input[1]);
   Sp_pred_at_cov_input = inv_logit(pred_at_cov_input[2]);
   DOR_at_cov_input = (Se_at_cov_input*Sp_at_cov_input) / ((1-Se_at_cov_input)*(1-Sp_at_cov_input));
   LRp_at_cov_input = Se_at_cov_input/(1-Sp_at_cov_input);
   LRn_at_cov_input = (1-Se_at_cov_input)/Sp_at_cov_input;

 // HSROC parameters (Harbourd et al.) (at covariate value inputted)
 beta_at_cov_input =  log(sigma[2]/sigma[1]); // shape parameter
 theta_at_cov_input = 0.5*( (((sigma[2]/sigma[1])^0.5)*lSe_at_cov_input) - (((sigma[1]/sigma[2])^0.5)*lSp_at_cov_input) ); 
 lambda_at_cov_input = ( ((sigma[2]/sigma[1])^0.5)*lSe_at_cov_input ) + ( ((sigma[1]/sigma[2])^0.5) * lSp_at_cov_input );
 sigma_sq_theta_at_cov_input  = 0.5*(sigma[1]*sigma[2] - Sigma[1,2]);
 sigma_sq_alpha_at_cov_input = 2*(sigma[1]*sigma[2] - Sigma[1,2]);

 // Points for HSROC curve  (at covariate value inputted)
  for (i in 1:100) { 
   logit_se_points[i] = lambda_at_cov_input*exp(-beta_at_cov_input/2) - exp(-beta_at_cov_input)*logit(roc_points_sp[i]);
   roc_points_tpr[i] = inv_logit(logit_se_points[i]);
   roc_points_fpr[i] = 1 - roc_points_sp[i];
  }


  for (n in 1:n_studies) { 
    d[n] =  TP[n] + FN[n];
    nd[n] = TN[n] + FP[n];
  }

    for (n in 1:n_studies) {
      se[n] = inv_logit(logit_pi[n,1]);
      sp[n] = inv_logit(logit_pi[n,2]);
      var_pw[n, 2] = se[n]*(1-se[n]);
      var_pw[n, 1] = sp[n]*(1-sp[n]);
    }

    for (n in 1:n_studies) {
      var_pw2[2*n] = var_pw[n, 2]; // diseased
      var_pw2[2*n - 1] = var_pw[n, 1];
    }

     Z = diag_matrix(rep_vector(1, 2*n_studies));
     B = diag_matrix(var_pw2);
     BI = inverse(B);
     A = diag_matrix(inv_n);
     G_one = Sigma;

}

