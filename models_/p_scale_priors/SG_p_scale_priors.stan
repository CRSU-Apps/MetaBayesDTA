// Bivariate MA model of Reitsma et al. 

data { 
    int n_studies; 
    int TP[n_studies];
    int FN[n_studies];
    int FP[n_studies];
    int TN[n_studies];
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

transformed data { 
    int pos[n_studies];
    int neg[n_studies];

  for (n in 1:n_studies) { 
    pos[n] = TP[n] + FN[n];
    neg[n] = TN[n] + FP[n];
  }
}
 
parameters { 
    matrix[num_levels, 2] mu_pre; 
    cholesky_factor_corr[2] L_Omega[num_levels]; 
    vector<lower=0>[2] sigma[num_levels]; 
    vector[2] z[n_studies]; 
}

transformed parameters {
    vector[2] logit_pi[n_studies];
    vector[2] log_lik[n_studies]; // log_lik for each study 
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

     logit_pi[n, ] =   to_vector(mu[Cov[n], ]) + diag_pre_multiply(sigma[Cov[n], ], L_Omega[Cov[n], ]) * z[n, ];  

  // Pointwise Likelihood 
    log_lik[n, 1] =  binomial_logit_lpmf(TP[n]  | pos[n], logit_pi[n, 1]);
    log_lik[n, 2] =  binomial_logit_lpmf(TN[n]  | neg[n], logit_pi[n, 2]);
    }
}

model {
    //Prior Model
       mu_pre[,1] ~ normal(SG_prior_mean_sens_mu, SG_prior_mean_sens_sd); 
       mu_pre[,2] ~ normal(SG_prior_mean_spec_mu, SG_prior_mean_spec_sd); 

  for (j in 1:2)  
       z[,j]  ~ std_normal();

  for (n in 1:n_studies) {
       sigma[Cov[n], 1] ~ normal(0, SG_prior_SD_sens_sd);
       sigma[Cov[n], 2] ~ normal(0, SG_prior_SD_spec_sd);
       L_Omega[Cov[n], ] ~ lkj_corr_cholesky(2); 
  }


  // Likelihood Model
    for (n in 1:n_studies) {
      if (holdout[n] == 0) {
        target += log_lik[n, 1];
        target += log_lik[n, 2];
      }
    }
}

generated quantities { 
     vector[num_levels] Se; 
     vector[num_levels] Sp;  
     vector[num_levels] DOR;  
     vector[num_levels] LRp;  
     vector[num_levels] LRn;  
     vector[num_levels] lSe; 
     vector[num_levels] lSp; 
     corr_matrix[2] Omega[num_levels]; 
     matrix[2,2] Sigma[num_levels]; 
     matrix[2, num_levels] pred; 
     vector[num_levels] Se_pred; 
     vector[num_levels] Sp_pred;  
     vector[num_levels] lSe_pred; 
     vector[num_levels] lSp_pred; 
     vector[num_levels] beta; 
     vector[num_levels] theta; 
     vector[num_levels] lambda;
     vector[num_levels] sigma_sq_theta;
     vector[num_levels] sigma_sq_alpha;
     vector[n_studies] se;
     vector[n_studies] sp;
     vector[2] var_pw[n_studies];
     vector[2*n_studies] var_pw2;
     vector[2*n_studies] inv_n;
     matrix[2*n_studies, 2*n_studies] B;
     matrix[2*n_studies, 2*n_studies] BI;
     matrix[2*n_studies, 2*n_studies] Z;
     matrix[2*n_studies, 2*n_studies] A;
     matrix[2,2] G_one[num_levels]; 
     int d[n_studies];
     int nd[n_studies];
     matrix[num_levels, 101] roc_points_tpr;
     matrix[num_levels, 101] roc_points_fpr;
     matrix[num_levels, 101] logit_se_points;

  for (n in 1:n_studies) {
   Omega[Cov[n], ] = multiply_lower_tri_self_transpose(L_Omega[Cov[n], ]); // between-study correlation matrix
   Sigma[Cov[n], ] = quad_form_diag(Omega[Cov[n], ], sigma[Cov[n], ]); // between-study var-cov matrix
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

  for (n in 1:n_studies) 
     G_one[Cov[n], ] = Sigma[Cov[n], ];
  

 // Summary estimates for each value of covariate
 for (i in 1:num_levels) { 
  Se[i] = inv_logit(mu[Cov_level[i], 1]); 
  Sp[i] = inv_logit(mu[Cov_level[i], 2]); 
  lSe[i] = mu[Cov_level[i], 1];
  lSp[i] = mu[Cov_level[i], 2];
  pred[ ,i] = multi_normal_rng( mu[Cov_level[i],  ], Sigma[i,] );
  lSe_pred[i] = pred[1,i];
  lSp_pred[i] = pred[2,i];
  Se_pred[i] = inv_logit(pred[1,i]);
  Sp_pred[i] = inv_logit(pred[2,i]);
  DOR[i] = (Se[i]*Sp[i]) / ((1-Se[i])*(1-Sp[i]));
  LRp[i] = Se[i]/(1-Sp[i]);
  LRn[i] = (1-Se[i])/Sp[i];
 }

 // HSROC model parameters (Harbourd et al.) for each value of covariate
 for (i in 1:num_levels) { 
  beta[i] =  log(sigma[i,2]/sigma[i,1]); // shape parameter
  theta[i] = 0.5*( (((sigma[i,2]/sigma[i,1])^0.5)*lSe[i]) - (((sigma[i,1]/sigma[i,2])^0.5)*lSp[i]) ); 
  lambda[i] = ( ((sigma[i,2]/sigma[i,1])^0.5)*lSe[i] ) + ( ((sigma[i,1]/sigma[i,2])^0.5) * lSp[i] );
  sigma_sq_theta[i]  = 0.5*(sigma[i,1]*sigma[i,2] - Sigma[i,1,2]);
  sigma_sq_alpha[i] = 2*(sigma[i,1]*sigma[i,2] - Sigma[i,1,2]);
 }

 // Points for HSROC curve 
 for (j in 1:num_levels) { 
  for (i in 1:100) { 
   logit_se_points[j,i] = lambda[j]*exp(-beta[j]/2) - exp(-beta[j])*logit(roc_points_sp[i]);
   roc_points_tpr[j,i] = inv_logit(logit_se_points[j,i]);
   roc_points_fpr[j,i] = 1 - roc_points_sp[i];
  }
 }
}
