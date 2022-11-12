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
    real<lower=0, upper=1> MRcat_prior_sens_lower95;
    real<lower=0, upper=1> MRcat_prior_sens_upper95;
    real<lower=0, upper=1> MRcat_prior_spec_lower95;
    real<lower=0, upper=1> MRcat_prior_spec_upper95;
    real<lower=0> MRcat_prior_SD_sens_sd;
    real<lower=0> MRcat_prior_SD_spec_sd;
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
    cholesky_factor_corr[2] L_Omega; 
    vector<lower=0>[2] sigma; 
    vector[2] z[n_studies];
}

transformed parameters {
    vector[2] logit_pi[n_studies];
    vector[2] log_lik[n_studies]; // log_lik for each study 
    matrix[num_levels, 2] mu; 
    real MRcat_prior_mean_sens_mu;
    real MRcat_prior_mean_sens_sd;
    real MRcat_prior_mean_spec_mu;
    real MRcat_prior_mean_spec_sd;

    // convert interval priors to logit
     MRcat_prior_mean_sens_mu = ( logit(MRcat_prior_sens_upper95) + logit(MRcat_prior_sens_lower95) ) /2;
     MRcat_prior_mean_sens_sd = ( logit(MRcat_prior_sens_upper95) - logit(MRcat_prior_sens_lower95) ) /(2*1.96);
     MRcat_prior_mean_spec_mu = ( logit(MRcat_prior_spec_upper95) + logit(MRcat_prior_spec_lower95) ) /2;
     MRcat_prior_mean_spec_sd = ( logit(MRcat_prior_spec_upper95) - logit(MRcat_prior_spec_lower95) ) /(2*1.96);
     
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

     logit_pi[n, ] =   to_vector(mu[Cov[n], ]) + diag_pre_multiply(sigma, L_Omega) * z[n, ];  

  // Pointwise Likelihood 
    log_lik[n, 1] =  binomial_logit_lpmf(TP[n]  | pos[n], logit_pi[n, 1]);
    log_lik[n, 2] =  binomial_logit_lpmf(TN[n]  | neg[n], logit_pi[n, 2]);
    }
}

model {
    // Prior Model
       mu_pre[,1] ~ normal(MRcat_prior_mean_sens_mu, MRcat_prior_mean_sens_sd); 
       mu_pre[,2] ~ normal(MRcat_prior_mean_spec_mu, MRcat_prior_mean_spec_sd); 

  for (j in 1:2) 
       z[,j]  ~ std_normal();

       sigma[1] ~ normal(0, MRcat_prior_SD_sens_sd);
       sigma[2] ~ normal(0, MRcat_prior_SD_spec_sd);

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
     vector[num_levels] Se; 
     vector[num_levels] Sp;  
     vector[num_levels] DOR;  
     vector[num_levels] LRp;  
     vector[num_levels] LRn;  
     vector[num_levels] lSe; 
     vector[num_levels] lSp; 
     corr_matrix[2] Omega; 
     matrix[2,2] Sigma; 
     matrix[2, num_levels] pred; 
     vector[num_levels] Se_pred; 
     vector[num_levels] Sp_pred;  
     vector[num_levels] lSe_pred; 
     vector[num_levels] lSp_pred; 
     real beta; 
     vector[num_levels] theta; 
     vector[num_levels] lambda;
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
     matrix[num_levels, 101] roc_points_tpr;
     matrix[num_levels, 101] roc_points_fpr;
     matrix[num_levels, 101] logit_se_points;
     vector[choose(num_levels, 2)] diffs_Se;
     vector[choose(num_levels, 2)] diffs_Sp;
     vector[choose(num_levels, 2)] ratios_Se;
     vector[choose(num_levels, 2)] ratios_Sp;

  Omega = multiply_lower_tri_self_transpose(L_Omega); // between-study correlation matrix
  Sigma = quad_form_diag(Omega, sigma); // between-study var-cov matrix

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

 // Summary estimates for each value of covariate
 for (i in 1:num_levels) { 
  Se[i] = inv_logit(mu[Cov_level[i], 1]); 
  Sp[i] = inv_logit(mu[Cov_level[i], 2]); 
  lSe[i] = mu[Cov_level[i], 1];
  lSp[i] = mu[Cov_level[i], 2];
  pred[ ,i] = multi_normal_rng( mu[Cov_level[i],  ], Sigma );
  lSe_pred[i] = pred[1,i];
  lSp_pred[i] = pred[2,i];
  Se_pred[i] = inv_logit(pred[1,i]);
  Sp_pred[i] = inv_logit(pred[2,i]);
  DOR[i] = (Se[i]*Sp[i]) / ((1-Se[i])*(1-Sp[i]));
  LRp[i] = Se[i]/(1-Sp[i]);
  LRn[i] = (1-Se[i])/Sp[i];
 }

 // HSROC model parameters (Harbourd et al.) for each value of covariate
  beta =  log(sigma[2]/sigma[1]); // shape parameter
 for (i in 1:num_levels) { 
  theta[i] = 0.5*( (((sigma[2]/sigma[1])^0.5)*lSe[i]) - (((sigma[1]/sigma[2])^0.5)*lSp[i]) ); 
  lambda[i] = ( ((sigma[2]/sigma[1])^0.5)*lSe[i] ) + ( ((sigma[1]/sigma[2])^0.5) * lSp[i] );
 }
  sigma_sq_theta  = 0.5*(sigma[1]*sigma[2] - Sigma[1,2]);
  sigma_sq_alpha = 2*(sigma[1]*sigma[2] - Sigma[1,2]);

 // Points for HSROC curve 
 for (j in 1:num_levels) { 
  for (i in 1:100) { 
   logit_se_points[j,i] = lambda[j]*exp(-beta/2) - exp(-beta)*logit(roc_points_sp[i]);
   roc_points_tpr[j,i] = inv_logit(logit_se_points[j,i]);
   roc_points_fpr[j,i] = 1 - roc_points_sp[i];
  }
 }

 if (num_levels == 2) { 
     diffs_Se[1] = Se[1] - Se[2];
     diffs_Sp[1] = Sp[1] - Sp[2];
     ratios_Se[1] = Se[1]/Se[2];
     ratios_Sp[1] = Sp[1]/Sp[2];
   }
 if (num_levels == 3) { 
     diffs_Se[1] = Se[1] - Se[2];
     diffs_Se[2] = Se[1] - Se[3];
     diffs_Se[3] = Se[2] - Se[3];
     
     diffs_Sp[1] = Sp[1] - Sp[2];
     diffs_Sp[2] = Sp[1] - Sp[3];
     diffs_Sp[3] = Sp[2] - Sp[3];
     
     ratios_Se[1] = Se[1]/Se[2];
     ratios_Se[2] = Se[1]/Se[3];
     ratios_Se[3] = Se[2]/Se[3];
     
     ratios_Sp[1] = Sp[1]/Sp[2];
     ratios_Sp[2] = Sp[1]/Sp[3];
     ratios_Sp[3] = Sp[2]/Sp[3];
   }
 if (num_levels == 4) { 
     diffs_Se[1] = Se[1] - Se[2];
     diffs_Se[2] = Se[1] - Se[3];
     diffs_Se[3] = Se[1] - Se[4];
     diffs_Se[4] = Se[2] - Se[3];
     diffs_Se[5] = Se[2] - Se[4];
     diffs_Se[6] = Se[3] - Se[4];
     
     diffs_Sp[1] = Sp[1] - Sp[2];
     diffs_Sp[2] = Sp[1] - Sp[3];
     diffs_Sp[3] = Sp[1] - Sp[4];
     diffs_Sp[4] = Sp[2] - Sp[3];
     diffs_Sp[5] = Sp[2] - Sp[4];
     diffs_Sp[6] = Sp[3] - Sp[4];
     
     
     ratios_Se[1] = Se[1]/Se[2];
     ratios_Se[2] = Se[1]/Se[3];
     ratios_Se[3] = Se[1]/Se[4];
     ratios_Se[4] = Se[2]/Se[3];
     ratios_Se[5] = Se[2]/Se[4];
     ratios_Se[6] = Se[3]/Se[4];
     
     ratios_Sp[1] = Sp[1]/Sp[2];
     ratios_Sp[2] = Sp[1]/Sp[3];
     ratios_Sp[3] = Sp[1]/Sp[4];
     ratios_Sp[4] = Sp[2]/Sp[3];
     ratios_Sp[5] = Sp[2]/Sp[4];
     ratios_Sp[6] = Sp[3]/Sp[4];
     
   }
    if (num_levels == 5) { 
     diffs_Se[1] = Se[1] - Se[2];
     diffs_Se[2] = Se[1] - Se[3];
     diffs_Se[3] = Se[1] - Se[4];
     diffs_Se[4] = Se[1] - Se[5];
     diffs_Se[5] = Se[2] - Se[3];
     diffs_Se[6] = Se[2] - Se[4];
     diffs_Se[7] = Se[2] - Se[5];
     diffs_Se[8] = Se[3] - Se[4];
     diffs_Se[9] = Se[3] - Se[5];
     diffs_Se[10] = Se[4] - Se[5];
     
     diffs_Sp[1] = Sp[1] - Sp[2];
     diffs_Sp[2] = Sp[1] - Sp[3];
     diffs_Sp[3] = Sp[1] - Sp[4];
     diffs_Sp[4] = Sp[1] - Sp[5];
     diffs_Sp[5] = Sp[2] - Sp[3];
     diffs_Sp[6] = Sp[2] - Sp[4];
     diffs_Sp[7] = Sp[2] - Sp[5];
     diffs_Sp[8] = Sp[3] - Sp[4];
     diffs_Sp[9] = Sp[3] - Sp[5];
     diffs_Sp[10] = Sp[4] - Sp[5];
     
     
     ratios_Se[1] = Se[1]/Se[2];
     ratios_Se[2] = Se[1]/Se[3];
     ratios_Se[3] = Se[1]/Se[4];
     ratios_Se[4] = Se[1]/Se[5];
     ratios_Se[5] = Se[2]/Se[3];
     ratios_Se[6] = Se[2]/Se[4];
     ratios_Se[7] = Se[2]/Se[5];
     ratios_Se[8] = Se[3]/Se[4];
     ratios_Se[9] = Se[3]/Se[5];
     ratios_Se[10] = Se[4]/Se[5];
     
     ratios_Sp[1] = Sp[1]/Sp[2];
     ratios_Sp[2] = Sp[1]/Sp[3];
     ratios_Sp[3] = Sp[1]/Sp[4];
     ratios_Sp[4] = Sp[1]/Sp[5];
     ratios_Sp[5] = Sp[2]/Sp[3];
     ratios_Sp[6] = Sp[2]/Sp[4];
     ratios_Sp[7] = Sp[2]/Sp[5];
     ratios_Sp[8] = Sp[3]/Sp[4];
     ratios_Sp[9] = Sp[3]/Sp[5];
     ratios_Sp[10] = Sp[4]/Sp[5];
     
   }
   if (num_levels == 6) { 
     // diffs_Se[1] = Se[1] - Se[2];
     // diffs_Se[2] = Se[1] - Se[3];
     // diffs_Se[3] = Se[1] - Se[4];
     // diffs_Se[4] = Se[1] - Se[5];
     // diffs_Se[5] = Se[1] - Se[6];
     diffs_Se[1:5] = Se[1] - Se[2:6];
     
     // diffs_Se[6] = Se[2] - Se[3];
     // diffs_Se[7] = Se[2] - Se[4];
     // diffs_Se[8] = Se[2] - Se[5];
     // diffs_Se[9] = Se[2] - Se[6];
     diffs_Se[6:9] = Se[2] - Se[3:6];
     
     // diffs_Se[10] = Se[3] - Se[4];
     // diffs_Se[11] = Se[3] - Se[5];
     // diffs_Se[12] = Se[3] - Se[6];
     diffs_Se[10:12] = Se[3] - Se[4:6];
     
     // diffs_Se[13] = Se[4] - Se[5];
     // diffs_Se[14] = Se[4] - Se[6];
     diffs_Se[13:14] = Se[4] - Se[5:6];
     diffs_Se[15] = Se[5] - Se[6];
     

     diffs_Sp[1:5]   = Sp[1] - Sp[2:6];
     diffs_Sp[6:9]   = Sp[2] - Sp[3:6];
     diffs_Sp[10:12] = Sp[3] - Sp[4:6];
     diffs_Sp[13:14] = Sp[4] - Sp[5:6];
     diffs_Sp[15]    = Sp[5] - Sp[6];
     
     
     ratios_Se[1:5]   = Se[1] ./ Se[2:6];
     ratios_Se[6:9]   = Se[2] ./ Se[3:6];
     ratios_Se[10:12] = Se[3] ./ Se[4:6];
     ratios_Se[13:14] = Se[4] ./ Se[5:6];
     ratios_Se[15]    = Se[5] / Se[6];
     
     ratios_Sp[1:5]   = Sp[1] ./ Sp[2:6];
     ratios_Sp[6:9]   = Sp[2] ./ Sp[3:6];
     ratios_Sp[10:12] = Sp[3] ./ Sp[4:6];
     ratios_Sp[13:14] = Sp[4] ./ Sp[5:6];
     ratios_Sp[15]    = Sp[5] / Sp[6];
     
   }
   if (num_levels == 7) { 

     diffs_Se[1:6]   = Se[1] - Se[2:7];
     diffs_Se[7:11]  = Se[2] - Se[3:7];
     diffs_Se[12:15] = Se[3] - Se[4:7];
     diffs_Se[16:18] = Se[4] - Se[5:7];
     diffs_Se[19:20] = Se[5] - Se[6:7];
     diffs_Se[21]    = Se[6] - Se[7];

     diffs_Sp[1:6]   = Sp[1] - Sp[2:7];
     diffs_Sp[7:11]  = Sp[2] - Sp[3:7];
     diffs_Sp[12:15] = Sp[3] - Sp[4:7];
     diffs_Sp[16:18] = Sp[4] - Sp[5:7];
     diffs_Sp[19:20] = Sp[5] - Sp[6:7];
     diffs_Sp[21]    = Sp[6] - Sp[7];
     
     
     ratios_Se[1:6]    = Se[1] ./ Se[2:7];
     ratios_Se[7:11]   = Se[2] ./ Se[3:7];
     ratios_Se[12:15]  = Se[3] ./ Se[4:7];
     ratios_Se[16:18]  = Se[4] ./ Se[5:7];
     ratios_Se[19:20]  = Se[5] ./ Se[6:7];
     ratios_Se[21]     = Se[6] / Se[7];
     
     ratios_Sp[1:6]    = Sp[1] ./ Sp[2:7];
     ratios_Sp[7:11]   = Sp[2] ./ Sp[3:7];
     ratios_Sp[12:15]  = Sp[3] ./ Sp[4:7];
     ratios_Sp[16:18]  = Sp[4] ./ Sp[5:7];
     ratios_Sp[19:20]  = Sp[5] ./ Sp[6:7];
     ratios_Sp[21]     = Sp[6] / Sp[7];
     
   }
   if (num_levels == 8) { 

     diffs_Se[1:7]   = Se[1] - Se[2:8];
     diffs_Se[8:13]  = Se[2] - Se[3:8];
     diffs_Se[14:18] = Se[3] - Se[4:8];
     diffs_Se[19:22] = Se[4] - Se[5:8];
     diffs_Se[23:25] = Se[5] - Se[6:8];
     diffs_Se[26:27] = Se[6] - Se[7:8];
     diffs_Se[28]    = Se[7] - Se[8];

     diffs_Sp[1:7]   = Sp[1] - Sp[2:8];
     diffs_Sp[8:13]  = Sp[2] - Sp[3:8];
     diffs_Sp[14:18] = Sp[3] - Sp[4:8];
     diffs_Sp[19:22] = Sp[4] - Sp[5:8];
     diffs_Sp[23:25] = Sp[5] - Sp[6:8];
     diffs_Sp[26:27] = Sp[6] - Sp[7:8];
     diffs_Sp[28]    = Sp[7] - Sp[8];
     
     ratios_Se[1:7]    = Se[1] ./ Se[2:8];
     ratios_Se[8:13]   = Se[2] ./ Se[3:8];
     ratios_Se[14:18]  = Se[3] ./ Se[4:8];
     ratios_Se[19:22]  = Se[4] ./ Se[5:8];
     ratios_Se[23:25]  = Se[5] ./ Se[6:8];
     ratios_Se[26:27]  = Se[6] ./ Se[7:8];
     ratios_Se[28]     = Se[7] / Se[8];
     
     ratios_Sp[1:7]    = Sp[1] ./ Sp[2:8];
     ratios_Sp[8:13]   = Sp[2] ./ Sp[3:8];
     ratios_Sp[14:18]  = Sp[3] ./ Sp[4:8];
     ratios_Sp[19:22]  = Sp[4] ./ Sp[5:8];
     ratios_Sp[23:25]  = Sp[5] ./ Sp[6:8];
     ratios_Sp[26:27]  = Sp[6] ./ Sp[7:8];
     ratios_Sp[28]     = Sp[7] / Sp[8];
     
   }
   if (num_levels == 9) { 

     diffs_Se[1:8]   = Se[1] - Se[2:9];
     diffs_Se[9:15]  = Se[2] - Se[3:9];
     diffs_Se[16:21] = Se[3] - Se[4:9];
     diffs_Se[22:26] = Se[4] - Se[5:9];
     diffs_Se[27:30] = Se[5] - Se[6:9];
     diffs_Se[31:33] = Se[6] - Se[7:9];
     diffs_Se[34:35] = Se[7] - Se[8:9];
     diffs_Se[36]    = Se[8] - Se[9];

     diffs_Sp[1:8]   = Sp[1] - Sp[2:9];
     diffs_Sp[9:15]  = Sp[2] - Sp[3:9];
     diffs_Sp[16:21] = Sp[3] - Sp[4:9];
     diffs_Sp[22:26] = Sp[4] - Sp[5:9];
     diffs_Sp[27:30] = Sp[5] - Sp[6:9];
     diffs_Sp[31:33] = Sp[6] - Sp[7:9];
     diffs_Sp[34:35] = Sp[7] - Sp[8:9];
     diffs_Sp[36]    = Sp[8] - Sp[9];
     
     ratios_Se[1:8]    = Se[1] ./ Se[2:9];
     ratios_Se[9:15]   = Se[2] ./ Se[3:9];
     ratios_Se[16:21]  = Se[3] ./ Se[4:9];
     ratios_Se[22:26]  = Se[4] ./ Se[5:9];
     ratios_Se[27:30]  = Se[5] ./ Se[6:9];
     ratios_Se[31:33]  = Se[6] ./ Se[7:9];
     ratios_Se[34:35]  = Se[7] ./ Se[8:9];
     ratios_Se[36]     = Se[8] / Se[9];
     
     ratios_Sp[1:8]    = Sp[1] ./ Sp[2:9];
     ratios_Sp[9:15]   = Sp[2] ./ Sp[3:9];
     ratios_Sp[16:21]  = Sp[3] ./ Sp[4:9];
     ratios_Sp[22:26]  = Sp[4] ./ Sp[5:9];
     ratios_Sp[27:30]  = Sp[5] ./ Sp[6:9];
     ratios_Sp[31:33]  = Sp[6] ./ Sp[7:9];
     ratios_Sp[34:35]  = Sp[7] ./ Sp[8:9];
     ratios_Sp[36]     = Sp[8] / Sp[9];
     
   }
   if (num_levels == 10) { 

     diffs_Se[1:9]   = Se[1] - Se[2:10];
     diffs_Se[10:17] = Se[2] - Se[3:10];
     diffs_Se[18:24] = Se[3] - Se[4:10];
     diffs_Se[25:30] = Se[4] - Se[5:10];
     diffs_Se[31:35] = Se[5] - Se[6:10];
     diffs_Se[36:39] = Se[6] - Se[7:10];
     diffs_Se[40:42] = Se[7] - Se[8:10];
     diffs_Se[43:44] = Se[8] - Se[9:10];
     diffs_Se[45]    = Se[9] - Se[10];

     diffs_Sp[1:9]   = Sp[1] - Sp[2:10];
     diffs_Sp[10:17] = Sp[2] - Sp[3:10];
     diffs_Sp[18:24] = Sp[3] - Sp[4:10];
     diffs_Sp[25:30] = Sp[4] - Sp[5:10];
     diffs_Sp[31:35] = Sp[5] - Sp[6:10];
     diffs_Sp[36:39] = Sp[6] - Sp[7:10];
     diffs_Sp[40:42] = Sp[7] - Sp[8:10];
     diffs_Sp[43:44] = Sp[8] - Sp[9:10];
     diffs_Sp[45]    = Sp[9] - Sp[10];
     
     ratios_Se[1:9]    = Se[1] ./ Se[2:10];
     ratios_Se[10:17]  = Se[2] ./ Se[3:10];
     ratios_Se[18:24]  = Se[3] ./ Se[4:10];
     ratios_Se[25:30]  = Se[4] ./ Se[5:10];
     ratios_Se[31:35]  = Se[5] ./ Se[6:10];
     ratios_Se[36:39]  = Se[6] ./ Se[7:10];
     ratios_Se[40:42]  = Se[7] ./ Se[8:10];
     ratios_Se[43:44]  = Se[8] ./ Se[9:10];
     ratios_Se[45]     = Se[9] / Se[10];
     
     ratios_Sp[1:9]    = Sp[1] ./ Sp[2:10];
     ratios_Sp[10:17]  = Sp[2] ./ Sp[3:10];
     ratios_Sp[18:24]  = Sp[3] ./ Sp[4:10];
     ratios_Sp[25:30]  = Sp[4] ./ Sp[5:10];
     ratios_Sp[31:35]  = Sp[5] ./ Sp[6:10];
     ratios_Sp[36:39]  = Sp[6] ./ Sp[7:10];
     ratios_Sp[40:42]  = Sp[7] ./ Sp[8:10];
     ratios_Sp[43:44]  = Sp[8] ./ Sp[9:10];
     ratios_Sp[45]     = Sp[9] / Sp[10];
     
   }


}
