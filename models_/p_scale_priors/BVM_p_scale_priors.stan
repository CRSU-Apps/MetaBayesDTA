// Bivariate MA model of Reitsma et al. 

// logit_u = m + 1.96*sd, so
// m = logit_u - 1.96*sd
// 
// logit_l = m - 1.96*sd so 
// 1.96*sd = m - logit_l so 
// 
// 1.96*sd = logit_u - 1.96*sd - logit_l, so 
// 2*1.96*sd = logit_u - logit_l


data { 
    int n_studies; 
    int TP[n_studies]; 
    int FN[n_studies]; 
    int FP[n_studies]; 
    int TN[n_studies]; 
    vector[101] roc_points_sp; 
    // user inputs priors for Se/Sp directly on the probability scale (aka p scale) 
    real<lower=0, upper=1> MA_prior_sens_lower95; 
    real<lower=0, upper=1> MA_prior_sens_upper95; 
    real<lower=0, upper=1> MA_prior_spec_lower95; 
    real<lower=0, upper=1> MA_prior_spec_upper95; 
    // scale priors stay inputted on logit scale 
    real<lower=0> MA_prior_SD_sens_sd; 
    real<lower=0> MA_prior_SD_spec_sd; 
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
    vector[2] mu;  
    cholesky_factor_corr[2] L_Omega; 
    vector<lower=0>[2] sigma; 
    vector[2] z[n_studies];
}

transformed parameters {
    vector[2] logit_pi[n_studies];
    vector[2] log_lik[n_studies]; // log_lik for each study 
    real MA_prior_mean_sens_mu;
    real MA_prior_mean_sens_sd;
    real MA_prior_mean_spec_mu;
    real MA_prior_mean_spec_sd;
    
    
        // convert interval priors to logit
     MA_prior_mean_sens_mu = ( logit(MA_prior_sens_upper95) + logit(MA_prior_sens_lower95) ) / 2 ;
     MA_prior_mean_sens_sd = ( logit(MA_prior_sens_upper95) - logit(MA_prior_sens_lower95) ) / (2*1.96) ;
     MA_prior_mean_spec_mu = ( logit(MA_prior_spec_upper95) + logit(MA_prior_spec_lower95) )/ 2 ;
     MA_prior_mean_spec_sd = ( logit(MA_prior_spec_upper95) - logit(MA_prior_spec_lower95) )/ (2*1.96) ;
     
    for (n in 1:n_studies) {
        logit_pi[n, ] =   mu +   diag_pre_multiply(sigma, L_Omega) * z[n, ]  ; 

      // Pointwise Likelihood 
        log_lik[n, 1] =  binomial_logit_lpmf(TP[n]  | pos[n], logit_pi[n, 1]);
        log_lik[n, 2] =  binomial_logit_lpmf(TN[n]  | neg[n], logit_pi[n, 2]);
    }
}

model {

    //Prior Model
       mu[1] ~ normal(MA_prior_mean_sens_mu, MA_prior_mean_sens_sd); 
       mu[2] ~ normal(MA_prior_mean_spec_mu, MA_prior_mean_spec_sd); 
  for (j in 1:2) 
       z[,j]  ~ std_normal();

       sigma[1] ~ normal(0, MA_prior_SD_sens_sd);
       sigma[2] ~ normal(0, MA_prior_SD_spec_sd);
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
     real Se; 
     real Sp;  
     real lSe; 
     real lSp; 
     real DOR;
     real LRp;
     real LRn; 
     corr_matrix[2] Omega; 
     matrix[2,2] Sigma; 
     vector[2] pred; 
     real Se_pred; 
     real Sp_pred;  
     real lSe_pred; 
     real lSp_pred; 
     real beta; 
     real theta; 
     real lambda;
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

  for (n in 1:n_studies) { 
    d[n] =  TP[n] + FN[n];
    nd[n] = TN[n] + FP[n];
    inv_n[2*n] = 1/d[n];
    inv_n[2*n - 1] = 1/nd[n];
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

  Se = inv_logit(mu[1]); 
  Sp = inv_logit(mu[2]); 
  lSe = mu[1];
  lSp = mu[2];
  pred = multi_normal_rng( mu , Sigma );
  lSe_pred = pred[1];
  lSp_pred = pred[2];
  Se_pred = inv_logit(pred[1]);
  Sp_pred = inv_logit(pred[2]);

  DOR = (Se*Sp) / ((1-Se)*(1-Sp));
  LRp = Se/(1-Sp);
  LRn = (1-Se)/Sp;

 // HSROC parameters (Harbourd et al.)
 beta =  log(sigma[2]/sigma[1]); // shape parameter
 theta = 0.5*( (((sigma[2]/sigma[1])^0.5)*lSe) - (((sigma[1]/sigma[2])^0.5)*lSp) ); 
 lambda = ( ((sigma[2]/sigma[1])^0.5)*lSe ) + ( ((sigma[1]/sigma[2])^0.5) * lSp );
 sigma_sq_theta  = 0.5*(sigma[1]*sigma[2] - Sigma[1,2]);
 sigma_sq_alpha = 2*(sigma[1]*sigma[2] - Sigma[1,2]);

 // Points for HSROC curve 
  for (i in 1:100) { 
   logit_se_points[i] = lambda*exp(-beta/2) - exp(-beta)*logit(roc_points_sp[i]);
   roc_points_tpr[i] = inv_logit(logit_se_points[i]);
   roc_points_fpr[i] = 1 - roc_points_sp[i];
  }


}
