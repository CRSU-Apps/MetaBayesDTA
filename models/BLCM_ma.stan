data {
    int<lower=1> num_ref;  // total number of ref tests 
  	int<lower=0> n_studies;	// total number of studies
  	int<lower=0> n[n_studies]; // n for each study
    int<lower=0> r[n_studies,4];
    int Ref[n_studies]; // indicator of ref. test 
    int<lower=0, upper=1> ci;
    int<lower=0, upper=1> SeR_fixed;
    int<lower=0, upper=1> SpR_fixed;
    int<lower=0, upper=1> SeI_fixed;
    int<lower=0, upper=1> SpI_fixed;
    vector[101] roc_points_sp;
 // priors for index test (fixed as 1 index test)
    real LCM_prior_mean_sens_index_mu;
    real LCM_prior_mean_sens_index_sd;
    real LCM_prior_mean_spec_index_mu;
    real LCM_prior_mean_spec_index_sd;
    real LCM_prior_SD_sens_index_sd;
    real LCM_prior_SD_spec_index_sd;
 // priors for reference (dynamic as 1+ reference tests)
    vector[num_ref] LCM_prior_mean_sens_ref_mu;
    vector[num_ref] LCM_prior_mean_sens_ref_sd;
    vector[num_ref] LCM_prior_mean_spec_ref_mu;
    vector[num_ref] LCM_prior_mean_spec_ref_sd;
 // static priors for reference (as between-study SD is assumed the same between different reference tests)
    real LCM_prior_SD_sens_ref_sd;
    real LCM_prior_SD_spec_ref_sd;
 // prev priors
    real LCM_prior_prev_a;
    real LCM_prior_prev_b;
	}

parameters {
    vector<lower = 0, upper = 1>[n_studies] p;
    vector<lower = 0, upper = 1>[n_studies] vec_nd;
    vector<lower = 0, upper = 1>[n_studies] vec_d;
    vector[2] index_logit_mu; 
    matrix[num_ref, 2] ref_logit_mu;
    vector<lower=0>[2] sigma_ref;
    vector<lower=0>[2] sigma_index;
    cholesky_factor_corr[2] L_Omega_ref; 
    cholesky_factor_corr[2] L_Omega_index; 
    matrix[n_studies,2] z_ref;
    matrix[n_studies,2] z_index;
	}

transformed parameters {
    vector<lower = 0, upper = 1>[4] pr[n_studies];
  	vector[n_studies] LBd;
  	vector[n_studies] LBnd;
  	vector[n_studies] UBd;
  	vector[n_studies] UBnd;
    matrix[n_studies, 2] ref_logit; 
    matrix[n_studies, 2] index_logit; 
    vector<lower = -1, upper = 1>[n_studies] cv1;
    vector<lower = -1, upper = 1>[n_studies] cv2;
    vector<lower = 0, upper = 1>[n_studies] f1;
    vector<lower = 0, upper = 1>[n_studies] f2;
    vector<lower = 0, upper = 1>[n_studies] c1;
  	vector<lower = 0, upper = 1>[n_studies] s1; 
    vector<lower = 0, upper = 1>[n_studies] c2;
    vector<lower = 0, upper = 1>[n_studies] s2;

     for (s in 1:n_studies) { 
                // test 1 (refs) 
                  if ( (SeR_fixed == 0) && (SpR_fixed == 0) ) 
                   ref_logit[s, 1:2] = to_row_vector( to_vector(ref_logit_mu[Ref[s], 1:2]) + 
                                       diag_pre_multiply(sigma_ref, L_Omega_ref) * to_vector(z_ref[s,1:2]) );
                  if ( (SeR_fixed == 1) && (SpR_fixed == 0) ) {
                   ref_logit[s, 1] = ref_logit_mu[Ref[s], 1];  
                   ref_logit[s, 2] = ref_logit_mu[Ref[s], 2] + sigma_ref[2]*z_ref[s,2];   
                  }
                  if ( (SeR_fixed == 0) && (SpR_fixed == 1) ) {
                   ref_logit[s, 1] = ref_logit_mu[Ref[s], 1] + sigma_ref[1]*z_ref[s,1];     
                   ref_logit[s, 2] = ref_logit_mu[Ref[s], 2];
                  }
                  if ( (SeR_fixed == 1) && (SpR_fixed == 1) ) {
                   ref_logit[s, 1] = ref_logit_mu[Ref[s], 1];  
                   ref_logit[s, 2] = ref_logit_mu[Ref[s], 2];
                  }
                   s1[s]  =  inv_logit(ref_logit[s,1]);
                   c1[s]  =  inv_logit(ref_logit[s,2]);

                // test 2 (index) 
                   index_logit[s, 1:2] = to_row_vector( index_logit_mu[1:2] +  
                                         diag_pre_multiply(sigma_index, L_Omega_index) * to_vector(z_index[s,1:2]) );  
                  if ( (SeI_fixed == 0) && (SpI_fixed == 0) ) 
                   index_logit[s, 1:2] = to_row_vector( to_vector(index_logit_mu) +  
                                         diag_pre_multiply(sigma_index, L_Omega_index) * to_vector(z_index[s,1:2]) );
                  if ( (SeI_fixed == 1) && (SpI_fixed == 0) ) {
                   index_logit[s, 1] = index_logit_mu[1];  
                   index_logit[s, 2] = index_logit_mu[2] + sigma_index[2]*z_index[s,2];   
                  }
                  if ( (SeI_fixed == 0) && (SpI_fixed == 1) ) {
                   index_logit[s, 1] = index_logit_mu[1] + sigma_index[1]*z_index[s,1];     
                   index_logit[s, 2] = index_logit_mu[2];
                  }
                  if ( (SeI_fixed == 1) && (SpI_fixed == 1) ) {
                   index_logit[s, 1] = index_logit_mu[1];  
                   index_logit[s, 2] = index_logit_mu[2];
                  }

                   s2[s]  =  inv_logit(index_logit[s,1]);
                   c2[s]  =  inv_logit(index_logit[s,2]);

                   LBd[s]  = fmax(-s1[s]*s2[s], -(1-s1[s])*(1-s2[s]));
                   LBnd[s] = fmax(-c1[s]*c2[s], -(1-c1[s])*(1-c2[s]));
                   UBd[s]  = fmin(s1[s]*(1-s2[s]), (1-s1[s])*s2[s]);
                   UBnd[s] = fmin(c1[s]*(1-c2[s]), (1-c1[s])*c2[s]);
       
       if (ci == 1) {      // conditional independence: 
        cv1[s] = 0; 
        cv2[s] = 0; 
       }
       else {        // conditional dependence, study-specific: 
        cv1[s] = LBd[s]  + (UBd[s] -LBd[s])  * vec_d[s] ;      // bounded in (LBd, UBd) given above
        cv2[s] = LBnd[s] + (UBnd[s]-LBnd[s]) * vec_nd[s] ;     // bounded in (LBnd, UBnd) given above
       }

     } 

   for (s in 1:n_studies) { 
       f1[s] = 1- c1[s];
       f2[s] = 1- c2[s];
     pr[s,1] = (p[s]*(s1[s]*s2[s]         +cv1[s] ))    +   ((1-p[s])*(f1[s] *f2[s]        +cv2[s]    )); //  TP
     pr[s,2] = (p[s]*(s1[s]*(1-s2[s])     -cv1[s]))   +     ((1-p[s])*(f1[s]*(1-f2[s])     -cv2[s]    )); // pos on GS, neg on index - FN
     pr[s,3] = (p[s]*((1-s1[s])*s2[s]     -cv1[s] ))   +    ((1-p[s])*((1-f1[s])*f2[s]     -cv2[s]    )); // neg on GS, pos on index - FP
     pr[s,4] = (p[s]*((1-s1[s])*(1-s2[s]) +cv1[s]))   +     ((1-p[s])*((1-f1[s])*(1-f2[s]) +cv2[s]    )); // TN
              } 
     }

model {
       // prior model
     	index_logit_mu[1]  ~ normal(LCM_prior_mean_sens_index_mu, LCM_prior_mean_sens_index_sd);
     	index_logit_mu[2]  ~ normal(LCM_prior_mean_spec_index_mu, LCM_prior_mean_spec_index_sd);

		ref_logit_mu[, 1] ~ normal(LCM_prior_mean_sens_ref_mu, LCM_prior_mean_sens_ref_sd);		
		ref_logit_mu[, 2] ~ normal(LCM_prior_mean_spec_ref_mu, LCM_prior_mean_spec_ref_sd);	

	    sigma_ref[1]    ~ normal(0, LCM_prior_SD_sens_ref_sd);
	    sigma_ref[2]    ~ normal(0, LCM_prior_SD_spec_ref_sd);

	    sigma_index[1]    ~ normal(0, LCM_prior_SD_sens_index_sd);
	    sigma_index[2]    ~ normal(0, LCM_prior_SD_spec_index_sd);

        p ~ beta(LCM_prior_prev_a, LCM_prior_prev_b);

        to_vector(z_ref) ~ std_normal();
        to_vector(z_index) ~ std_normal();

        L_Omega_ref ~ lkj_corr_cholesky(2);
        L_Omega_index ~ lkj_corr_cholesky(2);

	    // observational model (i.e likelihood) 
	    for (s in 1:n_studies) 
                    r[s,1:4] ~ multinomial(pr[s, 1:4]); 	    
 }

generated quantities {
        vector[num_ref+1] Se; 
        vector[num_ref+1] Sp; 
        vector[4] e[n_studies]; 
        vector[4] o[n_studies]; 
        vector[4] dv[n_studies]; 
        vector[4] ot[n_studies]; 
        vector[4] dt[n_studies]; 
        real dev[n_studies]; 
        real  ec[n_studies]; 
        real  oc[n_studies]; 
        real  dc[n_studies]; 
        real  resdev; 
        vector[n_studies] corr1; 
        vector[n_studies] corr2; 
        corr_matrix[2] Omega_ref; 
        matrix[2,2] Sigma_ref; 
        corr_matrix[2] Omega_index; 
        matrix[2,2] Sigma_index; 
        vector[n_studies] se_ref;
        vector[n_studies] sp_ref;
        vector[n_studies] se_index;
        vector[n_studies] sp_index;
        real Se_index; 
        real Sp_index; 
        vector[num_ref] Se_ref; 
        vector[num_ref] Sp_ref; 

     real DOR_index;
     real LRp_index;
     real LRn_index; 
     vector[2] pred_index; 
     real Se_pred_index; 
     real Sp_pred_index;  
     real lSe_pred_index; 
     real lSp_pred_index; 
     real beta_index; 
     real theta_index; 
     real lambda_index;
     real sigma_sq_theta_index;
     real sigma_sq_alpha_index;
     vector[101] roc_points_tpr_index;
     vector[101] roc_points_fpr_index;
     vector[101] logit_se_points_index;

     vector[num_ref] DOR_ref;
     vector[num_ref] LRp_ref;
     vector[num_ref] LRn_ref; 
     matrix[num_ref, 2] pred_ref; 
     vector[num_ref] Se_pred_ref; 
     vector[num_ref] Sp_pred_ref;  
     vector[num_ref] lSe_pred_ref; 
     vector[num_ref] lSp_pred_ref; 
     real beta_ref; 
     vector[num_ref] theta_ref; 
     vector[num_ref] lambda_ref;
     real sigma_sq_theta_ref;
     real sigma_sq_alpha_ref;
     matrix[num_ref, 101] roc_points_tpr_ref;
     matrix[num_ref, 101] roc_points_fpr_ref;
     matrix[num_ref, 101] logit_se_points_ref;



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

 // refs 
for (s in 1:n_studies) { 
  pred_ref[Ref[s], ] = to_row_vector( multi_normal_rng( ref_logit_mu[Ref[s], ] , Sigma_ref ) ) ;
  lSe_pred_ref[Ref[s]] = pred_ref[Ref[s], 1];
  lSp_pred_ref[Ref[s]] = pred_ref[Ref[s], 2];
  Se_pred_ref[Ref[s]] = inv_logit(pred_ref[Ref[s], 1]);
  Sp_pred_ref[Ref[s]] = inv_logit(pred_ref[Ref[s], 2]);

  DOR_ref[Ref[s]] = (Se_ref[Ref[s]]*Sp_ref[Ref[s]]) / ((1-Se_ref[Ref[s]])*(1-Sp_ref[Ref[s]]));
  LRp_ref[Ref[s]] =  Se_ref[Ref[s]]/(1-Sp_ref[Ref[s]]);
  LRn_ref[Ref[s]] = (1-Se_ref[Ref[s]])/Sp_ref[Ref[s]];

 // HSROC parameters (Harbourd et al.) 
 beta_ref =  log(sigma_ref[2]/sigma_ref[1]); // shape parameter
 
 theta_ref[Ref[s]] = 0.5*( (((sigma_ref[2]/sigma_ref[1])^0.5)*ref_logit_mu[Ref[s],1]) - 
                           (((sigma_ref[1]/sigma_ref[2])^0.5)*ref_logit_mu[Ref[s],2]) ); 
                         
 lambda_ref[Ref[s]] = ( ((sigma_ref[2]/sigma_ref[1])^0.5)*ref_logit_mu[Ref[s],1] ) +
                      ( ((sigma_ref[1]/sigma_ref[2])^0.5)*ref_logit_mu[Ref[s],2] );
                      
 sigma_sq_theta_ref  = 0.5*(sigma_ref[1]*sigma_ref[2] - Sigma_ref[1,2]);
 
 sigma_sq_alpha_ref = 2*(sigma_ref[1]*sigma_ref[2] - Sigma_ref[1,2]);

 // Points for HSROC curve 
  for (i in 1:101) { 
   logit_se_points_ref[Ref[s], i] = lambda_ref[Ref[s]]*exp(-beta_ref/2) - exp(-beta_ref)*logit(roc_points_sp[i]);
   roc_points_tpr_ref[Ref[s], i] = inv_logit(logit_se_points_ref[Ref[s], i]);
   roc_points_fpr_ref[Ref[s], i] = 1 - roc_points_sp[i];
  }
}

  // index 
  pred_index = multi_normal_rng(  index_logit_mu , Sigma_index );
  lSe_pred_index  = pred_index[1];
  lSp_pred_index  = pred_index[2];
  Se_pred_index  = inv_logit(pred_index[1]);
  Sp_pred_index  = inv_logit(pred_index[2]);

  DOR_index  = (Se_index *Sp_index ) / ((1-Se_index )*(1-Sp_index ));
  LRp_index  =  Se_index /(1-Sp_index );
  LRn_index  = (1-Se_index )/Sp_index ;

 // HSROC parameters (Harbourd et al.) 
 beta_index =  log(sigma_index[2]/sigma_index[1]); // shape parameter
 
 theta_index  = 0.5*( (((sigma_index[2]/sigma_index[1])^0.5)*index_logit_mu[1]) - 
                      (((sigma_index[1]/sigma_index[2])^0.5)*index_logit_mu[2]) ); 
                      
 lambda_index  = ( ((sigma_index[2]/sigma_index[1])^0.5)* index_logit_mu[1] ) + 
                 ( ((sigma_index[1]/sigma_index[2])^0.5)* index_logit_mu[2] );
                 
 sigma_sq_theta_index  = 0.5*(sigma_index[1]*sigma_index[2] - Sigma_index[1,2]);
 
 sigma_sq_alpha_index = 2*(sigma_index[1]*sigma_index[2] - Sigma_index[1,2]);

 // Points for HSROC curve 
  for (i in 1:101) { 
   logit_se_points_index[i] = lambda_index  *exp(-beta_index/2) - exp(-beta_index)*logit(roc_points_sp[i]);
   roc_points_tpr_index[i] = inv_logit(logit_se_points_index[i]);
   roc_points_fpr_index[i] = 1 - roc_points_sp[i];
  }



 for (s in 1:n_studies) {
   se_ref[s] = s1[s];
   sp_ref[s] = c1[s]; 
   se_index[s] = s2[s];
   sp_index[s] = c2[s]; 

   corr1[s] = cv1[s]/sqrt( s1[s]*s2[s]*(1-s1[s])*(1-s2[s]) );
   corr2[s] = cv2[s]/sqrt( c1[s]*c2[s]*(1-c1[s])*(1-c2[s]) );

  for (a in 1:4)  {
        ot[s,a] = r[s,a];
     if (ot[s,a] != 0) {
    e[s,a] = n[s] * pr[s,a] ;                        // expected cell count (2x2 tables so 4)
    o[s,a] = ot[s,a] / n[s] ;                         //number observed prob
    dv[s,a] = 2 * ot[s,a] * log(ot[s,a]/e[s,a]) ;
    dt[s,a] = o[s,a] - pr[s,a];
         }
        if (ot[s,a] == 0) { 
    e[s,a] = n[s] * pr[s,a] ;                        
    o[s,a] = ot[s,a] / n[s] ;   
    dt[s,a] = o[s,a] - pr[s,a];
             dv[s,a] = 0; 
   }
              }           
    dev[s] = sum(dv[s,]) ;   
       
  //    r is the observed cell count, o is observed cell count probability 
   //  CORRELATION RESIDUALS (Qu et al, 1996)
    ec[s] = (pr[s,1] - (pr[s,1]+pr[s,2]) * (pr[s,1]+pr[s,3]) )/             // expected correlations
    sqrt((pr[s,1]+pr[s,2]) * (pr[s,1]+pr[s,3]) * (1-pr[s,1]-pr[s,2]) * (1-pr[s,1]-pr[s,3]));
    oc[s] = (o[s,1] - (o[s,1]+o[s,2]) * (o[s,1]+o[s,3])) /            // number observed correlations
    sqrt((o[s,1]+o[s,2]) * (o[s,1]+o[s,3]) * (1-o[s,1]-o[s,2]) * (1-o[s,1]-o[s,3]));
    dc[s] = oc[s] - ec[s];                                           //  number correlation residual
   }    
    resdev = sum(dev[]);
}

