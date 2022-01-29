


#  Run Model - prior model  ----------------------------------  --------------------------------------------------------------------------------------------------------

# server-side function to run prior-only model via Stan
MR_run_model_priors_only <- function(id, 
                                     stan_model_cts, 
                                     stan_model_cat, 
                                     stan_model_cat_p_scale_priors,
                                     p_scale_priors_indicator,
                                     cts_cov_indicator = MR_cts_cov_indicator,
                                     data,
                                     button) { 
  
  moduleServer(
    id,
    function(input, output, session) {
      
      rstan_options(auto_write = TRUE)
      options(mc.cores = parallel::detectCores())
      
      tfile <- tempfile( fileext = ".txt" )
      r <- reactiveValues( progress_mtime = -1 )
      
      observeEvent(button(), {
                    
                    req(data(), input$covcheck_model)
                    
                    cov_index <- as.integer(as.double(input$covcheck_model)) - 1
                    
                    X <- data()
                    
                    Cov <- if_else_Cov(X, cov_index)$out
                    num_levels <- if_else_Cov(X, cov_index)$num_levels
                    Cov_level <-  if_else_Cov_level(X, cov_index)
                    
              
                    cts_cov_indicator <-  cts_cov_indicator$cts_cov_indicator
                    
            if (cts_cov_indicator == 1)  { # if continuous covariate ---------------------------
                 
                     Z_Cov <- if_else_Cov(X, cov_index)$out
                     cts_cov_points <- if_else_Cov(X, cov_index)$cts_cov_points
                     
                     
                     r$bg_process  <<-   callr::r_bg(
                        
                        func = function(stan_model_cts, 
                                        X, 
                                        Cov, 
                                        num_levels, 
                                        Cov_level, 
                                        Z_Cov, 
                                        cts_cov_points,
                                        centering_value,
                                        input_cov,
                                        MRcts_prior_mean_sens_mu,
                                        MRcts_prior_mean_sens_sd,
                                        MRcts_prior_mean_spec_mu,
                                        MRcts_prior_mean_spec_sd,
                                        MRcts_prior_SD_sens_sd,
                                        MRcts_prior_SD_spec_sd,
                                        MRcts_prior_coeff_sens_mean,
                                        MRcts_prior_coeff_sens_sd,
                                        MRcts_prior_coeff_spec_mean,
                                        MRcts_prior_coeff_spec_sd) {
                          
                          rstan::sampling(
                            object = stan_model_cts,
                            data =  list(n_studies = length(X$author), 
                                         holdout = rep(0, length(X$author)),
                                         TP = X$TP, 
                                         FN = X$FN, 
                                         TN = X$TN, 
                                         FP = X$FP, 
                                         roc_points_sp = seq(by = 0.01, from = 0, to = 1), 
                                         Cov = Cov,
                                         num_levels = num_levels, 
                                         Cov_level = Cov_level, 
                                         ind_meta_reg = 1,
                                         MRcts_prior_mean_sens_mu = MRcts_prior_mean_sens_mu,
                                         MRcts_prior_mean_sens_sd = MRcts_prior_mean_sens_sd,
                                         MRcts_prior_mean_spec_mu = MRcts_prior_mean_spec_mu,
                                         MRcts_prior_mean_spec_sd = MRcts_prior_mean_spec_sd,
                                         MRcts_prior_SD_sens_sd   = MRcts_prior_SD_sens_sd,
                                         MRcts_prior_SD_spec_sd   = MRcts_prior_SD_spec_sd,
                                         MRcts_prior_coeff_sens_mean = MRcts_prior_coeff_sens_mean,
                                         MRcts_prior_coeff_sens_sd   = MRcts_prior_coeff_sens_sd,
                                         MRcts_prior_coeff_spec_mean = MRcts_prior_coeff_spec_mean,
                                         MRcts_prior_coeff_spec_sd   = MRcts_prior_coeff_spec_sd,
                                         # for cts. cov only 
                                         cts_cov_points = cts_cov_points, 
                                         Z_Cov = Z_Cov,
                                         centering_value = centering_value,
                                         input_cov = input_cov),
                            chains = 4,
                            iter = 400,
                            warmup = 200,
                            control=list(adapt_delta=0.80, 
                                         max_treedepth = 10),
                            seed= 123)
                        }, # end of function 
                        args = list(stan_model_cts = stan_model_cts, 
                                    X = X, 
                                    Cov = Cov, 
                                    num_levels = num_levels, 
                                    Cov_level = Cov_level, 
                                    cts_cov_points = cts_cov_points -  input$centered_value_input, 
                                    Z_Cov = Z_Cov,
                                    centering_value = input$centered_value_input,
                                    input_cov = input$MRcts_input_cov -  input$centered_value_input,
                                    # e.g. if using same as centered value (the default - which is mean for both), 
                                    # then this will equal 0, 
                                    # which means the summary Se and Sp will be calculated at the mean value of the covariate 
                                    # (since the covariate is centered @ the mean)
                                    MRcts_prior_mean_sens_mu = input$MRcts_prior_mean_sens_mu, 
                                    MRcts_prior_mean_sens_sd = input$MRcts_prior_mean_sens_sd, 
                                    MRcts_prior_mean_spec_mu = input$MRcts_prior_mean_spec_mu, 
                                    MRcts_prior_mean_spec_sd = input$MRcts_prior_mean_spec_sd, 
                                    MRcts_prior_SD_sens_sd   = input$MRcts_prior_SD_sens_sd, 
                                    MRcts_prior_SD_spec_sd   = input$MRcts_prior_SD_spec_sd, 
                                    MRcts_prior_coeff_sens_mean = input$MRcts_prior_coeff_sens_mean, 
                                    MRcts_prior_coeff_sens_sd   = input$MRcts_prior_coeff_sens_sd,   
                                    MRcts_prior_coeff_spec_mean = input$MRcts_prior_coeff_spec_mean, 
                                    MRcts_prior_coeff_spec_sd   = input$MRcts_prior_coeff_spec_sd), 
                        stdout = tfile,
                        supervise = TRUE
                     ) # end of obj <- callr::r_bg(..)
                      
                      
               } # end of if
                    
        # discrete or categorical covariate  ------------------------------------------------------------------------------------
            else {
              
              Cov <- if_else_Cov(X, cov_index)$out 
              num_levels <- if_else_Cov(X, cov_index)$num_levels  
              Cov_level <- if_else_Cov_level(X, cov_index)
              

              p_scale_priors_indicator <- p_scale_priors_indicator$p_scale_priors_indicator

              if (p_scale_priors_indicator == TRUE) {
                
                          r$bg_process  <<-     callr::r_bg(
                                                func = function(stan_model_cat_p_scale_priors,
                                                                X,
                                                                Cov,
                                                                num_levels,
                                                                Cov_level,
                                                                MRcat_prior_sens_lower95,
                                                                MRcat_prior_sens_upper95,
                                                                MRcat_prior_spec_lower95,
                                                                MRcat_prior_spec_upper95,
                                                                MRcat_prior_SD_sens_sd,
                                                                MRcat_prior_SD_spec_sd) {
                        
                                                  rstan::sampling(
                                                    object = stan_model_cat_p_scale_priors,
                                                    data =  list(n_studies = length(X$author),
                                                                 holdout = rep(0, length(X$author)),
                                                                 TP = X$TP,
                                                                 FN = X$FN,
                                                                 TN = X$TN,
                                                                 FP = X$FP,
                                                                 roc_points_sp = seq(by = 0.01, from = 0, to = 1),
                                                                 Cov = Cov,
                                                                 num_levels = num_levels,
                                                                 Cov_level = Cov_level,
                                                                 ind_meta_reg = 1,
                                                                 MRcat_prior_sens_lower95 = MRcat_prior_sens_lower95,
                                                                 MRcat_prior_sens_upper95 = MRcat_prior_sens_upper95,
                                                                 MRcat_prior_spec_lower95 = MRcat_prior_spec_lower95,
                                                                 MRcat_prior_spec_upper95 = MRcat_prior_spec_upper95,
                                                                 MRcat_prior_SD_sens_sd = MRcat_prior_SD_sens_sd,
                                                                 MRcat_prior_SD_spec_sd = MRcat_prior_SD_spec_sd),
                                                    chains = 4,
                                                    iter = 400,
                                                    warmup = 200,
                                                    control=list(adapt_delta=0.80,
                                                                 max_treedepth = 10),
                                                    seed= 123)
                                                }, # end of function
                                                args = list(stan_model_cat_p_scale_priors = stan_model_cat_p_scale_priors,
                                                            X = X,
                                                            Cov = Cov,
                                                            num_levels = num_levels,
                                                            Cov_level = Cov_level,
                                                            MRcat_prior_sens_lower95 = input$MRcat_prior_sens_lower95,
                                                            MRcat_prior_sens_upper95 = input$MRcat_prior_sens_upper95,
                                                            MRcat_prior_spec_lower95 = input$MRcat_prior_spec_lower95,
                                                            MRcat_prior_spec_upper95 = input$MRcat_prior_spec_upper95,
                                                            MRcat_prior_SD_sens_sd = input$MRcat_prior_SD_sens_sd,
                                                            MRcat_prior_SD_spec_sd = input$MRcat_prior_SD_spec_sd),
                                                stdout = tfile,
                                                supervise = TRUE
                                        ) # end of obj <- callr::r_bg(..)
                          
              }
              else {
                
                r$bg_process  <<-     callr::r_bg(
                                      func = function(stan_model_cat,
                                                      X,
                                                      Cov,
                                                      num_levels,
                                                      Cov_level,
                                                      MRcat_prior_mean_sens_mu,
                                                      MRcat_prior_mean_sens_sd,
                                                      MRcat_prior_mean_spec_mu,
                                                      MRcat_prior_mean_spec_sd,
                                                      MRcat_prior_SD_sens_sd,
                                                      MRcat_prior_SD_spec_sd) {
                                        
                                        rstan::sampling(
                                          object = stan_model_cat,
                                          data =  list(n_studies = length(X$author),
                                                       holdout = rep(0, length(X$author)),
                                                       TP = X$TP,
                                                       FN = X$FN,
                                                       TN = X$TN,
                                                       FP = X$FP,
                                                       roc_points_sp = seq(by = 0.01, from = 0, to = 1),
                                                       Cov = Cov,
                                                       num_levels = num_levels,
                                                       Cov_level = Cov_level,
                                                       ind_meta_reg = 1,
                                                       MRcat_prior_mean_sens_mu = MRcat_prior_mean_sens_mu,
                                                       MRcat_prior_mean_sens_sd = MRcat_prior_mean_sens_sd,
                                                       MRcat_prior_mean_spec_mu = MRcat_prior_mean_spec_mu,
                                                       MRcat_prior_mean_spec_sd = MRcat_prior_mean_spec_sd,
                                                       MRcat_prior_SD_sens_sd = MRcat_prior_SD_sens_sd,
                                                       MRcat_prior_SD_spec_sd = MRcat_prior_SD_spec_sd),
                                          chains = 4,
                                          iter = 400,
                                          warmup = 200,
                                          control=list(adapt_delta=0.80,
                                                       max_treedepth = 10),
                                          seed= 123)
                                      }, # end of function
                                      args = list(stan_model_cat = stan_model_cat,
                                                  X = X,
                                                  Cov = Cov,
                                                  num_levels = num_levels,
                                                  Cov_level = Cov_level,
                                                  MRcat_prior_mean_sens_mu = input$MRcat_prior_mean_sens_mu,
                                                  MRcat_prior_mean_sens_sd = input$MRcat_prior_mean_sens_sd,
                                                  MRcat_prior_mean_spec_mu = input$MRcat_prior_mean_spec_mu,
                                                  MRcat_prior_mean_spec_sd = input$MRcat_prior_mean_spec_sd,
                                                  MRcat_prior_SD_sens_sd = input$MRcat_prior_SD_sens_sd,
                                                  MRcat_prior_SD_spec_sd = input$MRcat_prior_SD_spec_sd),
                                      stdout = tfile,
                                      supervise = TRUE
                                    ) # end of obj <- callr::r_bg(..)
                
              } # end of else (p-scale - false)

              } # end of else (cov - cat)

              
                    

      
      r$poll <- TRUE 
          
          observe({
                  req(r$bg_process, r$poll)
                  invalidateLater(millis = 1000, session)
                  mtime <- file.info(tfile)$mtime
                  if (mtime > r$progress_mtime) {
                    r$progress <- readLines(tfile)
                    r$progress_mtime <- mtime
                  }
                  if (!r$bg_process$is_alive()) {
                    r$draws <- r$bg_process$get_result() 
                    r$poll <- FALSE 
                  }
          })
          
          ## print progress
          output$progress_prior_model <- renderText({
            req(r$progress)
            paste(r$progress, 
                  collapse = "\n")
          })
          
      
      })
      
      return( list( draws = reactive({ r$draws  }) ))
      
      
    }
  )
}








#  Run Model  - full model  ----------------------------------------------------  --------------------------------------------------------------------------------

# server-side function to run model via Stan
MR_run_model <- function(id, 
                         data, 
                         stan_model_cts,
                         stan_model_cat,
                         stan_model_cat_p_scale_priors,
                         p_scale_priors_indicator,
                         cts_cov_indicator,
                         button) {  
  moduleServer(
    id,
    function(input, output, session) {
      
      shinyalert("Note",
                 "Be sure to check the Model Diagnostics tab
                    once the model has finished running.
                   For some guidance on how posterior and trace plots should look like, please see:", 
                 tags$a(href="https://policies.google.com/privacy?hl=en", "this link",target="_blank"), 
                 "Note that informative priors may be needed for normal model diagnostics, particularly for 
                   models not assuming a gold standard. In general, known information on the sensitivity and 
                   specificity of tests should be incorporated into the models whnever possible",
                 type = "info")
      
      tfile <- tempfile(fileext = ".txt")
      r <- reactiveValues( progress_mtime = -1 )
      
      observeEvent(button(), {
          
        req(data(), input$covcheck_model)
        
        X <- data()
        cov_index <- as.integer(as.double(input$covcheck_model)) - 1
        

        Cov_level <-  if_else_Cov_level(X, cov_index) # for both cts and discrete/cat MR
        
        cts_cov_indicator <-  cts_cov_indicator$cts_cov_indicator
        
        # cts covariate ---------------------------------------------------------------------
        if (cts_cov_indicator == 1)  { # if continuous covariate ---------------------------

                       Z_Cov <- if_else_Cov(X, cov_index)$out 
                       cts_cov_points <- if_else_Cov(X, cov_index)$cts_cov_points
                       
                       r$bg_process  <-   callr::r_bg(
                          
                          func = function(stan_model_cts, 
                                          X, 
                                          Cov_level,  
                                          Z_Cov, 
                                          cts_cov_points, 
                                          centering_value, 
                                          input_cov,
                                          MRcts_prior_mean_sens_mu,
                                          MRcts_prior_mean_sens_sd,
                                          MRcts_prior_mean_spec_mu,
                                          MRcts_prior_mean_spec_sd,
                                          MRcts_prior_SD_sens_sd,
                                          MRcts_prior_SD_spec_sd,
                                          MRcts_prior_coeff_sens_mean,
                                          MRcts_prior_coeff_sens_sd,
                                          MRcts_prior_coeff_spec_mean,
                                          MRcts_prior_coeff_spec_sd,
                                          chains,
                                          total_iter,
                                          warmup_iter,
                                          adapt_delta, 
                                          max_treedepth,
                                          seed) {
                            
                          rstan::sampling(
                            object = stan_model_cts,
                            data =    list(n_studies = length(X$author), 
                                           holdout = rep(0, length(X$author)),
                                           TP = X$TP, 
                                           FN = X$FN, 
                                           TN = X$TN, 
                                           FP = X$FP, 
                                           roc_points_sp = seq(by = 0.01, from = 0, to = 1), 
                                           Cov_level = Cov_level,
                                           ind_meta_reg = 1,
                                           MRcts_prior_mean_sens_mu = MRcts_prior_mean_sens_mu,
                                           MRcts_prior_mean_sens_sd = MRcts_prior_mean_sens_sd,
                                           MRcts_prior_mean_spec_mu = MRcts_prior_mean_spec_mu,
                                           MRcts_prior_mean_spec_sd = MRcts_prior_mean_spec_sd,
                                           MRcts_prior_SD_sens_sd = MRcts_prior_SD_sens_sd,
                                           MRcts_prior_SD_spec_sd = MRcts_prior_SD_spec_sd,
                                           MRcts_prior_coeff_sens_mean = MRcts_prior_coeff_sens_mean,
                                           MRcts_prior_coeff_sens_sd = MRcts_prior_coeff_sens_sd,
                                           MRcts_prior_coeff_spec_mean = MRcts_prior_coeff_spec_mean,
                                           MRcts_prior_coeff_spec_sd = MRcts_prior_coeff_spec_sd,
                                           # for cts. cov only 
                                           cts_cov_points = cts_cov_points, 
                                           Z_Cov = Z_Cov,
                                           centering_value = centering_value,
                                           input_cov = input_cov),
                            chains = chains,
                            iter = total_iter,
                            warmup = warmup_iter,
                            control=list(adapt_delta= adapt_delta, 
                                         max_treedepth = max_treedepth),
                            seed= seed)
                      }, # end of function 
                      args = list(stan_model_cts = stan_model_cts, 
                                  X = X, 
                                  Cov_level = Cov_level, 
                                  cts_cov_points = cts_cov_points -  input$centered_value_input, 
                                  Z_Cov = Z_Cov,
                                  centering_value = input$centered_value_input,
                                  input_cov = input$MRcts_input_cov -  input$centered_value_input,
                                  # e.g. if using same as centered value (the default - which is mean for both), 
                                  # then this will equal 0, 
                                  # which means the summary Se and Sp will be calculated at the mean value of the covariate 
                                  # (since the covariate is centered @ the mean)
                                  MRcts_prior_mean_sens_mu = input$MRcts_prior_mean_sens_mu, 
                                  MRcts_prior_mean_sens_sd = input$MRcts_prior_mean_sens_sd, 
                                  MRcts_prior_mean_spec_mu = input$MRcts_prior_mean_spec_mu, 
                                  MRcts_prior_mean_spec_sd = input$MRcts_prior_mean_spec_sd, 
                                  MRcts_prior_SD_sens_sd   = input$MRcts_prior_SD_sens_sd, 
                                  MRcts_prior_SD_spec_sd   = input$MRcts_prior_SD_spec_sd, 
                                  MRcts_prior_coeff_sens_mean = input$MRcts_prior_coeff_sens_mean, 
                                  MRcts_prior_coeff_sens_sd   = input$MRcts_prior_coeff_sens_sd,   
                                  MRcts_prior_coeff_spec_mean = input$MRcts_prior_coeff_spec_mean, 
                                  MRcts_prior_coeff_spec_sd   = input$MRcts_prior_coeff_spec_sd, 
                                  chains = input$MA_num_chains,
                                  total_iter = input$MA_total_iter,
                                  warmup_iter = input$MA_warmup_iter,
                                  adapt_delta = input$MA_adapt_delta, 
                                  max_treedepth = input$MA_max_treedepth,
                                  seed= input$MA_seed),
                      stdout = tfile,
                      supervise = TRUE
                      ) # end of obj <- callr::r_bg(..)
                        
                        
                  } # end of if   
        
          # discrete or categorical covariate  ---------------------------------------------------------------------
          else {
                    
            Cov <- if_else_Cov(X, cov_index)$out 
            num_levels <- if_else_Cov(X, cov_index)$num_levels 
            
            print( input$MRcat_prior_mean_sens_mu )
            
            p_scale_priors_indicator <- p_scale_priors_indicator$p_scale_priors_indicator
            
            if (p_scale_priors_indicator == TRUE) {    # p-scale priors for Se and Sp ---------------------------------------------------------
              
                      r$bg_process  <-    callr::r_bg(
                                
                                func = function(stan_model_cat_p_scale_priors,
                                                X,
                                                Cov,
                                                num_levels,
                                                Cov_level,
                                                MRcat_prior_sens_lower95,
                                                MRcat_prior_sens_upper95,
                                                MRcat_prior_spec_lower95,
                                                MRcat_prior_spec_upper95,
                                                MRcat_prior_SD_sens_sd,
                                                MRcat_prior_SD_spec_sd,
                                                chains,
                                                total_iter,
                                                warmup_iter,
                                                adapt_delta, 
                                                max_treedepth,
                                                seed) {
                                  
                              rstan::sampling(
                                object = stan_model_cat_p_scale_priors,
                                data =  list(n_studies = length(X$author), 
                                             holdout = rep(0, length(X$author)),
                                             TP = X$TP, 
                                             FN = X$FN, 
                                             TN = X$TN,
                                             FP = X$FP,
                                             roc_points_sp = seq(by = 0.01, from = 0, to = 1), 
                                             Cov = Cov,
                                             num_levels = num_levels,
                                             Cov_level = Cov_level,
                                             ind_meta_reg = 1,
                                             MRcat_prior_sens_lower95 = MRcat_prior_sens_lower95,
                                             MRcat_prior_sens_upper95 = MRcat_prior_sens_upper95,
                                             MRcat_prior_spec_lower95 = MRcat_prior_spec_lower95,
                                             MRcat_prior_spec_upper95 = MRcat_prior_spec_upper95,
                                             MRcat_prior_SD_sens_sd = MRcat_prior_SD_sens_sd,
                                             MRcat_prior_SD_spec_sd = MRcat_prior_SD_spec_sd),
                                chains = chains,
                                iter = total_iter,
                                warmup = warmup_iter,
                                control=list(adapt_delta= adapt_delta, 
                                             max_treedepth = max_treedepth),
                                seed= seed)
                            },  # end of function 
                            args = list(stan_model_cat_p_scale_priors = stan_model_cat_p_scale_priors, 
                                        X = X, 
                                        Cov = Cov, 
                                        num_levels = num_levels, 
                                        Cov_level = Cov_level, 
                                        MRcat_prior_sens_lower95 = input$MRcat_prior_sens_lower95,
                                        MRcat_prior_sens_upper95 = input$MRcat_prior_sens_upper95,
                                        MRcat_prior_spec_lower95 = input$MRcat_prior_spec_lower95,
                                        MRcat_prior_spec_upper95 = input$MRcat_prior_spec_upper95,
                                        MRcat_prior_SD_sens_sd = input$MRcat_prior_SD_sens_sd,
                                        MRcat_prior_SD_spec_sd = input$MRcat_prior_SD_spec_sd,
                                        chains = input$MA_num_chains,
                                        total_iter = input$MA_total_iter,
                                        warmup_iter = input$MA_warmup_iter,
                                        adapt_delta = input$MA_adapt_delta, 
                                        max_treedepth = input$MA_max_treedepth,
                                        seed= input$MA_seed), 
                            stdout = tfile,
                            supervise = TRUE
                            ) # end of obj <- callr::r_bg(..)
            }
            else {  # logit-scale priors for Se and Sp ---------------------------------------------------------
              
              r$bg_process  <-    callr::r_bg(
                
                func = function(stan_model_cat,
                                X,
                                Cov,
                                num_levels,
                                Cov_level,
                                MRcat_prior_mean_sens_mu,
                                MRcat_prior_mean_sens_sd,
                                MRcat_prior_mean_spec_mu,
                                MRcat_prior_mean_spec_sd,
                                MRcat_prior_SD_sens_sd,
                                MRcat_prior_SD_spec_sd,
                                chains,
                                total_iter,
                                warmup_iter,
                                adapt_delta, 
                                max_treedepth,
                                seed) {
                  
                  rstan::sampling(
                    object = stan_model_cat,
                    data =  list(n_studies = length(X$author), 
                                 holdout = rep(0, length(X$author)),
                                 TP = X$TP, 
                                 FN = X$FN, 
                                 TN = X$TN,
                                 FP = X$FP,
                                 roc_points_sp = seq(by = 0.01, from = 0, to = 1), 
                                 Cov = Cov,
                                 num_levels = num_levels,
                                 Cov_level = Cov_level,
                                 ind_meta_reg = 1,
                                 MRcat_prior_mean_sens_mu = MRcat_prior_mean_sens_mu,
                                 MRcat_prior_mean_sens_sd = MRcat_prior_mean_sens_sd,
                                 MRcat_prior_mean_spec_mu = MRcat_prior_mean_spec_mu,
                                 MRcat_prior_mean_spec_sd = MRcat_prior_mean_spec_sd,
                                 MRcat_prior_SD_sens_sd = MRcat_prior_SD_sens_sd,
                                 MRcat_prior_SD_spec_sd = MRcat_prior_SD_spec_sd),
                    chains = chains,
                    iter = total_iter,
                    warmup = warmup_iter,
                    control=list(adapt_delta= adapt_delta, 
                                 max_treedepth = max_treedepth),
                    seed= seed)
                },  # end of function 
                args = list(stan_model_cat = stan_model_cat, 
                            X = X, 
                            Cov = Cov, 
                            num_levels = num_levels, 
                            Cov_level = Cov_level, 
                            MRcat_prior_mean_sens_mu = input$MRcat_prior_mean_sens_mu,
                            MRcat_prior_mean_sens_sd = input$MRcat_prior_mean_sens_sd,
                            MRcat_prior_mean_spec_mu = input$MRcat_prior_mean_spec_mu,
                            MRcat_prior_mean_spec_sd = input$MRcat_prior_mean_spec_sd,
                            MRcat_prior_SD_sens_sd = input$MRcat_prior_SD_sens_sd,
                            MRcat_prior_SD_spec_sd = input$MRcat_prior_SD_spec_sd,
                            chains = input$MA_num_chains,
                            total_iter = input$MA_total_iter,
                            warmup_iter = input$MA_warmup_iter,
                            adapt_delta = input$MA_adapt_delta, 
                            max_treedepth = input$MA_max_treedepth,
                            seed= input$MA_seed), 
                stdout = tfile,
                supervise = TRUE
              ) # end of obj <- callr::r_bg(..)
              
              }
            
            } # end of else 
                

    
          r$poll <- TRUE 
          
          observe({
            req(r$bg_process, r$poll)
            
            invalidateLater(millis = 1000, session)
            
            mtime <- file.info(tfile)$mtime
            
            if (mtime > r$progress_mtime) {
              r$progress <- readLines(tfile)
              r$progress_mtime <- mtime
            }
            if (!r$bg_process$is_alive()) {
              r$draws <- r$bg_process$get_result() 
              r$poll <- FALSE 
            }
          })
          
          ## print progress
          output$progress_main_model <- renderText({
            req(r$progress)
            paste(r$progress, 
                  collapse = "\n")
          })
          
     }) # end of r$bgprocess
     
          return( list( draws = reactive({ r$draws  }) ))
      
    }
  )
}













































