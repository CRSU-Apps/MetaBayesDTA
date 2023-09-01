


#  Run Model - prior model  ----------------------------------  --------------------------------------------------------------------------------------------------------

# server-side function to run prior-only model via Stan
MR_run_model_priors_only <- function(id, 
                                     stan_model_cts, 
                                     stan_model_cat, 
                                     stan_model_cat_PO_p_scale_priors,
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
        
        # info / guidance on priors  [generic info - applies to ALL models]
        shinyalert(title = "Note on priors",
                   text =  paste("The default prior distributions ('priors') are weakly informative [see note 1]. Weakly informative priors do not incorporate any known subject-matter knowledge 
                               (e.g. known information on the sensitivity and/or specificity of the test(s) under evaluation), and are designed to be widely applicable and to help stabilize computation - 
                                 they will work relatively well for many cases [see note 2]. 
                               However, this is not guaranteed - there are no 'default' priors which will work for all situations. 
                               
                               This is more likely to be the case when one is assuming a perfect gold standard, and when there are many studies (e.g., > 10) available. 
                               
                               On the other hand, these default priors are less likely to work well when [see note 3]: 
                                 (i) modelling an imperfect gold standard, particularly when modelling conditional dependence [see note 4], and/or when assuming more random effects, and in the 
                               presence of multiple reference tests;
                               (ii) there are few studies; 
                               (iii) for meta-regression; 
                               (iv) in the presence of high between-study heterogeneity.  
                               
                               In this case, more informative priors may be needed in order to achieve normal model diagnostics - for example, more information on the sensitivity and/or specificity 
                               of the test(s) under evaluation may need to be incorporated into the model. 
                               
                               Moreover, since this is a Bayesian analysis, it generally makes sense to incorporate known information 
                               (i.e., subject-matter knowledge) into the statistical models wherever possible - regardless of whether it is possible to estimate 
                               a model without incorporating any known information.
                               
                               However, one must ensure that the appropriate uncertainty is encoded into the priors; otherwise, this could result in biased estimates of test accuracy. 
                               
                               For more information and guidance on priors in general, please see the following links:",  
                                 
                                 tags$a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3728030/", "#1 (test accuracy example - application to Tuberculous pleuritis - see section 2 and table 1.", 
                                        target="_blank"), 
                                 tags$a(href="https://discourse.mc-stan.org/t/priors-for-a-novice/9415/6", "#2 (regression example - not test accuracy specific).",
                                        target="_blank"), 
                                 
                                 " \n
                                NOTE:
                                [1]  The default priors are weakly informative so they do not incorporate any known subject-matter knowledge and allow the data to dominate. For example, 
                                     for the between-study deviation parameters for all three tests, the default priors for all of the models weakly pull the study-specific sensitivities 
                                     and specificities towards each other, whilst allowing for very large between-study heterogeneity if the data demands. For example, if $0.8$ is the
                                     value found for the summary sensitivity, and the data suggests a standard deviation equal to 2 (corresponding to a high degree 
                                     of between-study heterogeneity), then these priors would allow the study-specific sensitivities and specificities to be in the interval $(0.23, 0.94)$ with 95% probability. 
                                 
                                [2] 'work relatively well' means that the default, weakly informative priors will: (i)  result in adequate model diagnostics (see 'model diagnostics' tab); and 
                                (ii) be unlikely to bias the estimates of interest (i.e. sensitivity and specificity) due to placing too much prior weight on unreasonable values of test accuracy 
                                in other words, incorrectly assuming - a priori - that values of accuracy which are
                                not true and/or unlikely to be observed in clinical practice are in fact likely to be observed. 
                                
                                [3] This is not an exhaustive list. 
                                
                                [4] 'Modelling conditional dependence' refers to modelling the correlation between the test results for all individuals within 
                                each of the two disease classes - diseased and non-diseased. This should almost always be attempted wherever possible, due to the fact that tests are unlikely to be 
                                conditionally independent in clinical practice. 
                                "
                   ),
                   type = "info",
                   confirmButtonText = "OK",
                   html = TRUE)
      })
      
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
                     
                     stan_model_cts_model <- stan_model_cts$getModel()
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
                            iter = 2000,
                            warmup = 200,
                            control=list(adapt_delta=0.80, 
                                         max_treedepth = 10),
                            seed= 123)
                        }, # end of function 
                        args = list(stan_model_cts = stan_model_cts_model, 
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
                          stan_model_cat_PO_p_scale_priors_model <- stan_model_cat_PO_p_scale_priors$getModel()
                          r$bg_process  <<-     callr::r_bg(
                                                func = function(stan_model_cat_PO_p_scale_priors,
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
                                                    object = stan_model_cat_PO_p_scale_priors,
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
                                                    iter = 2000,
                                                    warmup = 200,
                                                    control=list(adapt_delta=0.80,
                                                                 max_treedepth = 10),
                                                    seed= 123)
                                                }, # end of function
                                                args = list(stan_model_cat_PO_p_scale_priors = stan_model_cat_PO_p_scale_priors_model,
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
                stan_model_cat_model <- stan_model_cat$getModel()
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
                                          iter = 2000,
                                          warmup = 200,
                                          control=list(adapt_delta=0.80,
                                                       max_treedepth = 10),
                                          seed= 123)
                                      }, # end of function
                                      args = list(stan_model_cat = stan_model_cat_model,
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
                  show_modal_spinner(spin = "atom", color = "#005398", text = "Running Model")
                  invalidateLater(millis = 1000, session)
                  mtime <- file.info(tfile)$mtime
                  if (mtime > r$progress_mtime) {
                    r$progress <- readLines(tfile)
                    r$progress_mtime <- mtime
                  }
                  if (!r$bg_process$is_alive()) {
                    r$draws <- r$bg_process$get_result() 
                    remove_modal_spinner()
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
      # Run the Garabage Collector to Ensure any excess memory used by stan is freed
      gc()
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
      
      observeEvent(button(), { 
        
      shinyalert(title = "Note",
                 text =  paste("Please ensure that you carefully check the 'Model diagnostics' tab once the model has finished running. 
                               If any of the results are abnormal, it is imperative that one does not use the outputs for this model, 
                               as the results are likely to be misleading and nonsensical. This can be due to a lack of convergence and/or 
                               parameter identifiability [see note 1] issues. 
                               
                               For guidance on how model diagnostics (e.g., posterior distribution and trace plots) should look like, 
                               please see the following links:",  
                               tags$a(href="https://cran.r-project.org/web/packages/JointAI/vignettes/AfterFitting.html", "#1.", target="_blank"), 
                               tags$a(href="https://m-clark.github.io/bayesian-basics/diagnostics.html#monitoring-convergence", "#2.", target="_blank"),
                               
                               "\n 
                     NOTE: 
                     [1] identifiability - we must to ensure that the number of parameters being estimated from our model is not greater than what is possible
                     for the given dataset; otherwise, it may be non-identifiable - which means that the model will give misleading, nonsensical results.
                     For example, it may estimate the sensitivity for a test to be equal to both both 0.20 and 0.80.
                     "       
                 ),
                 type = "info",
                 confirmButtonText = "OK",
                 html = TRUE)
            })
      
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
                       stan_model_cts_model <- stan_model_cts$getModel()
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
                      args = list(stan_model_cts = stan_model_cts_model, 
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
                      stan_model_cat_p_scale_priors_model <- stan_model_cat_p_scale_priors$getModel()
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
                            args = list(stan_model_cat_p_scale_priors = stan_model_cat_p_scale_priors_model, 
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
              stan_model_cat_model <- stan_model_cat$getModel()
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
                args = list(stan_model_cat = stan_model_cat_model, 
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
            show_modal_spinner(spin = "atom", color = "#005398", text = "Running Model")
            invalidateLater(millis = 1000, session)
            
            mtime <- file.info(tfile)$mtime
            
            if (mtime > r$progress_mtime) {
              r$progress <- readLines(tfile)
              r$progress_mtime <- mtime
            }
            if (!r$bg_process$is_alive()) {
              r$draws <- r$bg_process$get_result() 
              remove_modal_spinner()
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
          # Run the Garabage Collector to Ensure any excess memory used by stan is freed
          gc()
          return( list( draws = reactive({ r$draws  }) ))
      
    }
  )
}













































