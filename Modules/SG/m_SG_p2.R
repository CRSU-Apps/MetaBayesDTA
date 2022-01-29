




#  Run Model (w/ loading bar ) - prior model  ---------------------------------- -----------------------------------------------------------

# server-side function to run prior-only model via Stan
SG_run_model_priors_only <- function(id, 
                                     stan_model, 
                                     stan_model_p_scale_priors,
                                     p_scale_priors_indicator,
                                     data, 
                                     button) { 
  moduleServer(
    id,
    function(input, output, session) {
      
      tfile <- tempfile(fileext = ".txt")
      r <- reactiveValues( progress_mtime = -1 )
      
       observeEvent(button(), {
          
                      req(data(),  input$covariate_subgroup)
                      
                      X <- data()
                      cov_index <- as.integer(as.double(input$covariate_subgroup)) - 1 # the selected covariate 
                      
                      C <- ncol(X)
                      Names <- colnames(X)
                      
                      if (C > 8 & Names[7] != "rob_PS") { 
                        j <<- 6   
                      } else { 
                        j <<- 13  
                      }
                      
                      X <- X %>% dplyr::arrange(!!as.name(colnames(X)[j + cov_index]))
                      
                      Cov <- if_else_Cov(X, cov_index)$out 
                      num_levels <- if_else_Cov(X, cov_index)$num_levels 
                      Cov_level <-  if_else_Cov_level(X, cov_index)
                      
                      p_scale_priors_indicator <- p_scale_priors_indicator$p_scale_priors_indicator
                      
             if (p_scale_priors_indicator == TRUE) {   # p-scale priors for Se and Sp  -------------------------------------------

                     
                          r$bg_process  <-   callr::r_bg(
                            
                                      func = function(stan_model_p_scale_priors, 
                                                      X, 
                                                      Cov, 
                                                      num_levels, 
                                                      Cov_level, 
                                                      SG_prior_sens_lower95,
                                                      SG_prior_sens_upper95,
                                                      SG_prior_spec_lower95,
                                                      SG_prior_spec_upper95,
                                                      SG_prior_SD_sens_sd,
                                                      SG_prior_SD_spec_sd) {
                                        
                                        rstan::sampling(
                                          object = stan_model_p_scale_priors,
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
                                                       SG_prior_sens_lower95 = SG_prior_sens_lower95,
                                                       SG_prior_sens_upper95 = SG_prior_sens_upper95,
                                                       SG_prior_spec_lower95 = SG_prior_spec_lower95,
                                                       SG_prior_spec_upper95 = SG_prior_spec_upper95,
                                                       SG_prior_SD_sens_sd = SG_prior_SD_sens_sd,
                                                       SG_prior_SD_spec_sd = SG_prior_SD_spec_sd),
                                          chains = 4,
                                          iter = 400,
                                          warmup = 200,
                                          control=list(adapt_delta=0.80, 
                                                       max_treedepth = 10),
                                          seed= 123)
                                      }, # end of function 
                                      args = list(stan_model_p_scale_priors = stan_model_p_scale_priors, 
                                                  X = X, 
                                                  Cov = Cov, 
                                                  num_levels = num_levels, 
                                                  Cov_level = Cov_level, 
                                                  SG_prior_sens_lower95 = input$SG_prior_sens_lower95,
                                                  SG_prior_sens_upper95 = input$SG_prior_sens_upper95,
                                                  SG_prior_spec_lower95 = input$SG_prior_spec_lower95,
                                                  SG_prior_spec_upper95 = input$SG_prior_spec_upper95,
                                                  SG_prior_SD_sens_sd = input$SG_prior_SD_sens_sd,
                                                  SG_prior_SD_spec_sd = input$SG_prior_SD_spec_sd), 
                                      stdout = tfile,
                                      supervise = TRUE
                                    )
                      
                   }
                   else {  # logit-scale priors for Se and Sp   -------------------------------------------  
                         
                         r$bg_process  <-   callr::r_bg(
                           
                                         func = function(stan_model, 
                                                         X, 
                                                         Cov, 
                                                         num_levels, 
                                                         Cov_level, 
                                                         SG_prior_mean_sens_mu,
                                                         SG_prior_mean_sens_sd,
                                                         SG_prior_mean_spec_mu,
                                                         SG_prior_mean_spec_sd,
                                                         SG_prior_SD_sens_sd,
                                                         SG_prior_SD_spec_sd) {
                                           
                                           rstan::sampling(
                                             object = stan_model,
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
                                                          SG_prior_mean_sens_mu = SG_prior_mean_sens_mu,
                                                          SG_prior_mean_sens_sd = SG_prior_mean_sens_sd,
                                                          SG_prior_mean_spec_mu = SG_prior_mean_spec_mu,
                                                          SG_prior_mean_spec_sd = SG_prior_mean_spec_sd,
                                                          SG_prior_SD_sens_sd = SG_prior_SD_sens_sd,
                                                          SG_prior_SD_spec_sd = SG_prior_SD_spec_sd),
                                             chains = 4,
                                             iter = 400,
                                             warmup = 200,
                                             control=list(adapt_delta=0.80, 
                                                          max_treedepth = 10),
                                             seed= 123)
                                         }, # end of function 
                                         args = list(stan_model = stan_model, 
                                                     X = X, 
                                                     Cov = Cov, 
                                                     num_levels = num_levels, 
                                                     Cov_level = Cov_level, 
                                                     SG_prior_mean_sens_mu = input$SG_prior_mean_sens_mu,
                                                     SG_prior_mean_sens_sd = input$SG_prior_mean_sens_sd,
                                                     SG_prior_mean_spec_mu = input$SG_prior_mean_spec_mu,
                                                     SG_prior_mean_spec_sd = input$SG_prior_mean_spec_sd,
                                                     SG_prior_SD_sens_sd = input$SG_prior_SD_sens_sd,
                                                     SG_prior_SD_spec_sd = input$SG_prior_SD_spec_sd), 
                                         stdout = tfile,
                                         supervise = TRUE
                                       )
                         
                     
                   }
                      
      
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
      
       })  # end of r$bgprocess
       
      return( list( draws = reactive({ r$draws  }) ))
      

      
    }
  )
}







#  Run Model (w/ loading bar )  - full model  --------------------------------- ------------------------------------------------------------
SG_run_model <- function(id, 
                         data, 
                         stan_model,
                         stan_model_p_scale_priors,
                         p_scale_priors_indicator,
                         button) {  
  moduleServer(
    id,
    function(input, output, session) {
      
      rstan_options(auto_write = TRUE)
      options(mc.cores = parallel::detectCores())
      
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
        
        req(data(), input$covariate_subgroup, cancelOutput = TRUE)
        
        X <- data()
        cov_index <- as.integer(as.double(input$covariate_subgroup)) - 1 
        
        Names <- colnames(X)
        C <- ncol(X)
        
        if (C > 8 & Names[7] != "rob_PS") { 
            j <<- 6 
        } else { 
            j <<- 13 
        }
        
        X <- X %>% arrange(!!as.name(colnames(X)[j + cov_index]))
        
        Cov <- if_else_Cov(X, cov_index)$out 
        num_levels <- if_else_Cov(X, cov_index)$num_levels 
        Cov_level <-  if_else_Cov_level(X, cov_index)
        
        if (input$p_scale_priors_indicator == TRUE) { # p-scale priors for Se and Sp   -------------------------------------------  
          
                    r$bg_process <-    callr::r_bg(
                      func = function(stan_model_p_scale_priors, 
                                      X, 
                                      Cov, 
                                      num_levels, 
                                      Cov_level, 
                                      SG_prior_sens_lower95,
                                      SG_prior_sens_upper95,
                                      SG_prior_spec_lower95,
                                      SG_prior_spec_upper95,
                                      SG_prior_SD_sens_sd,
                                      SG_prior_SD_spec_sd, 
                                      chains,
                                      total_iter,
                                      warmup_iter,
                                      adapt_delta, 
                                      max_treedepth,
                                      seed) {
                        
                        rstan::sampling(
                          object = stan_model_p_scale_priors, 
                          data = list(n_studies = length(X$author), 
                                      holdout = rep(0, length(X$author)),
                                      TP = X$TP,
                                      FN = X$FN, 
                                      TN = X$TN, 
                                      FP = X$FP,
                                      roc_points_sp = seq(by = 0.01, from = 0, to = 1), 
                                      Cov = Cov, 
                                      num_levels = num_levels,
                                      Cov_level =  Cov_level,
                                      ind_meta_reg = 1,
                                      SG_prior_sens_lower95 = SG_prior_sens_lower95,
                                      SG_prior_sens_upper95 = SG_prior_sens_upper95,
                                      SG_prior_spec_lower95 = SG_prior_spec_lower95,
                                      SG_prior_spec_upper95 = SG_prior_spec_upper95,
                                      SG_prior_SD_sens_sd = SG_prior_SD_sens_sd,
                                      SG_prior_SD_spec_sd = SG_prior_SD_spec_sd), 
                          chains = chains,
                          iter = total_iter,
                          warmup = warmup_iter,
                          control=list(adapt_delta= adapt_delta, 
                                       max_treedepth = max_treedepth),
                          seed= seed)
                      }, 
                      args = list(stan_model_p_scale_priors = stan_model_p_scale_priors, 
                                  X = X, 
                                  Cov = Cov, 
                                  num_levels = num_levels, 
                                  Cov_level = Cov_level, 
                                  SG_prior_sens_lower95= input$SG_prior_sens_lower95,
                                  SG_prior_sens_upper95= input$SG_prior_sens_upper95,
                                  SG_prior_spec_lower95= input$SG_prior_spec_lower95,
                                  SG_prior_spec_upper95= input$SG_prior_spec_upper95,
                                  SG_prior_SD_sens_sd = input$SG_prior_SD_sens_sd,
                                  SG_prior_SD_spec_sd = input$SG_prior_SD_spec_sd, 
                                  chains = input$MA_num_chains,
                                  total_iter = input$MA_total_iter,
                                  warmup_iter = input$MA_warmup_iter,
                                  adapt_delta = input$MA_adapt_delta, 
                                  max_treedepth = input$MA_max_treedepth,
                                  seed= input$MA_seed), 
                      stdout = tfile,
                      supervise = TRUE
                    )
                    
        }
        else {  # logit-scale priors for Se and Sp   -------------------------------------------  
          
          r$bg_process <-    callr::r_bg(
            func = function(stan_model, 
                            X, 
                            Cov, 
                            num_levels, 
                            Cov_level, 
                            SG_prior_mean_sens_mu,
                            SG_prior_mean_sens_sd,
                            SG_prior_mean_spec_mu,
                            SG_prior_mean_spec_sd,
                            SG_prior_SD_sens_sd,
                            SG_prior_SD_spec_sd, 
                            chains,
                            total_iter,
                            warmup_iter,
                            adapt_delta, 
                            max_treedepth,
                            seed) {
              
              rstan::sampling(
                object = stan_model, 
                data = list(n_studies = length(X$author), 
                            holdout = rep(0, length(X$author)),
                            TP = X$TP,
                            FN = X$FN, 
                            TN = X$TN, 
                            FP = X$FP,
                            roc_points_sp = seq(by = 0.01, from = 0, to = 1), 
                            Cov = Cov, 
                            num_levels = num_levels,
                            Cov_level =  Cov_level,
                            ind_meta_reg = 1,
                            SG_prior_mean_sens_mu = SG_prior_mean_sens_mu,
                            SG_prior_mean_sens_sd = SG_prior_mean_sens_sd,
                            SG_prior_mean_spec_mu = SG_prior_mean_spec_mu,
                            SG_prior_mean_spec_sd = SG_prior_mean_spec_sd,
                            SG_prior_SD_sens_sd = SG_prior_SD_sens_sd,
                            SG_prior_SD_spec_sd = SG_prior_SD_spec_sd), 
                chains = chains,
                iter = total_iter,
                warmup = warmup_iter,
                control=list(adapt_delta= adapt_delta, 
                             max_treedepth = max_treedepth),
                seed= seed)
            }, 
            args = list(stan_model = stan_model, 
                        X = X, 
                        Cov = Cov, 
                        num_levels = num_levels, 
                        Cov_level = Cov_level, 
                        SG_prior_mean_sens_mu = input$SG_prior_mean_sens_mu,
                        SG_prior_mean_sens_sd = input$SG_prior_mean_sens_sd,
                        SG_prior_mean_spec_mu = input$SG_prior_mean_spec_mu,
                        SG_prior_mean_spec_sd = input$SG_prior_mean_spec_sd,
                        SG_prior_SD_sens_sd = input$SG_prior_SD_sens_sd,
                        SG_prior_SD_spec_sd = input$SG_prior_SD_spec_sd, 
                        chains = input$MA_num_chains,
                        total_iter = input$MA_total_iter,
                        warmup_iter = input$MA_warmup_iter,
                        adapt_delta = input$MA_adapt_delta, 
                        max_treedepth = input$MA_max_treedepth,
                        seed= input$MA_seed), 
            stdout = tfile,
            supervise = TRUE
          )
          
          
          }
                    

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
      
    }) 
    
      
      return( list( draws = reactive({ r$draws  }) ))
      
    }
  )
}










