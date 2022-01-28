





#  BVM - Run Model - prior model - with loading bars --------------------------  -------------------------------
MA_run_model_priors_only <- function(id, 
                                     stan_model,
                                     stan_model_p_scale_priors,
                                     button,
                                     p_scale_priors_indicator) {  
  
  moduleServer(
    id,
    function(input, output, session) {
      
      rstan_options(auto_write = TRUE)
      options(mc.cores = parallel::detectCores())
      
      tfile <- tempfile(fileext = ".txt")
      r <- reactiveValues( progress_mtime = -1 )
      
      observeEvent(button(), {
        
        p_scale_priors_indicator <- p_scale_priors_indicator$p_scale_priors_indicator
        
        if (p_scale_priors_indicator == TRUE) {  # p-scale priors for Se and Sp   -------------------------------------------  
          
          r$bg_process <<-   callr::r_bg(
            
            func = function(stan_model_p_scale_priors, 
                            MA_prior_sens_lower95,
                            MA_prior_sens_upper95,
                            MA_prior_spec_lower95,
                            MA_prior_spec_upper95,
                            MA_prior_SD_sens_sd, 
                            MA_prior_SD_spec_sd) {
              
              rstan::sampling(
                object = stan_model_p_scale_priors,
                data =  list(   MA_prior_sens_lower95 = MA_prior_sens_lower95,
                                MA_prior_sens_upper95 = MA_prior_sens_upper95,
                                MA_prior_spec_lower95 = MA_prior_spec_lower95,
                                MA_prior_spec_upper95 = MA_prior_spec_upper95,
                                MA_prior_SD_sens_sd = MA_prior_SD_sens_sd,
                                MA_prior_SD_spec_sd = MA_prior_SD_spec_sd),
                chains = 4,
                iter = 400,
                warmup = 200,
                control=list(adapt_delta=0.80,
                             max_treedepth = 10),
                seed= 123
              )
            }, # end of function 
            args = list(stan_model_p_scale_priors = stan_model_p_scale_priors, 
                        MA_prior_sens_lower95 = input$MA_prior_sens_lower95,
                        MA_prior_sens_upper95 = input$MA_prior_sens_upper95,
                        MA_prior_spec_lower95 = input$MA_prior_spec_lower95,
                        MA_prior_spec_upper95 = input$MA_prior_spec_upper95,
                        MA_prior_SD_sens_sd = input$MA_prior_SD_sens_sd,
                        MA_prior_SD_spec_sd = input$MA_prior_SD_spec_sd), 
            stdout = tfile,
            supervise = TRUE
          )
          
        }
        else {   # logit-scale priors for Se and Sp   -------------------------------------------  
          
          r$bg_process <<-   callr::r_bg(
            
            func = function(stan_model, 
                            MA_prior_mean_sens_mu,
                            MA_prior_mean_sens_sd,
                            MA_prior_mean_spec_mu,
                            MA_prior_mean_spec_sd,
                            MA_prior_SD_sens_sd, 
                            MA_prior_SD_spec_sd) {
              
              rstan::sampling(
                object = stan_model,
                data =  list(   MA_prior_mean_sens_mu = MA_prior_mean_sens_mu,
                                MA_prior_mean_sens_sd = MA_prior_mean_sens_sd,
                                MA_prior_mean_spec_mu = MA_prior_mean_spec_mu,
                                MA_prior_mean_spec_sd = MA_prior_mean_spec_sd,
                                MA_prior_SD_sens_sd = MA_prior_SD_sens_sd,
                                MA_prior_SD_spec_sd = MA_prior_SD_spec_sd),
                chains = 4,
                iter = 400,
                warmup = 200,
                control=list(adapt_delta=0.80,
                             max_treedepth = 10),
                seed= 123
              )
            }, # end of function 
            args = list(stan_model = stan_model, 
                        MA_prior_mean_sens_mu = input$MA_prior_mean_sens_mu,
                        MA_prior_mean_sens_sd = input$MA_prior_mean_sens_sd,
                        MA_prior_mean_spec_mu = input$MA_prior_mean_spec_mu,
                        MA_prior_mean_spec_sd = input$MA_prior_mean_spec_sd,
                        MA_prior_SD_sens_sd = input$MA_prior_SD_sens_sd,
                        MA_prior_SD_spec_sd = input$MA_prior_SD_spec_sd), 
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
        
      })
      
      
      return( list( draws = reactive({ r$draws  }) ))
      
    }
  )
}






































#  BVM - Run Model - full model  ----------------------------------------------  ---------------------------

# server-side function to run bivariate model via Stan
MA_run_model <- function(id, 
                         dataset, 
                         stan_model, 
                         stan_model_p_scale_priors,
                         button, 
                         priors,
                         sampler_options, 
                         SA_indicator,
                         SA_indicator_local, 
                         p_scale_priors_indicator) {  
  
  
  moduleServer(
    id,
    function(input, output, session) {
      
      rstan_options(auto_write = TRUE)
      options(mc.cores = parallel::detectCores())
      
      observeEvent(button(), {
        shinyalert("Note",
                   "Be sure to check the Model Diagnostics tab
                    once the model has finished running",
                   type = "info")
      })
      
      tfile <- tempfile(fileext = ".txt")
      r <- reactiveValues( progress_mtime = -1)
      
      X <<- reactive({
        return(dataset())
      })
      
      
      # update X according to studies selected to exclude ("input$triallist")  if SA indicator is checked (note that inputting dataset as SA_data did not work when combined with  callr::r_bg due to a bug, so coded this way instead)
      # Note this observeEvent is only going to be triggered when "input$triallist" changes,  which originates from the "SA_list_studies_server" function which is tied to the  SA-specific namespace  (i.e. "SA_MA_model_id"), so it won't be triggered when using general namespace (i.e. "MA_model_id")
      
      observeEvent(c(input$triallist, button()), { 
        
        SA_indicator <-  SA_indicator$SA_indicator
        
        if ( SA_indicator == TRUE && SA_indicator_local == 1) { 
          
          X <<- reactive({
            SA_data <- dataset() %>% 
              dplyr::mutate(author2 = as.numeric(as.factor(author))) %>% 
              dplyr::filter(!(author2 %in% c(input$triallist))) %>% 
              dplyr::select(-author2) 
            return(SA_data) 
          })
        }
        
        
        if (SA_indicator_local == 0) { # if not doing SA leave X as full dataset (regardless of SA_indicator)
          
          X <<- reactive({
            return(dataset())
          })
          
        }
        
        
      })
      
      
      observeEvent(button(), {
        
        req(dataset())
        
        X <- X()
        
        p_scale_priors_indicator <- p_scale_priors_indicator$p_scale_priors_indicator
        
        SA_indicator <-  SA_indicator$SA_indicator
        
        print(SA_indicator)
        print(SA_indicator_local)
        
        if (p_scale_priors_indicator == TRUE) {  # p-scale priors for Se and Sp   -------------------------------------------  
          
          r$bg_process <<-   callr::r_bg(
            
            func = function(stan_model_p_scale_priors,
                            X,
                            MA_prior_sens_lower95,
                            MA_prior_sens_upper95,
                            MA_prior_spec_lower95,
                            MA_prior_spec_upper95,
                            MA_prior_SD_sens_sd,
                            MA_prior_SD_spec_sd,
                            chains,
                            total_iter,
                            warmup_iter,
                            adapt_delta,
                            max_treedepth,
                            seed) {
              
              rstan::sampling(
                object = stan_model_p_scale_priors,
                data =  list(n_studies = length(X$author),
                             holdout = rep(0, length(X$author)), 
                             TP = X$TP,  
                             FN = X$FN,  
                             TN = X$TN,  
                             FP = X$FP,  
                             MA_prior_sens_lower95 = MA_prior_sens_lower95, 
                             MA_prior_sens_upper95 = MA_prior_sens_upper95, 
                             MA_prior_spec_lower95 = MA_prior_spec_lower95, 
                             MA_prior_spec_upper95 = MA_prior_spec_upper95, 
                             MA_prior_SD_sens_sd = MA_prior_SD_sens_sd, 
                             MA_prior_SD_spec_sd = MA_prior_SD_spec_sd, 
                             roc_points_sp =  seq(by = 0.01, from =0, to = 1)), 
                chains = chains,
                iter = total_iter,
                warmup = warmup_iter,
                control=list(adapt_delta= adapt_delta, 
                             max_treedepth = max_treedepth),
                seed= seed)
            },
            args = list(stan_model_p_scale_priors = stan_model_p_scale_priors, 
                        X = X, 
                        MA_prior_sens_lower95 = priors$MA_prior_sens_lower95,
                        MA_prior_sens_upper95 = priors$MA_prior_sens_upper95,
                        MA_prior_spec_lower95 = priors$MA_prior_spec_lower95,
                        MA_prior_spec_upper95 = priors$MA_prior_spec_upper95,
                        MA_prior_SD_sens_sd = priors$MA_prior_SD_sens_sd,
                        MA_prior_SD_spec_sd = priors$MA_prior_SD_spec_sd, 
                        chains = sampler_options$MA_num_chains,
                        total_iter = sampler_options$MA_total_iter,
                        warmup_iter = sampler_options$MA_warmup_iter,
                        adapt_delta = sampler_options$MA_adapt_delta, 
                        max_treedepth = sampler_options$MA_max_treedepth,
                        seed= sampler_options$MA_seed
            ), 
            stdout = tfile,
            supervise = TRUE
          )
          
        } # end of if
        else {   # logit-scale priors for Se and Sp   -------------------------------------------  
          
          r$bg_process <<-   callr::r_bg(
            
            func = function(stan_model,
                            X,
                            MA_prior_mean_sens_mu,
                            MA_prior_mean_sens_sd,
                            MA_prior_mean_spec_mu,
                            MA_prior_mean_spec_sd,
                            MA_prior_SD_sens_sd,
                            MA_prior_SD_spec_sd,
                            chains,
                            total_iter,
                            warmup_iter,
                            adapt_delta,
                            max_treedepth,
                            seed) {
              
              rstan::sampling(
                object = stan_model,
                data =  list(n_studies = length(X$author),
                             holdout = rep(0, length(X$author)), 
                             TP = X$TP,  
                             FN = X$FN, 
                             TN = X$TN,  
                             FP = X$FP,  
                             MA_prior_mean_sens_mu = MA_prior_mean_sens_mu, 
                             MA_prior_mean_sens_sd = MA_prior_mean_sens_sd, 
                             MA_prior_mean_spec_mu = MA_prior_mean_spec_mu, 
                             MA_prior_mean_spec_sd = MA_prior_mean_spec_sd, 
                             MA_prior_SD_sens_sd = MA_prior_SD_sens_sd, 
                             MA_prior_SD_spec_sd = MA_prior_SD_spec_sd, 
                             roc_points_sp =  seq(by = 0.01, from =0, to = 1)), 
                chains = chains,
                iter = total_iter,
                warmup = warmup_iter,
                control=list(adapt_delta= adapt_delta, 
                             max_treedepth = max_treedepth),
                seed= seed)
            },
            args = list(stan_model = stan_model, 
                        X = X(), 
                        MA_prior_mean_sens_mu = priors$MA_prior_mean_sens_mu,
                        MA_prior_mean_sens_sd = priors$MA_prior_mean_sens_sd,
                        MA_prior_mean_spec_mu = priors$MA_prior_mean_spec_mu,
                        MA_prior_mean_spec_sd = priors$MA_prior_mean_spec_sd,
                        MA_prior_SD_sens_sd = priors$MA_prior_SD_sens_sd,
                        MA_prior_SD_spec_sd = priors$MA_prior_SD_spec_sd, 
                        chains = sampler_options$MA_num_chains,
                        total_iter = sampler_options$MA_total_iter,
                        warmup_iter = sampler_options$MA_warmup_iter,
                        adapt_delta = sampler_options$MA_adapt_delta, 
                        max_treedepth = sampler_options$MA_max_treedepth,
                        seed= sampler_options$MA_seed
            ), 
            stdout = tfile,
            supervise = TRUE
          )
          
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
        
      })
      
      
      # Output list -------------------------------------------------------------
      
      
      my_list <-             list(
        draws    = reactive({ r$draws  })
      )
      
      
      
      return(my_list)
      
    }
  )
}






