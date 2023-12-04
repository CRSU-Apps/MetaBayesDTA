






# LCM -  Run  Model - prior model  --------------------------------------------  ----------------------------------------------

# server-side function to run LCM prior-only model via Stan
LCM_run_model_priors_only <- function(id, 
                                      stan_model,
                                      stan_model_p_scale_priors,
                                      data,
                                      p_scale_priors_indicator,
                                      priors,
                                      button) {


  observeEvent(button(), {
    

    if (is.null(data()$reference.cat) == TRUE) {
      
      shinyalert("Warning",
                 "To use meta-analysis without a gold standard, please select a dataset with covariates,
                 and ensure that the reference test data is inputted as a column named 'reference.cat'",
                 type = "error")
    }
    else { }
  
  })
  
  
        
  moduleServer(
    id,
    function(input, output, session) {
      
      rstan_options(auto_write = TRUE)
      options(mc.cores = parallel::detectCores())
      
      tfile <- tempfile(fileext = ".txt")
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
      
      X <- reactive({
        data() 
      })
      
      observeEvent(button(), {
        
        req(X()$reference.cat)
        
        p_scale_priors_indicator <- p_scale_priors_indicator$p_scale_priors_indicator
        
     if (p_scale_priors_indicator == TRUE) {
                          stan_model_p_scale_priors_model <- stan_model_p_scale_priors$getModel()
                           r$bg_process <<-   callr::r_bg(
                             func = function(stan_model_p_scale_priors,
                                             X,
                                             LCM_prior_sens_ref_lower95,
                                             LCM_prior_sens_ref_upper95,
                                             LCM_prior_spec_ref_lower95,
                                             LCM_prior_spec_ref_upper95,
                                             LCM_prior_SD_sens_ref_sd,
                                             LCM_prior_SD_spec_ref_sd,
                                             LCM_prior_sens_index_lower95,
                                             LCM_prior_sens_index_upper95,
                                             LCM_prior_spec_index_lower95,
                                             LCM_prior_spec_index_upper95,
                                             LCM_prior_SD_sens_index_sd,
                                             LCM_prior_SD_spec_index_sd,
                                             LCM_prior_prev_a,
                                             LCM_prior_prev_b) {
                               
                               rstan::sampling( 
                                 object = stan_model_p_scale_priors, 
                                 data =  list(num_ref = length(unique(as.numeric(as.factor(X$reference.cat)))), 
                                              n_studies = nrow(X), 
                                              Ref = as.numeric(as.factor(X$reference.cat)), 
                                              # "dynamic" priors for ref test (# of priors is dynamic as depends on # of ref. tests)
                                              LCM_prior_sens_ref_lower95 = LCM_prior_sens_ref_lower95, 
                                              LCM_prior_sens_ref_upper95 = LCM_prior_sens_ref_upper95, 
                                              LCM_prior_spec_ref_lower95 = LCM_prior_spec_ref_lower95, 
                                              LCM_prior_spec_ref_upper95 = LCM_prior_spec_ref_upper95, 
                                              # static priors for ref test (between-study SD's, as meta-reg covariate only)
                                              LCM_prior_SD_sens_ref_sd = LCM_prior_SD_sens_ref_sd, 
                                              LCM_prior_SD_spec_ref_sd = LCM_prior_SD_spec_ref_sd, 
                                              # priors for index test
                                              LCM_prior_sens_index_lower95 = LCM_prior_sens_index_lower95, 
                                              LCM_prior_sens_index_upper95 = LCM_prior_sens_index_upper95, 
                                              LCM_prior_spec_index_lower95 = LCM_prior_spec_index_lower95, 
                                              LCM_prior_spec_index_upper95 = LCM_prior_spec_index_upper95, 
                                              LCM_prior_SD_sens_index_sd = LCM_prior_SD_sens_index_sd, 
                                              LCM_prior_SD_spec_index_sd = LCM_prior_SD_spec_index_sd, 
                                              # prev priors (beta dist)
                                              LCM_prior_prev_a = LCM_prior_prev_a, 
                                              LCM_prior_prev_b = LCM_prior_prev_b  
                                 ),
                                 chains = 4,
                                 iter = 2000,
                                 warmup = 200,
                                 control=list(adapt_delta=0.80,
                                              max_treedepth = 10),
                                 seed= 123
                               )
                             }, # end of function 
                             
                             args = list(stan_model_p_scale_priors = stan_model_p_scale_priors_model,
                                         X = X(), 
                                         # "dynamic" priors for ref test (# of priors is dynamic as depends on # of ref. tests)
                                         LCM_prior_sens_ref_lower95 = as.array(priors$LCM_prior_sens_ref_lower95$vec),
                                         LCM_prior_sens_ref_upper95 = as.array(priors$LCM_prior_sens_ref_upper95$vec),
                                         LCM_prior_spec_ref_lower95 = as.array(priors$LCM_prior_spec_ref_lower95$vec),
                                         LCM_prior_spec_ref_upper95 = as.array(priors$LCM_prior_spec_ref_upper95$vec),
                                         # static priors for ref test (between-study SD's, as meta-reg covariate only)
                                         LCM_prior_SD_sens_ref_sd = priors$LCM_prior_SD_sens_ref_sd,
                                         LCM_prior_SD_spec_ref_sd = priors$LCM_prior_SD_spec_ref_sd,
                                         # priors for index test
                                         LCM_prior_sens_index_lower95 = priors$LCM_prior_sens_index_lower95, 
                                         LCM_prior_sens_index_upper95 = priors$LCM_prior_sens_index_upper95, 
                                         LCM_prior_spec_index_lower95 = priors$LCM_prior_spec_index_lower95,
                                         LCM_prior_spec_index_upper95 = priors$LCM_prior_spec_index_upper95,
                                         LCM_prior_SD_sens_index_sd = priors$LCM_prior_SD_sens_index_sd,
                                         LCM_prior_SD_spec_index_sd = priors$LCM_prior_SD_spec_index_sd,
                                         # prev priors (beta dist)
                                         LCM_prior_prev_a = priors$LCM_prior_prev_a, 
                                         LCM_prior_prev_b = priors$LCM_prior_prev_b 
                             ), 
                             stdout = tfile,
                             supervise = TRUE
                           )
        
     }
     else {   # logit-scale priors
                      stan_model_model <- stan_model$getModel()         
                       r$bg_process <<-   callr::r_bg(

                         func = function(stan_model, 
                                         X,
                                         LCM_prior_mean_sens_ref_mu,
                                         LCM_prior_mean_sens_ref_sd,
                                         LCM_prior_mean_spec_ref_mu,
                                         LCM_prior_mean_spec_ref_sd,
                                         LCM_prior_SD_sens_ref_sd,
                                         LCM_prior_SD_spec_ref_sd,
                                         LCM_prior_mean_sens_index_mu,
                                         LCM_prior_mean_sens_index_sd,
                                         LCM_prior_mean_spec_index_mu,
                                         LCM_prior_mean_spec_index_sd,
                                         LCM_prior_SD_sens_index_sd,
                                         LCM_prior_SD_spec_index_sd,
                                         LCM_prior_prev_a,
                                         LCM_prior_prev_b) {
                           
                           rstan::sampling(
                             object = stan_model,
                             data =  list(num_ref = length(unique(as.numeric(as.factor(X$reference.cat)))), 
                                          n_studies = nrow(X),
                                          Ref = as.numeric(as.factor(X$reference.cat)),
                                          # "dynamic" priors for ref test (# of priors is dynamic as depends on # of ref. tests)
                                          LCM_prior_mean_sens_ref_mu = LCM_prior_mean_sens_ref_mu,
                                          LCM_prior_mean_sens_ref_sd = LCM_prior_mean_sens_ref_sd,
                                          LCM_prior_mean_spec_ref_mu = LCM_prior_mean_spec_ref_mu,
                                          LCM_prior_mean_spec_ref_sd = LCM_prior_mean_spec_ref_sd,
                                          # static priors for ref test (between-study SD's, as meta-reg covariate only)
                                          LCM_prior_SD_sens_ref_sd = LCM_prior_SD_sens_ref_sd,
                                          LCM_prior_SD_spec_ref_sd = LCM_prior_SD_spec_ref_sd,
                                          # priors for index test
                                          LCM_prior_mean_sens_index_mu = LCM_prior_mean_sens_index_mu, 
                                          LCM_prior_mean_sens_index_sd = LCM_prior_mean_sens_index_sd, 
                                          LCM_prior_mean_spec_index_mu = LCM_prior_mean_spec_index_mu,
                                          LCM_prior_mean_spec_index_sd = LCM_prior_mean_spec_index_sd,
                                          LCM_prior_SD_sens_index_sd = LCM_prior_SD_sens_index_sd,
                                          LCM_prior_SD_spec_index_sd = LCM_prior_SD_spec_index_sd,
                                          # prev priors (beta dist)
                                          LCM_prior_prev_a = LCM_prior_prev_a, 
                                          LCM_prior_prev_b = LCM_prior_prev_b  
                             ),
                             chains = 4,
                             iter = 2000,
                             warmup = 200,
                             control=list(adapt_delta=0.80,
                                          max_treedepth = 10),
                             seed= 123
                           )
                         }, # end of function 
                         
                         args = list(stan_model = stan_model_model,
                                     X = X(), 
                                     # "dynamic" priors for ref test (# of priors is dynamic as depends on # of ref. tests)
                                     LCM_prior_mean_sens_ref_mu = as.array(priors$LCM_prior_mean_sens_ref_mu$vec),
                                     LCM_prior_mean_sens_ref_sd = as.array(priors$LCM_prior_mean_sens_ref_sd$vec),
                                     LCM_prior_mean_spec_ref_mu = as.array(priors$LCM_prior_mean_spec_ref_mu$vec),
                                     LCM_prior_mean_spec_ref_sd = as.array(priors$LCM_prior_mean_spec_ref_sd$vec),
                                     # static priors for ref test (between-study SD's, as meta-reg covariate only)
                                     LCM_prior_SD_sens_ref_sd = priors$LCM_prior_SD_sens_ref_sd,
                                     LCM_prior_SD_spec_ref_sd = priors$LCM_prior_SD_spec_ref_sd,
                                     # priors for index test
                                     LCM_prior_mean_sens_index_mu = priors$LCM_prior_mean_sens_index_mu, 
                                     LCM_prior_mean_sens_index_sd = priors$LCM_prior_mean_sens_index_sd, 
                                     LCM_prior_mean_spec_index_mu = priors$LCM_prior_mean_spec_index_mu,
                                     LCM_prior_mean_spec_index_sd = priors$LCM_prior_mean_spec_index_sd,
                                     LCM_prior_SD_sens_index_sd = priors$LCM_prior_SD_sens_index_sd,
                                     LCM_prior_SD_spec_index_sd = priors$LCM_prior_SD_spec_index_sd,
                                     # prev priors (beta dist)
                                     LCM_prior_prev_a = priors$LCM_prior_prev_a, 
                                     LCM_prior_prev_b = priors$LCM_prior_prev_b 
                         ), 
                         stdout = tfile,
                         supervise = TRUE
                       )
       
     }
        
        
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
      
      my_list <-             list(
        draws    = reactive({ r$draws  })
      )
      # Run the Garabage Collector to Ensure any excess memory used by stan is freed
      gc()
      return(my_list)
      
    }
  )
}






#  LCM - Run Model  - full model  ---------------------------------------------  -----------------------------------------
LCM_run_model <- function( id, 
                           dataset, 
                           stan_model,
                           stan_model_p_scale_priors,
                           p_scale_priors_indicator,
                           button, 
                           priors, 
                           sampler_options, 
                           SA_indicator, 
                           SA_indicator_local, 
                           LCM_options_indicators) {  
  
  moduleServer(
    id,
    function(input, output, session) {
      
      rstan_options(auto_write = TRUE)
      options(mc.cores = parallel::detectCores())
      
      observeEvent(button(), {
        
            if (is.null(dataset()$reference.cat) == TRUE) {
              
              shinyalert("Warning",
                         "To use meta-analysis without a gold standard, please select a dataset with covariates,
                          and ensure that the reference test data is inputted as a column named 'reference.cat'",
                         type = "error")
            }
        
            else { 

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
            
            }
   
      
    })
  
      tfile <- tempfile( fileext = ".txt" )
      r <- reactiveValues( progress_mtime = -1 )
      
      observeEvent(button(), {
        
                    req(dataset(), dataset()$reference.cat)
                                
                    SA_indicator <- SA_indicator$SA_indicator
                    
                    if ( SA_indicator == TRUE && SA_indicator_local == 1 ) { # note  input$SA_indicator will only be readable if using NS 
                              # corresponding to SA (comes from "SA_indicator_UI" function)
                              X <<- reactive({
                                
                              #  req(dataset()$reference.cat)
                                SA_data <-  dataset() %>% 
                                  dplyr::mutate(author2 = as.numeric(as.factor(author))) %>%
                                  dplyr::filter(!(author2 %in% c(input$triallist))) %>%
                                  dplyr::select(-author2)  %>% 
                                  dplyr::mutate(ref_numeric = as.numeric(as.factor(reference.cat)))
                                SA_data
                              })
                              
                              ref_indexes <<- reactive({
                             #   req(dataset()$reference.cat)
                                ref_indexes_obj  <- c(sort(unique(X()$ref_numeric)))
                                ref_indexes_obj
                              })
                      
                    }
                    
                    if (SA_indicator_local == 0) { # if not doing SA leave X as full dataset (regardless of SA_indicator)
                      
                                X <<- reactive({
                                #  req(dataset()$reference.cat)
                                  
                                  dataset()  %>% 
                                    dplyr::mutate(ref_numeric = as.numeric(as.factor(reference.cat)))
                                })
                                
                                ref_indexes <<- reactive({
                                #  req(dataset()$reference.cat)
                                  ref_indexes_obj  <- c(sort(unique(X()$ref_numeric)))
                                  ref_indexes_obj
                                })
                                
                    }
                    
        
                    X <- X()
                    
                    n_studies <- nrow(X)
                    
                    r1 <- X$TP 
                    r2 <- X$FN 
                    r3 <- X$FP 
                    r4 <- X$TN 
                    
                    study_sizes <- c()
                    for (i in 1:n_studies) {
                      study_sizes[i] <- r1[i] + r2[i] + r3[i] + r4[i]
                    }
                    
                    tables <- matrix(ncol = 4, nrow = n_studies, c(r1,r2,r3,r4))
                    
                    p_scale_priors_indicator <- p_scale_priors_indicator$p_scale_priors_indicator
                    
            
                    if (p_scale_priors_indicator == TRUE) { 
                                stan_model_p_scale_priors_model <- stan_model_p_scale_priors$getModel()   # p-scale priors for Se and Sp ----------------------------------------------------------
                                r$bg_process <<-   callr::r_bg(
                                          func = function(stan_model_p_scale_priors, 
                                                          X,
                                                          study_sizes,
                                                          LCM_conditional_independence_indicator,
                                                          SeR_fixed,
                                                          SpR_fixed,
                                                          SeI_fixed,
                                                          SpI_fixed,
                                                          tables, 
                                                          LCM_prior_sens_ref_lower95,
                                                          LCM_prior_sens_ref_upper95,
                                                          LCM_prior_spec_ref_lower95,
                                                          LCM_prior_spec_ref_upper95,
                                                          LCM_prior_SD_sens_ref_sd,
                                                          LCM_prior_SD_spec_ref_sd,
                                                          LCM_prior_sens_index_lower95,
                                                          LCM_prior_sens_index_upper95,
                                                          LCM_prior_spec_index_lower95,
                                                          LCM_prior_spec_index_upper95,
                                                          LCM_prior_SD_sens_index_sd,
                                                          LCM_prior_SD_spec_index_sd,
                                                          LCM_prior_prev_a,
                                                          LCM_prior_prev_b,
                                                          chains,
                                                          total_iter,
                                                          warmup_iter,
                                                          adapt_delta,
                                                          max_treedepth,
                                                          seed, 
                                                          inits) {
                                            
                                            rstan::sampling(
                                              object = stan_model_p_scale_priors,
                                              data =  list(num_ref = length(unique(as.numeric(as.factor(X$reference.cat)))), 
                                                           n_studies = nrow(X),
                                                           n = study_sizes, 
                                                           r = tables, # 2x2 tables 
                                                           Ref = as.numeric(as.factor(X$reference.cat)), 
                                                           ci = LCM_conditional_independence_indicator, 
                                                           SeR_fixed = SeR_fixed, 
                                                           SpR_fixed = SpR_fixed, 
                                                           SeI_fixed = SeI_fixed, 
                                                           SpI_fixed = SpI_fixed, 
                                                           roc_points_sp = seq(by = 0.01, from = 0, to = 1),
                                                           # dynamic priors for ref test
                                                           LCM_prior_sens_ref_lower95 = LCM_prior_sens_ref_lower95,
                                                           LCM_prior_sens_ref_upper95 = LCM_prior_sens_ref_upper95,
                                                           LCM_prior_spec_ref_lower95 = LCM_prior_spec_ref_lower95,
                                                           LCM_prior_spec_ref_upper95 = LCM_prior_spec_ref_upper95,
                                                           # static priors for ref test (between-study SD's, as meta-reg covariate only)
                                                           LCM_prior_SD_sens_ref_sd = LCM_prior_SD_sens_ref_sd,
                                                           LCM_prior_SD_spec_ref_sd = LCM_prior_SD_spec_ref_sd,
                                                           # priors for index test
                                                           LCM_prior_sens_index_lower95 = LCM_prior_sens_index_lower95,
                                                           LCM_prior_sens_index_upper95 = LCM_prior_sens_index_upper95,
                                                           LCM_prior_spec_index_lower95 = LCM_prior_spec_index_lower95,
                                                           LCM_prior_spec_index_upper95 = LCM_prior_spec_index_upper95,
                                                           LCM_prior_SD_sens_index_sd   = LCM_prior_SD_sens_index_sd,
                                                           LCM_prior_SD_spec_index_sd   = LCM_prior_SD_spec_index_sd,
                                                           # prev priors (beta dist)
                                                           LCM_prior_prev_a = LCM_prior_prev_a, 
                                                           LCM_prior_prev_b = LCM_prior_prev_b 
                                              ), 
                                              chains = chains,
                                              iter = total_iter,
                                              warmup = warmup_iter,
                                              control=list(adapt_delta= adapt_delta, 
                                                           max_treedepth = max_treedepth),
                                              seed= seed,
                                              init = inits
                                              )
                                          },
                                          args = list(stan_model_p_scale_priors = stan_model_p_scale_priors_model, 
                                                      X = X, 
                                                      study_sizes = study_sizes,
                                                      LCM_conditional_independence_indicator = LCM_options_indicators$LCM_conditional_independence_indicator, 
                                                      SeR_fixed = LCM_options_indicators$LCM_SeR_fixed_indicator, 
                                                      SpR_fixed = LCM_options_indicators$LCM_SpR_fixed_indicator, 
                                                      SeI_fixed = LCM_options_indicators$LCM_SeI_fixed_indicator, 
                                                      SpI_fixed = LCM_options_indicators$LCM_SpI_fixed_indicator, 
                                                      tables = tables, 
                                                      # dynamic priors for ref test
                                                      LCM_prior_sens_ref_lower95 = as.array(priors$LCM_prior_sens_ref_lower95$vec[ref_indexes()]),
                                                      LCM_prior_sens_ref_upper95 = as.array(priors$LCM_prior_sens_ref_upper95$vec[ref_indexes()]),
                                                      LCM_prior_spec_ref_lower95 = as.array(priors$LCM_prior_spec_ref_lower95$vec[ref_indexes()]),
                                                      LCM_prior_spec_ref_upper95 = as.array(priors$LCM_prior_spec_ref_upper95$vec[ref_indexes()]),
                                                      # static priors for ref test (between-study SD's, as meta-reg covariate only)
                                                      LCM_prior_SD_sens_ref_sd = priors$LCM_prior_SD_sens_ref_sd,
                                                      LCM_prior_SD_spec_ref_sd = priors$LCM_prior_SD_spec_ref_sd,
                                                      # priors for index test
                                                      LCM_prior_sens_index_lower95 = priors$LCM_prior_sens_index_lower95,
                                                      LCM_prior_sens_index_upper95 = priors$LCM_prior_sens_index_upper95,
                                                      LCM_prior_spec_index_lower95 = priors$LCM_prior_spec_index_lower95,
                                                      LCM_prior_spec_index_upper95 = priors$LCM_prior_spec_index_upper95,
                                                      LCM_prior_SD_sens_index_sd   = priors$LCM_prior_SD_sens_index_sd,
                                                      LCM_prior_SD_spec_index_sd   = priors$LCM_prior_SD_spec_index_sd,
                                                      # prev priors (beta dist)
                                                      LCM_prior_prev_a = priors$LCM_prior_prev_a, 
                                                      LCM_prior_prev_b = priors$LCM_prior_prev_b,
                                                      chains = sampler_options$MA_num_chains,
                                                      total_iter = sampler_options$MA_total_iter,
                                                      warmup_iter = sampler_options$MA_warmup_iter,
                                                      adapt_delta = sampler_options$MA_adapt_delta, 
                                                      max_treedepth = sampler_options$MA_max_treedepth,
                                                      seed= sampler_options$MA_seed, 
                                                      inits = rep(list(list(index_logit_mu = c(LCM_options_indicators$LCM_index_logit_mu_se, 
                                                                                          LCM_options_indicators$LCM_index_logit_mu_sp))), 
                                                                                          times = sampler_options$MA_num_chains  )
                                          ), 
                                          stdout = tfile,
                                          supervise = TRUE
                                        )
                                
                    }
                    else {  # logit-scale priors for Se and Sp  ---------------------------------------------------------- 
                      stan_model_model <- stan_model$getModel()
                      r$bg_process <<-   callr::r_bg(
                                          func = function(stan_model, 
                                                          X,
                                                          study_sizes,
                                                          LCM_conditional_independence_indicator,
                                                          SeR_fixed,
                                                          SpR_fixed,
                                                          SeI_fixed,
                                                          SpI_fixed,
                                                          tables, 
                                                          LCM_prior_mean_sens_ref_mu,
                                                          LCM_prior_mean_sens_ref_sd,
                                                          LCM_prior_mean_spec_ref_mu,
                                                          LCM_prior_mean_spec_ref_sd,
                                                          LCM_prior_SD_sens_ref_sd,
                                                          LCM_prior_SD_spec_ref_sd,
                                                          LCM_prior_mean_sens_index_mu,
                                                          LCM_prior_mean_sens_index_sd,
                                                          LCM_prior_mean_spec_index_mu,
                                                          LCM_prior_mean_spec_index_sd,
                                                          LCM_prior_SD_sens_index_sd,
                                                          LCM_prior_SD_spec_index_sd,
                                                          LCM_prior_prev_a,
                                                          LCM_prior_prev_b,
                                                          chains,
                                                          total_iter,
                                                          warmup_iter,
                                                          adapt_delta,
                                                          max_treedepth,
                                                          seed, 
                                                          inits) {
                                            
                                            rstan::sampling(
                                              object = stan_model,
                                              data =  list(num_ref = length(unique(as.numeric(as.factor(X$reference.cat)))), 
                                                           n_studies = nrow(X),
                                                           n = study_sizes, 
                                                           r = tables, # 2x2 tables 
                                                           Ref = as.numeric(as.factor(X$reference.cat)), 
                                                           ci = LCM_conditional_independence_indicator, 
                                                           SeR_fixed = SeR_fixed, 
                                                           SpR_fixed = SpR_fixed, 
                                                           SeI_fixed = SeI_fixed, 
                                                           SpI_fixed = SpI_fixed, 
                                                           roc_points_sp = seq(by = 0.01, from = 0, to = 1),
                                                           # dynamic priors for ref test
                                                           LCM_prior_mean_sens_ref_mu = LCM_prior_mean_sens_ref_mu,
                                                           LCM_prior_mean_sens_ref_sd = LCM_prior_mean_sens_ref_sd,
                                                           LCM_prior_mean_spec_ref_mu = LCM_prior_mean_spec_ref_mu,
                                                           LCM_prior_mean_spec_ref_sd = LCM_prior_mean_spec_ref_sd,
                                                           # static priors for ref test (between-study SD's, as meta-reg covariate only)
                                                           LCM_prior_SD_sens_ref_sd = LCM_prior_SD_sens_ref_sd,
                                                           LCM_prior_SD_spec_ref_sd = LCM_prior_SD_spec_ref_sd,
                                                           # priors for index test
                                                           LCM_prior_mean_sens_index_mu = LCM_prior_mean_sens_index_mu,
                                                           LCM_prior_mean_sens_index_sd = LCM_prior_mean_sens_index_sd,
                                                           LCM_prior_mean_spec_index_mu = LCM_prior_mean_spec_index_mu,
                                                           LCM_prior_mean_spec_index_sd = LCM_prior_mean_spec_index_sd,
                                                           LCM_prior_SD_sens_index_sd   = LCM_prior_SD_sens_index_sd,
                                                           LCM_prior_SD_spec_index_sd   = LCM_prior_SD_spec_index_sd,
                                                           # prev priors (beta dist)
                                                           LCM_prior_prev_a = LCM_prior_prev_a, 
                                                           LCM_prior_prev_b = LCM_prior_prev_b 
                                              ), 
                                              chains = chains,
                                              iter = total_iter,
                                              warmup = warmup_iter,
                                              control=list(adapt_delta= adapt_delta, 
                                                           max_treedepth = max_treedepth),
                                              seed= seed, 
                                              init = inits
                                         
                                            )
                                          },
                                          
                                          args = list(stan_model = stan_model_model, 
                                                      X = X, 
                                                      study_sizes = study_sizes,
                                                      LCM_conditional_independence_indicator = LCM_options_indicators$LCM_conditional_independence_indicator, 
                                                      SeR_fixed = LCM_options_indicators$LCM_SeR_fixed_indicator, 
                                                      SpR_fixed = LCM_options_indicators$LCM_SpR_fixed_indicator, 
                                                      SeI_fixed = LCM_options_indicators$LCM_SeI_fixed_indicator, 
                                                      SpI_fixed = LCM_options_indicators$LCM_SpI_fixed_indicator, 
                                                      tables = tables, 
                                                      # dynamic priors for ref test
                                                      LCM_prior_mean_sens_ref_mu = as.array(priors$LCM_prior_mean_sens_ref_mu$vec[ref_indexes()]),
                                                      LCM_prior_mean_sens_ref_sd = as.array(priors$LCM_prior_mean_sens_ref_sd$vec[ref_indexes()]),
                                                      LCM_prior_mean_spec_ref_mu = as.array(priors$LCM_prior_mean_spec_ref_mu$vec[ref_indexes()]),
                                                      LCM_prior_mean_spec_ref_sd = as.array(priors$LCM_prior_mean_spec_ref_sd$vec[ref_indexes()]),
                                                      # static priors for ref test (between-study SD's, as meta-reg covariate only)
                                                      LCM_prior_SD_sens_ref_sd = priors$LCM_prior_SD_sens_ref_sd,
                                                      LCM_prior_SD_spec_ref_sd = priors$LCM_prior_SD_spec_ref_sd,
                                                      # priors for index test
                                                      LCM_prior_mean_sens_index_mu = priors$LCM_prior_mean_sens_index_mu,
                                                      LCM_prior_mean_sens_index_sd = priors$LCM_prior_mean_sens_index_sd,
                                                      LCM_prior_mean_spec_index_mu = priors$LCM_prior_mean_spec_index_mu,
                                                      LCM_prior_mean_spec_index_sd = priors$LCM_prior_mean_spec_index_sd,
                                                      LCM_prior_SD_sens_index_sd   = priors$LCM_prior_SD_sens_index_sd,
                                                      LCM_prior_SD_spec_index_sd   = priors$LCM_prior_SD_spec_index_sd,
                                                      # prev priors (beta dist)
                                                      LCM_prior_prev_a = priors$LCM_prior_prev_a, 
                                                      LCM_prior_prev_b = priors$LCM_prior_prev_b,
                                                      chains = sampler_options$MA_num_chains,
                                                      total_iter = sampler_options$MA_total_iter,
                                                      warmup_iter = sampler_options$MA_warmup_iter,
                                                      adapt_delta = sampler_options$MA_adapt_delta, 
                                                      max_treedepth = sampler_options$MA_max_treedepth,
                                                      seed= sampler_options$MA_seed, 
                                                      inits = rep(list(list(index_logit_mu = c(LCM_options_indicators$LCM_index_logit_mu_se, 
                                                                                          LCM_options_indicators$LCM_index_logit_mu_sp))), 
                                                                                          times = sampler_options$MA_num_chains  )

                                                                            
                                          ), 
                                          stdout = tfile,
                                          supervise = TRUE
                                        )
                      
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
        
      })
      
      # Output list -------------------------------------------------------------
      my_list <-             list(
                                  draws    = reactive({ r$draws  })
      )
      
      # Run the Garabage Collector to Ensure any excess memory used by stan is freed
      gc()
      return(my_list)
      
      
      
    }
  )
}













