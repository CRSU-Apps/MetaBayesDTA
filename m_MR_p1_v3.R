


# renderUI server function for p-sale priors checkbox ----------------------------------------------------------------------------------
MR_p_scale_priors_indicator_checkbox_UI <- function(id) {
  ns <- NS(id) 
  uiOutput(ns("p_scale_priors_indicator_ui"))
}




# p-scale priors indicator / checkbox - renderUI ---------------------------------------------------------------------------------------
MR_p_scale_priors_indicator_checkbox_renderUI <-  function(id,
                                                           cts_cov_indicator) {
  
  moduleServer(id,
               function(input, output, session) {

                 
                 output$p_scale_priors_indicator_ui <- renderUI({
                   
                           ns <- session$ns
                           
                           cts_cov_indicator  <- cts_cov_indicator$cts_cov_indicator
                           
                           if ( cts_cov_indicator == 0 ) { 
                             
                             awesomeCheckbox(inputId = ns("p_scale_priors_indicator"), 
                                             "Specify priors for sensitivities and specificities directly on the probability scale?", 
                                             TRUE)
                           }
                           else {  }
                 })
                 
                 outputOptions(output, 'p_scale_priors_indicator_ui', suspendWhenHidden = FALSE)
                 
                 
               }
  )
}


# Indicator for whether the selected covariate is continuous ("cts") or categorical/discrete ("cat") -------------------------------------
MR_cts_cov_indicator_out <-  function(id,
                                      data) {
  
  moduleServer(id,
               function(input, output, session) {
                 
                 vals <- reactiveValues(cts_cov_indicator = 0)
                 
                 observe({
                       
                   req(data(), input$covcheck_model)
                   
                       C <- reactive({ 
                       #  print(ncol(data()) )
                         ncol(data())  
                       })
                       
                       Names <- reactive({ 
                         colnames(data()) 
                       })
                       
                       j <- reactive({
                       #  print(j)
                         if (C() > 8 & Names()[7] != "rob_PS" & (ncol(dplyr::select(data(), contains("rob_")))  == 0) ) { # if Cov dataset type selected
                           j <<- 6 
                         } 
                         if (C() > 8 & Names()[7] != "rob_PS" & (ncol(dplyr::select(data(), contains("rob_"))) > 0) ) { # QA_Cov dataset type selected
                           j <<- 13  
                         }
                         if (C() == 8) { # standard dataset (so only covariates are year.cts and prevalence.cts)
                           j <<- 6
                         }
                         if (C() > 8 & Names()[7] == "rob_PS" & (ncol(dplyr::select(data(), contains("rob_"))) > 0) ) { # QA dataset type selected
                           j <<- 13
                         }
                      #   print(j)
                      #   print(data())
                       #  print(C())
                         
                         j
                       })
                       
                       cov_index <- reactive({   
                         as.integer(as.double(input$covcheck_model)) - 1  
                       })
                       
                       
                   #   if (ncol(cbind(dplyr::select(data(), contains(".cts")),dplyr::select(data(), contains(".cat")))) > 2) { # if data frame has any covariates (besides year.cts and prev.cts)
                      #  print(data())
                           if ( (str_sub(colnames(data())[ j() + cov_index() ], start = -3) != 'cts') ) {
                             vals$cts_cov_indicator <<-  0 
                           }
                           else {
                             vals$cts_cov_indicator <<-  1 
                           }
                   #    }
                      # else { # no covariates in dataset  (besides year.cts and prev.cts)
                      #   print(data())
                      #     vals$cts_cov_indicator <<- 0
                      #   }
                   
                   #   print(str_sub(colnames(data())[ j() + cov_index() ], start = -3))
                         
                 })

                 return(vals)
                 
                 
               }
  )
}


# p-scale priors indicator / checkbox - renderUI ---------------------------------------------------------------------------------------
MR_p_scale_priors_indicator_checkbox_out <-  function(id,
                                                      cts_cov_indicator) {
  
  moduleServer(id,
               function(input, output, session) {
                 
            vals <- reactiveValues()
                 
            observe({

              cts_cov_indicator <- cts_cov_indicator$cts_cov_indicator
              
                      if ( cts_cov_indicator == 0 ) { 
            
                            vals$p_scale_priors_indicator <- input$p_scale_priors_indicator
                      
                      }
                      else {   }
      
            })
    
       return(vals)
    
    }
  )
}




# Model diagnostics tab  - UI ("master") module  ------------------------------  ----------------------------------
MR_model_diagnostics_tab_UI <- function(id) {
  ns <- NS(id)  
  tagList( 
    h3("Sampler diagnostics"), 
    sampler_diagnostics_table_UI(id = "MR_model_id"),
    br(),
    h3("R-hat statistics"), 
    MR_rhat_table_UI(id = "MR_model_id"), 
    br(), 
    h3("Posterior density plots"),
    dropdownButton(
      model_posterior_density_plots_settings_UI(id = "MR_model_id"),
      circle = TRUE,
      status = "danger",
      icon = icon("gear"), 
      width = "300px",
      tooltip = tooltipOptions(title = "Click to customise plot")
    ),
    model_posterior_density_plots_UI(id = "MR_model_id"),
    br(),
    h3("Trace plots"),
    dropdownButton(
      model_trace_plots_settings_UI(id = "MR_model_id"),
      circle = TRUE, 
      status = "danger",
      icon = icon("gear"), 
      width = "300px",
      tooltip = tooltipOptions(title = "Click to customise plot")
    ),
    model_trace_plots_UI(id = "MR_model_id")
  )
}




# MR (cts only) - UI function to select covariate  ---------------------------------------------------------------
MR_select_covariate_UI <- function(id) {
  ns <- NS(id)  
  uiOutput(ns("covariate_model"))
}




#  MR (cts only) -  UI function for centering value + covariate value to calc. pooled estimates  ----------------- 
MR_pick_cts_output_value_UI <- function(id) {
  ns <- NS(id)  
  tagList(
                     uiOutput(ns("covariate_model_centering_ui")),
                     br(), 
                     uiOutput(ns("covariate_model_summary_cov_value_ui"))
  )
}




# covariate to use for MR model   -------------------------------------------------------------------------------
MR_covariate_model_server <- function(id, 
                                      data,
                                      cts_cov_indicator) {  
  moduleServer(
    id,
    function(input, output, session) {
      
      X <- reactive({ data() })
      
      C <- reactive({ ncol(data())  })
      Names <- reactive({ colnames(data()) })
      
      choicesCov_obj <- reactive({
        
                    initial <- c("None")
                    
                      # if (( C() > 8 & Names()[7] != "rob_PS") |  (C() > 15 & Names()[13] == "ac_RS")) { # Cov or QA_Cov dataset types
                      #   
                      #         if (C() > 8 & Names()[7] != "rob_PS") { # Cov dataset type 
                      #           if (C() == 9) {
                      #             covariates <- colnames(X()[7]) # 1 covariate 
                      #           }
                      #           else {
                      #             covariates <- colnames(X()[,7:C()])
                      #           }
                      #         }
                      #         if (C() > 15 & Names()[13] == "ac_RS") {
                      #           if (C() == 16) {
                      #             covariates <- colnames(X()[14])
                      #           }
                      #           else {
                      #             covariates <- colnames(X()[,14:C()])
                      #           }
                      #         }
                      #   
                      #       combined <- c(initial, covariates)
                      #       number <- 1:length(combined)
                      #       choicesCov <- setNames(number, combined)
                      #       choicesCov
                      #       
                      # }
                      # 
                    

                      if (C() > 8 & Names()[7] != "rob_PS" & (ncol(dplyr::select(data(), contains("rob_")))  == 0) ) { # if Cov dataset type selected
                        covariates <- colnames(X()[,7:C()])
                      }
                      if (C() > 8 & Names()[7] != "rob_PS" & (ncol(dplyr::select(data(), contains("rob_"))) > 0) ) { # QA_Cov dataset type selected
                        covariates <- colnames(X()[,14:C()])
                      }
                      if (C() == 8) { # standard dataset (so only covariates are year.cts and prevalence.cts)
                        covariates <- colnames(X()[,7:C()])
                      }
                      if (C() > 8 & Names()[7] == "rob_PS" & (ncol(dplyr::select(data(), contains("rob_"))) > 0) ) { # QA dataset type selected
                        covariates <- colnames(X()[,14:C()])
                      }
                    
                    combined <- c(initial, covariates)
                    number <- 1:length(combined)
                    choicesCov <- setNames(number, combined)
                    choicesCov

                    

                    
      })
      

      output$covariate_model <- renderUI({ 
        
                    ns <- session$ns
                    
                    selectInput(inputId = ns("covcheck_model"), 
                                label= "Please select covariate to use for model", 
                                choices = choicesCov_obj(),
                                selected = 2)

      })
      
      default_value_centering <- reactive({ 
        
      #  if (C() > 8 & Names()[7] != "rob_PS") {   j <<- 6  } else { j <<- 13  }
        


          if (C() > 8 & Names()[7] != "rob_PS" & (ncol(dplyr::select(data(), contains("rob_")))  == 0) ) { # if Cov dataset type selected
            j <<- 6 
          } 
          if (C() > 8 & Names()[7] != "rob_PS" & (ncol(dplyr::select(data(), contains("rob_"))) > 0) ) { # QA_Cov dataset type selected
            j <<- 13  
          }
          if (C() == 8) { # standard dataset (so only covariates are year.cts and prevalence.cts)
            j <<- 6
          }
          if (C() > 8 & Names()[7] == "rob_PS" & (ncol(dplyr::select(data(), contains("rob_"))) > 0) ) { # QA dataset type selected
            j <<- 13
          }

        
        centered_covariate_default <- round(mean( X()[, j +   as.integer(as.double(input$covcheck_model)) - 1] ) ,3) 
        # default value is the mean of the covariate
        return(centered_covariate_default)
        
      })
      
      min_value_centering <- reactive({ 
        
        if (C() > 8 & Names()[7] != "rob_PS" & (ncol(dplyr::select(data(), contains("rob_")))  == 0) ) { # if Cov dataset type selected
          j <<- 6 
        } 
        if (C() > 8 & Names()[7] != "rob_PS" & (ncol(dplyr::select(data(), contains("rob_"))) > 0) ) { # QA_Cov dataset type selected
          j <<- 13  
        }
        if (C() == 8) { # standard dataset (so only covariates are year.cts and prevalence.cts)
          j <<- 6
        }
        if (C() > 8 & Names()[7] == "rob_PS" & (ncol(dplyr::select(data(), contains("rob_"))) > 0) ) { # QA dataset type selected
          j <<- 13
        }
        

        
        centered_covariate_default <- min( X()[, j +   as.integer(as.double(input$covcheck_model)) - 1 ] )  
        # default value is the mean of the covariate
        return(centered_covariate_default)
        
      })
      
      max_value_centering <- reactive({ 
        
        if (C() > 8 & Names()[7] != "rob_PS" & (ncol(dplyr::select(data(), contains("rob_")))  == 0) ) { # if Cov dataset type selected
          j <<- 6 
        } 
        if (C() > 8 & Names()[7] != "rob_PS" & (ncol(dplyr::select(data(), contains("rob_"))) > 0) ) { # QA_Cov dataset type selected
          j <<- 13  
        }
        if (C() == 8) { # standard dataset (so only covariates are year.cts and prevalence.cts)
          j <<- 6
        }
        if (C() > 8 & Names()[7] == "rob_PS" & (ncol(dplyr::select(data(), contains("rob_"))) > 0) ) { # QA dataset type selected
          j <<- 13
        }
        

        centered_covariate_default <- max( X()[, j +   as.integer(as.double(input$covcheck_model)) - 1] )  
        # default value is the mean of the covariate
        return(centered_covariate_default)
        
      })
      
      
      output$covariate_model_centering_ui <- renderUI({ 
        
                    ns <- session$ns
                    
                    cts_cov_indicator <-  cts_cov_indicator$cts_cov_indicator
                    
                    if (cts_cov_indicator == 1)  { # if continuous covariate --------------------------- 
                      
                    numericInput(inputId = ns("centered_value_input"), 
                                 label= "Please select covariate value to use for centering
                                        (default is the mean of the observed values of the selected covariate)", 
                                 value = default_value_centering(), 
                                 min = min_value_centering(), 
                                 max = max_value_centering())
                    } else { }
        
      })
      
      
      output$covariate_model_summary_cov_value_ui <- renderUI({ 
        
                    ns <- session$ns
                    
                    # default value to calc. pooled estimates is the same as the centered value 
                    # (i.e. the mean, which mean zero - which corresponds to the mean if using
                    # default centering value, or if not then 0 + X, where X is the value the user inputs)
                    
                    cts_cov_indicator <-  cts_cov_indicator$cts_cov_indicator
                    
                    if (cts_cov_indicator == 1)  { # if continuous covariate ---------------------------
                      
                    numericInput(inputId = ns("MRcts_input_cov"), 
                                 label = h4("Covariate value to calculate pooled accuracy estimates at"), 
                                 value = default_value_centering(), 
                                 min = min_value_centering(), 
                                 max = max_value_centering())
                    } else { }
      })
      
      outputOptions(output, 'covariate_model', suspendWhenHidden=FALSE)
      outputOptions(output, 'covariate_model_centering_ui', suspendWhenHidden=FALSE)
      outputOptions(output, 'covariate_model_summary_cov_value_ui', suspendWhenHidden=FALSE)

    }
  )
}


# prior distribution options  -------------------------------------------------  -------------------------------------------------------------

# UI function to input prior distribution options 
MR_priors_options_UI <- function(id) {
  ns <- NS(id)                  
  uiOutput(ns("priors_options"))
}

# server-side function to input prior dist. options 
MR_priors_options_server <- function(id, 
                                     cts_cov_indicator) {  
  
  moduleServer(
    id,
    function(input, output, session) {


      output$priors_options <- renderUI({
        
        ns <- session$ns
        cts_cov_indicator <- cts_cov_indicator$cts_cov_indicator

        
         req(input$covcheck_model, cancelOutput = TRUE)
        
        if (cts_cov_indicator == 0) {
          div(id = ns("priors_options_div"),
              if (input$p_scale_priors_indicator == FALSE) { 
                tagList(
                    h4("Prior Distributions (For categorical / discrete covariate):"),
                    numericInput(inputId = ns("MRcat_prior_mean_sens_mu"), label=h5("Prior mean of pooled sensitivities - mean"), value = 0),
                    numericInput(inputId = ns("MRcat_prior_mean_sens_sd"), label=h5("Prior mean of pooled sensitivities - SD"), value = 1.5),
                    numericInput(inputId = ns("MRcat_prior_mean_spec_mu"), label=h5("Prior mean of pooled specificities - mean"), value = 0),
                    numericInput(inputId = ns("MRcat_prior_mean_spec_sd"), label=h5("Prior mean of pooled specificities - SD"), value = 1.5),
                    numericInput(inputId = ns("MRcat_prior_SD_sens_sd"), label=h5("Prior for between-study SD of logit(sensitivity) - SD"), value = 1),
                    numericInput(inputId = ns("MRcat_prior_SD_spec_sd"), label=h5("Prior for between-study SD of logit(specificity) - SD"), value = 1)
                )
              }
              else { 
                tagList(
                  h4("Prior Distributions (For categorical / discrete covariate):"),
                  h5("Sensitivity - 95% credible interval:"),
                  numericInputRow(inputId =  ns("MRcat_prior_sens_lower95"), label=h5("Lower interval (2.5th percentile)"), value = 0.05, min = 0, max = 1),
                  numericInputRow(inputId =  ns("MRcat_prior_sens_upper95"), label=h5("Upper interval (97.5th percentile)"), value = 0.95, min = 0, max = 1),
                  h5("Specificity - 95% credible interval:"),
                  numericInputRow(inputId =  ns("MRcat_prior_spec_lower95"), label=h5("Lower interval (2.5th percentile)"), value = 0.05, min = 0, max = 1),
                  numericInputRow(inputId =  ns("MRcat_prior_spec_upper95"), label=h5("Upper interval (97.5th percentile)"), value = 0.95, min = 0, max = 1),
                  h5("Standard deviation (SD) priors:"),
                  numericInput(inputId = ns("MRcat_prior_SD_sens_sd"), label=h5("Prior for between-study SD of logit(sensitivity) - SD"), value = 1),
                  numericInput(inputId = ns("MRcat_prior_SD_spec_sd"), label=h5("Prior for between-study SD of logit(specificity) - SD"), value = 1)
                )
              }
      )
        }
        else {
          div(id = ns("priors_options_div"),
              h4("Prior Distributions (For continuous covariate):"),
              numericInput(inputId = ns("MRcts_prior_mean_sens_mu"), label=h5("Prior mean of pooled logit(sensitivity) intercept - mean"), value = 0),
              numericInput(inputId = ns("MRcts_prior_mean_sens_sd"), label=h5("Prior mean of pooled logit(sensitivity) intercept - SD"), value = 1.5),
              numericInput(inputId = ns("MRcts_prior_mean_spec_mu"), label=h5("Prior mean of pooled logit(specificity) intercept - mean"), value = 0),
              numericInput(inputId = ns("MRcts_prior_mean_spec_sd"), label=h5("Prior mean of pooled logit(specificity) intercept - SD"), value = 1.5),
              numericInput(inputId = ns("MRcts_prior_SD_sens_sd"), label=h5("Prior for between-study SD of logit(sensitivity) - SD"), value = 1),
              numericInput(inputId = ns("MRcts_prior_SD_spec_sd"), label=h5("Prior for between-study SD of logit(specificity) - SD"), value = 1),
              numericInput(inputId = ns("MRcts_prior_coeff_sens_mean"), label=h5("Prior for coefficient of logit(sensitivity) - mean"), value = 0),
              numericInput(inputId = ns("MRcts_prior_coeff_sens_sd"), label=h5("Prior for coefficient of logit(sensitivity) - SD"), value = 1),
              numericInput(inputId = ns("MRcts_prior_coeff_spec_mean"), label=h5("Prior for coefficient of logit(specificity) - mean"), value = 0),
              numericInput(inputId = ns("MRcts_prior_coeff_spec_sd"), label=h5("Prior for coefficient of logit(specificity) - SD"), value = 1)
              )
        }
        
      })
      
      observeEvent(input$reset,{
        shinyjs::reset("priors_options_div")
      })
      
      
      outputOptions(output, "priors_options", suspendWhenHidden = FALSE)
      
    }
  )
}


# MR - Plot of prior distributions  -------------------------------------------------  ----------------------------------------------------------------------

# server function
MR_model_priors_plot_server <- function(id,
                                        draws_PO, 
                                        cts_cov_indicator) { 
  
  moduleServer(
    id,
    function(input, output, session) {
      
      priors_plot_obj <-  reactive({ # can just refer to the plot name ("model_priors_plot") without using "ns" here in the server function. 

                        validate(
                          need(!(is.null(draws_PO())), 
                               "Please run prior model to display plot")
                        )
        
                          req(draws_PO(), input$covcheck_model, cancelOutput = TRUE)
                          
                          params <- rstan::extract(draws_PO())

                          n_samps <- (2000 - 200)*4
                          
                          cts_cov_indicator <-  cts_cov_indicator$cts_cov_indicator
                          
                          if (cts_cov_indicator == 0)  { # if categorical / discrete meta-reg ------
                              
                                    se <- params$Se[, 1] # chose 1st group (since priors are the same for each subgroup / discrete covariate)
                                    sp <- params$Sp[, 1] # chose 1st group (since priors are the same for each subgroup / discrete covariate)
                                    corr <- params$Omega[, 1, 2]
                                    sd_se <- params$sigma[, 1]
                                    sd_sp <- params$sigma[, 2]
                                    
                                    data <- tibble(
                                      Samples = c(se, sp, corr, sd_se, sd_sp), 
                                      Parameter = c(rep("Sensitivity (for each level)", n_samps), 
                                                    rep("Specificity (for each level)", n_samps), 
                                                    rep("Between-study correlation", n_samps),
                                                    rep("Between-study SD for logit(sensitivity)", n_samps), 
                                                    rep("Between-study SD for logit(specificity)", n_samps)))
                                    
                                    g <-    ggplot(data = data, aes(x=Samples)) + 
                                      geom_density(alpha = 0.50) + 
                                      theme_bw() +
                                      theme(legend.position = "none") +
                                      facet_wrap( ~ Parameter, scales = "free") + 
                                      xlab(" ") + 
                                      ylab(" ")
                                    
                                    g
                              
                            } 
                          else { # if cts covariate --------
                                    se <- params$Se_at_cov_input
                                    sp <- params$Sp_at_cov_input
                                    corr <- params$Omega[, 1, 2]
                                    sd_se <- params$sigma[, 1]
                                    sd_sp <- params$sigma[, 2]
                                    logit_intercept_se <- params$mu[, 1]
                                    logit_intercept_sp <- params$mu[, 2]
                                    logit_coeff_se <- params$coeff[, 1]
                                    logit_coeff_sp <- params$coeff[, 2]
                                    
                                    data <- tibble(
                                      Samples = c(logit_intercept_se, logit_intercept_sp, 
                                                  logit_coeff_se, logit_coeff_sp, 
                                                  se, sp, corr, sd_se, sd_sp), 
                                      Parameter = c(rep("Pooled logit(sensitivity) intercept", n_samps), 
                                                    rep("Pooled logit(specificity) intercept", n_samps), 
                                                    rep("Pooled logit(sensitivity) coefficient", n_samps), 
                                                    rep("Pooled logit(specificity) coefficient", n_samps), 
                                                    rep("Sensitivity at inputted value of covariate", n_samps),
                                                    rep("Specificity  at inputted value of covariate", n_samps),
                                                    rep("Between-study correlation", n_samps),
                                                    rep("Between-study SD for logit(sensitivity)", n_samps), 
                                                    rep("Between-study SD for logit(specificity)", n_samps)))
                                    
                                    g <-    ggplot(data = data, aes(x=Samples)) + 
                                      geom_density(alpha = 0.50) + 
                                      theme_bw() +
                                      theme(legend.position = "none") +
                                      facet_wrap( ~ Parameter, scales = "free") + 
                                      xlab(" ") + 
                                      ylab(" ")
                                    
                                    g
            
          }

      })
      
      
      # output plot 
      observe({
        output$model_priors_plot <- renderPlot({

          
          validate(
            need(!(is.null(draws_PO())), "Please run prior model to display plot")
          )
          
          req(draws_PO(), input$covcheck_model, cancelOutput = TRUE)
          
          
          priors_plot_obj()
          
        }, 
        height = input$model_priors_plot_dimension_slider_height, 
        width  = input$model_priors_plot_dimension_slider_width)
      })
      
      
      
      # Download ggplot object 
      output$download_priors_plot <- downloadHandler(
        filename = function(){
          paste("priors_plot.png")
        },
        content = function(file) { 
          {ggsave(file, 
                  priors_plot_obj(),  
                  width = input$priors_plot_dl_width,
                  height = input$priors_plot_dl_height,
                  dpi = input$priors_plot_dl_dpi,
                  units = "in"
          )}
        } 
      )
      
    }
  )
}



# MR -  Table of prior distributions  -----------------------------------------------  ----------------------------------------------------------

MR_model_priors_table_UI <- function(id) {
  ns <- NS(id)   
  tagList(
    tableOutput(outputId =  ns("model_priors_table")) ,
  p("NOTE: The default priors are:"),  
  p("For continuous meta-regression:"),
  p("For logit sensitivity and specificity intercepts - normal distribution with mean 0 and SD of 1.5, equivalent to a 95% prior interval of (-2.97, 2.9) "),
  p("For logit sensitivity and specificity coefficients - normal distribution with mean 0 and SD of 1, equivalent to a 95% prior interval of (-1.96, 1.96) "),
  p("For between-study SD's of logit sensitivities and specificities - truncated (at 0) normal distribution with mean 0 and SD of 1, equivalent to a 95% prior interval of (0.03, 2.25) "),
  p("For between-study correlation between study-specific logit sensitivities and specificities - LKJ(2) prior, equivalent to 95% prior interval of (-0.8, 0.8)"),
  p("For categorical meta-regression:"),
  p("For logit sensitivities and specificities - normal distribution with mean 0 and SD of 1, equivalent to a 95% prior interval of (0.05, 0.95) on the probability scale"),
  p("For between-study SD's of logit sensitivities and specificities - truncated (at 0) normal distribution with mean 0 and SD of 1, equivalent to a 95% prior interval of (0.03, 2.25) "),
  p("For between-study correlation between study-specific logit sensitivities and specificities - LKJ(2) prior, equivalent to 95% prior interval of (-0.8, 0.8)")
  )
}



MR_model_priors_table_server <- function(id, 
                                         draws_PO, 
                                         cts_cov_indicator) {  
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$model_priors_table <- renderTable({
      
                      validate(
                        need(!(is.null(draws_PO())),
                             "Please run prior model to display table")
                      )
        
                      req(draws_PO(), cancelOutput = TRUE)
                      
                      mod <- draws_PO()

                      cov_index <-  as.integer(as.double(input$covcheck_model)) - 1

                      
                      validate(
                        need(cov_index != 0, "Please select a covariate")
                      )
                      
                      num_levels <- length(rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se"))$summary[,5])
                   
                      cts_cov_indicator <-  cts_cov_indicator$cts_cov_indicator
                      
                        # MR - Table of prior distributions - cat/discrete covariate --------------------------------------------------------
                        
                           if (cts_cov_indicator == 0)  { 
                                  
                                  
                                  Sens <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se"))$summary
                                  Spec <-  rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Sp"))$summary
                                  correlation <-  rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Omega"))$summary
                                  between_study_sd <-  rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sigma"))$summary
                                
                                  
                                  Z = data.frame(estimate = c(Sens[,5][1], Spec[,5][1],
                                                              1 - Spec[,5][1], 
                                                              correlation[,5][3], 
                                                              between_study_sd[,5][1], between_study_sd[,5][2]),
                                                 lci = c(Sens[,4][1], Spec[,4][1],
                                                         1 - Spec[,6][1], 
                                                         correlation[,4][3],
                                                         between_study_sd[,4][1], between_study_sd[,4][2]),
                                                 uci = c(Sens[,6][1], Spec[,6][1],
                                                         1 - Spec[,4][1], 
                                                         correlation[,6][3],
                                                         between_study_sd[,6][1], between_study_sd[,6][2]),
                                                 row.names=c("Sensitivity", "Specifcity", "FPR", 
                                                             "Correlation",
                                                             "sd_sens", "sd_spec")
                                  )
                                  
                                  # Create a matrix to store the parameter estimates
                                  nrow <- 7
                                  s.matrix <- matrix(nrow=nrow, ncol=5)
                                  
                                  s.matrix[1,1] <- paste0("Sensitivity [same prior across all levels of covariate] ", "( ", paste0("logit", HTML("<sup>-1</sup>")), "(", HTML("&mu;<sub>1</sub>"), ")",  ")")
                                  s.matrix[2,1] <- paste0("Specificity [same prior across all levels of covariate] ", "( ", paste0("logit", HTML("<sup>-1</sup>")), "(", HTML("&mu;<sub>0</sub>"), ")",  ")")
                                  s.matrix[3,1] <- "False positive rate (1 - specificity) [same prior across all levels of covariate] "
                                  s.matrix[4,1] <- paste0("Between-study Correlation [same prior and parameter across all levels of covariate] " , "( ", HTML("&rho;"), " )")
                                  s.matrix[5,1] <- paste0("Between-study SD for logit(Sensitivity) [same prior and parameter across all levels of covariate] ", "( ", HTML("&sigma;<sub>1</sub>"), " )")
                                  s.matrix[6,1] <- paste0("Between-study SD for logit(Specificity) [same prior and parameter across all levels of covariate] ", "( ", HTML("&sigma;<sub>0</sub>"), " )")
                                  
                                  for (i in 1:3) {
                                    for (j in 1:(nrow)-1) { 
                                      s.matrix[j,i+1] <- sprintf('%4.3f', Z[j,i])
                                    }
                                  }
                                  
                                  s.matrix[,5] <- paste0("(", s.matrix[,3], ", ", s.matrix[,4], ")")
                                  s.matrix[nrow, 1:5] <- ""
                                  s.matrix <- s.matrix[, c(1,2,5)]
                                  
                                  #Name the columns of the matrix
                                  colnames(s.matrix) <- c("Parameter", "Prior Median", "95% Prior Interval")
                                  
                                  s.matrix
                                  
                                }
                        

                    # MR - Table of prior distributions - cts covariate ----------------------------------------------------------------------
                        else { 
                                  logit_intercepts <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("mu"))$summary
                                  logit_coeffs <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("coeff"))$summary
                                  Sens <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se_at_cov_input"))$summary
                                  Spec <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Sp_at_cov_input"))$summary
                                  correlation <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Omega"))$summary
                                  between_study_sd <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sigma"))$summary
                                  
                                  print(logit_intercepts)
                                  
                                  Z = data.frame(estimate = c(logit_intercepts[,5][1], logit_intercepts[,5][2], 
                                                              logit_coeffs[,5][1], logit_coeffs[,5][2], 
                                                              Sens[,5], Spec[,5],
                                                              correlation[,5][3], 
                                                              between_study_sd[,5][1], between_study_sd[,5][2]),
                                                 lci = c(logit_intercepts[,4][1], logit_intercepts[,4][2], 
                                                         logit_coeffs[,4][1], logit_coeffs[,4][2], 
                                                         Sens[,4], Spec[,4],
                                                         correlation[,4][3],
                                                         between_study_sd[,4][1], between_study_sd[,4][2]),
                                                 uci = c(logit_intercepts[,6][1], logit_intercepts[,6][2], 
                                                         logit_coeffs[,6][1], logit_coeffs[,6][2], 
                                                         Sens[,6], Spec[,6],
                                                         correlation[,6][3],
                                                         between_study_sd[,6][1], between_study_sd[,6][2]),
                                                 row.names=c("logit_intercept_se", "logit_intercept_sp", 
                                                             "logit_coeff_se", "logit_coeff_sp", 
                                                             "Sensitivity", "Specifcity",  
                                                             "Correlation",
                                                             "sd_sens", 
                                                             "sd_spec")
                                  )
                                  
                                  
                                  # Create a matrix to store the parameter estimates
                                  nrow <- 10
                                  s.matrix <- matrix(nrow=nrow, ncol=5)

                                  s.matrix[1,1] <- paste0("intercept for logit(sensitivity) ", "( ", HTML("&alpha;<sub>1</sub>"), " )")
                                  s.matrix[2,1] <- paste0("intercept for logit(specificity) ", "( ", HTML("&alpha;<sub>0</sub>"), " )")
                                  s.matrix[3,1] <- paste0("coefficient for logit(sensitivity) ", "( ", HTML("&nu;<sub>1</sub>"), " )")
                                  s.matrix[4,1] <- paste0("coefficient for logit(specificity) ", "( ", HTML("&nu;<sub>0</sub>"), " )")
                                  s.matrix[5,1] <- paste0("Sensitivity at inputted value of coefficient", "( ", paste0("logit", HTML("<sup>-1</sup>")), "(", HTML("&alpha;<sub>1</sub>"), "+", input$MRcts_input_cov, HTML("&nu;<sub>1</sub>"), ")",  ")")
                                  s.matrix[6,1] <- paste0("Specificity at inputted value of coefficient", "( ", paste0("logit", HTML("<sup>-1</sup>")), "(", HTML("&alpha;<sub>0</sub>"), "+", input$MRcts_input_cov, HTML("&nu;<sub>0</sub>"), ")",  ")")
                                  s.matrix[7,1] <-  paste0("Between-study Correlation" , "( ", HTML("&rho;"), " )")
                                  s.matrix[8,1] <-  paste0("Between-study SD for logit(Sensitivity) ", "( ", HTML("&sigma;<sub>1</sub>"), " )")
                                  s.matrix[9,1] <-  paste0("Between-study SD for logit(Specificity) ", "( ", HTML("&sigma;<sub>0</sub>"), " )")
                                  
                                  
                                  for (i in 1:3) {
                                    for (j in 1:(nrow)-1) { 
                                      s.matrix[j,i+1] <- sprintf('%4.3f', Z[j,i])
                                    }
                                  }

                                  s.matrix[,5] <- paste0("(", s.matrix[,3], ", ", s.matrix[,4], ")")
                                  s.matrix[nrow, 1:5] <- ""
                                  s.matrix <- s.matrix[, c(1,2,5)]
                                  
                                  #Name the columns of the matrix
                                  colnames(s.matrix) <- c("Parameter", "Prior Median", "95% Prior Interval")
                                  
                                  s.matrix
                            }
                      
                            }, sanitize.text.function = function(x) x)
    }
      
  )
}



# Trace plots ------------------------------------------------------------------  --------------------------------------------------------------------------------------
MR_model_trace_plots_server <- function(id,
                                        draws, 
                                        cts_cov_indicator) {  # "mod" is the rstan file which is reactive. 
  moduleServer(
    id,
    function(input, output, session) {
      
           trace_plots_obj <- reactive({
             
                            req(draws(), input$covcheck_model, cancelOutput = TRUE)
   
                            mod <- draws()
                            
                            cts_cov_indicator <-  cts_cov_indicator$cts_cov_indicator
                            
                            if (cts_cov_indicator == 0)  { # if categorical/discrete covariate

                                  plot <-   stan_trace(mod, 
                                               nrow = input$trace_plot_nrow, 
                                               ncol = input$trace_plot_ncol,
                                           pars = c("Se", "Sp", "Sigma", "se", "sp")) 
                            }
                            else { #- if cts covariate
                                  plot <-   stan_trace(mod, 
                                               nrow = input$trace_plot_nrow, 
                                               ncol = input$trace_plot_ncol,
                                               pars = c("Se_at_cov_input", "Sp_at_cov_input", "Sigma", "se", "sp"))
                            }
                          return(plot)
    })
           
           observe({
               output$trace_plots <- renderPlot({
                                                trace_plots_obj()
                                     }, height = input$trace_plot_dimension_slider_height,
                                        width  = input$trace_plot_dimension_slider_width)
           })
           
           # Download ggplot object 
           output$download_trace_plot <- downloadHandler(
             filename = function(){
               paste("trace_plot.png")
             },
             content = function(file) { 
               {ggsave(file, trace_plots_obj(),
                       width = input$trace_plot_dl_width,
                       height = input$trace_plot_dl_height,
                       dpi = input$trace_plot_dl_dpi,
                       units = "in"
               )}
             } 
           )
                     
    }
  )
}


# Posterior density plots  -----------------------------------------------------  --------------------------------------------------------------------------------------
MR_model_posterior_density_plots_server <- function(id, 
                                                    draws, 
                                                    cts_cov_indicator) {  # "mod" is the rstan file which is reactive. 
  moduleServer(
    id,
    function(input, output, session) {
      
                    posterior_density_plots_obj <- reactive({
    
                            req(draws(), input$covcheck_model, cancelOutput = TRUE)

                            mod <- draws()
                            
                            cts_cov_indicator <-  cts_cov_indicator$cts_cov_indicator
                            
                             if (cts_cov_indicator == 0)  { # if categorical/discrete covariate
                                plot <- stan_dens(mod, 
                                                  nrow = input$posterior_density_plot_nrow, 
                                                  ncol = input$posterior_density_plot_ncol,
                                                  pars = c("Se", "Sp", "Sigma", "se", "sp"))  
                              }
                              else { # cts covariate
                               plot <-  stan_dens(mod, 
                                                  nrow = input$posterior_density_plot_nrow, 
                                                  ncol = input$posterior_density_plot_ncol,
                                                  pars = c("Se_at_cov_input", "Sp_at_cov_input", "Sigma", "se", "sp")) 
                              }
                            
                            return(plot)
                    })
                    
                    observe({
                    output$posterior_density_plots <- renderPlot({
                                                                   posterior_density_plots_obj()
                                                      }, height = as.numeric(input$posterior_density_plot_dimension_slider_height),
                                                         width  = as.numeric(input$posterior_density_plot_dimension_slider_width))
                    })
                    
                    
                    # Download ggplot object 
                    output$download_dens_plot <- downloadHandler(
                      filename = function(){
                        paste("posterior_density_plot.png")
                      },
                      content = function(file) { 
                        {ggsave(file, posterior_density_plots_obj(),
                                width = input$dens_plot_dl_width,
                                height = input$dens_plot_dl_height,
                                dpi = input$dens_plot_dl_dpi,
                                units = "in"
                        )}
                      } 
                    )
    }
  )
}


# Table which displays observed data and study weights from model (study-level outcomes) ------  --------------------------------------------------------------------------------------------------------------


MR_data_table_UI <- function(id) {
  ns <- NS(id)   
    DT::dataTableOutput(outputId =  ns("data_table"))
}

MR_data_table_settings_UI <- function(id) {
  ns <- NS(id)   
    downloadButton(outputId = ns("data_table_download"), label = "Download Table")
}

MR_data_table_server <- function(id, 
                                 cts_cov_indicator , 
                                 data,
                                 draws) {  # "mod" is the rstan file which is reactive. 
  moduleServer(
    id,
    function(input, output, session) {

      data_table_obj <- reactive({
        
                req(draws(), input$covcheck_model, cancelOutput = TRUE)
                
                cts_cov_indicator <-  cts_cov_indicator$cts_cov_indicator
  
                mod <- draws()
                
                X <- data()
                
                cov_index <-  as.integer(as.double(input$covcheck_model)) - 1
                
                C <-  ncol(data())  
                Names <-  colnames(data()) 
                if (C > 8 & Names[7] != "rob_PS") {   j <<- 6  } else { j <<- 13  }
                
               if (cts_cov_indicator == 0)  { # if categorical/discrete covariate ----------------------------
        
                    num_levels <- length(rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se"))$summary[,5])
                    
                    X <- MA_weights(X, mod)
                    
                    # observed values
                    ss <- tibble( 
                      Study =as.numeric(as.factor(X$author)), 
                      !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)) := factor(X[, j + cov_index]),
                      TP=X$TP, FN=X$FN, FP=X$FP, TN=X$TN,
                      N=(X$TP+X$FN+X$FP+X$TN) ,
                      Sensitivity= (TP/(TP+FN))  , 
                      Specificity= (TN/(TN+FP))  , 
                      prev = round((TP+FN)/N, 2), 
                      pctse = X$pctse, 
                      pctsp = X$pctsp
                    )
        
                    ss2 <- left_join(ss, X, by = c("TP", "FN", "FP", "TN")) 

                    no_covariates <- length(colnames(X[,(j+1):C])) # total number of covariates       
                    
                    # make cols without .cts and .cat endings for display
                    for (i in 1:no_covariates) { 
                      ss2 <- dplyr::mutate(ss2,
                                           !!as.name( str_sub(colnames(X)[j + i], end = -5) ) := !!as.name( colnames(X)[j + i]) )
                    }
                    
                    ss2 <- dplyr::mutate(ss2, 
                                         Weight_Sens = round(pctse.y, 2), 
                                         Weight_Spec = round(pctsp.y, 2),
                                         Sensitivity = round(Sensitivity, 2), 
                                         Specificity = round(Specificity, 2))
                    
                    # select columns to display
                    cols <- c("Study", "author", "year", str_sub( colnames(X)[c((j+1):(j+no_covariates))] ,
                                                                  end = -5), 
                              "Sensitivity", "Specificity", "Weight_Sens", "Weight_Spec")
                    
                    # arrange data by the selected covariate
                    data <- dplyr::select(ss2, cols) %>% 
                            dplyr::arrange( !!as.name( str_sub(colnames(X)[j + cov_index], end = -5) ) )
                    
                    data <- data.frame(data)
                    # Run the Garabage Collector to Ensure any excess memory used by stan is freed
                    gc()
                    return(data)

                    
                  }
                  
               else {          #  cts covariate --------------------------------------------------------------
                    # study weights 
                    X <- MA_weights(X, mod)
                    
                    # observed values
                    ss<- tibble( 
                      Study =as.numeric(as.factor(X$author)), 
                      TP=X$TP, FN=X$FN, FP=X$FP, TN=X$TN,
                      N=(X$TP+X$FN+X$FP+X$TN) ,
                      Sensitivity= (TP/(TP+FN))  , 
                      Specificity= (TN/(TN+FP))  , 
                      prev = round((TP+FN)/N, 2), 
                      FPR = 1 - Specificity, 
                      pctse = X$pctse, 
                      pctsp = X$pctsp
                    )
                    
                    ss2 <- left_join(ss, X, by = c("TP", "FN", "FP", "TN"))
                    
                    no_covariates <- length(colnames(X[,(j+1):C])) # total number of covariates    
                    
                    # make cols without .cts and .cat endings for display
                    for (i in 1:no_covariates) { 
                      ss2 <- dplyr::mutate(ss2, 
                                           !!as.name( str_sub(colnames(X)[j + i], end = -5) ) := !!as.name( colnames(X)[j + i]) )
                    }
                    
                    ss2 <- dplyr::mutate(ss2, Weight_Sens = round(pctse.y, 2), 
                                               Weight_Spec = round(pctsp.y, 2),
                                               Sensitivity = round(Sensitivity, 2), 
                                               Specificity = round(Specificity, 2))
                    
                    
                    # select columns to display
                    cols <- c("Study", "author", "year", str_sub( colnames(X)[c((j+1):(j+no_covariates))] , end = -5),  
                              "Sensitivity", "Specificity", "Weight_Sens", "Weight_Spec")
                    
                    # arrange data by the selected covariate
                    data <- dplyr::select(ss2, cols) %>% 
                            dplyr::arrange( !!as.name( str_sub(colnames(X)[j + cov_index], end = -5) ) )
                    
                    data <- data.frame(data)
                    # Run the Garabage Collector to Ensure any excess memory used by stan is freed
                    gc()
                    return(data)
                    
           }
              
      })
      
      output$data_table <- DT::renderDataTable({  
                    
                              options(DT.options = list(pageLength = 30, 
                                                        autoWidth = TRUE, 
                                                        scrollX=T))
                            
                              # Run the Garabage Collector to Ensure any excess memory used by stan is freed
                              gc()
                              return(DT::datatable( data_table_obj() ))

        })
  
                                     
      output$data_table_download <- downloadHandler(
                                            filename = function(){
                                              paste("table.csv")
                                            },
                                            content = function(file) { 
                                              write.csv(data_table_obj(), 
                                                          file, sep=",", row.names=FALSE) 
                                              } 
      )
    }
  )
}




# Parameter estimates table  ---------------------------------------------------  -----------------------------------------------------------------------------------------------------

# UI function for parameter estimates table 
MR_parameter_estimates_table_UI <- function(id) {
  ns <- NS(id)  
  uiOutput(outputId = ns("parameter_estimates_table"))
}


MR_parameter_estimates_table_renderUI <- function(id,
                                                   cts_cov_indicator) {
  
  moduleServer(
    id,
    function(input, output, session) {

      output$parameter_estimates_table <- renderUI({ 
        
        cts_cov_indicator <-  cts_cov_indicator$cts_cov_indicator
        ns <- session$ns 
        
        if (cts_cov_indicator == 0) { # cat covariate
          tagList(
            h4("Shared parameters"),
            tableOutput(ns("table_1")),
            downloadButton(ns("download_table_1"), "Download Table"),
            br(),
            br(),
            h4("Group-specific parameters"),
            tableOutput(ns("table_2")),
            downloadButton(ns("download_table_2"), "Download Table"),
            br(),
            br(),
            h4("Table of pairwise accuracy differences and ratios"),
            tableOutput(ns("table_3")),
            downloadButton(ns("download_table_3"), "Download Table")
          ) 
        } else { # cts covariate 
          tagList(
            h4(paste0("Parameters which depend on value of coefficient ", "(", input$MRcts_input_cov, ")")),
            tableOutput(ns("table_1")),
            downloadButton(ns("download_table_1"), "Download Table"),
            br(), 
            h4(paste0("Parameters which do not depend on value of coefficient ", "(", input$MRcts_input_cov, ")")),
            tableOutput(ns("table_2")),
            downloadButton(ns("download_table_2"), "Download Table"),
            br()
          )
        }
        
      })
  
    })
}


# Server function for parameter estimates tables
MR_parameter_estimates_table_server <- function(id, 
                                                cts_cov_indicator,
                                                data,
                                                draws) {  
  
  moduleServer(
    id,
    function(input, output, session) {
        
      
      # MR: table 1 / Shared params  ------------------------------  --------------------------------------------------------------------------------------------
        table_obj_1 <- reactive({            
          
          req(draws(), input$covcheck_model, cancelOutput = TRUE)
          
            mod <- draws()
            cov_index <-  as.integer(as.double(input$covcheck_model)) - 1
            
            cts_cov_indicator <-  cts_cov_indicator$cts_cov_indicator
            
            if (cts_cov_indicator == 1)  { # if cts covariate -------------------------------------------------------------------------------
              
                      Sens <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se_at_cov_input"))$summary
                      Spec <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Sp_at_cov_input"))$summary
                      lSens <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("lSe_at_cov_input"))$summary
                      lSpec <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("lSp_at_cov_input"))$summary
                      Sens_pred <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se_pred_at_cov_input"))$summary
                      Spec_pred <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Sp_pred_at_cov_input"))$summary
                      DOR <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("DOR_at_cov_input"))$summary
                      LRp <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("LRp_at_cov_input"))$summary
                      LRn <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("LRn_at_cov_input"))$summary
                      Theta <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("theta_at_cov_input"))$summary
                      Lambda <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("lambda_at_cov_input"))$summary
                      beta <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("beta_at_cov_input"))$summary
                      sigma2_theta <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sigma_sq_theta_at_cov_input"))$summary
                      sigma2_alpha <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sigma_sq_alpha_at_cov_input"))$summary
                      
                      Z = data.frame(estimate =  c(Sens[,5], Spec[,5], 1 - Spec[,5],
                                                   Sens_pred[,5], Spec_pred[,5], 1 - Spec_pred[,5],
                                                   lSens[,5], lSpec[,5], 
                                                   DOR[,5], LRp[,5], LRn[,5],
                                                   Theta[,5], Lambda[,5], sigma2_theta[,5], sigma2_alpha[,5]
                                                   ),
                                     lci = c(Sens[,4], Spec[,4], 1 - Spec[,6],
                                             Sens_pred[,4], Spec_pred[,4], 1 - Spec_pred[,6], 
                                             lSens[,4], lSpec[,4], 
                                             DOR[,4], LRp[,4], LRn[,4],
                                             Theta[,4], Lambda[,4], sigma2_theta[,4], sigma2_alpha[,4]
                                             ),
                                     uci =  c(Sens[,6], Spec[,6],  1 - Spec[,4],
                                              Sens_pred[,6], Spec_pred[,6], 1 - Spec_pred[,4],
                                              lSens[,6],lSpec[,6], 
                                              DOR[,6], LRp[,6], LRn[,6],
                                              Theta[,6], Lambda[,6], sigma2_theta[,6], sigma2_alpha[,6]
                                              ),
                                     row.names=c("Sens", "Spec", "fp",
                                                 "Predicted Sens", "Predicted spec", "Predicted fp",
                                                 "logit Sens", "logit Spec", 
                                                 "DOR", "LRp", "LRn",
                                                 "theta", "lambda", "sigma2_theta", "sigma2_alpha")
                      )
                      
                      # Create a matrix to store the parameter estimates
                      nrow <- 16
                      s.matrix <- matrix(nrow=nrow, ncol=5)
                      
                      s.matrix[1,1] <- paste0("Sensitivity ", "( ", paste0("logit", HTML("<sup>-1</sup>")), "(", HTML("&alpha;<sub>1</sub>"), "+", input$MRcts_input_cov, HTML("&nu;<sub>1</sub>"), ")",  ")")
                      s.matrix[2,1] <- paste0("Specificity ", "( ", paste0("logit", HTML("<sup>-1</sup>")), "(", HTML("&alpha;<sub>0</sub>"), "+", input$MRcts_input_cov, HTML("&nu;<sub>0</sub>"), ")",  ")")
                      s.matrix[3,1] <- "False positive rate (1 - specificity)"
                      s.matrix[4,1] <- "Prediction interval for Sensitivity"
                      s.matrix[5,1] <- "Prediction interval for Specificity"
                      s.matrix[6,1] <- "Prediction interval for False positive rate"
                      s.matrix[7,1] <-  paste0("logit(sensitivity) ", "( ", HTML("&mu;<sub>1</sub>"), " )")
                      s.matrix[8,1] <-  paste0("logit(specificity) ", "( ", HTML("&mu;<sub>0</sub>"), " )")
                      s.matrix[9,1] <- "DOR"
                      s.matrix[10,1] <- "Likelihood Ratio +ve"
                      s.matrix[11,1] <- "Likelihood Ratio -ve"
                      s.matrix[12,1] <-  paste0("Cutpoint parameter ", "( ", HTML("&Theta;"), " )")
                      s.matrix[13,1] <-  paste0("Accuracy parameter ", "( ", HTML("&Lambda;"), " )")
                      s.matrix[14,1] <-  paste0("SD of cutpoint parameter ", "( ", HTML("&sigma;<sub>&theta;</sub>"), " )")
                      s.matrix[15,1] <-  paste0("SD of accuracy parameter ", "( ", HTML("&sigma;<sub>&alpha;</sub>"), " )")

                      
                      for (i in 1:3) {
                        for (j in 1:(nrow-1)) { 
                          s.matrix[j,i+1] <- sprintf('%4.3f', Z[j,i])
                        }
                      }

                      s.matrix[4:6,2] <- "-" # prediction intervals 
                      
                      s.matrix[,5] <- paste0("(", s.matrix[,3], ", ", s.matrix[,4], ")")
                      s.matrix[nrow, 1:5] <- ""
                      s.matrix <- s.matrix[, c(1,2,5)]
                      
                      #Name the columns of the matrix
                      colnames(s.matrix) <- c("Parameter", "Posterior Median", "95% Posterior Interval")
                      
                      s.matrix
              }
              
              else {    # discrete / cetegorical covariate ------------------------------------------------------------------------------------------------
                
                      num_levels <- length(rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se"))$summary[,5])
                      
                      correlation <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Omega"))$summary
                      beta <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("beta"))$summary
                      sigma2_theta <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sigma_sq_theta"))$summary
                      sigma2_alpha <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sigma_sq_alpha"))$summary
                      between_study_sd <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sigma"))$summary
                      
                      Z_shared <- data.frame(estimate = c(correlation[,5][3], 
                                                          between_study_sd[,5][1],
                                                          between_study_sd[,5][2],
                                                          beta[,5], 
                                                          sigma2_theta[,5], 
                                                          sigma2_alpha[,5]),
                                             lci =  c(correlation[,4][3], 
                                                      between_study_sd[,4][1],
                                                      between_study_sd[,4][2],
                                                      beta[,4], 
                                                      sigma2_theta[,4], 
                                                      sigma2_alpha[,4]),
                                             uci =  c(correlation[,6][3], 
                                                      between_study_sd[,6][1],
                                                      between_study_sd[,6][2],
                                                      beta[,6], 
                                                      sigma2_theta[,6], 
                                                      sigma2_alpha[,6]),
                                             row.names=c("Correlation", 
                                                         "sd_sens", 
                                                         "sd_spec", 
                                                         "beta", 
                                                         "sigma2_theta",
                                                         "sigma2_alpha")
                      )
                      
                      # Create a matrix to store the parameter estimates
                      nrow <- 7
                      s.matrix.shared <- matrix(nrow=nrow, ncol=5)
                      
                      s.matrix.shared[1,1] <- paste0("Between-study Correlation" , "( ", HTML("&rho;"), " )")
                      s.matrix.shared[2,1] <- paste0("Between-study SD for logit(Sensitivity) ", "( ", HTML("&sigma;<sub>1</sub>"), " )")
                      s.matrix.shared[3,1] <- paste0("Between-study SD for logit(Specificity) ", "( ", HTML("&sigma;<sub>0</sub>"), " )")
                      s.matrix.shared[4,1] <- paste0("Shape parameter ", "( ", HTML("&beta;"), " )")
                      s.matrix.shared[5,1] <- paste0("SD of cutpoint parameter ", "( ", HTML("&sigma;<sub>&theta;</sub>"), " )")
                      s.matrix.shared[6,1] <- paste0("SD of accuracy parameter ", "( ", HTML("&sigma;<sub>&alpha;</sub>"), " )")
                      
                      for (i in 1:3) {
                        for (j in 1:(nrow)-1) { 
                          s.matrix.shared[j,i+1] <- sprintf('%4.3f', Z_shared[j,i])
                        }
                      }
                      
                      s.matrix.shared[,5] <- paste0("(", s.matrix.shared[,3], ", ", s.matrix.shared[,4], ")")
                      s.matrix.shared[nrow, 1:5] <- ""
                      s.matrix.shared <- s.matrix.shared[, c(1,2,5)]
                      
                      #Name the columns of the matrix
                      colnames(s.matrix.shared) <- c("Parameter", "Posterior Median", "95% Posterior Interval")
                      
                      s.matrix.shared
                    }

            
          })
          
          
        # MR: table 2 (for cts covariate) / group-specific params (for categorical covariate) --------------------------------------------------------------------------------------
          table_obj_2 <- reactive({
            
            req(draws(), input$covcheck_model, cancelOutput = TRUE)
            
            
            mod <- draws()
            cov_index <-  as.integer(as.double(input$covcheck_model)) - 1
            
            C <-  ncol(data())
            Names <-  colnames(data()) 
            X <- data()
            
            if (C > 8 & Names[7] != "rob_PS") {   m <<- 6  } else { m <<- 13  }
            
            cts_cov_indicator <-  cts_cov_indicator$cts_cov_indicator
              
              if  (cts_cov_indicator == 0) {    # categorical / discrete MR  ---------------------------------------------------------------------------------
                
                s.matrix.group.dataframes <- list()
                num_levels <- length(rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se"))$summary[,5])
                
                logit_Sens <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("lSe"))$summary
                logit_Spec <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("lSp"))$summary
                Sens <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se"))$summary
                Spec <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Sp"))$summary
                DOR <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("DOR"))$summary
                LRp <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("LRp"))$summary
                LRn <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("LRn"))$summary
                Theta <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("theta"))$summary
                Lambda <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("lambda"))$summary
                
                Z_group <- list()
                
                
                for (i in 1:num_levels) {
                  Z_group[[i]] <- data.frame(estimate = c(logit_Sens[,5][i], 
                                                          logit_Spec[,5][i], 
                                                          Sens[,5][i],
                                                          Spec[,5][i], 
                                                          1 - Spec[,5][i],
                                                          DOR[,5][i], 
                                                          LRp[,5][i], 
                                                          LRn[,5][i],
                                                          Theta[,5][i],
                                                          Lambda[,5][i]), 
                                             lci  = c(logit_Sens[,4][i], 
                                                      logit_Spec[,4][i],
                                                      Sens[,4][i],
                                                      Spec[,4][i], 
                                                      1 - Spec[,6][i],
                                                      DOR[,4][i], 
                                                      LRp[,4][i], 
                                                      LRn[,4][i],
                                                      Theta[,4][i],
                                                      Lambda[,4][i]), 
                                             uci = c(logit_Sens[,6][i], 
                                                     logit_Spec[,6][i],
                                                     Sens[,6][i],
                                                     Spec[,6][i], 
                                                     1 - Spec[,4][i],
                                                     DOR[,6][i], 
                                                     LRp[,6][i], 
                                                     LRn[,6][i],
                                                     Theta[,6][i],
                                                     Lambda[,6][i]), 
                                             row.names = c("Logit Sens", "Logit Spec", "Sensitivity", "Specificity",
                                                           "FPR", "DOR", "LR+" , "LR-",
                                                           "Theta", "Lambda")
                  )
                }
                
                s.matrix.group <- vector("list", num_levels)
                statticks <- list()
                
              
                
                for (i in 1:num_levels) {
                  nrow <- 11
                  s.matrix.group[[i]] <- matrix(nrow=nrow, ncol=5)
          
                    s.matrix.group[[i]][1,1] <-   paste0("logit(sensitivity) ", "( ", HTML("&mu;<sub>1</sub>"), "L=", levels(factor(X[, m + cov_index]))[i], " )")
                    s.matrix.group[[i]][2,1] <-   paste0("logit(specificity) ", "( ", HTML("&mu;<sub>0</sub>"), "L=", levels(factor(X[, m + cov_index]))[i], " )")
                    s.matrix.group[[i]][3,1] <-   paste0("Sensitivity ", "( ", paste0("logit", HTML("<sup>-1</sup>")), "(", HTML("&mu;<sub>1</sub>"), "L=", levels(factor(X[, m + cov_index]))[i],")",")")
                    s.matrix.group[[i]][4,1] <-   paste0("Specificity ", "( ", paste0("logit", HTML("<sup>-1</sup>")), "(", HTML("&mu;<sub>0</sub>"), "L=", levels(factor(X[, m + cov_index]))[i],")",")")
                    s.matrix.group[[i]][5,1] <-   "False positive rate (1 - specificity)"
                    s.matrix.group[[i]][6,1] <-  "Diagnostic Odds Ratio"
                    s.matrix.group[[i]][7,1] <-  "Likelihood Ratio +ve"
                    s.matrix.group[[i]][8,1] <-  "Likelihood Ratio -ve"
                    s.matrix.group[[i]][9,1] <-  paste0("Cutpoint parameter ", "( ", HTML("&theta;"),  "L=", levels(factor(X[, m + cov_index]))[i], ")")
                    s.matrix.group[[i]][10,1] <- paste0("Accuracy parameter ", "( ", HTML("&lambda;"), "L=", levels(factor(X[, m + cov_index]))[i], ")")
                            
                            for (k in 1:3) {
                              for (j in 1:(nrow)-1) { 
                                s.matrix.group[[i]][j,k+1] <- sprintf('%4.3f', Z_group[[i]][j,k])
                              }
                            }
                            s.matrix.group[[i]][nrow, 1:4] <- ""
                            
                            s.matrix.group[[i]][,5] <- paste0("(", s.matrix.group[[i]][,3], ", ", s.matrix.group[[i]][,4], ")")
                            s.matrix.group[[i]][nrow, 1:5] <- ""
                            s.matrix.group[[i]] <- s.matrix.group[[i]][, c(1,2,5)]
                            

                            s.matrix.group.dataframes[[i]] <- data.frame(s.matrix.group[[i]])
                            
                            
                            colnames(s.matrix.group.dataframes[[i]]) <- c( paste("Parameters for",
                                                                                 ((  levels(factor(X[, m + cov_index])    )[i]))) , 
                                                                           "Posterior Median", 
                                                                           "95% Posterior Interval")
                  
                }
                
                s.matrix.group.dataframe_allgroups <- tibble(rbindlist(s.matrix.group.dataframes))
                
                
                s.matrix.group.dataframe_allgroups2 <- s.matrix.group.dataframe_allgroups %>% 
                  dplyr::mutate( " "
                                 = (lag(!!as.name(paste("Parameters for", ((levels(factor(X[, m + cov_index]) )[1])))))), 
                                 `Posterior Median` = lag(`Posterior Median`), 
                                 `95% Posterior Interval` = lag(`95% Posterior Interval`))
                
                s.matrix.group.dataframe_allgroups3 <- s.matrix.group.dataframe_allgroups2 %>%
                                                        dplyr::select(" ",
                                                               `Posterior Median`, 
                                                               `95% Posterior Interval` )
                
                s.matrix.group.dataframe_allgroups3
                s.matrix.group.dataframe_allgroups3[1,2] <- ""
                s.matrix.group.dataframe_allgroups3[1,3] <- ""

                for (i in 1:num_levels) {
                  s.matrix.group.dataframe_allgroups3[(i-1)*nrow+1,1] <- (paste0("<b> Parameters for ",
                                                                               ((levels(factor(X[, m + cov_index]) )[i])),":", " </b>"))
                }
                # Run the Garabage Collector to Ensure any excess memory used by stan is freed
                gc()
                return(s.matrix.group.dataframe_allgroups3)
                
              }  else { # cts covariate -------------------------------------------------------------------------------------------------------------------------------
                
                
                Coeffs <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("coeff"))$summary
                intercepts <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("mu"))$summary
                correlation <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Omega"))$summary
                between_study_sd <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sigma"))$summary
                beta <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("beta_at_cov_input"))$summary
                
                
                Z = data.frame(estimate =  c(beta[,5],
                                             Coeffs[,5][1], Coeffs[,5][2], 
                                             intercepts[,5][1], intercepts[,5][2], 
                                             correlation[,5][3], 
                                             between_study_sd[,5][1], between_study_sd[,5][2]
                ),
                lci = c(beta[,4],
                        Coeffs[,4][1], Coeffs[,4][2], 
                        intercepts[,4][1], intercepts[,4][2], 
                        correlation[,4][3], 
                        between_study_sd[,4][1], between_study_sd[,4][2]
                ),
                uci =  c(beta[,6],
                         Coeffs[,6][1], Coeffs[,6][2], 
                         intercepts[,6][1], intercepts[,6][2], 
                         correlation[,6][3], 
                         between_study_sd[,6][1], between_study_sd[,6][2]
                ),
                row.names=c("beta", 
                            "Coefficient for Sens", "Coefficient for Spec", 
                            "Intercept for Sens", "Intercept for Spec", 
                            "Correlation",
                            "sd_sens", "sd_spec"
                )
                )
                
                # Create a matrix to store the parameter estimates
                nrow <- 9
                s.matrix <- matrix(nrow=nrow, ncol=5)

                s.matrix[1,1] <- paste0("Shape parameter ", "( ", HTML("&beta;"), " )")
                s.matrix[2,1] <- paste0("coefficient for logit(sensitivity) ", "( ", HTML("&nu;<sub>1</sub>"), " )")
                s.matrix[3,1] <- paste0("coefficient for logit(specificity) ", "( ", HTML("&nu;<sub>0</sub>"), " )")
                s.matrix[4,1] <- paste0("intercept for logit(sensitivity) ", "( ", HTML("&alpha;<sub>1</sub>"), " )")
                s.matrix[5,1] <- paste0("intercept for logit(specificity) ", "( ", HTML("&alpha;<sub>0</sub>"), " )")
                s.matrix[6,1] <- paste0("Between-study Correlation" , "( ", HTML("&rho;"), " )")
                s.matrix[7,1] <- paste0("Between-study SD for logit(Sensitivity) ", "( ", HTML("&sigma;<sub>1</sub>"), " )")
                s.matrix[8,1] <- paste0("Between-study SD for logit(Specificity) ", "( ", HTML("&sigma;<sub>0</sub>"), " )")
                
                for (i in 1:3) {
                  for (j in 1:(nrow-1)) { 
                    s.matrix[j,i+1] <- sprintf('%4.3f', Z[j,i])
                  }
                }

                s.matrix[,5] <- paste0("(", s.matrix[,3], ", ", s.matrix[,4], ")")
                s.matrix[nrow, 1:5] <- ""
                s.matrix <- s.matrix[, c(1,2,5)]
                
                #Name the columns of the matrix
                colnames(s.matrix) <- c("Parameter", "Posterior Median", "95% Posterior Interval")
                
                s.matrix

                }
            
          })
          
          # MR: table 3 - table of pairwise differences and ratio's of Se/Sp's (only for  categorical covariate) --------------------------------------------------------------------------------------

          table_obj_3 <- reactive({
            
            req(draws(), input$covcheck_model, cancelOutput = TRUE)
            

            
            
            mod <- draws()
            cov_index <-  as.integer(as.double(input$covcheck_model)) - 1
            
            C <-  ncol(data())
            Names <-  colnames(data()) 
            X <- data()
            
            if (C > 8 & Names[7] != "rob_PS") {   m <<- 6  } else { m <<- 13  }
            
            
            s.matrix.group.dataframes <- list()
            num_levels <- length(rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se"))$summary[,5])
            n_comparisons <- choose(num_levels, 2)
            
            validate(
              need(num_levels <= 10, "This table only displays for covariates with less than or equal to 10 groups")
            )
            
            
           # print(n_comparisons)
            
            diffs_sens <-  rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("diffs_Se"))$summary 
            diffs_spec <-  rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("diffs_Sp"))$summary
            ratios_sens <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("ratios_Se"))$summary
            ratios_spec <- rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("ratios_Sp"))$summary

            
            Z_group <- list()
            
            # we have n_comparisons lots of estimates for each of the 4 variables (diffs / ratios of Se/Sp's)
            # therefore will have n_comparisons separate "tables" to display (display each set of 4 estimates in sep. table)
            
            
            for (i in 1:n_comparisons) {
              Z_group[[i]] <- data.frame(estimate = c(diffs_sens[,5][i], 
                                                      diffs_spec[,5][i], 
                                                      ratios_sens[,5][i],
                                                      ratios_spec[,5][i]), 
                                         lci  = c(diffs_sens[,4][i], 
                                                  diffs_spec[,4][i], 
                                                  ratios_sens[,4][i],
                                                  ratios_spec[,4][i]), 
                                         uci =  c(diffs_sens[,6][i], 
                                                  diffs_spec[,6][i], 
                                                  ratios_sens[,6][i],
                                                  ratios_spec[,6][i]), 
                                         row.names = c("Difference in sensitivities", 
                                                       "Difference in specificities", 
                                                       "Ratio of sensitivities",
                                                       "Ratio of specificities")
              )
            }
            
            s.matrix.group <- vector("list", n_comparisons)
            statticks <- list()
            
            comparisons <- tibble(comparisons = rep(NA, n_comparisons))
            


              counter = 1
              for (i in 1:(num_levels-1)) { 
                for (j in ((i+1):num_levels)) { 
                  comparisons$comparisons[counter] <-  (paste(  levels(factor(X[, m + cov_index]))[i], "vs", levels(factor(X[, m + cov_index]))[j] ))
                  counter = counter + 1 
                }
              }
                     

            
            print(comparisons)
            
            for (i in 1:n_comparisons) {
              nrow <- 5
              s.matrix.group[[i]] <- matrix(nrow=nrow, ncol=5)
              
              s.matrix.group[[i]][1,1] <-   paste0("Difference in sensitivities") # , "( ", HTML("&mu;<sub>1</sub>"), "L=", levels(factor(X[, m + cov_index]))[i], " )")
              s.matrix.group[[i]][2,1] <-   paste0("Difference in specificities")
              s.matrix.group[[i]][3,1] <-   paste0("Ratio of sensitivities")
              s.matrix.group[[i]][4,1] <-   paste0("Ratio of specificities") 

              
              for (k in 1:3) {
                for (j in 1:(nrow)-1) { 
                  s.matrix.group[[i]][j,k+1] <- sprintf('%4.3f', Z_group[[i]][j,k])
                }
              }
              s.matrix.group[[i]][nrow, 1:4] <- ""
              
              s.matrix.group[[i]][,5] <- paste0("(", s.matrix.group[[i]][,3], ", ", s.matrix.group[[i]][,4], ")")
              s.matrix.group[[i]][nrow, 1:5] <- ""
              s.matrix.group[[i]] <- s.matrix.group[[i]][, c(1,2,5)]
              
              
              s.matrix.group.dataframes[[i]] <- data.frame(s.matrix.group[[i]])
              
              
              colnames(s.matrix.group.dataframes[[i]]) <- c( paste("Parameters for comparison ",
                                                                   comparisons$comparisons[i]), # ((  levels(factor(X[, m + cov_index])    )[i]))) , 
                                                             "Posterior Median", 
                                                             "95% Posterior Interval")
              
            }
            
            s.matrix.group.dataframe_allgroups <- tibble(rbindlist(s.matrix.group.dataframes))
            
            
            s.matrix.group.dataframe_allgroups2 <- s.matrix.group.dataframe_allgroups %>% 
              dplyr::mutate( " "
                             # = (lag(!!as.name(paste("Parameters for comparison",
                             #                        comparisons$comparisons[i])))), 
                             = (lag((paste("Parameters for comparison ",
                                                    comparisons$comparisons[i])))), 
                             `Posterior Median` = lag(`Posterior Median`), 
                             `95% Posterior Interval` = lag(`95% Posterior Interval`))
            
            s.matrix.group.dataframe_allgroups3 <- s.matrix.group.dataframe_allgroups2 %>%
              dplyr::select(" ",
                            `Posterior Median`, 
                            `95% Posterior Interval` )
            
            s.matrix.group.dataframe_allgroups3
            s.matrix.group.dataframe_allgroups3[1,2] <- ""
            s.matrix.group.dataframe_allgroups3[1,3] <- ""
            
            for (i in 1:n_comparisons) {
              s.matrix.group.dataframe_allgroups3[(i-1)*nrow+1,1] <- (paste0("<b> Parameters for comparison ",
                                                                             comparisons$comparisons[i], # ((levels(factor(X[, m + cov_index]) )[i])),
                                                                             ":", " </b>"))
              #s.matrix.group.dataframe_allgroups3[(2+(5*(i-1))):(5+(5*(i-1))), 1] <- " "
              s.matrix.group.dataframe_allgroups3[(2+(5*(i-1))), 1] <- "Difference in sensitivities"
              s.matrix.group.dataframe_allgroups3[(3+(5*(i-1))), 1] <- "Difference in specificities"
              s.matrix.group.dataframe_allgroups3[(4+(5*(i-1))), 1] <- "Ratio of sensitivities"
              s.matrix.group.dataframe_allgroups3[(5+(5*(i-1))), 1] <- "Ratio of specificities"
            }
            # Run the Garabage Collector to Ensure any excess memory used by stan is freed
            gc()
            return(s.matrix.group.dataframe_allgroups3)
            
            
          })

        # output tables
        output$table_1 <- renderTable({
          req(data(), draws(), cancelOutput = TRUE)
                                         table_obj_1()
        }
        , sanitize.text.function = function(x) x)
        
        
        
        output$table_2 <- renderTable({
          req(data(), draws(), cancelOutput = TRUE)
                                         table_obj_2()
        }
        , sanitize.text.function = function(x) x)
        
        
        output$table_3 <- renderTable({
          req(data(), draws(), cancelOutput = TRUE)
          table_obj_3()
        }
        , sanitize.text.function = function(x) x)
        
        
        
        # download tables
        output$download_table_1 <- downloadHandler(
          filename = "MR_parameter_estimates_table_1.csv",
          content = function(file){
            write.csv(table_obj_1(), file, sep=",", row.names=FALSE) 
          }
         )
        
        output$download_table_2 <- downloadHandler(
          filename =  "MR_parameter_estimates_table_2.csv",
          content = function(file){
            write.csv(table_obj_2(), file, sep=",", row.names=FALSE) 
          }
        )
        
        output$download_table_3 <- downloadHandler(
          filename =  "MR_parameter_estimates_table_3.csv",
          content = function(file){
            write.csv(table_obj_3(), file, sep=",", row.names=FALSE) 
          }
        )
        
        
      }
     )    
}


# R-hat statistics table ------------------------------------------------------  ----------------------------------------------------------

# UI function for R-hat table 
MR_rhat_table_UI <- function(id) {
  ns <- NS(id)  
  tagList(
    verbatimTextOutput(ns("rhats_summary_msg")),
    DT::dataTableOutput(ns("rhats_table")),
    downloadButton(ns("download_rhat_table"), 
                   "Download Table"),
    p("NOTE:"), 
    p("If there are any R-hat values > 1.05, try:"),
    p("Increasing the adapt_delta above the default of 0.80,"),  
    p("Increasing the number of iterations above the default of 1500,"), 
    p("Using more informative priors"), 
    p(" "), 
    p("If there are any divergent transitions, try:"),
    p("Increasing the adapt_delta above the default of 0.80,"),
    p("Using more informative priors"),
    p(" "), 
    p("If there are any iterations that have exceeded the maximum treedepth, try increasing the treedepth above the default of 10")
  )
}


# Server function for R-hat table 
MR_rhat_table_server <- function(id, 
                                 draws, 
                                 cts_cov_indicator) {  
  
  moduleServer(
    id,
    function(input, output, session) {
      
      rhats_obj <- reactive({
        
        mod <- req(draws())
        
        cts_cov_indicator <-  cts_cov_indicator$cts_cov_indicator
        
        if (cts_cov_indicator == 1)  { # if continuous covariate ---------------------------
            
              pars <-  c( "mu", 
                          "coeff",
                          "sigma", 
                          "Omega", 
                          "se", "sp")
            
          } else { # cat. covariate 
            
            pars <-  c( "mu",
                        "sigma", 
                        "Omega", 
                        "se", "sp")
            
          }

          rhats <- round(summary(mod, 
                                 probs = c(0.025,  0.5, 0.975), 
                                 pars = pars)$summary[,8], 3)
          
          rhats_2 <- tibble(parameter = names(rhats), 
                            r_hat = rhats, 
                            category = case_when( r_hat < 1.05 ~ "good", 
                                                  r_hat %in% c(1.05, 1.10) ~ "borderline", 
                                                  r_hat > 1.10 ~ "bad")) %>% 
            dplyr::filter(r_hat != "NaN")
          
          
        # Run the Garabage Collector to Ensure any excess memory used by stan is freed
        gc()
        return(rhats_2)
        
      })
      
      output$rhats_summary_msg <- renderPrint({ 
        
        req(draws(), cancelOutput = TRUE)
        
        n_total_rhats <- nrow(rhats_obj())
        
        rhats_good <- filter(rhats_obj(), category == "good")
        rhats_borderline <- filter(rhats_obj(), category == "borderline")
        rhats_bad <- filter(rhats_obj(), category == "bad")
        
        n_good <- nrow(rhats_good)
        n_borderline <- nrow(rhats_borderline)
        n_bad <- nrow(rhats_bad)
        
        
        # print summary message 
        ifelse(n_total_rhats == n_good, 
               print("all R-hat's are good (i.e. < 1.05)"), 
               ifelse(n_bad == 0 && n_borderline != 0, 
                      paste0(n_borderline, print(" R-hat's are borderline (i.e. between 1.05 and 1.10")), 
                      paste0(n_bad, print(" R-hat's are bad (i.e. > 1.10) and "), 
                             n_borderline, print(" R-hat's are borderline (i.e. between 1.05 and 1.10)"))))
        
        
      })
      
      
      # print r-hat table (ordered by bad, moderate, good)
      output$rhats_table <- DT::renderDataTable({ 
        
        rhats_good <- filter(rhats_obj(), category == "good")
        rhats_borderline <- filter(rhats_obj(), category == "borderline")
        rhats_bad <- filter(rhats_obj(), category == "bad")
        
        n_good <- nrow(rhats_good)
        n_borderline <- nrow(rhats_borderline)
        n_bad <- nrow(rhats_bad)
        
        rhat_table <- rbind(rhats_bad, rhats_borderline, rhats_good)
        
        options(DT.options = list(autoWidth = TRUE,  
                                  pageLength = n_bad + n_borderline + 10))
        
        rhat_table
        
      })
      
      
      # download table
      output$download_rhat_table <- downloadHandler(
        
        filename = function(){
          paste("table.csv")
        },
        content = function(file) { 
          write.csv(rhats_obj(), file, sep=",", row.names=FALSE) 
        } 
      )
      
      
    }
  )    
}




























