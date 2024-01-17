


# Model diagnostics tab  - UI ("master") module  ------------------------------  -----------------------------------------

MA_model_diagnostics_tab_UI <- function(id) {
  ns <- NS(id)  
  uiOutput(ns("MA_model_diagnostics_tab_ui"))
}

MA_model_diagnostics_tab_renderUI_server <- function(id, SA_indicator) {  
  
  moduleServer(
    id,
    function(input, output, session) {

   output$MA_model_diagnostics_tab_ui <- renderUI({
     
          ns <- session$ns
          SA_indicator <- SA_indicator$SA_indicator
          
          # Main model ------------------------
              tagList( 
                h4("Main model"),
                h3("Sampler diagnostics"), 
                sampler_diagnostics_table_UI(id = "MA_model_id"),
                br(),
                h3("R-hat statistics"), 
                MA_rhat_table_UI(id = "MA_model_id"), 
                br(), 
                h3("Posterior density plots"),
                dropdownButton(
                  model_posterior_density_plots_settings_UI(id = "MA_model_id"),
                  circle = TRUE, status = "danger",
                  icon = icon("gear"), width = "300px",
                  tooltip = tooltipOptions(title = "Click to customise plot")
                ),
                model_posterior_density_plots_UI(id = "MA_model_id"),
                br(),
                h3("Trace plots"),
                dropdownButton(
                  model_trace_plots_settings_UI(id = "MA_model_id"),
                  circle = TRUE, status = "danger",
                  icon = icon("gear"), width = "300px",
                  tooltip = tooltipOptions(title = "Click to customise plot")
                ),
                model_trace_plots_UI(id = "MA_model_id"),
                
            # SA ------------------------   
                if (SA_indicator == TRUE) { 
                         tagList(
                           h4("Sensitivity analysis"),
                          sampler_diagnostics_table_UI(id = "SA_MA_model_id"),
                           br(),
                           h3("R-hat statistics"), 
                           MA_rhat_table_UI(id = "SA_MA_model_id"), 
                           br(), 
                           h3("Posterior density plots"),
                           dropdownButton(
                             model_posterior_density_plots_settings_UI(id = "SA_MA_model_id"),
                             circle = TRUE, status = "danger",
                             icon = icon("gear"), width = "300px",
                             tooltip = tooltipOptions(title = "Click to customise plot")
                           ),
                           model_posterior_density_plots_UI(id = "SA_MA_model_id"),
                           br(),
                           h3("Trace plots"),
                           dropdownButton(
                             model_trace_plots_settings_UI(id = "SA_MA_model_id"),
                             circle = TRUE, status = "danger",
                             icon = icon("gear"), width = "300px",
                             tooltip = tooltipOptions(title = "Click to customise plot")
                           ),
                           model_trace_plots_UI(id = "SA_MA_model_id")
                         )
                } # end of if
              
              ) # end of taglist
              
      })
   
    }
  )
}


# Parameter estimates tab  - UI ("master") module  ----------------------------  -----------------------------------------------------------------------------------

MA_parameter_estimates_tab_UI <- function(id) {
  ns <- NS(id)  
  uiOutput(ns("MA_parameter_estimates_tab_ui"))
}




MA_parameter_estimates_tab_renderUI_server <- function(id, SA_indicator) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      
      output$MA_parameter_estimates_tab_ui <- renderUI({
        
        print(SA_indicator$SA_indicator)
        
        ns <- session$ns
        
        tagList( 
          dropdownButton(
            MA_parameter_estimates_table_menu_UI(id = "MA_model_id"),
            circle = TRUE, status = "danger",
            icon = icon("gear"), width = "300px",
            tooltip = tooltipOptions(title = "Click to customise plot")
          ),
          MA_parameter_estimates_table_UI(id = "MA_model_id"),
          br(), 
          br(), 
          
          # SA ------------------------
          if (SA_indicator$SA_indicator == TRUE) { 

                           tagList(
                             h4("Sensitivity analysis"),
                             dropdownButton(
                               MA_parameter_estimates_table_menu_UI(id = "SA_MA_model_id"),
                               circle = TRUE, status = "danger",
                               icon = icon("gear"), width = "300px",
                               tooltip = tooltipOptions(title = "Click to customise plot")
                             ),
                             MA_parameter_estimates_table_UI(id = "SA_MA_model_id")
                           )
          }
          
          )
        
      })
      
      
    }
  )
}


# BVM - Forest Plots   ---------------------------------------------------------- -----------------------------

MA_forest_plots_UI <- function(id) {
  ns <- NS(id)  
  tagList( 
          fluidRow(
            splitLayout(cellWidths = c("50%", "50%"), #
                        plotOutput(ns("forest_plot_sens"))
                        ,plotOutput(ns("forest_plot_spec")))
          ),
          radioButtons(ns("filetype_forest"), 
                       label="Select plot format",
                       choices=list("png", "PDF")),
          downloadButton(ns("forest_plot_sens_download"), "Download Sensitivity Forest Plot"),
          downloadButton(ns("forest_plot_spec_download"), "Download Specificity Forest Plot")
  )
}



MA_forest_plots_server <- function(id, data) {  # "mod" is the rstan file which is reactive.
  
  moduleServer(
    id,
    function(input, output, session) {

   ### forest plot for sens ----------------------------------
      forest_plot_sens_obj <-  function() {
        
                                      X <- data()
                                      D <- mada::madad(X, correction.control = "any")
                                      plot <- forest(D, 
                                                      type = "sens", 
                                                      snames = X$author, 
                                                      xlab = "Sensitivity", 
                                                      main = "Forest plot of sensitivity")
                                      plot
      }
      
      # Output plot object
        output$forest_plot_sens <- renderPlot({  
          
          forest_plot_sens_obj()
          
           })
      
      # Download plot object 
      output$forest_plot_sens_download <- downloadHandler(
                                            filename =  function(){
                                              paste("Sensitivity_forest_plot", input$filetype_forest, sep=".")
                                            },
                                            content = function(file) { 
                                              if (input$filetype_forest == "png") {
                                                png(file)
                                                forest_plot_sens_obj()
                                                dev.off()
                                              }
                                              else {
                                                pdf(file)
                                                forest_plot_sens_obj()
                                                dev.off()
                                              }
                                            }
                                          )
      
      

### forest plot for spec ---- ------------------------------ 
      forest_plot_spec_obj <-  function() {
        
                                      X <- data()
                                      D <- madad(X, correction.control = "any")
                                      plot <- forest(D, 
                                                     type = "spec",
                                                     snames = X$author, 
                                                     xlab = "Specificity", 
                                                     main = "Forest plot of specificity")
                                      plot
                                      
      }
      
      # Output plot object
        output$forest_plot_spec <- renderPlot({  
          
          forest_plot_spec_obj()
          
        })
      
      # Download plot object 
      output$forest_plot_spec_download <- downloadHandler(
                                            filename =  function(){
                                              paste("Specificity_forest_plot", input$filetype_forest, sep=".")
                                            },
                                            content = function(file) { 
                                              if (input$filetype_forest == "png") {
                                                png(file)
                                                forest_plot_spec_obj()
                                                dev.off()
                                              }
                                              else {
                                                pdf(file)
                                                forest_plot_spec_obj()
                                                dev.off()
                                              }
                                            }
                                          )
      
    }
  )
}




# BVM - Table - Parameters for revman    -----------------------------  ---------------------------------------------------------------------------------------------------


MA_revman_plots_UI <- function(id) {
  ns <- NS(id)  
  tagList( 
    tableOutput(ns("revman")),
    downloadButton(ns("revman_download"), "Download Table")
  )
}



MA_revman_plots_server <- function(id, 
                                   data, 
                                   draws) {  # "mod" is the rstan file which is reactive. 
  
  moduleServer(
    id,
    function(input, output, session) {
      
                           revman_obj <- reactive({
                                               
                           mod <- draws()
                           X <- data()
                           
                           N <- length(X$TP)
                           
                           logit_Sens <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("lSe"))$summary[5]
                           logit_Spec <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("lSp"))$summary[5]
                           
                           between_study_sd_se <- (summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sigma"))$summary[1,5])
                           between_study_sd_sp <- (summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sigma"))$summary[2,5])
                           
                           between_study_var_se <- ( between_study_sd_se )^2
                           between_study_var_sp <- ( between_study_sd_sp )^2
                           
                           se_logit_sens <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("lSe"))$summary[2]
                           se_logit_spec <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("lSp"))$summary[2]
                           
                           correlation <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Omega"))$summary[2, 5]
                           
                           covariance <- correlation*between_study_sd_se*between_study_sd_sp
                             
                           
                           Z = data.frame(estimate = c(logit_Sens, logit_Spec, 
                                                       between_study_var_se, between_study_var_sp,
                                                       correlation,
                                                       se_logit_sens, se_logit_spec, 
                                                       covariance
                                                       ),
                                          row.names=c("logit(sens)", "logit(spec)", 
                                                      "Variance logit(sens)", "Variance logit(spec)", 
                                                      "Correlation",
                                                      "SE(logit(sens))", "SE(logit(spec))", 
                                                      "Covariance")
                           )
                           
                           # Create a matrix to store the parameter estimates
                           s.matrix <- matrix(nrow=8, ncol=2)
                           
                           s.matrix[1,1] <- "E(logitSe)"
                           s.matrix[2,1] <- "E(logitSp)"
                           s.matrix[3,1] <- "Var(logitSe)"
                           s.matrix[4,1] <- "Var(logitSp)"
                           s.matrix[5,1] <- "Corr(logits)"
                           s.matrix[6,1] <- "SE(E(logitSe))"
                           s.matrix[7,1] <- "SE(E(logitSp))"
                           s.matrix[8,1] <- "Cov(Es)"
                           
                           s.matrix[1,2] <- sprintf('%4.6f', Z[1,])
                           s.matrix[2,2] <- sprintf('%4.6f', Z[2,])
                           s.matrix[3,2] <- sprintf('%4.6f', Z[3,])
                           s.matrix[4,2] <- sprintf('%4.6f', Z[4,])
                           s.matrix[5,2] <- sprintf('%4.6f', Z[8,])
                           s.matrix[6,2] <- sprintf('%4.6f', Z[6,])
                           s.matrix[7,2] <- sprintf('%4.6f', Z[7,])
                           s.matrix[8,2] <- sprintf('%4.6f', Z[5,])
                           
                           
                           #Name the columns of the matrix
                           colnames(s.matrix) <- c("Parameter", "Estimate (Posterior Median)")
                           
                           #Only the rows of s.matrix where statticks=T will be displayed
                           s.matrix
                   
                 })
                                
          output$revman <- renderTable({

                                    req(draws(), cancelOutput = TRUE)
                           
                                    revman_obj()
                            
                          }, sanitize.text.function = function(x) x)
                 
          output$revman_download <- downloadHandler(
            filename = "parameters_for_revman.csv",
            content = function(file){
              write.csv(revman_obj(), file, sep=",", row.names=FALSE)
            }
          )
                                                           
                                                   
                    }
  )
}



# BVM - Plot of prior distributions  --------------- ---------------------------------------------------------------------------
MA_model_priors_plot_server <- function(id,
                                        draws_PO) {  # "mod" is the rstan file which is reactive. 
  moduleServer(
    id,
    function(input, output, session) {
      
        priors_plot_obj <- reactive({
          
          
                                validate(
                                  need(!(is.null(draws_PO())), "Please run prior model to display plot")
                                )
                                
                                req(draws_PO(), cancelOutput = TRUE)
          
                                params <- rstan::extract(draws_PO())
                                
                                se <- params$Se
                                sp <- params$Sp
                                corr <- params$Omega[, 1, 2]
                                sd_se <- params$sigma[, 1]
                                sd_sp <- params$sigma[, 2]
                                
                                n_samps <- (2000-200)*4
                                
                                data <- tibble(
                                  Samples = c(se, sp, corr, sd_se, sd_sp), 
                                  Parameter = c(rep("Sensitivity", n_samps), 
                                                rep("Specificity", n_samps), 
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
      })
      
      # output plot 
      observe({
            output$model_priors_plot <- renderPlot({
              
              validate(
                need(!(is.null(draws_PO())), "Please run prior model to display plot")
              )
              
              req(draws_PO(), cancelOutput = TRUE)
              
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

# BVM - table of prior distributions  --------------------------------  -----------------------------------------------------

# UI function
MA_model_priors_table_UI <- function(id) {
  ns <- NS(id)   
  tagList(
    tableOutput(outputId =  ns("model_priors_table")),
    p("NOTE: The default priors are:"),  
    p("For logit sensitivities and specificities - normal distribution with mean 0 and SD of 1, equivalent to a 95% prior interval of (0.05, 0.95) "),
    p("For between-study SD's of logit sensitivities and specificities - truncated (at 0) normal distribution with mean 0 and SD of 1, equivalent to a 95% prior interval of (0.03, 2.25) "),
    p("For between-study correlation between study-specific logit sensitivities and specificities - LKJ(2) prior, equivalent to 95% prior interval of (-0.8, 0.8)")
  )
}

# server function
MA_model_priors_table_server <- function(id, draws_PO) {  
  
  moduleServer(
    id,
    function(input, output, session) {
      
          output$model_priors_table <- renderTable({
            
            validate(
              need(!(is.null(draws_PO())), "Please run prior model to display plot")
            )
            
            req(draws_PO(), cancelOutput = TRUE)
              
                model <- req(draws_PO())
  
                Sens <- rstan::summary(model, probs = c(0.025,  0.5, 0.975), pars = c("Se"))$summary
                Spec <-  rstan::summary(model, probs = c(0.025,  0.5, 0.975), pars = c("Sp"))$summary
                correlation <-  rstan::summary(model, probs = c(0.025,  0.5, 0.975), pars = c("Omega"))$summary
                between_study_sd <-  rstan::summary(model, probs = c(0.025,  0.5, 0.975), pars = c("sigma"))$summary
                
                Z = data.frame(estimate = c(Sens[,5], Spec[,5],
                                            1 - Spec[,5], 
                                            correlation[,5][3], 
                                            between_study_sd[,5][1], between_study_sd[,5][2]),
                               lci = c(Sens[,4], Spec[,4],
                                       1 - Spec[,6], 
                                       correlation[,4][3],
                                       between_study_sd[,4][1], between_study_sd[,4][2]),
                               uci = c(Sens[,6], Spec[,6],
                                       1 - Spec[,4], 
                                       correlation[,6][3],
                                       between_study_sd[,6][1], between_study_sd[,6][2]),
                               row.names=c("Sensitivity", "Specifcity", "FPR", 
                                           "Correlation",
                                           "sd_sens", "sd_spec")
                )
                
                # Create a matrix to store the parameter estimates
                nrow <- 7
                s.matrix <- matrix(nrow=nrow, ncol=5)
                
                s.matrix[1,1] <-  paste0("Sensitivity ", "( ", paste0("logit", HTML("<sup>-1</sup>")), "(", HTML("&mu;<sub>1</sub>"), ")",  ")")
                s.matrix[2,1] <-  paste0("Specificity ", "( ", paste0("logit", HTML("<sup>-1</sup>")), "(", HTML("&mu;<sub>0</sub>"), ")",  ")")
                s.matrix[3,1] <-  "False Positive Rate (1 - Specificity)"
                s.matrix[4,1] <-  paste0("Between-study Correlation" , "( ", HTML("&rho;"), " )")
                s.matrix[5,1] <-  paste0("Between-study SD for logit(Sensitivity) ", "( ", HTML("&sigma;<sub>1</sub>"), " )")
                s.matrix[6,1] <-  paste0("Between-study SD for logit(Specificity) ", "( ", HTML("&sigma;<sub>0</sub>"), " )")
                
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
                # Run the Garabage Collector to Ensure any excess memory used by stan is freed
                gc()
                return(s.matrix)
                
              }, sanitize.text.function = function(x) x)
      
    })
  
}


# BVM - Trace plots ------------------------------------------------------------  --------------------------------------------------------------
MA_model_trace_plots_server <- function(id, draws, data) {  # "mod" is the rstan file which is reactive. 
  moduleServer(
    id,
    function(input, output, session) {
      
      trace_plots_obj <- reactive({
        
        req(data(), draws(), cancelOutput = TRUE)
        
        X <- data()
        mod <- draws()
        
        
        plot <-   stan_trace(mod, 
                                 nrow = input$trace_plot_nrow, 
                                 ncol = input$trace_plot_ncol,
                                 pars = c("Se", "Sp", "Sigma", "se", "sp")) 


        plot
        
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


# BVM - Posterior density plots  ----------------------------------------------  --------------------------------------------------------------
MA_model_posterior_density_plots_server <- function(id, draws, data) {  # "mod" is the rstan file which is reactive.
  
  moduleServer(
    id,
    function(input, output, session) {
      
      posterior_density_plots_obj <- reactive({
        
        req(data(), draws(), cancelOutput = TRUE)

        X <- data()
        mod <- draws()
        
        plot <- stan_dens(mod, 
                              nrow = input$posterior_density_plot_nrow, 
                              ncol = input$posterior_density_plot_ncol,
                              pars = c("Se", "Sp", "Sigma", "se", "sp"))  
  
        plot
      })
      
      observe({
        output$posterior_density_plots <- renderPlot({
          
          posterior_density_plots_obj()
          
        }, height = input$posterior_density_plot_dimension_slider_height,
           width  = input$posterior_density_plot_dimension_slider_width)
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


# BVM - table which displays observed data and study weights from model ("study-level outcomes") -----------  --------------------------------

MA_data_table_UI <- function(id) {
  ns <- NS(id)   
  DT::dataTableOutput(outputId =  ns("data_table"))
}



MA_data_table_settings_UI <- function(id) {
  ns <- NS(id)   
  downloadButton(outputId = ns("data_table_download"), label = "Download Table")
}



MA_data_table_server <- function(id, data, draws) {  # "mod" is the rstan file which is reactive. 
  moduleServer(
    id,
    function(input, output, session) {
      
      data_table_obj <- reactive({
        
                X <- data()
                model <- draws()
                
                b <- study_level_outcomes(X) # get sens, spec for each trial
                bb <- data.frame(Author=X$author, Year=X$year, TP=X$TP, FN=X$FN, FP=X$FP, 
                                 TN=X$TN, N=b$N, Sens=b$Sens, Spec=b$Spec)
                bb$Sens <- sprintf('%4.3f', bb$Sens) # restrict number of figures after decimal place for sens
                bb$Spec <- sprintf('%4.3f', bb$Spec)
                
                X <- MA_weights(X, model)
                bb$Weight_Sens <- sprintf('%4.3f', X$pctse)
                bb$Weight_Spec <- sprintf('%4.3f', X$pctsp)
                
                bb <- data.frame(bb)
                # Run the Garabage Collector to Ensure any excess memory used by stan is freed
                gc()
                return(bb)
                        
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
                  write.csv(data_table_obj(), file, sep=",", row.names=FALSE) 
                  }
      )
        
      
    }
    
    )
}

# BVM - Prior distribution options -------------------------------------------   --------------------------------------

# UI function to input prior distribution options 
MA_priors_options_inputModule_UI <- function(id) {
  ns <- NS(id)                  
  uiOutput(ns("priors_options"))
}

# server-side function to input prior dist. options - outputs a list of inputs which can be called within the shiny program
MA_priors_options_inputModule_renderUI_server <- function(id) {  
  
  moduleServer(
    id,
    function(input, output, session) {

      output$priors_options <- renderUI({
        
       ns <- session$ns
        
       div(id = ns("priors_options_div"), 
              if (input$p_scale_priors_indicator == FALSE) { 
                tagList(
                  h4("Prior Distributions:"),
                  numericInput(inputId =  ns("MA_prior_mean_sens_mu"), label=h5("Prior mean of pooled logit(sensitivity) - mean"), value = 0),
                  numericInput(inputId =  ns("MA_prior_mean_sens_sd"), label=h5("Prior mean of pooled logit(sensitivity) - SD"), value = 1.5, min = 0),
                  numericInput(inputId =  ns("MA_prior_mean_spec_mu"), label=h5("Prior mean of pooled logit(specificity) - mean"), value = 0),
                  numericInput(inputId =  ns("MA_prior_mean_spec_sd"), label=h5("Prior mean of pooled logit(specificity) - SD"), value = 1.5, min = 0),
                  numericInput(inputId =  ns("MA_prior_SD_sens_sd"), label=h5("Prior for between-study SD of logit(sensitivity) - SD"), value = 1, min = 0),
                  numericInput(inputId =  ns("MA_prior_SD_spec_sd"), label=h5("Prior for between-study SD of logit(specificity) - SD"), value = 1, min = 0)
                )
              } 
           else {
             tagList(
                  h5("Sensitivity - 95% credible interval:"),
                  numericInputRow(inputId =  ns("MA_prior_sens_lower95"), label=h5("Lower interval (2.5th percentile)"), value = 0.05, min = 0, max = 1),
                  numericInputRow(inputId =  ns("MA_prior_sens_upper95"), label=h5("Upper interval (97.5th percentile)"), value = 0.95, min = 0, max = 1),
                  h5("Specificity - 95% credible interval:"),
                  numericInputRow(inputId =  ns("MA_prior_spec_lower95"), label=h5("Lower interval (2.5th percentile)"), value = 0.05, min = 0, max = 1),
                  numericInputRow(inputId =  ns("MA_prior_spec_upper95"), label=h5("Upper interval (97.5th percentile)"), value = 0.95, min = 0, max = 1),
                  h5("Standard deviation (SD) priors:"),
                  numericInput(inputId =  ns("MA_prior_SD_sens_sd"), label=h5("Prior for between-study SD of logit(sensitivity) - SD"), value = 1, min = 0),
                  numericInput(inputId =  ns("MA_prior_SD_spec_sd"), label=h5("Prior for between-study SD of logit(specificity) - SD"), value = 1, min = 0)
               )
              }
                  )
      })
      
      observeEvent(input$reset,{
        shinyjs::reset("priors_options_div")
      })
              
      outputOptions(output, "priors_options", suspendWhenHidden = FALSE)
    }
  )
}


MA_priors_options_inputModule_server <- function(id) {  
  
  moduleServer(
    id,
    function(input, output, session) {
      
      vals <- reactiveValues()
      
      observe({
        
            if (input$p_scale_priors_indicator == FALSE) { 
              
                    observe({vals$MA_prior_mean_sens_mu <- input$MA_prior_mean_sens_mu})
                    observe({vals$MA_prior_mean_sens_sd <- input$MA_prior_mean_sens_sd})
                    observe({vals$MA_prior_mean_spec_mu <- input$MA_prior_mean_spec_mu})
                    observe({vals$MA_prior_mean_spec_sd <- input$MA_prior_mean_spec_sd})
                    observe({vals$MA_prior_SD_sens_sd <- input$MA_prior_SD_sens_sd})
                    observe({vals$MA_prior_SD_spec_sd <- input$MA_prior_SD_spec_sd})
            
            }
            else {
                    observe({vals$MA_prior_sens_lower95 <- input$MA_prior_sens_lower95})
                    observe({vals$MA_prior_sens_upper95 <- input$MA_prior_sens_upper95})
                    observe({vals$MA_prior_spec_lower95 <- input$MA_prior_spec_lower95})
                    observe({vals$MA_prior_spec_upper95 <- input$MA_prior_spec_upper95})
                    observe({vals$MA_prior_SD_sens_sd <- input$MA_prior_SD_sens_sd})
                    observe({vals$MA_prior_SD_spec_sd <- input$MA_prior_SD_spec_sd})
              
            }
        
      })
      # Run the Garabage Collector to Ensure any excess memory used by stan is freed
      gc()
      return(vals)
      

    }
  )
}













# BVM - Parameter estimates table ---------------------------------------------------  ----------------------------------------


# UI function for parameter estimates table 
MA_parameter_estimates_table_UI <- function(id) {
  ns <- NS(id)  
  tagList(
    tableOutput(ns("table")),
    downloadButton(ns("download_table"), "Download Table")
  )
}

# UI function to output the options for summary results/ parameter estimates table (to let the user hide specific rows of the table)
MA_parameter_estimates_table_menu_UI <- function(id) {
  ns <- NS(id)   
  checkboxGroupInput(inputId= ns("statscheck"), 
                     label=h4("Options"),
                     choices=list("Sensitivity"=1, "Specificity"=2, "False Positive Rate"=3, "Between-study Correlation"=4,
                                  "Between-Study SD"=5, "HSROC parameters"=6, "Diagnostic Odds Ratio"=7, "Likelihood Ratios"=8),
                     selected=list(1,2,3,4,5,6,7,8)) 
}


# Server function for parameter estimates table 
MA_parameter_estimates_table_server <- function(id, 
                                                data, 
                                                draws) {  
  
  moduleServer(
    id,
    function(input, output, session) {
      
      table_obj <- reactive({
        
        req(data(),  draws(), cancelOutput = TRUE)
        
        mod <- draws()
        
        logit_Sens <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("lSe"))$summary
        logit_Spec <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("lSp"))$summary
        Sens <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se"))$summary
        Spec <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Sp"))$summary
        correlation <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Omega"))$summary
        DOR <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("DOR"))$summary
        LRp <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("LRp"))$summary
        LRn <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("LRn"))$summary
        Theta <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("theta"))$summary
        Lambda <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("lambda"))$summary
        beta <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("beta"))$summary
        sigma2_theta <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sigma_sq_theta"))$summary
        sigma2_alpha <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sigma_sq_alpha"))$summary
        between_study_sd <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sigma"))$summary
        
        Z = data.frame(estimate = c(logit_Sens[,5], logit_Spec[,5], 
                                    Sens[,5], Spec[,5],
                                    1 - Spec[,5], 
                                    DOR[,5], LRp[,5] , LRn[,5],
                                    correlation[,5][3], between_study_sd[,5][1], between_study_sd[,5][2], 
                                    Theta[,5], Lambda[,5], beta[,5], sigma2_theta[,5], sigma2_alpha[,5]),
                       lci = c(logit_Sens[,4], logit_Spec[,4], 
                               Sens[,4], Spec[,4],
                               1 - Spec[,6], 
                               DOR[,4], LRp[,4] , LRn[,4], 
                               correlation[,4][3], between_study_sd[,4][1], between_study_sd[,4][2],
                               Theta[,4], Lambda[,4], beta[,4], sigma2_theta[,4], sigma2_alpha[,4]),
                       uci = c(logit_Sens[,6], logit_Spec[,6], 
                               Sens[,6], Spec[,6],
                               1 - Spec[,4], 
                               DOR[,6], LRp[,6] , LRn[,6], 
                               correlation[,6][3], between_study_sd[,6][1], between_study_sd[,6][2],
                               Theta[,6], Lambda[,6], beta[,6], sigma2_theta[,6], sigma2_alpha[,6]),
                       row.names=c("Logit Sens", "Logit Spec", 
                                   "Sensitivity", "Specifcity",
                                   "FPR", 
                                   "DOR", "LR+" , "LR-", 
                                   "Correlation", "sd_sens", "sd_spec", 
                                   "Theta", "Lambda", "beta",  "sigma2_theta", "sigma2_alpha")
        )
        
        # Create a matrix to store the parameter estimates
        nrow <- 17
        s.matrix <- matrix(nrow=nrow, ncol=5)
        
        s.matrix[1,1] <-  paste0("logit(sensitivity) ", "( ", HTML("&mu;<sub>1</sub>"), " )")
        s.matrix[2,1] <-  paste0("logit(specificity) ", "( ", HTML("&mu;<sub>0</sub>"), " )")
        s.matrix[3,1] <-  paste0("Sensitivity ", "( ", paste0("logit", HTML("<sup>-1</sup>")), "(", HTML("&mu;<sub>1</sub>"), ")",  ")")
        s.matrix[4,1] <-  paste0("Specificity ", "( ", paste0("logit", HTML("<sup>-1</sup>")), "(", HTML("&mu;<sub>0</sub>"), ")",  ")")
        s.matrix[5,1] <- "False Positive Rate (1 - Specificity)"
        s.matrix[6,1] <- "Diagnostic Odds Ratio"
        s.matrix[7,1] <- "Likelihood Ratio +ve"
        s.matrix[8,1] <- "Likelihood Ratio -ve"
        s.matrix[9,1] <-  paste0("Between-study Correlation" , "( ", HTML("&rho;"), " )")
        s.matrix[10,1] <- paste0("Between-study SD for logit(Sensitivity) ", "( ", HTML("&sigma;<sub>1</sub>"), " )")
        s.matrix[11,1] <- paste0("Between-study SD for logit(Specificity) ", "( ", HTML("&sigma;<sub>0</sub>"), " )")
        # HSROC model parameters 
        s.matrix[12,1] <- paste0("Cutpoint parameter ", "( ", HTML("&Theta;"), " )")
        s.matrix[13,1] <- paste0("Accuracy parameter ", "( ", HTML("&Lambda;"), " )")
        s.matrix[14,1] <- paste0("Shape parameter ", "( ", HTML("&beta;"), " )")
        s.matrix[15,1] <- paste0("SD of cutpoint parameter ", "( ", HTML("&sigma;<sub>&theta;</sub>"), " )")
        s.matrix[16,1] <- paste0("SD of accuracy parameter ", "( ", HTML("&sigma;<sub>&alpha;</sub>"), " )")
        
        
        for (i in 1:3) {
          for (j in 1:(nrow)-1) { 
            s.matrix[j,i+1] <- sprintf('%4.3f', Z[j,i])
          }
        }
        
        s.matrix[,5] <- paste0("(", s.matrix[,3], ", ", s.matrix[,4], ")")
        s.matrix[nrow, 1:5] <- ""
        s.matrix <- s.matrix[, c(1,2,5)]
        
        #Name the columns of the matrix
        colnames(s.matrix) <- c("Parameter", "Posterior Median", "95% Posterior Interval")
        
        #Conditions to display which statistics are shown in the table
        #Start with a logical vector of false and replace ths with true if the corresponding box is ticked
        statticks <- logical(length=nrow) # default is false
        
        statticks[nrow] <- TRUE # always have the bottom empty row showing
        
        # which rows are displayed will depend on the options selected
        if ('1' %in% input$statscheck) {statticks[1] <- TRUE}  # sens
        if ('2' %in% input$statscheck) {statticks[2] <- TRUE}  # spec
        if ('1' %in% input$statscheck) {statticks[3] <- TRUE}
        if ('2' %in% input$statscheck) {statticks[4] <- TRUE}
        if ('3' %in% input$statscheck) {statticks[5] <- TRUE}  # fp
        if ('4' %in% input$statscheck) {statticks[9] <- TRUE}  # between-study corr
        if ('5' %in% input$statscheck) {statticks[10:11] <- TRUE}  # between-study SD
        if ('6' %in% input$statscheck) {statticks[12:16] <- TRUE} # HSROC model parameters
        if ('7' %in% input$statscheck) {statticks[6] <- TRUE} # DOR
        if ('8' %in% input$statscheck) {statticks[7:8] <- TRUE} # Likelihood ratios
        
        
        #Only the rows of s.matrix where statticks=T will be displayed
        s.matrix <- s.matrix[statticks,]
        # Run the Garabage Collector to Ensure any excess memory used by stan is freed
        gc()
        return(s.matrix)
        
      })
      
      # output table 
      output$table <- renderTable({
        
        req(data(), draws(), cancelOutput = TRUE)
        
        table_obj()
        
      }, sanitize.text.function = function(x) x)
      
      # download table
      output$download_table <- downloadHandler(
        filename = "parameter_estimates_table.csv", 
        content = function(file) {
          write.csv(table_obj(), file, row.names = TRUE)
        }
      )
      
    }
  )    
}





# BVM - R-hat statistics table ------------------------------------------------  ----------------------------------------------------------

# UI function for R-hat table 
MA_rhat_table_UI <- function(id) {
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
MA_rhat_table_server <- function(id, 
                                 draws) {  
  
  moduleServer(
    id,
    function(input, output, session) {
      
      rhats_obj <- reactive({
        
        mod <- req(draws())
        
        pars <-  c( "Se", "Sp",
                    "Omega", "Sigma", 
                    "se", "sp")
        
        rhats <- round(summary(mod, 
                               probs = c(0.025,  0.5, 0.975), 
                               pars = pars)$summary[,8], 3)
        
        rhats_2 <- tibble(parameter = names(rhats), 
                          r_hat = rhats, 
                          category = case_when( r_hat < 1.05 ~ "good", 
                                                r_hat %in% c(1.05, 1.10) ~ "borderline", 
                                                r_hat > 1.10 ~ "bad")) %>% 
          dplyr::filter(r_hat != "NaN")
        
        rhats_2
        
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
        
        options(DT.options = list(   autoWidth = TRUE, 
                                     pageLength = n_bad + n_borderline + 10))
        
        rhat_table
        
      })
      
      
      # download table
      output$download_rhat_table <- downloadHandler(
        filename = "rhat_statistics_table.csv", 
        content = function(file) { 
          write.csv(rhats_obj(), file, sep=",", row.names=FALSE) 
        } 
      )
      
      
    }
  )    
}


























