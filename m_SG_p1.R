
# Model diagnostics tab  - UI ("master") module  ---------------------------------- ---------------------------------------
SG_model_diagnostics_tab_UI <- function(id) {
  ns <- NS(id)  
  tagList( 
    sampler_diagnostics_table_UI(id = "SG_model_id"),
    br(),
    h3("R-hat statistics"), 
    SG_rhat_table_UI(id = "SG_model_id"), 
    br(), 
    h3("Posterior density plots"),
    dropdownButton(
      model_posterior_density_plots_settings_UI(id = "SG_model_id"),
      circle = TRUE, status = "danger",
      icon = icon("gear"), width = "300px",
      tooltip = tooltipOptions(title = "Click to customise plot")
    ),
    model_posterior_density_plots_UI(id = "SG_model_id"),
    br(),
    h3("Trace plots"),
    dropdownButton(
      SG_model_trace_plots_settings_UI(id = "SG_model_id"),
      circle = TRUE, status = "danger",
      icon = icon("gear"), width = "300px",
      tooltip = tooltipOptions(title = "Click to customise plot")
    ),
    SG_model_trace_plots_UI(id = "SG_model_id")
  )
}


# UI function to select subgroup
SG_input_UI <- function(id) {
  ns <- NS(id) 
    uiOutput(ns("covariate_subgroup_ui"))
}


# covariate to use for SG analysis 
SG_covariate_model_server <- function(id, 
                                      data) {  
  moduleServer(
    id,
    function(input, output, session) {
      
      X <- reactive({req(data())})
      
      C <- reactive({ncol(X())})
      Names <- reactive({colnames(X())})
      
      
      j <- reactive({
        if (C() > 8 & Names()[7] != "rob_PS") { 
          j <- 6
        } else {
          j <- 13  
        }
        return(j)
      })
      
      
      
      choicesCov_obj <- reactive({
                    initial <- c("None")
                    
                    if (( C() > 8  & Names()[7] != "rob_PS") | 
                        ( C() > 15 & Names()[13] == "ac_RS")) {
                          
                          if (C() > 8 & Names()[7] != "rob_PS") {
                            if (C() == 9) {
                              covariates <- colnames(X()[7])
                            }
                            else {
                              covariates <- colnames(X()[,7:C()])
                            }
                          }
                          if (C() > 15 & Names()[13] == "ac_RS") {
                            if (C() == 16) {
                              covariates <- colnames(X()[14])
                            }
                            else {
                              covariates <- colnames(X()[,14:C()])
                            }
                          }
                          combined <- c(initial, covariates)
                          number <- 1:length(combined)
                          choicesCov <- setNames(number, combined)
                          choicesCov
                        }
                        else { }
      })
      
      output$covariate_subgroup_ui <- renderUI({ 
        
        validate(
          need(j() %in% c(6,13), "Please select a dataset with covariates to use meta-regression")
        )
        
        
            ns <- session$ns
            
            selectInput(inputId = ns("covariate_subgroup"), 
                          label= "Please select subgroup", 
                          choices = choicesCov_obj(),
                          selected = 2)
        
        })

      output$progress_prior_model_ui <- renderUI({ 
        ns <- session$ns
        verbatimTextOutput(ns("progress_prior_model"))
      })
      
      output$progress_main_model_ui <- renderUI({ 
        ns <- session$ns
        verbatimTextOutput(ns("progress_main_model"))
      })

    }
  )
}




# prior distribution options  ------------------------------------------------- ----------------------------------------

SG_priors_options_UI <- function(id) {
  ns <- NS(id)                  
  uiOutput(ns("priors_options"))
}

# server-side function to input prior dist. options 
SG_priors_options_server <- function(id, data) {  
  moduleServer(
    id,
    function(input, output, session) {

      X <- reactive({req(data())})
      C <-reactive({ ncol(X()) })
      Names <- reactive({ colnames(X()) })
      
     
      
      priors_options_toggle_obj <- reactive({
        
                req(data(), input$covariate_subgroup)
                
                    if (C() > 8 & Names()[7] != "rob_PS") { 
                      j <<- 6 
                    } else { 
                        j <<- 13  
                    }
                
                       if ( (str_sub(colnames(X())[j +  as.integer(as.double(input$covariate_subgroup)) - 1  ], start = -3) != 'cts') &&
                            is.null(input$covcheck_model) != FALSE ) {
                         cov_cts_indicator <<- 0
                       }
                
                       else {
                         cov_cts_indicator <<- 1
                       }
                
                     return(cov_cts_indicator)
      })
      
      reset_obj <- reactive({
                  input$reset
      })
      
      output$priors_options <- renderUI({
        
                ns <- session$ns
                
                   if (( C() > 8 & Names()[7] != "rob_PS") | (C() > 15 & Names()[13] == "ac_RS")) {
                        if (C() > 8 & Names()[7] != "rob_PS") {
                            j <<- 6 
                        } else {
                            j <<- 13  
                        }
                   } else {
                     j <<- 0
                   }
                        
                  validate(
                    need( (j %in% c(6,13) ) ,
                          "To use subgroup analysis, please select a dataset which contains categorical or discrete covariates")
                  )
                  
                  validate(
                    need( priors_options_toggle_obj() == 0  ,
                          "To use subgroup analysis, please select a categorical covariate")
                  )
        
                  reset <- reset_obj()
                  
                  div(id = ns("priors_options_div"), 
                      if (input$p_scale_priors_indicator == FALSE) { 
                      tagList(
                          h4("Prior Distributions (For each subgroup):"),
                          numericInput(inputId = ns("SG_prior_mean_sens_mu"), label=h5("Prior mean of pooled sensitivities - mean"), value = 0),
                          numericInput(inputId = ns("SG_prior_mean_sens_sd"), label=h5("Prior mean of pooled sensitivities - SD"), value = 1.5),
                          numericInput(inputId = ns("SG_prior_mean_spec_mu"), label=h5("Prior mean of pooled specificities - mean"), value = 0),
                          numericInput(inputId = ns("SG_prior_mean_spec_sd"), label=h5("Prior mean of pooled specificities - SD"), value = 1.5),
                          numericInput(inputId = ns("SG_prior_SD_sens_sd"), label=h5("Prior for between-study SD of logit(sensitivity) - SD"), value = 1),
                          numericInput(inputId = ns("SG_prior_SD_spec_sd"), label=h5("Prior for between-study SD of logit(specificity) - SD"), value = 1)
                          )
                      }
                      else {
                        tagList(
                          h4("Prior Distributions (For each subgroup):"),
                          h5("Sensitivity - 95% credible interval:"),
                          numericInputRow(inputId =  ns("SG_prior_sens_lower95"), label=h5("Lower interval (2.5th percentile)"), value = 0.05, min = 0, max = 1),
                          numericInputRow(inputId =  ns("SG_prior_sens_upper95"), label=h5("Upper interval (97.5th percentile)"), value = 0.95, min = 0, max = 1),
                          h5("Specificity - 95% credible interval:"),
                          numericInputRow(inputId =  ns("SG_prior_spec_lower95"), label=h5("Lower interval (2.5th percentile)"), value = 0.05, min = 0, max = 1),
                          numericInputRow(inputId =  ns("SG_prior_spec_upper95"), label=h5("Upper interval (97.5th percentile)"), value = 0.95, min = 0, max = 1),
                          h5("Standard deviation (SD) priors:"),
                          numericInput(inputId = ns("SG_prior_SD_sens_sd"), label=h5("Prior for between-study SD of logit(sensitivity) - SD"), value = 1),
                          numericInput(inputId = ns("SG_prior_SD_spec_sd"), label=h5("Prior for between-study SD of logit(specificity) - SD"), value = 1)
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



# Plot of prior distributions  --------------------------------------------------- ---------------------------------------

# server function
SG_model_priors_plot_server <- function(id, draws_PO, data) {  # "mod" is the rstan file which is reactive. 
  moduleServer(
    id,
    function(input, output, session) {
      
      
      priors_plot_obj <-  reactive({ # can just refer to the plot name ("model_priors_plot") without using "ns" here in the server function. 
                                        
                req(data(), draws_PO(), input$covariate_subgroup, cancelOutput = TRUE)
                
                params <- rstan::extract(draws_PO())
                X <- data()
                N <- nrow(X)
                cov_index <-  as.integer(as.double(input$covariate_subgroup)) - 1
                
                C <- ncol(X)
                Names <- colnames(X)
                
                if (C > 8 & Names[7] != "rob_PS") { j <<- 6 } else { j <<- 13  }
                
                n_samps <- (2000 - 200)*4
                
                if (cov_index == 0) {
                  print("Please select a covariate")
                } else {
                  if (str_sub(colnames(X)[j + cov_index], start = -3) != "cts") { # categorical / discrete meta-reg
                    
                    se <- params$Se[, 1] # chose 1st group (since priors are the same for each subgroup / discrete covariate)
                    sp <- params$Sp[, 1] # chose 1st group (since priors are the same for each subgroup / discrete covariate)
                    corr <- params$Omega[, 1 , 1, 2]
                    sd_se <- params$sigma[, 1, 1]
                    sd_sp <- params$sigma[, 1, 2]
                    
                    data <- tibble(
                      Samples = c(se, sp, corr, sd_se, sd_sp), 
                      Parameter = c(rep("Sensitivity (for each subgroup)", n_samps), 
                                    rep("Specificity (for each subgroup)", n_samps), 
                                    rep("Between-study correlation (for each subgroup)", n_samps), 
                                    rep("Between-study SD for logit(sensitivity) (for each subgroup)", n_samps), 
                                    rep("Between-study SD for logit(specificity) (for each subgroup)", n_samps)))
                    
                    g <-    ggplot(data = data, aes(x=Samples)) + 
                      geom_density(alpha = 0.50) + 
                      theme_bw() + 
                      theme(legend.position = "none") + 
                      facet_wrap( ~ Parameter, scales = "free") + 
                      xlab(" ") + 
                      ylab(" ") 
                    
                    g
                    
                  } 
                            
                }
                
      })
      
      C <- reactive({ 
        ncol(data()) 
        })
      
      Names <- reactive({ 
        colnames(data()) 
        })
      
      
      
      
      priors_options_toggle_obj <- reactive({
        
        req(data(), input$covariate_subgroup)
        
        if (C() > 8 & Names()[7] != "rob_PS") { j <<- 6 } else { j <<- 13  }
        
        if ( (str_sub(colnames(X())[j +  as.integer(as.double(input$covariate_subgroup)) - 1  ], start = -3) != 'cts') &&
             is.null(input$covcheck_model) != FALSE ) {
          cov_cts_indicator <<- 0
        }
        
        else {
          cov_cts_indicator <<- 1
        }
        
        return(cov_cts_indicator)
      })
      
      
      # output plot 
      observe({
            output$model_priors_plot <- renderPlot({
              
              if (( C() > 8 & Names()[7] != "rob_PS") | (C() > 15 & Names()[13] == "ac_RS")) {
                    if (C() > 8 & Names()[7] != "rob_PS") {
                      j <<- 6   
                    } else {
                      j <<- 13  
                    }
              }
              else {
                j <<- 0
              }
              
              validate(
                need(!(is.null(draws_PO())), "Please run prior model to display plot")
              )
              
              validate(
                need( (j %in% c(6,13)),
                      "To use subgroup analysis, please select a dataset which contains categorical or discrete covariates")
              )
              
              validate(
                need( priors_options_toggle_obj() == 0,
                      "To use subgroup analysis, please select a categorical covariate")
              )
              
              req(data(), draws_PO(), cancelOutput = TRUE)
              
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



# Table of prior distributions  ---------------------------------------------------------------- -------------------------------

# UI function
SG_model_priors_table_UI <- function(id) {
  ns <- NS(id)   
  tagList(
    tableOutput(outputId =  ns("model_priors_table")),
    p("NOTE: The default priors are:"),  
    p("For logit sensitivities and specificities - normal distribution with mean 0 and SD of 1, equivalent to a 95% prior interval of (0.05, 0.95) on the probability scale"),
    p("For between-study SD's of logit sensitivities and specificities - truncated (at 0) normal distribution with mean 0 and SD of 1, equivalent to a 95% prior interval of (0.03, 2.25) "),
    p("For between-study correlation between study-specific logit sensitivities and specificities - LKJ(2) prior, equivalent to 95% prior interval of (-0.8, 0.8)")
  )
}

# server function
SG_model_priors_table_server <- function(id, draws_PO, data) {  
  
  
  moduleServer(
    id,
    function(input, output, session) {
      
      
      C <- reactive({ 
        ncol(data()) 
      })
      
      Names <- reactive({ 
        colnames(data()) 
      })
      

      
      
      
      priors_options_toggle_obj <- reactive({
        
        req(data(), input$covariate_subgroup)
        
        if (C() > 8 & Names()[7] != "rob_PS") { j <<- 6 } else { j <<- 13  }
        
        if ( (str_sub(colnames(X())[j +  as.integer(as.double(input$covariate_subgroup)) - 1  ], start = -3) != 'cts') &&
             is.null(input$covcheck_model) != FALSE ) {
          cov_cts_indicator <<- 0
        }
        
        else {
          cov_cts_indicator <<- 1
        }
        
        return(cov_cts_indicator)
      })
      
      output$model_priors_table <- renderTable({
      
                      validate(
                        need(!(is.null(draws_PO())), "Please run prior model to display table")
                      )
                      
                      
                      validate(
                        need( (j %in% c(6,13) ) ,
                              "To use subgroup analysis, please select a dataset which contains categorical or discrete covariates")
                      )
                      
                      validate(
                        need( priors_options_toggle_obj() == 0  ,
                              "To use subgroup analysis, please select a categorical covariate")
                      )
                      
                          
                          if (( C() > 8 & Names()[7] != "rob_PS") | (C() > 15 & Names()[13] == "ac_RS")) {
                                if (C() > 8 & Names()[7] != "rob_PS") {
                                  j <<- 6   
                                } else {
                                  j <<- 13  
                                }
                              }
                          else {
                            j <<- 0
                          }
                          
                          
                      req(data(), draws_PO(), cancelOutput = TRUE)
                      
                      mod <- draws_PO()
                      X <- data()
                      N <- nrow(X)
                      
                      cov_index <-  as.integer(as.double(input$covariate_subgroup)) - 1
                      
                      C <- ncol(X)
                      Names <- colnames(X)
                      
                      if (C > 8 & Names[7] != "rob_PS") { j <<- 6 } else { j <<- 13  }
                      
                      num_levels <- length(rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se"))$summary[,5])
                      
                    
                      
                      if (cov_index == 0) {
                        print("Please select a covariate")
                      } else {
                                if (str_sub(colnames(X)[j + cov_index], start = -3) != "cts") { # categorical only
                                  
                                  
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
                                                 row.names=c("Sensitivity", "Specifcity",
                                                             "FPR", 
                                                             "Correlation",
                                                             "sd_sens", "sd_spec")
                                  )
                                  
                                  # Create a matrix to store the parameter estimates
                                  nrow <- 7
                                  s.matrix <- matrix(nrow=nrow, ncol=5)
                                  
                                  
  
                                  s.matrix[1,1] <-  paste0("Sensitivity [same prior across subgroups]", "( ", paste0("logit", HTML("<sup>-1</sup>")), "(", HTML("&mu;<sub>1</sub>"), ")",  ")")
                                  s.matrix[2,1] <-  paste0("Specificity [same prior across subgroups]", "( ", paste0("logit", HTML("<sup>-1</sup>")), "(", HTML("&mu;<sub>0</sub>"), ")",  ")")
                                  s.matrix[3,1] <-  "False positive rate (1 - specificity) [same prior across subgroups]"
                                  s.matrix[4,1] <-  paste0("Between-study Correlation [same prior across subgroups] " , "( ", HTML("&rho;"), " )")
                                  s.matrix[5,1] <-  paste0("Between-study SD for logit(Sensitivity) [same prior across subgroups] ", "( ", HTML("&sigma;<sub>1</sub>"), " )")
                                  s.matrix[6,1] <-  paste0("Between-study SD for logit(Specificity) [same prior across subgroups] ", "( ", HTML("&sigma;<sub>0</sub>"), " )")
                                  
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
                              }
                            }, sanitize.text.function = function(x) x)
      
    }
  )
}


# Trace plots ------------------------------------------- ---------------------------------------------------------------------------------------------
SG_model_trace_plots_UI <- function(id) {
  ns <- NS(id)  
  plotOutput(outputId =  ns("trace_plots"))
}

SG_model_trace_plots_settings_UI <- function(id) {
  ns <- NS(id)  
  tagList(
    numericInput(ns("trace_plot_nrow"), 
                 label = "Number of rows",
                 value = 10),
    numericInput(ns("trace_plot_ncol"), 
                 label = "Number of columns",
                 value = 10),
    sliderInput(inputId = ns("trace_plot_dimention_slider_height"), 
                label = "Change size of plot - height", 
                min = 1, max = 2000, 
                value = 300),
    sliderInput(inputId = ns("trace_plot_dimention_slider_width"), 
                label = "Change size of plot - width", 
                min = 1, max = 2000, 
                value = 500),
    # Download plot:
    h5("Download plot:"),
    numericInput(inputId =  ns("trace_plot_dl_width"), label=h5("Plot width"), value = 10),
    numericInput(inputId =  ns("trace_plot_dl_height"), label=h5("Plot height"), value = 5),
    numericInput(inputId =  ns("trace_plot_dl_dpi"), label=h5("Plot DPI"), value = 600),
    downloadButton(outputId = ns("download_trace_plot"), label = "Download Plot")
  )
}



SG_model_trace_plots_server <- function(id, draws, data) {  # "mod" is the rstan file which is reactive. 
  moduleServer(
    id,
    function(input, output, session) {
      
                     trace_plots_obj <- reactive({
                       
                                     req(data(), draws(), input$covariate_subgroup, cancelOutput = TRUE)
             
                                      X <- data()
                                      cov_index <-  as.integer(as.double(input$covariate_subgroup)) - 1
                                      
                                      C <- ncol(X)
                                      Names <- colnames(X)
                                      
                                      if (C > 8 & Names[7] != "rob_PS") { j <<- 6 } else { j <<- 13  }
                                      
                                      mod <- draws()
                                      
                                      if (cov_index == 0) {
                                        print("Please select a covariate")
                                      }
                                      
                                      if (cov_index != 0) {
                                        if (str_sub(colnames(X)[j + cov_index], start = -3) != "cts") { # categorical / discrete meta-reg
  
                                        plot <-   stan_trace(mod, 
                                                     nrow = input$trace_plot_nrow, 
                                                     ncol = input$trace_plot_ncol,
                                                     pars = c("Se", "Sp", "Sigma", "se", "sp")) 
                                        }
                                        else { 
                                        }
                                      }
                                      
                                    plot
                                    
                        })
                     
                     observe({
                     output$trace_plots <- renderPlot({
                                                      trace_plots_obj()
                                           }, height = input$trace_plot_dimention_slider_height,
                                              width  = input$trace_plot_dimention_slider_width)
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

# Posterior density plots  ------------------------------------------------------------- -----------------------------------------------------------------

SG_model_posterior_density_plots_UI <- function(id) {
  ns <- NS(id)  
  plotOutput(outputId =  ns("posterior_density_plots"))
}

SG_model_posterior_density_plots_settings_UI <- function(id) {
  ns <- NS(id)  
  tagList(
    numericInput(ns("posterior_density_plot_nrow"), 
                 label = "Number of rows",
                 value = 10),
    numericInput(ns("posterior_density_plot_ncol"), 
                 label = "Number of columns",
                 value = 10),
    sliderInput(inputId = ns("posterior_density_plot_dimention_slider_height"), 
                label = "Change size of plot - height", 
                min = 1, max = 2000, 
                value = 300,
                ticks = FALSE),
    sliderInput(inputId = ns("posterior_density_plot_dimention_slider_width"), 
                label = "Change size of plot - width", 
                min = 1, max = 2000, 
                value = 500,
                ticks = FALSE),
    # Download plot:
    h5("Download plot:"),
    numericInput(inputId =  ns("dens_plot_dl_width"), label=h5("Plot width"), value = 10),
    numericInput(inputId =  ns("dens_plot_dl_height"), label=h5("Plot height"), value = 5),
    numericInput(inputId =  ns("dens_plot_dl_dpi"), label=h5("Plot DPI"), value = 600),
    downloadButton(outputId = ns("download_dens_plot"), label = "Download Plot")
  )
}



SG_model_posterior_density_plots_server <- function(id, draws, data) {  # "mod" is the rstan file which is reactive. 
  moduleServer(
    id,
    function(input, output, session) {
                    posterior_density_plots_obj <- reactive({
                                  req(data(), draws(), input$covariate_subgroup, cancelOutput = TRUE)
                                  
                                  X <- data()
                                  cov_index <-  as.integer(as.double(input$covariate_subgroup)) - 1
                                  
                                  C <- ncol(X)
                                  Names <- colnames(X)
                                  
                                  if (C > 8 & Names[7] != "rob_PS") { j <<- 6 } else { j <<- 13  }
                                  
                                  mod <- draws()
                                  
                                  if (cov_index == 0) {
                                    print("Please select a covariate")
                                  }
                                  
                                  if (cov_index != 0) {
                                    if (str_sub(colnames(X)[j + cov_index], start = -3) != "cts") { # categorical / discrete meta-reg
                                      plot <- stan_dens(mod, 
                                                        nrow = input$posterior_density_plot_nrow, 
                                                        ncol = input$posterior_density_plot_ncol,
                                                        pars = c("Se", "Sp", "Sigma", "se", "sp"))
                                    }
                                    else { }
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

# Table which displays observed data and study weights from model (study-level outcomes) ------------------- -----------------------------------------
SG_data_table_UI <- function(id) {
  ns <- NS(id)   
  DT::dataTableOutput(outputId =  ns("data_table"))
}

SG_data_table_settings_UI <- function(id) {
  ns <- NS(id)   
  downloadButton(outputId = ns("data_table_download"), label = "Download Table")
}



SG_data_table_server <- function(id, data, draws) {  # "mod" is the rstan file which is reactive. 
  moduleServer(
    id,
    function(input, output, session) {

      data_table_obj <- reactive({
                          
                      X <- data()
                      mod <- draws()
                      cov_index <-  as.integer(as.double(input$covariate_subgroup)) - 1
                      
                      N <- nrow(X)
                      
                      C <- ncol(X)
                      Names <- colnames(X)
                      
                      if (C > 8 & Names[7] != "rob_PS") { j <<- 6 } else { j <<- 13  }
                      
                      if (cov_index == 0) {
                        print("Please select a covariate")
                      }
                      
                      if (cov_index != 0) {
                        if (str_sub(colnames(X)[j + cov_index], start = -3) != "cts") { # categorical / discrete meta-reg
              
                          num_levels <- length(rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se"))$summary[,5])
                          
                          # add study weights 
                          X <- SG_weights(X, mod, cov_index)
                          
                          # observed values
                          ss <- tibble( 
                            Study =as.numeric(as.factor(X$author)), 
                            !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)) := factor(X[, j + cov_index]),
                            TP=X$TP, FN=X$FN, FP=X$FP, TN=X$TN,
                            N=(X$TP+X$FN+X$FP+X$TN) ,
                            Sensitivity= (TP/(TP+FN))  , 
                            Specificity= (TN/(TN+FP))  , 
                            prev = round((TP+FN)/N, 2), 
                            pctse = X$pctse$pctse,
                            pctsp = X$pctsp$pctsp
                          )
              
                          ss2 <- left_join(ss, X, by = c("TP", "FN", "FP", "TN")) 
                          
                          no_covariates <- length(colnames(X[,(j+1):C])) # total number of covariates       
                          
                          # make cols without .cts and .cat endings for display
                          for (i in 1:no_covariates) { 
                            ss2 <- dplyr::mutate(ss2,
                                                 !!as.name( str_sub(colnames(X)[j + i], end = -5) ) := !!as.name( colnames(X)[j + i]) )
                          }
                          
                          ss2 <- dplyr::mutate(ss2, 
                                               Weight_Sens = round(pctse.y$pctse, 2), 
                                               Weight_Spec = round(pctsp.y$pctsp, 2),
                                               Sensitivity = round(Sensitivity, 2), 
                                               Specificity = round(Specificity, 2))
                          
                         
                          # select columns to display
                          cols <- c("Study", "author", "year", str_sub( colnames(X)[c((j+1):(j+no_covariates))] , end = -5), 
                                    "Sensitivity", "Specificity", "Weight_Sens", "Weight_Spec")
                          
                          # arrange data by the selected covariate
                          data <- dplyr::select(ss2, cols) %>% 
                            dplyr::arrange( !!as.name( str_sub(colnames(X)[j + cov_index], end = -5) ) ) %>%
                            dplyr::mutate(Weight_Sens = ifelse(Weight_Sens == 100, "-", Weight_Sens), 
                                          Weight_Spec = ifelse(Weight_Spec == 100, "-", Weight_Spec))
                          data <- data.frame(data)
                          return(data)
                          

                          
                        }
                        
                        else { }
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
                                              write.csv(data_table_obj(), file, sep=",", row.names=FALSE) 
                                            }
      )
      
    }
  )
}



# Parameter estimates table  --------------------------------------------------- ---------------------------------------------------------
# UI function for parameter estimates table 
SG_parameter_estimates_table_UI <- function(id) {
  ns <- NS(id)  
  tagList(
    tableOutput(ns("table")),
    downloadButton(ns("download_table"), "Download Table")
  )
}


# Server function for parameter estimates tables
SG_parameter_estimates_table_server <- function(id, 
                                                 data, 
                                                 draws) {  
  
  moduleServer(
    id,
    function(input, output, session) {
      
          table_obj <- reactive({
            
                        req(data(), draws(), input$covariate_subgroup, cancelOutput = TRUE)
                        
                        
                        mod <- draws()
                        cov_index <-  as.integer(as.double(input$covariate_subgroup)) - 1
                        
                        X <- data()

                        C <- ncol(X)
                        Names <- colnames(X)
                        
                        if (C > 8 & Names[7] != "rob_PS") { m <<- 6 } else { m <<- 13  }
                        
                        num_levels <- length(rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se"))$summary[,5])
                        
                        
                        if (cov_index != 0) {
                          if (str_sub(colnames(X)[j + cov_index], start = -3) != "cts") {  # cat only 
                            
                            logit_Sens <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("lSe"))$summary
                            logit_Spec <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("lSp"))$summary
                            Sens <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se"))$summary
                            Spec <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Sp"))$summary
                            DOR <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("DOR"))$summary
                            LRp <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("LRp"))$summary
                            LRn <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("LRn"))$summary
                            Theta <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("theta"))$summary
                            Lambda <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("lambda"))$summary
                            correlation <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Omega"))$summary
                            beta <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("beta"))$summary
                            sigma2_theta <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sigma_sq_theta"))$summary
                            sigma2_alpha <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sigma_sq_alpha"))$summary
                            between_study_sd <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sigma"))$summary
                            
                            # identify subgroups w/ N = 1
                            X2 <- dplyr::mutate(X, numeric_cov = as.numeric(as.factor(!!as.name(colnames(X)[m + cov_index]))))
                            X_counts <- X2 %>% 
                              group_by(numeric_cov) %>%
                              summarise(no_rows = length(numeric_cov))
                            
                            X_counts_one <- filter(X_counts, no_rows != 1)
                            X_counts_one_df <- data.frame(X_counts_one)
                            vec <- X_counts_one_df[,1] # vector of factor levels s.t. N > 1
                            
                            Z_group <- list()
                            
                            # make vector which indicates the groups s.t. #(studies in group) > 1
                            
                            for (i in vec) {
                              Z_group[[i]] <- data.frame(estimate = c(logit_Sens[,5][i], 
                                                                      logit_Spec[,5][i],
                                                                      Sens[,5][i],
                                                                      Spec[,5][i], 
                                                                      1 - Spec[,5][i],
                                                                      DOR[,5][i], 
                                                                      LRp[,5][i], 
                                                                      LRn[,5][i],
                                                                      correlation[,5][2+4*(i-1)],
                                                                      Theta[,5][i],
                                                                      Lambda[,5][i],
                                                                      beta[,5][i],
                                                                      sigma2_theta[,5][i],
                                                                      sigma2_alpha[,5][i],
                                                                      between_study_sd[,5][1 + 2*(i-1)], 
                                                                      between_study_sd[,5][2 + 2*(i-1)]), 
                                                         lci  = c(logit_Sens[,4][i], 
                                                                  logit_Spec[,4][i],
                                                                  Sens[,4][i],
                                                                  Spec[,4][i], 
                                                                  1 - Spec[,4][i],
                                                                  DOR[,4][i], 
                                                                  LRp[,4][i], 
                                                                  LRn[,4][i],
                                                                  correlation[,4][2+4*(i-1)],
                                                                  Theta[,4][i],
                                                                  Lambda[,4][i],
                                                                  beta[,4][i],
                                                                  sigma2_theta[,4][i],
                                                                  sigma2_alpha[,4][i],
                                                                  between_study_sd[,4][1 + 2*(i-1)], 
                                                                  between_study_sd[,4][2 + 2*(i-1)]), 
                                                         uci = c(logit_Sens[,6][i], 
                                                                 logit_Spec[,6][i],
                                                                 Sens[,6][i],
                                                                 Spec[,6][i], 
                                                                 1 - Spec[,6][i],
                                                                 DOR[,6][i], 
                                                                 LRp[,6][i], 
                                                                 LRn[,6][i],
                                                                 correlation[,6][2+4*(i-1)],
                                                                 Theta[,6][i],
                                                                 Lambda[,6][i],
                                                                 beta[,6][i],
                                                                 sigma2_theta[,6][i],
                                                                 sigma2_alpha[,6][i],
                                                                 between_study_sd[,6][1 + 2*(i-1)], 
                                                                 between_study_sd[,6][2 + 2*(i-1)]), 
                                                         row.names = c("Logit Sens", "Logit Spec", "Sensitivity", "Specificity",
                                                                       "FPR", "DOR", "LR+" , "LR-", "correlation", 
                                                                       "Theta", "Lambda", "beta", "sigma2_theta", "sigma2_alpha",
                                                                       "between_study_sd_sens", "between_study_sd_spec")
                              )
                            }
                            s.matrix.group <- vector("list", num_levels)
                            
                            for (i in 1:num_levels) {
                              s.matrix.group[[i]] <- matrix(nrow=17, ncol=5)
                            }
                            
                            for (i in 1:num_levels) {
                              s.matrix.group[[i]][1,1] <- paste0("Sensitivity ", "( ", paste0("logit", HTML("<sub>-1;</sub>")), "(", HTML("&mu;<sub>1;</sub>"), "[SG]",i, ")",  ")")
                              s.matrix.group[[i]][2,1] <- paste0("Specificity ", "( ", paste0("logit", HTML("<sub>-1;</sub>")), "(", HTML("&mu;<sub>0;</sub>"), "[SG]",i, ")",  ")")
                              s.matrix.group[[i]][3,1] <- "False positive rate (1 - Specificity)"
                              s.matrix.group[[i]][4,1] <- paste0("Between-study Correlation" , "( ", HTML("&rho;"), "[SG]",i," )")
                              s.matrix.group[[i]][5,1] <- paste0("Between-study SD for logit(Sensitivity) ", "( ", HTML("&sigma;<sub>1;</sub>"), "[SG]",i," )")
                              s.matrix.group[[i]][6,1] <- paste0("Between-study SD for logit(Specificity) ", "( ", HTML("&sigma;<sub>0;</sub>"), "[SG]",i," )")
                              
                              
                              s.matrix.group[[i]][7,1] <-  paste0("Cutpoint parameter ", "( ", HTML("&Theta;"),"[SG]",i, " )")
                              s.matrix.group[[i]][8,1] <-  paste0("Accuracy parameter ", "( ", HTML("&Lambda;"),"[SG]",i, " )")
                              s.matrix.group[[i]][9,1] <-  paste0("Shape parameter ", "( ", HTML("&beta;"), "[SG]",i," )")
                              s.matrix.group[[i]][10,1] <- paste0("SD of cutpoint parameter ", "( ", HTML("&sigma;<sub>&theta;</sub>"), "[SG]",i, " )")
                              s.matrix.group[[i]][11,1] <- paste0("SD of accuracy parameter ", "( ", HTML("&sigma;<sub>&alpha;</sub>"), "[SG]",i," )")
                              
                              s.matrix.group[[i]][12,1] <- "Diagnostic Odds Ratio"
                              s.matrix.group[[i]][13,1] <- "Likelihood Ratio +ve"
                              s.matrix.group[[i]][14,1] <- "Likelihood Ratio -ve"
                              
                              s.matrix.group[[i]][15,1] <-   paste0("logit(sensitivity) ", "( ", HTML("&mu;<sub>1;</sub>"), "[SG]",i," )")
                              s.matrix.group[[i]][16,1] <-   paste0("logit(specificity) ", "( ", HTML("&mu;<sub>0;</sub>"), "[SG]",i," )")
                            }
                            

                            for (i in vec) {
                              for (k in 1:3) {
                                s.matrix.group[[i]][1,k+1] <- sprintf('%4.3f', Z_group[[i]][3,k])
                                s.matrix.group[[i]][2,k+1] <- sprintf('%4.3f', Z_group[[i]][4,k])
                                s.matrix.group[[i]][3,k+1] <- sprintf('%4.3f', Z_group[[i]][5,k])
                                s.matrix.group[[i]][4,k+1] <- sprintf('%4.3f', Z_group[[i]][9,k])
                                s.matrix.group[[i]][5,k+1] <- sprintf('%4.3f', Z_group[[i]][15,k])
                                s.matrix.group[[i]][6,k+1] <- sprintf('%4.3f', Z_group[[i]][16,k])
                                s.matrix.group[[i]][7,k+1] <- sprintf('%4.3f', Z_group[[i]][10,k])
                                s.matrix.group[[i]][8,k+1] <- sprintf('%4.3f', Z_group[[i]][11,k])
                                s.matrix.group[[i]][9,k+1] <- sprintf('%4.3f', Z_group[[i]][12,k])
                                s.matrix.group[[i]][10,k+1] <- sprintf('%4.3f', Z_group[[i]][13,k])
                                s.matrix.group[[i]][11,k+1] <- sprintf('%4.3f', Z_group[[i]][14,k])
                                s.matrix.group[[i]][12,k+1] <- sprintf('%4.3f', Z_group[[i]][6,k])
                                s.matrix.group[[i]][13,k+1] <- sprintf('%4.3f', Z_group[[i]][7,k])
                                s.matrix.group[[i]][14,k+1] <- sprintf('%4.3f', Z_group[[i]][8,k])
                                s.matrix.group[[i]][15,k+1] <- sprintf('%4.3f', Z_group[[i]][1,k])
                                s.matrix.group[[i]][16,k+1] <- sprintf('%4.3f', Z_group[[i]][2,k])
                              }
                            }
                            
                            s.matrix.group.dataframes <- list()
                            
                            for (i in 1:num_levels) {
                              
                              s.matrix.group[[i]][17, 1:4] <- ""
                              
                              s.matrix.group[[i]][ ,5] <- paste0("(", s.matrix.group[[i]][ ,3], ", ", s.matrix.group[[i]][ ,4], ")")
                              s.matrix.group[[i]][17, 1:5] <- ""
                              
                              s.matrix.group[[i]] <- s.matrix.group[[i]][, c(1,2,5)]
                              
                              s.matrix.group.dataframes[[i]] <- data.frame(s.matrix.group[[i]])
                              
                              colnames(s.matrix.group.dataframes[[i]]) <- c( paste("Parameters for",((levels(factor(X[, m + cov_index]) )[i]))),
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
                                                                   dplyr::select(" ", `Posterior Median`, `95% Posterior Interval`)
                            
                            
                            s.matrix.group.dataframe_allgroups3[1,2] <- ""
                            s.matrix.group.dataframe_allgroups3[1,3] <- ""

                            for (i in vec) {
                              s.matrix.group.dataframe_allgroups3[(i-1)*17+1,1] <- (paste0("<b> Parameters for ",
                                                                                           ((levels(factor(X[, j + cov_index]) )[i])),
                                                                                           ":",
                                                                                           " </b>"))
                            }
                            
                            s.matrix.group.dataframe_allgroups4 <- filter(s.matrix.group.dataframe_allgroups3, !is.na(`Posterior Median`))
                            
                            s.matrix.group.dataframe_allgroups4
                            
                          }
                        }
                        
          })

        # output table 
        output$table <- renderTable({
                                         table_obj()
        }
        , sanitize.text.function = function(x) x)
        

        # download table
        output$download_table <- downloadHandler(
          filename = "SG_parameter_estimates_table.csv",
          content = function(file){
            write.csv(table_obj(), file, sep=",", row.names=FALSE) 
          }
         )

        
      }
     )    
}


# R-hat statistics table ---------------------------------------- ----------------------------------------------------------------
# UI function for R-hat table 
SG_rhat_table_UI <- function(id) {
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
SG_rhat_table_server <- function(id, 
                                 draws, 
                                 data) {  
  
  moduleServer(
    id,
    function(input, output, session) {
      
      rhats_obj <- reactive({
        
        mod <- req(draws())
        
        cov_index <-  as.integer(as.double(input$covariate_subgroup)) - 1
        X <- data()
        
        C <- ncol(X)
        Names <- colnames(X)
        
        if (C > 8 & Names[7] != "rob_PS") { m <<- 6 } else { m <<- 13  }
        
        num_levels <- length(rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se"))$summary[,5])
        
        if (cov_index != 0) {
            
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
        
        rhats_good <- dplyr::filter(rhats_obj(), category == "good")
        rhats_borderline <- dplyr::filter(rhats_obj(), category == "borderline")
        rhats_bad <- dplyr::filter(rhats_obj(), category == "bad")
        
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






