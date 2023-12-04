





# LCM input / model options -------------------------------------------------  ------------------------------------------------
LCM_model_options_UI <- function(id) {
  ns <- NS(id)  
  tagList(
  uiOutput(ns("lcm_model_options"))
  )
}


# server-side function to input prior dist. options - outputs a list of inputs which can be called within the shiny program
LCM_model_options_inputModule_renderUI_server <- function(id) {  
  
  moduleServer(
    id,
    function(input, output, session) {
      

      output$lcm_model_options <- renderUI({
        
        ns <- session$ns 
        
            div(id = session$ns("lcm_model_options_div"), 
              tagList( 
                textInput(inputId = ns("index_test_name"), 
                          label = "Enter name of Index test here", 
                          value = "Index"), 
                h4("Model options:"),
                awesomeCheckbox(inputId = ns("LCM_conditional_independence_indicator"), "Assume conditional independence between tests?", FALSE),
                awesomeCheckbox(inputId = ns("LCM_SeR_fixed_indicator"), "Assume reference test sensitivity fixed between studies?", TRUE),
                awesomeCheckbox(inputId = ns("LCM_SpR_fixed_indicator"), "Assume reference test specificity fixed between studies?", TRUE),
                awesomeCheckbox(inputId = ns("LCM_SeI_fixed_indicator"), "Assume index test sensitivity fixed between studies?", FALSE),
                awesomeCheckbox(inputId = ns("LCM_SpI_fixed_indicator"), "Assume index test specificity fixed between studies?", FALSE),
                h4("Initial values:"),
                numericInput(ns("LCM_index_logit_mu_se"), "initial value for mean of logit(Se) for index test",
                             value = 2), 
                numericInput(ns("LCM_index_logit_mu_sp"), "initial value for mean of logit(Sp) for index test",
                             value = 2)
              )
            )
    })
      
     observeEvent(input$reset, {
       shinyjs::reset("lcm_model_options_div")
     })
      
      outputOptions(output, "lcm_model_options", suspendWhenHidden = FALSE)
      
    }
  )
}


LCM_model_options_inputModule_server <- function(id) {  
  
  moduleServer(
    id,
    function(input, output, session) {
      
      vals <- reactiveValues()
      
      observe({vals$LCM_conditional_independence_indicator <- input$LCM_conditional_independence_indicator})
      observe({vals$LCM_SeR_fixed_indicator <- input$LCM_SeR_fixed_indicator})
      observe({vals$LCM_SpR_fixed_indicator <- input$LCM_SpR_fixed_indicator})
      observe({vals$LCM_SeI_fixed_indicator <- input$LCM_SeI_fixed_indicator})
      observe({vals$LCM_SpI_fixed_indicator <- input$LCM_SpI_fixed_indicator})
      # observe({vals$LCM_custom_inits_indicator <- input$LCM_custom_inits_indicator})
      
      # initial values
      observe({
          vals$LCM_index_logit_mu_se <- input$LCM_index_logit_mu_se
          vals$LCM_index_logit_mu_sp <- input$LCM_index_logit_mu_sp
      })
      
      # Run the Garabage Collector to Ensure any excess memory used by stan is freed
      gc()
      return(vals)
      
    }
  )
  
}



# Correlation residual plot  --------------------------------------------------   ----------------------------------------------
LCM_correlation_residual_plot_UI <-  function(id) {
  ns <- NS(id)  
    plotOutput(ns("corr_plot"))
}

LCM_correlation_residual_plot_settings_UI <-  function(id) {
  ns <- NS(id)  
  tagList(
    sliderInput(inputId = ns("correlation_residual_plot_dimension_slider_height"), 
                label = "Change size of plot - height", 
                min = 1, 
                max = 2000, 
                value = 300, 
                ticks = FALSE),
    sliderInput(inputId = ns("correlation_residual_plot_dimension_slider_width"), 
                label = "Change size of plot - width", 
                min = 1, 
                max = 2000, 
                value = 500,
                ticks = FALSE),
    downloadButton(outputId = ns("download_corr_plot"), 
                   label = "Download Plot")
  )
}





LCM_correlation_residual_plot_server <- function(id, 
                                                 draws) {  
  
  moduleServer(
    id,
    function(input, output, session) {
      
      corr_plot_obj <- reactive({
        
        mod <- draws()
        
        # plot
        dc <- round(summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("dc"))$summary[,5],3) 
        dc_l <- round(summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("dc"))$summary[,4],3) 
        dc_u <- round(summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("dc"))$summary[,6],3) 
        
        dc_data <- tibble(dc, dc_l, dc_u, obs = seq(1, length(dc), by = 1))
        
        cutoff <- data.frame( x = c(-Inf, Inf), y = 0, cutoff = factor(0) )
        
        g <- ggplot(data = dc_data, aes(y = dc, x=obs)) + geom_point(size = 3) + 
          geom_errorbar(aes(ymin=dc_l, ymax=dc_u), width= 0.75, position=position_dodge(.9)) +
          geom_hline(yintercept = 0) +
          theme_bw() +
         # ylim(-0.31, 0.31) + 
          ylab("Correlation Residuals") + 
          xlab("Study") + 
          theme(text = element_text(size=14),
                axis.text.x = element_text())
        
        g
        
      })
      
      
      # output plot 
      observe({
        output$corr_plot <- renderPlot({
          req(data(), draws(), cancelOutput = TRUE)
          
          corr_plot_obj()
          
        }, 
        height = input$correlation_residual_plot_dimension_slider_height, 
        width  = input$correlation_residual_plot_dimension_slider_width)
      })
      
      # Download ggplot object 
      output$download_corr_plot <- downloadHandler(
        filename = function(){
          paste("plot.png")
        },
        content = function(file) { 
          {ggsave(file, corr_plot_obj())}
        } 
      )
      
    }
  )
}




# table count residual plot  --------------------------------------------------   ----------------------------------------------
LCM_table_prob_residual_plot_UI <-  function(id) {
  ns <- NS(id)  
  plotOutput(ns("table_prob_resid_plot"))
}

LCM_table_prob_residual_plot_settings_UI <-  function(id) {
  ns <- NS(id)  
  tagList(
    sliderInput(inputId = ns("table_prob_resid_plot_dimension_slider_height"), 
                label = "Change size of plot - height", 
                min = 1, 
                max = 2000, 
                value = 300, 
                ticks = FALSE),
    sliderInput(inputId = ns("table_prob_resid_plot_dimension_slider_width"), 
                label = "Change size of plot - width", 
                min = 1, 
                max = 2000, 
                value = 500,
                ticks = FALSE),
    downloadButton(outputId = ns("download_table_prob_resid_plot"), 
                   label = "Download Plot")
  )
}





LCM_table_prob_resid_plot_server <- function(id, 
                                              draws) {  
  
  moduleServer(
    id,
    function(input, output, session) {
      
      table_prob_resid_plot_obj <- reactive({
        
        mod <- draws()
        
        # plot
        dt <- round(summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("dt"))$summary[,5],3) 
        dt_l <- round(summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("dt"))$summary[,4],3) 
        dt_u <- round(summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("dt"))$summary[,6],3) 
        
        dt_data <- tibble(dt, dt_l, dt_u, obs = seq(1, length(dt), by = 1), cell = rep(c(1:4), length(dt)/4))
        
        cutoff <- data.frame( x = c(-Inf, Inf), y = 0, cutoff = factor(0) )
        
        g <- ggplot(data = dt_data, aes(y = dt, x=obs)) + 
          geom_point(size = 3) + 
          geom_errorbar(aes(ymin=dt_l, ymax=dt_u), width= 0.75, position=position_dodge(.9)) +
          geom_hline(yintercept = 0) +
          theme_bw() +
        #  ylim(-0.31, 0.31) + 
          ylab("Table Probability Residuals") + 
          xlab("Study") + 
          theme(text = element_text(size=14),
                axis.text.x = element_text()) + 
          facet_wrap(~ cell)
        
        g
        
      })
      
      
      # output plot 
      observe({
        output$table_prob_resid_plot <- renderPlot({
          req(data(), draws(), cancelOutput = TRUE)
          
          table_prob_resid_plot_obj()
          
        }, 
        height = input$table_prob_resid_plot_dimension_slider_height, 
        width  = input$table_prob_resid_plot_dimension_slider_width)
      })
      
      # Download ggplot object 
      output$download_table_prob_resid_plot <- downloadHandler(
        filename = function(){
          paste("plot.png")
        },
        content = function(file) { 
          {ggsave(file, table_prob_resid_plot_obj())}
        } 
      )
      
    }
  )
}










# Forest Plots  (for estimated data as assuming IGS)  -------------------------  -----------------------------------------------------

LCM_forest_plots_UI <- function(id) {
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
          downloadButton(ns("download_forestLCM_sens"), "Download Sensitivity Forest Plot"),
          downloadButton(ns("download_forestLCM_spec"), "Download Specificity Forest Plot")
  )
}



LCM_forest_plots_server <- function(id, data) {  # "mod" is the rstan file which is reactive. 
  moduleServer(
    id,
    function(input, output, session) {

   ### forest plot for sens ----------------------------------
      forest_plot_sens_obj <-  function(){
        
                                      X <- data()
                                      D <- mada::madad(X, correction.control = "any")
                                       forest(D, 
                                                     type = "sens", 
                                                     snames = X$author, 
                                                     xlab = "Sensitivity", 
                                                     main = "Forest plot of sensitivity")
                                   
                                      
      }
      
      # Output plot object
        output$forest_plot_sens <- renderPlot({  
          
          forest_plot_sens_obj()
          
           })
      
      # Download plot object 
      output$forest_plot_sens_download <- downloadHandler(
                                            filename = function() {
                                              paste("Sensitivity_forest_plot", 
                                                    input$filetype_forest,
                                                    sep=".")
                                            },
                                            content = function(file) { 
                                              png(file)
                                              if (input$filetype_forest == "png") { 
                                                png(file)
                                              }  else {
                                                pdf(file)
                                              }
                                              
                                              forest_plot_sens_obj()
                                              dev.off()
                                            }
      )
      
      
      
      
      
      
      
      
      
      
      
      
   
### forest plot for spec ------------------------------------------------------  -----------------------------------------------------
      
      forest_plot_spec_obj <-  reactive({
                X <- data()
                D <- madad(X, correction.control = "any")
                plot <- forest(D, type = "spec", snames = X$author, xlab = "Specificity", main = "Forest plot of specificity")
                plot
      })
      
      # Output plot object
        output$forest_plot_spec <- renderPlot({  
          
          forest_plot_spec_obj()
          
        })
      
      # Download plot object 
      output$forest_plot_spec_download <- downloadHandler(
        filename = function(){
          paste("Specificity Forest Plot", input$filetype_forest, sep=".")
        },
        content = function(file) {
          if (input$filetype_forest == "png") {
            png(file)
          }  else {
            pdf(file)
          }

          forest_plot_spec_obj()

         # dev.off()
        }
      )
      
    }
  )
}















# LCM MA - Plot of prior distributions  --------------------------------------   -------------------------------------------------------
LCM_model_priors_plot_server <- function(id, 
                                         draws, 
                                         data) {  
  
  moduleServer(
    id,
    function(input, output, session) {
      
      
      priors_plot_obj <- reactive({

        
                req(draws(), X()$reference.cat, cancelOutput = TRUE)
                
                X <- data()
                mod <- draws()
                
                params <- rstan::extract(draws())
                
                num_refs <- ncol(params$Se_ref) 
                refs_names <- c(levels(factor(X$reference.cat)))
                
                se_ref <- list()
                sp_ref <- list()
                
                # extract summary-level params
                for (i in 1:num_refs) { 
                  se_ref[[i]] <- data.frame(params$Se_ref[,i])
                  sp_ref[[i]] <- data.frame(params$Sp_ref[,i])
                }
                
                se_refs <- rlist::list.cbind(se_ref)
                colnames(se_refs) <-  paste0("Se", ", ", refs_names)
                
                sp_refs <- rlist::list.cbind(sp_ref)
                colnames(sp_refs) <- paste0("Sp", ", ", refs_names)
                
                corr_ref <-  tibble(!!as.name(paste0("Correlation", ", ", "Reference tests")) := params$Omega_ref[, 1, 2])
                sd_se_ref <- tibble(!!as.name(paste0("SD for logit(Se)", ", ", "Reference tests")) := params$sigma_ref[, 1])
                sd_sp_ref <- tibble(!!as.name(paste0("SD for logit(Sp)", ", ", "Reference tests")) := params$sigma_ref[, 2])
                
                se_index <-    tibble( !!as.name(paste0("Se", ", ", "index test")) := params$Se_index)
                sp_index <-    tibble( !!as.name(paste0("Sp", ", ", "index test")) := params$Sp_index)
                corr_index <-  tibble( !!as.name(paste0("Correlation", ", ", "index test")) := params$Omega_index[, 1, 2])
                sd_se_index <- tibble(!!as.name(paste0("SD for logit(Se)", ", ", "index test")) := params$sigma_index[, 1])
                sd_sp_index <- tibble(!!as.name(paste0("SD for logit(Sp)", ", ", "index test")) := params$sigma_index[, 2])
                
                prev <- tibble(!!as.name("Prevalence") := params$p)
                
                n_samps <- (2000 - 200)*4
                
                Samples_wide <- tibble(cbind(se_refs, sp_refs, corr_ref, sd_se_ref, sd_sp_ref, 
                                             se_index, sp_index, corr_index, sd_se_index, sd_sp_index, prev))
                
                Samples      <- tidyr::gather(Samples_wide, 
                                              key = Parameter, 
                                              value = samples, 
                                              !!as.name(paste0("Se", ", ", refs_names[[1]])):Prevalence)
                
                g <-    ggplot(data = Samples, aes(x=samples)) + 
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
            need(!(is.null(draws())), "Please run prior model to display plot")
          )
          
          validate(
            need(data()$reference.cat, "Please select a dataset with a covariate for
                                            reference test type (inputted as a column named reference.cat)
                                            to use latent class model")
          )
          
          req(data(), draws(), cancelOutput = TRUE)
          
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
























#  LCM - table of prior distributions  ------------------------------------------- -----------------------------------------------------

# UI function
LCM_model_priors_table_UI <- function(id) {
  ns <- NS(id)   
  
  
  tagList(
    tableOutput(outputId =  ns("model_priors_table")),
    h4("Priors for index test parameters:"),
    tableOutput(ns("priors_table_1")),
   # downloadButton(ns("download_priors_table_1"), "Download Table"),
    h4("Priors for reference test parameters (test-specific):"),
    tableOutput(ns("priors_table_2")),
   # downloadButton(ns("download_priors_table_2"), "Download Table"),
    h4("Priors for reference test parameters (shared):"),
    tableOutput(ns("priors_table_3")),
  #  downloadButton(ns("download_priors_table_3"), "Download Table"), 
    h4("Prior for prevelance:"),
    tableOutput(ns("priors_table_4")),
  #  downloadButton(ns("download_priors_table_4"), "Download Table")
    p("NOTE: The default priors are:"),  
    p("For logit sensitivities and specificities - normal distribution with mean 0 and SD of 1, equivalent to a 95% prior interval of (0.05, 0.95) on the probability scale "),
    p("For between-study SD's of logit sensitivities and specificities - truncated (at 0) normal distribution with mean 0 and SD of 1, equivalent to a 95% prior interval of (0.03, 2.25) "),
    p("For between-study correlation between study-specific logit sensitivities and specificities - LKJ(2) prior, equivalent to 95% prior interval of (-0.8, 0.8)"),
    p("For study specific prevalences - Beta(1,1), equivalent to a 95% prior interval of (0.03, 0.97)")
  )
  
  
  
}

# server function
LCM_model_priors_table_server <- function(id, 
                                          draws, 
                                          data,
                                          LCM_options_indicators) {  
  moduleServer(
    id,
    function(input, output, session) {
      
      # LCM: priors table 1 -  index test params  ------------------------------  ----------------------------------
      priors_table_obj_1 <- reactive({
        
                                req(data(), draws(), cancelOutput = TRUE)
            
                                validate(
                                  need(data()$reference.cat, "Please select a dataset with a covariate for
                                                                                  reference test type (inputted as a column named reference.cat)
                                                                                  to use latent class model")
                                )
            
                              X <- data()
                              mod <- draws()
                              
                              params <- rstan::extract(mod)

                              Sens_index <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se_index"))$summary
                              Spec_index <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Sp_index"))$summary
                              correlation_index <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Omega_index"))$summary
                              between_study_sd_index <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sigma_index"))$summary
                              
                              prev <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("p"))$summary
                              
                              Z = data.frame(estimate = c(Sens_index[,5], Spec_index[,5], 
                                                          1 - Spec_index[,5], 
                                                          correlation_index[,5][3], 
                                                          between_study_sd_index[,5][1], between_study_sd_index[,5][2]), 
                                             
                                             lci = c(Sens_index[,4], Spec_index[,4], 
                                                          1 - Spec_index[,6], 
                                                          correlation_index[,4][3], 
                                                          between_study_sd_index[,4][1], between_study_sd_index[,4][2]), 
                                             
                                             uci = c(Sens_index[,6], Spec_index[,6], 
                                                     1 - Spec_index[,4], 
                                                     correlation_index[,6][3], 
                                                     between_study_sd_index[,6][1], between_study_sd_index[,6][2]), 
                                             
                                             row.names=c("Sensitivity_index", "Specifcity_index", "FPR_index", 
                                                         "Correlation_index",
                                                         "sd_sens_index", "sd_spec_index")
                              )
                              
                              # Create a matrix to store the parameter estimates
                              nrow <- 6 + 1
                              s.matrix <- matrix(nrow=nrow, ncol=5)
                              
                              
                              s.matrix[1,1] <-   paste0("Sensitivity ", "( ", paste0("logit", HTML("<sup>-1;</sup>")), "(", HTML("&mu;<sub>1</sub>"),"[",input$index_test_name,"]", ")",  ")") 
                              s.matrix[2,1] <-   paste0("Specificity ", "( ", paste0("logit", HTML("<sup>-1;</sup>")), "(", HTML("&mu;<sub>0</sub>"),"[",input$index_test_name,"]", ")",  ")") 
                              s.matrix[3,1] <-   "False Positive Rate (1 - specificity) "
                              s.matrix[4,1] <-   paste0("Between-study Correlation" , "( ", HTML("&rho;"),"[",input$index_test_name,"]", " )")
                              s.matrix[5,1] <-   paste0("Between-study SD for logit(Sensitivity) ", "( ", HTML("&sigma;<sub>1</sub>"),"[",input$index_test_name,"]", " )")
                              s.matrix[6,1] <-   paste0("Between-study SD for logit(Specificity) ", "( ", HTML("&sigma;<sub>0</sub>"),"[",input$index_test_name,"]", " )")
                              
                              
                              for (i in 1:3) {
                                for (j in 1:(nrow)-1) { 
                                  s.matrix[j,i+1] <- sprintf('%4.3f', Z[j,i])
                                }
                              }
                              
                              
                              # if either Se or Sp for index test. is fixed
                              if (   ((LCM_options_indicators$LCM_SeI_fixed_indicator == 1) || (LCM_options_indicators$LCM_SpI_fixed_indicator == 1))
                              ) {
                                
                                s.matrix[4,2:4] <- "-"
                                
                                if (LCM_options_indicators$LCM_SeI_fixed_indicator == 1) { # if SeI fixed, no between-study SD for SeI
                                  s.matrix[5,2:4] <- "-"
                                }
                                if (LCM_options_indicators$LCM_SpI_fixed_indicator == 1) { # if SpI fixed, no between-study SD for SpI
                                  s.matrix[6,2:4] <- "-"
                                }
                              }
                              else {
                                # do nothing 
                              }
                              
                              s.matrix[,5] <- paste0("(", s.matrix[,3], ", ", s.matrix[,4], ")")
                              s.matrix[nrow, 1:5] <- ""
                              s.matrix <- s.matrix[, c(1,2,5)]
                              
                              #Name the columns of the matrix
                              colnames(s.matrix) <- c("Parameter", "Prior Median", "95% Prior Interval")
                              
                              s.matrix
                              
          })
      
      
      
      # LCM: priors table 2 -  reference test params (test-specific)  ------------------------------  ----------------------------------
      priors_table_obj_2 <- reactive({
        
        req(data(), draws(), cancelOutput = TRUE)
        
        validate(
          need(data()$reference.cat, "Please select a dataset with a covariate for
                                      reference test type (inputted as a column named reference.cat)
                                      to use latent class model")
        )
        
        X <- data()
        mod <- draws()
        
        

        
        params <- rstan::extract(mod)
        num_refs <- ncol(params$Se_ref)
        refs_names <- c(levels(factor(X$reference.cat)))
        
        for (i in 1:num_refs) {
          Sens_refs <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se_ref"))$summary
          Spec_refs <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Sp_ref"))$summary
        }
        
        Z_group <- list()
        
        for (i in 1:num_refs) {
                            Z_group[[i]] <- data.frame(
                                        
                                        estimate = c(Sens_refs[,5], Spec_refs[,5], 
                                                     1 - Spec_refs[,5]), 
                                                                    
                                        lci = c(Sens_refs[,4], Spec_refs[,4], 
                                                1 - Spec_refs[,6]), 
                                        
                                        uci = c(Sens_refs[,6], Spec_refs[,6], 
                                                1 - Spec_refs[,4]), 
                                        
                                        row.names=c(paste0("Sensitivity_ref","_",refs_names),
                                                    paste0("Specificity_ref","_",refs_names),
                                                    paste0("FPR_ref","_",refs_names))
                                                          
                            )
        }
        
        
        s.matrix.group <- vector("list", num_refs)
        statticks <- list()
        
        s.matrix.group.dataframes <- list()
        
        for (i in 1:num_refs) {
          nrow <- 3 + 1
          s.matrix.group[[i]] <- matrix(nrow=nrow, ncol=5)
          
          s.matrix.group[[i]][1,1] <-    paste0("Sensitivity", "( ", paste0("logit", HTML("<sup>-1</sup>")), "(", HTML("&mu;<sub>1</sub>"),"[", refs_names[i],"]", ")",  ")") 
          s.matrix.group[[i]][2,1] <-    paste0("Specificity", "( ", paste0("logit", HTML("<sup>-1</sup>")), "(", HTML("&mu;<sub>0</sub>"),"[", refs_names[i],"]", ")",  ")") 
          s.matrix.group[[i]][3,1] <-    paste0("False Positive Rate (1 - specificity)")

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
                                                               refs_names[i]), 
                                                         "Prior Median", 
                                                         "95% Prior Interval")
          
        }
        
        rbindlist(s.matrix.group.dataframes)
        
        s.matrix.group.dataframe_allgroups <- tibble(rbindlist(s.matrix.group.dataframes))
        
        
        s.matrix.group.dataframe_allgroups2 <- s.matrix.group.dataframe_allgroups %>% 
          dplyr::mutate( " " = lag(!!as.name(paste("Parameters for", refs_names[1]))),
                         `Prior Median` = lag(`Prior Median`), 
                         `95% Prior Interval` = lag(`95% Prior Interval`))
        
        s.matrix.group.dataframe_allgroups3 <- s.matrix.group.dataframe_allgroups2 %>%
          dplyr::select(" ",
                        `Prior Median`, 
                        `95% Prior Interval` )
        
        s.matrix.group.dataframe_allgroups3
        s.matrix.group.dataframe_allgroups3[1,2] <- ""
        s.matrix.group.dataframe_allgroups3[1,3] <- ""
        
        for (i in 1:num_refs) {
          s.matrix.group.dataframe_allgroups3[(i-1)*nrow+1,1] <- (paste0("<b> Parameters for ",  refs_names[i], ":", " </b>"))
        }
        
        s.matrix.group.dataframe_allgroups3
        
        
      })
      
      
      
      # LCM: priors table 3 -  reference test params (shared across tests)  ------------------------------  ----------------------------------
      priors_table_obj_3 <- reactive({
        
        req(data(), draws(), cancelOutput = TRUE)
        
        validate(
          need(data()$reference.cat, "Please select a dataset with a covariate for
                                      reference test type (inputted as a column named reference.cat)
                                      to use latent class model")
        )
        
        X <- data()
        mod <- draws()
      
      params <- rstan::extract(mod)
      num_refs <- ncol(params$Se_ref)
      refs_names <- c(levels(factor(X$reference.cat)))
      

      correlation_ref <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Omega_ref"))$summary
      between_study_sd_ref <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sigma_ref"))$summary
      
      
      Z = data.frame(estimate = c(correlation_ref[,5][3], 
                                  between_study_sd_ref[,5][1],
                                  between_study_sd_ref[,5][2]), 
                     
                     lci = c(correlation_ref[,4][3], 
                             between_study_sd_ref[,4][1],
                             between_study_sd_ref[,4][2]), 
                     
                     uci = c(correlation_ref[,6][3], 
                             between_study_sd_ref[,6][1], 
                             between_study_sd_ref[,6][2]), 
                     
                     row.names=c("Correlation_ref",
                                 "sd_sens_ref", 
                                 "sd_spec_ref")
                     
      )
      
      # Create a matrix to store the parameter estimates
      nrow <- 3 + 1
      s.matrix <- matrix(nrow=nrow, ncol=5)
      

      s.matrix[1,1] <- paste0("Between-study Correlation " , "( ", HTML("&rho;"), "[Refs]", " )")
      s.matrix[2,1] <- paste0("Between-study SD for logit(Sensitivity) ", "( ", HTML("&sigma;<sub>1</sub>"), "[Refs]", " )")
      s.matrix[3,1] <- paste0("Between-study SD for logit(Specificity) ", "( ", HTML("&sigma;<sub>0</sub>"), "[Refs]", " )")
      
      
      for (i in 1:3) {
        for (j in 1:(nrow)-1) { 
          s.matrix[j,i+1] <- sprintf('%4.3f', Z[j,i])
        }
      }
      
      # if either Se or Sp for reference test. is fixed, no HSROC params and no between-study SD
      if (   ((LCM_options_indicators$LCM_SeR_fixed_indicator == 1) || (LCM_options_indicators$LCM_SpR_fixed_indicator == 1))
      ) {
        
        s.matrix[1,2:4] <- "-"
        
        if (LCM_options_indicators$LCM_SeR_fixed_indicator == 1) { # if SeR fixed, no between-study SD for SeR
          s.matrix[2,2:4] <- "-"
        }
        if (LCM_options_indicators$LCM_SpR_fixed_indicator == 1) { # if SpR fixed, no between-study SD for SpR
          s.matrix[3,2:4] <- "-"
        }
      }
      else {
        # do nothing 
      }
      
      
      
      s.matrix[,5] <- paste0("(", s.matrix[,3], ", ", s.matrix[,4], ")")
      s.matrix[nrow, 1:5] <- ""
      s.matrix <- s.matrix[, c(1,2,5)]
      
      #Name the columns of the matrix
      colnames(s.matrix) <- c("Parameter", "Prior Median", "95% Prior Interval")
      
      s.matrix
      
      
      
      })
      
      
      
      
      
      # LCM: priors table 4 -  prevelance prior  ------------------------------  ----------------------------------
      priors_table_obj_4 <- reactive({
        
        req(data(), draws(), cancelOutput = TRUE)
        
        validate(
          need(data()$reference.cat, "Please select a dataset with a covariate for
                                      reference test type (inputted as a column named reference.cat)
                                      to use latent class model")
        )
      
      
      X <- data()
      mod <- draws()
      
      prev <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("p"))$summary
      
      Z = data.frame(estimate = c(prev[,5]), 
                     lci = c(prev[,4]),
                     uci = c(prev[,6]),
                     row.names=c("prev")
      )
      
      # Create a matrix to store the parameter estimates
      nrow <- 1 + 1
      s.matrix <- matrix(nrow=nrow, ncol=5)
      
      s.matrix[1,1] <- "Disease prevalence"
      
      
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
      
      
    })
      
      
      
      # output tables 
          output$priors_table_1 <- renderTable({
            
                      req(data(), draws(), cancelOutput = TRUE)
                      
                      validate(
                        need(data()$reference.cat, "Please select a dataset with a covariate for
                                                                                                reference test type (inputted as a column named reference.cat)
                                                                                                to use latent class model")
                      )
                      
                      priors_table_obj_1()
                      
                    }, sanitize.text.function = function(x) x)
                    
          
          
          output$priors_table_2 <- renderTable({
            
                      req(data(), draws(), cancelOutput = TRUE)
                      
                      validate(
                        need(data()$reference.cat, "Please select a dataset with a covariate for
                                                                                                reference test type (inputted as a column named reference.cat)
                                                                                                to use latent class model")
                      )
                      
                      priors_table_obj_2()
                      
                    }, sanitize.text.function = function(x) x)
          
          
          
          
          output$priors_table_3 <- renderTable({
            
                      req(data(), draws(), cancelOutput = TRUE)
                      
                      validate(
                        need(data()$reference.cat, "Please select a dataset with a covariate for
                                                                                                reference test type (inputted as a column named reference.cat)
                                                                                                to use latent class model")
                      )
                      
                      priors_table_obj_3()
                      
                    }, sanitize.text.function = function(x) x)
                    
          
          
          
          output$priors_table_4 <- renderTable({
            
                      req(data(), draws(), cancelOutput = TRUE)
                      
                      validate(
                        need(data()$reference.cat, "Please select a dataset with a covariate for
                                                                                                reference test type (inputted as a column named reference.cat)
                                                                                                to use latent class model")
                      )
                      
                      priors_table_obj_4()
                      
                    }, sanitize.text.function = function(x) x)
                    
                
      
      
    }
    )
}


















# Trace plots -------------------------------------------------------------------- ----------------------------------------------------
LCM_model_trace_plots_server <- function(id, draws, data) {  # "mod" is the rstan file which is reactive. 
  moduleServer(
    id,
    function(input, output, session) {
      
      
      trace_plots_obj <- reactive({
        
        req(data(), draws(), cancelOutput = TRUE)
        
        X <- data()
        mod <- draws()
        
        
        plot <-           stan_trace(mod,   
                                     nrow = input$trace_plot_nrow, 
                                     ncol = input$trace_plot_ncol,
                                     pars= c("Se_ref", "Sp_ref", "Se_index", "Sp_index",
                                             # "Omega_ref", "Omega_index", 
                                             "sigma_ref", "sigma_index", 
                                             "se_ref", "sp_ref",
                                             "se_index", "sp_index", 
                                             "p"))


        plot
        
      })
      
      observe({
              output$trace_plots <- renderPlot({
                trace_plots_obj()
                
               }
              , height = input$trace_plot_dimension_slider_height,
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

























# Posterior density plots  ----------------------------------------------------- -------------------------------------------------------
LCM_model_posterior_density_plots_server <- function(id, draws, data) {  # "mod" is the rstan file which is reactive.
  
  moduleServer(
    id,
    function(input, output, session) {
      
      posterior_density_plots_obj <- reactive({
        
        req(data(), draws(), cancelOutput = TRUE)

        X <- data()
        mod <- draws()
        
        plot <-               stan_dens(mod,           
                                        nrow = input$posterior_density_plot_nrow, 
                                        ncol = input$posterior_density_plot_ncol,
                                        pars= c("Se_ref", "Sp_ref", "Se_index", "Sp_index",
                                                #  "Omega_ref", "Omega_index", 
                                                "sigma_ref", "sigma_index", 
                                                "se_ref", "sp_ref",
                                                "se_index", "sp_index", 
                                                "p"))                                             
  
        plot
        
        
      })
      
      
      observe({
        output$posterior_density_plots <- renderPlot({
                                                      posterior_density_plots_obj()
        }
        , height = input$posterior_density_plot_dimension_slider_height,
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





















# LCM MA - table which displays observed data and study weights from model -------------------------------------------- ------------
LCM_data_table_UI <- function(id) {
  ns <- NS(id)   
  DT::dataTableOutput(outputId =  ns("data_table"))
}

LCM_data_table_settings_UI <- function(id) {
  ns <- NS(id)   
  downloadButton(outputId = ns("data_table_download"), label = "Download Table")
}


LCM_data_table_server <- function(id, data) {  # "mod" is the rstan file which is reactive. 
  moduleServer(
    id,
    function(input, output, session) {
      
      data_table_obj <- reactive({
        
                              X <- data()
                              
                              b <- study_level_outcomes(X) # get sens, spec for each trial
                              bb <- data.frame(Author=X$author,
                                               Year=X$year, 
                                               TP=X$TP, 
                                               FN=X$FN, 
                                               FP=X$FP, 
                                               TN=X$TN, 
                                               N=b$N,
                                               Sens=b$Sens, Spec=b$Spec)
                              bb$Sens <- sprintf('%4.3f', bb$Sens) # restrict number of figures after decimal place for sens
                              bb$Spec <- sprintf('%4.3f', bb$Spec)
                              
                              bb <- data.frame(bb)
                              return(bb)
                              
                              
      })
      
     
      output$data_table <- DT::renderDataTable({  
        
        options(DT.options = list(pageLength = 30, 
                                  autoWidth = TRUE, 
                                  scrollX=T))
        
      #  print(data_table_obj())
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




























# Parameter estimates table ---------------------------------------------------- ---------------------------------------------------

# UI function for parameter estimates table 
LCM_parameter_estimates_table_UI <- function(id) {
  ns <- NS(id)  
  tagList(
    h5("Index test parameters"),
    tableOutput(ns("table_1")),
    downloadButton(ns("download_table_1"), "Download Table"),
    h5("Reference test parameters (test-specific)"),
    tableOutput(ns("table_2")),
    downloadButton(ns("download_table_2"), "Download Table"),
    h5("Reference test parameters (shared)"),
    tableOutput(ns("table_3")),
    downloadButton(ns("download_table_3"), "Download Table")
  )
}



# Server function for parameter estimates table 
LCM_parameter_estimates_table_server <- function(id, 
                                                 data, 
                                                 draws,
                                                 LCM_options_indicators) {  
  
  moduleServer(
    id,
    function(input, output, session) {
      
      
      
      # LCM: table 1 -  index test params  ------------------------------  ----------------------------------
        table_obj_1 <- reactive({
                      
          req(data(),  draws(), cancelOutput = TRUE)
          
          X <- data()
          mod <- draws()
          
          params <- rstan::extract(mod)
          num_refs <- ncol(params$Se_ref)
          refs_names <- c(levels(factor(X$reference.cat)))
          
          ## for index tests 
          Sens_index <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se_index"))$summary
          Spec_index <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Sp_index"))$summary
          fp_index <- 1 - Spec_index
          correlation_index <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Omega_index"))$summary
          DOR_index <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("DOR_index"))$summary
          LRp_index <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("LRp_index"))$summary
          LRn_index <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("LRn_index"))$summary
          Theta_index <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("theta_index"))$summary
          Lambda_index <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("lambda_index"))$summary
          beta_index <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("beta_index"))$summary
          sigma2_theta_index <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sigma_sq_theta_index"))$summary
          sigma2_alpha_index <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sigma_sq_alpha_index"))$summary
          between_study_sd_index <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sigma_index"))$summary

            
      Z = data.frame(
         estimate = c(Sens_index[,5], Spec_index[,5],
                      fp_index[,5], 
                      DOR_index[,5], LRp_index[,5] , LRn_index[,5], 
                      correlation_index[,5][3], 
                      Theta_index[,5], Lambda_index[,5], beta_index[,5], sigma2_theta_index[,5], sigma2_alpha_index[,5], 
                      between_study_sd_index[,5][1], between_study_sd_index[,5][2]),
         lci = c(Sens_index[,4], Spec_index[,4],
                 fp_index[,6], 
                 DOR_index[,4], LRp_index[,4] , LRn_index[,4], 
                 correlation_index[,4][3], 
                 Theta_index[,4], Lambda_index[,4], beta_index[,4], sigma2_theta_index[,4], sigma2_alpha_index[,4], 
                 between_study_sd_index[,4][1], between_study_sd_index[,4][2]),
         uci = c(Sens_index[,6], Spec_index[,6],
                 fp_index[,4], 
                 DOR_index[,6], LRp_index[,6] , LRn_index[,6], 
                 correlation_index[,6][3], 
                 Theta_index[,6], Lambda_index[,6], beta_index[,6], sigma2_theta_index[,6], sigma2_alpha_index[,6], 
                 between_study_sd_index[,6][1], between_study_sd_index[,6][2]),
         row.names=c(paste("Sensitivity"), 
                     paste("Specificity"), 
                     paste("FPR"), 
                     paste("DOR"), 
                     paste("LR+"),
                     paste("LR-"),
                     paste("Correlation"),
                     paste("Theta"),
                     paste("Lamba"),
                     paste("Beta"),
                     paste("sigma2_theta"),
                     paste("sigma2_alpha"),
                     paste("sd_sens"),
                     paste("sd_spec")
          )
      )
          
          # Create a matrix to store the parameter estimates
          nrow <- 14 + 1
          
          s.matrix <- matrix(nrow=nrow, ncol=5)
          
          # index test params
          s.matrix[1,1] <- paste0("Sensitivity ", "( ", paste0("logit", HTML("<sup>-1;</sup>")), "(", HTML("&mu;<sub>1;</sub>"),"[",input$index_test_name,"]", ")",  ")") 
          s.matrix[2,1] <- paste0("Specificity ", "( ", paste0("logit", HTML("<sup>-1;</sup>")), "(", HTML("&mu;<sub>0;</sub>"),"[",input$index_test_name,"]", ")",  ")") 
          s.matrix[3,1] <- "False Positive Rate (1 - specificity) "
          s.matrix[4,1] <- "Diagnostic Odds Ratio "
          s.matrix[5,1] <- "Likelihood Ratio +ve "
          s.matrix[6,1] <- "Likelihood Ratio -ve "
          s.matrix[7,1] <-  paste0("Between-study Correlation" , "( ", HTML("&rho;"),"[",input$index_test_name,"]", " )")
          s.matrix[8,1] <-  paste0("Cutpoint parameter ", "( ", HTML("&Theta;"),"[",input$index_test_name,"]", " )")
          s.matrix[9,1] <-  paste0("Accuracy parameter ", "( ", HTML("&Lambda;"),"[",input$index_test_name,"]", " )")
          s.matrix[10,1] <- paste0("Shape parameter ", "( ", HTML("&beta;"),"[",input$index_test_name,"]", " )")
          s.matrix[11,1] <- paste0("SD of cutpoint parameter ", "( ", HTML("&sigma;<sub>&theta;</sub>"),"[",input$index_test_name,"]", " )")
          s.matrix[12,1] <- paste0("SD of accuracy parameter ", "( ", HTML("&sigma;<sub>&alpha;</sub>"),"[",input$index_test_name,"]", " )")
          s.matrix[13,1] <- paste0("Between-study SD for logit(Sensitivity) ", "( ", HTML("&sigma;<sub>1;</sub>"),"[",input$index_test_name,"]", " )")
          s.matrix[14,1] <- paste0("Between-study SD for logit(Specificity) ", "( ", HTML("&sigma;<sub>0;</sub>"),"[",input$index_test_name,"]", " )")
          

          for (i in 1:3) {
            for (j in 1:(nrow-1)) {
              s.matrix[j, i+1] <- sprintf('%4.3f', Z[j,i])
            }
          }
          
          # if either Se or Sp for index test. is fixed, no HSROC params and no between-study SD
          if (   ((LCM_options_indicators$LCM_SeI_fixed_indicator == 1) || (LCM_options_indicators$LCM_SpI_fixed_indicator == 1))
          ) {
            
            s.matrix[7,2:4] <- "-"
            s.matrix[8,2:4] <- "-"
            s.matrix[9,2:4] <- "-"
            s.matrix[10,2:4] <- "-"
            s.matrix[11,2:4] <- "-"
            s.matrix[12,2:4] <- "-"
            
            if (LCM_options_indicators$LCM_SeI_fixed_indicator == 1) { # if SeI fixed, no between-study SD for SeI
              s.matrix[13,2:4] <- "-"
            }
            if (LCM_options_indicators$LCM_SpI_fixed_indicator == 1) { # if SpI fixed, no between-study SD for SpI
              s.matrix[14,2:4] <- "-"
            }
          }
          else {
            # do nothing 
          }
          
          
          s.matrix[,5] <- paste0("(", s.matrix[,3], ", ", s.matrix[,4], ")")
          s.matrix[nrow, 1:5] <- ""
          s.matrix <- s.matrix[, c(1,2,5)]
          

          #Name the columns of the matrix
          colnames(s.matrix) <- c("Parameter", "Posterior Median", "95% Posterior Interval")
          
          s.matrix
          

        })
        
        
        
        
        # LCM: table 2 -  ref test params - test-specific params  ------------------------------  ----------------------------------
        table_obj_2 <- reactive({
          
          req(data(),  draws(), cancelOutput = TRUE)
          
          X <- data()
          mod <- draws()
          
          s.matrix.group.dataframes <- list()
          
          params <- rstan::extract(mod)
          num_refs <- ncol(params$Se_ref)
          refs_names <- c(levels(factor(X$reference.cat)))
          
          Sens_ref <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se_ref"))$summary
          Spec_ref <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Sp_ref"))$summary
          fp_ref <- 1 - Spec_ref
          DOR_ref <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("DOR_ref"))$summary
          LRp_ref <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("LRp_ref"))$summary
          LRn_ref <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("LRn_ref"))$summary
          Theta_ref <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("theta_ref"))$summary
          Lambda_ref <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("lambda_ref"))$summary
          
          Z_group <- list()
          
          for (i in 1:num_refs) {
                              Z_group[[i]] <- data.frame(
                                                  estimate = c(
                                                                Sens_ref[,5][i], Spec_ref[,5][i],
                                                                fp_ref[,5][i], 
                                                                DOR_ref[,5][i], LRp_ref[,5][i] , LRn_ref[,5][i], 
                                                                Theta_ref[,5][i], Lambda_ref[,5][i]),
                                                              lci = c(
                                                                Sens_ref[,4][i], Spec_ref[,4][i],
                                                                fp_ref[,6][i], 
                                                                DOR_ref[,4][i], LRp_ref[,4][i] , LRn_ref[,4][i], 
                                                                Theta_ref[,4][i], Lambda_ref[,4][i]),
                                                              uci = c(
                                                                Sens_ref[,6][i], Spec_ref[,6][i],
                                                                fp_ref[,4][i], 
                                                                DOR_ref[,6][i], LRp_ref[,6][i] , LRn_ref[,6][i], 
                                                                Theta_ref[,6][i], Lambda_ref[,6][i]),
                                                              row.names=c(
                                                                    paste0("Sensitivity_ref","_",refs_names[i]),
                                                                    paste0("Specificity_ref","_",refs_names[i]), 
                                                                    paste0("FPR_ref","_",refs_names[i]),
                                                                    paste0("DOR_ref","_",refs_names[i]),
                                                                    paste0("LR+_ref","_",refs_names[i]),
                                                                    paste0("LR-_ref","_",refs_names[i]),
                                                                    paste0("Theta","_",refs_names[i]),
                                                                    paste0("Lambda","_",refs_names[i]))
                              )
          }
          
          
          s.matrix.group <- vector("list", num_refs)
          statticks <- list()
          
          for (i in 1:num_refs) {
            nrow <- 8 + 1
            s.matrix.group[[i]] <- matrix(nrow=nrow, ncol=5)
            
            s.matrix.group[[i]][1,1] <-  paste0("Sensitivity", "( ", paste0("logit", HTML("<sup>-1</sup>")), "(", HTML("&mu;<sub>1</sub>"),"[", refs_names[i],"]", ")",  ")") 
            s.matrix.group[[i]][2,1] <-  paste0("Specificity", "( ", paste0("logit", HTML("<sup>-1</sup>")), "(", HTML("&mu;<sub>0</sub>"),"[", refs_names[i],"]", ")",  ")") 
            s.matrix.group[[i]][3,1] <-  paste0("False Positive Rate (1 - specificity)   - ","[", refs_names[i],"]") 
            s.matrix.group[[i]][4,1] <-  paste0("Diagnostic Odds Ratio  - ","[", refs_names[i],"]")  
            s.matrix.group[[i]][5,1] <-  paste0("Likelihood Ratio +ve  - ","[", refs_names[i],"]")    
            s.matrix.group[[i]][6,1] <-  paste0("Likelihood Ratio -ve  - ","[", refs_names[i],"]") 
            s.matrix.group[[i]][7,1] <-  paste0("Cutpoint parameter ", "( ", HTML("&Theta;"),"[", refs_names[i],"]", " )")  
            s.matrix.group[[i]][8,1] <-  paste0("Accuracy parameter ", "( ", HTML("&Lambda;"),"[", refs_names[i],"]", " )")   
            
          for (k in 1:3) {
              for (j in 1:(nrow)-1) { 
                s.matrix.group[[i]][j,k+1] <- sprintf('%4.3f', Z_group[[i]][j,k])
              }
            }
            s.matrix.group[[i]][nrow, 1:4] <- ""
            
            # if either Se or Sp for index test. is fixed, no HSROC params and no between-study SD
             if (   ((LCM_options_indicators$LCM_SeR_fixed_indicator == 1) || (LCM_options_indicators$LCM_SpR_fixed_indicator == 1))
             ) {
            
              s.matrix.group[[i]][7,2:4] <- "-"
              s.matrix.group[[i]][8,2:4] <- "-"
            
              }
              else {
            # do nothing 
               }
            
            s.matrix.group[[i]][,5] <- paste0("(", s.matrix.group[[i]][,3], ", ", s.matrix.group[[i]][,4], ")")
            s.matrix.group[[i]][nrow, 1:5] <- ""
            s.matrix.group[[i]] <- s.matrix.group[[i]][, c(1,2,5)]
            
            
            s.matrix.group.dataframes[[i]] <- data.frame(s.matrix.group[[i]])
            
            
            colnames(s.matrix.group.dataframes[[i]]) <- c( paste("Parameters for",
                                                                 refs_names[i]), 
                                                           "Posterior Median", 
                                                           "95% Posterior Interval")
            
          }
          
          rbindlist(s.matrix.group.dataframes)
          
          s.matrix.group.dataframe_allgroups <- tibble(rbindlist(s.matrix.group.dataframes))
          
          
          s.matrix.group.dataframe_allgroups2 <- s.matrix.group.dataframe_allgroups %>% 
                                                  dplyr::mutate( " " = lag(!!as.name(paste("Parameters for", refs_names[1]))),
                                                                 `Posterior Median` = lag(`Posterior Median`), 
                                                                 `95% Posterior Interval` = lag(`95% Posterior Interval`))
          
          s.matrix.group.dataframe_allgroups3 <- s.matrix.group.dataframe_allgroups2 %>%
                                                  dplyr::select(" ",
                                                                `Posterior Median`, 
                                                                `95% Posterior Interval` )
                                                
          s.matrix.group.dataframe_allgroups3
          s.matrix.group.dataframe_allgroups3[1,2] <- ""
          s.matrix.group.dataframe_allgroups3[1,3] <- ""
          
          for (i in 1:num_refs) {
            s.matrix.group.dataframe_allgroups3[(i-1)*nrow+1,1] <- (paste0("<b> Parameters for ",  refs_names[i], ":", " </b>"))
          }
          
          s.matrix.group.dataframe_allgroups3
        
        })
        
        
        
        # LCM: table 3 -  ref test params - shared params  ------------------------------  ----------------------------------
        table_obj_3 <- reactive({
          
          req(data(),  draws(), cancelOutput = TRUE)
          
          X <- data()
          mod <- draws()
          
          params <- rstan::extract(mod)
          num_refs <- ncol(params$Se_ref)
          refs_names <- c(levels(factor(X$reference.cat)))

          
          ## for reference tests 
          correlation_ref <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Omega_ref"))$summary
          beta_ref <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("beta_ref"))$summary
          sigma2_theta_ref <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sigma_sq_theta_ref"))$summary
          sigma2_alpha_ref <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sigma_sq_alpha_ref"))$summary
          between_study_sd_ref <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sigma_ref"))$summary
          

          
          Z = data.frame(
            estimate = c(correlation_ref[,5][3], 
                         beta_ref[,5], sigma2_theta_ref[,5], sigma2_alpha_ref[,5], 
                         between_study_sd_ref[,5][1], between_study_sd_ref[,5][2]),
            
            lci = c(correlation_ref[,4][3], 
                         beta_ref[,4], sigma2_theta_ref [,4], sigma2_alpha_ref[,4], 
                         between_study_sd_ref[,4][1], between_study_sd_ref[,4][2]),
            
            
            uci = c(correlation_ref[,6][3], 
                         beta_ref[,6], sigma2_theta_ref[,6], sigma2_alpha_ref[,6], 
                         between_study_sd_ref[,6][1], between_study_sd_ref[,6][2]),
            
            row.names=c(
                        "Correlation, ref",
                        "beta, ref", 
                        "sigma2_theta, ref", 
                        "sigma2_alpha, ref", 
                        "sd_sens, ref", 
                        "sd_spec, ref")
          )
          
          # Create a matrix to store the parameter estimates
          nrow <-  6 + 1
          
          s.matrix <- matrix(nrow=nrow, ncol=5)

          s.matrix[1,1] <-  paste0("Between-study Correlation " , "( ", HTML("&rho;"), "[Refs]", " )")
          s.matrix[2,1] <-  paste0("Shape parameter ", "( ", HTML("&beta;"), "[Refs]", " )")
          s.matrix[3,1] <-  paste0("SD of cutpoint parameter ", "( ", HTML("&sigma;<sub>&theta;</sub>"), "[Refs]", " )")
          s.matrix[4,1] <-  paste0("SD of accuracy parameter ", "( ", HTML("&sigma;<sub>&alpha;</sub>"), "[Refs]", " )")
          s.matrix[5,1] <-  paste0("Between-study SD for logit(Sensitivity) ", "( ", HTML("&sigma;<sub>1;</sub>"), "[Refs]", " )")
          s.matrix[6,1] <-  paste0("Between-study SD for logit(Specificity) ", "( ", HTML("&sigma;<sub>0;</sub>"), "[Refs]", " )")
          
          
          for (i in 1:3) {
            for (j in 1:(nrow-1)) {
              s.matrix[j, i+1] <- sprintf('%4.3f', Z[j,i])
            }
          }
          
          # if either Se or Sp for index test. is fixed, no HSROC params and no between-study SD
          if (   ((LCM_options_indicators$LCM_SeR_fixed_indicator == 1) || (LCM_options_indicators$LCM_SpR_fixed_indicator == 1))
          ) {
            
            s.matrix[1,2:4] <- "-"
            s.matrix[2,2:4] <- "-"
            s.matrix[3,2:4] <- "-"
            s.matrix[4,2:4] <- "-"
            
            if (LCM_options_indicators$LCM_SeR_fixed_indicator == 1) { # if SeI fixed, no between-study SD for SeI
              s.matrix[5,2:4] <- "-"
            }
            if (LCM_options_indicators$LCM_SpR_fixed_indicator == 1) { # if SpI fixed, no between-study SD for SpI
              s.matrix[6,2:4] <- "-"
            }
          }
          else {
            # do nothing 
          }
          
          
          s.matrix[,5] <- paste0("(", s.matrix[,3], ", ", s.matrix[,4], ")")
          s.matrix[nrow, 1:5] <- ""
          s.matrix <- s.matrix[, c(1,2,5)]
          
          
          #Name the columns of the matrix
          colnames(s.matrix) <- c("Parameter", "Posterior Median", "95% Posterior Interval")
          
          s.matrix
          
          
        })
        
        
        
        
        # output tables
        output$table_1 <- renderTable({
                req(data(), draws(), cancelOutput = TRUE)
                table_obj_1()
              }, sanitize.text.function = function(x) x)
        
        output$table_2 <- renderTable({
                req(data(), draws(), cancelOutput = TRUE)
                table_obj_2()
              }, sanitize.text.function = function(x) x)
        
        output$table_3 <- renderTable({
                req(data(), draws(), cancelOutput = TRUE)
                table_obj_3()
              }, sanitize.text.function = function(x) x)
        

        # download tables
        output$download_table_1 <- downloadHandler(
          filename = "LCM_parameter_estimates_table_1.csv",
          content = function(file){
            write.csv(table_obj_1(), file, sep=",", row.names=FALSE) 
          }
        )
        
        output$download_table_2 <- downloadHandler(
          filename =  "LCM_parameter_estimates_table_2.csv",
          content = function(file){
            write.csv(table_obj_2(), file, sep=",", row.names=FALSE) 
          }
        )
        
        output$download_table_3 <- downloadHandler(
          filename =  "LCM_parameter_estimates_table_3.csv",
          content = function(file){
            write.csv(table_obj_3(), file, sep=",", row.names=FALSE) 
          }
        )
        
        
        
      }
     )    
}






















# R-hat statistics table ------------------------------------------------------------------------ ---------------------------------------

# UI function for R-hat table 
LCM_rhat_table_UI <- function(id) {
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
LCM_rhat_table_server <- function(id, 
                                  draws, 
                                  data) {  
    
  moduleServer(
    id,
    function(input, output, session) {
      
      rhats_obj <- reactive({
        
        X <- data()
        mod <- draws()
        
        params <- rstan::extract(mod)
        num_refs <- ncol(params$Se_ref)
        refs_names <- c(levels(factor(X$reference.cat)))
            
        pars <-  c( "index_logit_mu",
                    "ref_logit_mu",
                    "sigma_ref",
                    "sigma_index",
                    "Omega_ref",
                    "Omega_index", 
                    "cv1", "cv2", 
                    "p", 
                    "se_ref", "sp_ref",
                    "se_index", "sp_index")

        
        rhats <- round(summary(mod, 
                               probs = c(0.025,  0.5, 0.975), 
                               pars = pars)$summary[,8], 2)
        
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
        
        rhats_2 <- rhats_obj()
        
        n_total_rhats <- nrow(rhats_2)
        
        rhats_good <- dplyr::filter(rhats_2, category == "good")
        rhats_borderline <- dplyr::filter(rhats_2, category == "borderline")
        rhats_bad <- dplyr::filter(rhats_2, category == "bad")
        
        n_good <- nrow(rhats_good)
        n_borderline <- nrow(rhats_borderline)
        n_bad <- nrow(rhats_bad)
        
        
        # print summary message 
        ifelse(n_total_rhats == n_good, 
               print("all R-hat's are good (i.e. < 1.05)"), 
               print(paste0(n_bad, " R-hat's are bad (i.e. > 1.10), and ", 
                           n_borderline, " R-hat's are borderline (i.e. between 1.05 and 1.10)")))
        
        
      })
      
      
      # print r-hat table (ordered by bad, moderate, good)
      output$rhats_table <- DT::renderDataTable({ 
        
        req(draws(), cancelOutput = TRUE)
        
        rhats_good <- dplyr::filter(rhats_obj(), category == "good")
        rhats_borderline <- dplyr::filter(rhats_obj(), category == "borderline")
        rhats_bad <- dplyr::filter(rhats_obj(), category == "bad")
        
        n_good <- nrow(rhats_good)
        n_borderline <- nrow(rhats_borderline)
        n_bad <- nrow(rhats_bad)
        
        rhat_table <- rbind(rhats_bad, rhats_borderline, rhats_good)
      
        options(   
                   DT.options = list(autoWidth = TRUE,  
                                     pageLength = n_bad + n_borderline + 10)
                   )
        
        rhat_table
        
      })
      
      
      # download table
      output$download_rhat_table <- downloadHandler(
        
        filename = function() {
          paste("table.csv")
        },
        content = function(file) { 
          write.csv(rhats_obj(), file, sep=",", row.names=FALSE) 
        } 
      )
      
      
    }
  )    
}






# deviance table ------------------------------------------------------------------------ ---------------------------------------

# UI function for deviance table 
LCM_deviance_table_UI <- function(id) {
  ns <- NS(id)  
  tagList(
    verbatimTextOutput(ns("deviance_summary_msg")),
    DT::dataTableOutput(ns("deviance_table")),
    downloadButton(ns("download_deviance_table"), 
                   "Download Table")
  )
}


# Server function for deviance table 
LCM_deviance_table_server <- function(id, 
                                  draws, 
                                  data) {  
  
  moduleServer(
    id,
    function(input, output, session) {
      
      deviance_obj <- reactive({
        
        X <- data()
        mod <- draws()
        
        params <- rstan::extract(mod)
        
        pars <-  c("resdev", "dev")
        
        
        devs_median <- round(summary(mod, 
                               probs = c(0.025,  0.5, 0.975), 
                               pars = pars)$summary[,5], 2)
        
        devs_mean <- round(summary(mod, 
                                     probs = c(0.025,  0.5, 0.975), 
                                     pars = pars)$summary[,1], 2)
        
        devs_2 <- tibble(parameter = c("Overall deviance", names(devs_median)[2:length(devs_median)]), 
                         median = devs_median,
                         mean = devs_mean)
        
        
        
        devs_2
        
      })
      
      
      
      # print deviance table 
      output$deviance_table <- DT::renderDataTable({ 
        
        req(draws(), cancelOutput = TRUE)
        
        options(   
          DT.options = list(autoWidth = TRUE)
        )
        
        deviance_obj()
        
      })
      
      
      # download table
      output$download_deviance_table <- downloadHandler(
        
        filename = function() {
          paste("table.csv")
        },
        content = function(file) { 
          write.csv(deviance_obj(), file, sep=",", row.names=FALSE) 
        } 
      )
      
      
    }
  )    
}














# Parameter estimates tab  - UI ("master") module  ------------------------------ ------------------------------------------------
LCM_parameter_estimates_tab_UI <- function(id) {
  ns <- NS(id)  
  uiOutput(ns("LCM_parameter_estimates_tab_ui"))
}


LCM_parameter_estimates_tab_renderUI_server <- function(id, SA_indicator) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$LCM_parameter_estimates_tab_ui <- renderUI({
        
                  ns <- session$ns
                  SA_indicator <- SA_indicator$SA_indicator
                  
                  tagList( 
                    h4("Main analysis"),
                    LCM_parameter_estimates_table_UI(id = "LCM_model_id"),
                    br(),
                  
                  
                  
                  # SA ------------------------
                  if (SA_indicator == TRUE) { 
                    tagList(
                      h4("Sensitivity analysis"),
                      LCM_parameter_estimates_table_UI(id = "SA_LCM_model_id")
                    )
                  }
                  )
                  
      })
      
    }
  )
}














# Model diagnostics tab  - UI ("master") module  --------------------------------- -------------------------------------------

LCM_model_diagnostics_tab_UI <- function(id) {
  ns <- NS(id)  
  uiOutput(ns("LCM_model_diagnostics_tab_ui"))
}

LCM_model_diagnostics_tab_renderUI_server <- function(id, SA_indicator) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$LCM_model_diagnostics_tab_ui <- renderUI({
        
        ns <- session$ns
        SA_indicator <- SA_indicator$SA_indicator
        
        tagList( 
          h3("Stan sampler diagnostics table"),
          sampler_diagnostics_table_UI(id = "LCM_model_id"),
          br(),
          h3("R-hat statistics"), 
          LCM_rhat_table_UI(id = "LCM_model_id"), 
          br(),
          h3("Deviance statistics"), 
          LCM_deviance_table_UI(id = "LCM_model_id"), 
          br(),
          h3("Correlation residual plot"),
          dropdownButton(
            LCM_correlation_residual_plot_settings_UI(id = "LCM_model_id"),
            circle = TRUE, status = "danger",
            icon = icon("gear"), width = "300px",
            tooltip = tooltipOptions(title = "Click to customise plot")
          ),
          LCM_correlation_residual_plot_UI(id = "LCM_model_id"),
          br(),
          h3("Table probability residual plot"),
          dropdownButton(
            LCM_table_prob_residual_plot_settings_UI(id = "LCM_model_id"),
            circle = TRUE, status = "danger",
            icon = icon("gear"), width = "300px",
            tooltip = tooltipOptions(title = "Click to customise plot")
          ),
          LCM_table_prob_residual_plot_UI(id = "LCM_model_id"),
          br(),
          h3("Posterior density plots"),
          dropdownButton(
            model_posterior_density_plots_settings_UI(id = "LCM_model_id"),
            circle = TRUE, status = "danger",
            icon = icon("gear"), width = "300px",
            tooltip = tooltipOptions(title = "Click to customise plot")
          ),
          model_posterior_density_plots_UI(id = "LCM_model_id"),
          br(),
          h3("Trace plots"),
          dropdownButton(
            model_trace_plots_settings_UI(id = "LCM_model_id"),
            circle = TRUE, status = "danger",
            icon = icon("gear"), width = "300px",
            tooltip = tooltipOptions(title = "Click to customise plot")
          ),
          model_trace_plots_UI(id = "LCM_model_id"),
          
          # SA ------------------------   
          if (SA_indicator == TRUE) { 
                  tagList(
                    h2("Sensitivity analysis"),
                    br(),
                    h3("Stan sampler diagnostics table"),
                    sampler_diagnostics_table_UI(id = "SA_LCM_model_id"),
                    br(),
                    h3("R-hat statistics"), 
                    LCM_rhat_table_UI(id = "SA_LCM_model_id"), 
                    br(),
                    h3("Deviance statistics"), 
                    LCM_deviance_table_UI(id = "SA_LCM_model_id"), 
                    br(),
                    h3("Correlation residual plot"),
                    dropdownButton(
                      LCM_correlation_residual_plot_settings_UI(id = "SA_LCM_model_id"),
                      circle = TRUE, status = "danger",
                      icon = icon("gear"), width = "300px",
                      tooltip = tooltipOptions(title = "Click to customise plot")
                    ),
                    LCM_correlation_residual_plot_UI(id = "SA_LCM_model_id"),
                    br(),
                    h3("Table count residual plot"),
                    dropdownButton(
                      LCM_table_prob_residual_plot_settings_UI(id = "SA_LCM_model_id"),
                      circle = TRUE, status = "danger",
                      icon = icon("gear"), width = "300px",
                      tooltip = tooltipOptions(title = "Click to customise plot")
                    ),
                    LCM_table_prob_residual_plot_UI(id = "SA_LCM_model_id"),
                    br(),
                    h3("Posterior density plots"),
                    dropdownButton(
                      model_posterior_density_plots_settings_UI(id = "SA_LCM_model_id"),
                      circle = TRUE, status = "danger",
                      icon = icon("gear"), width = "300px",
                      tooltip = tooltipOptions(title = "Click to customise plot")
                    ),
                    model_posterior_density_plots_UI(id = "SA_LCM_model_id"),
                    br(),
                    h3("Trace plots"),
                    dropdownButton(
                      model_trace_plots_settings_UI(id = "SA_LCM_model_id"),
                      circle = TRUE, status = "danger",
                      icon = icon("gear"), width = "300px",
                      tooltip = tooltipOptions(title = "Click to customise plot")
                    ),
                    model_trace_plots_UI(id = "SA_LCM_model_id")
                  )
            
          }
        )
        
        
      })
    }
  )
}















# Prior distribution options ---------------------------------------------------- -----------------------------------------

# UI function to input prior distribution options 
LCM_priors_options_inputModule_UI <- function(id) {
  ns <- NS(id)                  
  uiOutput(ns("priors_options"))
}

# server-side function to input prior dist. options - outputs a list of inputs which can be called within the shiny program
LCM_priors_options_inputModule_renderUI_server <- function(id, data) {  
  
  moduleServer(
    id,
    function(input, output, session) {
      
      num_refs <- reactive({ 
        X <- req(data())
        
        num_refs <- length(unique(as.numeric(as.factor(X$reference.cat))))
        
        num_refs
      })
      
      ref_names <- reactive({ 
        X <- req(data())
        
        ref_names <- c(levels(factor(X$reference.cat)))
        
        ref_names
      })
      
      
      output$priors_options <- renderUI({
        
        ns <- session$ns
        
        n_refs <- num_refs()
        refs_names <- ref_names()
        
       div(id = ns("priors_options_div"), 
            if (input$p_scale_priors_indicator == TRUE) {  # p-scale priors ----------------------------------------------------------
                     tagList(
                        h4("Prior Distributions"),
                         # priors for reference test (dynamic UI, as the number of numericInput's must correspond to the number of reference tests in the uploaded data set)
                         h5("Parameters for reference tests:"),
                         h5("Sensitivity - 95% credible interval:"),
                          lapply(1:n_refs, function(i) {
                                      numericInputRow(inputId =  ns(paste0("LCM_prior_sens_ref_lower95_",i)), 
                                                      label= h5(paste0("Prior for lower interval (2.5th percentile), for reference test "), "(", refs_names[i], ")"),
                                                      value = 0.05, 
                                                      min = 0, 
                                                      max = 1)
                          }), 
                          lapply(1:n_refs, function(i) {
                                      numericInputRow(inputId =   ns(paste0("LCM_prior_sens_ref_upper95_",i)),
                                                      label= h5(paste0("Prior for upper interval (97.5th percentile), for reference test "), "(", refs_names[i], ")"),
                                                      value = 0.95, 
                                                      min = 0, 
                                                      max = 1)
                          }), 
                         h5("Specificity - 95% credible interval:"),
                          lapply(1:n_refs, function(i) {
                                      numericInputRow(inputId =   ns(paste0("LCM_prior_spec_ref_lower95_",i)),
                                                      label= h5(paste0("Prior for lower interval (2.5th percentile), for reference test "), "(", refs_names[i], ")"),
                                                      value = 0.05, 
                                                      min = 0, 
                                                      max = 1)
                            }),
                          lapply(1:n_refs, function(i) {
                                      numericInputRow(inputId =   ns(paste0("LCM_prior_spec_ref_upper95_",i)),
                                                      label= h5(paste0("Prior for upper interval (97.5th percentile), for reference test "), "(", refs_names[i], ")"),
                                                      value = 0.95, 
                                                      min = 0, 
                                                      max = 1)
                          }), 
                          # Priors for SD's of between-study SD's (mean is fixed as 0 as half-normal w/ mean 0) 
                          numericInput(inputId = ns("LCM_prior_SD_sens_ref_sd"), label = h5("Prior for between-study SD of logit(Se), for reference test - SD"),
                                       value = 1, min = 0),
                          numericInput(inputId = ns("LCM_prior_SD_spec_ref_sd"), label = h5("Prior for between-study SD of logit(Sp), for reference test - SD"), 
                                       value = 1, min = 0),
                        h5("Parameters for index test:"),
                          numericInput(inputId = ns("LCM_prior_sens_index_lower95"), label=h5("Prior for lower interval (2.5th percentile) of sensitivity for index test "), 
                                       value = 0.05, min = 0, max = 1),
                          numericInput(inputId = ns("LCM_prior_sens_index_upper95"), label=h5("Prior for upper interval (97.5th percentile) of sensitivity for index test "),   
                                       value = 0.95, min = 0, max = 1),
                          numericInput(inputId = ns("LCM_prior_spec_index_lower95"), label=h5("Prior for lower interval (2.5th percentile) of specificity for index test "), 
                                       value = 0.05, min = 0, max = 1),
                          numericInput(inputId = ns("LCM_prior_spec_index_upper95"), label=h5("Prior for upper interval (97.5th percentile) of specificity for index test "),   
                                       value = 0.95, min = 0, max = 1),
                          numericInput(inputId = ns("LCM_prior_SD_sens_index_sd"), label=h5("Prior for between-study SD of logit(Se), index test - SD"), value = 1, min = 0),
                          numericInput(inputId = ns("LCM_prior_SD_spec_index_sd"), label=h5("Prior for between-study SD of logit(Sp), index test - SD"), value = 1, min = 0),
                         h5("Priors for disease prevalence:"),
                          numericInput(inputId = ns("LCM_prior_prev_a"), label=h5("Prior for prevalence - beta shape parameter 1"), value = 1),
                          numericInput(inputId = ns("LCM_prior_prev_b"), label=h5("Prior for prevalence - beta shape parameter 2"), value = 1),
                          br()
                        ) # end of taglist
            }
            else {  # logit-scale priors ----------------------------------------------------------
                    tagList(
                      h4("Prior Distributions"),
                      # priors for reference test (dynamic UI, as the number of numericInput's must correspond to the number 
                      # of reference tests in the uploaded data set)
                      h5("Parameters for reference tests:"),
                      # Priors for means of pooled SeR's - mean
                      lapply(1:n_refs, function(i) {
                        numericInput(inputId = ns(paste0("LCM_prior_mean_sens_ref_mu_",i)),
                                     label   = h5(paste0("Prior mean of pooled logit(Se), for reference test ", "(", refs_names[i], ")", " - mean")), 
                                     value   = 0)
                      }),
                      # Priors for means of pooled SeR's - SD's
                      lapply(1:n_refs, function(i) {
                        numericInput(inputId = ns(paste0("LCM_prior_mean_sens_ref_sd_",i)),
                                     label   = h5(paste0("Prior mean of pooled logit(Se), for reference test ", "(", refs_names[i], ")", " - SD")), 
                                     value   = 1.5)
                      }),
                      # Priors for means of pooled SpR's - mean
                      lapply(1:n_refs, function(i) {
                        numericInput(inputId = ns(paste0("LCM_prior_mean_spec_ref_mu_",i)),
                                     label   = h5(paste0("Prior mean of pooled logit(Sp), for reference test ", "(", refs_names[i], ")", " - mean")), 
                                     value   = 0)
                      }),
                      # Priors for means of pooled SpR's - SD's
                      lapply(1:n_refs, function(i) {
                        numericInput(inputId = ns(paste0("LCM_prior_mean_spec_ref_sd_",i)),
                                     label   = h5(paste0("Prior mean of pooled logit(Sp), for reference test ", "(", refs_names[i], ")", " - SD")), 
                                     value   = 1.5)
                      }),
                      # Priors for SD's of between-study SD's (mean is fixed as 0 as half-normal w/ mean 0) 
                      numericInput(inputId = ns("LCM_prior_SD_sens_ref_sd"), label = h5("Prior for between-study SD of logit(Se), for reference test - SD"), value = 1),
                      numericInput(inputId = ns("LCM_prior_SD_spec_ref_sd"), label = h5("Prior for between-study SD of logit(Sp), for reference test - SD"), value = 1),
                      h5("Parameters for index test:"),
                      numericInput(inputId = ns("LCM_prior_mean_sens_index_mu"), label=h5("Prior mean of pooled logit(Se) - mean"), value = 0),
                      numericInput(inputId = ns("LCM_prior_mean_sens_index_sd"), label=h5("Prior mean of pooled logit(Se) - SD"), value = 1.5),
                      numericInput(inputId = ns("LCM_prior_mean_spec_index_mu"), label=h5("Prior mean of pooled logit(Sp) - mean"), value = 0),
                      numericInput(inputId = ns("LCM_prior_mean_spec_index_sd"), label=h5("Prior mean of pooled logit(Sp) - SD"), value = 1.5),
                      numericInput(inputId = ns("LCM_prior_SD_sens_index_sd"), label=h5("Prior for between-study SD of logit(Se), index test - SD"), value = 1),
                      numericInput(inputId = ns("LCM_prior_SD_spec_index_sd"), label=h5("Prior for between-study SD of logit(Sp), index test - SD"), value = 1),
                      h5("Priors for disease prevalence:"),
                      numericInput(inputId = ns("LCM_prior_prev_a"), label=h5("Prior for prevalence - beta shape parameter 1"), value = 1),
                      numericInput(inputId = ns("LCM_prior_prev_b"), label=h5("Prior for prevalence - beta shape parameter 2"), value = 1),
                      br()
                    ) # end of taglist
              
              }
        ) # end of div
      }) # end of renderUI
      
      observeEvent(input$reset,{
        shinyjs::reset("priors_options_div")
      })
      
      outputOptions(output, "priors_options", suspendWhenHidden = FALSE)
    }
  )
}


LCM_priors_options_inputModule_server <- function(id, data) {  
  
  moduleServer(
    id,
    function(input, output, session) {
      
      num_refs <- reactive({
        req(data(), data()$reference.cat)
        num_refs <- length(unique(as.numeric(as.factor(data()$reference.cat))))
        num_refs
      })
      
      
      LCM_priors_ref <-  reactiveValues(
        vec1 = c(),
        vec2 = c(),
        vec3 = c(),
        vec4 = c()
      )
      
      vals <- reactiveValues()
      
      observe({ 
        
        if (input$p_scale_priors_indicator == TRUE) { # p-scale priors ----------------------------------------------------------
          
              LCM_priors_ref$vec1 <- sapply(1:num_refs(),
                                            function(i){
                                              req(input[[paste0("LCM_prior_sens_ref_lower95_",i)]])
                                            }
              )
              
              LCM_priors_ref$vec2 <- sapply(1:num_refs(),
                                            function(i){
                                              req(input[[paste0("LCM_prior_sens_ref_upper95_",i)]])
                                            }
              )
              
              LCM_priors_ref$vec3 <- sapply(1:num_refs(),
                                            function(i){
                                              req(input[[paste0("LCM_prior_spec_ref_lower95_",i)]])
                                            }
              )
              
              LCM_priors_ref$vec4 <- sapply(1:num_refs(),
                                            function(i){
                                              req(input[[paste0("LCM_prior_spec_ref_upper95_",i)]])
                                            }
              )
               
                
                  vals$LCM_prior_sens_ref_lower95 <- reactiveValues(vec = LCM_priors_ref$vec1)
                  vals$LCM_prior_sens_ref_upper95 <- reactiveValues(vec = LCM_priors_ref$vec2)
                  vals$LCM_prior_spec_ref_lower95 <- reactiveValues(vec = LCM_priors_ref$vec3)
                  vals$LCM_prior_spec_ref_upper95 <- reactiveValues(vec = LCM_priors_ref$vec4)
                  vals$LCM_prior_SD_sens_ref_sd <- input$LCM_prior_SD_sens_ref_sd 
                  vals$LCM_prior_SD_spec_ref_sd <- input$LCM_prior_SD_spec_ref_sd 
                  vals$LCM_prior_sens_index_lower95 <- input$LCM_prior_sens_index_lower95
                  vals$LCM_prior_sens_index_upper95 <- input$LCM_prior_sens_index_upper95
                  vals$LCM_prior_spec_index_lower95 <- input$LCM_prior_spec_index_lower95
                  vals$LCM_prior_spec_index_upper95 <- input$LCM_prior_spec_index_upper95
                  vals$LCM_prior_SD_sens_index_sd <- input$LCM_prior_SD_sens_index_sd 
                  vals$LCM_prior_SD_spec_index_sd <- input$LCM_prior_SD_spec_index_sd 
                  vals$LCM_prior_prev_a <- input$LCM_prior_prev_a 
                  vals$LCM_prior_prev_b <- input$LCM_prior_prev_b 
        
        }
        
        else {   # logit-scale priors ----------------------------------------------------------

          LCM_priors_ref$vec1 <- sapply(1:num_refs(),
                                        function(i){
                                          req(input[[paste0("LCM_prior_mean_sens_ref_mu_",i)]])
                                          }
          )
          
          LCM_priors_ref$vec2 <- sapply(1:num_refs(),
                                        function(i){
                                          req(input[[paste0("LCM_prior_mean_sens_ref_sd_",i)]])
                                        }
          )
          
          LCM_priors_ref$vec3 <- sapply(1:num_refs(),
                                        function(i){
                                          req(input[[paste0("LCM_prior_mean_spec_ref_mu_",i)]])
                                        }
          )
          
          LCM_priors_ref$vec4 <- sapply(1:num_refs(),
                                        function(i){
                                          req(input[[paste0("LCM_prior_mean_spec_ref_sd_",i)]])
                                        }
          )
          
          
          vals$LCM_prior_mean_sens_ref_mu <- reactiveValues(vec = LCM_priors_ref$vec1)
          vals$LCM_prior_mean_sens_ref_sd <- reactiveValues(vec = LCM_priors_ref$vec2)
          vals$LCM_prior_mean_spec_ref_mu <- reactiveValues(vec = LCM_priors_ref$vec3)
          vals$LCM_prior_mean_spec_ref_sd <- reactiveValues(vec = LCM_priors_ref$vec4)
          vals$LCM_prior_SD_sens_ref_sd <- input$LCM_prior_SD_sens_ref_sd 
          vals$LCM_prior_SD_spec_ref_sd <- input$LCM_prior_SD_spec_ref_sd 
          vals$LCM_prior_mean_sens_index_mu <- input$LCM_prior_mean_sens_index_mu 
          vals$LCM_prior_mean_sens_index_sd <- input$LCM_prior_mean_sens_index_sd 
          vals$LCM_prior_mean_spec_index_mu <- input$LCM_prior_mean_spec_index_mu 
          vals$LCM_prior_mean_spec_index_sd <- input$LCM_prior_mean_spec_index_sd 
          vals$LCM_prior_SD_sens_index_sd <- input$LCM_prior_SD_sens_index_sd 
          vals$LCM_prior_SD_spec_index_sd <- input$LCM_prior_SD_spec_index_sd 
          vals$LCM_prior_prev_a <- input$LCM_prior_prev_a 
          vals$LCM_prior_prev_b <- input$LCM_prior_prev_b 
          
        }
        
      
      })
      # Run the Garabage Collector to Ensure any excess memory used by stan is freed
      gc()
      return(vals)
    }
  )
}




