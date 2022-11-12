

# Plot 2 - Covariate vs accuracy plot ------------------------------------------------------------------------------------------------------------------------------------------------


# UI for plot (output the plot)
MR_plot_2_UI <- function(id)  {
  ns <- NS(id)  
  tagList(
    uiOutput(outputId = ns("plot_notes_2")),
    uiOutput(outputId =  ns("my_tooltip_2")),
    plotOutput(outputId =  ns("plot_2"),
               click =  ns("plot_click_2"))
  
  )

}

MR_plot_2_renderUI    <- function(id, 
                                  cts_cov_indicator) {
  moduleServer(
    id,
    function(input, output, session) {
      

      output$plot_notes_2 <- renderUI({
          
            cts_cov_indicator <- cts_cov_indicator$cts_cov_indicator
            ns <- session$ns 
            
          if (cts_cov_indicator == 1) {
            tagList(
                p("NOTE: The vertical bars represent the 95% credible intervals for each level of the categorical/discrete covariate
               from the bivariate model;
               the black points represent the study-specific data points for sensitivity (left panel) and specificity (right panel)
               for each level of the categorical/discrete covariate from the bivariate model;"),
                br()
            )
            
          } 
          else { 
            tagList(
                p("NOTE: The vertical bars represent the 95% credible intervals for each level of the categorical/discrete covariate
               from the bivariate model;
               the black points represent the study-specific data points for sensitivity (left panel) and specificity (right panel)
               for each level of the categorical/discrete covariate from the bivariate model;"),
                br()
            )
        }
        
      })
      
    }
  )
}



# UI for plot 2 - settings menu  ------------------------------------------------------------------------------------------------------
MR_plot_2_settings_menu_UI <- function(id)   {
  ns <- NS(id) 
  tagList(
    h4("Options"),
    br(), 
    sliderInput(inputId = ns("plot_2_dimention_slider_height"), 
                label = "Change size of plot - height", 
                min = 1, max = 2000, 
                value = 300),
    sliderInput(inputId = ns("plot_2_dimention_slider_width"), 
                label = "Change size of plot - width", 
                min = 1, max = 2000, 
                value = 500),
    checkboxInput(inputId = ns("plot_2_study_level_data_points"), 
                  "Observed data Points", TRUE),
    checkboxInput(inputId = ns("plot_2_study_id"), 
                  "Study ID", FALSE),
    conditionalPanel(condition = 'input.plot_2_study_id == 1',
                     ns = ns,
                    sliderInput(ns("plot_2_study_id_nudge_x"), "select x-axis nudge for Study ID label", 
                                min = -50, max = 50, value = 20)),
    conditionalPanel(condition = 'input.plot_2_study_id == 1',
                     ns = ns,
                    sliderInput(ns("plot_2_study_id_nudge_y"), "select y-axis nudge for Study ID label", 
                                min = -50, max = 50, value = 20)),
   checkboxInput(inputId = ns("plot_2_prevcheck"), "Display disease prevalence", FALSE),
   conditionalPanel(condition = 'input.plot_2_prevcheck == 1',
                    ns = ns,
                    sliderInput(ns("plot_2_prevcheck_nudge_x"), 
                                "select x-axis nudge for prevelance label", 
                                min = -50, max = 50, value = -20)),
   conditionalPanel(condition = 'input.plot_2_prevcheck == 1',
                    ns = ns,
                    sliderInput(ns("plot_2_prevcheck_nudge_y"), 
                                "select y-axis nudge for prevelance label", 
                                min = -50, max = 50, value = -20)),
    br(), 
   # Download plot:
   h5("Download plot options:"),
   numericInput(inputId =  ns("plot_width_2"), label=h5("Plot width"), value = 5),
   numericInput(inputId =  ns("plot_height_2"), label=h5("Plot height"), value = 5),
   numericInput(inputId =  ns("plot_dpi_2"), label=h5("Plot DPI"), value = 600),
   downloadButton(outputId = ns("plot_download_2"), label = "Download Plot")
  )
}





# Server function to generate and download plot -----------------------------------------------------------------------------------------
MR_plot_2_server <- function(id, 
                             data,
                             cts_cov_indicator = MR_cts_cov_indicator,
                             draws) {
  moduleServer(
    id,
    function(input, output, session) {
      
      plot_object_2 <- reactive({

        req(data(), draws(), input$covcheck_model, cancelOutput = TRUE)
        
                      mod <- draws()
                      X <- data()
                      cov_index <-  as.integer(as.double(input$covcheck_model)) - 1
                      
                      N <- nrow(X)
                      
                      cts_cov_indicator <-  cts_cov_indicator$cts_cov_indicator
                      
            if (cts_cov_indicator == 1)  { # if continuous covariate --------------------------- 
                          
                          median_sens <- (summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se"))$summary[,5])
                          l_sens <- (summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se"))$summary[,4])
                          u_sens <- (summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se"))$summary[,6])
                          
                          median_spec <- (summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Sp"))$summary[,5])
                          l_spec <- (summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Sp"))$summary[,4])
                          u_spec <- (summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Sp"))$summary[,6])
                          
                          len <- length(median_sens)
                          
                          medians_facet <- tibble(Measure = c(rep("Sensitivity", times = len), rep("Specificity", times = len)), 
                                                  Median = c(median_sens, median_spec), 
                                                  lower = c(l_sens, l_spec), 
                                                  upper = c(u_sens, u_spec), 
                                                  !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)) := c(seq(from = min(as.numeric(X[, j + cov_index])), to = max(as.numeric(X[, j + cov_index])), length = 101),
                                                                                                                seq(from = min(as.numeric(X[, j + cov_index])), to = max(as.numeric(X[, j + cov_index])), length = 101)))
                          
                          # observed values
                          ss<- tibble( 
                            Study =as.numeric(as.factor(X$author)), 
                            TP=X$TP, FN=X$FN, FP=X$FP, TN=X$TN,
                            N=(X$TP+X$FN+X$FP+X$TN) ,
                            Sensitivity= (TP/(TP+FN))  , 
                            Specificity= (TN/(TN+FP))  , 
                            prev = round((TP+FN)/N, 2), 
                            !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)) := factor(X[, j + cov_index])
                          )
                          
                          
                          ss_facet <- tibble(Study = rep(ss$Study, 2), 
                                             Measure = c(rep("Sensitivity", times = N), rep("Specificity", times = N)), 
                                             Accuracy = c(ss$Sensitivity, ss$Specificity), 
                                             TP = rep(ss$TP, 2), 
                                             FN = rep(ss$FN, 2), 
                                             FP = rep(ss$FP, 2), 
                                             TN = rep(ss$TN, 2), 
                                             N = rep(ss$N, 2), 
                                             prev = rep(ss$prev, 2), 
                                             !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)) := rep((X[, j + cov_index]), 2))
                          
                          ss2_facet <- left_join(ss_facet, X)
                          
                          # plot
                          g <- ggplot(data = medians_facet, aes(y=Median, x =  !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)))) + 
                            geom_line(size=2)  +      # summary points
                            geom_ribbon(data = medians_facet, aes(ymin = lower, ymax = upper), alpha = 0.3) + 
                            theme_bw() + 
                            scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1))  + 
                            theme(text = element_text(size=15)) + 
                            ylab("Accuracy") + 
                            facet_wrap(~Measure)
                          g
                          
                          #get ranges of the plot axes
                          x_range <- ggplot_build(g)$layout$panel_params[[1]]$x.range[2] - ggplot_build(g)$layout$panel_params[[1]]$x.range[1]
                          y_range <- ggplot_build(g)$layout$panel_params[[1]]$y.range[2] - ggplot_build(g)$layout$panel_params[[1]]$y.range[1]
                          
                          # Plot observed data (study points) 
                          if (input$plot_2_study_level_data_points == TRUE) {
                            g =                g + geom_point(data = ss2_facet, 
                                                              inherit.aes = FALSE, 
                                                              aes(y = Accuracy, 
                                                                  x =  !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)))) + 
                              facet_wrap(~Measure)
                            g
                          }
                          else { g }
                          
                          # display study ID # (matches tables) [default]
                          if (input$plot_2_study_id  == TRUE) { 
                            g =   g  +   geom_label_repel(data = ss2_facet,  
                                                          #inherit.aes = FALSE, 
                                                          aes(y = Accuracy, 
                                                              x =  !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)), 
                                                              label = Study), 
                                                          size = 3.5,
                                                          box.padding = 0.25, 
                                                          nudge_x = x_range/input$plot_2_study_id_nudge_x, 
                                                          nudge_y = y_range/input$plot_2_study_id_nudge_y)
                            g
                          }
                          else { g }
                          
                          # display disease prevalence's
                          if (input$plot_2_prevcheck  == TRUE) { 
                            g =  g +   geom_label_repel(data = ss2_facet,  
                                                        #inherit.aes = FALSE, 
                                                        aes(y = Accuracy,
                                                            x =  !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)), 
                                                            label = prev),  
                                                        size = 3.5, 
                                                        box.padding = 0.25, 
                                                        nudge_x = x_range/input$plot_2_prevcheck_nudge_x, 
                                                        nudge_y = y_range/input$plot_2_prevcheck_nudge_y)                
                            g
                          }
                          else { g }
                          
                          
                        }
                        
            else { # categorical covariate -------------------------------------------------------------------------------------
                          num_levels <- length(rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se"))$summary[,5])
                          
                          median_sens <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se"))$summary[,5])
                          l_sens <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se"))$summary[,4])
                          u_sens <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se"))$summary[,6])
                          median_spec <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Sp"))$summary[,5])
                          l_spec <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Sp"))$summary[,4])
                          u_spec <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Sp"))$summary[,6])
                          
                          
                          len <- length(median_sens)
                          
                          
                          medians_facet <- tibble(Measure = c(rep("Sensitivity", times = len), rep("Specificity", times = len)), 
                                                  Median = c(median_sens, median_spec), 
                                                  lower = c(l_sens, l_spec), 
                                                  upper = c(u_sens, u_spec), 
                                                  !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)) := c(factor(seq(from = 1, to = num_levels, by = 1)),
                                                                                                                factor(seq(from = 1, to = num_levels, by = 1))))
                          
                          medians_facet2 <- dplyr::mutate(medians_facet,  !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)) := factor(c((levels(factor(X[, j + cov_index]))), 
                                                                                                                                               (levels(factor(X[, j + cov_index]))))))
                          
                          
                          # observed values
                          ss<- tibble( 
                            Study =as.numeric(as.factor(X$author)), 
                            TP=X$TP, FN=X$FN, FP=X$FP, TN=X$TN,
                            N=(X$TP+X$FN+X$FP+X$TN) ,
                            Sensitivity= (TP/(TP+FN))  , 
                            Specificity= (TN/(TN+FP))  , 
                            prev = round((TP+FN)/N, 2), 
                            !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)) := factor(X[, j + cov_index])
                          )
                          
                          ss_facet <- tibble(Study = rep(ss$Study, 2), 
                                             Measure = c(rep("Sensitivity", times = N), rep("Specificity", times = N)), 
                                             Accuracy = c(ss$Sensitivity, ss$Specificity), 
                                             TP = rep(ss$TP, 2), 
                                             FN = rep(ss$FN, 2), 
                                             FP = rep(ss$FP, 2), 
                                             TN = rep(ss$TN, 2), 
                                             N = rep(ss$N, 2), 
                                             prev = rep(ss$prev, 2), 
                                             !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)) := factor(rep((X[, j + cov_index]), 2)))
                          
                          ss2_facet <- left_join(ss_facet, X)
                          
                          # plot
                          g <- ggplot(data = medians_facet2, aes(y=Median, x =  !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)))) + 
                            geom_point(size=5, shape = 21)  +      # summary points
                            geom_errorbar(data = medians_facet2, aes(ymin = lower, ymax = upper), alpha = 0.5) + 
                            theme_bw() + 
                            scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1))  + 
                            theme(text = element_text(size=15)) + 
                            ylab("Accuracy") + 
                            facet_wrap(~Measure)
                          g
                          
                          
                          #get ranges of the plot axes
                          x_range <- ggplot_build(g)$layout$panel_params[[1]]$x.range[2] - ggplot_build(g)$layout$panel_params[[1]]$x.range[1]
                          y_range <- ggplot_build(g)$layout$panel_params[[1]]$y.range[2] - ggplot_build(g)$layout$panel_params[[1]]$y.range[1]
                          
                          # Plot observed data (study points) 
                          if (input$plot_2_study_level_data_points == TRUE) {
                            g =                g + geom_point(data = ss2_facet, 
                                                              inherit.aes = FALSE, 
                                                              aes(y = Accuracy, 
                                                                  x =  !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)))) + 
                              facet_wrap(~Measure)
                            g
                            
                          }
                          else { g }
                          
                          # display study ID # (matches tables) [default]
                          if (input$plot_2_study_id  == TRUE) { 
                            g =   g  +   geom_label_repel(data = ss2_facet,  
                                                          #inherit.aes = FALSE, 
                                                          aes(y = Accuracy, 
                                                              x =  !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)), 
                                                              label = Study), 
                                                          size = 3.5,
                                                          box.padding = 0.25, 
                                                          nudge_x = x_range/input$plot_2_study_id_nudge_x, 
                                                          nudge_y = y_range/input$plot_2_study_id_nudge_y)
                            g
                          }
                          else { g }
                          
                          # display disease prevalence's
                          if (input$plot_2_prevcheck  == TRUE) { 
                            g =  g +   geom_label_repel(data = ss2_facet,  
                                                        #inherit.aes = FALSE, 
                                                        aes(y = Accuracy, 
                                                            x =  !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)), 
                                                            label = prev),  
                                                        size = 3.5, 
                                                        box.padding = 0.25, 
                                                        nudge_x = x_range/input$plot_2_prevcheck_nudge_x, 
                                                        nudge_y = y_range/input$plot_2_prevcheck_nudge_y)
                            g
                          }
                          else { g }
                          
                        }
                    
      })
        
      

        
        # Output ggplot object
        observe({
          output$plot_2 <- renderPlot({
            plot_object_2()
          },  height = as.numeric(input$plot_2_dimention_slider_height),
              width = as.numeric(input$plot_2_dimention_slider_width)
          )
        })
        
        # Download ggplot object 
        output$plot_download_2 <- downloadHandler(
          filename = function(){
            paste("plot.png")
          },
          content = function(file) { 
            ggsave(file,
                   plot_object_2(),
                   width = input$plot_width_2,
                   height = input$plot_height_2,
                   dpi = input$plot_dpi_2,
                   units = "in")
          } 
        )
        
      # reactive mouse click object 
      click <- reactive({
          input$plot_click_2
      })
                                          
      # values to display on plot
      output$vals_2 <- renderPrint({
        
        req(data(),  draws(), cancelOutput = TRUE)
        
        
        X <- data()
        
        cov_index <-  as.integer(as.double(input$covcheck_model)) - 1
        N <- nrow(X)
        
        C <- ncol(X)
        Names <- colnames(X)

        if (C > 8 & Names[7] != "rob_PS") { 
          j <<- 6 
          no_covariates <<- length(colnames(X[,7:C])) 
        } else { 
          j <<- 13 
          no_covariates <<- length(colnames(X[,14:C])) 
        }
        
        
        # observed values
        ss<- tibble( 
          Study =as.numeric(as.factor(X$author)), 
          TP=X$TP, FN=X$FN, FP=X$FP, TN=X$TN,
          N=(X$TP+X$FN+X$FP+X$TN) ,
          Sensitivity= (TP/(TP+FN))  , 
          Specificity= (TN/(TN+FP))  , 
          prev = round((TP+FN)/N, 2), 
          !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)) := as.numeric(X[, j + cov_index])
        )
        
        ss_facet <- tibble(Study = rep(ss$Study, 2), 
                           Measure = c(rep("Sensitivity", times = N), rep("Specificity", times = N)), 
                           Accuracy = c(ss$Sensitivity, ss$Specificity), 
                           TP = rep(ss$TP, 2), 
                           FN = rep(ss$FN, 2), 
                           FP = rep(ss$FP, 2), 
                           TN = rep(ss$TN, 2), 
                           N = rep(ss$N, 2), 
                           prev = rep(ss$prev, 2), 
                           !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)) := rep((X[, j + cov_index]), 2))
        
        ss2_facet <- left_join(ss_facet, X)
        
        # make cols without .cts and .cat endings for display
        for (i in 1:no_covariates) { 
          ss2_facet <- dplyr::mutate(ss2_facet, !!as.name( str_sub(colnames(X)[j + i], end = -5) ) := !!as.name( colnames(X)[j + i]) )
        }
        
        # select columns to display
        cols <- c("author", "year", 
                  str_sub( colnames(X)[c( (j+1):( (j-1) + no_covariates ) )] , end = -5), 
                  "Accuracy", "Measure")
        
        data <- dplyr::select(ss2_facet, cols)
        data[] <- lapply(data, function(x) if (is.numeric(x) ) round(x, 3) else x) # round all numeric cols to 3 s.f. 
        data <- data.frame(data)
        
        data
        
        
        y <- nearPoints(df = data, click() , yvar ="Accuracy")
        req(nrow(y) != 0)
        
        y
                                      
      })
      
      # tooltip
      output$my_tooltip_2 <- renderUI({
        
        req(data(),  draws(), cancelOutput = TRUE)
        
        ns <- session$ns 
        
        
        
        X <- req(data())
        
        cov_index <-  as.integer(as.double(input$covcheck_model)) - 1
        N <- nrow(X)
        
        C <- ncol(X)
        Names <- colnames(X)
        
        if (C > 8 & Names[7] != "rob_PS") { 
          j <<- 6 
          no_covariates <<- length(colnames(X[,7:C])) 
        } else { 
          j <<- 13 
          no_covariates <<- length(colnames(X[,14:C])) 
        }
        
        
        # observed values
        ss<- tibble( 
          Study =as.numeric(as.factor(X$author)), 
          TP=X$TP, FN=X$FN, FP=X$FP, TN=X$TN,
          N=(X$TP+X$FN+X$FP+X$TN) ,
          Sensitivity= (TP/(TP+FN))  , 
          Specificity= (TN/(TN+FP))  , 
          prev = round((TP+FN)/N, 2), 
          !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)) := as.numeric(X[, j + cov_index])
        )
        
        ss_facet <- tibble(Study = rep(ss$Study, 2), 
                           Measure = c(rep("Sensitivity", times = N), rep("Specificity", times = N)), 
                           Accuracy = c(ss$Sensitivity, ss$Specificity), 
                           TP = rep(ss$TP, 2), 
                           FN = rep(ss$FN, 2), 
                           FP = rep(ss$FP, 2), 
                           TN = rep(ss$TN, 2), 
                           N = rep(ss$N, 2), 
                           prev = rep(ss$prev, 2), 
                           !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)) := rep((X[, j + cov_index]), 2))
        
        ss2_facet <- left_join(ss_facet, X)
        
        # make cols without .cts and .cat endings for display
        for (i in 1:no_covariates) { 
          ss2_facet <- dplyr::mutate(ss2_facet, !!as.name( str_sub(colnames(X)[j + i], end = -5) ) := !!as.name( colnames(X)[j + i]) )
        }
        
        # select columns to display
        cols <- c("author", "year", 
                  str_sub( colnames(X)[c( (j+1):( (j-1) + no_covariates ) )] , end = -5), 
                  "Accuracy", "Measure")
        
        data <- dplyr::select(ss2_facet, cols)
        data[] <- lapply(data, function(x) if (is.numeric(x) ) round(x, 3) else x) # round all numeric cols to 3 s.f. 
        data <- data.frame(data)
        
        data
        
        
        y <- nearPoints(df = data, click(), yvar ="Accuracy")
        req(nrow(y) != 0)
        
        verbatimTextOutput(ns("vals_2"))
        
      })

    }

  )
}























