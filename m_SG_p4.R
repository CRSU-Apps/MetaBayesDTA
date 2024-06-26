
# Plot 2 - Covariate vs accuracy plot ------------------------------------------------------------------------------------------------------------------------------------------------

# UI for plot (output the plot)
SG_plot_2_UI <- function(id)   {
  ns <- NS(id)   
  tagList(
    
    p("NOTE: The vertical bars represent the 95% credible intervals for each subgroup from the bivariate model; 
       the black points represent the study-specific data points for sensitivity (left panel) and specificity (right panel)
       for each subgroup"),
    
    
    uiOutput(outputId =  ns("my_tooltip_2")),
    plotOutput(outputId =  ns("plot_2"), 
               click =  ns("plot_click_2"))
    
  )
}

# UI for plot 2 - settings menu
SG_plot_2_settings_menu_UI <- function(id)   {
  ns <- NS(id) 
  tagList(
    h4("Options"),
    br(), 
    sliderInput(inputId = ns("plot_2_dimention_slider_height"), 
                label = "Change size of plot - height", 
                min = 1,
                max = 2000, 
                value = 300,
                ticks = FALSE),
    sliderInput(inputId = ns("plot_2_dimention_slider_width"), 
                label = "Change size of plot - width", 
                min = 1, 
                max = 2000, 
                value = 500,
                ticks = FALSE),
    checkboxInput(inputId = ns("plot_2_study_level_data_points"), 
                  "Observed data Points", 
                  TRUE),
    checkboxInput(inputId = ns("plot_2_study_id"), 
                  "Study ID",
                  FALSE),
    conditionalPanel(condition = 'input.plot_2_study_id == 1',
                     ns = ns,
                    sliderInput(ns("plot_2_study_id_nudge_x"), 
                                "select x-axis nudge for Study ID label", 
                                min = -50, 
                                max = 50, 
                                value = 20, 
                                ticks = FALSE)),
    conditionalPanel(condition = 'input.plot_2_study_id == 1',
                     ns = ns,
                    sliderInput(ns("plot_2_study_id_nudge_y"), 
                                "select y-axis nudge for Study ID label", 
                                min = -50, 
                                max = 50, 
                                value = 20,
                                ticks = FALSE)),
   checkboxInput(inputId = ns("plot_2_prevcheck"), 
                 "Display disease prevalence", 
                 FALSE),
   conditionalPanel(condition = 'input.plot_2_prevcheck == 1',
                    ns = ns,
                    sliderInput(ns("plot_2_prevcheck_nudge_x"), 
                                "select x-axis nudge for prevelance label", 
                                min = -50, 
                                max = 50, 
                                value = -20,
                                ticks = FALSE)),
   conditionalPanel(condition = 'input.plot_2_prevcheck == 1',
                    ns = ns,
                    sliderInput(ns("plot_2_prevcheck_nudge_y"), 
                                "select y-axis nudge for prevelance label", 
                                min = -50,
                                max = 50, 
                                value = -20,
                                ticks = FALSE)),
    br(), 
   
   # Download plot options:
   h5("Download plot options:"),
   radioButtons(inputId = ns("plot_file_type_2"), label = h5("File type"), choices = c(".png", ".pdf")),
   numericInput(inputId =  ns("plot_width_2"), label=h5("Plot width"), value = 5),
   numericInput(inputId =  ns("plot_height_2"), label=h5("Plot height"), value = 5),
   numericInput(inputId =  ns("plot_dpi_2"), label=h5("Plot DPI"), value = 600),
   downloadButton(outputId = ns("plot_download_2"), label = "Download Plot")
  )
}


# Server function to generate and download plot
SG_plot_2_server <- function(id, 
                             data,
                             draws) {
  moduleServer(
    id,
    function(input, output, session) { 
      
      plot_object_2 <- reactive({
                      
            req(data(), draws(), input$covariate_subgroup, cancelOutput = TRUE)
            
            mod <- draws()
            X <- data()
            
            C <- ncol(X)
            Names <- colnames(X)
            
            if (C > 8 & Names[7] != "rob_PS") { j <<- 6 } else { j <<- 13  }
            
            cov_index <-  as.integer(as.double(input$covariate_subgroup)) - 1
            X <- X %>% arrange(!!as.name(colnames(X)[j + cov_index]))
            
            N <- nrow(X)
            
            if (cov_index != 0) {
              if (str_sub(colnames(X)[j + cov_index], start = -3) != "cts") {  # categorical covariate only
                
                num_levels <- length(rstan::summary(mod,
                                                    probs = c(0.025,  0.5, 0.975), 
                                                    pars = c("Se"))$summary[,5])
                
                    ## medians
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
                                            !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)) := c(factor(seq(from = 1, to = num_levels, by = 1)),
                                                                                                          factor(seq(from = 1, to = num_levels, by = 1))))
                    
                    medians_facet2 <- dplyr::mutate(medians_facet,  !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)) := factor(c((levels(factor(X[, j + cov_index]))), 
                                                                                                                                         (levels(factor(X[, j + cov_index]))))))
                    
                    
                    # identify subgroups w/ N = 1
                    X_counts <- X %>% 
                      group_by(!!as.name(colnames(X)[j + cov_index])) %>%
                      summarise(no_rows = length(!!as.name(colnames(X)[j + cov_index])))
                    X_counts_one <- filter(X_counts, no_rows==1)
                    X_counts_one_df <- data.frame(X_counts_one)
                    X_counts_one_df[,1] # vector of factor levels s.t. N = 1
                    
                    # remove summary points, credible and prediction regions for any subgroups s.t. N = 1
                    medians_facet3 <- filter( medians_facet2,  !(!!as.name(str_sub(colnames(X)[j + cov_index], end = -5)) %in% c(X_counts_one_df[,1]) ) )
                    
                    # plot
                    g <- ggplot(data = medians_facet3, aes(y=Median, x =  !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)))) + 
                      geom_point(size=5, shape = 21)  +      # summary points
                      geom_errorbar(data = medians_facet3, aes(ymin = lower, ymax = upper), alpha = 0.3) + 
                      theme_bw() + 
                      scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1))  + 
                      theme(text = element_text(size=15)) + 
                      ylab("Accuracy") + 
                      facet_wrap(~Measure)
                    
                    g
                    
                    
                    # observed values
                    ss<- tibble(
                      Study =as.numeric(as.factor(X$author)),
                      TP=X$TP, FN=X$FN, FP=X$FP, TN=X$TN,
                      N=(X$TP+X$FN+X$FP+X$TN) ,
                      Sensitivity= (X$TP/(X$TP+X$FN))  ,
                      Specificity= (X$TN/(X$TN+X$FP))  ,
                      prev = round((X$TP+X$FN)/N, 2),
                      !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)) := factor(X[, j + cov_index])
                    )
                    
                    ss_facet <- tibble(Study = rep(ss$Study, 2),
                                       Measure = c(rep("Sensitivity", times = nrow(X)), rep("Specificity", times = nrow(X))),
                                       Accuracy = c(ss$Sensitivity, ss$Specificity),
                                       TP = rep(ss$TP, 2),
                                       FN = rep(ss$FN, 2),
                                       FP = rep(ss$FP, 2),
                                       TN = rep(ss$TN, 2),
                                       N = rep(ss$N, 2),
                                       prev = rep(ss$prev, 2),
                                       !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)) := factor(rep((X[, j + cov_index]), 2)))
                    
                    ss2_facet <- left_join(ss_facet, X)
                    
                    #get ranges of the plot axes
                    x_range <- ggplot_build(g)$layout$panel_params[[1]]$x.range[2] - ggplot_build(g)$layout$panel_params[[1]]$x.range[1]
                    y_range <- ggplot_build(g)$layout$panel_params[[1]]$y.range[2] - ggplot_build(g)$layout$panel_params[[1]]$y.range[1]
                    
                    # Plot observed data (study points) 
                    if (input$plot_2_study_level_data_points == TRUE) {
                      g <-                g + geom_point(data = ss2_facet, 
                                                        inherit.aes = FALSE, 
                                                        aes(y = Accuracy, x =  !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)))) + 
                        facet_wrap(~Measure)
                      g
                      
                    }
                    else { g }
                    
                    # display study ID # (matches tables) [default]
                    if (input$plot_2_study_id  == TRUE) { 
                      g <-   g  +   geom_label_repel(data = ss2_facet,  
                                                    #inherit.aes = FALSE, 
                                                    aes(y = Accuracy, x =  !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)), 
                                                        label = Study),  size = 3.5, box.padding = 0.25, 
                                                    nudge_x = x_range/input$plot_2_study_id_nudge_x, 
                                                    nudge_y = y_range/input$plot_2_study_id_nudge_y)
                      g
                    }
                    else { g }
                    
                    # display disease prevalence's
                    if (input$plot_2_prevcheck  == TRUE) { 
                      g <-  g +   geom_label_repel(data = ss2_facet,  
                                                  #inherit.aes = FALSE, 
                                                  aes(y = Accuracy, x =  !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)), 
                                                      label = prev),  size = 3.5, box.padding = 0.25, 
                                                  nudge_x = x_range/input$plot_2_prevcheck_nudge_x, 
                                                  nudge_y = y_range/input$plot_2_prevcheck_nudge_y)
                      g
                    }
                    else { g }
              }
            }
                    
      })
        
      

        
        # Output ggplot object
        observe({
          output$plot_2 <- renderPlot({
            plot_object_2()
          },  height = as.numeric(input$plot_2_dimention_slider_height),
              width  = as.numeric(input$plot_2_dimention_slider_width)
          )
        })
        
        # Download ggplot object 
        output$plot_download_2 <- downloadHandler(
          filename = function(){
            paste0("accuracy_vs_covariate_subgroup", input$plot_file_type_2)
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
        
        
        # tooltips 
        click <- reactive({
          input$plot_click_2
        })
        
        # values to display on plot 
        output$vals_2 <- renderPrint({
          
          req(data(),  draws(), cancelOutput = TRUE)
          
          X <- data()
          
          cov_index <-  as.integer(as.double(input$covariate_subgroup)) - 1
          
          C <- ncol(X)
          Names <- colnames(X)
          
          if (C > 8 & Names[7] != "rob_PS") { 
            j <<- 6 
            no_covariates <<- length(colnames(X[,7:C])) 
          } else { 
            j <<- 13 
            no_covariates <<- length(colnames(X[,14:C])) 
          }
          
          X <- X %>% 
            dplyr::arrange(!!as.name(colnames(X)[j + cov_index]))
          
          N <- nrow(X)
          
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
                             !!as.name(str_sub(colnames(X)[j + cov_index], end = -5)) 
                             := factor(rep((X[, j + cov_index]), 2)))
          
          ss2_facet <- left_join(ss_facet, X)
          
          
          # make cols without .cts and .cat endings for display
          for (i in 1:no_covariates) {
            ss2_facet <- dplyr::mutate(ss2_facet, 
                                       !!as.name( str_sub(colnames(X)[j + i], end = -5) ) := !!as.name( colnames(X)[j + i]) )
          }
          
          # select columns to display
          cols <- c("author", "year", 
                    str_sub( colnames(X)[c( (j+1):(j - 1 + no_covariates) )] , end = -5), 
                    "Accuracy", "Measure")
          data <- dplyr::select(ss2_facet, cols)
          data[] <- lapply(data, function(x) if (is.numeric(x) ) round(x, 3) else x) # round all numeric cols to 3 s.f. 
          
          data <- data.frame(data)
          
          y <- nearPoints(df = data, click(), yvar ="Accuracy")
          req(nrow(y) != 0)
          
          y
          
        })
        
        # tooltip
        output$my_tooltip_2 <- renderUI({
          
          req(data(),  draws(), cancelOutput = TRUE)
          
          ns <- session$ns 
          
          X <- data()
          
          cov_index <-  as.integer(as.double(input$covariate_subgroup)) - 1

          C <- ncol(X)
          Names <- colnames(X)
          
          if (C > 8 & Names[7] != "rob_PS") { 
            j <<- 6 
            no_covariates <<- length(colnames(X[,7:C])) 
          } else { 
            j <<- 13 
            no_covariates <<- length(colnames(X[,14:C])) 
          }
          
          X <- X %>% dplyr::arrange(!!as.name(colnames(X)[j + cov_index]))
          N <- nrow(X)
          

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
          

          # make cols without .cts and .cat endings for display
          for (i in 1:no_covariates) {
            ss2_facet <- dplyr::mutate(ss2_facet, 
                                       !!as.name( str_sub(colnames(X)[j + i], end = -5) ) := !!as.name( colnames(X)[j + i]) )
          }
          
          # select columns to display
          cols <- c("author", "year", 
                    str_sub( colnames(X)[c( (j+1):(j - 1 + no_covariates) )] , end = -5), 
                    "Accuracy", "Measure")
          data <- dplyr::select(ss2_facet, cols)
          
          data[] <- lapply(data, function(x) if (is.numeric(x) ) round(x, 3) else x) # round all numeric cols to 3 s.f. 
          data <- data.frame(data)
          
          y <- nearPoints(df = data, click() , yvar ="Accuracy")
          req(nrow(y) != 0)
          
          verbatimTextOutput(ns("vals_2"))
          
        })
        
       
        
    }

  )
}






















