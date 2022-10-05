
# BVM - sROC plot code -------------------------------------------------------------------  ----------------------------------------------------------


# BVM -  UI function to output sROC plot settings menu
MA_sroc_plot_settings_menu_UI <- function(id) { # id used in app file is "MA_model_id"
  ns <- NS(id)
  tagList(
    sliderInput(inputId = ns("plot_dimension_slider"), 
                label = "Change size of plot", 
                min = 1, max = 2000, 
                value = 500,
                ticks = FALSE), 
    sliderInput(inputId = ns("size_summary"), 
                label = "Size of summary estimates", 
                min = 0, max = 30, 
                step = 0.5,
                ticks = FALSE, 
                value = 10),
    sliderInput(inputId = ns("size_study_specific"), 
                label = "Size of study-specific estimates", 
                min = 0, max = 30, 
                step = 0.5,
                ticks = FALSE, 
                value = 5),
    awesomeCheckboxGroup(inputId = ns("HSROCcheck"), 
                       label = h4("Options"),
                       choices = list("Observed data Points"=1, 
                                      "sROC curve"=2, 
                                      "set x and y-axis limits to (0,1)"=3),
                       selected=list(1,3)),
    awesomeCheckbox(inputId = ns("prevcheck"), 
                  "Display disease prevalence", 
                  FALSE),
    awesomeCheckbox(inputId = ns("weightcheck"), 
                  "Display percentage study weights",
                  FALSE),
    uiOutput(ns("SA_display_conditional_plot_checkbox_ui")),
    conditionalPanel(condition = 'input.weightcheck == 1',  # works
                     ns=ns, 
                     sliderInput(inputId = ns("weight_scale"), 
                                 label = "scale to display study weights", 
                                 min = 1, 
                                 max = 2000, 
                                 value = 500)),
    awesomeCheckboxGroup(inputId = ns("cicheck"), 
                       label = "Display 95% study level confidence intervals for observed data",
                       choices = list("Sensitivity"=1, "Specificity"=2)),
    conditionalPanel(condition = 'output.dataset_QA_indicator_ui == 1',
                     ns = ns,
                     selectInput(inputId = ns("QAcheck"),
                                 label = "Display quality assessment scores",
                                 choices = list("None"=1, "Risk of bias: Patient selection"=2, "Risk of bias: Index test"=3,
                                                "Risk of bias: Reference standard"=4, "Risk of bias: Flow & timing"=5,
                                                "Applicability concerns: Patient selection"=6, "Applicability concerns: Index test"=7,
                                                "Applicability concerns: Reference standard"=8, "Risk of bias (all)"=9, "Applicability concerns (all)"=10,
                                                "Both risk of bias and applicability concerns" =11),
                                 selected = 1)),
    conditionalPanel(condition = 'input.QAcheck == 9 || input.QAcheck == 10 || input.QAcheck == 11',   # works
                     ns=ns, 
                     sliderInput(inputId = ns("pie_scale"), 
                                 label = "scale for pie charts", 
                                 min = 1, 
                                 max = 20, 
                                 value = 5)),
    conditionalPanel(condition = 'output.dataset_covariate_indicator_ui == 1', # works
                     ns=ns,
                     uiOutput(ns("covariate_display_ui"))),
    conditionalPanel( # condition = 'output.dataset_covariate_indicator_ui == 1', # works
      condition = 'output.dataset_covariate_indicator_ui == 1 && input.covcheck_display != 1',
      ns=ns, 
      radioButtons(inputId = ns("cov_toggle"), 
                   label = "Display options for covariates", 
                   choices = list("Text" = 1, 
                                  "Coloured points"= 2, 
                                  "Both" =3), 
                   selected = 1)), 
    # Download plot:
    h5("Download plot:"),
    numericInput(inputId =  ns("plot_width"), label=h5("Plot width"), value = 5),
    numericInput(inputId =  ns("plot_height"), label=h5("Plot height"), value = 5),
    numericInput(inputId =  ns("plot_dpi"), label=h5("Plot DPI"), value = 600),
    downloadButton(outputId = ns("plot_download"), label = "Download Plot")
  )
}



# BVM - SA_display_indicator -------------------------------------------------------------------------------------------------


SA_display_indicator_renderUI <- function(id, SA_indicator) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      
      output$SA_display_indicator_ui <- renderUI({
        
            print(SA_indicator$SA_indicator)
        
            ns <- session$ns
            SA_indicator <- SA_indicator$SA_indicator
            
            if (SA_indicator == 1) { 
                       awesomeCheckbox(inputId = ns("SA_display_indicator"), 
                                       "Display sensitivity analysis estimates",
                                       TRUE)
            }
      })
      
    }
  )
}



# BVM - server-side function w/ renderUI's for the sROC plot settings menu (for dynamic UI's) ---------------------------------------------------------------------
sroc_plot_settings_menu_server <- function(id, 
                                           data) {  
  moduleServer(
    id,
    function(input, output, session) {
      
      X <- reactive({req(data())})
      C <- reactive({ncol(dplyr::select(X(), -year.cts, -prevalence.cts))})
      Names <- reactive({colnames(dplyr::select(X(), -year.cts, -prevalence.cts))})
      
      output$dataset_covariate_indicator_ui <- reactive({
        if ((C() > 6 & Names()[7] != "rob_PS") | (C() > 13 & Names()[13] == "ac_RS")) {
          dataset_covariate_indicator <- 1
        }
        else { 
          dataset_covariate_indicator <- 0
        }
        return(dataset_covariate_indicator)
      })
      
      
      output$dataset_QA_indicator_ui <- reactive({
        if ( ( C() == 13 & Names()[7] == "rob_PS" )  | (C() > 13 & Names()[13] == "ac_RS")) {
          dataset_QA_indicator <- 1
        }
        else { 
          dataset_QA_indicator <- 0
        }
        return(dataset_QA_indicator)
      })
      
      outputOptions(output, 'dataset_covariate_indicator_ui', suspendWhenHidden=FALSE)
      outputOptions(output, 'dataset_QA_indicator_ui', suspendWhenHidden=FALSE)
      
    }
  )
}


# BVM - server function for Covariate to display on plot (for info) -----------  -----------------------------------------------------------------------------------------
covariate_display_server <- function(id, 
                                     data) {  
  moduleServer(
    id,
    function(input, output, session) {
      
      
      X <- reactive({req(data())})
      C <- reactive({ncol(dplyr::select(X(), -year.cts, -prevalence.cts))})
      Names <- reactive({colnames(dplyr::select(X(), -year.cts, -prevalence.cts))})
      
      covariate_display_obj_chicesCov <- reactive({
        
        initial <- c("None")
        if (( C() > 6 & Names()[7] != "rob_PS") | (C() > 13 & Names()[13] == "ac_RS")) {
          if (C() > 6 & Names()[7] != "rob_PS") {
            if (C() == 7) {
              covariates <- colnames(X()[7])
            }
            else {
              covariates <- colnames(X()[,7:C()])
            }
          }
          if (C() > 13 & Names()[13] == "ac_RS") {
            if (C() == 14) {
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
      
      
      output$covariate_display_ui <- renderUI({ 
        ns <- session$ns
        
        selectInput(inputId = ns("covcheck_display"), 
                    label= "Please select covariate to display on plot", 
                    choices = covariate_display_obj_chicesCov(),
                    selected = 1)
      })
      
      outputOptions(output, 'covariate_display_ui', suspendWhenHidden=FALSE)
      
    }
  )
}


# BVM - UI for sROC plot (output the plot) --------------------------------------  ----------------------------------------------------------------------------------------------
MA_sroc_plot_UI <- function(id)   {
  ns <- NS(id)   
  tagList(
    
    
    p("NOTE: The dotted line represents the 95% prediction region from the bivariate model; 
       the greyed out area represents the 95% credible region from the bivariate model"),
    
    conditionalPanel(condition = "input.SA_indicator==1",
                     ns=ns, 
                     p("; the lighter hollow diamond represents the summary estimate from the bivariate model
                          for the sensitivity analysis")
    ),
    
    
    plotOutput(outputId =  ns("piechart"),
               height = "300px",
               width = "500px"),
    uiOutput(outputId =  ns("my_tooltip")),
    plotOutput(outputId =  ns("plot"),
               click =  ns("plot_click")), 


    
    
  )
}



# BVM - sROC - Plot  -----------------------------------------------------------  --------------------------------------------------------

MA_sroc_plot_server <- function(id, 
                                data,
                                draws,
                                SA_draws, 
                                SA_indicator)
 {
  moduleServer(
    id,
    function(input, output, session) {
      
      plot_object <- reactive({
        
        
        validate(
          need(!(is.null(draws())), "Please run model to display plot")
        )
        
        req(data(),  draws(), cancelOutput = TRUE)
        
        SA_indicator <- SA_indicator$SA_indicator
        SA_display_indicator <- input$SA_display_indicator
        
        print(paste("1", SA_indicator))
        print(paste("2", SA_display_indicator))
        
        X <- data()
        n_studies <- nrow(X)
        
        C <- ncol(dplyr::select(X, -year.cts, -prevalence.cts))
        Names <- colnames(dplyr::select(X, -year.cts, -prevalence.cts))
        
        
        # run function to get roc curve, credible and prediction regions and summary estimates from Stan model output file
        ss <- MA_cred_pred_roc(X, draws())$ss
        X <- MA_cred_pred_roc(X, draws())$X
        roc_points2 <- MA_cred_pred_roc(X, draws())$roc_points2
        pred_region <- MA_cred_pred_roc(X, draws())$pred_region
        credible_region <- MA_cred_pred_roc(X, draws())$credible_region
        medians <- MA_cred_pred_roc(X, draws())$medians
        
        
        if (SA_indicator == TRUE) { 
          # run function to get roc curve, credible and prediction regions and summary estimates from Stan model output file for SA
          SA_roc_points2 <- MA_cred_pred_roc(X, SA_draws())$roc_points2
          SA_pred_region <- MA_cred_pred_roc(X, SA_draws())$pred_region
          SA_credible_region <- MA_cred_pred_roc(X, SA_draws())$credible_region
          SA_medians <- MA_cred_pred_roc(X, SA_draws())$medians
        }

        size_summary <- {input$size_summary}
        weight_scale <- {input$weight_scale}
        size_study_specific <- {input$size_study_specific}
        pie_scale <- {input$pie_scale}
        
        ### Base Plot using ggplot
        g <- ggplot(data = ss, size = 2, aes(y=Sensitivity, x = 1-Specificity)) + 
          geom_point(data = medians,  aes(y=median_sens, x = 1 - median_spec),
                     size=size_summary,
                     shape = 5,
                     inherit.aes = F)  +      # summary points
          geom_path(data = pred_region, aes(x= 1 - x, y= y), linetype = 2, size = 0.4, inherit.aes = F)   +                 # prediction region
          geom_polygon(data = credible_region, aes(x= 1 - x, y= y), alpha=0.20, size=0.4, linetype = 2,inherit.aes = F) + # conf region
          theme_bw() + 
          theme(text = element_text(size=15)) + 
          coord_fixed()
        
        g 
        
        
        # Add sensitivity analysis summary estimate to plot 
        if (SA_display_indicator == 1) { 
            if (SA_indicator == TRUE) { 
             g =  g + geom_point(data = SA_medians,  aes(y=median_sens, x = 1 - median_spec),
                                 size=size_summary,
                                 shape = 5, 
                                 alpha = 0.40, 
                                 inherit.aes = F)  + 
               geom_path(data = SA_pred_region, aes(x= 1 - x, y= y), 
                         alpha = 0.40, 
                         linetype = 2, 
                         size = 0.4,   
                         inherit.aes = F)   +                 # prediction region
               geom_polygon(data = SA_credible_region, aes(x= 1 - x, y= y), 
                            alpha=0.05, 
                            size=0.4, 
                            linetype = 2,
                            inherit.aes = F)  # conf region
             g
            
            if ('2' %in% input$HSROCcheck) {  
               g = g + geom_path(data = SA_roc_points2, aes(x = FPR, y = TPR), 
                                 alpha = 0.40)
               g 
            } 
           }
        } else { 
          g 
        }
        
        # Plot study-level estimates 
        if ('1' %in% input$HSROCcheck) { # plot study-specific observed data points
          if (input$weightcheck == TRUE) { # w/ study-specific weight info
            g = g + geom_ellipse(data = ss, 
                                 aes(x0 = 1-Specificity, y0 = Sensitivity, 
                                     a=pctsp/weight_scale, b=pctse/weight_scale,
                                     angle = 0)) 
            g 
          }
          else { # w/o study-specific weight info
            g = g + geom_point(data = ss, aes(y=Sensitivity, x = 1-Specificity), 
                               size = size_study_specific,
                               alpha=0.7,
                               inherit.aes = F)  
            g
          }
        }  else {
          g
        }
        
        # Plot HSROC curve
        if ('2' %in% input$HSROCcheck) {  
          g = g + geom_path(data = roc_points2, aes(x = FPR, y = TPR)) 
          g 
        } else { 
          g
        }
        
        # Set x and y-axis limits to (0,1)
        if ('3' %in% input$HSROCcheck) {
          g = g +       scale_x_continuous(breaks = seq(0,1,0.2), limits = c(0,1.05))  + 
            scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1.05))  
          g
        } else { g 
        }
        
        # display disease prevalence's on sROC plot 
        if ( input$prevcheck == TRUE ) {   
          g = g +   geom_label_repel(data = ss, aes(label = prev),  size = 3.5, box.padding = 0.25)
          g
        } else { 
          g 
        }
        
        
        
        # sROC plot (ctd.) - (1) Plots when no QA data or covariate data ("Standard.csv") ----------------
        if (C == 6) {  # 6 columns, so no quality assessment data or covariate data
          # Plot observed sens CI's
          if ('1' %in% {{input$cicheck}}) {
            g = g + geom_errorbar(data = ss, 
                                  aes(ymin = Sens_LCI, ymax = Sens_UCI),
                                  width = 0.01, alpha = 0.4)
            g 
          }  else {
            g 
          }
          # Plot observed spec CI's
          if ('2' %in% {{input$cicheck}}) {   
            g = g + geom_errorbar(data = ss, 
                                  aes(xmin = FPR_LCI, xmax = FPR_UCI),
                                  width = 0.01, alpha = 0.4)
            g 
          }  else {
            g 
          }
          
        }   else if ( C == 13 & Names[7] == "rob_PS" ) {  #  sROC plot (ctd.) -  (2) Plots when Quality Assessment data is available but no covariate data ("QA.csv") -------------------------------------------------------------------------
          study_level <- study_level_outcomes(X)
          data_QA <- dplyr::left_join(X, study_level) %>%
            dplyr::mutate(rob_PS = factor(rob_PS, levels = c(1,2,3), labels = c("Low", "High", "Unclear")),
                          rob_IT = factor(rob_IT, levels = c(1,2,3), labels = c("Low", "High", "Unclear")),
                          rob_RS = factor(rob_RS, levels = c(1,2,3), labels = c("Low", "High", "Unclear")),
                          rob_FT = factor(rob_FT, levels = c(1,2,3), labels = c("Low", "High", "Unclear")),
                          ac_PS = factor(ac_PS, levels = c(1,2,3), labels = c("Low", "High", "Unclear")),
                          ac_IT = factor(ac_IT, levels = c(1,2,3), labels = c("Low", "High", "Unclear")),
                          ac_RS = factor(ac_RS, levels = c(1,2,3), labels = c("Low", "High", "Unclear")))
          data_QA2 <- left_join(data_QA, ss)
          

          # Reshape the data to allow for pie charts to be plotted as summary points
          # For ROB outcomes
          P_rob <- X[,-which(names(X) == "ac_PS" | names(X) == "ac_RS"| names(X) == "ac_IT")] # Delete applicability concerns for ROB plot
          P_rob <- reshape(P_rob, direction = "long", varying = c("rob_PS", "rob_IT", "rob_RS", "rob_FT"),
                           v.names = "score", timevar = "rob", times = c("rob_PS", "rob_IT", "rob_RS", "rob_FT"))
          # For AC outcomes
          P_ac <- X[,-which(names(X) == "rob_PS" | names(X) == "rob_RS"| names(X) == "rob_IT"|names(X) == "rob_FT")] # Delete applicability concerns for ROB plot
          P_ac <- reshape(P_ac, direction = "long", varying = c("ac_PS", "ac_IT", "ac_RS"),
                          v.names = "score", timevar = "rob", times = c("ac_PS", "ac_IT", "ac_RS"))
          # For both outcomes together
          P_both <- reshape(X, direction = "long", varying = c("rob_PS", "rob_IT", "rob_RS", "rob_FT", "ac_PS", "ac_IT", "ac_RS"),
                            v.names = "score", timevar = "rob", times = c("rob_PS", "rob_IT", "rob_RS", "rob_FT", "ac_PS", "ac_IT", "ac_RS"))
          
          # Coloured study level estimates based on 1 of the 7 outcomes of QA
          if (i != 1) { # i.e. if the user selects a QA item to display
            for (i in 1:7) { # i.e. if the QA item is rob_PS, rob_IT, rob_RS, rob_FT, ac_PS, ac_IT, ac_RS
              if ( {input$QAcheck} == (i+1) ) { # the selected quality assessment item
                if (input$weightcheck == FALSE) {
                  g =  g + geom_point(data = data_QA2, 
                                      size = size_study_specific,
                                      aes(x = FPR, y = Sensitivity, 
                                          shape = !!as.name(colnames(data_QA2)[6+i]))) 
                  g
                } else { # i.e. input$weightcheck == TRUE
                  g =   g  +  geom_ellipse(data = data_QA2, 
                                           #    size = 0.3,
                                           aes(x0 = 1-Specificity, y0 = Sensitivity,
                                               linetype =  !!as.name(colnames(data_QA2)[6+i]), 
                                               a=pctsp/weight_scale, b=pctse/weight_scale, angle = 0))
                  g
                }
              }
              g
            }
          } else { # if no QA is selected
            g
          }
          g
          
          # Pie charts to represent study level estimates, split into risk of bias and applicability concerns
          # Pie charts for  RoB
          if ('9' %in% {input$QAcheck}) {
            P_rob_wide <- P_rob %>% spread(rob, score)  %>% mutate(Study = seq(from = 1, to = nrow(X)))
            P_rob_wide2 <- left_join(P_rob_wide, ss, by = "Study") 
            P_rob_wide3 <- dplyr::mutate(P_rob_wide2, 
                                         rob_FT_score = as.numeric(factor(rob_FT, levels = c(1,2,3))), 
                                         rob_IT_score = as.numeric(factor(rob_IT, levels = c(1,2,3))), 
                                         rob_PS_score = as.numeric(factor(rob_PS, levels = c(1,2,3))), 
                                         rob_RS_score = as.numeric(factor(rob_RS, levels = c(1,2,3))))
            P_rob_wide4 <-  dplyr::mutate(P_rob_wide3, 
                                          Study = seq(from = 1, to = nrow(X)), 
                                          Sensitivity = as.numeric(Sensitivity),  
                                          Specificity = as.numeric(Specificity), 
                                          FPR =  as.numeric(1 - Specificity), 
                                          rob_FT = rep(1, length = nrow(X)), 
                                          rob_IT = rep(1, length = nrow(X)),
                                          rob_PS = rep(1, length = nrow(X)),
                                          rob_RS = rep(1, length = nrow(X)), 
                                          rob_FT_colour = ifelse(rob_FT == 1, "green",
                                                                 ifelse(rob_FT == 2, "red", "grey")), 
                                          rob_IT_colour = ifelse(rob_IT == 1, "green",
                                                                 ifelse(rob_IT == 2, "red", "grey")),
                                          rob_PS_colour = ifelse(rob_PS == 1, "green",
                                                                 ifelse(rob_PS == 2, "red", "grey")),
                                          rob_RS_colour = ifelse(rob_RS == 1, "green",
                                                                 ifelse(rob_RS == 2, "red", "grey")))  %>%
              dplyr::select(Study, FPR, Sensitivity, 
                            rob_PS, rob_IT, rob_RS, rob_FT, 
                            rob_PS_score, rob_IT_score, rob_RS_score, rob_FT_score)
            P_rob_wide5 <- tibble(P_rob_wide4)
            
            colours_group = c("green", "red", "black")
            colours_category = c("white", "white", "white", "white")
            
            # add little pie charts to plot
            g = g + geom_scatterpie(data = P_rob_wide5, aes(x =  FPR,
                                                            y = Sensitivity, 
                                                            group = Study,
                                                            colour = cut(x = c(P_rob_wide5$rob_FT_score, P_rob_wide5$rob_IT_score, 
                                                                               P_rob_wide5$rob_PS_score, P_rob_wide5$rob_RS_score), 
                                                                         breaks = c(0,1,2,3), 
                                                                         labels = c("Low", "High", "Unclear"), 
                                                                         ordered_result = TRUE)),  
                                    col = c("rob_FT", "rob_IT", "rob_PS", "rob_RS"), 
                                    size = 1, 
                                    pie_scale = pie_scale, 
                                    alpha = 0.5) +
              coord_equal() + 
              theme_bw() + 
              theme(legend.position = "bottom", 
                    legend.title = element_text(size=10), 
                    legend.text = element_text(size=10)
              ) + 
              scale_colour_manual("Risk", values = colours_group) + 
              scale_fill_manual(values = colours_category) + 
              guides(fill = FALSE)
            
            g
          }
          
          
          # Pie charts for applicability concerns
          if ('10' %in% {input$QAcheck}) {
            P_ac_wide <- P_ac %>% spread(rob, score)  %>% mutate(Study = seq(from = 1, to = nrow(X)))
            P_ac_wide2 <- left_join(P_ac_wide, ss, by = "Study") 
            P_ac_wide3 <- dplyr::mutate(P_ac_wide2, 
                                        ac_IT_score = as.numeric(factor(ac_IT, levels = c(1,2,3))), 
                                        ac_PS_score = as.numeric(factor(ac_PS, levels = c(1,2,3))), 
                                        ac_RS_score = as.numeric(factor(ac_RS, levels = c(1,2,3))))
            P_ac_wide4 <-  dplyr::mutate(P_ac_wide3, 
                                         Study = seq(from = 1, to = nrow(X)), 
                                         Sensitivity = as.numeric(Sensitivity),  
                                         Specificity = as.numeric(Specificity), 
                                         FPR =  as.numeric(1 - Specificity), 
                                         ac_IT = rep(1, length = nrow(X)),
                                         ac_PS = rep(1, length = nrow(X)),
                                         ac_RS = rep(1, length = nrow(X)), 
                                         ac_IT_colour = ifelse(ac_IT == 1, "green",
                                                               ifelse(rob_IT == 2, "red", "grey")),
                                         ac_PS_colour = ifelse(ac_PS == 1, "green",
                                                               ifelse(rob_PS == 2, "red", "grey")),
                                         ac_RS_colour = ifelse(ac_RS == 1, "green",
                                                               ifelse(rob_RS == 2, "red", "grey")))  %>%
              dplyr::select(Study, FPR, Sensitivity, 
                            ac_PS, ac_IT, ac_RS,
                            ac_PS_score, ac_IT_score, ac_RS_score)
            P_ac_wide5 <- tibble(P_ac_wide4)
            
            colours_group = c("green", "red", "black")
            colours_category = c("white", "white", "white")
            
            # add little pie charts to plot
            g = g + geom_scatterpie(data = P_ac_wide5, aes(x =  FPR,
                                                           y = Sensitivity, 
                                                           group = Study,
                                                           colour = cut(x = c(P_ac_wide5$ac_IT_score, 
                                                                              P_ac_wide5$ac_PS_score, P_ac_wide5$ac_RS_score), 
                                                                        breaks = c(0,1,2,3), 
                                                                        labels = c("Low", "High", "Unclear"), 
                                                                        ordered_result = TRUE)),  
                                    col = c("ac_IT", "ac_PS", "ac_RS"), 
                                    size = 1, 
                                    pie_scale = pie_scale, 
                                    alpha = 0.5) +
              coord_equal() + 
              theme_bw() + 
              theme(legend.position = "bottom", 
                    legend.title = element_text(size=10), 
                    legend.text = element_text(size=10)
              ) + 
              scale_colour_manual("Risk", values = colours_group) + 
              scale_fill_manual(values = colours_category) + 
              guides(fill = FALSE)
            g
            
          }
          
          # Pie chart that shows both applicability concerns and risk of bias
          if ('11' %in% {input$QAcheck}) {
            P_rob_wide <- P_rob %>% spread(rob, score)  %>% mutate(Study = seq(from = 1, to = nrow(X)))
            P_rob_wide2 <- left_join(P_rob_wide, ss, by = "Study") 
            P_rob_wide3 <- dplyr::mutate(P_rob_wide2, 
                                         rob_FT_score = as.numeric(factor(rob_FT, levels = c(1,2,3))), 
                                         rob_IT_score = as.numeric(factor(rob_IT, levels = c(1,2,3))), 
                                         rob_PS_score = as.numeric(factor(rob_PS, levels = c(1,2,3))), 
                                         rob_RS_score = as.numeric(factor(rob_RS, levels = c(1,2,3))))
            P_rob_wide4 <-  dplyr::mutate(P_rob_wide3, 
                                          Study = seq(from = 1, to = nrow(X)), 
                                          Sensitivity = as.numeric(Sensitivity),  
                                          Specificity = as.numeric(Specificity), 
                                          FPR =  as.numeric(1 - Specificity), 
                                          rob_FT = rep(1, length = nrow(X)), 
                                          rob_IT = rep(1, length = nrow(X)),
                                          rob_PS = rep(1, length = nrow(X)),
                                          rob_RS = rep(1, length = nrow(X)), 
                                          rob_FT_colour = ifelse(rob_FT == 1, "green",
                                                                 ifelse(rob_FT == 2, "red", "grey")), 
                                          rob_IT_colour = ifelse(rob_IT == 1, "green",
                                                                 ifelse(rob_IT == 2, "red", "grey")),
                                          rob_PS_colour = ifelse(rob_PS == 1, "green",
                                                                 ifelse(rob_PS == 2, "red", "grey")),
                                          rob_RS_colour = ifelse(rob_RS == 1, "green",
                                                                 ifelse(rob_RS == 2, "red", "grey")))  %>%
              dplyr::select(Study, FPR, Sensitivity, 
                            rob_PS, rob_IT, rob_RS, rob_FT, 
                            rob_PS_score, rob_IT_score, rob_RS_score, rob_FT_score)
            
            P_rob_wide5 <- tibble(P_rob_wide4)
            P_ac_wide <- P_ac %>% spread(rob, score)  %>% mutate(Study = seq(from = 1, to = nrow(X)))
            P_ac_wide2 <- left_join(P_ac_wide, ss, by = "Study") 
            P_ac_wide3 <- dplyr::mutate(P_ac_wide2, 
                                        ac_IT_score = as.numeric(factor(ac_IT, levels = c(1,2,3))), 
                                        ac_PS_score = as.numeric(factor(ac_PS, levels = c(1,2,3))), 
                                        ac_RS_score = as.numeric(factor(ac_RS, levels = c(1,2,3))))
            
            P_ac_wide4 <-  dplyr::mutate(P_ac_wide3, 
                                         Study = seq(from = 1, to = nrow(X)), 
                                         Sensitivity = as.numeric(Sensitivity),  
                                         Specificity = as.numeric(Specificity), 
                                         FPR =  as.numeric(1 - Specificity), 
                                         ac_IT = rep(1, length = nrow(X)),
                                         ac_PS = rep(1, length = nrow(X)),
                                         ac_RS = rep(1, length = nrow(X)), 
                                         ac_IT_colour = ifelse(ac_IT == 1, "green",
                                                               ifelse(rob_IT == 2, "red", "grey")),
                                         ac_PS_colour = ifelse(ac_PS == 1, "green",
                                                               ifelse(rob_PS == 2, "red", "grey")),
                                         ac_RS_colour = ifelse(ac_RS == 1, "green",
                                                               ifelse(rob_RS == 2, "red", "grey")))  %>%
              dplyr::select(Study, FPR, Sensitivity, 
                            ac_PS, ac_IT, ac_RS,
                            ac_PS_score, ac_IT_score, ac_RS_score)
            
            P_ac_wide5 <- tibble(P_ac_wide4)
            
            P_rob_and_ac_wide <- left_join(P_ac_wide5, P_rob_wide5, by = "Study") %>% 
              dplyr::select(Study, FPR.x, Sensitivity.x, 
                            ac_PS, ac_IT, ac_RS,
                            ac_PS_score, ac_IT_score, ac_RS_score,
                            rob_PS, rob_IT, rob_RS, rob_FT, 
                            rob_PS_score, rob_IT_score, rob_RS_score, rob_FT_score) %>%
              dplyr::rename(FPR = FPR.x, Sensitivity = Sensitivity.x)
            
            P_rob_and_ac_wide
            
            colours_group = c("green", "red", "black")
            colours_category = c("white", "white", "white", "white", "white", "white", "white")
            
            
            # add little pie charts to plot
            g = g + geom_scatterpie(data = P_rob_and_ac_wide, aes(x =  FPR,
                                                                  y = Sensitivity, 
                                                                  group = Study,
                                                                  colour = cut(x = c(
                                                                    P_rob_and_ac_wide$rob_IT_score,
                                                                    P_rob_and_ac_wide$rob_FT_score,
                                                                    P_rob_and_ac_wide$ac_RS_score,
                                                                    P_rob_and_ac_wide$ac_IT_score,
                                                                    P_rob_and_ac_wide$ac_PS_score,
                                                                    P_rob_and_ac_wide$rob_RS_score,
                                                                    P_rob_and_ac_wide$rob_PS_score
                                                                  ), 
                                                                  breaks = c(0,1,2,3), 
                                                                  labels = c("Low", "High", "Unclear"), 
                                                                  ordered_result = TRUE)),  
                                    col = c("rob_IT", "rob_FT", "ac_RS", 
                                            "ac_IT", "ac_PS", "rob_RS", "rob_PS"), 
                                    size = 1, 
                                    pie_scale = pie_scale, 
                                    alpha = 1) +
              coord_equal() + 
              theme_bw() + 
              theme(
                #legend.position = "none", 
                legend.position = "bottom",
                legend.title = element_text(size=10), 
                legend.text = element_text(size=10)
              ) + 
              scale_colour_manual("Risk", values = colours_group) + 
              scale_fill_manual(values = colours_category) + 
              guides(fill = FALSE)
            
            g
            
          }
          
          g
          
          # Add CIs for sens and spec at study level
          if ('1' %in% {{input$cicheck}} & ({input$QAcheck} == 1 | {input$QAcheck} == 9 | {input$QAcheck} == 10 | {input$QAcheck} == 11)) {
            g = g + geom_errorbar(data = data_QA2,
                                  aes(ymin = Sens_LCI, ymax = Sens_UCI),
                                  width = 0.02, alpha = 1) 
            g
          }
          else { 
            g
          } 
          g
          
          if ('2' %in% {{input$cicheck}} & ({input$QAcheck} == 1 | {input$QAcheck} == 9 |{input$QAcheck} == 10 | {input$QAcheck} == 11)) {
            g = g + geom_errorbar(data = data_QA2,
                                  aes(xmin = FPR_LCI, xmax = FPR_UCI),
                                  width = 0.02, alpha = 1) 
            g
          } else { 
            g
          }
          g
          
          #  if one of the individual 7 QA items is selected by the user 
          for (i in 1:7) {
            if ('1' %in% {{input$cicheck}} & {input$QAcheck} == (i+1)) {
              g = g + geom_errorbar(data = data_QA2,
                                    aes(ymin = Sens_LCI, ymax = Sens_UCI,
                                        linetype = !!as.name(colnames(data_QA2)[6+i])),
                                    width = 0.02, alpha = 1) 
              g
            } else {
              g
            }
            g
            if ('2' %in% {{input$cicheck}} & {input$QAcheck} == (i+1)) {
              g = g + geom_errorbar(data = data_QA2,
                                    aes(xmin = FPR_LCI, xmax = FPR_UCI,
                                        linetype = !!as.name(colnames(data_QA2)[6+i])),
                                    width = 0.02, alpha = 1) 
            } else { 
              g
            }
          }
          
          g
          
        }   else if (C > 6 & Names[7] != "rob_PS") {   # sROC plot (ctd.) - (3)  Plots with covariate data, but no QA data ("Cov.csv")   ----------------
          cov_names <-  num_covariates(X)$combined[-1] # extract names of covariates
          cols_covariates <- dplyr::select(X, cov_names, year.cts, prevalence.cts) # columns containing only covariate data
          no_cov <- num_covariates(X)$no_covariates  # number of covariates
          ss <- obs_values(X)$ss
          ss <- obs_values(X)$ss %>% dplyr::mutate(pctse = X$pctse, pctsp = X$pctsp) # add study weights 
          ss <- cbind(ss, cols_covariates)
          
          # make covariates factors for display
          for (i in 1:no_cov) { 
            ss <- dplyr::mutate(ss, !!as.name(colnames(ss)[12+i]) := factor( !!as.name(colnames(ss)[12+i])))
          }
          
          
          # Calculate sens and spec confidence intervals at the study level
          # Add the confidence intervals to the dataset
          foreach (i = 1:n_studies) %do% {
            ss$Sens_LCI[i] <- binconf(ss$TP[i], ss$TP[i]+ss$FN[i], method="exact")[2]
            ss$Sens_UCI[i] <- binconf(ss$TP[i], ss$TP[i]+ss$FN[i], method="exact")[3]
            ss$FPR_LCI[i]  <- 1 - binconf(ss$TN[i], ss$FP[i]+ss$TN[i], method="exact")[3]
            ss$FPR_UCI[i]  <- 1 - binconf(ss$TN[i], ss$FP[i]+ss$TN[i], method="exact")[2]
          }
          
          for (i in 1:no_cov) {
            if ({input$covcheck_display} == i+1) { # the covariate selected by the user
              if (input$weightcheck == FALSE) { # without study weights
                if ({input$cov_toggle} == 3) { # plot covariates as text labels AND coloured points
                  g = g + geom_label_repel(data = ss,
                                           aes(label = !!as.name(colnames(ss)[12+i])) ) +
                    geom_point(data = ss,
                               size = size_study_specific,
                               inherit.aes = FALSE,
                               aes(x = 1 - Specificity, y = Sensitivity,
                                   colour = !!as.name(colnames(ss)[12+i])))  
                  g
                }
                else { g }
                if ({input$cov_toggle} == 2) {   # plot covariates as coloured points only
                  g = g + geom_point(data = ss,
                                     inherit.aes = FALSE,
                                     size = size_study_specific,
                                     aes(x = 1 - Specificity, y = Sensitivity,
                                         colour = !!as.name(colnames(ss)[12+i])))
                  g
                }
                else { g }
                if ({input$cov_toggle} == 1) {   # plot covariates as text labels only
                  g = g + geom_label_repel(data = ss,
                                           aes(label = !!as.name(colnames(ss)[12+i])) )
                  g
                }
                else { g }
              }
              
              if (input$weightcheck == TRUE) { # with study weights
                if ({input$cov_toggle} == 3) {  # plot covariates as text AND coloured ellipses
                  g = g + geom_label_repel(data = ss,
                                           aes(label = !!as.name(colnames(ss)[12+i])) ) +
                    geom_ellipse(data = ss,
                                 aes(x0 = 1-Specificity, y0 = Sensitivity,
                                     a=pctsp/weight_scale, b=pctse/weight_scale, angle = 0,
                                     colour = !!as.name(colnames(ss)[12+i])))
                  g
                }
                else { g }
                if ({input$cov_toggle} == 2) { # plot covariates as coloured ellipses only
                  g = g + geom_ellipse(data = ss,
                                       aes(x0 = 1-Specificity, y0 = Sensitivity,
                                           a=pctsp/weight_scale, b=pctse/weight_scale, angle = 0,
                                           colour = !!as.name(colnames(ss)[12+i])))
                  g
                }
                else { g }
                if ({{input$cov_toggle}} == 1) { # plot covariates as text only
                  g = g + geom_label_repel(data = ss,
                                           aes(label = !!as.name(colnames(ss)[12+i])))  +
                    geom_ellipse(data = ss,
                                 aes(x0 = 1-Specificity, y0 = Sensitivity,
                                     a=pctsp/weight_scale, b=pctse/weight_scale, angle = 0))
                  g
                }
                else { g }
              }
            }
          }
          g
          
          # plot sensitivity and specificity CI's
          # When no covariates are selected (sens)
          if ('1' %in% {input$cicheck} & {input$covcheck_display} == 1) {
            g = g + geom_errorbar(data = ss, aes(ymin = Sens_LCI, ymax = Sens_UCI),
                                  width = 0.02, alpha = 1)
            g
          }
          else { g }
          # When covariates are selected as text (sens)
          if ('1' %in% {input$cicheck} & {input$covcheck_display} != 1) {
            if ({input$cov_toggle} == 1) {
              g = g + geom_errorbar(data = ss, aes(ymin = Sens_LCI, ymax = Sens_UCI),
                                    width = 0.02, alpha = 1)
              g
            }
            else { g }
          }
          #When no covariates are selected (spec)
          if ('2' %in% {input$cicheck} & {input$covcheck_display} == 1) {
            g = g + geom_errorbar(data = ss, aes(xmin = FPR_LCI, xmax = FPR_UCI),
                                  width = 0.02, alpha = 1)
            g
          }
          else { g }
          # When covariates are selected as text (spec)
          if ('2' %in% {input$cicheck} & {input$covcheck_display} != 1) {
            if ({input$cov_toggle} == 1) {
              g = g + geom_errorbar(data = ss, aes(xmin = FPR_LCI, xmax = FPR_UCI),
                                    width = 0.02, alpha = 1)
              g
            }
            else { g }
          }
          g
          
          for(i in 1:no_cov) {
            # When covariates are selected as coloured points (sens)
            if ('1' %in% {input$cicheck} & {input$covcheck_display} == i+1) {
              if ({input$cov_toggle} == 2 | {input$cov_toggle} == 3) {
                g = g + geom_errorbar(data = ss,
                                      aes(ymin = Sens_LCI, ymax = Sens_UCI,
                                          colour = factor(!!as.name(colnames(ss)[12+i]))),
                                      width = 0.02, alpha = 1)
                g
              }
              else { g }
            }
            # When covariates are selected as coloured points (spec)
            if ('2' %in% {input$cicheck} & {input$covcheck_display} == i+1) {
              if (({input$cov_toggle} == 2 | {input$cov_toggle} == 3)) {
                g = g + geom_errorbar(data = ss, aes(xmin = FPR_LCI, xmax = FPR_UCI,
                                                     colour = factor(!!as.name(colnames(ss)[12+i]))),
                                      width = 0.02, alpha = 1)
              }
              else { g }
            }
          }
          g
          
        }   else if (C > 13 & Names[13] == "ac_RS")  {   #  sROC plot (ctd.) - (4) When QA AND covariate data is available ("QA_Cov.csv") ----------------
          # set up the data
          study_level <- study_level_outcomes(X)
          data_QA_Cov <- dplyr::left_join(X, study_level) %>%
            dplyr::mutate(rob_PS = factor(rob_PS, levels = c(1,2,3), labels = c("Low", "High", "Unclear")),
                          rob_IT = factor(rob_IT, levels = c(1,2,3), labels = c("Low", "High", "Unclear")),
                          rob_RS = factor(rob_RS, levels = c(1,2,3), labels = c("Low", "High", "Unclear")),
                          rob_FT = factor(rob_FT, levels = c(1,2,3), labels = c("Low", "High", "Unclear")),
                          ac_PS = factor(ac_PS, levels = c(1,2,3), labels = c("Low", "High", "Unclear")),
                          ac_IT = factor(ac_IT, levels = c(1,2,3), labels = c("Low", "High", "Unclear")),
                          ac_RS = factor(ac_RS, levels = c(1,2,3), labels = c("Low", "High", "Unclear")))
          data_QA_Cov2 <- left_join(data_QA_Cov, ss)
          
          
          
          # Reshape the data to allow for pie charts to be plotted as summary points
          # For ROB outcomes
          P_rob <- X[,-which(names(X) == "ac_PS" | names(X) == "ac_RS"| names(X) == "ac_IT")] # Delete applicability concerns for ROB plot
          P_rob <- reshape(P_rob, direction = "long", varying = c("rob_PS", "rob_IT", "rob_RS", "rob_FT"),
                           v.names = "score", timevar = "rob", times = c("rob_PS", "rob_IT", "rob_RS", "rob_FT"))
          # For AC outcomes
          P_ac <- X[,-which(names(X) == "rob_PS" | names(X) == "rob_RS"| names(X) == "rob_IT"|names(X) == "rob_FT")] # Delete applicability concerns for ROB plot
          P_ac <- reshape(P_ac, direction = "long", varying = c("ac_PS", "ac_IT", "ac_RS"),
                          v.names = "score", timevar = "rob", times = c("ac_PS", "ac_IT", "ac_RS"))
          # For both outcomes together
          P_both <- reshape(X, direction = "long", varying = c("rob_PS", "rob_IT", "rob_RS", "rob_FT", "ac_PS", "ac_IT", "ac_RS"),
                            v.names = "score", timevar = "rob", times = c("rob_PS", "rob_IT", "rob_RS", "rob_FT", "ac_PS", "ac_IT", "ac_RS"))
          
          ####
          # Plots
          ####
          ## plot QA items
          # Coloured study-level estimates based on 1 of the 7 outcomes of QA
          for (i in 1:7) { # i.e. if the QA item is rob_PS, rob_IT, rob_RS, rob_FT, ac_PS, ac_IT, ac_RS
            if ({input$QAcheck} == i+1) { # the selected QA  item (QA is on since in interval (2,8))
              if (input$weightcheck == FALSE) { # without study weights
                g =  g + geom_point(data = data_QA_Cov2,
                                    size = size_study_specific,
                                    aes(x = FPR, y = Sensitivity,
                                        shape = !!as.name(colnames(data_QA_Cov2)[6+i])))  
                g
              } else { # with study weights
                g =  g +  geom_ellipse(data = data_QA_Cov2, aes(x0 = 1-Specificity, y0 = Sensitivity,
                                                                linetype = !!as.name(colnames(data_QA_Cov2)[6+i]), 
                                                                a=pctsp/weight_scale, b=pctse/weight_scale, angle = 0))
                g
              }
            } else { 
              g
            }
          }
          
          if (C == 14) { # if only 1 covariate
            covariates <- cbind(as.character(X[,14]))
            no_cov <- 1
          }
          else { # if > 1 covariate
            covariates <- X[,14:C] # extract all covariates
            no_cov <- length(colnames(covariates)) # number of covariates
          }
          study_level <- cbind(study_level, covariates) # combine with study_level data frame
          
          # make covariates factors for display
          for (i in 1:no_cov) { 
            data_QA_Cov2 <- dplyr::mutate(data_QA_Cov2, !!as.name(colnames(data_QA_Cov2)[13+i]) := factor( !!as.name(colnames(data_QA_Cov2)[13+i])))
          }
          ### without study-level weights
          if (input$weightcheck == FALSE) { 
            for (i in 1:7) {
              if ({input$QAcheck} == i+1) {
                # Plot with QA items
                g =  g + geom_point(data = data_QA_Cov2,
                                    size = size_study_specific,
                                    aes(x = FPR, y = Sensitivity, 
                                        shape = !!as.name(colnames(data_QA_Cov2)[6+i]))) 
                g
              }
            }
            for (i in 1:no_cov) {
              if ({input$covcheck_display} == i+1) { # i.e. the user has a covariate selected 
                if ({input$cov_toggle} == 3) {   # plot covariates as text and coloured points
                  # add the text labels for the covariates first
                  g = g + geom_label_repel(data = data_QA_Cov2,
                                           size = size_study_specific,
                                           aes(label = !!as.name(colnames(data_QA_Cov2)[13+i])) )
                  g
                  if ({input$QAcheck} == 1) { # add the coloured points for covariates, when no QA is selected
                    g = g + geom_point(data = data_QA_Cov2,
                                       size = size_study_specific,
                                       aes(x = 1 - Specificity, y = Sensitivity,
                                           colour = !!as.name(colnames(data_QA_Cov2)[13+i]))) # colour for covariates 
                    g
                  }
                  else { #  i.e. QA is selected - add the coloured points for covariate level and shapes for QA items (need interaction)
                    for (j in 1:7) {
                      if ({input$QAcheck} == j+1) {
                        g =   g +   geom_point(data = data_QA_Cov2,
                                               size = size_study_specific,
                                               aes(x = FPR, y = Sensitivity,
                                                   group = interaction(!!as.name(colnames(data_QA_Cov2)[6+j]), !!as.name(colnames(data_QA_Cov2)[13+i])), 
                                                   shape = !!as.name(colnames(data_QA_Cov2)[6+j]),  # shape by the Risk level in given QA category
                                                   colour =  !!as.name(colnames(data_QA_Cov2)[13+i]))) 
                      }
                    }
                    g
                  }
                }
                if ({input$cov_toggle} == 2) {   # plot covariates as coloured points only
                  if ({input$QAcheck} == 1) { #  when no QA is selected
                    g = g + geom_point(data = data_QA_Cov2,
                                       size = size_study_specific,
                                       aes(x = 1 - Specificity, y = Sensitivity,
                                           colour = !!as.name(colnames(data_QA_Cov2)[13+i])))  # colour for covariates 
                    g
                  }
                  else { #  when QA is selected (using interaction for QA and covariates)
                    for (j in 1:7) {
                      if ({input$QAcheck} == j+1) {
                        g =   g +   geom_point(data = data_QA_Cov2,
                                               size = size_study_specific,
                                               aes(x = FPR, y = Sensitivity,
                                                   group = interaction(!!as.name(colnames(data_QA_Cov2)[6+j]), !!as.name(colnames(data_QA_Cov2)[13+i])), 
                                                   shape = !!as.name(colnames(data_QA_Cov2)[6+j]),  # shape by the Risk level in given QA category
                                                   colour =  !!as.name(colnames(data_QA_Cov2)[13+i]))) 
                        g
                      }
                    }
                  }
                }
                if ({input$cov_toggle} == 1) {   # plot covariates as text only
                  g =  g + geom_label_repel(data = data_QA_Cov2,
                                            aes(label = !!as.name(colnames(data_QA_Cov2)[13+i])) ) 
                  g
                }
                else { g }
              }
            }
          }
          
          if (input$weightcheck == TRUE) { # with study-level weights
            # linetype study level estimates based on 1 of the 7 outcomes of quality assessment
            for (i in 1:7) {
              if ({input$QAcheck} == i+1) {
                g =  g +   geom_ellipse(data = data_QA_Cov2, aes(x0 = 1-Specificity, y0 = Sensitivity,
                                                                 linetype = !!as.name(colnames(data_QA_Cov2)[6+i]), 
                                                                 a=pctsp/weight_scale, b=pctse/weight_scale, angle = 0))
                g
              }
              else { g }
            }
            
            for (i in 1:no_cov) {
              if ({input$covcheck_display} == i+1) { # i.e. the user has selected a covariate to plot 
                if ({input$cov_toggle} == 3) { # plot covariates as text and coloured points
                  # add the text labels first
                  g = g + geom_label_repel(data = data_QA_Cov2,
                                           aes(label = !!as.name(colnames(data_QA_Cov2)[13+i])) )
                  g
                  if ({input$QAcheck} == 1) { # When no QA is selected - ellipses (coloured by covariate)
                    g =  g +     geom_ellipse(data = data_QA_Cov2,
                                              aes(x0 = 1-Specificity, y0 = Sensitivity,
                                                  a=pctsp/weight_scale, b=pctse/weight_scale, angle = 0,
                                                  colour = !!as.name(colnames(data_QA_Cov2)[13+i])))
                    g
                  }
                  else { # When QA is selected - ellipses coloured by covriate + linetype by QA risk level 
                    g =  g + geom_ellipse(data = data_QA_Cov2,
                                          aes(x0 = 1-Specificity, y0 = Sensitivity,
                                              a=pctsp/weight_scale, b=pctse/weight_scale, angle = 0,
                                              group = interaction(!!as.name(colnames(data_QA_Cov2)[6+i]), !!as.name(colnames(data_QA_Cov2)[13+i])), 
                                              colour =  !!as.name(colnames(data_QA_Cov2)[13+i]),
                                              linetype = !!as.name(colnames(data_QA_Cov2)[6+i])))
                    g
                  }
                }
                if ({input$cov_toggle} == 2) { # plot covariates as coloured points only
                  if ({input$QAcheck} == 1) {  # When no QA is selected
                    g =  g + geom_ellipse(data = data_QA_Cov2,
                                          aes(x0 = 1-Specificity, y0 = Sensitivity,
                                              a=pctsp/weight_scale, b=pctse/weight_scale, angle = 0,
                                              colour = !!as.name(colnames(data_QA_Cov2)[13+i])))
                    g
                  }
                  else { # When QA is selected - ellipses coloured by covriate + linetype by QA risk level 
                    g =  g + geom_ellipse(data = data_QA_Cov2,
                                          aes(x0 = 1-Specificity, y0 = Sensitivity,
                                              a=pctsp/weight_scale, b=pctse/weight_scale, angle = 0,
                                              group = interaction(!!as.name(colnames(data_QA_Cov2)[6+i]), !!as.name(colnames(data_QA_Cov2)[13+i])), 
                                              colour =  !!as.name(colnames(data_QA_Cov2)[13+i]),
                                              linetype = !!as.name(colnames(data_QA_Cov2)[6+i])))
                    g
                  }
                }
                if ({input$cov_toggle} == 1) {  # plot covariates as text labels only
                  g = g + geom_label_repel(data = data_QA_Cov2,
                                           aes(label = !!as.name(colnames(data_QA_Cov2)[13+i])) )
                  g
                  
                }
                else { g }
              }
            }
          }
          
          g
          
          ####
          # Add CI's for sens and spec at study level
          ####
          # set the linetypes
          linetypes <- c(1,5,7)
          scales_for_linetypes <- scale_linetype_manual(values = linetypes)
          
          
          # CI's for Sens
          if ('1' %in% {input$cicheck}) {
            # plot sensitivity CI's when NO COVARIATES selected and  no QA (except piercharts)
            if ({input$covcheck_display} == 1 & ({input$QAcheck} == 1 | {input$QAcheck} == 9 |{input$QAcheck} == 10 |{input$QAcheck} == 11)) {
              g = g + geom_errorbar(data = data_QA_Cov2,
                                    aes(ymin = Sens_LCI, 
                                        ymax = Sens_UCI),
                                    width = 0.02, alpha = 1) 
              g
            }
            else { g }
            # plot sensitivity CI's when covariates only dispalyed as text, no QA (except piercharts)
            if ({input$covcheck_display} != 1 & ({input$QAcheck} == 1 | {input$QAcheck} == 9 |{input$QAcheck} == 10 |{input$QAcheck} == 11)) {
              if ({input$cov_toggle} == 1) { # i.e. covariates displayed as text only
                g = g + geom_errorbar(data = data_QA_Cov2,
                                      aes(ymin = Sens_LCI, 
                                          ymax = Sens_UCI),
                                      width = 0.02, alpha = 1)
                g
              }
              else { g }
            }
            else { g }
            # Plot sensitivity CI's with QA only, NO COVARIATES
            for (i in 1:7) {
              if ({input$covcheck_display} == 1 & {input$QAcheck} == (i+1) ) {
                g = g + geom_errorbar(data = data_QA_Cov2,
                                      aes(ymin = Sens_LCI, ymax = Sens_UCI,
                                          linetype = !!as.name(colnames(data_QA_Cov2)[6+i])),
                                      width = 0.02, alpha = 1) #+ 
                # scales_for_linetypes
                g
              }
              else { g }
            }
            
            # Plot sensitivity CI's when QA is selected, and covariates displayed as TEXT ONLY
            for (i in 1:7) {
              if ({input$covcheck_display} != 1 & {input$QAcheck} == (i+1) ) {
                if ({input$cov_toggle} == 1) {
                  g = g + geom_errorbar(data = data_QA_Cov2,
                                        aes(ymin = Sens_LCI, ymax = Sens_UCI,
                                            linetype = !!as.name(colnames(data_QA_Cov2)[6+i])),
                                        width = 0.02, alpha = 1) #+ 
                  #   scales_for_linetypes
                  g
                }
                else { g }
              }
            }
            
            # Plot sensitivity CI's when covariate is selected, no QA (except pie charts)
            for (i in 1:no_cov) {
              if (({input$QAcheck} == 1 |{input$QAcheck} == 9 |{input$QAcheck} == 10 |{input$QAcheck} == 11) & {input$covcheck_display} == i+1) {
                if ({input$cov_toggle} == 2 | {input$cov_toggle} == 3) { # i.e. covariate selected as coloured only or coloured+text
                  g = g + geom_errorbar(data = data_QA_Cov2,
                                        aes(ymin = Sens_LCI, ymax = Sens_UCI,
                                            colour = !!as.name(colnames(data_QA_Cov2)[13+i])),
                                        width = 0.02, alpha = 1)
                  g
                }
                else { g }
              }
            }
            g
            
            str(data_QA_Cov2)
            
            # Plot sensitivity CI's when BOTH Cov and QA both selected 
            if ({input$covcheck_display} != 1 & 
                ({input$QAcheck} == 2 |{input$QAcheck} == 3 | {input$QAcheck} == 4 |{input$QAcheck} == 5 | {input$QAcheck} == 6 |{input$QAcheck} == 7 | {input$QAcheck} == 8)) {
              if ( {input$cov_toggle} == 2 | {input$cov_toggle} == 3) {
                for (i in 1:no_cov) {
                  if ({input$covcheck_display} == i+1) { # covariate selected
                    g = g + geom_errorbar(data = data_QA_Cov2,
                                          aes(ymin = Sens_LCI, ymax = Sens_UCI, 
                                              colour = !!as.name(colnames(data_QA_Cov2)[13+i])), # colour for QA
                                          width = 0.02, alpha = 1) 
                    g
                  }
                  else { g }
                }
                for (i in 1:7) { 
                  if ({input$QAcheck} == (i+1) ) {
                    g = g + geom_errorbar(data = data_QA_Cov2,
                                          aes(ymin = Sens_LCI, ymax = Sens_UCI,
                                              linetype = !!as.name(colnames(data_QA_Cov2)[6+i])),
                                          width = 0.02, alpha = 1) #+ 
                    # scales_for_linetypes
                    g
                  }
                  else { g }
                }
              }
            }
          } # end of Sens CI
          
          g
          
          ####
          # CI's for spec
          if ('2' %in% {input$cicheck}) {
            # plot specificity CI's when no covariates selected and no QA (except pie charts)
            if ({input$covcheck_display} == 1 & ({input$QAcheck} == 1 | {input$QAcheck} == 9 |{input$QAcheck} == 10 |{input$QAcheck} == 11)) {
              g = g + geom_errorbar(data = data_QA_Cov2,
                                    aes(xmin = FPR_LCI, xmax = FPR_UCI),
                                    width = 0.02, alpha = 1)
              g
            }
            else { g }
            
            # plot specificity CI's when covariates only displayed as text, no QA (except pie charts)
            if ({input$covcheck_display} != 1 & ({input$QAcheck} == 1 | {input$QAcheck} == 9 |{input$QAcheck} == 10 |{input$QAcheck} == 11)) {
              if ({input$cov_toggle} == 1) {
                g = g + geom_errorbar(data = data_QA_Cov2,
                                      aes(xmin = FPR_LCI, xmax = FPR_UCI),
                                      width = 0.02, alpha = 0.4)
                g
              }
              else { g }
            }
            
            # plot specificity CI's with QA only, no covariates
            for (i in 1:7) {
              if ({input$covcheck_display} == 1 & {input$QAcheck} == (i+1) ) {
                g = g + geom_errorbar(data = data_QA_Cov2,
                                      aes(xmin = FPR_LCI, xmax = FPR_UCI,
                                          colour = !!as.name(colnames(data_QA_Cov2)[6+i])),
                                      width = 0.02, alpha = 1) 
                g
              }
              else { g }
            }
            
            # plot specificity CI's when QA selected, and covariates displayed as TEXT ONLY
            for (i in 1:7) {
              if ({input$covcheck_display} != 1 & {input$QAcheck} == (i+1) ) {
                if ({input$cov_toggle} == 1) {
                  g = g + geom_errorbar(data = data_QA_Cov2,
                                        aes(xmin = FPR_LCI, xmax = FPR_UCI,
                                            colour = !!as.name(colnames(data_QA_Cov2)[6+i])),
                                        width = 0.02, alpha = 1) +
                    geom_label_repel(data = data_QA_Cov2,
                                     aes(label = !!as.name(colnames(data_QA_Cov2)[13+i])))  
                  g
                }
                else { g }
              }
            }
            
            # plot spec CI's when covariate selected as coloured points, and no QA (except pie charts)
            for (i in 1:no_cov) {
              if (({input$QAcheck} == 1 |{input$QAcheck} == 9 |{input$QAcheck} == 10 |{input$QAcheck} == 11) & {input$covcheck_display} == i+1) {
                if ({input$cov_toggle} == 2 | {input$cov_toggle} == 3) {
                  g = g + geom_errorbar(data = data_QA_Cov2,
                                        aes(xmin = FPR_LCI, xmax = FPR_UCI,
                                            colour = !!as.name(colnames(data_QA_Cov2)[13+i])),
                                        width = 0.02, alpha = 1) 
                  g
                }
                else { g }
              }
            }
            
            # Plot spec CI's when BOTH Cov and QA both selected 
            if ({input$covcheck_display} != 1 & 
                ({input$QAcheck} == 2 |{input$QAcheck} == 3 | {input$QAcheck} == 4 |{input$QAcheck} == 5 | {input$QAcheck} == 6 |{input$QAcheck} == 7 | {input$QAcheck} == 8)) {
              if ( {input$cov_toggle} == 2 | {input$cov_toggle} == 3) {
                for (i in 1:no_cov) {
                  if ({input$covcheck_display} == i+1) { # covariate selected
                    g = g + geom_errorbar(data = data_QA_Cov2,
                                          aes(xmin = FPR_LCI, xmax = FPR_UCI,
                                              colour = !!as.name(colnames(data_QA_Cov2)[13+i])),
                                          width = 0.02, alpha = 1) 
                    g
                  }
                  else { g }
                }
                for (i in 1:7) { 
                  if ({input$QAcheck} == (i+1) ) {
                    g = g + geom_errorbar(data = data_QA_Cov2,
                                          aes(xmin = FPR_LCI, xmax = FPR_UCI,
                                              linetype = !!as.name(colnames(data_QA_Cov2)[6+i])),
                                          width = 0.02, alpha = 1) 
                    g
                  }
                  else { g }
                }
              }
            }
          } # end of Spec CI's
          
          g
          
          
          #### PIE CHARTS 
          #       # Pie charts to represent study level estimates split into risk of bias and applicability concerns
          if ('9' %in% {input$QAcheck}) {  #       # for RoB
            for (i in 1:max(P_rob$id)) {
              P_rob_wide <- P_rob %>% spread(rob, score)  %>% mutate(Study = seq(from = 1, to = nrow(X)))
              P_rob_wide2 <- left_join(P_rob_wide, ss, by = "Study") 
              P_rob_wide3 <- dplyr::mutate(P_rob_wide2, 
                                           rob_FT_score = as.numeric(factor(rob_FT, levels = c(1,2,3))), 
                                           rob_IT_score = as.numeric(factor(rob_IT, levels = c(1,2,3))), 
                                           rob_PS_score = as.numeric(factor(rob_PS, levels = c(1,2,3))), 
                                           rob_RS_score = as.numeric(factor(rob_RS, levels = c(1,2,3))))
              P_rob_wide4 <-  dplyr::mutate(P_rob_wide3, 
                                            Study = seq(from = 1, to = nrow(X)), 
                                            Sensitivity = as.numeric(Sensitivity),  
                                            Specificity = as.numeric(Specificity), 
                                            FPR =  as.numeric(1 - Specificity), 
                                            rob_FT = rep(1, length = nrow(X)), 
                                            rob_IT = rep(1, length = nrow(X)),
                                            rob_PS = rep(1, length = nrow(X)),
                                            rob_RS = rep(1, length = nrow(X)), 
                                            rob_FT_colour = ifelse(rob_FT == 1, "green",
                                                                   ifelse(rob_FT == 2, "red", "grey")), 
                                            rob_IT_colour = ifelse(rob_IT == 1, "green",
                                                                   ifelse(rob_IT == 2, "red", "grey")),
                                            rob_PS_colour = ifelse(rob_PS == 1, "green",
                                                                   ifelse(rob_PS == 2, "red", "grey")),
                                            rob_RS_colour = ifelse(rob_RS == 1, "green",
                                                                   ifelse(rob_RS == 2, "red", "grey")))  %>%
                dplyr::select(Study, FPR, Sensitivity, 
                              rob_PS, rob_IT, rob_RS, rob_FT, 
                              rob_PS_score, rob_IT_score, rob_RS_score, rob_FT_score)
              P_rob_wide5 <- tibble(P_rob_wide4)
              
              colours_group = c("green", "red", "black")
              colours_category = c("white", "white", "white", "white")
              
              # add little pie charts to plot
              g = g + geom_scatterpie(data = P_rob_wide5, aes(x =  FPR,
                                                              y = Sensitivity, 
                                                              group = Study,
                                                              colour = cut(x = c(P_rob_wide5$rob_FT_score, P_rob_wide5$rob_IT_score, 
                                                                                 P_rob_wide5$rob_PS_score, P_rob_wide5$rob_RS_score), 
                                                                           breaks = c(0,1,2,3), 
                                                                           labels = c("Low", "High", "Unclear"), 
                                                                           ordered_result = TRUE)),  
                                      col = c("rob_FT", "rob_IT", "rob_PS", "rob_RS"), 
                                      size = 1, 
                                      pie_scale = pie_scale, 
                                      alpha = 0.5) +
                coord_equal() + 
                theme_bw() + 
                theme(legend.position = "bottom", 
                      legend.title = element_text(size=10), 
                      legend.text = element_text(size=10)
                ) + 
                scale_colour_manual("Risk", values = colours_group) + 
                scale_fill_manual("Risk of Bias \ncategory", values = colours_category, 
                                  labels= c("Flow and \nTiming", "Index \ntest", 
                                            "Patient \nselection", "Reference \nstandard")) + 
                guides(fill = FALSE)
              g
              
            }
          }
          
          # Pie charts for applicability concerns
          if ('10' %in% {input$QAcheck}) {
            P_ac_wide <- P_ac %>% spread(rob, score)  %>% mutate(Study = seq(from = 1, to = nrow(X)))
            P_ac_wide2 <- left_join(P_ac_wide, ss, by = "Study") 
            P_ac_wide3 <- dplyr::mutate(P_ac_wide2, 
                                        ac_IT_score = as.numeric(factor(ac_IT, levels = c(1,2,3))), 
                                        ac_PS_score = as.numeric(factor(ac_PS, levels = c(1,2,3))), 
                                        ac_RS_score = as.numeric(factor(ac_RS, levels = c(1,2,3))))
            P_ac_wide4 <-  dplyr::mutate(P_ac_wide3, 
                                         Study = seq(from = 1, to = nrow(X)), 
                                         Sensitivity = as.numeric(Sensitivity),  
                                         Specificity = as.numeric(Specificity), 
                                         FPR =  as.numeric(1 - Specificity), 
                                         ac_IT = rep(1, length = nrow(X)),
                                         ac_PS = rep(1, length = nrow(X)),
                                         ac_RS = rep(1, length = nrow(X)), 
                                         ac_IT_colour = ifelse(ac_IT == 1, "green",
                                                               ifelse(rob_IT == 2, "red", "grey")),
                                         ac_PS_colour = ifelse(ac_PS == 1, "green",
                                                               ifelse(rob_PS == 2, "red", "grey")),
                                         ac_RS_colour = ifelse(ac_RS == 1, "green",
                                                               ifelse(rob_RS == 2, "red", "grey")))  %>%
              dplyr::select(Study, FPR, Sensitivity, 
                            ac_PS, ac_IT, ac_RS,
                            ac_PS_score, ac_IT_score, ac_RS_score)
            P_ac_wide5 <- tibble(P_ac_wide4)
            
            colours_group = c("green", "red", "black")
            colours_category = c("white", "white", "white")
            
            # add little pie charts to plot
            g = g + geom_scatterpie(data = P_ac_wide5, aes(x =  FPR,
                                                           y = Sensitivity, 
                                                           group = Study,
                                                           colour = cut(x = c(P_ac_wide5$ac_IT_score, 
                                                                              P_ac_wide5$ac_PS_score, P_ac_wide5$ac_RS_score), 
                                                                        breaks = c(0,1,2,3), 
                                                                        labels = c("Low", "High", "Unclear"), 
                                                                        ordered_result = TRUE)),  
                                    col = c("ac_IT", "ac_PS", "ac_RS"), 
                                    size = 1, 
                                    pie_scale = pie_scale, 
                                    alpha = 0.5) +
              coord_equal() + 
              theme_bw() + 
              theme(legend.position = "bottom", 
                    legend.title = element_text(size=10), 
                    legend.text = element_text(size=10)
              ) + 
              scale_colour_manual("Risk", values = colours_group) + 
              scale_fill_manual("Applicability concern \ncategory", values = colours_category, 
                                labels= c("Index \ntest", 
                                          "Patient \nselection", 
                                          "Reference \nstandard")) + 
              guides(fill = FALSE)
            g
          }
          
        }
        # Pie chart that shows both applicability concerns and risk of bias
        if ('11' %in% {input$QAcheck}) {
          for (i in 1:max(P_both$id)) {
            
            P_rob_wide <- P_rob %>% spread(rob, score)  %>% mutate(Study = seq(from = 1, to = nrow(X)))
            P_rob_wide2 <- left_join(P_rob_wide, ss, by = "Study") 
            P_rob_wide3 <- dplyr::mutate(P_rob_wide2, 
                                         rob_FT_score = as.numeric(factor(rob_FT, levels = c(1,2,3))), 
                                         rob_IT_score = as.numeric(factor(rob_IT, levels = c(1,2,3))), 
                                         rob_PS_score = as.numeric(factor(rob_PS, levels = c(1,2,3))), 
                                         rob_RS_score = as.numeric(factor(rob_RS, levels = c(1,2,3))))
            P_rob_wide4 <-  dplyr::mutate(P_rob_wide3, 
                                          Study = seq(from = 1, to = nrow(X)), 
                                          Sensitivity = as.numeric(Sensitivity),  
                                          Specificity = as.numeric(Specificity), 
                                          FPR =  as.numeric(1 - Specificity), 
                                          rob_FT = rep(1, length = nrow(X)), 
                                          rob_IT = rep(1, length = nrow(X)),
                                          rob_PS = rep(1, length = nrow(X)),
                                          rob_RS = rep(1, length = nrow(X)), 
                                          rob_FT_colour = ifelse(rob_FT == 1, "green",
                                                                 ifelse(rob_FT == 2, "red", "grey")), 
                                          rob_IT_colour = ifelse(rob_IT == 1, "green",
                                                                 ifelse(rob_IT == 2, "red", "grey")),
                                          rob_PS_colour = ifelse(rob_PS == 1, "green",
                                                                 ifelse(rob_PS == 2, "red", "grey")),
                                          rob_RS_colour = ifelse(rob_RS == 1, "green",
                                                                 ifelse(rob_RS == 2, "red", "grey")))  %>%
              dplyr::select(Study, FPR, Sensitivity, 
                            rob_PS, rob_IT, rob_RS, rob_FT, 
                            rob_PS_score, rob_IT_score, rob_RS_score, rob_FT_score)
            
            P_rob_wide5 <- tibble(P_rob_wide4)
            P_ac_wide <- P_ac %>% spread(rob, score)  %>% mutate(Study = seq(from = 1, to = nrow(X)))
            P_ac_wide2 <- left_join(P_ac_wide, ss, by = "Study") 
            P_ac_wide3 <- dplyr::mutate(P_ac_wide2, 
                                        ac_IT_score = as.numeric(factor(ac_IT, levels = c(1,2,3))), 
                                        ac_PS_score = as.numeric(factor(ac_PS, levels = c(1,2,3))), 
                                        ac_RS_score = as.numeric(factor(ac_RS, levels = c(1,2,3))))
            
            P_ac_wide4 <-  dplyr::mutate(P_ac_wide3, 
                                         Study = seq(from = 1, to = nrow(X)), 
                                         Sensitivity = as.numeric(Sensitivity),  
                                         Specificity = as.numeric(Specificity), 
                                         FPR =  as.numeric(1 - Specificity), 
                                         ac_IT = rep(1, length = nrow(X)),
                                         ac_PS = rep(1, length = nrow(X)),
                                         ac_RS = rep(1, length = nrow(X)), 
                                         ac_IT_colour = ifelse(ac_IT == 1, "green",
                                                               ifelse(rob_IT == 2, "red", "grey")),
                                         ac_PS_colour = ifelse(ac_PS == 1, "green",
                                                               ifelse(rob_PS == 2, "red", "grey")),
                                         ac_RS_colour = ifelse(ac_RS == 1, "green",
                                                               ifelse(rob_RS == 2, "red", "grey")))  %>%
              dplyr::select(Study, FPR, Sensitivity, 
                            ac_PS, ac_IT, ac_RS,
                            ac_PS_score, ac_IT_score, ac_RS_score)
            
            P_ac_wide5 <- tibble(P_ac_wide4)
            
            P_rob_and_ac_wide <- left_join(P_ac_wide5, P_rob_wide5, by = "Study") %>% 
              dplyr::select(Study, FPR.x, Sensitivity.x, 
                            ac_PS, ac_IT, ac_RS,
                            ac_PS_score, ac_IT_score, ac_RS_score,
                            rob_PS, rob_IT, rob_RS, rob_FT, 
                            rob_PS_score, rob_IT_score, rob_RS_score, rob_FT_score) %>%
              dplyr::rename(FPR = FPR.x, Sensitivity = Sensitivity.x)
            
            P_rob_and_ac_wide
            
            colours_group = c("green", "red", "black")
            colours_category = c("white", "white", "white", "white", "white", "white", "white")
            
            # add little pie charts to plot
            g = g + geom_scatterpie(data = P_rob_and_ac_wide, aes(x =  FPR,
                                                                  y = Sensitivity, 
                                                                  group = Study,
                                                                  colour = cut(x = c(
                                                                    P_rob_and_ac_wide$rob_IT_score,
                                                                    P_rob_and_ac_wide$rob_FT_score,
                                                                    P_rob_and_ac_wide$ac_RS_score,
                                                                    P_rob_and_ac_wide$ac_IT_score,
                                                                    P_rob_and_ac_wide$ac_PS_score,
                                                                    P_rob_and_ac_wide$rob_RS_score,
                                                                    P_rob_and_ac_wide$rob_PS_score
                                                                  ), 
                                                                  breaks = c(0,1,2,3), 
                                                                  labels = c("Low", "High", "Unclear"), 
                                                                  ordered_result = TRUE)),  
                                    col = c("rob_IT", "rob_FT", "ac_RS", 
                                            "ac_IT", "ac_PS", "rob_RS", "rob_PS"), 
                                    size = 1, 
                                    pie_scale = pie_scale, 
                                    alpha = 1) +
              coord_equal() + 
              theme_bw() + 
              theme(
                #legend.position = "none", 
                legend.position = "bottom",
                legend.title = element_text(size=10), 
                legend.text = element_text(size=10)
              ) + 
              scale_colour_manual("Risk", values = colours_group) + 
              scale_fill_manual(values = colours_category) + 
              guides(fill = FALSE)
            
            g
            
          }
        }
        
        g
        
        
      })
      
      
      # Output ggplot object
      observe({
        output$plot <- renderPlot({  
          
          validate(
            need(!(is.null(draws())), "Please run model to display plot")
          )
          
          req(draws(), cancelOutput = TRUE)
          
          plot_object()
          
        }, height = as.numeric({input$plot_dimension_slider}))
      })
      
      # Download ggplot object 
      output$plot_download <- downloadHandler(
        filename = function() {
          paste("plot.png")
        },
        content = function(file) { 
          ggsave(file,
                 plot_object(),
                 width = input$plot_width,
                 height = input$plot_height,
                 dpi = input$plot_dpi,
                 units = "in")
        }
      )
      
    # tooltips 
      click <- reactive({
        input$plot_click
      })
      
      
      # values to display on plot
      output$vals <- renderPrint({
        
        validate(
          need(!(is.null(draws())), "Please run model to display plot")
        )
        
        
        req(data(),  draws(), cancelOutput = TRUE)
        
        X <- data()
        X <- data.frame(X)
        
        C <- ncol(dplyr::select(X, -year.cts, -prevalence.cts))
        Names <- colnames(dplyr::select(X, -year.cts, -prevalence.cts))
        
        data1 <- data_summary_for_display(X)
        data2 <- MA_weights(X, draws()) # add the study weights
        
        data <- dplyr::mutate(data1,
                              pct_se_weight = data2$pctse,
                              pct_sp_weight = data2$pctsp)
        
        data <- data1
        
        data[] <- lapply(data, function(x) if (is.numeric(x) ) round(x, 3) else x) # round all numeric cols to 3 s.f.
        
        data <- data.frame(data)
        
        y <- nearPoints(df = data, click(), xvar = "FPR", yvar = "Sensitivity" )
        req(nrow(y) != 0)
        
        y
        
      })
      
      # tooltip
      output$my_tooltip <- renderUI({
        
        # validate(
        #   need(!(is.null(draws())), "Please run model to display plot")
        # )
        # 
        
        req(data(),  draws(), cancelOutput = TRUE)
        
        ns <- session$ns
        
        X <- data()
        X <- data.frame(X)
        
        C <- ncol(dplyr::select(X, -year.cts, -prevalence.cts))
        Names <- colnames(dplyr::select(X, -year.cts, -prevalence.cts))
        
        data1 <- data_summary_for_display(X)
        data2 <- MA_weights(X, draws()) # add the study weights
        
        
        data <- dplyr::mutate(data1,
                              pct_se_weight = data2$pctse,
                              pct_sp_weight = data2$pctsp)
        
        data <- data1
        
        data[] <- lapply(data, function(x) if (is.numeric(x) ) round(x, 3) else x) # round all numeric cols to 3 s.f.
        
        data <- data.frame(data)
        
        y <- nearPoints(df = data, click() , xvar = "FPR", yvar = "Sensitivity" )
        req(nrow(y) != 0)
        
        verbatimTextOutput(ns("vals"))
        
      })
      
      # Pop-up of study-specific pie charts onto sROC plot, when study is clicked on --------------
      output$piechart <- renderPlot({
        
        validate(
          need(!(is.null(draws())), "Please run model to display plot")
        )
        
        
        req(data(), draws(), input$plot_click, cancelOutput = TRUE)
        
        X <- req(data())
        e <- study_level_outcomes(X)
        
        if ('9' %in% input$QAcheck) {
          ee <- data.frame(Author=X$author,Year=X$year, TP=X$TP, FN=X$FN, FP=X$FP, 
                           TN=X$TN, Sensitivity=e$Sensitivity, Specificity=e$Specificity, 
                           FPR=e$FPR, 
                           rob_FT=X$rob_FT, 
                           rob_IT=X$rob_IT, 
                           rob_PS=X$rob_PS, 
                           rob_RS=X$rob_RS) 
          
          clickselect_pc <- nearPoints(ee, input$plot_click , xvar="FPR", yvar="Sensitivity", threshold=5, maxpoints=1)
          clickselect_pc <- reshape(clickselect_pc, direction = "long", 
                                    varying = c("rob_FT", "rob_RS", "rob_PS", "rob_IT"), 
                                    v.names = "score", 
                                    timevar = "rob", 
                                    times = c("rob_FT", "rob_RS", "rob_PS", "rob_IT"))
          labels_rob <- c("Flow & timing",  "Reference standard" , "Patient selection","Index test" )
          weight <-c(1,1,1,1)
          pie(weight, labels_rob, main = "Risk of bias for each domain from the selected study",
              col=ifelse(clickselect_pc$score == 1, "green", ifelse(clickselect_pc$score == 2, "red", "black")))
          legend(0.9,0.1, c("Low", "High", "Unclear"), cex =0.7, fill = c("green", "red", "black"))
        }
        
        if ('10' %in% input$QAcheck) {
          ee <- data.frame(Author=X$author,Year=X$year, TP=X$TP, FN=X$FN, FP=X$FP, 
                           TN=X$TN, Sensitivity=e$Sensitivity, Specificity=e$Specificity, 
                           FPR=e$FPR, ac_PS=X$ac_PS, ac_IT=X$ac_IT, ac_RS=X$ac_RS)
          
          clickselect_pc <- nearPoints(ee, input$plot_click, xvar="FPR", yvar="Sensitivity", threshold=5, maxpoints=1)
          clickselect_pc <- reshape(clickselect_pc, direction = "long", varying = c("ac_PS", "ac_IT", "ac_RS"), 
                                    v.names = "score", timevar = "rob", times = c("ac_PS", "ac_IT", "ac_RS"))
          labels_ac <- c("Patient selection", "Index test",  "Reference standard")
          weight <-c(1,1,1)
          pie(weight, labels_ac, main = "Applicability concern for each domain from the selected study",
              init.angle = -150,
              col=ifelse(clickselect_pc$score == 1, "green", ifelse(clickselect_pc$score == 2, "red", "black")))
          legend(0.9,0.1, c("Low", "High", "Unclear"), cex =0.7, fill = c("green", "red", "black"))
        }
        
        if ('11' %in% input$QAcheck) {
          ee <- data.frame(Author=X$author,Year=X$year, TP=X$TP, FN=X$FN, FP=X$FP, 
                           TN=X$TN, Sensitivity=e$Sensitivity, Specificity=e$Specificity, 
                           FPR=e$FPR, rob_PS=X$rob_PS, rob_IT=X$rob_IT, rob_RS=X$rob_RS, rob_FT=X$rob_FT,
                           ac_PS=X$ac_PS, ac_IT=X$ac_IT, ac_RS=X$ac_RS)
          
          clickselect_pc <- nearPoints(ee, input$plot_click, xvar="FPR", yvar="Sensitivity", threshold=5, maxpoints=1)
          clickselect_pc <-reshape(clickselect_pc, direction = "long", varying = c("rob_FT", "rob_IT", "rob_PS", "rob_RS", "ac_PS", "ac_IT", "ac_RS"),
                                   v.names = "score", timevar = "rob", times = c("rob_FT", "rob_IT", "rob_PS", "rob_RS", "ac_PS", "ac_IT", "ac_RS"))
          labels_both <- c("rob_FT", "rob_IT", "rob_PS", "rob_RS", "ac_PS", "ac_IT", "ac_RS")
          #weight <- c(1.25, 1.25, 1.25, 1.25, 5/3, 5/3, 5/3) # weight for pie chart split in half
          weight <- c(1,1,1,1,1,1,1) # weight where each section is equally split
          pie(weight, labels_both, main = "Scores from each element of the QUADAS-2 tool",
              init.angle = -10,
              col=ifelse(clickselect_pc$score == 1, "green", ifelse(clickselect_pc$score == 2, "red", "black")))
          legend(0.9,0.1, c("Low", "High", "Unclear"), cex =0.7, fill = c("green", "red", "black"))
        }
        
      })
      
      
    }
    
  )
}












