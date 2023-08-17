
# LCM - sROC plot code ------------------------------------------------------------------------------------------------------------------------------


# LCM - UI function to output sROC plot settings menu -------------------------------------------------  -----------------------------------------
LCM_sroc_plot_settings_menu_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sliderInput(inputId = ns("plot_dimension_slider"), 
                label = "Plot size", 
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
                value = 2.5),
    awesomeCheckboxGroup(inputId = ns("HSROCcheck"), 
                       label = " ",
                       choices = list("Model-estimated study-specific estimates for index test" = 1, 
                                      "Model-estimated study-specific estimates for reference test(s)" = 2, 
                                      "sROC curve for index test" = 3, 
                                      "sROC curve for reference test(s)" = 4, 
                                      "set x and y-axis limits to (0,1)"= 5),
                       selected=list(1)),
    uiOutput(ns("HSROC_curve_type_ref_ui")),
    uiOutput(ns("HSROC_curve_type_index_ui")),
    uiOutput(ns("SA_display_conditional_plot_checkbox_ui")),
    awesomeCheckboxGroup(inputId = ns("cicheck"), 
                       label = h4("Display 95% study level confidence intervals for observed data:"),
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
    h5("Download plot options:"),
    numericInput(inputId =  ns("plot_width"), label=h5("Plot width"), value = 10),
    numericInput(inputId =  ns("plot_height"), label=h5("Plot height"), value = 10),
    numericInput(inputId =  ns("plot_dpi"), label=h5("Plot DPI"), value = 600),
    downloadButton(outputId = ns("plot_download"), 
                   label = "Download Plot")
  )
}





LCM_HSROC_curve_type_renderUI  <- function(id) {  
  
  moduleServer(
    id,
    function(input, output, session) {
      
      
      output$HSROC_curve_type_index_ui <- renderUI({ 
        
        ns <- session$ns 
        
        if (1 %in% input$HSROCcheck) { 
          awesomeCheckbox(inputId = ns("HSROCcheck_unrestricted_HSROC_curve_index"), 
                          label = "extrapolate index test HSROC curve beyond estimated Se/Sp points?", 
                          FALSE)
        }
        else { }
        
      })
      
      output$HSROC_curve_type_ref_ui <- renderUI({ 
        
        ns <- session$ns 
        
        if (2 %in% input$HSROCcheck) { 
          awesomeCheckbox(inputId = ns("HSROCcheck_unrestricted_HSROC_curve_ref"), 
                          label = "extrapolate reference test(s) HSROC curve beyond estimated Se/Sp points?", 
                          FALSE)
        }
        else { }
        
     })
      
      outputOptions(output, 'HSROC_curve_type_index_ui', suspendWhenHidden=FALSE)
      outputOptions(output, 'HSROC_curve_type_ref_ui', suspendWhenHidden=FALSE)
      
    }
  )
}




## LCM - server-side function w/ renderUI's for the sROC plot settings menu (for dynamic UI's)   ------------------------------------  ---------------------------------
LCM_sroc_plot_settings_menu_server <- function(id, 
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







# LCM - server function for Covariate to display on plot (for info) --------------------------------------  ---------------------------------
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
          
          return(choicesCov)
        }
        else { }
      })
      
      
      output$covariate_display_ui <- renderUI({ 
        ns <- session$ns
        
        selectInput(inputId = ns("covcheck_display"), 
                    label= "Please select covariate to display on plot:", 
                    choices = covariate_display_obj_chicesCov(),
                    selected = 1)
      })
      
      outputOptions(output, 'covariate_display_ui', suspendWhenHidden=FALSE)
      
    }
  )
}


# UI for sROC plot (output the plot) -----------------------------------------    ----------------------------------------------------------------------
LCM_sroc_plot_UI <- function(id)   {
  ns <- NS(id)   # namespace function
  tagList(
  uiOutput(ns("LCM_sroc_plot_ui_notes")), 
  uiOutput(ns("LCM_sroc_plot_ui_plot"))
  )
}


LCM_sroc_plot_renderUI     <- function(id, 
                                       data, 
                                       draws,
                                       SA_indicator) {  
  moduleServer(
    id,
    function(input, output, session) {

      output$LCM_sroc_plot_ui_notes <- renderUI({

               ns <- session$ns
               SA_indicator <- SA_indicator$SA_indicator
               
               tagList(
                       p("NOTE: The dotted lines represent the 95% prediction regions from the latent class model; 
                          the greyed out areas represent the 95% credible regions from the latent class model"),
                     if (SA_indicator == TRUE) { 
                           tagList(
                           p("; the lighter points correspond to the summary estimates from the sensitivity analysis")
                           )
                     }
                     else { } 
                     
               )
               
          })
      
      
      
      output$LCM_sroc_plot_ui_plot <- renderUI({
              
              ns <- session$ns

            tagList(
              plotOutput(outputId =  ns("piechart"),
                         height = "300px",
                         width = "500px"),
              uiOutput(outputId =  ns("my_tooltip")),
              plotOutput(outputId =  ns("plot"),
                         click =  ns("plot_click"))
      )
      
      
      })
      

    
      outputOptions(output, "LCM_sroc_plot_ui_notes", suspendWhenHidden = FALSE)
      outputOptions(output, "LCM_sroc_plot_ui_plot", suspendWhenHidden = FALSE)
    }
  )
}









# LCM - sROC Plot  ------------------------------------------------------------  --------------------------------------------------------------------------

# Server function to generate and download sROC plot
LCM_sroc_plot_server <- function(id, 
                                 data,
                                 SA_data, 
                                 draws,
                                 SA_draws, 
                                 SA_indicator,
                                 LCM_options_indicators)
 {
  moduleServer(
    id,
    function(input, output, session) {
      
      plot_object <- reactive({
        
        req(data(),  draws(), cancelOutput = TRUE)
        
        mod <- draws()
        X <- data()
        
        C <- ncol(dplyr::select(X, -year.cts, -prevalence.cts))
        Names <- colnames(dplyr::select(X, -year.cts, -prevalence.cts))
        
        # run functions to get roc curve, credible and prediction regions and summary estimates from Stan model output file
        ss <- LCM_cred_pred_roc(X, mod, index_test = input$index_test_name)$ss
        X <- LCM_cred_pred_roc(X, mod, index_test = input$index_test_name)$X
        roc_points_refs_restricted <- LCM_cred_pred_roc(X, mod, index_test = input$index_test_name)$roc_points_refs_restricted
        roc_points_index_restricted <- LCM_cred_pred_roc(X, mod, index_test = input$index_test_name)$roc_points_index_restricted
        roc_points_refs_unrestricted <- LCM_cred_pred_roc(X, mod, index_test = input$index_test_name)$roc_points_refs_unrestricted
        roc_points_index_unrestricted <- LCM_cred_pred_roc(X, mod, index_test = input$index_test_name)$roc_points_index_unrestricted
        pred_region <- LCM_cred_pred_roc(X, mod, index_test = input$index_test_name)$pred_region
        credible_region <- LCM_cred_pred_roc(X, mod, index_test = input$index_test_name)$credible_region
        medians <- LCM_cred_pred_roc(X, mod, index_test = input$index_test_name)$medians
        num_refs <- LCM_cred_pred_roc(X, mod, index_test = input$index_test_name)$num_refs
        refs_names_short <- LCM_cred_pred_roc(X, mod, index_test = input$index_test_name)$refs_names_short
        refs_names <- LCM_cred_pred_roc(X, mod, index_test = input$index_test_name)$refs_names
        
        # For SA 
        SA_indicator <- SA_indicator$SA_indicator
        SA_display_indicator <- input$SA_display_indicator
        X_SA <- SA_data()
        SA_mod <- SA_draws()
        
         if ( SA_indicator == TRUE && is.null(SA_mod) == FALSE ) {
         #  if ( SA_indicator == TRUE) {
          # run function to get roc curve, credible and prediction regions and summary estimates from Stan model output file for SA
          SA_ss <- LCM_cred_pred_roc(X_SA, SA_mod, index_test = input$index_test_name)$ss
          SA_X <- LCM_cred_pred_roc(X_SA, SA_mod, index_test = input$index_test_name)$X
          SA_roc_points_refs_restricted <- LCM_cred_pred_roc(X_SA, SA_mod, index_test = input$index_test_name)$roc_points_refs_restricted
          SA_roc_points_index_restricted <- LCM_cred_pred_roc(X_SA, SA_mod, index_test = input$index_test_name)$roc_points_index_restricted
          SA_pred_region <- LCM_cred_pred_roc(X_SA, SA_mod, index_test = input$index_test_name)$pred_region
          SA_credible_region <- LCM_cred_pred_roc(X_SA, SA_mod, index_test = input$index_test_name)$credible_region
          SA_medians <- LCM_cred_pred_roc(X_SA, SA_mod, index_test = input$index_test_name)$medians
          SA_num_refs <- LCM_cred_pred_roc(X_SA, SA_mod, index_test = input$index_test_name)$num_refs
          SA_refs_names_short <- LCM_cred_pred_roc(X_SA, SA_mod, index_test = input$index_test_name)$refs_names_short
          SA_refs_names <- LCM_cred_pred_roc(X_SA, SA_mod, index_test = input$index_test_name)$refs_names
        }

        size_summary <- {input$size_summary}
        weight_scale <- {input$weight_scale}
        size_study_specific <- {input$size_study_specific}
        pie_scale <- {input$pie_scale}
        
        # Base plot (default)
        
        # if either Se or Sp for ref test. is fixed AND either Se or Sp for ref test is fixed, then there are no prediction regions 
        if (  ((LCM_options_indicators$LCM_SeR_fixed_indicator == 1) || (LCM_options_indicators$LCM_SpR_fixed_indicator == 1)) &&
              ((LCM_options_indicators$LCM_SeI_fixed_indicator == 1) || (LCM_options_indicators$LCM_SpI_fixed_indicator == 1))
              ) {
          
        g <- ggplot(data = ss, aes(y=se_ref, x = 1-sp_ref)) + 
          geom_point(data = medians,  
                     aes(y=median_sens, 
                         x = 1 - median_spec,  
                         colour = Test
                                          # shape = Test
          ), 
          size= size_summary ,
          alpha = 0.7)  +      # summary points
          geom_polygon(data = credible_region,
                       aes(x= 1 - x, y= y, colour = Test),
                       alpha=0.05,
                       size=0.4, 
                       linetype = 2,
                       inherit.aes = F) + # conf region
          theme_bw() + 
          theme(text = element_text(size=15), 
                legend.position = "left") + 
          xlab("1 - Specificity") + 
          ylab("Sensitivity") + 
          coord_fixed()
        
        g
        }
        else {
          # pred regions for reference only (SeR and SpR both random but either one or both of SeI or SpI are fixed)
          if (  ((LCM_options_indicators$LCM_SeR_fixed_indicator == 0) && (LCM_options_indicators$LCM_SpR_fixed_indicator == 0)) && 
                ((LCM_options_indicators$LCM_SeI_fixed_indicator == 1) || (LCM_options_indicators$LCM_SpI_fixed_indicator == 1)) 
                ) {  
        g <- ggplot(data = ss, aes(y=se_ref, x = 1-sp_ref)) + 
          geom_point(data = medians,  
                     aes(y=median_sens, 
                         x = 1 - median_spec,  
                         colour = Test
                         # shape = Test
                     ), 
                     size= size_summary ,
                     alpha = 0.7)  +      # summary points
          geom_path(data = dplyr::filter(pred_region, Test != input$index_test_name),
                    aes(x= 1 - x, y= y, colour = Test),
                    linetype = 2, 
                    size = 0.4,
                    inherit.aes = F) +                         # prediction region
          geom_polygon(data = credible_region,
                       aes(x= 1 - x, y= y, colour = Test),
                       alpha=0.05,
                       size=0.4, 
                       linetype = 2,
                       inherit.aes = F) + # conf region
          theme_bw() + 
          theme(text = element_text(size=15), 
                legend.position = "left") + 
          xlab("1 - Specificity") + 
          ylab("Sensitivity") + 
          coord_fixed()
        
        g
          }
          # pred regions for index only (SeI and SpI both random, but either one or both of SeR or SpR fixed)
          if (  ((LCM_options_indicators$LCM_SeR_fixed_indicator == 1) || (LCM_options_indicators$LCM_SpR_fixed_indicator == 1)) && 
                ((LCM_options_indicators$LCM_SeI_fixed_indicator == 0) && (LCM_options_indicators$LCM_SpI_fixed_indicator == 0)) 
          ) {  
            g <- ggplot(data = ss, aes(y=se_ref, x = 1-sp_ref)) + 
              geom_point(data = medians,  
                         aes(y=median_sens, 
                             x = 1 - median_spec,  
                             colour = Test
                             # shape = Test
                         ), 
                         size= size_summary ,
                         alpha = 0.7)  +      # summary points
              geom_path(data = dplyr::filter(pred_region, Test == input$index_test_name),
                        aes(x= 1 - x, y= y, colour = Test),
                        linetype = 2, 
                        size = 0.4,
                        inherit.aes = F) +                         # prediction region
              geom_polygon(data = credible_region,
                           aes(x= 1 - x, y= y, colour = Test),
                           alpha=0.05,
                           size=0.4, 
                           linetype = 2,
                           inherit.aes = F) + # conf region
              theme_bw() + 
              theme(text = element_text(size=15), 
                    legend.position = "left") + 
              xlab("1 - Specificity") + 
              ylab("Sensitivity") + 
              coord_fixed()
            
            g
          }
          # pred regions for index and ref tests (SpR, SeR, SpI, SeI all random)
          if (  ((LCM_options_indicators$LCM_SeR_fixed_indicator == 0) && (LCM_options_indicators$LCM_SpR_fixed_indicator == 0)) && 
                ((LCM_options_indicators$LCM_SeI_fixed_indicator == 0) && (LCM_options_indicators$LCM_SpI_fixed_indicator == 0)) 
          ) {  
            g <- ggplot(data = ss, aes(y=se_ref, x = 1-sp_ref)) + 
              geom_point(data = medians,  
                         aes(y=median_sens, 
                             x = 1 - median_spec,  
                             colour = Test
                             # shape = Test
                         ), 
                         size= size_summary ,
                         alpha = 0.7)  +      # summary points
              geom_path(data = pred_region, 
                        aes(x= 1 - x, y= y, colour = Test),
                        linetype = 2, 
                        size = 0.4,
                        inherit.aes = F) +                         # prediction region
              geom_polygon(data = credible_region,
                           aes(x= 1 - x, y= y, colour = Test),
                           alpha=0.05,
                           size=0.4, 
                           linetype = 2,
                           inherit.aes = F) + # conf region
              theme_bw() + 
              theme(text = element_text(size=15), 
                    legend.position = "left") + 
              xlab("1 - Specificity") + 
              ylab("Sensitivity") + 
              coord_fixed()
            
            g
          }
        }
        # SA
        # Add sensitivity analysis summary estimate to plot 
        if (SA_display_indicator == 1 && 
            SA_indicator == TRUE && 
            is.null(SA_mod) == FALSE &&
            is.null(SA_medians) == FALSE && 
            is.null(SA_pred_region) == FALSE &&
            is.null(SA_credible_region) == FALSE &&
            is.null(SA_roc_points_index_restricted) == FALSE &&
            is.null(SA_roc_points_refs_restricted)  == FALSE     ) { 
          
          # if either Se or Sp for ref test. is fixed AND either Se or Sp for ref test is fixed, then there are no prediction regions 
          if (  ((LCM_options_indicators$LCM_SeR_fixed_indicator == 1) || (LCM_options_indicators$LCM_SpR_fixed_indicator == 1)) &&
                ((LCM_options_indicators$LCM_SeI_fixed_indicator == 1) || (LCM_options_indicators$LCM_SpI_fixed_indicator == 1))
          ) {
            g = g +  geom_point(data = SA_medians,  
                                aes(y=median_sens, 
                                    x = 1 - median_spec,  
                                    colour = Test
                                    # shape = Test
                                ), 
                                size= size_summary ,
                                alpha = 0.4)  +    # summary points
              geom_polygon(data = SA_credible_region,
                           aes(x= 1 - x, y= y, colour = Test),
                           alpha=0.05,
                           size=0.4, 
                           linetype = 2,
                           alpha = 0.4,
                           inherit.aes = F)                 # conf region
            g
          }
          else {
            # pred regions for reference only (SeR and SpR both random but either one or both of SeI or SpI are fixed)
            if (  ((LCM_options_indicators$LCM_SeR_fixed_indicator == 0) && (LCM_options_indicators$LCM_SpR_fixed_indicator == 0)) && 
                  ((LCM_options_indicators$LCM_SeI_fixed_indicator == 1) || (LCM_options_indicators$LCM_SpI_fixed_indicator == 1)) 
            ) {  
              g = g +  geom_point(data = SA_medians,  
                                  aes(y=median_sens, 
                                      x = 1 - median_spec,  
                                      colour = Test
                                      # shape = Test
                                  ), 
                                  size= size_summary ,
                                  alpha = 0.4)  +    # summary points
                geom_path(data = dplyr::filter(SA_pred_region, Test != input$index_test_name),
                          aes(x= 1 - x, y= y, colour = Test),
                          linetype = 2, 
                          size = 0.4,
                          alpha = 0.4,
                          inherit.aes = F) +            # prediction region
                geom_polygon(data = SA_credible_region,
                             aes(x= 1 - x, y= y, colour = Test),
                             alpha=0.05,
                             size=0.4, 
                             linetype = 2,
                             alpha = 0.4,
                             inherit.aes = F)                 # conf region
              g
            }
            # pred regions for index only (SeI and SpI both random, but either one or both of SeR or SpR fixed)
            if (  ((LCM_options_indicators$LCM_SeR_fixed_indicator == 1) || (LCM_options_indicators$LCM_SpR_fixed_indicator == 1)) && 
                  ((LCM_options_indicators$LCM_SeI_fixed_indicator == 0) && (LCM_options_indicators$LCM_SpI_fixed_indicator == 0)) 
            ) {
              
              g = g +  geom_point(data = SA_medians,  
                                  aes(y=median_sens, 
                                      x = 1 - median_spec,  
                                      colour = Test
                                      # shape = Test
                                  ), 
                                  size= size_summary ,
                                  alpha = 0.4)  +    # summary points
                geom_path(data = dplyr::filter(SA_pred_region, Test == input$index_test_name),
                          aes(x= 1 - x, y= y, colour = Test),
                          linetype = 2, 
                          size = 0.4,
                          alpha = 0.4,
                          inherit.aes = F) +            # prediction region
                geom_polygon(data = SA_credible_region,
                             aes(x= 1 - x, y= y, colour = Test),
                             alpha=0.05,
                             size=0.4, 
                             linetype = 2,
                             alpha = 0.4,
                             inherit.aes = F)                 # conf region
              g
              
            }
            # pred regions for index and ref tests (SpR, SeR, SpI, SeI all random)
            if (  ((LCM_options_indicators$LCM_SeR_fixed_indicator == 0) && (LCM_options_indicators$LCM_SpR_fixed_indicator == 0)) && 
                  ((LCM_options_indicators$LCM_SeI_fixed_indicator == 0) && (LCM_options_indicators$LCM_SpI_fixed_indicator == 0)) 
            ) {  
              g = g +  geom_point(data = SA_medians,  
                                  aes(y=median_sens, 
                                      x = 1 - median_spec,  
                                      colour = Test
                                      # shape = Test
                                  ), 
                                  size= size_summary ,
                                  alpha = 0.4)  +    # summary points
                geom_path(data = SA_pred_region, 
                          aes(x= 1 - x, y= y, colour = Test),
                          linetype = 2, 
                          size = 0.4,
                          alpha = 0.4,
                          inherit.aes = F) +            # prediction region
                geom_polygon(data = SA_credible_region,
                             aes(x= 1 - x, y= y, colour = Test),
                             alpha=0.05,
                             size=0.4, 
                             linetype = 2,
                             alpha = 0.4,
                             inherit.aes = F)                 # conf region
              g
            }
          }
            

          
          # HSROC curve for index
          if ('3' %in% input$HSROCcheck) {
            g = g + geom_path(data = SA_roc_points_index_restricted,
                              colour = colour_index,
                              aes(x = FPR_index,
                                  y = TPR_index)) 
            g
            
            if (input$HSROCcheck_unrestricted_HSROC_curve_index == TRUE) { 
              g = g + geom_path(data = SA_roc_points_index_unrestricted,
                                colour = colour_index,
                                aes(x = FPR_index,
                                    y = TPR_index)) 
              g
            }
          }
          
          
          # HSROC curve for refs
          if ('4' %in% input$HSROCcheck) {
            g = g + geom_path(data = SA_roc_points_refs_restricted,
                              aes(x = FPR,
                                  y = TPR,
                                  colour= Test)) 
            g
            if (input$HSROCcheck_unrestricted_HSROC_curve_ref == TRUE) { 
              g = g + geom_path(data = SA_roc_points_refs_unrestricted,
                                aes(x = FPR,
                                    y = TPR,
                                    colour= Test)) 
              g
            }
          }
          
          # option to force x- and y-axis limits to (0,1) 
          if ('5' %in% input$HSROCcheck) { 
            g = g +  scale_x_continuous(breaks = seq(0,1,0.2), limits = c(0,1.05))  + 
              scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1.05))  
            g
          }
          
          g
          
        }
        
        
        # get the colours used for the ref and index tests
        g_build <- ggplot_build(g)
        
        colours <- g_build$data[[1]]$colour
        colour_refs  <- colours[1:num_refs]
        colour_index <- colours[num_refs+1]
        
        
        
        # Plot study-level estimates for index
        if ('1' %in% input$HSROCcheck) {
          g = g + geom_point(data = ss, 
                             aes(y=se_index, x = 1-sp_index),
                             colour = colour_index,
                             size = size_study_specific, 
                             shape = 16,
                             inherit.aes = F) 
          g
        }
        
        # Plot study-level estimates for ref
        if ('2' %in% input$HSROCcheck) {
          g = g + geom_point(data = ss, aes(y=se_ref, 
                                            x = 1-sp_ref,
                                            colour = Test
          ),
          size = size_study_specific, 
          shape = 1,
          #   inherit.aes = F
          )  
          g
        }
        
        # HSROC curve for index
        if ('3' %in% input$HSROCcheck) {
          g = g + geom_path(data = roc_points_index_restricted,
                            colour = colour_index,
                            aes(x = FPR_index,
                                y = TPR_index)) 
          g
          if (input$HSROCcheck_unrestricted_HSROC_curve_index == TRUE) { 
            g = g + geom_path(data = roc_points_index_unrestricted,
                              colour = colour_index,
                              aes(x = FPR_index,
                                  y = TPR_index)) 
            g
            }
        }
        
        # HSROC curve for refs
        if ('4' %in% input$HSROCcheck) {
          g = g + geom_path(data = roc_points_refs_restricted,
                            aes(x = FPR,
                                y = TPR,
                                colour= Test)) 
          g
          
          if (input$HSROCcheck_unrestricted_HSROC_curve_ref == TRUE) { 
            g = g + geom_path(data = roc_points_refs_unrestricted,
                              aes(x = FPR,
                                  y = TPR,
                                  colour= Test)) 
            g
          }
        }
        
        # option to force x- and y-axis limits to (0,1) 
        if ('5' %in% input$HSROCcheck) { 
          g = g +  scale_x_continuous(breaks = seq(0,1,0.2), limits = c(0,1.05))  + 
                   scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1.05))  
          g
        }
        

                
         else { 
          g 
        }
        
        
        
        
   # dataset 3 - "cov.csv" ---------------------------------------------------------------------------------------------
   if  (C > 6 & Names[7] != "rob_PS") {  # LCM sROC plot (ctd.) (dataset 3) Plots with covariate [or just ref test info] data, but no QA data ("Cov.csv") --------------------------------
          
          cov_names <-  num_covariates(X)$combined[-1] # extract names of covariates
          cols_covariates <- dplyr::select(X, cov_names, year.cts, prevalence.cts) # columns containing only covariate data
          no_cov <- num_covariates(X)$no_covariates  # number of covariates
          ss <- cbind(ss, cols_covariates)
          
          # make covariates factors for display
          for (i in 1:no_cov) { 
            ss <- dplyr::mutate(ss, !!as.name(colnames(ss)[21+i]) := factor( !!as.name(colnames(ss)[21+i])))
          }
          
          
          for (i in 1:no_cov) {
            if (input$covcheck_display == i+1) { # the covariate selected by the user
              if (input$cov_toggle == 3) { # plot covariates as text labels AND coloured points (over the index test estimates)
                g = g + geom_label_repel(data = ss,
                                         size = 5,
                                         inherit.aes = FALSE,
                                         aes(x = 1 - sp_index, 
                                             y = se_index, 
                                             label = !!as.name(colnames(ss)[21+i])) ) +
                  new_scale_color() + 
                  geom_point(data = ss,
                             inherit.aes = FALSE,
                             size = size_study_specific,
                             aes(x = 1 - sp_index, 
                                 y = se_index,
                                 colour = !!as.name(colnames(ss)[21+i]))) + 
                  theme(legend.position = "left") 
                g
              } else {
                g 
              }
              if (input$cov_toggle == 2) {   # plot covariates as coloured points only
                g = g + new_scale_color() +
                  geom_point(data = ss,
                             inherit.aes = FALSE,
                             size =  size_study_specific,
                             aes(x = 1 - sp_index, 
                                 y = se_index,
                                 colour = !!as.name(colnames(ss)[21+i]))) + 
                  theme(legend.position = "left") 
                g
              }   else { 
                g 
              }
              if (input$cov_toggle == 1) {   # plot covariates as text labels only
                g = g + geom_label_repel(data = ss,
                                         size = 5,
                                         inherit.aes = FALSE,
                                         aes(x = 1 - sp_index, 
                                             y = se_index,
                                             label = !!as.name(colnames(ss)[21+i])) )
                g
              }  else {
                g
              }
            }
          }
          
          g
          
          
          # LCM sRoc plot (ctd) - "Cov.csv" - CrI's for index test  ---------------------------------------------------------------------
          
          ### plot Se and Sp CrI's
          ## for index test
          #  Se CrI's
          if ('3' %in% input$cicheck) {  # i.e. user selected to display 95% CrI's for index Se's 
            if ('1' %in% input$covcheck_display ) { # when no covariates are selected for display on plot
              g = g + geom_errorbar(data = ss, 
                                    inherit.aes = F,
                                    aes(y = se_index, 
                                        x = 1-sp_index,
                                        ymin = se_index_LCI,
                                        ymax = se_index_UCI),
                                    width = 0.02, 
                                    alpha = 1,
                                    colour = colour_index)
              g
            }
            else { g }
            if (input$covcheck_display != 1) {     # when covariate is selected to be displayed on plot
              for (i in 1:no_cov) {
                if (input$covcheck_display == i+1) { # the covariate selected by the user
                  if (input$cov_toggle == 1) {  # when covariate is selected as text only
                    g = g + geom_errorbar(data = ss, 
                                          inherit.aes = F,
                                          aes(y = se_index, 
                                              x = 1-sp_index,
                                              ymin = se_index_LCI,
                                              ymax = se_index_UCI),
                                          width = 0.02,
                                          alpha = 1,
                                          colour = colour_index)
                    g
                  }
                  else { # when covariate is selected as coloured points or coloured points + text labels
                    g = g + geom_errorbar(data = ss, 
                                          inherit.aes = F,
                                          aes(y = se_index, 
                                              x = 1-sp_index,
                                              ymin = se_index_LCI,
                                              ymax = se_index_UCI,
                                              colour = !!as.name(colnames(ss)[21+i])),
                                          width = 0.02, 
                                          alpha = 1)
                    g
                  }
                }
              }
            }
          }
          
          g
          
          # Sp CrI's
          if ('4' %in% input$cicheck) {  # i.e. user selected to display 95% CrI's for index Sp's
            if ('1' %in% input$covcheck_display ) { # when no covariates are selected for display on plot
              g = g + geom_errorbar(data = ss, 
                                    inherit.aes = F,
                                    aes(y = se_index, 
                                        x = 1-sp_index,
                                        xmin = 1 - sp_index_UCI,
                                        xmax = 1 - sp_index_LCI),
                                    width = 0.02,
                                    alpha = 1,
                                    colour = colour_index)
              g
            }
            else { g }
            if (input$covcheck_display != 1) {     # when covariate is selected to be displayed on plot
              for (i in 1:no_cov) {
                if (input$covcheck_display == i+1) { # the covariate selected by the user
                  if (input$cov_toggle == 1) {  # when covariate is selected as text
                    g = g +    new_scale_colour() + 
                      geom_errorbar(data = ss, 
                                    inherit.aes = F,
                                    aes(y = se_index, 
                                        x = 1-sp_index,
                                        xmin = 1 - sp_index_UCI,
                                        xmax = 1 - sp_index_LCI),
                                    width = 0.02, 
                                    alpha = 1,
                                    colour = colour_index)
                    g
                  }
                  else { # when covariate is selected as coloured points or coloured points + text labels
                    g = g +  new_scale_colour() + 
                      geom_errorbar(data = ss, 
                                    inherit.aes = F,
                                    aes(y = se_index, 
                                        x = 1-sp_index,
                                        xmin = 1 - sp_index_UCI,
                                        xmax = 1 - sp_index_LCI,
                                        colour = !!as.name(colnames(ss)[21+i])),
                                    width = 0.02, 
                                    alpha = 1)
                    g
                  }
                }
              }
            }
          }
          g
          
          # LCM sRoc plot (ctd) - "Cov.csv" - CrI's for REF tests (no covariates as these will only be displayed over index test estimates)  ---------------------------------------------------------------------
          # Se CrI's
          if ('1' %in% input$cicheck) {  # i.e. user selected to display 95% CrI's for ref Se's 
            g = g + 
              new_scale_colour() + 
              geom_errorbar(data = ss, 
                            #  inherit.aes = F,
                            aes(y = se_ref, 
                                x = 1-sp_ref,
                                ymin = se_ref_LCI,
                                ymax = se_ref_UCI, 
                                colour = Test
                            ),
                            width = 0.02,
                            alpha = 1,
                            linetype = 2) + 
              scale_colour_manual(values = colour_refs) + 
              guides(colour = FALSE)
            g
          }
          else { g }
          g
          # Sp CrI's
          if ('2' %in% input$cicheck) {  # i.e. user selected to display 95% CrI's for ref Sp's
            g = g + 
              new_scale_colour() + 
              geom_errorbar(data = ss, 
                            #  inherit.aes = F,
                            aes(y = se_ref, 
                                x = 1-sp_ref,
                                xmin = 1 - sp_ref_UCI,
                                xmax = 1 - sp_ref_LCI,
                                colour = Test),
                            width = 0.02,
                            alpha = 1,
                            linetype = 2) + 
              scale_colour_manual(values = colour_refs) + 
              guides(colour = FALSE)
            g
          }
          else { g }
          g
          
        
          
          

    # Dataset 4 - "QA_Cov.csv" ------------------------------------------------

        
        }  else  {   # LCM sROC plot (ctd.) - (dataset 4) When QA AND covariate [or just ref test info] data is available ("QA_Cov.csv") ----------------
          
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
          # Coloured study-level estimates based on 1 of the 7 outcomes of QA (over index test estimates)
          
          for (i in 1:7) { # i.e. if the QA item is rob_PS, rob_IT, rob_RS, rob_FT, ac_PS, ac_IT, ac_RS
            if (input$QAcheck == i+1) { # the selected QA  item (QA is on since in interval (2,8))
              g =  g + 
                new_scale("shape") +
                geom_point(data = data_QA_Cov2,
                           size =  size_study_specific,
                           colour = colour_index,
                           aes(x = 1 - sp_index, 
                               y = se_index,
                               shape = !!as.name(colnames(data_QA_Cov2)[6+i]))) + 
                theme(legend.position = "left") 
              g
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
          
          # make covariates factors for display
          for (i in 1:no_cov) { 
            data_QA_Cov2 <- dplyr::mutate(data_QA_Cov2,
                                          !!as.name(colnames(data_QA_Cov2)[13+i]) := factor( !!as.name(colnames(data_QA_Cov2)[13+i])))
          }
          
          for (i in 1:7) {
            if (input$QAcheck == i+1) {
              # Plot with QA items
              g =  g + 
                new_scale("shape") + 
                geom_point(data = data_QA_Cov2,
                           size = size_study_specific,
                           colour = colour_index,
                           aes(x = 1 - sp_index, 
                               y = se_index, 
                               shape = !!as.name(colnames(data_QA_Cov2)[6+i])))  + 
                theme(legend.position = "left") 
              g
            }
          }
          
          for (i in 1:no_cov) {
            if (input$covcheck_display == i+1) { # i.e. the user has a covariate selected 
              if (input$cov_toggle == 3) {   # plot covariates as text and coloured points
                # add the text labels for the covariates first
                g = g + geom_label_repel(data = data_QA_Cov2,
                                         size = size_study_specific,
                                         aes(x = 1 - sp_index, 
                                             y = se_index, 
                                             label = !!as.name(colnames(data_QA_Cov2)[13+i])) )
                g
                if (input$QAcheck == 1) { # add the coloured points for covariates, when no QA is selected
                  g = g +
                    new_scale_color() + 
                    geom_point(data = data_QA_Cov2,
                               size = size_study_specific,
                               aes(x = 1 - sp_index, 
                                   y = se_index, 
                                   colour = !!as.name(colnames(data_QA_Cov2)[13+i]))) + # colour for covariates 
                    theme(legend.position = "left") 
                  g
                }
                else { #  i.e. QA is selected - add the coloured points for covariate level and shapes for QA items (need interaction)
                  for (j in 1:7) {
                    if (input$QAcheck == j+1) {
                      g =   g + 
                        new_scale_color() + 
                        new_scale("shape") + 
                        geom_point(data = data_QA_Cov2,
                                   size = size_study_specific,
                                   aes(x = 1 - sp_index, 
                                       y = se_index, 
                                       group = interaction(!!as.name(colnames(data_QA_Cov2)[6+j]), !!as.name(colnames(data_QA_Cov2)[13+i])), 
                                       shape = !!as.name(colnames(data_QA_Cov2)[6+j]),  # shape by the Risk level in given QA category
                                       colour =  !!as.name(colnames(data_QA_Cov2)[13+i])))  + 
                        theme(legend.position = "left") 
                    }
                  }
                  g
                }
              }
              if (input$cov_toggle == 2) {   # plot covariates as coloured points only
                if (input$QAcheck == 1) { #  when no QA is selected
                  g = g + 
                    new_scale_color() + 
                    geom_point(data = data_QA_Cov2,
                               size = size_study_specific,
                               aes(x = 1 - sp_index, 
                                   y = se_index, 
                                   colour = !!as.name(colnames(data_QA_Cov2)[13+i])))  + # colour for covariates 
                    theme(legend.position = "left") 
                  g
                }
                else { #  when QA is selected (using interaction for QA and covariates)
                  for (j in 1:7) {
                    if (input$QAcheck == j+1) {
                      g =   g +   
                        new_scale_color() + 
                        new_scale("shape") + 
                        geom_point(data = data_QA_Cov2,
                                   size = size_study_specific,
                                   aes(x = 1 - sp_index, 
                                       y = se_index, 
                                       group = interaction(!!as.name(colnames(data_QA_Cov2)[6+j]), !!as.name(colnames(data_QA_Cov2)[13+i])), 
                                       shape = !!as.name(colnames(data_QA_Cov2)[6+j]),  # shape by the Risk level in given QA category
                                       colour =  !!as.name(colnames(data_QA_Cov2)[13+i]))) + 
                        theme(legend.position = "left") 
                      g
                    }
                  }
                }
              }
              if (input$cov_toggle == 1) {   # plot covariates as text only
                g =  g + geom_label_repel(data = data_QA_Cov2,
                                          aes(x = 1 - sp_index, 
                                              y = se_index,
                                              label = !!as.name(colnames(data_QA_Cov2)[13+i])) ) + 
                  theme(legend.position = "left") 
                g
              }
              else { g }
            }
          }
          
          
          g
          
          ####
          # Add 95% CrI's for sens and spec at study level (for INDEX test - i.e. overlay covariate and QA info here)
          ####
          
          # set the linetypes
          linetypes <- c(1,5,7)
          scales_for_linetypes <- scale_linetype_manual(values = linetypes)
          
          ###  CrI's for reference test (no covariates as these will only be displayed over index test estimates)
          # Se CrI's
          if ('1' %in% input$cicheck) {  # i.e. user selected to display 95% CrI's for ref Se's 
            g =      g + 
                        new_scale_colour() + 
                        geom_errorbar(data = ss, 
                                      #   inherit.aes = F,
                                      aes(y = se_ref, 
                                          x = 1-sp_ref,
                                          ymin = se_ref_LCI,
                                          ymax = se_ref_UCI, 
                                          colour = Test),
                                      width = 0.02,
                                      alpha = 1,
                                      linetype = 2) + 
                        scale_colour_manual(values = colour_refs) + 
                        guides(colour = FALSE) + 
              theme(legend.position = "left") 
            
            g
          }
          else { g }
          g
          
          # Sp CrI's
          if ('2' %in% input$cicheck) {  # i.e. user selected to display 95% CrI's for ref Sp's
            g = g +
              new_scale_colour() + 
              geom_errorbar(data = ss, 
                            inherit.aes = F,
                            aes(y = se_ref, 
                                x = 1-sp_ref,
                                xmin = 1 - sp_ref_UCI,
                                xmax = 1 - sp_ref_LCI,
                                colour = Test),
                            width = 0.02, 
                            alpha = 1,
                            linetype = 2) + 
              scale_colour_manual(values = colour_refs) + 
              guides(colour = FALSE) + 
              theme(legend.position = "left") 
            
            g
          }
          else { g }
          g
          
          
          ### 95% CrI's for index Se
          if ('3' %in% input$cicheck) { # index test Se CrI's
            # plot sensitivity CI's when NO COVARIATES selected and  no QA (except piercharts)
            if (input$covcheck_display == 1 & (input$QAcheck == 1 | input$QAcheck == 9 |input$QAcheck == 10 |input$QAcheck == 11)) {
              g = g + geom_errorbar(data = data_QA_Cov2,
                                    aes(x = 1 - sp_index, 
                                        y = se_index,
                                        ymin= se_index_LCI, 
                                        ymax= se_index_UCI),
                                    width = 0.02, 
                                    alpha = 1,
                                    colour = colour_index) + 
                theme(legend.position = "left") 
              g
            }
            else { g }
            # plot sensitivity CI's when covariates only dispalyed as text, no QA (except piercharts)
            if (input$covcheck_display != 1 & (input$QAcheck == 1 | input$QAcheck == 9 |input$QAcheck == 10 |input$QAcheck == 11)) {
              if (input$cov_toggle == 1) { # i.e. covariates displayed as text only
                g = g + geom_errorbar(data = data_QA_Cov2,
                                      aes(x = 1 - sp_index, 
                                          y = se_index,
                                          ymin= se_index_LCI, 
                                          ymax= se_index_UCI),
                                      width = 0.02,
                                      alpha = 1, 
                                      colour = colour_index) + 
                  theme(legend.position = "left") 
                g
              }
              else { g }
            }
            else { g }
            # Plot sensitivity CI's with QA only, NO COVARIATES
            for (i in 1:7) {
              if (input$covcheck_display == 1 & input$QAcheck == (i+1) ) {
                g = g + geom_errorbar(data = data_QA_Cov2,
                                      aes(x = 1 - sp_index, 
                                          y = se_index,
                                          ymin= se_index_LCI, 
                                          ymax= se_index_UCI,
                                          linetype = !!as.name(colnames(data_QA_Cov2)[6+i])),
                                      width = 0.02, 
                                      alpha = 1,
                                      colour = colour_index) +  
                # scales_for_linetypes
                  theme(legend.position = "left") 
                g
              }
              else { g }
            }
            # Plot sensitivity CI's when QA is selected, and covariates displayed as TEXT ONLY
            for (i in 1:7) {
              if (input$covcheck_display != 1 & input$QAcheck == (i+1) ) {
                if (input$cov_toggle == 1) {
                  g = g + geom_errorbar(data = data_QA_Cov2,
                                        aes(x = 1 - sp_index, 
                                            y = se_index,
                                            ymin= se_index_LCI, 
                                            ymax= se_index_UCI,
                                            linetype = !!as.name(colnames(data_QA_Cov2)[6+i])),
                                        width = 0.02, 
                                        alpha = 1,
                                        colour = colour_index) + 
                    theme(legend.position = "left") 
                  #   scales_for_linetypes
                  g
                }
                else { g }
              }
            }
            # Plot sensitivity CI's when covariate is selected, no QA (except pie charts)
            for (i in 1:no_cov) {
              if ((input$QAcheck == 1 |input$QAcheck == 9 |input$QAcheck == 10 |input$QAcheck == 11) & input$covcheck_display == i+1) {
                if (input$cov_toggle == 2 | input$cov_toggle == 3) { # i.e. covariate selected as coloured only or coloured+text
                  g = g + 
                    new_scale_color() + 
                    geom_errorbar(data = data_QA_Cov2,
                                  aes(x = 1 - sp_index, 
                                      y = se_index,
                                      ymin= se_index_LCI, 
                                      ymax= se_index_UCI,
                                      colour = !!as.name(colnames(data_QA_Cov2)[13+i])),
                                  width = 0.02, alpha = 1) + 
                    theme(legend.position = "left") 
                  g
                }
                else { g }
              }
            }
            g
            # Plot sensitivity CI's when BOTH Cov and QA both selected 
            if (input$covcheck_display != 1 &  # i.e. covariate is selected 
                (input$QAcheck == 2 |input$QAcheck == 3 | input$QAcheck == 4 |input$QAcheck == 5 | input$QAcheck == 6 |input$QAcheck == 7 | input$QAcheck == 8)) {
              if ( input$cov_toggle == 2 | input$cov_toggle == 3) {
                for (i in 1:no_cov) {
                  if (input$covcheck_display == i+1) { # the covariate selected
                    for (j in 1:7) { 
                      if (input$QAcheck == (j+1) ) { # the QA item selected
                        g = g + 
                          geom_errorbar(data = data_QA_Cov2,
                                        aes(x = 1 - sp_index, 
                                            y = se_index,
                                            ymin= se_index_LCI, 
                                            ymax= se_index_UCI,
                                            colour = !!as.name(colnames(data_QA_Cov2)[13+i]), # colour for covariate
                                            linetype = !!as.name(colnames(data_QA_Cov2)[6+j])), # linetype for QA
                                        width = 0.02, 
                                        alpha = 1) + 
                          theme(legend.position = "left") 
                        # scales_for_linetypes
                        g
                      }
                    }
                  }
                  else { g }
                }
              }
            }
          } # end of Sens CrI 's
          
          g
          
          ### 95% CrI's for Spec
          if ('4' %in% input$cicheck) { # index test Se CrI's
            # plot sensitivity CI's when NO COVARIATES selected and  no QA (except piercharts)
            if (input$covcheck_display == 1 & (input$QAcheck == 1 | input$QAcheck == 9 |input$QAcheck == 10 |input$QAcheck == 11)) {
              g = g + geom_errorbar(data = data_QA_Cov2,
                                    aes(x = 1 - sp_index, 
                                        y = se_index,
                                        xmin = 1 - sp_index_UCI,
                                        xmax = 1 - sp_index_LCI),
                                    width = 0.02, alpha = 1,
                                    colour = colour_index) + 
                theme(legend.position = "left") 
              g
            }
            else { g }
            # plot sensitivity CI's when covariates only dispalyed as text, no QA (except piercharts)
            if (input$covcheck_display != 1 & (input$QAcheck == 1 | input$QAcheck == 9 |input$QAcheck == 10 |input$QAcheck == 11)) {
              if (input$cov_toggle == 1) { # i.e. covariates displayed as text only
                g = g + geom_errorbar(data = data_QA_Cov2,
                                      aes(x = 1 - sp_index, 
                                          y = se_index,
                                          xmin = 1 - sp_index_UCI,
                                          xmax = 1 - sp_index_LCI),
                                      width = 0.02, alpha = 1, 
                                      colour = colour_index) + 
                  theme(legend.position = "left") 
                g
              }
              else { g }
            }
            else { g }
            # Plot sensitivity CI's with QA only, NO COVARIATES
            for (i in 1:7) {
              if (input$covcheck_display == 1 & input$QAcheck == (i+1) ) {
                g = g + geom_errorbar(data = data_QA_Cov2,
                                      aes(x = 1 - sp_index, 
                                          y = se_index,
                                          xmin = 1 - sp_index_UCI,
                                          xmax = 1 - sp_index_LCI,
                                          linetype = !!as.name(colnames(data_QA_Cov2)[6+i])),
                                      width = 0.02, alpha = 1) +  
                  theme(legend.position = "left") 
                # scales_for_linetypes
                g
              }
              else { g }
            }
            # Plot sensitivity CI's when QA is selected, and covariates displayed as TEXT ONLY
            for (i in 1:7) {
              if (input$covcheck_display != 1 & input$QAcheck == (i+1) ) {
                if (input$cov_toggle == 1) {
                  g = g + geom_errorbar(data = data_QA_Cov2,
                                        aes(x = 1 - sp_index, 
                                            y = se_index,
                                            xmin = 1 - sp_index_UCI,
                                            xmax = 1 - sp_index_LCI,
                                            linetype = !!as.name(colnames(data_QA_Cov2)[6+i])),
                                        width = 0.02, alpha = 1) + 
                    theme(legend.position = "left") 
                  #   scales_for_linetypes
                  g
                }
                else { g }
              }
            }
            # Plot sensitivity CI's when covariate is selected, no QA (except pie charts)
            for (i in 1:no_cov) {
              if ((input$QAcheck == 1 |input$QAcheck == 9 |input$QAcheck == 10 |input$QAcheck == 11) & input$covcheck_display == i+1) {
                if (input$cov_toggle == 2 | input$cov_toggle == 3) { # i.e. covariate selected as coloured only or coloured+text
                  g = g + 
                    new_scale_color() + 
                    geom_errorbar(data = data_QA_Cov2,
                                  aes(x = 1 - sp_index, 
                                      y = se_index,
                                      xmin = 1 - sp_index_UCI,
                                      xmax = 1 - sp_index_LCI,
                                      colour = !!as.name(colnames(data_QA_Cov2)[13+i])),
                                  width = 0.02, alpha = 1) + 
                    theme(legend.position = "left") 
                  g
                }
                else { g }
              }
            }
            g
            # Plot Sp CI's when BOTH Cov and QA both selected 
            if (input$covcheck_display != 1 &  # i.e. covariate is selected 
                (input$QAcheck == 2 |input$QAcheck == 3 | input$QAcheck == 4 |input$QAcheck == 5 | 
                 input$QAcheck == 6 |input$QAcheck == 7 | input$QAcheck == 8)) {
              if ( input$cov_toggle == 2 | input$cov_toggle == 3) {
                for (i in 1:no_cov) {
                  if (input$covcheck_display == i+1) { # the covariate selected
                    for (j in 1:7) { 
                      if (input$QAcheck == (j+1) ) { # the QA item selected
                        g = g + 
                          geom_errorbar(data = data_QA_Cov2,
                                        aes(x = 1 - sp_index, 
                                            y = se_index,
                                            xmin = 1 - sp_index_UCI,
                                            xmax = 1 - sp_index_LCI,
                                            colour = !!as.name(colnames(data_QA_Cov2)[13+i]), # colour for covariate
                                            linetype = !!as.name(colnames(data_QA_Cov2)[6+j])), # linetype for QA
                                        width = 0.02, 
                                        alpha = 1) + 
                          theme(legend.position = "left") 
                        # scales_for_linetypes
                        g
                      }
                    }
                  }
                  else { g }
                }
              }
            }
          }   # end of Spec CrI's
          
          g
          
          pie_scale <- input$pie_scale
          

       # for QA_Cov.csv - plot percharts  ----------------------------------------
          
          # Pie charts for RoB
          if ('9' %in% input$QAcheck) { # RoB
            P_rob_wide <- P_rob %>% spread(rob, score)  %>% dplyr::mutate(Study = seq(from = 1, to = nrow(X)))
            P_rob_wide2 <- left_join(P_rob_wide, ss, by = "Study") 
            P_rob_wide3 <- dplyr::mutate(P_rob_wide2, 
                                         rob_FT_score = as.numeric(factor(rob_FT, levels = c(1,2,3))), 
                                         rob_IT_score = as.numeric(factor(rob_IT, levels = c(1,2,3))), 
                                         rob_PS_score = as.numeric(factor(rob_PS, levels = c(1,2,3))), 
                                         rob_RS_score = as.numeric(factor(rob_RS, levels = c(1,2,3))))
            P_rob_wide4 <-  dplyr::mutate(P_rob_wide3, 
                                          fpr_index = 1 - sp_index, 
                                          fpr_ref = 1 - sp_ref,
                                          Study = seq(from = 1, to = nrow(X)), 
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
              dplyr::select(Study, se_index, sp_index, se_ref, sp_ref, fpr_index, fpr_ref, 
                            rob_PS, rob_IT, rob_RS, rob_FT, 
                            rob_PS_score, rob_IT_score, rob_RS_score, rob_FT_score)
            P_rob_wide5 <- tibble(P_rob_wide4)
            
            colours_group = c("green", "red", "black")
            colours_category = c("white", "white", "white", "white")
            
            # add little pie charts to plot
            g = g + new_scale_colour() + 
              geom_scatterpie(data = P_rob_wide5, aes(y=se_index, x = fpr_index,
                                                      group = Study,
                                                      colour = cut(x = c(P_rob_wide5$rob_FT_score, P_rob_wide5$rob_IT_score, 
                                                                         P_rob_wide5$rob_PS_score, P_rob_wide5$rob_RS_score), 
                                                                   breaks = c(0,1,2,3), 
                                                                   labels = c("Low", "High", "Unclear"), 
                                                                   ordered_result = TRUE)),  
                              col = c("rob_FT", "rob_IT", "rob_PS", "rob_RS"), 
                              # size = 1, 
                              pie_scale = pie_scale,
                              alpha = 0.5) +
              coord_equal() + 
              theme_bw() + 
              theme(legend.position = "left", 
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
          
          
          # Pie charts for  AC
          if ('10' %in% input$QAcheck) { 
            P_ac_wide <- P_ac %>% spread(rob, score)  %>% dplyr::mutate(Study = seq(from = 1, to = nrow(X)))
            P_ac_wide2 <- left_join(P_ac_wide, ss, by = "Study") 
            P_ac_wide3 <- dplyr::mutate(P_ac_wide2, 
                                        ac_IT_score = as.numeric(factor(ac_IT, levels = c(1,2,3))), 
                                        ac_PS_score = as.numeric(factor(ac_PS, levels = c(1,2,3))), 
                                        ac_RS_score = as.numeric(factor(ac_RS, levels = c(1,2,3))))
            P_ac_wide4 <-  dplyr::mutate(P_ac_wide3, 
                                         fpr_index = 1 - sp_index, 
                                         fpr_ref = 1 - sp_ref,
                                         Study = seq(from = 1, to = nrow(X)), 
                                         ac_IT = rep(1, length = nrow(X)),
                                         ac_PS = rep(1, length = nrow(X)),
                                         ac_RS = rep(1, length = nrow(X)), 
                                         ac_IT_colour = ifelse(ac_IT == 1, "green",
                                                               ifelse(rob_IT == 2, "red", "grey")),
                                         ac_PS_colour = ifelse(ac_PS == 1, "green",
                                                               ifelse(rob_PS == 2, "red", "grey")),
                                         ac_RS_colour = ifelse(ac_RS == 1, "green",
                                                               ifelse(rob_RS == 2, "red", "grey")))  %>%
              dplyr::select(Study, se_index, sp_index, se_ref, sp_ref, fpr_index, fpr_ref,  
                            ac_PS, ac_IT, ac_RS,
                            ac_PS_score, ac_IT_score, ac_RS_score)
            P_ac_wide5 <- tibble(P_ac_wide4)
            
            colours_group = c("green", "red", "black")
            colours_category = c("white", "white", "white")
            
            # add little pie charts to plot
            g = g +
              new_scale_colour() + 
              geom_scatterpie(data = P_ac_wide5, aes(y=se_index, x = fpr_index,
                                                     group = Study,
                                                     colour = cut(x = c(P_ac_wide5$ac_IT_score, 
                                                                        P_ac_wide5$ac_PS_score, P_ac_wide5$ac_RS_score), 
                                                                  breaks = c(0,1,2,3), 
                                                                  labels = c("Low", "High", "Unclear"), 
                                                                  ordered_result = TRUE)),  
                              col = c("ac_IT", "ac_PS", "ac_RS"), 
                              # size = 1, 
                              pie_scale = pie_scale, 
                              alpha = 0.5) +
              coord_equal() + 
              theme_bw() + 
              theme(legend.position = "left", 
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
          
          # Pie charts for both RoB and AC
          if ('11' %in% input$QAcheck) {
            P_rob_wide <- P_rob %>% spread(rob, score)  %>% mutate(Study = seq(from = 1, to = nrow(X)))
            P_rob_wide2 <- left_join(P_rob_wide, ss, by = "Study") 
            P_rob_wide3 <- dplyr::mutate(P_rob_wide2, 
                                         rob_FT_score = as.numeric(factor(rob_FT, levels = c(1,2,3))), 
                                         rob_IT_score = as.numeric(factor(rob_IT, levels = c(1,2,3))), 
                                         rob_PS_score = as.numeric(factor(rob_PS, levels = c(1,2,3))), 
                                         rob_RS_score = as.numeric(factor(rob_RS, levels = c(1,2,3))))
            P_rob_wide4 <-  dplyr::mutate(P_rob_wide3, 
                                          fpr_index = 1 - sp_index, 
                                          fpr_ref = 1 - sp_ref,
                                          Study = seq(from = 1, to = nrow(X)), 
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
              dplyr::select(Study, se_index, sp_index, se_ref, sp_ref,
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
                                         fpr_index = 1 - sp_index, 
                                         fpr_ref = 1 - sp_ref,
                                         ac_IT = rep(1, length = nrow(X)),
                                         ac_PS = rep(1, length = nrow(X)),
                                         ac_RS = rep(1, length = nrow(X)), 
                                         ac_IT_colour = ifelse(ac_IT == 1, "green",
                                                               ifelse(rob_IT == 2, "red", "grey")),
                                         ac_PS_colour = ifelse(ac_PS == 1, "green",
                                                               ifelse(rob_PS == 2, "red", "grey")),
                                         ac_RS_colour = ifelse(ac_RS == 1, "green",
                                                               ifelse(rob_RS == 2, "red", "grey")))  %>%
              dplyr::select(Study, se_index, sp_index, se_ref, sp_ref,
                            ac_PS, ac_IT, ac_RS,
                            ac_PS_score, ac_IT_score, ac_RS_score)
            
            P_ac_wide5 <- tibble(P_ac_wide4)
            
            P_rob_and_ac_wide <- left_join(P_ac_wide5, P_rob_wide5, by = "Study") %>% 
              dplyr::select(Study, se_index.x, sp_index.x,  se_ref.x, sp_ref.x, 
                            ac_PS, ac_IT, ac_RS,
                            ac_PS_score, ac_IT_score, ac_RS_score,
                            rob_PS, rob_IT, rob_RS, rob_FT, 
                            rob_PS_score, rob_IT_score, rob_RS_score, rob_FT_score) %>%
              dplyr::rename(se_index = se_index.x, 
                            sp_index = sp_index.x,  
                            se_ref = se_ref.x, 
                            sp_ref = sp_ref.x) %>%
              dplyr::mutate(fpr_index = 1 - sp_index, 
                            fpr_ref = 1 - sp_ref)
            
            P_rob_and_ac_wide
            
            colours_group = c("green", "red", "black")
            colours_category = c("white", "white", "white", "white", "white", "white", "white")
            
            # add little pie charts to plot
            g = g + new_scale_color() + 
              geom_scatterpie(data = P_rob_and_ac_wide, aes(y=se_index, x = fpr_index,
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
                              #size = 1, 
                              pie_scale = pie_scale, 
                              alpha = 1) +
              coord_equal() + 
              theme_bw() + 
              theme(
                legend.position = "left",
                legend.title = element_text(size=10), 
                legend.text = element_text(size=10)
              ) + 
              scale_fill_manual("Applicability concern (AC) or \nRisk of Bias (RoB) category", values = colours_category, 
                                labels= c("AC - Index \ntest", 
                                          "AC - Patient \nselection", 
                                          "AC - Reference \nstandard",
                                          "RoB - Flow and timing", 
                                          "RoB - Index test", 
                                          "RoB - Patient selection", 
                                          "RoB - Reference standard")) + 
              scale_colour_manual("Risk", values = colours_group) + 
              guides(fill = FALSE)
            
            g
          }
        }
        
        g
        
      })
      

      # Output ggplot object
      observe({
            
            output$plot <- renderPlot({  
              
              req(data(),  draws(), cancelOutput = TRUE)
              
              validate(
                need(data()$reference.cat, "Please select a dataset with a covariate for
                                            reference test type (inputted as a column named reference.cat)
                                            to use latent class model")
              )
              
              plot_object()
              
            }, height = as.numeric({input$plot_dimension_slider}))
        
      })
      
      # Download ggplot object 
      output$plot_download <- downloadHandler(
        
        filename = function(){
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
      
      # Study info for sROC plot (will display when plot is clicked on) ----------------------
      
      
      click <- reactive({
        input$plot_click
      })
      
      
      # # values to display on plot
      output$vals <- renderPrint({


        X <- data()
        N <- nrow(X)
        mod <- draws()
        
        no_cov <- num_covariates(X)$no_covariates  # number of covariates
        
        # Count the number of columns (used to determine if quality assessment data is present)
        C <- ncol(dplyr::select(X, -year.cts, -prevalence.cts))
        # Store the names of the columns
        Names <- colnames(dplyr::select(X, -year.cts, -prevalence.cts))
        
        # model-estimated study-specific points
        se_ref <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("se_ref"))$summary[,5])
        sp_ref <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sp_ref"))$summary[,5])
        se_index <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("se_index"))$summary[,5])
        sp_index <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sp_index"))$summary[,5])
        prevs <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("p"))$summary[,5])
        
        se_ref_LCI <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("se_ref"))$summary[,4])
        se_ref_UCI <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("se_ref"))$summary[,6])
        sp_ref_LCI <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sp_ref"))$summary[,4])
        sp_ref_UCI <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sp_ref"))$summary[,6])
        
        se_index_LCI <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("se_index"))$summary[,4])
        se_index_UCI <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("se_index"))$summary[,6])
        sp_index_LCI <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sp_index"))$summary[,4])
        sp_index_UCI <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sp_index"))$summary[,6])
        
        ss<- tibble( 
          Study =as.numeric(as.factor(X$author)), 
          TP=X$TP, FN=X$FN, FP=X$FP, TN=X$TN,
          N=(X$TP+X$FN+X$FP+X$TN) ,
          se_ref = se_ref   , 
          se_ref_LCI = se_ref_LCI,
          se_ref_UCI = se_ref_UCI,
          sp_ref = sp_ref  , 
          sp_ref_LCI = sp_ref_LCI,
          sp_ref_UCI = sp_ref_UCI,
          se_index = se_index   , 
          se_index_LCI = se_index_LCI,
          se_index_UCI = se_index_UCI,
          sp_index = sp_index  , 
          sp_index_LCI = sp_index_LCI,
          sp_index_UCI = sp_index_UCI,
          obs_prev = round((TP+FN)/N, 2), 
          est_prev = round(prevs, 2)
        )
        
        ss[] <- lapply(ss, function(x) if (is.numeric(x) ) round(x, 3) else x) # round all numeric cols to 3 s.f. 
        
        ss2 <- left_join(ss, X) %>% 
          dplyr::mutate(se_ref_95_CrI = paste0("(", se_ref_LCI, ",", se_ref_UCI, ")"), 
                        sp_ref_95_CrI = paste0("(", sp_ref_LCI, ",", sp_ref_UCI, ")"), 
                        se_index_95_CrI = paste0("(", se_index_LCI, ",", se_index_UCI, ")"), 
                        sp_index_95_CrI = paste0("(", sp_index_LCI, ",", sp_index_UCI, ")"), 
                        fpr_index = 1 - sp_index)
        
        if (no_cov > 0 ) { 
          if (C > 6 & Names[7] != "rob_PS") { j <<- 6 } else { j <<- 13  }
          # make cols without .cts and .cat endings for display
          for (i in 1:no_cov) { 
            ss2 <- dplyr::mutate(ss2, !!as.name( str_sub(colnames(X)[j + i], end = -5) ) := !!as.name( colnames(X)[j + i]) )
          }
          # select columns to display
          cols <- c("author", "year", str_sub( colnames(X)[c((j+1):(j+no_cov))] , end = -5),
                    "se_ref", "se_ref_95_CrI", 
                    "sp_ref", "sp_ref_95_CrI", 
                    "se_index", "se_index_95_CrI", 
                    "sp_index", "sp_index_95_CrI", 
                    "fpr_index",
                    "obs_prev", "est_prev")
        } else { # using dataset w/o covariates 
          ss2 <- ss2
        }
        
        data <- dplyr::select(ss2, cols)
        y <- nearPoints(df = data.frame(data), click(), xvar = "fpr_index", yvar = "se_index" )
        req(nrow(y) != 0)
        
        
        y
        
        
      })
      
      # tooltip
      output$my_tooltip <- renderUI({
        
        ns <- session$ns
        
        validate(
          need(!(is.null(draws())), "Please run model to display plot")
        )
        
        req(data(),  draws(), cancelOutput = TRUE)
        
        X <- data()
        N <- nrow(X)
        mod <- draws()
        
        no_cov <- num_covariates(X)$no_covariates  # number of covariates
        
        C <- ncol(dplyr::select(X, -year.cts, -prevalence.cts))
        Names <- colnames(dplyr::select(X, -year.cts, -prevalence.cts))
        
        # model-estimated study-specific points
        se_ref <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("se_ref"))$summary[,5])
        sp_ref <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sp_ref"))$summary[,5])
        se_index <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("se_index"))$summary[,5])
        sp_index <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sp_index"))$summary[,5])
        prevs <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("p"))$summary[,5])
        
        se_ref_LCI <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("se_ref"))$summary[,4])
        se_ref_UCI <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("se_ref"))$summary[,6])
        sp_ref_LCI <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sp_ref"))$summary[,4])
        sp_ref_UCI <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sp_ref"))$summary[,6])
        
        se_index_LCI <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("se_index"))$summary[,4])
        se_index_UCI <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("se_index"))$summary[,6])
        sp_index_LCI <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sp_index"))$summary[,4])
        sp_index_UCI <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sp_index"))$summary[,6])
        
        ss<- tibble( 
          Study =as.numeric(as.factor(X$author)), 
          TP=X$TP, FN=X$FN, FP=X$FP, TN=X$TN,
          N=(X$TP+X$FN+X$FP+X$TN) ,
          se_ref = se_ref   , 
          se_ref_LCI = se_ref_LCI,
          se_ref_UCI = se_ref_UCI,
          sp_ref = sp_ref  , 
          sp_ref_LCI = sp_ref_LCI,
          sp_ref_UCI = sp_ref_UCI,
          se_index = se_index   , 
          se_index_LCI = se_index_LCI,
          se_index_UCI = se_index_UCI,
          sp_index = sp_index  , 
          sp_index_LCI = sp_index_LCI,
          sp_index_UCI = sp_index_UCI,
          obs_prev = round((TP+FN)/N, 2), 
          est_prev = round(prevs, 2)
        )
        
        ss[] <- lapply(ss, function(x) if (is.numeric(x) ) round(x, 3) else x) # round all numeric cols to 3 s.f. 
        
        ss2 <- left_join(ss, X) %>% 
          dplyr::mutate(se_ref_95_CrI = paste0("(", se_ref_LCI, ",", se_ref_UCI, ")"), 
                        sp_ref_95_CrI = paste0("(", sp_ref_LCI, ",", sp_ref_UCI, ")"), 
                        se_index_95_CrI = paste0("(", se_index_LCI, ",", se_index_UCI, ")"), 
                        sp_index_95_CrI = paste0("(", sp_index_LCI, ",", sp_index_UCI, ")"), 
                        fpr_index = 1 - sp_index)
        
        if (no_cov > 0 ) { 
          if (C > 6 & Names[7] != "rob_PS") { j <<- 6 } else { j <<- 13  }
          # make cols without .cts and .cat endings for display
          for (i in 1:no_cov) { 
            ss2 <- dplyr::mutate(ss2, !!as.name( str_sub(colnames(X)[j + i], end = -5) ) := !!as.name( colnames(X)[j + i]) )
          }
          # select columns to display
          cols <- c("author", "year", str_sub( colnames(X)[c((j+1):(j+no_cov))] , end = -5),
                    "se_ref", "se_ref_95_CrI", 
                    "sp_ref", "sp_ref_95_CrI", 
                    "se_index", "se_index_95_CrI", 
                    "sp_index", "sp_index_95_CrI", 
                    "fpr_index",
                    "obs_prev", "est_prev")
        } else { # using dataset w/o covariates 
          ss2 <- ss2
        }
        
        data <- dplyr::select(ss2, cols)
        y <- nearPoints(df = data.frame(data), click(), xvar = "fpr_index", yvar = "se_index" )
        req(nrow(y) != 0)

        
        verbatimTextOutput(ns("vals"))
        
      })
      
      
      
      
      # Pop-up of study-specific pie charts onto sROC plot, when study is clicked on --------------
      output$piechart <- renderPlot({
        
        req(data(), draws(), input$plot_click, cancelOutput = TRUE)
        
        X <- data()
        N <- nrow(X)
        mod <- draws()
        
        e <- study_level_outcomes(X)
        

        no_cov <- num_covariates(X)$no_covariates  # number of covariates
        
        C <- ncol(dplyr::select(X, -year.cts, -prevalence.cts))
        Names <- colnames(dplyr::select(X, -year.cts, -prevalence.cts))
        
        # model-estimated study-specific points
        se_ref <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("se_ref"))$summary[,5])
        sp_ref <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sp_ref"))$summary[,5])
        se_index <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("se_index"))$summary[,5])
        sp_index <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("sp_index"))$summary[,5])
        prevs <- (rstan::summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("p"))$summary[,5])
        
        ss<- tibble( 
          Study =as.numeric(as.factor(X$author)), 
          TP=X$TP, FN=X$FN, FP=X$FP, TN=X$TN,
          N=(X$TP+X$FN+X$FP+X$TN) ,
          se_ref = se_ref   , 
          sp_ref = sp_ref  , 
          se_index = se_index   , 
          sp_index = sp_index  , 
          obs_prev = round((TP+FN)/N, 2), 
          est_prev = round(prevs, 2)
        )
        
        
        X <- left_join(ss, X) %>%
          dplyr::mutate(fpr_index = 1 - sp_index)
        
        if ('9' %in% input$QAcheck){
          ee <- data.frame(Author=X$author,Year=X$year, 
                           TP=X$TP, FN=X$FN, FP=X$FP, TN=X$TN, 
                           se_index=X$se_index, fpr_index=X$fpr_index, 
                           rob_FT=X$rob_FT, 
                           rob_IT=X$rob_IT,
                           rob_PS=X$rob_PS, 
                           rob_RS=X$rob_RS)
          clickselect_pc <- nearPoints(ee, input$plot_click, xvar = "fpr_index", yvar = "se_index", threshold=5, maxpoints=1)
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
        if ('10' %in% input$QAcheck){
          ee <- data.frame(Author=X$author,Year=X$year, 
                           TP=X$TP, FN=X$FN, FP=X$FP, TN=X$TN, 
                           se_index=X$se_index, fpr_index=X$fpr_index, 
                           ac_PS=X$ac_PS, ac_IT=X$ac_IT, ac_RS=X$ac_RS)
          clickselect_pc <- nearPoints(ee, input$plot_click,  xvar = "fpr_index", yvar = "se_index", threshold=5, maxpoints=1)
          clickselect_pc <- reshape(clickselect_pc, direction = "long", varying = c("ac_PS", "ac_IT", "ac_RS"), 
                                    v.names = "score", timevar = "rob", times = c("ac_PS", "ac_IT", "ac_RS"))
          labels_ac <- c("Patient selection", "Index test",  "Reference standard")
          weight <-c(1,1,1)
          pie(weight, labels_ac, main = "Applicability concern for each domain from the selected study",
              init.angle = -150,
              col=ifelse(clickselect_pc$score == 1, "green", ifelse(clickselect_pc$score == 2, "red", "black")))
          legend(0.9,0.1, c("Low", "High", "Unclear"), cex =0.7, fill = c("green", "red", "black"))
        }
        if ('11' %in% input$QAcheck){
          ee <- data.frame(Author=X$author,Year=X$year,
                           TP=X$TP, FN=X$FN, FP=X$FP, TN=X$TN, 
                           se_index=X$se_index, fpr_index=X$fpr_index, 
                           rob_PS=X$rob_PS, rob_IT=X$rob_IT, rob_RS=X$rob_RS, rob_FT=X$rob_FT,
                           ac_PS=X$ac_PS, ac_IT=X$ac_IT, ac_RS=X$ac_RS)
          clickselect_pc <- nearPoints(ee, input$plot_click,  xvar = "fpr_index", yvar = "se_index", threshold=5, maxpoints=1)
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


  





