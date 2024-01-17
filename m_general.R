

HSROC_curve_type_renderUI  <- function(id) {  
  
  moduleServer(
    id,
    function(input, output, session) {
      
      
      output$HSROC_curve_type_ui <- renderUI({ 
        
        ns <- session$ns 
        
        if (2 %in% input$HSROCcheck) { 
          awesomeCheckbox(inputId = ns("HSROCcheck_unrestricted_HSROC_curve"), 
                          label = "extrapolate HSROC curve beyond observed Se/Sp data points?", 
                          FALSE)
        }
        else { }
        
      })

      outputOptions(output, 'HSROC_curve_type_ui', suspendWhenHidden=FALSE)
      
    }
  )
}




p_scale_priors_indicator_checkbox_UI <- function(id) {
  ns <- NS(id) 
  awesomeCheckbox(inputId = ns("p_scale_priors_indicator"), 
                  "Specify priors for sensitivities and specificities directly on the probability scale?", 
                  TRUE)
}


p_scale_priors_indicator_checkbox_out <-  function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    vals <- reactiveValues()
    
    observe({vals$p_scale_priors_indicator <- input$p_scale_priors_indicator})
    # Run the Garabage Collector to Ensure any excess memory used by stan is freed
    gc()
    return(vals)
    
  }
  )
}

# server function for Covariate to display on plot (for info) -----------  -----------------------------------------------------------------------------------------
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


# progress indicators  / loading bars  ------------------------------------- -------------------------------------
progress_prior_model_UI <- function(id) {
  ns <- NS(id) 
  uiOutput(ns("progress_prior_model_ui"))
}

progress_main_model_UI <- function(id) {
  ns <- NS(id) 
  uiOutput(ns("progress_main_model_ui"))
}


progress_prior_model_server <- function(id) { 

  moduleServer(
    id,
    function(input, output, session) {
      output$progress_prior_model_ui <- renderUI({ 
        ns <- session$ns
        verbatimTextOutput(ns("progress_prior_model"))
      })
    }
  )
  
}

progress_main_model_server <- function(id) {  
  
  moduleServer(
    id,
    function(input, output, session) {
      output$progress_main_model_ui <- renderUI({ 
        ns <- session$ns
        verbatimTextOutput(ns("progress_main_model"))
      })
    }
  )
  
}



# Priors plot UI -------------------------------------  ---------------------
model_priors_plot_UI <- function(id) {
  ns <- NS(id)  
  tagList(
    dropdownButton(
      tagList(
        sliderInput(inputId = ns("model_priors_plot_dimension_slider_height"), 
                    label = "Change size of plot - height", 
                    min = 1,
                    max = 2000, 
                    value = 600, 
                    ticks = FALSE),
        sliderInput(inputId = ns("model_priors_plot_dimension_slider_width"), 
                    label = "Change size of plot - width", 
                    min = 1, 
                    max = 2000, 
                    value = 1000,
                    ticks = FALSE),
        # Download plot:
        h5("Download plot:"),
        numericInput(inputId =  ns("priors_plot_dl_width"), label=h5("Plot width"), value = 5),
        numericInput(inputId =  ns("priors_plot_dl_height"), label=h5("Plot height"), value = 5),
        numericInput(inputId =  ns("priors_plot_dl_dpi"), label=h5("Plot DPI"), value = 600),
        downloadButton(outputId = ns("download_priors_plot"), label = "Download Plot"),
        # downloadButton(outputId = ns("download_priors_plot"), 
        #                label = "Download Plot")
      ),
      circle = TRUE, 
      status = "danger",
      icon = icon("gear"),
      width = "300px",
      tooltip = tooltipOptions(title = "Click to customise plot")
    ),
    plotOutput(outputId =  ns("model_priors_plot"))
  )
}


# Dataset import ---------------------------------- --------------------------------------------------------------------------------------------------------------------------------------------
dataset_import_ui <- function(id) {
  ns <- NS(id) 
  tagList(
    uiOutput(ns("data_input")),
    helpText("Default maximum file size is 5MB"),
    tags$hr(),
    h4(helpText(tags$strong("File options"))),
    awesomeCheckbox(inputId = ns("header"), label = "First row as column headings", value = TRUE),
    actionButton(ns("reset_file"), "Reset file input"),
    br(),
    awesomeRadio(inputId = ns("default"),
                 label = h4(helpText(tags$strong("Select example dataset"))),
                 choices = list("Standard" = 1, 
                                "With Quality Assessment" = 2,
                                "With Covariates" = 3,
                                "With Quality assessment and Covariates" = 4), 
                 selected = 4),
    br(),
    h4(helpText(tags$strong("Download example datasets"))),
    downloadButton(ns("downloadData1"), "Standard Example"),
    br(),
    downloadButton(ns("downloadData2"), "Quality Assessment Example"),
    br(),
    downloadButton(ns("downloadData3"), "Covariate Example"),
    br(),
    downloadButton(ns("downloadData4"), "Quality Assessment and Covariate Example")
  )
}


dataset_default_import_server <- function(id,
                                          QA, Cov, QA_Cov, Standard) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # Default data
      default_data <- reactive({
        df <- NULL
        if ('2' %in% input$default) {
          df <- QA
        } else if ('3' %in% input$default) {
          df <- Cov
        } else if ('4' %in% input$default) {
          df <- QA_Cov
        } else {
          df <- Standard
        }
        df <- df %>%
          dplyr::mutate(year.cts = year, prevalence.cts = round((TP+FN)/(TP+FN+FP+TN), 3))
        
        return(df)
        
      })

      output$defaultData <- default_data
      
      return(default_data)
      
    }
  )
}


# "Data for analysis" tab ---------------------- -----------------------

data_for_analysis_UI <- function(id) {
  ns <- NS(id)  
  DT::dataTableOutput(outputId =  ns("tb"))
}


data_for_analysis_server <- function(id,
                                     data) { 
  moduleServer(
    id, 
    function(input, output, session) { 

      data_table_obj <- reactive({
        table <- req(data()) %>%
          dplyr::select(-year.cts)
        return(table)
      })
      
      output$tb <- DT::renderDataTable({  
        options(DT.options = list(pageLength = 30, 
                                  autoWidth = TRUE, 
                                  scrollX=T))
        return(DT::datatable( data_table_obj() ))
      })
      
      }
   )
}

# Dataset import  ------------------------ ---------------------------------
dataset_import_server <- function(id,
                                  defaultData) {

  moduleServer(
    id,
    function(input, output, session) {

      # Create a definable reactive value to allow reloading of data
      reload <- reactiveVal(FALSE)

      # Initalise input field
      output$data_input <- renderUI(DefaultFileInput(id))

      # Reset the data input and load default data when user
      # clicks the reset button
      observeEvent(input$reset_file, {
        output$data_input <- renderUI(DefaultFileInput(id))
        reload(TRUE)
      })

      # Set the reload value to false when data is uploaded
      observeEvent(input$data_input, {
        reload(FALSE)
      })

      # Make the data file that was uploaded reactive
      data <- reactive({
        file1 <- input$data_input
        if (reload()) {
          return(defaultData())
        }
        if (is.null(file1)) {
          return(defaultData())
        } else {
          tryCatch({
            tmp_df <- rio::import(file1$datapath,
              header = input$header,
              stringsAsFactors = FALSE
            )
            if (!IsValid(tmp_df)) {
              stop(
                paste0(
                  paste(GetMissingCols(tmp_df), collapse = ", ")
                ),
                " column(s) are Missing from Data"
              )
            }
            tmp_df <- tmp_df %>%
              dplyr::mutate(
                year.cts = year, prevalence.cts = round((TP+FN)/(TP+FN+FP+TN), 3)
              )
            return(tmp_df)
          }, error = function(e) {
            shinyalert::shinyalert(
              title = "Error",
              text = e$message,
              type = "error"
            )
            output$data_input <- renderUI(DefaultFileInput(id))
            return(defaultData())
          })
        }
      })
      # Explicitly return the data reactive
      return(data)
    }
  )
}



dataset_example_download_server <- function(id,
                                            QA, Cov, QA_Cov, Standard
                                            ) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # Allow users the option to download the standard example dataset
      output$downloadData1 <- downloadHandler(
        # Specify the file name 
        filename = function(){
          paste("Standard.csv")
        },
        content = function(file){
          Standard
          write.table(Standard, file, sep=",", row.names=FALSE)
        }
      )
      
      # Allow users the option to download the quality assessment example dataset
      output$downloadData2 <- downloadHandler(
        # Specify the file name 
        filename = function(){
          paste("QA.csv")
        },
        content = function(file){
          QA
          write.table(QA, file, sep=",", row.names=FALSE)
        }
      )
      
      # Allow users the option to download the covariate example dataset
      output$downloadData3 <- downloadHandler(
        # Specify the file name
        filename = function(){
          paste("Cov.csv")
        },
        content = function(file){
          Cov
          write.table(Cov, file, sep=",", row.names=FALSE)
        }
      )
      
      # Allow users the option to download the quality assessment and covariate example dataset
      output$downloadData4 <- downloadHandler(
        # Specify the file name
        filename = function(){
          paste("QA_Cov.csv")
        },
        content = function(file){
          QA_Cov
          write.table(QA_Cov, file, sep=",", row.names=FALSE)
        }
      )
      
    }
  )
}


# Buttons --------------------------------------------------------------------   ------------------------------------------------------------------------------------

# Reset button 

# UI function to output the reset button
reset_button_UI <- function(id) {
  ns <- NS(id)  
  tagList(
    br(), 
    actionButton(inputId = ns("reset"), 
                 label = "Reset all inputs"), 
    br()
  )
}

# Button to run prior model 

# UI function for "run prior model" button
run_prior_model_button_UI <- function(id) {
  ns <- NS(id)
  tagList( 
    br(), 
    actionButton(inputId = ns("run_prior_model"),
                 label = "Click to run prior model"), 
    add_busy_bar(color = "#005398", height = "8px"),
    br()
  )
}


# Server function for "run prior model" button
run_prior_model_button_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive(input$run_prior_model)
  })
}


# Button to run model  
# UI function for "run model" button
run_model_button_UI <- function(id) {
  ns <- NS(id)  
  tagList(
    br(), 
    actionButton(inputId = ns("run_model"),
                 label = "Click to run model"), 
    add_busy_bar(color = "#005398", height = "8px"), 
    br()
  )
}


# Server function for "run model" button
run_model_button_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive(input$run_model)
  })
}

 
# Sensitivity analysis functions  ---------------------------------------------  -------------------------------------------------------

# UI function which allows users to select which studies to exclude for SA

SA_indicator_UI <- function(id) {
  ns <- NS(id)  
  tagList(
    awesomeCheckbox(ns("SA_indicator"), 
                    "Run sensitivity analysis?", 
                    FALSE),
    conditionalPanel(condition = 'input.SA_indicator==1',
                     ns = ns, 
                     uiOutput(ns("SA_list_studies_ui")))
  )
}



SA_indicator_out <-  function(id) {
  
  moduleServer(id, 
               function(input, output, session) {
    
    vals <- reactiveValues()
    
    observe({
      vals$SA_indicator <- input$SA_indicator
      })
    # Run the Garabage Collector to Ensure any excess memory used by stan is freed
    gc()
    return(vals)
    
  }
 )
}


SA_run_model_button_UI <- function(id) {
    ns <- NS(id)  
    actionButton(inputId = ns("run_model_sa"),
                 label = "Click to run sensitivity analysis model")
}

# Server function for "run model" button
SA_run_model_button_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive(input$run_model_sa)
  }
 )
}




# server-side function to input sampler options  - outputs a list of inputs which can be called within the shiny program
SA_list_studies_server <- function(id, data) {  
  
  moduleServer(
    id,
    function(input, output, session) {
      
      SA_list_studies_obj <- reactive({
        
        X <- data()
        
        sadf <- data.frame(Author=X$author, Year=X$year, TP=X$TP, FN=X$FN, FP=X$FP, TN=X$TN)
        NT <- nrow(sadf) # count the number of rows of data
        a <- as.factor(sadf$Author)
        choicesTrials <- setNames(as.numeric(a), sadf$Author)

        return(choicesTrials)
        
      })
      
      output$SA_list_studies_ui <- renderUI({
        
        ns <- session$ns
        
        tagList(
           awesomeCheckboxGroup(ns("triallist"), 
                               label=h4("Select studies to exclude:"),
                               choices=SA_list_studies_obj(),
                               selected=NULL),
           DT::dataTableOutput(outputId =  ns("SA_data_table_ui"))
        )
        
      })
      
    }
  )
}

SA_triallist_out <- function(id) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      
      vals <- reactiveValues()
      observe({vals$triallist <- input$triallist})
      # Run the Garabage Collector to Ensure any excess memory used by stan is freed
      gc()
      return(vals)
      
    }
  )
}


# uses input$triallist from SA_list_studies_server function above 

SA_data_server <- function(id, data) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$SA_data <- reactive({ 
        
        req(data(),  cancelOutput = TRUE)
        
        X <- data()
        
        sa_data <- X %>% 
          dplyr::mutate(author2 = as.numeric(as.factor(author))) %>%
          dplyr::filter(!(author2 %in% c(input$triallist))) %>%
          dplyr::select(-author2)
        
        
        #print(sa_data)
        # Run the Garabage Collector to Ensure any excess memory used by stan is freed
        gc()
        return(sa_data)
      })
      
    }
  )
}

SA_data_table_server <- function(id, data) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$SA_data_table_ui <- DT::renderDataTable({  
        
        options(DT.options = list(pageLength = 30))
        req(data(),  cancelOutput = TRUE)
        
        X <- data()
        
        sa_data <- X %>% 
          dplyr::mutate(author2 = as.numeric(as.factor(author))) %>%
          dplyr::filter(!(author2 %in% c(input$triallist))) %>%
          dplyr::select(-author2)
        
      #  print(sa_data) 
        
        sa_data
        
      })
    }
  )
}



SA_display_conditional_plot_checkbox_renderUI     <- function(id, 
                                                              SA_indicator) {  
  
  moduleServer(
    id,
    function(input, output, session) {

            output$SA_display_conditional_plot_checkbox_ui   <- renderUI({
              
              ns <- session$ns
              SA_indicator <- SA_indicator$SA_indicator
              
              if (SA_indicator == TRUE) { 
                awesomeCheckbox(inputId = ns("SA_display_indicator"),
                                "Display sensitivity analysis estimates",
                                TRUE)
              }
              else { } 
              
            })

outputOptions(output, "SA_display_conditional_plot_checkbox_ui", suspendWhenHidden = FALSE)

    }
  )
}



##### Prevalence tab functions ---------------------------------------------------------  ---------------------------------------------------------------------------------------------------------------


# Prevalence tab 'master' UI  module -----------------------  -------
prevalence_tab_UI <- function(id) {
  ns <- NS(id)  
  tagList(
    prevalence_tab_settings_menu_UI(id = "MA_model_id"), 
    prevalence_plot_UI(id = "MA_model_id"),
    br(), 
    br(),
    conditionalPanel(condition = 'input.SA_indicator==1', 
                     ns = ns, 
                     tagList(
                      h4("Sensitivity analysis"), 
                      prevalence_tab_settings_menu_UI(id = "MA_SA_model_id"),
                      prevalence_plot_UI(id = "MA_SA_model_id")
                     ))
  )
}


prevalence_tab_settings_menu_UI <- function(id) {
  ns <- NS(id)  
  tagList(
    uiOutput(ns("Prev_input")),
    actionButton(inputId = ns("Prev_reset"), 
                 label = "Reset inputs"),
    br(),
    br(),
    radioButtons(inputId = ns("treecheck"),
                 label = "Choose format to view expected results", 
                 choices = list("Tree 1"=1, "Tree 2" = 2),
                 selected = 1)
  )
}


prevalence_plot_UI <- function(id) {
  ns <- NS(id)  
  tagList(
    plotOutput(ns("prev_treeplot")),
    radioButtons(ns("prev_treeplot_filetype"), 
                 label = "Select image format", 
                 choices = list("png", "PDF")),
    downloadButton(ns("prev_treeplot_download"), 
                   "Download Plot"),
    br(),
    br(),
    p("Note: The numbers in brackets represent 95% posterior intervals."),
    
  )
}

      
# Display inputs for the prevalence tab
Prev_input_server <- function(id, data) {  # "mod" is the rstan file which is reactive. 
              moduleServer(
                id,
                function(input, output, session) {
                  
                Prev_input_obj <- reactive({

                            times <- input$Prev_reset
                            
                            X <- data()
                            Prev <- ((X$TP + X$FN) / (X$TP + X$FN + X$FP + X$TN))*100 # multiply by 100 to get as a percentage
                            meanPrev <- round(mean(Prev), 0)
                            
                            meanPrev
                 })
                  
                 output$Prev_input <- renderUI({
                   
                   ns <- session$ns 
                   
                        tagList(
                          numericInput(inputId = ns("patients"), label = "Number of patients", min = 0, value = 1000),
                          sliderInput(inputId = ns("prevslide"), label = "Prevalence of the disease in the population (as a percentage)",
                                      min = 0, max =100, step = 0.1, width = '100%', value = Prev_input_obj()),
                          p("The default value of the slider is the mean value of the prevalence of the disease from all studies included in the dataset.")
                        )
                 })
                 
                  
                  
    }
  )
}



#  Plot the tree diagram

# Display inputs for the prevalence tab
Prev_plot_server <- function(id, 
                              data, 
                              draws) { 
  moduleServer(
    id,
    function(input, output, session) {
                                
                          
              prev_treeplot_obj <- function() {
                
                                  req(data(), draws(), cancelOutput = TRUE)
   
                                  X <- data()
                                  mod <- draws()
                                  
                                  N <- length(X$TP)
  
                                  # Sensitivity and specificity with 95% CI's 
                                  Sens <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se"))$summary[5]
                                  Sens_lci <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se"))$summary[4]
                                  Sens_uci <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Se"))$summary[6]
                                  Spec <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Sp"))$summary[5]
                                  Spec_lci <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Sp"))$summary[4]
                                  Spec_uci <- summary(mod, probs = c(0.025,  0.5, 0.975), pars = c("Sp"))$summary[6]
                                  
                                  # Calculate TP, FP, FN and TN based on sensitivity, specificity and inputted prevalence 
                                  Diseased <-(input$prevslide / 100) * input$patients # assuming population of 1000 
                                  NonDiseased <- input$patients - Diseased 
                                  
                                  TP <- round(Sens*Diseased)
                                  TN <- round(Spec*NonDiseased)
                                  FN <- round((1-Sens)*Diseased)
                                  FP <- round((1-Spec)*NonDiseased)
                                  Pos <- TP + FP # number of positive results
                                  Neg <- TN + FN # number of negative results
                                  
                                  # Calculate for lci and uci 
                                  TP_lci <- round(Sens_lci*Diseased)
                                  TP_uci <- round(Sens_uci*Diseased)
                                  
                                  TN_lci <- round(Spec_lci*NonDiseased)
                                  TN_uci <- round(Spec_uci*NonDiseased)
                                  
                                  # Calculations of lci and uci specifically for FN and FP differ slightly
                                  # the lci is used to calculate the uci and vice versa due to the mapping of sens/spec and 1-sens/1-spec
                                  FN_uci <- round((1-Sens_lci)*Diseased)
                                  FP_uci <- round((1-Spec_lci)*NonDiseased)
                                  
                                  FN_lci <- round((1-Sens_uci)*Diseased)
                                  FP_lci <- round((1-Spec_uci)*NonDiseased)
                                  
                                  Pos_lci <- TP_lci  + FP_lci  # number of positive results
                                  Neg_lci <- TN_lci  + FN_lci  # number of negative results
                                  
                                  Pos_uci <- TP_uci  + FP_uci  # number of positive results
                                  Neg_uci <- TN_uci  + FN_uci  # number of negative results
                                  
                                  Diseased_lci <- TP_lci + FN_lci
                                  NonDiseased_lci <- TN_lci + FP_lci
                                  
                                  Diseased_uci <- TP_uci + FN_uci
                                  NonDiseased_uci <- TN_uci + FP_uci
                                  
                                  # Plot tree 1 - patients - positive/negative -  
                                  if ('1' %in% input$treecheck){
                                    par(mar=c(0,0,0,0))
                                    plot(-1:6,-1:6,type="n", xlim=c(0.3,4.7), ylim=c(-1,6), xlab="", ylab="", axes=F)
                                    
                                    text(2.5,5.2,labels=bquote(paste(.(input$patients), " Patients", " (", .(input$prevslide), "% Prevalence)" )), cex=1.4, col="blue")
                                    
                                    text(1.5,3.75,labels=bquote(paste(.(Pos), " (", .(Pos_lci), ", ", .(Pos_uci),")")), cex=1.2, col="blue")
                                    text(3.5,3.75,labels=bquote(paste(.(Neg), " (", .(Neg_lci), ", ", .(Neg_uci),")")), cex=1.2, col="blue")
                                    text(1.5,3.45,"test +ve", cex=1.2)
                                    text(3.5,3.45,"test -ve", cex=1.2)
                                    
                                    text(1,2.1,labels=bquote(paste(.(TP), " (", .(TP_lci), ", ", .(TP_uci),")")), cex=1.2, col="blue")
                                    text(1,1.8," are diseased", cex=1.2)
                                    text(1,1.5,"and test +ve", cex=1.2)
                                    
                                    text(2,2.1,labels=bquote(paste(.(FP), " (", .(FP_lci), ", ", .(FP_uci),")")), cex=1.2, col="blue")
                                    text(2,1.8," are not diseased", cex=1.2)
                                    text(2,1.5,"but test +ve", cex=1.2)
                                    
                                    text(3,2.1,labels=bquote(paste(.(FN), " (", .(FN_lci), ", ", .(FN_uci),")")), cex=1.2, col="blue")
                                    text(3,1.8, " are diseased", cex=1.2)
                                    text(3,1.5,"but test -ve", cex=1.2)
                                    
                                    text(4,2.1,labels=bquote(paste(.(TN), " (", .(TN_lci), ", ", .(TN_uci),")")), cex=1.2, col="blue")
                                    text(4,1.8," are not diseased", cex=1.2)
                                    text(4.0,1.5,"and test -ve", cex=1.2)
                                    
                                    ax<-c(2.2,2.8,1.4,1.6,3.4,3.6) 
                                    ay<-c(4.8,4.8,3.2,3.2,3.2,3.2)
                                    axx<-c(1.5,3.5,1.1,1.9,3.1,3.9)
                                    ayy<-c(4.1,4.1,2.3,2.3,2.3,2.3)
                                    
                                    for ( i in (1:6) ) {
                                      arrows(x0=ax[i],y0=ay[i], x1=axx[i],y1=ayy[i], lwd=2)
                                    }
                                    
                                    rasterImage(TPimg,0.8,-0.5,1.2,1.2)
                                    rasterImage(FPimg,1.8,-0.5,2.2,1.2)
                                    rasterImage(FNimg,2.8,-0.5,3.2,1.2)
                                    rasterImage(TNimg,3.8,-0.5,4.2,1.2)
                                    
                                  }
                                  
                                  if ('2' %in% input$treecheck){
                                    par(mar=c(0,0,0,0))
                                    plot(-1:6,-1:6,type="n", xlim=c(0.3,4.7), ylim=c(-1,6), xlab="", ylab="", axes=F)
                                    
                                    
                                    text(2.5,5.2,labels=bquote(paste(.(input$patients), " Patients", " (", .(input$prevslide), "% Prevalence)" )), cex=1.4, col="blue")
                                    
                                    text(1.5,3.75,labels=bquote(paste(.(Diseased), " (", .(Diseased_lci), ", ", .(Diseased_uci),")")), cex=1.2, col="blue")
                                    text(3.5,3.75,labels=bquote(paste(.(NonDiseased), " (", .(NonDiseased_lci), ", ", .(NonDiseased_uci),")")), cex=1.2, col="blue")
                                    text(1.5,3.45,"are diseased", cex=1.2)
                                    text(3.5,3.45,"are healthy", cex=1.2)
                                    
                                    text(1,2.1,labels=bquote(paste(.(TP), " (", .(TP_lci), ", ", .(TP_uci),")")), cex=1.2, col="blue")
                                    text(1,1.8," are diseased", cex=1.2)
                                    text(1,1.5,"and test +ve", cex=1.2)
                                    
                                    text(2,2.1,labels=bquote(paste(.(FN), " (", .(FN_lci), ", ", .(FN_uci),")")), cex=1.2, col="blue")
                                    text(2,1.8," are diseased", cex=1.2)
                                    text(2,1.5,"but test -ve", cex=1.2)
                                    
                                    text(3,2.1,labels=bquote(paste(.(FP), " (", .(FP_lci), ", ", .(FP_uci),")")), cex=1.2, col="blue")
                                    text(3,1.8, " are not diseased", cex=1.2)
                                    text(3,1.5,"but test +ve", cex=1.2)
                                    
                                    text(4,2.1,labels=bquote(paste(.(TN), " (", .(TN_lci), ", ", .(TN_uci),")")), cex=1.2, col="blue")
                                    text(4,1.8," are not diseased", cex=1.2)
                                    text(4.0,1.5,"and test -ve", cex=1.2)
                                    
                                    ax<-c(2.2,2.8,1.4,1.6,3.4,3.6) 
                                    ay<-c(4.8,4.8,3.2,3.2,3.2,3.2)
                                    axx<-c(1.5,3.5,1.1,1.9,3.1,3.9)
                                    ayy<-c(4.1,4.1,2.3,2.3,2.3,2.3)
                                    
                                    for ( i in(1:6) ) {
                                      arrows(x0=ax[i],y0=ay[i], x1=axx[i],y1=ayy[i], lwd=2)
                                    }
                                    
                                    rasterImage(TPimg,0.8,-0.5,1.2,1.2)
                                    rasterImage(FNimg,1.8,-0.5,2.2,1.2)
                                    rasterImage(FPimg,2.8,-0.5,3.2,1.2)
                                    rasterImage(TNimg,3.8,-0.5,4.2,1.2)
                                  }
                                  
                                                  
              }
              
              # Output plot object
              output$prev_treeplot <- renderPlot({  
                
                req(data(), draws(), cancelOutput = TRUE)
                
                prev_treeplot_obj()
                
              })
              
              # Download plot object 
              output$prev_treeplot_download <- downloadHandler(
                filename =  function(){
                  paste("MA_tree", input$prev_treeplot_filetype, sep=".")
                },
                content = function(file) { 
                                      if (input$prev_treeplot_filetype == "png") {
                                        png(file)
                                        prev_treeplot_obj()
                                        dev.off()
                                      }
                                      else {
                                          pdf(file)
                                          prev_treeplot_obj()
                                          dev.off()
                                      }

                }
              )
              
    }
  )
}



       




  
##### Posterior and trace plots functions  ------------------------------------  -------------------
model_priors_plot_settings_UI <- function(id) {
  ns <- NS(id)  
  tagList(
    numericInput(ns("priors_plot_nrow"), 
                 label = "Number of rows",
                 value = 4),
    numericInput(ns("priors_plott_ncol"), 
                 label = "Number of columns",
                 value = 5),
    sliderInput(inputId = ns("priors_plot_dimension_slider_height"), 
                label = "Change size of plot - height", 
                min = 1, max = 2000, 
                value = 600,
                ticks = FALSE),
    sliderInput(inputId = ns("priors_plot_dimension_slider_width"), 
                label = "Change size of plot - width", 
                min = 1, max = 2000, 
                value = 1000,
                ticks = FALSE)
  )
  
}





# Trace plots ----------------------------------------  ---------------------

model_trace_plots_UI <- function(id) {
  ns <- NS(id)  
  plotOutput(outputId =  ns("trace_plots"))
}

model_trace_plots_settings_UI <- function(id) {
  ns <- NS(id)  
  tagList(
    numericInput(ns("trace_plot_nrow"), 
                 label = "Number of rows",
                 value = 10),
    numericInput(ns("trace_plot_ncol"), 
                 label = "Number of columns",
                 value = 10),
    sliderInput(inputId = ns("trace_plot_dimension_slider_height"), 
                label = "Change size of plot - height", 
                min = 1, max = 2000, 
                value = 300,
                ticks = FALSE),
    sliderInput(inputId = ns("trace_plot_dimension_slider_width"), 
                label = "Change size of plot - width", 
                min = 1, max = 2000, 
                value = 500,
                ticks = FALSE),
    # Download plot:
    h5("Download plot:"),
    numericInput(inputId =  ns("trace_plot_dl_width"), label=h5("Plot width"), value = 10),
    numericInput(inputId =  ns("trace_plot_dl_height"), label=h5("Plot height"), value = 5),
    numericInput(inputId =  ns("trace_plot_dl_dpi"), label=h5("Plot DPI"), value = 600),
    downloadButton(outputId = ns("download_trace_plot"), label = "Download Plot")
  )
}




# Posterior density plots --------------------------------  -----------------------

model_posterior_density_plots_UI <- function(id) {
  ns <- NS(id)  
  plotOutput(outputId =  ns("posterior_density_plots"))
}

model_posterior_density_plots_settings_UI <- function(id) {
  ns <- NS(id)  
  tagList(
    numericInput(ns("posterior_density_plot_nrow"), 
                 label = "Number of rows",
                 value = 10),
    numericInput(ns("posterior_density_plot_ncol"), 
                 label = "Number of columns",
                 value = 10),
    sliderInput(inputId = ns("posterior_density_plot_dimension_slider_height"), 
                label = "Change size of plot - height", 
                min = 1, max = 2000, 
                value = 300, 
                ticks = FALSE),
    sliderInput(inputId = ns("posterior_density_plot_dimension_slider_width"), 
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



##### Stan sampler diagnostics table -----------------------------------------    ----------------------------

sampler_diagnostics_table_UI <- function(id) {
  ns <- NS(id)   
 # tableHTML_output(outputId =  ns("sampler_diagnostics_table"))
  DT::dataTableOutput(ns("sampler_diagnostics_table"))
}

sampler_diagnostics_table_server <- function(id, draws, sampler_options) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
      sampler_diagnostics_table_obj <- reactive({ 
            
            n_divs <- get_num_divergent(draws())
            n_low_treedepth <- get_num_max_treedepth(draws())
            
            Z <- data.frame(Info = c("divergent transitions", 
                                     "iterations which exceeded 
                                           max treedepth"),
                            Number = c(n_divs,
                                       n_low_treedepth))
    
            return(Z)
      
        })
      
      
     #  output$sampler_diagnostics_table <- render_tableHTML({
     #    
     #        data <- sampler_diagnostics_table_obj()
     #        
     #        # calc. total number of post-warmup iterations
     #        total_iter <- (sampler_options$MA_total_iter - sampler_options$MA_warmup_iter)*sampler_options$MA_num_chains
     #          
     #        data %>% 
     #          tableHTML(rownames = TRUE) %>% 
     #          add_css_conditional_column(columns = c("Number"), 
     #                                     conditional = '==',
     #                                     value = 0,
     #                                     css = list(c('background-color'),
     #                                                c('green'))) %>%
     #          # set threshold for red text to 1% of iterations
     #          add_css_conditional_column(columns = c("Number"), 
     #                                     conditional = 'between',
     #                                     between = c(1,total_iter*0.01),
     #                                     css = list(c('background-color'),
     #                                              c('orange'))) %>% 
     #          add_css_conditional_column(columns = c("Number"), 
     #                                     conditional = '>',
     #                                     value = total_iter*0.01, 
     #                                     css = list(c('background-color'),
     #                                                c('red')))
     #      
     # })

      output$sampler_diagnostics_table <- DT::renderDataTable({ 
        
        options(DT.options = list(   autoWidth = TRUE) )

        data <- sampler_diagnostics_table_obj()
        data
        
      })
      
       }
    )
}


# Stan sampler options (general function) -------------------------------------   ------------------------------------------------------

# UI function to input sampler options 
sampler_options_UI <- function(id) {
  ns <- NS(id)                  
  uiOutput(ns("sampler_options"))
}

# server-side function to input sampler options  - outputs a list of inputs which can be called within the shiny program
sampler_options_server <- function(id) {  
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$sampler_options <- renderUI({
        
        ns <- session$ns
        
        div(id = ns("sampler_options_div"), 
          tagList(
              h4("Stan sampler options"),
              numericInput(inputId = ns("MA_max_treedepth"), label=h5("Set the maximum treedepth"), value = 10),
              numericInput(inputId = ns("MA_adapt_delta"), label=h5("Set the adapt_delta"), value = 0.80),
              numericInput(inputId = ns("MA_num_chains"), label=h5("Total number of chains"), value = 2),
              numericInput(inputId = ns("MA_warmup_iter"), label=h5("Number of warmup iterations"), value = 500),
              numericInput(inputId = ns("MA_total_iter"), label=h5("Total number of iterations"), value = 1500),
              numericInput(inputId = ns("MA_seed"), label=h5("Set the seed"), value = 123)
          )
        )
          
      })
      
      observeEvent(input$reset,{
        shinyjs::reset("sampler_options_div")
      })
      
      
      outputOptions(output, "sampler_options", suspendWhenHidden = FALSE)
    }
    
  )
}

# module to extract values from sampler_options_server 
# useful to use across modules w/ different namespaces, e.g. for access MA parameters for SA_MA

sampler_options_inputModule_server <- function(id) {  
  
  moduleServer(
    id,
    function(input, output, session) {
      
      vals <- reactiveValues()
      
      observe({vals$MA_num_chains <- input$MA_num_chains})
      observe({vals$MA_total_iter <- input$MA_total_iter})
      observe({vals$MA_warmup_iter <- input$MA_warmup_iter})
      observe({vals$MA_adapt_delta <- input$MA_adapt_delta})
      observe({vals$MA_max_treedepth <- input$MA_max_treedepth})
      observe({vals$MA_seed <- input$MA_seed})
      # Run the Garabage Collector to Ensure any excess memory used by stan is freed
      gc()
      return(vals)

    }
   )
}





































