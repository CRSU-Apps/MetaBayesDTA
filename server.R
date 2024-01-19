###############################################################################
#### IMPORTANT NOTE FOR ****** APP DEVELOPERS *******
###############################################################################
# At the time of writing, it is only possible to publish this app to shinyapps.io
# if using a UNIX-based opertating sustem (e.g., Linux Mint, Ubuntu, MacOS)
# This is because the Stan models need to be pre-compiled, and the compiler
# flags (i.e., CXXFLAGS) need to match the ones that
# shinyapps.io (shinyapps.io is hosted on a UNIX server).
# See https://discourse.mc-stan.org/t/compile-stan-model-in-shiny-app/10022/16
# for more details
# # Although not using pre-compiled models would allow app developers to publish
# the app using Windows, Without doing this, users would need to wait 15+ minutes 
# for the models to compile before  they can even start using the app, every
# time the app is opened or refreshed!
###############################################################################

server <- function(input, output, session) {

  shinyalert(title = "Message from Authors",
             text =  paste0("If you have time it would be greatly appreciated if you could fill 
                            out the user feedback questionnaire ",
                            tags$a(href="https://docs.google.com/forms/d/e/1FAIpQLSdBvMFpWma87JV1R0lAkmiVWxcIFf9I0m2BiDS6JV20MrvE9Q/viewform?vc=0&c=0&w=1&flr=0"
                                   , "here.",target="_blank"),
                            " Please report any bugs or suggestions for new features to the CRSU Team at ", tags$a(href="mailto:apps@crsu.org.uk", "apps@crsu.org.uk"), "."),
             type = "info",
             confirmButtonText = "Okay",
             html = TRUE)
  
    shinyalert(title = "Important message",
               text =  paste("In accordance with Data Protection legislation, we would like to inform you of the following before you
                        use our website: We collect your usage data within the MetaBayesDTA app to perform analytics of usage and
                        improve our app.  By clicking",   tags$i(tags$u("I consent")), "below, you consent to the use of data
                        by us through Google Analytics.  For details of policy, please check",
                        tags$a(href="https://policies.google.com/privacy?hl=en", "Google Privacy & Terms.",target="_blank") 
                        ),
               type = "info",
               confirmButtonText = "I consent",
               html = TRUE)
  
  
  Standard <- read.csv('./Standard.csv') 
  QA <- read.csv('./QA.csv') 
  Cov <- read.csv('./Cov.csv') 
  QA_Cov <- read.csv('./QA_Cov.csv')
 
defaultData <- dataset_default_import_server(id = "dataset_id", 
                                             QA = QA,
                                             Cov = Cov,
                                             QA_Cov = QA_Cov, 
                                             Standard = Standard)

  
# Load data 
data <- dataset_import_server(
  id = "dataset_id",
  defaultData
)

# Download User Guide
# Allow users the option to download the standard example dataset
output$downloadUG <- downloadHandler(
  
      # Specify the file name 
      filename = function() {
        paste("MetaDTA User Guide v1_0.pdf")
      },
      content = function(file) {
        file.copy("www/MetaDTA User Guide v1_0.pdf", file)
      }
  
)
  

#Download the instructions for reproducing analyses in the Cochrane Handbook
output$downloadCochraneGuide <- downloadHandler(
  filename = function(){
    paste("Cochrane-DTA-in-MetaBayesDTA v1_0.html")
  },
  content = function(file){
    utils::download.file("https://github.com/CRSU-Apps/MBDTA-Cochrane-guide/blob/main/Cochrane-DTA-in-MetaBayesDTA.html?raw=TRUE", file) 
  }
)


# Load data tab ---------------------------------------------------------------  ---------------------------------------------------------------------------------------------------------------
data_for_analysis_server(id = "dataset_id", 
                         data = data)

dataset_example_download_server(id = "dataset_id",
                                QA = QA,
                                Cov = Cov,
                                QA_Cov = QA_Cov, 
                                Standard = Standard)


#####  Standard bivariate model (BVM)  ----------------------------------------  --------------------------------------------------------------------------------------------------------------


# load sensitivity analysis (SA) study list and data 
SA_list_studies_server(id = "SA_MA_model_id", 
                       data = data)
SA_data <- SA_data_server(id = "SA_MA_model_id", 
                          data = data)
SA_data_table_server(id = "SA_MA_model_id", 
                     data = data)

SA_indicator <- SA_indicator_out(id = "SA_MA_model_id")
SA_display_conditional_plot_checkbox_renderUI(id = "MA_model_id", 
                                              SA_indicator = SA_indicator)

MA_p_scale_priors_indicator <- p_scale_priors_indicator_checkbox_out(id = "MA_model_id")

# BVM- Forest plots ------------------------------------------------------------  ----------------------------------
MA_forest_plots_server(id = "MA_model_id", 
                       data = data)

# BVM- Display input options for standard BVM tab ------------------------------  ----------------------------------

# BVM- server-side module for prior options UI (to be used w/ shinydashboard)
MA_priors_options_inputModule_renderUI_server(id = "MA_model_id")
priors <- MA_priors_options_inputModule_server(id = "MA_model_id")


# BVM- server-side module for Stan sampler options UI (to be used w/ shinydashboard)
sampler_options_server(id = "MA_model_id")
sampler_options <- sampler_options_inputModule_server(id = "MA_model_id")


# BVM- server functions for progress indicators / loading bars ----------------  ----------------------------------
progress_prior_model_server(id = "MA_model_id")
progress_main_model_server(id = "MA_model_id")
progress_main_model_server(id = "SA_MA_model_id")


#  BVM- Run Bivariate Model - prior-only model  -------------------------------  ----------------------------------
MA_model_PO <- StanModel$new(stan_model_rds_path = "./models/BVM_PO.rds",
                              stan_model_path = "./models/BVM_PO.stan")
MA_model_PO_p_scale_priors <- StanModel$new(stan_model_rds_path = "./models/p_scale_priors/BVM_PO_p_scale_priors.rds",
                              stan_model_path = "./models/p_scale_priors/BVM_PO_p_scale_priors.stan")

# BVM- call the server module for button
MA_model_PO_button <- run_prior_model_button_server(id = "MA_prior_model_button_id")

# BVM- call server module to run stan model and save posterior draws
draws_PO <- MA_run_model_priors_only(id = "MA_model_id",
                                     button = MA_model_PO_button,
                                     stan_model = MA_model_PO,
                                     stan_model_p_scale_priors = MA_model_PO_p_scale_priors,
                                     p_scale_priors_indicator = MA_p_scale_priors_indicator)$draws

# Run the Garabage Collector to Ensure any excess memory used by stan is freed
gc()

# BVM-  Run Bivariate Model - full model  -----------------------------  ----------------------------------
MA_model <- StanModel$new(stan_model_rds_path = "./models/BVM.rds",
                              stan_model_path = "./models/BVM.stan")
MA_model_p_scale_priors <- StanModel$new(stan_model_rds_path = "./models/p_scale_priors/BVM_p_scale_priors.rds",
                              stan_model_path = "./models/p_scale_priors/BVM_p_scale_priors.stan")

# BVM-  call the server module for buttons
MA_model_button    <- run_model_button_server(id = "MA_model_button_id")
SA_MA_model_button <- SA_run_model_button_server(id = "SA_MA_model_button_id")

## BVM-  call server module to run stan model and save posterior draws
draws <-  MA_run_model(id = "MA_model_id",
                       button = MA_model_button,
                       stan_model = MA_model,
                       stan_model_p_scale_priors = MA_model_p_scale_priors,
                       dataset = data,
                       priors = priors,
                       sampler_options = sampler_options, 
                       SA_indicator = SA_indicator,
                       SA_indicator_local = 0,
                       p_scale_priors_indicator = MA_p_scale_priors_indicator)$draws

SA_draws <-  MA_run_model(id = "SA_MA_model_id",
                          button = SA_MA_model_button,
                          stan_model = MA_model,
                          stan_model_p_scale_priors = MA_model_p_scale_priors,
                          dataset = data,
                          priors = priors,
                          sampler_options = sampler_options, 
                          SA_indicator = SA_indicator,
                          SA_indicator_local = 1,
                          p_scale_priors_indicator = MA_p_scale_priors_indicator)$draws


# BVM- Revman plots -----------------------------------------------------------  ------------------------------------
MA_revman_plots_server(id = "MA_model_id", 
                       data = data, 
                       draws = draws)

# Run the Garabage Collector to Ensure any excess memory used by stan is freed
gc()




# BVM- Table of model diagnostics ---------------------------------------------  ------------------------------------
MA_model_diagnostics_tab_renderUI_server(id = "MA_model_id", 
                                          SA_indicator = SA_indicator)

sampler_diagnostics_table_server(id = "MA_model_id",
                                 draws = draws,
                                 sampler_options = sampler_options)


#  SA
sampler_diagnostics_table_server(id = "SA_MA_model_id",
                                 draws = SA_draws,
                                 sampler_options = sampler_options)

#  BVM-  Table of r-hat statistics  --------------------------------------------  ------------------------------------
MA_rhat_table_server(id = "MA_model_id",
                     draws = draws)


#  SA
MA_rhat_table_server(id = "SA_MA_model_id",
                     draws = SA_draws)

# BVM-  Posterior density plots  ----------------------------------------------  -------------------------------------
MA_model_posterior_density_plots_server(id = "MA_model_id",
                                        data = data,
                                        draws = draws)

#  SA
MA_model_posterior_density_plots_server(id = "SA_MA_model_id",
                                        data = SA_data,
                                        draws = SA_draws)

# BVM-  Trace plots  ----------------------------------------------------------  -------------------
MA_model_trace_plots_server(id = "MA_model_id",
                            data = data,
                            draws = draws)

#  SA 
MA_model_trace_plots_server(id = "SA_MA_model_id",
                            data = SA_data,
                            draws = SA_draws)

# BVM- Plot of prior distributions  -------------------------------------------  --------------------------------------------------------
MA_model_priors_plot_server(id = "MA_model_id",
                            draws = draws_PO)

# BVM- Table of prior distributions  ------------------------------------------  ---------------------------------------------------------
MA_model_priors_table_server(id = "MA_model_id",
                             draws = draws_PO)

# BVM- Table which displays observed data and study weights from model + download ----------------------------------------------------------------
MA_data_table_server(id = "MA_model_id",
                     data = data,
                     draws = draws)

#  BVM- Parameter estimates table --------------------------------------------  -------------------------------------------------------

# renderUI module 
MA_parameter_estimates_tab_renderUI_server(id = "MA_model_id", 
                                           SA_indicator = SA_indicator)

MA_parameter_estimates_table_server(id = "MA_model_id",
                                    data = data,
                                    draws = draws)

# SA  
MA_parameter_estimates_table_server(id = "SA_MA_model_id",
                                    data = SA_data,
                                    draws = SA_draws)

# BVM-  sROC plot + download + tooltips ------------------------------------------  --------------------------------------------------------------------------------------
covariate_display_server(id = "MA_model_id",
                         data = data)

HSROC_curve_type_renderUI(id = "MA_model_id")

#SA_display_indicator_renderUI(id = "MA_model_id",  SA_indicator = SA_indicator)

sroc_plot_settings_menu_server(id =  "MA_model_id",
                               data = data)

MA_sroc_plot_server(id = "MA_model_id",
                    data = data,
                    SA_data = SA_data,
                    draws = draws,
                    SA_draws = SA_draws,
                    SA_indicator = SA_indicator)

# BVM - Scripts to overlay tooltips over the sROC plot, when study clicked on ---  -------------------------------

# for study info 
output$MA_sroc_study_info_css_head <- renderUI({
  tags$head(   (tags$style('
                                          #MA_model_id-my_tooltip {
                                           position: absolute;
                                           width: 500px;
                                           z-index: 10;
                                           padding: 0;
                                          }

                                          #MA_model_id-vals {
                                                         color: white;
                                                         background: blue;
                                                         font-size: 11px;
                                                        }
                                       '))      ) })

output$MA_sroc_study_info_css_script <- renderUI({
  tags$script('
                                         $(document).ready(function() {
                                           // put ID of the plot below
                                           $("#MA_model_id-plot").mousemove(function(e) {

                                             // put ID of uiOutput below
                                             $("#MA_model_id-my_tooltip").show();
                                             $("#MA_model_id-my_tooltip").css({
                                               top: (e.pageY + 50 ) + "px",
                                               left: (e.pageX - 1400) + "px"
                                             });
                                           });
                                         });
                                       ') })

## for piecharts 
output$MA_sroc_piechart_css_head <- renderUI({
  tags$head(  tags$style('
                                                             #MA_model_id-piechart {
                                                              position: absolute;
                                                              width: 500px;
                                                              z-index: 5;
                                                              padding: 0;
                                                             }
                                                          ') )  })


output$MA_sroc_piechart_css_script <- renderUI({
  tags$script('
                                                            $(document).ready(function() {
                                                              // put ID of the plot below
                                                              $("#MA_model_id-plot").mousemove(function(e) {

                                                                // put ID of uiOutput below
                                                                $("#MA_model_id-piechart").show();
                                                                $("#MA_model_id-piechart").css({
                                                                  top: (e.pageY + 200 ) + "px",
                                                                  left: (e.pageX - 2000) + "px"
                                                                });
                                                              });
                                                            });
                                                          ')   })




# BVM-  Prevalence tab -------------------------------------------------------------------------------------


Prev_input_server(id = "MA_model_id", 
                  data = data)

Prev_plot_server(id = "MA_model_id", 
                 data = data,
                 draws = draws)

# SA
Prev_input_server(id = "SA_MA_model_id", 
                  data = SA_data)

Prev_plot_server(id = "SA_MA_model_id", 
                 data = SA_data,
                 draws = SA_draws)





















# Meta-regression (MR) ---------------------------------------------------------------------------------------------------------------------------------------------------------------
MR_covariate_model_server(id = "MR_model_id",
                             data = data,
                             cts_cov_indicator = MR_cts_cov_indicator)

MR_cts_cov_indicator <- MR_cts_cov_indicator_out(id = "MR_model_id", 
                                                 data = data)

# observe({
# print(MR_cts_cov_indicator$cts_cov_indicator)
# })

#  MR -server-side module for prior options UI (to be used w/ shinydashboard)
MR_priors_options_server(id = "MR_model_id",
                         cts_cov_indicator = MR_cts_cov_indicator)

#  MR - server-side module for Stan sampler options UI (to be used w/ shinydashboard)
sampler_options_server(id = "MR_model_id")
sampler_options <- sampler_options_inputModule_server(id = "MR_model_id")

# MR - server functions for progress indicators / loading bars 
progress_prior_model_server(id = "MR_model_id")
progress_main_model_server(id = "MR_model_id")



### MR - Run models ----------------------------------------------
MR_p_scale_priors_indicator_checkbox_renderUI(id = "MR_model_id", 
                                              cts_cov_indicator = MR_cts_cov_indicator)

MR_p_scale_priors_indicator <- MR_p_scale_priors_indicator_checkbox_out(id = "MR_model_id", 
                                                                        cts_cov_indicator = MR_cts_cov_indicator)

#  MR - prior-only models ---
MR_model_cts_PO  <- StanModel$new(stan_model_rds_path = "./models/MR_cts_PO.rds",
                              stan_model_path = "./models/MR_cts_PO.stan")
MR_model_cat_PO<- StanModel$new(stan_model_rds_path = "./models/MR_cat_PO.rds",
                              stan_model_path = "./models/MR_cat_PO.stan")
MR_model_cat_PO_p_scale_priors<- StanModel$new(stan_model_rds_path = "./models/p_scale_priors/MR_cat_PO_p_scale_priors.rds",
                              stan_model_path = "./models/p_scale_priors/MR_cat_PO_p_scale_priors.stan")


#  MR - call the server module for button
MR_run_PO_prior_model_button_server <- run_prior_model_button_server(id = "MR_prior_model_button_id")


MR_draws_PO <-         MR_run_model_priors_only(id = "MR_model_id",
                                                data = data,
                                                stan_model_cts = MR_model_cts_PO,
                                                stan_model_cat = MR_model_cat_PO,
                                                stan_model_cat_PO_p_scale_priors = MR_model_cat_PO_p_scale_priors,
                                                cts_cov_indicator = MR_cts_cov_indicator,
                                                p_scale_priors_indicator = MR_p_scale_priors_indicator,
                                                button = MR_run_PO_prior_model_button_server)$draws

# Run the Garabage Collector to Ensure any excess memory used by stan is freed
gc()

#   MR - Full models ------
MR_model_cts  <- StanModel$new(stan_model_rds_path = "./models/MR_cts.rds",
                              stan_model_path = "./models/MR_cts.stan")
MR_model_cat  <- StanModel$new(stan_model_rds_path = "./models/MR_cat_v2.rds",
                              stan_model_path = "./models/MR_cat_v2.stan")
MR_model_cat_p_scale_priors  <- StanModel$new(stan_model_rds_path = "./models/p_scale_priors/MR_cat_p_scale_priors_v2.rds",
                              stan_model_path = "./models/p_scale_priors/MR_cat_p_scale_priors_v2.stan")                              


#  MR - call the server module for button
MR_run_model_button_server <- run_model_button_server(id = "MR_model_button_id")

MR_draws <-             MR_run_model(id = "MR_model_id",
                                     stan_model_cts = MR_model_cts,
                                     stan_model_cat = MR_model_cat,
                                     stan_model_cat_p_scale_priors = MR_model_cat_p_scale_priors,
                                     data = data,
                                     cts_cov_indicator = MR_cts_cov_indicator,
                                     p_scale_priors_indicator = MR_p_scale_priors_indicator,
                                     button = MR_run_model_button_server)$draws

# Run the Garabage Collector to Ensure any excess memory used by stan is freed
gc()

# MR -  Plot of prior distributions  ---------------------------------
MR_model_priors_plot_server(id = "MR_model_id",
                            draws_PO = MR_draws_PO,
                            cts_cov_indicator = MR_cts_cov_indicator)

#  MR -   table for prior distributions  -----------------------------
MR_model_priors_table_server(id = "MR_model_id",
                             draws_PO = MR_draws_PO,
                             cts_cov_indicator = MR_cts_cov_indicator)


#  MR -  Table of model diagnostics  -----------------------------
sampler_diagnostics_table_server(id = "MR_model_id",
                                 draws = MR_draws, 
                                 sampler_options = sampler_options)

#  MR - Table of r-hat statistics  ---------------------------------------------------------------------------------------------------
MR_rhat_table_server(id = "MR_model_id",
                     draws = MR_draws,
                     cts_cov_indicator = MR_cts_cov_indicator)


#  MR -  Posterior density plots  -----------------------------
MR_model_posterior_density_plots_server(id = "MR_model_id",
                                        draws = MR_draws,
                                        cts_cov_indicator = MR_cts_cov_indicator)

# MR -  Trace plots  -----------------------------
MR_model_trace_plots_server(id = "MR_model_id",
                            draws = MR_draws,
                            cts_cov_indicator = MR_cts_cov_indicator)

#  MR -  Observed data table  / study-level outcomes -----------------------
MR_data_table_server(id = "MR_model_id",
                     draws = MR_draws,
                     data = data,
                     cts_cov_indicator = MR_cts_cov_indicator)

# MR -  Table of parameter estimates   --------------------

MR_parameter_estimates_table_renderUI(id = "MR_model_id",
                                      cts_cov_indicator = MR_cts_cov_indicator) 

MR_parameter_estimates_table_server(id = "MR_model_id",
                                    cts_cov_indicator = MR_cts_cov_indicator,
                                    data = data,
                                    draws = MR_draws)

# MR -  sROC plot ---------------------------------------------
covariate_display_server(id = "MR_model_id",
                         data = data)

HSROC_curve_type_renderUI(id = "MR_model_id")

MR_sroc_plot_server(id = "MR_model_id",
                    draws = MR_draws,
                    cts_cov_indicator = MR_cts_cov_indicator,
                    data = data)

# MR -  sROC plot - Javascript code to overlay study info for Meta-regression sROC 

output$css_MR_sroc_head <-renderUI({(tags$style('
                                     #MR_model_id-my_tooltip {
                                      position: absolute;
                                      width: 500px;
                                      z-index: 10;
                                      padding: 0;
                                     }

                                     #MR_model_id-vals {
                                                    color: white;
                                                    background: blue;
                                                    font-size: 11px;
                                                   }
                                  '))})

output$css_MR_sroc_script <-renderUI({(tags$script('
                                      $(document).ready(function() {
                                        // put ID of the plot below
                                        $("#MR_model_id-plot").mousemove(function(e) {

                                          // put ID of uiOutput below
                                          $("#MR_model_id-my_tooltip").show();
                                          $("#MR_model_id-my_tooltip").css({
                                            top: (e.pageY + 50) + "px",
                                            left: (e.pageX - 1400) + "px"
                                          });
                                        });
                                      });
                                    '))})

# MR -  covariate level vs Se/Sp  Plots ----------------------------------------------------------

MR_plot_2_renderUI(id = "MR_model_id",
                   cts_cov_indicator = MR_cts_cov_indicator)

MR_plot_2_server(id = "MR_model_id",
                 draws = MR_draws,
                 cts_cov_indicator = MR_cts_cov_indicator,
                 data = data)

# MR - sROC plot - Javascript code to overlay study info for Meta-regression Se/Sp vs covariate plot
output$css_MR_plot_2_head <-renderUI({(tags$style('
                                       #MR_model_id-my_tooltip_2 {
                                        position: absolute;
                                        width: 500px;
                                        z-index: 10;
                                        padding: 0;
                                       }

                                       #MR_model_id-vals_2 {
                                                      color: white;
                                                      background: blue;
                                                      font-size: 11px;
                                                     }
                                    '))})

output$css_MR_plot_2_script <-renderUI({(tags$script('
                                        $(document).ready(function() {
                                          // put ID of the plot below
                                          $("#MR_model_id-plot_2").mousemove(function(e) {

                                            // put ID of uiOutput below
                                            $("#MR_model_id-my_tooltip_2").show();
                                            $("#MR_model_id-my_tooltip_2").css({
                                              top: (e.pageY + 50) + "px",
                                              left: (e.pageX - 1400) + "px"
                                            });
                                          });
                                        });
                                      '))})









































# Subgroup analysis (SG) ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
SG_covariate_model_server(id = "SG_model_id",
                             data = data)



SG_priors_options_server(id = "SG_model_id",
                         data = data)

# SG - server-side module for Stan sampler options UI (to be used w/ shinydashboard)
sampler_options_server(id = "SG_model_id")
sampler_options <- sampler_options_inputModule_server(id = "SG_model_id")


# SG - server functions for progress indicators / loading bars ----------------------------------------------------------------------
progress_prior_model_server(id = "SG_model_id")
progress_main_model_server(id = "SG_model_id")


### Run Subgroup analysis models ---------------------------------------------------------------------------------------------------------------------------------

# indicator for p-scale priors models 
SG_p_scale_priors_indicator <- p_scale_priors_indicator_checkbox_out(id = "SG_model_id")
  
# priors-only model-----
SG_model_PO  <- StanModel$new(stan_model_rds_path = "./models/SG_PO.rds",
                              stan_model_path = "./models/SG_PO.stan")                              

SG_model_PO_p_scale_priors  <- StanModel$new(stan_model_rds_path = "./models/p_scale_priors/SG_PO_p_scale_priors.rds",
                              stan_model_path = "./models/p_scale_priors/SG_PO_p_scale_priors.stan")                              

SG_run_prior_model_button_server <- run_prior_model_button_server(id = "SG_prior_model_button_id")

SG_draws_PO <- SG_run_model_priors_only(id = "SG_model_id",
                                                stan_model = SG_model_PO,
                                                stan_model_p_scale_priors = SG_model_PO_p_scale_priors,
                                                data = data,
                                                p_scale_priors_indicator = SG_p_scale_priors_indicator,
                                                button = SG_run_prior_model_button_server)$draws

# Run the Garabage Collector to Ensure any excess memory used by stan is freed
gc()

# full model --------
SG_model  <- StanModel$new(stan_model_rds_path = "./models/SG.rds",
                              stan_model_path = "./models/SG.stan")     
SG_model_p_scale_priors  <- StanModel$new(stan_model_rds_path = "./models/p_scale_priors/SG_p_scale_priors.rds",
                              stan_model_path = "./models/p_scale_priors/SG_p_scale_priors.stan")                                                          

SG_run_model_button_server <- run_model_button_server(id = "SG_model_button_id")

SG_draws <-             SG_run_model(id = "SG_model_id",
                                     stan_model = SG_model,
                                     stan_model_p_scale_priors = SG_model_p_scale_priors,
                                     data = data,
                                     p_scale_priors_indicator = SG_p_scale_priors_indicator,
                                     button = SG_run_model_button_server)$draws

# Run the Garabage Collector to Ensure any excess memory used by stan is freed
gc()

# Subgroup analysis - Plot of prior distributions  --------------------------------------------------------------------------------------------------------------------------------------------------------------------
SG_model_priors_plot_server(id = "SG_model_id",
                            draws_PO = SG_draws_PO,
                            data = data)

#  Subgroup analysis -  table for prior distributions  --------------------------------------------------------------------------------------------------------------------------------------------------------------------
SG_model_priors_table_server(id = "SG_model_id",
                             draws_PO = SG_draws_PO,
                             data = data)

#  Subgroup analysis - Table of model diagnostics  --------------------------------------------------------------------------------------------------------------------------------------------------------------------
sampler_diagnostics_table_server(id = "SG_model_id",
                                 draws = SG_draws, 
                                 sampler_options = sampler_options)

#  Subgroup analysis -  Table of r-hat statistics  ---------------------------------------------------------------------------------------------------
SG_rhat_table_server(id = "SG_model_id",
                     draws = SG_draws, 
                     data = data)

#  Subgroup analysis - Posterior density plots  --------------------------------------------------------------------------------------------------------------------------------------------------------------------
SG_model_posterior_density_plots_server(id = "SG_model_id",
                                        draws = SG_draws,
                                        data = data)

# Subgroup analysis - Trace plots  --------------------------------------------------------------------------------------------------------------------------------------------------------------------
SG_model_trace_plots_server(id = "SG_model_id",
                            draws = SG_draws,
                            data = data)

#  Subgroup analysis - Observed data table  / study-level outcomes --------------------------------------------------------------------------------------------------------------------------------------------------------------
SG_data_table_server(id = "SG_model_id",
                     draws = SG_draws,
                     data = data)

# Subgroup analysis - Table of parameter estimates   -----------------------------------------------------------------------------------------------------------------------------------------------------------
SG_parameter_estimates_table_server(id = "SG_model_id",
                                    data = data,
                                    draws = SG_draws)

# Subgroup analysis - sROC plot ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
SG_covariate_display_server(id = "SG_model_id",
                            data = data)

HSROC_curve_type_renderUI(id = "SG_model_id")

SG_sroc_plot_server(id = "SG_model_id",
                    draws = SG_draws,
                    data = data)


# sROC plot - Javascript code to overlay study info for Subgroup sROC plot 
output$css_SG_sroc_head <-renderUI({(tags$style('
                                               #SG_model_id-my_tooltip {
                                                position: absolute;
                                                width: 500px;
                                                z-index: 10;
                                                padding: 0;
                                               }
                                               
                                               #SG_model_id-vals { 
                                                              color: white;
                                                              background: blue;
                                                              font-size: 11px;
                                                             }
                                            '))})



output$css_SG_sroc_script <-renderUI({(tags$script('
                                                $(document).ready(function() {
                                                  // put ID of the plot below
                                                  $("#SG_model_id-plot").mousemove(function(e) {
                                            
                                                    // put ID of uiOutput below
                                                    $("#SG_model_id-my_tooltip").show();
                                                    $("#SG_model_id-my_tooltip").css({
                                                      top: (e.pageY + 50) + "px",
                                                      left: (e.pageX - 1400) + "px"
                                                    });
                                                  });
                                                });
                                                  '))})
                                                

# # Meta-regression: covariate level vs Se/Sp  Plots ----------------------------------------------------


SG_plot_2_server(id = "SG_model_id",
                 draws = SG_draws,
                 data = data)

  
# sROC plot - Javascript code to overlay study info for Subgroup Se/Sp vs covariate plot 
output$css_SG_plot_2_head <-renderUI({(tags$style('
                                                 #SG_model_id-my_tooltip_2 {
                                                  position: absolute;
                                                  width: 500px;
                                                  z-index: 10;
                                                  padding: 0;
                                                 }
                                                 
                                                 #SG_model_id-vals_2 { 
                                                                color: white;
                                                                background: blue;
                                                                font-size: 11px;
                                                               }
                                              '))})



output$css_SG_plot_2_script <-renderUI({(tags$script('
                                                  $(document).ready(function() {
                                                    // put ID of the plot below
                                                    $("#SG_model_id-plot_2").mousemove(function(e) {
                                              
                                                      // put ID of uiOutput below
                                                      $("#SG_model_id-my_tooltip_2").show();
                                                      $("#SG_model_id-my_tooltip_2").css({
                                                        top: (e.pageY + 50) + "px",
                                                        left: (e.pageX - 1400) + "px"
                                                      });
                                                    });
                                                  });
                                                '))})




























# Latent class model (LCM) --------------------------------------------------------------------------------------------------------------------------------------------------------------



# LCM - load sensitivity analysis (SA) study list and data 
SA_list_studies_server(id = "SA_LCM_model_id", 
                       data = data)

SA_data <- SA_data_server(id = "SA_LCM_model_id", 
                          data = data)

SA_data_table_server(id = "SA_LCM_model_id", 
                     data = data)

SA_LCM_indicator <- SA_indicator_out(id = "SA_LCM_model_id")
SA_display_conditional_plot_checkbox_renderUI(id = "LCM_model_id", 
                                              SA_indicator = SA_LCM_indicator)

# LCM - Forest plots -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
LCM_forest_plots_server(id = "LCM_model_id", 
                       data = data)


# LCM - server-side module for prior options UI (to be used w/ shinydashboard)
LCM_priors_options_inputModule_renderUI_server(id = "LCM_model_id", 
                                               data = data)

LCM_priors <- LCM_priors_options_inputModule_server(id = "LCM_model_id", 
                                                    data = data)

# LCM - server-side module for Stan sampler options UI (to be used w/ shinydashboard)
sampler_options_server(id = "LCM_model_id")
LCM_sampler_options <- sampler_options_inputModule_server(id = "LCM_model_id")  # extract sampler options

# LCM - server functions for progress indicators / loading bars ----------------------------------------------------------------------
progress_prior_model_server(id = "LCM_model_id")
progress_main_model_server(id = "LCM_model_id")
progress_main_model_server(id = "SA_LCM_model_id")


#  LCM - Run LCM Model - prior-only model    -----------------------------------------------------------------------------------------------------------------

LCM_p_scale_priors_indicator <- p_scale_priors_indicator_checkbox_out(id = "LCM_model_id")

#LCM_model_PO <- readRDS(file ='./models/BLCM_ma_PO.rds')
#LCM_model_PO <- stan_model(file ='./models/BLCM_ma_PO.stan')
#LCM_model_PO_p_scale_priors <- readRDS(file = './models/p_scale_priors/BLCM_ma_PO_p_scale_priors.rds')
#LCM_model_PO_p_scale_priors <- stan_model(file = './models/p_scale_priors/BLCM_ma_PO_p_scale_priors.stan')

LCM_model_PO <- StanModel$new(stan_model_rds_path = "./models/BLCM_ma_PO.rds",
                              stan_model_path = "./models/BLCM_ma_PO.stan")                                                          
LCM_model_PO_p_scale_priors <- StanModel$new(stan_model_rds_path = "./models/p_scale_priors/BLCM_ma_PO_p_scale_priors.rds",
                              stan_model_path = "./models/p_scale_priors/BLCM_ma_PO_p_scale_priors.stan")                                                          


# LCM -  call the server module for button
LCM_model_PO_button <- run_prior_model_button_server(id = "LCM_prior_model_button_id")


# LCM - call server module to run stan model and save posterior draws
LCM_draws_PO <- LCM_run_model_priors_only( id = "LCM_model_id",
                                                   data = data,
                                                   p_scale_priors_indicator = LCM_p_scale_priors_indicator,
                                                   priors = LCM_priors,
                                                   button = LCM_model_PO_button,
                                                   stan_model = LCM_model_PO,
                                                   stan_model_p_scale_priors = LCM_model_PO_p_scale_priors)$draws

# Run the Garabage Collector to Ensure any excess memory used by stan is freed
gc()

# LCM - LCM options 
LCM_model_options_inputModule_renderUI_server(id = "LCM_model_id")
LCM_options_indicators <- LCM_model_options_inputModule_server(id = "LCM_model_id") # extract options





#  LCM -  Run LCM Model - full model    -----------------------------------------------------------------------------------------------------------------
#LCM_model <- readRDS(file = './models/BLCM_ma.rds')
#LCM_model <- stan_model(file = './models/BLCM_ma.stan')
#LCM_model_p_scale_priors <- readRDS(file = './models/p_scale_priors/BLCM_ma_p_scale_priors.rds')
#LCM_model_p_scale_priors <- stan_model(file = './models/p_scale_priors/BLCM_ma_p_scale_priors.stan')

LCM_model <- StanModel$new(stan_model_rds_path = "./models/BLCM_ma.rds",
                              stan_model_path = "./models/BLCM_ma.stan")                                                          
LCM_model_p_scale_priors <- StanModel$new(stan_model_rds_path = "./models/p_scale_priors/BLCM_ma_p_scale_priors.rds",
                              stan_model_path = "./models/p_scale_priors/BLCM_ma_p_scale_priors.stan")                                                          


# LCM - call the server module for button
LCM_model_button <- run_model_button_server(id = "LCM_model_button_id")
SA_LCM_model_button <- SA_run_model_button_server(id = "SA_LCM_model_button_id")



## LCM - call server module to run stan model and save posterior draws
LCM_draws <- LCM_run_model( id = "LCM_model_id",
                            button = LCM_model_button,
                            stan_model = LCM_model,
                            stan_model_p_scale_priors = LCM_model_p_scale_priors,
                            dataset = data,
                            p_scale_priors_indicator = LCM_p_scale_priors_indicator,
                            priors = LCM_priors,
                            sampler_options = LCM_sampler_options, 
                            SA_indicator = SA_LCM_indicator,
                            SA_indicator_local = 0, 
                            LCM_options_indicators = LCM_options_indicators)$draws


LCM_SA_draws <- LCM_run_model( id = "SA_LCM_model_id",
                               button = SA_LCM_model_button,
                               stan_model = LCM_model,
                               stan_model_p_scale_priors = LCM_model_p_scale_priors,
                               dataset = data,
                               p_scale_priors_indicator = LCM_p_scale_priors_indicator,
                               priors = LCM_priors,
                               sampler_options = LCM_sampler_options, 
                               SA_indicator = SA_LCM_indicator,
                               SA_indicator_local = 1, 
                               LCM_options_indicators = LCM_options_indicators)$draws

# Run the Garabage Collector to Ensure any excess memory used by stan is freed
gc()

#  LCM - Table of model diagnostics  ---------------------------------------------------------------------------------------------------
LCM_model_diagnostics_tab_renderUI_server(id = "LCM_model_id", 
                                          SA_indicator = SA_LCM_indicator)

sampler_diagnostics_table_server(id = "LCM_model_id",
                                 draws = LCM_draws, 
                                 sampler_options = LCM_sampler_options)


#  SA 
sampler_diagnostics_table_server(id = "SA_LCM_model_id",
                                 draws = LCM_SA_draws, 
                                 sampler_options = LCM_sampler_options)

# LCM -  Table of r-hat statistics  ---------------------------------------------------------------------------------------------------
LCM_rhat_table_server(id = "LCM_model_id",
                      draws = LCM_draws, 
                      data = data)

#  SA
LCM_rhat_table_server(id = "SA_LCM_model_id",
                      draws = LCM_SA_draws, 
                      data = SA_data)

# LCM -  Table of deviance statistics  ---------------------------------------------------------------------------------------------------
LCM_deviance_table_server(id = "LCM_model_id",
                      draws = LCM_draws, 
                      data = data)

#  SA
LCM_deviance_table_server(id = "SA_LCM_model_id",
                      draws = LCM_SA_draws, 
                      data = SA_data)

# LCM - Posterior density plots  ---------------------------------------------------------------------------------------------------
LCM_model_posterior_density_plots_server(id = "LCM_model_id",
                                         data = data,
                                         draws = LCM_draws)

#  SA
LCM_model_posterior_density_plots_server(id = "SA_LCM_model_id",
                                         data = SA_data,
                                         draws = LCM_SA_draws)

# LCM - Trace plots  ------------------------------------------------------------------------------------- -----------------------------
LCM_model_trace_plots_server(id = "LCM_model_id",
                             data = data,
                             draws = LCM_draws)

#  SA 
LCM_model_trace_plots_server(id = "SA_LCM_model_id",
                             data = SA_data,
                             draws = LCM_SA_draws)


# LCM - Correlation residual plot  ------------------------------------------------------------------------------------------------------------------------------------------

LCM_correlation_residual_plot_server(id = "LCM_model_id", 
                                     draws = LCM_draws)

# SA
LCM_correlation_residual_plot_server(id = "SA_LCM_model_id", 
                                     draws = LCM_SA_draws)


# LCM - Table count residual plot  ------------------------------------------------------------------------------------------------------------------------------------------

LCM_table_prob_resid_plot_server(id = "LCM_model_id", 
                                     draws = LCM_draws)

# SA
LCM_table_prob_resid_plot_server(id = "SA_LCM_model_id", 
                                     draws = LCM_SA_draws)


# LCM - Plot of prior distributions  ---------------------------------------------------------------------------------------------------
LCM_model_priors_plot_server(id = "LCM_model_id",
                             draws = LCM_draws_PO, 
                             data = data)

# LCM - Table of prior distributions  ---------------------------------------------------------------------------------------------------
LCM_model_priors_table_server(id = "LCM_model_id",
                              draws = LCM_draws_PO, 
                              data = data,
                              LCM_options_indicators = LCM_options_indicators)

# LCM - table which displays observed data and study weights from model + download ----------------------------------------------------------------
LCM_data_table_server(id = "LCM_model_id",
                     data = data)



#  LCM - Parameter estimates table ---------------------------------------------------------------
LCM_parameter_estimates_tab_renderUI_server(id = "LCM_model_id", 
                                            SA_indicator = SA_LCM_indicator)

LCM_parameter_estimates_table_server(id = "LCM_model_id",
                                    data = data,
                                    draws = LCM_draws,
                                    LCM_options_indicators = LCM_options_indicators)

# SA
LCM_parameter_estimates_table_server(id = "SA_LCM_model_id",
                                    data = SA_data,
                                    draws = LCM_SA_draws,
                                    LCM_options_indicators = LCM_options_indicators)






# LCM - sROC plot + download + tooltips --------------------------------------------------------------------------------------------------------
LCM_sroc_plot_renderUI(id = "LCM_model_id", 
                       data = data,
                       draws = LCM_draws,
                       SA_indicator = SA_LCM_indicator)


LCM_HSROC_curve_type_renderUI(id = "LCM_model_id")

covariate_display_server(id = "LCM_model_id",
                         data = data)

LCM_sroc_plot_settings_menu_server(id =  "LCM_model_id",
                                   data = data)


LCM_sroc_plot_server(id = "LCM_model_id",
                     data = data,
                     SA_data = SA_data,
                     draws = LCM_draws,
                     SA_draws = LCM_SA_draws,
                     SA_indicator = SA_LCM_indicator,
                     LCM_options_indicators = LCM_options_indicators)



# LCM MA - css scripts to overlay tooltips over the sROC plot, when study clicked on --------------------------------------------------------
# for sROC plot
output$LCM_sroc_study_info_css_head <- renderUI({
  tags$head(   (tags$style('
                                          #LCM_model_id-my_tooltip {
                                           position: absolute;
                                           width: 500px;
                                           z-index: 10;
                                           padding: 0;
                                          }

                                          #LCM_model_id-vals {
                                                         color: white;
                                                         background: blue;
                                                         font-size: 11px;
                                                        }
                                       '))      ) })

output$LCM_sroc_study_info_css_script <- renderUI({
  tags$script('
                                         $(document).ready(function() {
                                           // put ID of the plot below
                                           $("#LCM_model_id-plot").mousemove(function(e) {

                                             // put ID of uiOutput below
                                             $("#LCM_model_id-my_tooltip").show();
                                             $("#LCM_model_id-my_tooltip").css({
                                               top: (e.pageY + 50 ) + "px",
                                               left: (e.pageX - 1400) + "px"
                                             });
                                           });
                                         });
                                       ') })

# for piecharts 
output$LCM_sroc_piechart_css_head <- renderUI({
  tags$head(  tags$style('
                                                             #LCM_model_id-piechart {
                                                              position: absolute;
                                                              width: 500px;
                                                              z-index: 5;
                                                              padding: 0;
                                                             }
                                                          ') )  })


output$LCM_sroc_piechart_css_script <- renderUI({
  tags$script('
                                                            $(document).ready(function() {
                                                              // put ID of the plot below
                                                              $("#LCM_model_id-plot").mousemove(function(e) {

                                                                // put ID of uiOutput below
                                                                $("#LCM_model_id-piechart").show();
                                                                $("#LCM_model_id-piechart").css({
                                                                  top: (e.pageY + 200 ) + "px",
                                                                  left: (e.pageX - 2000) + "px"
                                                                });
                                                              });
                                                            });
                                                          ')   })



# LCM - Prevalence tab -------------------------------------------------------------------------------------
Prev_input_server(id = "LCM_model_id", 
                  data = data)

Prev_plot_server(id = "LCM_model_id", 
                 data = data,
                 draws = LCM_draws)

# SA
Prev_input_server(id = "LCM_SA_model_id", 
                  data = SA_data)

Prev_plot_server(id = "LCM_SA_model_id", 
                 data = SA_data,
                 draws = LCM_SA_draws)


}


  
  










