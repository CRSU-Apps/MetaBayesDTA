# UI --------------------------------------------------------------------------  --------------------------------------------------------------------------------------------------------------------------
ui <- dashboardPage(title="MetaBayesDTA", skin = "black", 
dashboardHeader(title="MetaBayesDTA"),
dashboardSidebar(
tags$head(
  tags$style(
    HTML('.content-wrapper { height: 3000px !important;}')),
  includeHTML("www/favicon/favicon.html"),
  # SEO meta tags (partially redundant)
  tags$meta(name="description", content="Codeless Bayesian meta-analysis of test accuracy, with or without a gold standard"),
  tags$meta(name="keywords", content="Meta-Analysis, Diagnostic test accuracy, Application, Imperfect gold standard, Latent class, Bayesian"),
  # Open Graph Tags
  tags$meta(property="og:title", content="MetaBayesDTA v1.5.0"),
  tags$meta(property="og:description", content="Codeless Bayesian meta-analysis of test accuracy, with or without a gold standard"),
  tags$meta(property="og:image", content="https://raw.githubusercontent.com/CRSU-Apps/MetaBayesDTA/main/www/roc_curve.png")
  ), 
width = 350,
sidebarMenu(
id = "sidebarID",
menuItem("Home", tabName = "home_tab", icon = icon("table")),
menuItem("User Guide", tabName = "tutorial_tab", icon = icon("table")),
menuItem("Data", id = "dataID", tabName = "data", icon = icon("table"), 
         menuSubItem("File Upload", tabName = "load_data_subtab_fileupload"), 
         menuSubItem("Example Datasets", tabName = "load_data_subtab_example_datasets"), 
         menuSubItem("Data for Analysis", tabName = "load_data_subtab_data_for_analysis")),
menuItem("Perfect gold standard", tabName = "meta_analysis_gs", icon = icon("table"), 
         menuSubItem("Meta-analysis", tabName = "meta_analysis_bivariate"), 
         menuSubItem("Meta-regression", tabName = "meta_regression_bivariate"), 
         menuSubItem("Subgroup analysis", tabName = "SG_bivariate")), 
menuItem("Imperfect gold standard", tabName = "meta_analysis_igs", icon = icon("table"), 
         menuSubItem("Latent class meta-analysis", tabName = "meta_analysis_latent_class")), 
menuItem("References", tabName = "refs", icon = icon("table"))
)
),


dashboardBody(
  
useShinyjs(),
useSweetAlert(), 
useShinyalert(),

tabItems(

#  Tab 1: home page -----------------------------------------------------------  --------------------------------------------------------------------------------------------------------
tabItem(
  tabName = "home_tab",
    # navbarPage("", id = "home_tab_navbar",
         box(width = 12,
             tabPanel("Home",
                      h1("MetaBayesDTA (v1.5.0)"),
                      h2("FULL RELEASE - MetaBayesDTA has now left BETA", style="color:blue"),
                      h2("Bayesian meta-analysis of diagnostic test accuracy data, with or without a gold standard"),
                      br(),
                      h4("This is an extension of the frequentist version of the app, MetaDTA, which is described in this paper:",
                         tags$a(href="https://onlinelibrary.wiley.com/doi/full/10.1002/jrsm.1439", "Patel A, Cooper NJ, Freeman SC, Sutton AJ. 
                         Graphical enhancements to summary receiver operating charcateristic plots to facilitate the analysis and reporting
                         of meta-analysis of diagnostic test accuracy data. Research Synthesis Methods 2020, https://doi.org/10.1002/jrsm.1439.
                           "), 
                         "which can be accessed at", tags$a(href="https://crsu.shinyapps.io/dta_ma/", "MetaDTA version 2.0")),
                      br(),
                      h4("Which builds on the previous version as described in the paper:",
                         tags$a(href="https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-019-0724-x", "Freeman SC, Kerby CR, 
                         Patel A, Cooper NJ, Quinn T, Sutton AJ. Development of an interactive web-based tool to conduct and interrogate 
                         meta-analysis of diagnostic test accuracy studies: MetaDTA. BMC Medical Research Methodology 2019; 19: 81
                           "),
                         "which can be accessed at", tags$a(href="https://crsu.shinyapps.io/dta_ma_v1/", "MetaDTA version 1.27.")), 
                      h4("If you use outputs or screenshots generated from MetaBayesDTA, please cite these papers, as well as the following paper for 
                         MetaBayesDTA itself: ",
                         tags$a(href="https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-023-01910-y", "Cerullo E, Sutton AJ, 
                         Jones HE, Wu O, Quinn T, Cooper NJ. MetaBayesDTA: codeless Bayesian meta-analysis of test accuracy, with or without a gold standard
                           ")),
                      img(height=400,
                          width=600, 
                          src="roc_curve.png"),
                      br(),
                      h4("Enzo Cerullo, Suzanne Freeman, Clareece Nevill, Amit Patel, Terry Quinn, Alex Sutton, Nicola Cooper, Olivia Wu, Tom Morris, Ryan Field, Janion Nevill"),
                      p("For feedback/questions about this app please email the CRSU Team at ", tags$a(href="mailto:apps@crsu.org.uk", "apps@crsu.org.uk"), "."),
                      p("App powered by Rshiny with statistical analyses performed using Stan"),
                      p("(", tags$a(href="https://mc-stan.org/", "https://mc-stan.org/", target="_blank"), ")"),
                      br(),
                      p("An interactive primer on diagnostic test accuracy can be found at:"),
                      tags$a(href="https://crsu.shinyapps.io/diagprimer/", "https://crsu.shinyapps.io/diagprimer/", target="_blank"),
                      wellPanel(
                        fluidRow( 
                          column(3, img(src='CRSUlogo.jpg', width=220, height=110)),
                          column(9, tags$div(class="header", checked=NA,
                                             tags$p("For more information about the Complex Reviews Support Unit (CRSU)"),
                                             tags$a(href="https://www.gla.ac.uk/research/az/evidencesynthesis/apps-materials-guidence/", "please click here.", target="_blank")
                          )
                          )
                        )
                      ),
                      wellPanel(
                        img(src='CRSULogo.png', width = "100%"),
                        tags$strong("Funding and Support Acknowledgement:"),
                        tags$p("The Complex Reviews Support Unit is funded by the National Institute for Health Research (NIHR) (project number 14/178/29).
       Development of this app is also funded by the NIHR Applied Research Collaboration East Midlands (ARC EM) and the Leicester NIHR Biomedical Research Centre (BRC).
       The views expressed are those of the author(s) and not necessarily those of the NIHR or the Department of Health and Social Care."),
                      ),
                      br(),
                      p("THE SOFTWARE IS PROVIDED AS IS, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING 
                      BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
                      NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, 
                      DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
                      OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.")
             )
  )
), 

#  Tab 2: Guide / tutorial page -----------------------------------------------------------  --------------------------------------------------------------------------------------------------------
tabItem(
  tabName = "tutorial_tab",
  #navbarPage("", id = "tutorial_tab_navbar",
  box(width = 12,
      tabPanel("User Guide", 
               h1("User Guide"),
               br(),
               h4("Please click on the link before for a YouTube video (with subtitles) which gives a tutorial of MetaBayesDTA:",
                  tags$a(href="https://www.youtube.com/watch?v=UqouZ7EQc1w&t=36s&ab_channel=ESMARConf", "https://www.youtube.com/watch?v=UqouZ7EQc1w&t=36s&ab_channel=ESMARConf", target="_blank")),
               br(),
               h4("We also recommend reading the associated paper for this application here:",
                  tags$a(href="https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-023-01910-y", "https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-023-01910-y", target="_blank")),
               br(),
               h4("The Cochrane Handbook for Systematic Reviews of Diagnostic Test Accuracy Version 2 (",
                  tags$a(href="https://training.cochrane.org/handbook-diagnostic-test-accuracy/current", "https://training.cochrane.org/handbook-diagnostic-test-accuracy/current", target="_blank"), ") is an excellent resource for understanding the models utilised in this app. A guide to reproducing some of the examples in the handbook can be found here:",
                  downloadButton('downloadCochraneGuide', "Download Cochrane Guide")),
               br(),
               h4("In addition to the above, more in-depth tutorials with examples may be coming soon, and will be posted here")
      )
  )
), 
# Tab 3: load data tab -----------------------------------------------  ------------------------------------------------------------------------------------------------------------------
tabItem(
  tabName = "load_data_subtab_fileupload", 
  tabPanel("File Upload", 
                   box(width = 3,
                       dataset_import_ui(id = "dataset_id")
                   ),
                   box(width = 9,
                       h3("Please select a file to upload"),
                       br(),
                       p("The file should contain at least six columns. Labelling of columns is case sensitive."),
                       p("The", tags$strong("first"), "column should be labelled", tags$strong("author"), "and contain the name of 
                                             the study author. The author name must be unique for each study."),
                       p("The", tags$strong("second"), "column should be labelled", tags$strong("year"), "and contain the year of 
                                             publication."),
                       p("The", tags$strong("third"), "column should be labelled",  tags$strong("TP"), "and contain the number of 
                                             patients with a true positive test result."),
                       p("The", tags$strong("fourth"), "column should be labelled", tags$strong("FN"), "and contain the number of 
                                             patients with a false negative test result."),
                       p("The", tags$strong("fifth"), "column should be labelled",  tags$strong("FP"), "and contain the number of 
                                             patients with a false positive test result."),
                       p("The", tags$strong("sixth"), "column should be labelled",  tags$strong("TN"), "and contain the number of 
                                             patients with a true negative test result."),
                       br(),
                       
                       h4("Including quality assessment data (optional)"),
                       p("To allow the quality assessment results from the QUADAS-2 tool to be incorporated into the plots an
                         additional seven columns are required."),
                       p("The", tags$strong("seventh"), "column should be labelled", tags$strong("rob_PS"), ", representing the 
                                             risk of bias in terms of the patient selection."),
                       p("The", tags$strong("eighth"), "column should be labelled", tags$strong("rob_IT"), ", representing the 
                                             risk of bias in terms of the index test."),
                       p("The", tags$strong("ninth"), "column should be labelled", tags$strong("rob_RS"), ", representing the 
                                             risk of bias in terms of the reference standard."),
                       p("The", tags$strong("tenth"), "column should be labelled", tags$strong("rob_FT"), ", representing the 
                                             risk of bias in terms of the flow and timing."),
                       p("The", tags$strong("eleventh"), "column should be labelled", tags$strong("ac_PS"), ", representing the 
                                             applicability concerns in terms of the patient selection."),
                       p("The", tags$strong("twelfth"), "column should be labelled", tags$strong("ac_IT"), ", representing the 
                                             applicability concerns in terms of the index test."),
                       p("The", tags$strong("thirteenth"), "column should be labelled", tags$strong("ac_RS"), ", representing the 
                                             applicability concerns in terms of the reference standard."),
                       p("These columns should contain the numbers", tags$strong("1, 2 or 3"), "which represent", tags$strong("low, high or unclear"),
                         "risk of bias/applicability concerncs respectively."),
                       br(),
                       p("For information about the QUADAS-2 tool and how to use it please visit:"),
                       tags$a(href="https://www.bristol.ac.uk/population-health-sciences/projects/quadas/quadas-2/", 
                              "https://www.bristol.ac.uk/population-health-sciences/projects/quadas/quadas-2/", target="_blank"),
                       br(),
                       br(),
                       
                       h4("Including covariates (optional)"), 
                       p("If any covariates are to be added to the file, they should be included as the last columns in the file. If quality 
                                             assessment data is not included in the file the covariates should be entered starting at the", 
                                             tags$strong("seventh"), "column. If quality assessment
                                             data is included in the file the covariate data should be entered starting at the", 
                         tags$strong("fourteenth"), "column. Multiple covariates can be entered."),
                       
                       
                       br(),
                       p("Note: Excel files should be saved in csv format and the separator option 'comma' selected for upload."),
                       p("The default dataset, pre-loaded on the 'Data for Analysis' tab will be used for analysis if no file is 
                                              selected. The 'Data for Analysis' tab will automatically update once a file is successfully loaded."),
                       p("The default datasets can be downloaded using the buttons in the sidebar and used as templates to enter your own data."),
                       p("Please add", tags$strong("'.cat'"), "to the end of the column names of categorical or discrete covariates."), 
                       p("Please add", tags$strong("'.cts'"), "to the end of the column names of any continuous covariates."), 
                       p("If reference test information is available (which is", tags$strong("mandatory"),"for meta-analysis without a gold standard), 
                                      please add this as a categorical covariate named", tags$strong("'reference.cat'")), 
                       br(),
                       h4("Sensitivity analysis"),
                       p("To ensure the correct studies are excluded from sensitivity analyses please ensure that study data rows are ordered 
                                    by the 'author' column alphabetically from A to Z prior to uploading to MetaBayesDTA (Excel can do this easily).")))
  ),
tabItem(
  tabName = "load_data_subtab_example_datasets", 
  tabPanel("Example datasets",
                   box(width =12,
                       br(),
                       p("The default dataset uses data from a systematic review investigating the accuracy of an informant-based questionnaire, for detection of all cause
                                             dementia in adults. The dataset consists of thirteen studies assessing the use of the IQCODE (Informant Questionnaire
                                             on Cognitive Decline in the Elderly) tool for identifying adults with dementia within a secondary care setting."),
                       p("The IQCODE tool contains a number of questions which are scored on a five point scale. The IQCODE tool has a number of 
                                             different variants, depending on how many questions are asked. The questions are based on the performance of everyday
                                             tasks related to cognitive function. These are then rated on a scale of 1-5. The final score is an average score for each
                                             question. The IQCODE tool is only a screening tool and does not offer a definitive diagnosis of dementia."),
                       p("Under the 'Select example dataset' option there are four different datasets to choose from. The default is the 'Standard' dataset,
                                             which includes the author and year of each study along with the true positives (TP), false positives (FP), false negatives (FN) and 
                                             true negatives (TN). The other options add data onto this 'Standard' dataset and highlight how datasets with quality assessment scores and/or
                                             covariates should be displayed."),
                       p("With this dataset there are three different covariates. The first being the country in which each individual study was conducted.
                                             The second is the threshold used in each individual study. In this case if an individuals final score was higher than the threshold the individual
                                             was classified as having dementia and would require further diagnosis. The final covariate is labelled as 'IQCODE' and indicates 
                                             which variant of the tool was used in each individual study. The variants are identified by the number of questions used in the 
                                             questionnaire. There are three different variants the 16-item, 26-item and 32-item.")
                   ))
  ),
tabItem(
  tabName = "load_data_subtab_data_for_analysis", 
  tabPanel("Data for Analysis",
                                box(width = 12,
                                    br(),
                                    data_for_analysis_UI(id = "dataset_id"),
                                    br()
                                )
         )
),



# Tab 4:  Bivariate MA  model w/ no covariates (BVM) ----------------------------------  ---------------------------------------------------------------------------------
tabItem(
 tabName = "meta_analysis_bivariate",
  tabBox(
    tabPanel("Priors",
             
                                     dropdown(
                                       sampler_options_UI(id = "MA_model_id"),
                                       circle = TRUE, status = "danger",
                                       icon = icon("gear"), width = "300px",
                                       animate = animateOptions(
                                         enter = animations$fading_entrances$fadeInLeftBig,
                                         exit = animations$fading_exits$fadeOutRightBig
                                       ),
                                       tooltip = tooltipOptions(title = "Advanced option - click to change Stan sampler options")
                                     ),
             
                                     h4("Priors"),
                                     p_scale_priors_indicator_checkbox_UI(id = "MA_model_id"),
                                     dropdown(
                                       MA_priors_options_inputModule_UI(id = "MA_model_id"),
                                       circle = TRUE, status = "danger",
                                       icon = icon("gear"), width = "500px",
                                       animate = animateOptions(
                                         enter = animations$fading_entrances$fadeInLeftBig,
                                         exit = animations$fading_exits$fadeOutRightBig
                                       ),
                                       tooltip = tooltipOptions(title = "Click to change prior distributions")
                                     ),
                                     br(),
                                     # run prior model 
                                     run_prior_model_button_UI(id = "MA_prior_model_button_id"),
                                     br(), 
                                     tags$head(tags$style("#MA_model_id-progress_prior_model{overflow-y:scroll; max-height: 400px;}")),
                                     progress_prior_model_UI(id = "MA_model_id"), # progress indicator for prior model 
                                     br(),
                                     h3("Table of prior distributions"),
                                     MA_model_priors_table_UI(id = "MA_model_id"),
                                     br(),
                                     h3("Plot of prior distributions"),
                                     model_priors_plot_UI(id = "MA_model_id"),
                                     br(),
                                     reset_button_UI(id = "MA_model_id")
    ),
    tabPanel("Run model",

                                   br(),
              # run main model
                                   run_model_button_UI(id = "MA_model_button_id"),
                                   br(),
                                   br(),
                                   tags$head(tags$style("#MA_model_id-progress_main_model{overflow-y:scroll; max-height: 400px;}")),
                                   br(),
                                   progress_main_model_UI(id = "MA_model_id"), # progress indicator for main model 
                                   br(), 
                                   reset_button_UI(id = "MA_model_id"),
                                   br(),
              # run SA model
                                   SA_indicator_UI(id = "SA_MA_model_id"),
                                   SA_run_model_button_UI(id = "SA_MA_model_button_id"),
                                   br(), 
                                   tags$head(tags$style("#SA_MA_model_id-progress_main_model{overflow-y:scroll; max-height: 400px;}")),
                                   br(),
                                   progress_main_model_UI(id = "SA_MA_model_id"), # progress indicator for main model  - SA
                                   br()
    ),
    tabPanel("Study-level Outcomes",
                                   br(),
                                   p("Note: If the presence of zero's for two of TP, FN, FP and TN causes sensitivity or specificity to be calculated
                                                           as 0/0 than an error message will appear."),
                                   br(),
                                   dropdownButton(
                                     MA_data_table_settings_UI(id = "MA_model_id"),
                                     circle = TRUE, status = "danger",
                                     icon = icon("gear"), width = "300px",
                                     tooltip = tooltipOptions(title = "Table menu")
                                   ),
                                   MA_data_table_UI(id = "MA_model_id"),
             
                                   br(),
                                   br(),
                                   p("Note: Arrows to the right of the column headings can be used to sort data into ascending or descending order. "),
                                   p("N is  the total number of individuals in each study ( N = TP + FN + FP + TN )"),
                                   p("Sens is the sensitivity, which is the probability of a positive test result given that 
                                                           the patient has the disease ( Sens = TP / [TP + FN] )"),
                                   p("Spec is the specificity, which is the probability of a negative test result given that
                                                           the patient does not have the disease ( Spec = TN / [TN + FP] )"), 
                                   p("Weight_Sens is the precentage study weight of sensitvitiy, calculated using methods by Burke et al."),
                                   p("Weight_Spec is the percentage study weight of sensitivity, calculated using methods by Burke et al."),
                                   br()
    ),
    tabPanel("Parameter Estimates", 
                                   MA_parameter_estimates_tab_UI(id = "MA_model_id")
                                    ), 
    tabPanel("Parameters for RevMan", 
                                   h5("Below are the parameter values required by Cochrane's RevMan software to 
                                                            construct plots in the ROC space for users who wish to include the analysis results 
                                                            as part of a Cochrane review."),
                                   MA_revman_plots_UI(id = "MA_model_id"),
                                   br(), 
                                   br()
                          ), 
    tabPanel("Model diagnostics",
                                 MA_model_diagnostics_tab_UI(id = "MA_model_id")
    )
  ),
  tabBox(id = "Bivariate_sROC_box_2",
    tabPanel("sROC Plot",
                                 h3("sROC plot"),
                                 dropdownButton(
                                   MA_sroc_plot_settings_menu_UI(id = "MA_model_id"),
                                   circle = TRUE, status = "danger",
                                   icon = icon("gear"), width = "300px",
                                   tooltip = tooltipOptions(title = "Click to customise plot")
                                 ),
                             # sroc plot 
                                 MA_sroc_plot_UI(id = "MA_model_id"),
                                 tags$head(uiOutput("MA_sroc_study_info_css_head")),
                                 uiOutput("MA_sroc_study_info_css_script"),
    ), 
    tabPanel("Forest Plots", 
                                  MA_forest_plots_UI(id = "MA_model_id")
    ),
    tabPanel("Prevalence", 
                                  prevalence_tab_UI(id = "MA_model_id")
  )
)
), 





# Tab 5: Bivariate MA - Meta-regression (MR) ---------------------------------------  ----------------------------------------------------------------------------------------------------------
tabItem(
  tabName = "meta_regression_bivariate",
  tabBox(
    tabPanel("Model set up & priors",
                                   MR_select_covariate_UI(id = "MR_model_id"),
                                   MR_pick_cts_output_value_UI(id = "MR_model_id"),
                                   # prior model
                                   MR_p_scale_priors_indicator_checkbox_UI(id = "MR_model_id"),
             
                                   dropdown(
                                     sampler_options_UI(id = "MR_model_id"),
                                     circle = TRUE, status = "danger",
                                     icon = icon("gear"), width = "300px",
                                     animate = animateOptions(
                                       enter = animations$fading_entrances$fadeInLeftBig,
                                       exit = animations$fading_exits$fadeOutRightBig
                                     ),
                                     tooltip = tooltipOptions(title = "Advanced option - click to change Stan sampler options")
                                   ),
             
                                   h4("Priors:"),
                                   dropdown(
                                     MR_priors_options_UI(id = "MR_model_id"),
                                     circle = TRUE, status = "danger",
                                     icon = icon("gear"), width = "300px",
                                     animate = animateOptions(
                                       enter = animations$fading_entrances$fadeInLeftBig,
                                       exit = animations$fading_exits$fadeOutRightBig
                                     ),
                                     tooltip = tooltipOptions(title = "Click to change prior distributions")
                                   ),
                                   br(),
                                   run_prior_model_button_UI(id = "MR_prior_model_button_id"),
                                   br(),
                                   # progress indicator for prior model
                                   tags$head(tags$style("#MR_model_id-progress_prior_model{overflow-y:scroll; max-height: 400px;}")),
                                   progress_prior_model_UI(id = "MR_model_id"), 
                                   br(),
                                   h3("Table of prior distributions"),
                                   MR_model_priors_table_UI(id = "MR_model_id"),
                                   h3("Plot of prior distributions"),
                                   model_priors_plot_UI(id = "MR_model_id"),
                                   reset_button_UI(id = "MR_model_id")
    ),
    tabPanel("Run model",

                                   run_model_button_UI(id = "MR_model_button_id"),
                                   br(),
                                   # progress indicator for main model 
                                   tags$head(tags$style("#MR_model_id-progress_main_model{overflow-y:scroll; max-height: 400px;}")),
                                   progress_main_model_UI(id = "MR_model_id"), 
                                   br(),
                                   reset_button_UI(id = "MR_model_id")
    ),
    tabPanel("Study-level Outcomes",
                                   br(),
                                   p("Note: If the presence of zero's for two of TP, FN, FP and TN causes sensitivity or specificity to be calculated
                                                           as 0/0 than an error message will appear."),
                                   br(),
                                   dropdownButton(
                                     MR_data_table_settings_UI(id = "MR_model_id"),
                                     circle = TRUE, status = "danger",
                                     icon = icon("gear"), width = "300px",
                                     tooltip = tooltipOptions(title = "Table menu")
                                   ),
                                   MR_data_table_UI(id = "MR_model_id"),
                                   
                                   br(),
                                   p("Note: Arrows to the right of the column headings can be used to sort data into ascending or descending order. "),
                                   p("N is  the total number of individuals in each study ( N = TP + FN + FP + TN )"),
                                   p("Sens is the sensitivity, which is the probability of a positive test result given that
                                                           the patient has the disease ( Sens = TP / [TP + FN] )"),
                                   p("Spec is the specificity, which is the probability of a negative test result given that
                                                           the patient does not have the disease ( Spec = TN / [TN + FP] )"),
                                   p("Weight_Sens is the precentage study weight of sensitvitiy, calculated using methods by Burke et al."),
                                   p("Weight_Spec is the percentage study weight of sensitivity, calculated using methods by Burke et al."),
                                   br()
    ),
    tabPanel("Parameter Estimates",
                                           MR_parameter_estimates_table_UI(id = "MR_model_id")
    ),
    tabPanel("Model Diagnostics",
                                           MR_model_diagnostics_tab_UI(id = "MR_model_id")
    )
  ), # end of tab box

  tabBox( # second tab box on the right-hand side to display plots 

     tabPanel("sROC plot", 
                                             h3("Meta-regression - sROC plot"),
                                             br(),
                                             dropdownButton(
                                               MR_sroc_plot_settings_menu_UI(id = "MR_model_id"),
                                               circle = TRUE, status = "danger",
                                               icon = icon("gear"), width = "300px",
                                               tooltip = tooltipOptions(title = "Click to customise plot")
                                             ),
                                             tags$head(uiOutput("css_MR_sroc_head")),
                                             uiOutput("css_MR_sroc_script"),    
                                             MR_sroc_plot_UI(id = "MR_model_id")
                                    ),
     tabPanel("Accuracy vs covariate plot",
                                            h3("Meta-regression - accuracy vs covariate plot"),
                                            br(),
                                            dropdownButton(
                                              MR_plot_2_settings_menu_UI(id = "MR_model_id"),
                                              circle = TRUE, status = "danger",
                                              icon = icon("gear"), width = "300px",
                                              tooltip = tooltipOptions(title = "Click to customise plot")
                                            ),
                                            tags$head(uiOutput("css_MR_plot_2_head")),
                                            uiOutput("css_MR_plot_2_script"),
                                            MR_plot_2_UI(id = "MR_model_id")
     )
                                  ) # end of second tab box
),







# Tab 6: Bivariate MA - Subgroup analysis (SG) -------------------------------------- ------------------------------------------------------------------------------------------------------------------
tabItem(
  tabName = "SG_bivariate",
  tabBox(
    tabPanel("Model set up & priors",
                                             SG_input_UI(id = "SG_model_id"),
                                             # progress indicator for prior model 
                                             tags$head(tags$style("#SG_model_id-progress_prior_model{overflow-y:scroll; max-height: 400px;}")),
                                             progress_prior_model_UI(id = "SG_model_id"), 
                                             p_scale_priors_indicator_checkbox_UI(id = "SG_model_id"),
             
                                             dropdown(
                                               sampler_options_UI(id = "SG_model_id"),
                                               circle = TRUE, status = "danger",
                                               icon = icon("gear"), width = "300px",
                                               animate = animateOptions(
                                                 enter = animations$fading_entrances$fadeInLeftBig,
                                                 exit = animations$fading_exits$fadeOutRightBig
                                               ),
                                               tooltip = tooltipOptions(title = "Advanced option - click to change Stan sampler options")
                                             ), 
             
                                             h4("Priors:"),
                                             dropdown(
                                               SG_priors_options_UI(id = "SG_model_id"),
                                               circle = TRUE, status = "danger",
                                               icon = icon("gear"), width = "300px",
                                               animate = animateOptions(
                                                 enter = animations$fading_entrances$fadeInLeftBig,
                                                 exit = animations$fading_exits$fadeOutRightBig
                                               ),
                                               tooltip = tooltipOptions(title = "Click to change prior distributions")
                                             ),
                                             br(),
                                             run_prior_model_button_UI(id = "SG_prior_model_button_id"),
                                             br(),
                                             h3("Table of prior distributions"),
                                             SG_model_priors_table_UI(id = "SG_model_id"),
                                             h3("Plot of prior distributions"),
                                             model_priors_plot_UI(id = "SG_model_id"),
             
                                             reset_button_UI(id = "SG_model_id")
    ),
    tabPanel("Run model",
                                             run_model_button_UI(id = "SG_model_button_id"),
                                             tags$head(tags$style("#SG_model_id-progress_main_model{overflow-y:scroll; max-height: 400px;}")),
                                             progress_main_model_UI(id = "SG_model_id"),   # progress indicator for main model 
                                             br(),
                                             reset_button_UI(id = "SG_model_id")
    ),
    tabPanel("Study-level Outcomes",
                                             br(),
                                             p("Note: If the presence of zero's for two of TP, FN, FP and TN causes sensitivity or specificity to be calculated
                                                                     as 0/0 than an error message will appear."),
                                             br(),
                                             dropdownButton(
                                               SG_data_table_settings_UI(id = "SG_model_id"),
                                               circle = TRUE, status = "danger",
                                               icon = icon("gear"), width = "300px",
                                               tooltip = tooltipOptions(title = "Table menu")
                                             ),
                                             SG_data_table_UI(id = "SG_model_id"),
                                             br(),
                                             br(),
                                             p("Note: Arrows to the right of the column headings can be used to sort data into ascending or descending order. "),
                                             p("N is  the total number of individuals in each study ( N = TP + FN + FP + TN )"),
                                             p("Sens is the sensitivity, which is the probability of a positive test result given that
                                                                     the patient has the disease ( Sens = TP / [TP + FN] )"),
                                             p("Spec is the specificity, which is the probability of a negative test result given that
                                                                     the patient does not have the disease ( Spec = TN / [TN + FP] )"),
                                             p("Weight_Sens is the precentage study weight of sensitvitiy, calculated using methods by Burke et al."),
                                             p("Weight_Spec is the percentage study weight of sensitivity, calculated using methods by Burke et al."),
                                             br()
    ),
                                            tabPanel("Parameter Estimates", 
                                                      SG_parameter_estimates_table_UI(id = "SG_model_id")
    ),
                                            tabPanel("Model Diagnostics", 
                                                     SG_model_diagnostics_tab_UI(id = "SG_model_id")
    )
  ), # end of first tab box 
                                            tabBox(
                                              tabPanel("sROC plot",
                                                       h3("Subgroup analysis - sROC plot"),
                                                       dropdownButton(
                                                         SG_sroc_plot_settings_menu_UI(id = "SG_model_id"),
                                                         circle = TRUE, status = "danger",
                                                         icon = icon("gear"), width = "300px",
                                                         tooltip = tooltipOptions(title = "Click to customise plot")
                                                       ),
                                                       tags$head(uiOutput("css_SG_sroc_head")),
                                                       uiOutput("css_SG_sroc_script"),
                                                       SG_sroc_plot_UI(id = "SG_model_id")
                                              ),
                                              tabPanel("Accuracy vs subgroup plot",     
                                                       h3("Subgroup analysis - Accuracy vs subgroup plot"),
                                                       dropdownButton(
                                                         SG_plot_2_settings_menu_UI(id = "SG_model_id"),
                                                         circle = TRUE, status = "danger",
                                                         icon = icon("gear"), width = "300px",
                                                         tooltip = tooltipOptions(title = "Click to customise plot")
                                                       ),
                                                       tags$head(uiOutput("css_SG_plot_2_head")),
                                                       uiOutput("css_SG_plot_2_script"),
                                                       SG_plot_2_UI(id = "SG_model_id")
                                              )
                                            ) # end of second tab box 
), # end of tab item





# Tab 7: Latent class model (LCM)  --------------------------------------------  -------------------------------------------------------------------------------------------------------
tabItem(
  tabName = "meta_analysis_latent_class",
  tabBox(
    tabPanel("Model set up & priors",
                             dropdown(
                               sampler_options_UI(id = "LCM_model_id"),
                               circle = TRUE, status = "danger",
                               icon = icon("gear"), width = "300px",
                               animate = animateOptions(
                                 enter = animations$fading_entrances$fadeInLeftBig,
                                 exit = animations$fading_exits$fadeOutRightBig
                               ),
                               tooltip = tooltipOptions(title = "Advanced option - click to change Stan sampler options")
                             ),
                             br(), 
                             LCM_model_options_UI(id = "LCM_model_id"),
                             br(), 
                             h4("Priors"),
                             p_scale_priors_indicator_checkbox_UI(id = "LCM_model_id"),
                             dropdown(
                               LCM_priors_options_inputModule_UI(id = "LCM_model_id"),
                               circle = TRUE, status = "danger",
                               icon = icon("gear"), 
                               width = "500px",
                               animate = animateOptions(
                                 enter = animations$fading_entrances$fadeInLeftBig,
                                 exit = animations$fading_exits$fadeOutRightBig,
                               ),
                               tooltip = tooltipOptions(title = "Click to change prior distributions")
                             ),
                             br(),
                             # run prior model
                             run_prior_model_button_UI(id = "LCM_prior_model_button_id"), 
                             br(), 
                             br(), 
                             tags$head(tags$style("#LCM_model_id-progress_prior_model{overflow-y:scroll; max-height: 400px;}")),
                             br(), 
                             progress_prior_model_UI(id = "LCM_model_id"),                 # progress indicator for prior model 
                             br(),
                             h3("Table of prior distributions"),
                             LCM_model_priors_table_UI(id = "LCM_model_id"),
                             br(),
                             h3("Plot of prior distributions"),
                             model_priors_plot_UI(id = "LCM_model_id"),
    ),
             
    tabPanel("Run model",
                             run_model_button_UI(id = "LCM_model_button_id"),
                             br(),
                             br(),
                             tags$head(tags$style("#LCM_model_id-progress_main_model{overflow-y:scroll; max-height: 400px;}")), 
                             br(),
                             progress_main_model_UI(id = "LCM_model_id"),                # progress indicator for main model 
                             br(), 
                             reset_button_UI(id = "LCM_model_id"), 
                             br(), 
                             br(), 
          # run SA model
                             SA_indicator_UI(id = "SA_LCM_model_id"),
                             br(),
                             SA_run_model_button_UI(id = "SA_LCM_model_button_id"),
                             br(),
                             br(),
                             tags$head(tags$style("#SA_LCM_model_id-progress_main_model{overflow-y:scroll; max-height: 400px;}")),
                             br(),
                             progress_main_model_UI(id = "SA_LCM_model_id"), # progress indicator for main model  - SA
                             br()
    ),
    tabPanel("Study-level Outcomes",
             br(),
             p("Note: If the presence of zero's for two of TP, FN, FP and TN causes sensitivity or specificity to be calculated
                                     as 0/0 than an error message will appear."),
             br(),
             br(),
             dropdownButton(
               LCM_data_table_settings_UI(id = "LCM_model_id"),
               circle = TRUE, status = "danger",
               icon = icon("gear"), width = "300px",
               tooltip = tooltipOptions(title = "Table menu")
             ),
             LCM_data_table_UI(id = "LCM_model_id"),
             br(),
             br(),
             p("Note: Arrows to the right of the column headings can be used to sort data into ascending or descending order. "),
             p("N is  the total number of individuals in each study ( N = TP + FN + FP + TN )"),
             p("Sens is the sensitivity, which is the probability of a positive test result given that 
                                     the patient has the disease ( Sens = TP / [TP + FN] )"),
             p("Spec is the specificity, which is the probability of a negative test result given that
                                     the patient does not have the disease ( Spec = TN / [TN + FP] )"), 
             p("Weight_Sens is the precentage study weight of sensitvitiy, calculated using methods by Burke et al."),
             p("Weight_Spec is the percentage study weight of sensitivity, calculated using methods by Burke et al."),
             br()
    ),
    tabPanel("Parameter Estimates", 
             LCM_parameter_estimates_tab_UI(id = "LCM_model_id")
    ), 
    tabPanel("Model Diagnostics", 
             LCM_model_diagnostics_tab_UI(id = "LCM_model_id")
    )
  ),
  tabBox(
    tabPanel("sROC Plot",
             h3("Latent class model - sROC plot"),
             dropdownButton(
               LCM_sroc_plot_settings_menu_UI(id = "LCM_model_id"),  # output sROC settings menu
               circle = TRUE, status = "danger",
               icon = icon("gear"), width = "300px",
               tooltip = tooltipOptions(title = "Click to customise plot")
             ),
             br(),
             LCM_sroc_plot_UI(id = "LCM_model_id"), # output sROC plot
        # CSS code for study info pop-ups
             tags$head(uiOutput("LCM_sroc_study_info_css_head")),
             uiOutput("LCM_sroc_study_info_css_script"),
        # CSS code for piechart pop-ups (only for "QA.csv" and "Cov_QA.csv" )
             tags$head(uiOutput("css_LCM_sroc_piechart_head")),
             uiOutput("css_LCM_sroc_piechart_script")
    )
  )
), 









# Tab 8: References ----------------------------------------------------------  --------------------------------------------------------------------------------------------------------------------------------------------------------------
tabItem(
  tabName = "refs", 
  tabPanel("References", h1("References"),
           br(),
           
           p(a("Reitsma JB, Glas AS, Rutjes AW, Scholten RJ, Bossuyt PM, Zwinderman AH. 
                    Bivariate analysis of sensitivity andspecificity produces informative summary
                    measures in diagnostic reviews. Journal of Clinical Epidemiology, 2005",
               href = "https://linkinghub.elsevier.com/retrieve/pii/S0895435605001629", target = "_blank")),
           
           p(a("Carolyn M. Rutter and Constantine A. Gatsonis. “A hierarchical regression approach to meta-
                analysis of diagnostic test accuracy evaluations”. In: Statistics in Medicine (2001). issn: 02776715.
                doi: 10.1002/sim.942",
               href = "https://pubmed.ncbi.nlm.nih.gov/11568945/", target = "_blank")),
           
           
           p(a("Harbord R. A unification of models for meta-analysis of diagnostic accuracy studies. Biostatistics. 2007;8:239-251",
               href = "https://www.ncbi.nlm.nih.gov/pubmed/16698768", target = "_blank")),
           
           p(a("Haitao Chu, Sining Chen, and Thomas A. Louis. “Random effects models in a meta-analysis
                of the accuracy of two diagnostic tests without a gold standard”. In: Journal of the American
                Statistical Association (2009). issn: 01621459. doi: 10.1198/jasa.2009.0017",
               href = "https://pubmed.ncbi.nlm.nih.gov/19562044/", target = "_blank")),
           
           p(a(" J. Menten, M. Boelaert, and E. Lesaffre. “Bayesian meta-analysis of diagnostic tests allowing for
                 imperfect reference standards”. In: Statistics in Medicine (2013). issn: 10970258. doi: 10.1002/
                 sim.5959.",
               href = "https://pubmed.ncbi.nlm.nih.gov/24003003/", target = "_blank")),
           
           p(a("Burke DL, Ensor J, Snell KI, van der Windt D, Riley RD. Guidance for deriving and presenting percentage
                        study weights in meta-analysis o
                            tagList(f test accuracy studies. Research synthesis methods. 2018 Jun.", 
               href = "https://onlinelibrary.wiley.com/doi/full/10.1002/jrsm.1283", target = "_blank")),

           p(a("Harrison, J.K., Fearon, P., Noel-Storr, A.H., McShane, R., Stott, D.J. and Quinn, T.J., 2015. Informant 
                        Questionnaire on Cognitive Decline in the Elderly (IQCODE) for the diagnosis of dementia within a secondary 
                        care setting. Cochrane database of systematic reviews, (3).", href = "https://www.ncbi.nlm.nih.gov/pubmed/25754745", 
               target = "_blank")),
           
           p(a("Partlett C, Takwoingi Y. Meta-analysis of test accuracy studies in R: a summary of user-written
                      programs and step-by-step guide to using glmer. Version 1.0. August 2016.",
               href = "http://methods.cochrane.org/sdt/", target="_blank")),
           
           p(a("QUADAS-2 tool", href = "https://www.bristol.ac.uk/population-health-sciences/projects/quadas/quadas-2/", target = "_blank")),
           br(),
           
           p(a("Bob Carpenter et al. “Stan: A probabilistic programming language”. In: Journal of Statistical
                Software (2017). issn: 15487660. doi: 10.18637/jss.v076.i01.",
               href = "https://www.jstatsoft.org/article/view/v076i01", target = "_blank")),
           
           p(a(" Stan Modeling Language Users Guide and Reference Manual. https://mc-stan.org/docs/2_
                 25/reference-manual/. 2020.",
               href = "https://mc-stan.org/docs/2_25/stan-users-guide/index.html", target = "_blank")),

           h4("Code"),
           p(a("The R code for the app is available on GitHub here", 
               href="https://github.com/CRSU-Apps/MetaBayesDTA", target="_blank")),
           
  ))

) # end of tab items
))




