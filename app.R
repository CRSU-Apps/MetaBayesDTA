################################################################################
# MetaBayesDTA - Beta version 1.0 (Beta v1.0) -----------------------------
# Code Author: Enzo Cerullo
################################################################################

#install_version("shinydashboardPlus", version="0.7.5",repos = "http://cran.us.r-project.org")
#credentials::set_github_pat() # login for version control

require(shiny)
require(shinythemes)
require(shinyWidgets)
require(shinydashboard)
require(shinyjs)
require(rstan)
require(dplyr) 
require(readxl)
require(ggplot2)
require(ggforce)
require(ggrepel)
require(DT)
require(magic)
require(foreach)
require(Hmisc)
require(stringr)
require(data.table)
require(patchwork)
require(scatterpie)
require(tidyverse)
require(cowplot)
require(ggnewscale)
require(rlist)
require(rlang)
require(varhandle)
require(mada)
require(png)
require(devtools)
require(dashboardthemes)
require(tableHTML)
require(shinyalert)
require(shinybusy)
require(callr)
require(MASS)
require(shinybusy)



# Set working directory ---------------------------------------------------
setwd("/home/enzo/Documents/CRSU") # Home PC (Linux Mint)
#setwd("Z:/My Documents/CRSU") # Work PC (windows)
#setwd("C:/Users/Enzo/Documents/CRSU") # Home PC - windows

# Set Stan options -------------------------------------------------------------
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Load functions ----------------------------------------------------------
source('./Functions/fn_general.R', local  = TRUE)
source('./Functions/fn_BVM.R', local  = TRUE)
source('./Functions/fn_SG.R', local  = TRUE)
source('./Functions/fn_LCM.R', local  = TRUE)

# Load modules ------------------------------------------------------------
source('./Modules/m_general.R', local  = TRUE)

source('./Modules/BVM/m_BVM_p1.R', local  = TRUE)
source('./Modules/BVM/m_BVM_p2.R', local  = TRUE)
source('./Modules/BVM/m_BVM_p3.R', local  = TRUE)

source('./Modules/MR/m_MR_p1.R', local  = TRUE)
source('./Modules/MR/m_MR_p2.R', local  = TRUE)
source('./Modules/MR/m_MR_p3.R', local  = TRUE)
source('./Modules/MR/m_MR_p4.R', local  = TRUE)

source('./Modules/SG/m_SG_p1.R', local  = TRUE)
source('./Modules/SG/m_SG_p2.R', local  = TRUE)
source('./Modules/SG/m_SG_p3.R', local  = TRUE)
source('./Modules/SG/m_SG_p4.R', local  = TRUE)

source('./Modules/LCM/m_LCM_p1.R', local  = TRUE)
source('./Modules/LCM/m_LCM_p2.R', local  = TRUE)
source('./Modules/LCM/m_LCM_p3.R', local  = TRUE)

# Load in images for TP, FP, FN, TN  -------------------------------------------------------------------
TPimg <-readPNG('./www/TP.png')
TNimg <-readPNG('./www/TN.png')
FPimg <-readPNG('./www/FP.png')
FNimg <-readPNG('./www/FN.png')


# Load UI (ui.R file) -----------------------------------------------------
source('ui.R')

# Load server (server.R file) ---------------------------------------------
source('server.R')

# Pre-load Stan model files without R shiny first (so R doesn't need to re-compile the
# models overtime the app is launched) ----------------------------------------------
# MA_model_PO <- stan_model(file = './models/BVM_PO.stan')
# MA_model_PO_p_scale_priors <- stan_model(file = "./models/p_scale_priors/BVM_PO_p_scale_priors.stan")
# MA_model <- stan_model(file = "./models/BVM.stan")
# MA_model_p_scale_priors <- stan_model(file = "./models/p_scale_priors/BVM_p_scale_priors.stan")
# 
# MR_model_cts_PO <- stan_model(file = './models/MR_cts_PO.stan')
# MR_model_cat_PO <- stan_model(file = './models/MR_cat_PO.stan')
# MR_model_cat_PO_p_scale_priors <- stan_model(file = './models/p_scale_priors/MR_cat_PO_p_scale_priors.stan')
# MR_model_cts <- stan_model(file = './models/MR_cts.stan')
# MR_model_cat <- stan_model(file = './models/MR_cat.stan')
# MR_model_cat_p_scale_priors <- stan_model(file = './models/p_scale_priors/MR_cat_p_scale_priors.stan')
# 
# SG_model_PO <- stan_model(file = './models/SG_PO.stan')
# SG_model_PO_p_scale_priors <- stan_model(file = './models/p_scale_priors/SG_PO_p_scale_priors.stan')
# SG_model <- stan_model(file = './models/SG.stan')
# SG_model_p_scale_priors <- stan_model(file = './models/p_scale_priors/SG_p_scale_priors.stan')
# 
# LCM_model_PO <- stan_model(file ='./models/BLCM_ma_PO.stan')
# LCM_model_PO_p_scale_priors <- stan_model(file = './models/p_scale_priors/BLCM_ma_PO_p_scale_priors.stan')
# LCM_model <- stan_model(file = './models/BLCM_ma.stan')
# LCM_model_p_scale_priors <- stan_model(file = './models/p_scale_priors/BLCM_ma_p_scale_priors.stan')

# Run App -----------------------------------------------------------------
shinyApp(ui = ui, server = server)





