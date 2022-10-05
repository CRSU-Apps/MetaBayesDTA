# Load / install packages needed for app ----------------------------------


# .libPaths()
#  myPaths <- .libPaths()   # get the path(s)
#  myPaths
#  myPaths[1] <- "C:/Users/ec325/Documents" # for work laptop
# 
#  #myPaths[1] <- "/home/enzo/R/x86_64-pc-linux-gnu-library/4.1" # for linux comp
# 
#  myPaths
# 
#  #/usr/local/lib/R/site-library
#  #/home/enzo/R/x86_64-pc-linux-gnu-library/4.1
# 
#  .libPaths(myPaths)  # reassign the path
#  .libPaths()








# install.packages("shiny")
# install.packages("shinythemes")
# install.packages("shinyWidgets")
# install.packages("shinydashboard")
# install.packages("shinyjs")
# install.packages("rstan")
# install.packages("dplyr")
# install.packages("readxl")
# install.packages("ggplot2")
# install.packages("ggforce")
# install.packages("ggrepel")
# install.packages("DT")
# install.packages("magic")
# install.packages("foreach")
# install.packages("Hmisc")
# install.packages("stringr")
# install.packages("data.table")
# install.packages("patchwork")
# install.packages("scatterpie")
# install.packages("tidyverse")
# install.packages("cowplot")
# install.packages("ggnewscale")
# install.packages("rlist")
# install.packages("rlang")
# install.packages("varhandle")
# install.packages("mada")
# install.packages("png")
# install.packages("devtools")
# install.packages("dashboardthemes")
# install.packages("tableHTML")
# install.packages("shinyalert")
# install.packages("shinybusy")
# install.packages("callr")
# install.packages("MASS")
# install.packages("usethis")
# install.packages("htmltools")

require(shiny)
require(shinythemes)
require(shinyWidgets)
require(shinydashboard)
require(shinyjs)
require(StanHeaders)
require(rstan)
require(plyr)
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
require(htmltools)
# Set Stan options -------------------------------------------------------------
#rstan_options(auto_write = TRUE)
#options(mc.cores = parallel::detectCores())

# Load functions ----------------------------------------------------------
source('./fn_general.R', local  = TRUE)
source('./fn_BVM.R', local  = TRUE)
source('./fn_SG.R', local  = TRUE)
source('./fn_LCM.R', local  = TRUE)

# Load modules ------------------------------------------------------------
source('./m_general.R', local  = TRUE)

source('./m_BVM_p1.R', local  = TRUE)
source('./m_BVM_p2.R', local  = TRUE)
source('./m_BVM_p3.R', local  = TRUE)
source('./m_BVM_p4.R', local  = TRUE)

source('./m_MR_p1.R', local  = TRUE)
source('./m_MR_p2.R', local  = TRUE)
source('./m_MR_p3.R', local  = TRUE)
source('./m_MR_p4.R', local  = TRUE)

source('./m_SG_p1.R', local  = TRUE)
source('./m_SG_p2.R', local  = TRUE)
source('./m_SG_p3.R', local  = TRUE)
source('./m_SG_p4.R', local  = TRUE)

source('./m_LCM_p1.R', local  = TRUE)
source('./m_LCM_p2.R', local  = TRUE)
source('./m_LCM_p3.R', local  = TRUE)

# Load in images for TP, FP, FN, TN  -------------------------------------------------------------------
TPimg <-readPNG('./www/TP.png')
TNimg <-readPNG('./www/TN.png')
FPimg <-readPNG('./www/FP.png')
FNimg <-readPNG('./www/FN.png')


# # Load UI (ui.R file) -----------------------------------------------------
#source('ui.R')
# 
# # Load server (server.R file) ---------------------------------------------
#source('server.R')

# Pre-load Stan model files without R shiny first (so R doesn't need to re-compile the
# models overtime the app is launched) ----------------------------------------------
# Save RDS of each Stan model - the compiler flags / CXXFLAGS needs to match the one in shinyapps.io 
# only possible to change these on Linux 
# see https://discourse.mc-stan.org/t/compile-stan-model-in-shiny-app/10022/16

# saveRDS(stan_model(file = './models/BVM_PO.stan'), './models/BVM_PO.rds')
# saveRDS(stan_model(file = './models/p_scale_priors/BVM_PO_p_scale_priors.stan'), './models/p_scale_priors/BVM_PO_p_scale_priors.rds')
# saveRDS(stan_model(file = './models/BVM.stan'), './models/BVM.rds')
# saveRDS(stan_model(file = './models/p_scale_priors/BVM_p_scale_priors.stan'), './models/p_scale_priors/BVM_p_scale_priors.rds')
# 
# saveRDS(stan_model(file = './models/MR_cts_PO.stan'), './models/MR_cts_PO.rds')
# saveRDS(stan_model(file = './models/MR_cat_PO.stan'), './models/MR_cat_PO.rds')
# saveRDS(stan_model(file = './models/p_scale_priors/MR_cat_PO_p_scale_priors.stan'), './models/p_scale_priors/MR_cat_PO_p_scale_priors.rds')
# saveRDS(stan_model(file = './models/MR_cts.stan'), './models/MR_cts.rds')
# saveRDS(stan_model(file = './models/MR_cat.stan'), './models/MR_cat.rds')
# saveRDS(stan_model(file = './models/p_scale_priors/MR_cat_p_scale_priors.stan'), './models/p_scale_priors/MR_cat_p_scale_priors.rds')
# 
# saveRDS(stan_model(file = './models/SG_PO.stan'), './models/SG_PO.rds')
# saveRDS(stan_model(file = './models/p_scale_priors/SG_PO_p_scale_priors.stan'), './models/p_scale_priors/SG_PO_p_scale_priors.rds')
# saveRDS(stan_model(file = './models/SG.stan'), './models/SG.rds')
# saveRDS(stan_model(file = './models/p_scale_priors/SG_p_scale_priors.stan'), './models/p_scale_priors/SG_p_scale_priors.rds')
# 
# saveRDS(stan_model(file = './models/BLCM_ma_PO.stan'), './models/BLCM_ma_PO.rds')
# saveRDS(stan_model(file = './models/p_scale_priors/BLCM_ma_PO_p_scale_priors.stan'), './models/p_scale_priors/BLCM_ma_PO_p_scale_priors.rds')
# saveRDS(stan_model(file = './models/BLCM_ma.stan'), './models/BLCM_ma.rds')
# saveRDS(stan_model(file = './models/p_scale_priors/BLCM_ma_p_scale_priors.stan'), './models/p_scale_priors/BLCM_ma_p_scale_priors.rds')

MA_model_PO <- readRDS(file = './models/BVM_PO.rds')
MA_model_PO_p_scale_priors <- readRDS(file = "./models/p_scale_priors/BVM_PO_p_scale_priors.rds")
MA_model <- readRDS(file = "./models/BVM.rds")
MA_model_p_scale_priors <- readRDS(file = "./models/p_scale_priors/BVM_p_scale_priors.rds")

MR_model_cts_PO <- readRDS(file = './models/MR_cts_PO.rds')
MR_model_cat_PO <- readRDS(file = './models/MR_cat_PO.rds')
MR_model_cat_PO_p_scale_priors <- readRDS(file = './models/p_scale_priors/MR_cat_PO_p_scale_priors.rds')
MR_model_cts <- readRDS(file = './models/MR_cts.rds')
MR_model_cat <- readRDS(file = './models/MR_cat.rds')
MR_model_cat_p_scale_priors <- readRDS(file = './models/p_scale_priors/MR_cat_p_scale_priors.rds')

SG_model_PO <- readRDS(file = './models/SG_PO.rds')
SG_model_PO_p_scale_priors <- readRDS(file = './models/p_scale_priors/SG_PO_p_scale_priors.rds')
SG_model <- readRDS(file = './models/SG.rds')
SG_model_p_scale_priors <- readRDS(file = './models/p_scale_priors/SG_p_scale_priors.rds')

LCM_model_PO <- readRDS(file ='./models/BLCM_ma_PO.rds')
LCM_model_PO_p_scale_priors <- readRDS(file = './models/p_scale_priors/BLCM_ma_PO_p_scale_priors.rds')
LCM_model <- readRDS(file = './models/BLCM_ma.rds')
LCM_model_p_scale_priors <- readRDS(file = './models/p_scale_priors/BLCM_ma_p_scale_priors.rds')

