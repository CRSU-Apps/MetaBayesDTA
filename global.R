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
# install.packages("htmltools")
# install.packages("usethis")
# install.packages("R6")
# install.packages("rio")


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
require(usethis)
require(R6)
require(rio)
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

source('./m_MR_p1_v3.R', local  = TRUE)
source('./m_MR_p2.R', local  = TRUE)
source('./m_MR_p3.R', local  = TRUE)
source('./m_MR_p4.R', local  = TRUE)

source('./m_SG_p1.R', local  = TRUE)
source('./m_SG_p2.R', local  = TRUE)
source('./m_SG_p3.R', local  = TRUE)
source('./m_SG_p4.R', local  = TRUE)

source('./m_LCM_p1_v2.R', local  = TRUE)
source('./m_LCM_p2.R', local  = TRUE)
source('./m_LCM_p3_v2.R', local  = TRUE)
source('./utils.R', local = TRUE)

# Load in images for TP, FP, FN, TN  -------------------------------------------------------------------
TPimg <-readPNG('./www/TP.png')
TNimg <-readPNG('./www/TN.png')
FPimg <-readPNG('./www/FP.png')
FNimg <-readPNG('./www/FN.png')