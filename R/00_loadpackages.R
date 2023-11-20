# be sure to check for packages conflicts!
# 01 import/export ----
library(readxl) # read excel files
library(janitor) # simple tools to clean dirty data
library(here) # a simpler way to find your files
library(SWMPr) # working with SWMP data from the NERRS

# 02 tidy and wrangle ----
library(tidyverse) # because...tidyverse (ggplot2, tidyr, dplyr)
library(lubridate) # dates and times
library(tidyr) # in case of package updates outside of `tidyverse`
library(dplyr)

# 03 pulling information and statistics ----
library(broom) # convert statistical analysis objects into tidy tibbles
if(!require(wqtrends)){ 
  options(repos = c(
    tbeptech = 'https://tbep-tech.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))
  install.packages('wqtrends')} ; library(wqtrends) # models and plots for trends
if(!require(mgcv)){ install.packages("mgcv") } ;  library(mgcv)
# if(!require(changepoint)) { install.packages("changepoint")}; library(changepoint)
if(!require(strucchange)) { install.packages('strucchange')}; library(strucchange)
# library(psych)
library(wql)

# 04 markdown ----
library(rmarkdown)
library(knitr)
library(kableExtra) # https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html 

# 05 graphics ----
library(khroma) # color-blind friendly palettes
library(patchwork) # grid graphics
library(scales) # scale functions for visualization
library(plotly) # create interactive web graphics - use for html output files
library(ggthemes) # theme expansion for ggplot2

# 06 mapping ---------------------------------------------------------------
library(leaflet)
library(htmltools)
library(sf)

