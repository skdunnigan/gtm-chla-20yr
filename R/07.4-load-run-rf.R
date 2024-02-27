# random forest analysis using the correlation parameters
# 
# # load packages
library(here)
source(here('R', '00_loadpackages.R'))


# load correlation data for each station

pi_cor <- readxl::read_xlsx(here('data', 'correlations.xlsx'),
                            sheet = "PI") 
ss_cor <- readxl::read_xlsx(here('data', 'correlations.xlsx'),
                            sheet = "SS") %>% mutate_if(is.character, as.numeric)
fm_cor <- readxl::read_xlsx(here('data', 'correlations.xlsx'),
                            sheet = "FM") 
pc_cor <- readxl::read_xlsx(here('data', 'correlations.xlsx'),
                            sheet = "PC") 

# set up random forest analysis for each station
# 

pi.rf <- pi_cor %>% 
  select(-datetimestamp) %>% 
  randomForest(chla ~., data = ., 
               importance = TRUE, 
               nPerm = 1000,
               norm.votes = FALSE,
               na.action = na.exclude)
print(pi.rf)

# look at variable importance:
round(importance(pi.rf), 2)
print(pi.rf$importanceSD)

pi.rf2 <- pi_cor %>% 
  select(-datetimestamp, -totprcp, -)