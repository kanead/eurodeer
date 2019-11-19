#'##########################################################################
#' ame1 project - studying the relation between home range size and activity
#' 2019/10/04
#' Adam Kane, Enrico Pirotta & Barry McMahon
#' https://mecoco.github.io/ame1.html
#' statistical model 
############################################################################
#' load the packages
library(tidyverse)
library(lme4)

#' load in some helper functions e.g. for calculating VIF
source("code/6.Helper_functions.r")

#' load the data
mydata <- read_csv("results/combined_data.csv", col_names = TRUE)
head(mydata)
names(mydata)

#' extract variance inflation factors (VIF) 
#' https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/j.2041-210X.2009.00001.x
test_vif <- mydata %>% select(ndvi_Mean,altitude_Mean,slope_Mean,aspect_Mean,tree_Mean,mean_act,sd_act)
test_vif <- data.frame(test_vif)
corvif(test_vif)

#' mean_act is correlated with sd_act and
#' altitude_mean is correlated with slope mean 
#' let's remove mean_act and altitude_mean

#' mcp model
m1 <- glmer(
  mcp ~ 
    sex + 
    age +  
    ndvi_Mean + 
#    altitude_Mean + 
    slope_Mean + 
    aspect_Mean + 
    predator + 
    corine_Mode + 
    tree_Mean + 
    age +  
  #  mean_act * activity_sensors_id +
    sd_act * activity_sensors_id +
    (1 | study_areas_id) + (1 | month) +
    (1 | study_areas_id:month) +
    (1 | animals_id),
  data = mydata#,
  #family = Gamma
)

summary(m1)


