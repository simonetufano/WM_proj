#### OPTIONS ####

options(scipen=999)
set.seed(123456)

#### LIBRARIES ####
library(magrittr)
library(ggplot2)
library(forcats)
library(lubridate)
library(RQuantLib)
library(hrbrthemes)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(e1071)
library(ipred)
library(pROC)
library(funModeling)
library(arules)
library(arulesViz)
library(plyr)
library(dplyr)


#### DIRECTORIES ####
working_dir = "C:\\Users\\Simone\\Desktop\\digital marketing\\elaborato\\script"
data_dir = "C:\\Users\\Simone\\Desktop\\digital marketing\\elaborato\\script"

setwd(working_dir)

#### EXECUTION FULL PIPELINE ####

PIPELINE_scripts <- c(
  'B01_ingestion.R'
  , 'C01_preparation_df1.R'
  , 'C02_preparation_df2.R'
  , 'C03_preparation_df3.R'
  , 'C04_preparation_df4.R'
  , 'C05_preparation_df5.R'
  , 'C06_preparation_df6.R'
  , 'C07_preparation_df7.R'
  , 'RFM_MODEL.R'
  , 'CHURN_MODEL.R'
  , 'market basket analysis.R'
  )

for(i in PIPELINE_scripts){
  source(i, echo = TRUE)
}
