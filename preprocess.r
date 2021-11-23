library(tidyverse)


###########################
# Import data
##########################

setwd()
telecom <- read_csv(file = './data/WA_Fn-UseC_-Telco-Customer-Churn.csv',
                    col_types = 'cflffiffffffffffffnnf',
                    col_names = TRUE)


###########################
# Data Inspection 
##########################

# To see the data condition
summary(telecom)

# TotalCharges have na data, go deeper to see the row
checkNa <- telecom[is.na(telecom$TotalCharges),]
print(checkNa)

# All the na data have MonthlyCharges, but tenure is 0
# Dose 0 tenure cause na TotalCharges?
print(telecom[telecom$tenure == 0,c("customerID","tenure","TotalCharges")])
# Yes~!! They are the new costumer! Considering its tinny ratio , remote them!
telecomNoNA <- telecom[!is.na(telecom$TotalCharges),]

# Check again
summary(telecomNoNA)
# There are no na value any more!



###########################
# Start the Visualization
##########################

# For the Visualizational purpose change SeniorCitizen to YES/NO
telecomVisual <- telecomNoNA %>% 
  mutate(SeniorCitizen = ifelse(SeniorCitizen ==  0, 'NO', 'YES'))



#########################
# pre-process for moduel
#########################


####### Whole data ########

# gender
telecom %>% mutate(gender = ifelse(gender == 'Female', 0, 1),
                   SeniorCitizen = ifelse(SeniorCitizen == 'FALSE', 0, 1),
                   Partner = ifelse(Partner == 'FALSE', 0, 1),
                   Dependents = ifelse(Dependents == 'FALSE', 0, 1),
                   PhoneService = ifelse(PhoneService == 'FALSE', 0, 1),
                   )


####### Feature Selection ########

