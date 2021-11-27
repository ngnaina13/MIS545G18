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

#========
# Numeric data analysis
#========
summary(telecomVisual)
telecomVisNumeric <- telecomVisual %>% 
  select(tenure,MonthlyCharges,TotalCharges)

# Display all boxplot
displayAllBoxplots <- function(tibbleDataset) {
  tibbleDataset %>% 
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + 
    geom_boxplot(mapping = aes(x=value, fill=key),
                 color = "black",
                 outlier.shape = 1) +
    facet_wrap( ~ key, scales = "free") +
    theme_minimal() +
    coord_flip()
}
displayAllBoxplots(telecomVisual)
# -- No outlier in the data

# Display all histograms
displayAllHistograms <- function(tibbleDataset) {
  tibbleDataset %>% 
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + 
    geom_histogram(mapping = aes(x=value, fill=key),
                   color = "black") +
    facet_wrap( ~ key, scales = "free") +
    theme_minimal()
}
displayAllHistograms(telecomVisual)
# -- Go to find the interesting points. 
# -- Might need add the range in monthlyCharges as a category when improving


# Display a correlation matrix
round(cor(telecomVisNumeric),2)

corrplot(cor(telecomVisNumeric),
         method = "number",
         type = "lower")
# --  Note: if your algorithm have to do extra preprocess, you have to care 
#           about the threshold of 0.7, then remove the variables.(See HW6) 



#========
# Categorical data analysis
#========
summary(telecomVisual)
telecomVisual %>%
ggplot() +
  geom_bar(mapping = aes(x =Churn, fill = gender), color = "black") +
  labs(title = "xxxx",
     x = "xxxx", y = "number") 
# -- Go to find the interesting points. 



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

