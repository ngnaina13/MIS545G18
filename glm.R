# Naina Gupta
# MIS 545 Section 02
# LogisticRegression.R
# installed tidyverse, dummies packages
# install.packages("tidyverse")
# install.packages("dummies")
# install.packages("olsrr")
# install.packages("smotefamily")

library(tidyverse)
library(dummies)
library(corrplot)
library(olsrr)
library(smotefamily)

# set working directory 
setwd("C:/Users/ngnai/OneDrive/Desktop/ECON 511A")
customerChurn <- read_csv("ModelChurn.csv")

# display customerChurn in the console
print(customerChurn)

# display structure in the console
str(customerChurn)                  

# display summary in the consile
summary(customerChurn)

# remove customerID from the data
customerChurn1 <- customerChurn %>% select(-customerID)

# remove NA's from the data
customerChurn1$TotalCharges[is.na(customerChurn1$TotalCharges)] <-
 mean(customerChurn1$TotalCharges,na.rm=TRUE)

# summary of the customerChurn1
summary(customerChurn1)

# Recreating displayAllHistograms() function
displayAllHistograms <- function(tibbleDataset) {
 tibbleDataset %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot() + geom_histogram(mapping = aes(x=value, fill=key),
                            color = "black") +
  facet_wrap(~ key, scales = "free") +
  theme_minimal ()
}
# Call the displayAllHistograms() function, passing in zooSpending as an
# argument
displayAllHistograms(customerChurn1)

# display the correlation plot using number method and limit output to
# the bottom left
cor(customerChurn1)


# display the correlation plot using number method and limit output to
# the bottom left
corrplot(cor(customerChurn1),
         method = "number",
         type ="lower")

# setting random seed
set.seed(1234)

# putting the records from (75%) into customerChurn1
sampleSet <- sample(nrow(customerChurn1),
                    round(nrow(customerChurn1)*0.75),
                    replace= FALSE)

# putting the records from (75%) into customerChurn1
Training <- customerChurn1[sampleSet,]

# putting the record from (25%) into customerChurn1
Testing <- customerChurn1[-sampleSet,]

# Generating the logistic regression model 
churnModel <- glm(data = customerChurn1,
                  family = binomial,
                  formula = Churn ~.)

# summary of the model
summary(churnModel)


predictionModel <- predict(churnModel,
                 Testing,
                 type = "response")

# Used model to predict outcomes in the testing dataset and
# and treat anything below or equal to 0.5 as a 0, anything above 0.5 as a 1.
predictionModel <- ifelse(predictionModel >= 0.5,1,0)

print(predictionModel)  

churnConfusionMatrix <- table(Testing$Churn,
                       predictionModel)
print(churnConfusionMatrix)

print("Logistic Regression Accuracy")
misClasificError <- mean(predictionModel != Testing$Churn)





