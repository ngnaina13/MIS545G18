# Aadhithya Dinesh
# MIS 545 Section 02
# Lab12DineshA.R
# To import a dataset of people and generate a neural network model that will 
# predict if a fisher used a chartered boat service based on their 
# fishing catch rate and their annual income
# We will be importing csv files, assigning data types, 
# building a supervised neural network model, and testing for model fit.

# install.packages("tidyverse")
# install.packages("neuralnet")

library(tidyverse)
library(neuralnet)
library(factoextra)
library(cluster)
library(gridExtra)

# set the working directory
setwd("~/MIS/Projects/DataMining")

churn <- read_csv(file = "ModelChurn.csv",
                      col_types = "ciiiiiiiiiiiiiiiiiddi",
                      col_names = TRUE)

churn <- drop_na(churn)
# print the fishingCharter tibble
print(churn)

# print the structure of fishingCharter
print(str(churn))

# print the summary of fishingCharter
print(summary(churn))



# scaling the annual income to a value between 0 and 1
churn <- churn %>%
  mutate(tenureScaled = (tenure - min(tenure))/
           (max(tenure) - min(tenure)))

# scaling the catch rate to a value between 0 and 1
churn <- churn %>%
  mutate(MonthlyChargesScaled = (MonthlyCharges - min(MonthlyCharges))/
           (max(MonthlyCharges) - min(MonthlyCharges)))


churn <- churn %>%
  mutate(TotalChargesScaled = (TotalCharges - min(TotalCharges))/
           (max(TotalCharges) - min(TotalCharges)))


# churn <- select(.data = churn,
#                 Churn,
#                   tenureScaled, MonthlyChargesScaled, TotalChargesScaled,
#                   MultipleLines,
#                   InternetServiceFiberOptic, InternetServiceNo, OnlineSecurity, OnlineBackup
#                 # DeviceProtection, TechSupport, StreamingTV, StreamingMovies, 
#                 #   ContractOneYear, ContractTwoYear
#                 )
# set the seed to 591
set.seed(591)
sampleSet <- sample(nrow(churn),
                    round(nrow(churn)*0.75),
                    replace = FALSE)

# splitting into 75% training dataset
churnTraining <- churn[sampleSet, ]

# loading the remaining 25% of the dataset for testing
churnTesting <- churn[-sampleSet, ]

# generating the neural network
churnNeuralNet <- neuralnet(
  formula = Churn ~ tenureScaled + MonthlyChargesScaled + TotalChargesScaled
    + MultipleLines + InternetServiceFiberOptic +
     InternetServiceNo + OnlineSecurity + OnlineBackup + Gender + SeniorCitizen + Partner + Dependents + PhoneService +
    + tenureScaled + MonthlyChargesScaled + TotalChargesScaled + MultipleLines + InternetServiceFiberOptic + InternetServiceNo + OnlineSecurity + OnlineBackup
     + DeviceProtection + TechSupport + StreamingTV + StreamingMovies + ContractOneYear + ContractTwoYear,
  data = churnTraining,
  hidden = 3,
  act.fct = "logistic",
  linear.output = FALSE)

# displaying the neural network results
print(churnNeuralNet$result.matrix)

# using fishingCharterProbability to generate probablities on the testing dataset
churnProbability <- compute(churnNeuralNet, 
                            churnTesting)

# visualizing the neural network
plot(churnNeuralNet)

# displaying the results from the testing dataset on the console
print(churnProbability$net.result)

# converting probability predictions into 0 or 1 predictions
churnPrediction <- 
  ifelse(churnProbability$net.result > 0.5, 1, 0)

# displaying the predictions on the console
print(churnPrediction)

# evaluating the model by forming a confusion matrix
churnConfusionMatrix <- table(churnTesting$Churn,
                              churnPrediction)

# displaying confusion matrix on the console
print(churnConfusionMatrix)

# calculating model predictive accuracy
predictiveAccuracy <- sum(diag(churnConfusionMatrix)) /
  nrow(churnTesting)

# displaying the predictive accuracy
print(predictiveAccuracy)




