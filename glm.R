# Naina Gupta
# MIS 545 Section 02
# LogisticRegression.R
# installed tidyverse, dummies packages
# install.packages("tidyverse")
# install.packages("dummies")
# install.packages("olsrr")
# install.packages("smotefamily")
# import a dataset of telecom data to predict whether a customer will churn or
# not churn using Logistic regression

library(tidyverse)
library(dummies)
library(corrplot)
library(olsrr)
library(smotefamily)

# set working directory 
setwd("C:/Users/ngnai/OneDrive/Desktop/ECON 511A")
customerChurn <- read_csv("ModelChurn.csv",
                          col_types = "cllllnllllllllllllllllnnl",
                          col_names = TRUE)


# display customerChurn in the console
print(customerChurn)

# display structure in the console
str(customerChurn)                  

# display summary in the console
summary(customerChurn)

# remove customerID from the data
customerChurn <- customerChurn %>% select(-customerID)

# remove NA's from the data and fill in mean value
customerChurn$TotalCharges[is.na(customerChurn$TotalCharges)] <-
 mean(customerChurn$TotalCharges,na.rm=TRUE)

# summary of the customerChurn
summary(customerChurn)


#Recreating displayAllHistograms() function
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
displayAllHistograms(customerChurn)

# display the correlation plot using number method and limit output to
# the bottom left
cor(customerChurn)


# display the correlation plot using number method and limit output to
# the bottom left
corrplot(cor(customerChurn),
         method = "number",
         type ="lower")

# setting random seed
set.seed(777)
# is.numeric(customerChurn$Churn)
# customerChurn$Churn <- lapply(customerChurn$Churn, as.logical)

# putting the records from (75%) into customerChurn1
sampleSet <- sample(nrow(customerChurn),
                    round(nrow(customerChurn)*0.75),
                    replace= FALSE)

# putting the records from (75%) into customerChurn1
customerChurnTraining <- customerChurn[sampleSet,]

# putting the record from (25%) into customerChurn1
customerChurnTesting <- customerChurn[-sampleSet,]

summary(customerChurnTraining$Churn)

classImbalance <- 3872/1410

customerChurnTrainingSmoted <- 
 tibble(SMOTE(X = data.frame(customerChurnTraining),
                                          target = customerChurnTraining$Churn,
                                          dup_size = 3)$data)
summary(customerChurnTrainingSmoted)

# Convert CancelledService and RecentRenewal back into logical types
customerChurnTrainingSmoted <- customerChurnTrainingSmoted %>%
  mutate(Churn = as.logical(Churn),
         Gender=as.logical(Gender),
         SeniorCitizen= as.logical(SeniorCitizen),
         Partner= as.logical(Partner),
         Dependents= as.logical(Dependents),
         PhoneService= as.logical(PhoneService),
         MultipleLines=as.logical(MultipleLines),
         InternetServiceFiberOptic=as.logical(InternetServiceFiberOptic),
         InternetServiceNo=as.logical(InternetServiceNo),
         OnlineSecurity=as.logical(OnlineSecurity),
         OnlineBackup=as.logical(OnlineBackup),
         DeviceProtection=as.logical(DeviceProtection),
         TechSupport= as.logical(TechSupport),
         StreamingTV=as.logical(StreamingTV),
         StreamingMovies=as.logical(StreamingMovies),
         ContractOneYear=as.logical(ContractOneYear),
         ContractTwoYear=as.logical(ContractTwoYear),
         PaperlessBilling=as.logical(PaperlessBilling),
         PaymentMethodElectronicCheck=as.logical(PaymentMethodElectronicCheck),
         PaymentMethodMailedCheck=as.logical(PaymentMethodMailedCheck),
         PaymentMethodCreditCard= as.logical(PaymentMethodCreditCard))
         
         

customerChurnTrainingSmoted <- customerChurnTrainingSmoted %>%
 select(-class)

#is.numeric(customerChurn$Churn)
summary(customerChurnTrainingSmoted)

# Generating the logistic regression model 
churnModel <- glm(data = customerChurnTrainingSmoted,
                  family = binomial,
                  formula = Churn ~.)

# display the logistic regression
summary(churnModel)


#  Calculating the odds ratios 
exp(cbind(OR=coef(churnModel), confint(churnModel)))

# Used model to predict outcomes in the testing dataset and
# and treat anything below or equal to 0.5 as a 0, anything above 0.5 as a 1
predictionModel <- predict(churnModel,
                           customerChurnTesting,
                           type = "response")

# Used model to predict outcomes in the testing dataset and
# and treat anything below or equal to 0.5 as a 0, anything above 0.5 as a 1.
predictionModel <- ifelse(predictionModel >= 0.5,1,0)

print(predictionModel)  

# Generate a confusion matrix of predictions
churnConfusionMatrix <- table(customerChurnTesting$Churn,
                              predictionModel)
print(churnConfusionMatrix)

# Calculate the model prediction accuracy
sum(diag(churnConfusionMatrix))/nrow(customerChurnTesting)




