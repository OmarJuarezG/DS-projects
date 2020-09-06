# Model building part 2: Logistic Regression
#-----------------------------------------------

# Model 1
#-------------------------------------
# Setting seed for reproducibilty

set.seed(123)

# Data split

training.sample <- general_data_merged$Attrition %>%
  createDataPartition(p = 0.8, list = FALSE)

train.data <- general_data_merged[training.sample,]
test.data <- general_data_merged[-training.sample,]

mod_c <- glm(formula = Attrition ~ Age+BusinessTravel+Department+DistanceFromHome+Education+
               Gender+JobLevel+MaritalStatus+log(MonthlyIncome)+NumCompaniesWorked+YearsAtCompany+
               JobInvolvement+PerformanceRating+EnvironmentSatisfaction+JobSatisfaction+WorkLifeBalance+
               AvrWorkHrs, data = train.data, family = 'binomial')

mod_c_StepWise <- stepAIC(mod_c, direction = 'both', trace = FALSE)

summary(mod_c_StepWise)

p.training <- predict(mod_c_StepWise, train.data, type = 'response') # vector of probabilites from the training dataset

p.training.attrition <- as.factor(ifelse(p.training > 0.5, 'Yes', 'No')) # transforming those probabilites into factors (Yes or No)

# Obtaining the confusion matrix for this model in our training data

confusionMatrix(p.training.attrition, train.data$Attrition)

# Testing our results in the test dataset with threshold = 0.5

p.test <- predict(mod_c_StepWise, test.data, type = 'response')
p.test.attrition <- as.factor(ifelse(p.test > 0.5, 'Yes','No'))
confusionMatrix(p.test.attrition, test.data$Attrition)

# Select different threshold

# Selecting a different treshold based on ROC curve

ROCR_prediction <- prediction(p.training, train.data$Attrition)
ROCR_performance <- performance(ROCR_prediction, 'tpr', 'fpr')

plot(ROCR_performance, colorize = TRUE, print.cutoffs.at = seq(0.1, by = 0.1))

# Based on this we select the new threshold to be 0.2 and test our model again.

# Model 1 + new threshold
#-------------------------------------

p.training <- predict(mod_c_StepWise, train.data, type = 'response')
p.training.attrition <- as.factor(ifelse(p.training > 0.2, 'Yes', 'No'))
confusionMatrix(p.training.attrition, train.data$Attrition)

# Evaluating in test dataset

p.test <- predict(mod_c_StepWise, test.data, type = 'response')
p.test.attrition <-  as.factor(ifelse(p.test> 0.2, 'Yes', 'No'))

confusionMatrix(p.test.attrition, test.data$Attrition)

# Model 1 + oversampling
#-------------------------------------

# Using oversampling in the training dataset
over <- ovun.sample(formula = Attrition ~., data = train.data, method = 'over')$data

table(over$Attrition) # now the data is balanced between No and Yes

# Building the model

balanced_model <- glm(formula = Attrition ~ Age+BusinessTravel+Department+DistanceFromHome+Education+
                        Gender+JobLevel+MaritalStatus+log(MonthlyIncome)+NumCompaniesWorked+YearsAtCompany+
                        JobInvolvement+PerformanceRating+EnvironmentSatisfaction+JobSatisfaction+WorkLifeBalance+
                        AvrWorkHrs, data = over, family = 'binomial')

# Model output

summary(balanced_model)

# Step wise

balanced_model_step_wise <- stepAIC(balanced_model, direction = 'both', trace = FALSE)

# Step wise model output

summary(balanced_model_step_wise)

p.training <- predict(balanced_model_step_wise, train.data, type = 'response')

p.training.attrition <- as.factor(ifelse(p.training > 0.5, 'Yes', 'No'))

confusionMatrix(p.training.attrition, train.data$Attrition)

# testing dataset

p.test <- predict(balanced_model_step_wise, test.data, type = 'response')

p.test.attrition <- as.factor(ifelse(p.test > 0.5, 'Yes', 'No'))

confusionMatrix(p.test.attrition, test.data$Attrition)
