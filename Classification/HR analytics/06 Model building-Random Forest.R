# Model building part 2: Random forest
#-----------------------------------------------

# Model 1
#-------------------------------------

set.seed(123)

partition <- createDataPartition(general_data_merged$Attrition, p = 0.8, list = FALSE)

training <- general_data_merged[partition,]
test <- general_data_merged[-partition,]

# Random forest with cross-validation
model_rf_1 <- randomForest::randomForest(Attrition ~ Age+BusinessTravel+Department+DistanceFromHome+Education+
                                           Gender+JobLevel+MaritalStatus+MonthlyIncome+NumCompaniesWorked+YearsAtCompany+
                                           JobInvolvement+PerformanceRating+EnvironmentSatisfaction+JobSatisfaction+WorkLifeBalance+
                                           AvrWorkHrs, data = training, trControl = trainControl(method = 'cv', 10))

# Printing the model
model_rf_1

# Evaluation of the model with useen data
p <- predict(model_rf_1, test, type = 'response')

confusionMatrix(p, test$Attrition)

# Plotting error rate
plot(model_rf_1, main = 'Error rate vs number of trees')

# Plotting mtry
t <- randomForest::tuneRF(training[,-2], training[,2], stepFactor = 0.5, 
                          plot = TRUE, ntreeTry = 100, trace = TRUE, improve = 0.05)

# 100 trees should be fine

model_rf_2 <- randomForest::randomForest(Attrition ~ Age+BusinessTravel+Department+DistanceFromHome+Education+
                                           Gender+JobLevel+MaritalStatus+MonthlyIncome+NumCompaniesWorked+YearsAtCompany+
                                           JobInvolvement+PerformanceRating+EnvironmentSatisfaction+JobSatisfaction+WorkLifeBalance+
                                           AvrWorkHrs, data = training, ntree = 100, trControl = trainControl(method = 'cv', 10))

model_rf_2

p <- predict(model_rf_2, test, type = 'response')

confusionMatrix(p, test$Attrition)

# number of nodes

hist(randomForest::treesize(model_rf_2), main = 'No. of nodes', xlab = '', col = 'forestgreen')

# feature importance

randomForest::varImpPlot(model_rf_2, main = 'Feature Importance')

# dependencies

randomForest::partialPlot(model_rf_2, training, AvrWorkHrs, 'Yes')

# When the Average working hours is more than 7 it tends to predict Attrition ('Yes')

randomForest::partialPlot(model_rf_2, training, Age, 'Yes')

# In this case the model is confused. When is more than 40 it tends to predict 'Yes' but also 
# when the range is less than 40. This could be because of the outliers.