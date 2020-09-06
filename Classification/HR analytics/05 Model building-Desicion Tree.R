# Model building part 2: Desicion tree
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

# Decision tree model

model.tree <- rpart::rpart(formula = Attrition ~ Age+BusinessTravel+Department+DistanceFromHome+Education+
                             Gender+JobLevel+MaritalStatus+MonthlyIncome+NumCompaniesWorked+YearsAtCompany+
                             JobInvolvement+PerformanceRating+EnvironmentSatisfaction+JobSatisfaction+WorkLifeBalance+
                             AvrWorkHrs, data = train.data, method = 'class')

# Plotting decision tree
rpart.plot::prp(model.tree)

predict_tree <- predict(model.tree, test.data, type = 'class')

confusionMatrix(predict_tree, test.data$Attrition)

# Model 1 + oversampling
#-------------------------------------

over <- ovun.sample(formula = Attrition ~., data = train.data, method = 'over')$data

model.tree <- rpart::rpart(formula = Attrition ~ Age+BusinessTravel+Department+DistanceFromHome+Education+
                             Gender+JobLevel+MaritalStatus+MonthlyIncome+NumCompaniesWorked+YearsAtCompany+
                             JobInvolvement+PerformanceRating+EnvironmentSatisfaction+JobSatisfaction+WorkLifeBalance+
                             AvrWorkHrs, data = over, method = 'class')

# Plotting decision tree
rpart.plot::prp(model.tree)

predict_tree <- predict(model.tree, test.data, type = 'class')

confusionMatrix(predict_tree, test.data$Attrition)