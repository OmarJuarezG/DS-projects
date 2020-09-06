# Model building: Part 1
#------------------------

# Based on the findings in EDA we compute the first model based relevant features.

mod_a <- glm(formula = Attrition ~ Age+BusinessTravel+Department+DistanceFromHome+Education+
               Gender+JobLevel+MaritalStatus+MonthlyIncome+NumCompaniesWorked+YearsAtCompany+
               JobInvolvement+PerformanceRating+EnvironmentSatisfaction+JobSatisfaction+WorkLifeBalance+
               AvrWorkHrs, data = general_data_merged, family = 'binomial')

summary(mod_a)

vif(mod = mod_a)

# Using Akaike information critera to drop some features

mod_b <- stepAIC(mod_a, direction = 'both', trace = FALSE)

summary(mod_b)

vif(mod = mod_b)

# Model evaluation using all observations

predicted_data <- data.frame(prob_of_attrition = mod_b$fitted.values, attrition = general_data_merged$Attrition)

predicted_data <- predicted_data %>%
  arrange(prob_of_attrition) %>%
  mutate(rank = 1:nrow(predicted_data))

ggplot(data = predicted_data, aes(x = rank, y = prob_of_attrition))+
  geom_point(aes(color = attrition), alpha = 1, shape = 4, stroke = 2)+
  labs(y = 'Predicted probability of attrition', x = 'Index', title = 'Probability of attrition vs Actual attrition')

