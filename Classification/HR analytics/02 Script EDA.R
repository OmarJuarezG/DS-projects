# Exploratory data analysisi (EDA)
#---------------------------------------

# First we keep the numeric variables

nums <- unlist(lapply(general_data_merged, is.numeric))
general_data_merged_with_ordinal_values <- general_data_merged[, nums]

# With this data we create a correlation matrix in order to see the relantionship 
# between the features. Since the data contains ordinal data (hierarchy or ranks) we use Spearman correlation 
# instead of Pearson.

cor_matrix <- cor(general_data_merged_with_ordinal_values, method = 'spearman')
corrplot(corr = cor_matrix, method = 'color', addCoef.col = 'gray',tl.cex = 0.7, number.cex = 0.5)

# We create a second dataframe with the labels of the ordinal features

general_data_merged_with_categories <- general_data_merged

general_data_merged_with_categories$Education[which(general_data_merged_with_categories$Education == 1)] <- 'Below College'
general_data_merged_with_categories$Education[which(general_data_merged_with_categories$Education == 2)] <- 'College'
general_data_merged_with_categories$Education[which(general_data_merged_with_categories$Education == 3)] <- 'Bachelor'
general_data_merged_with_categories$Education[which(general_data_merged_with_categories$Education == 4)] <- 'Master'
general_data_merged_with_categories$Education[which(general_data_merged_with_categories$Education == 5)] <- 'Doctor'

general_data_merged_with_categories$EnvironmentSatisfaction[which(general_data_merged_with_categories$EnvironmentSatisfaction == 1)] <- 'Low'
general_data_merged_with_categories$EnvironmentSatisfaction[which(general_data_merged_with_categories$EnvironmentSatisfaction == 2)] <- 'Medium'
general_data_merged_with_categories$EnvironmentSatisfaction[which(general_data_merged_with_categories$EnvironmentSatisfaction == 3)] <- 'High'
general_data_merged_with_categories$EnvironmentSatisfaction[which(general_data_merged_with_categories$EnvironmentSatisfaction == 4)] <- 'Very High'

general_data_merged_with_categories$JobInvolvement[which(general_data_merged_with_categories$JobInvolvement == 1)] <- 'Low'
general_data_merged_with_categories$JobInvolvement[which(general_data_merged_with_categories$JobInvolvement == 2)] <- 'Medium'
general_data_merged_with_categories$JobInvolvement[which(general_data_merged_with_categories$JobInvolvement == 3)] <- 'High'
general_data_merged_with_categories$JobInvolvement[which(general_data_merged_with_categories$JobInvolvement == 4)] <- 'Very High'

general_data_merged_with_categories$JobSatisfaction[which(general_data_merged_with_categories$JobSatisfaction == 1)] <- 'Low'
general_data_merged_with_categories$JobSatisfaction[which(general_data_merged_with_categories$JobSatisfaction == 2)] <- 'Medium'
general_data_merged_with_categories$JobSatisfaction[which(general_data_merged_with_categories$JobSatisfaction == 3)] <- 'High'
general_data_merged_with_categories$JobSatisfaction[which(general_data_merged_with_categories$JobSatisfaction == 4)] <- 'Very High'

general_data_merged_with_categories$PerformanceRating[which(general_data_merged_with_categories$PerformanceRating == 1)] <- 'Low'
general_data_merged_with_categories$PerformanceRating[which(general_data_merged_with_categories$PerformanceRating == 2)] <- 'Good'
general_data_merged_with_categories$PerformanceRating[which(general_data_merged_with_categories$PerformanceRating == 3)] <- 'Excellent'
general_data_merged_with_categories$PerformanceRating[which(general_data_merged_with_categories$PerformanceRating == 4)] <- 'Outstanding'

general_data_merged_with_categories$WorkLifeBalance[which(general_data_merged_with_categories$WorkLifeBalance == 1)] <- 'Bad'
general_data_merged_with_categories$WorkLifeBalance[which(general_data_merged_with_categories$WorkLifeBalance == 2)] <- 'Good'
general_data_merged_with_categories$WorkLifeBalance[which(general_data_merged_with_categories$WorkLifeBalance == 3)] <- 'Better'
general_data_merged_with_categories$WorkLifeBalance[which(general_data_merged_with_categories$WorkLifeBalance == 4)] <- 'Best'

# Pivot tables

general_data_merged_with_categories %>%
  group_by(Gender, Education) %>%
  summarize(Average_Income = mean(MonthlyIncome)) %>%
  arrange(-Average_Income)

general_data_merged_with_categories %>%
  group_by(MaritalStatus, WorkLifeBalance) %>%
  summarize(Average_Work = mean(AvrWorkHrs), Percent_employees = round(n()/nrow(general_data_merged_with_categories)*100,0)) %>%
  arrange(-Percent_employees)

# Plots: part 1

# Creating some plots to see the rate of attrition by other variables such as Education, Environment Satisfaction, 
# Job Involvement, Job satisfaction, Performance rating, work balance

g1 <- ggplot(data = general_data_merged_with_categories, aes(x = factor(Attrition), group = Education)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = 'count', position = 'stack') +
  facet_grid(~Education) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(round((..prop..),2)), y = ..prop..), stat = 'count', vjust = -.2, size = 3) +
  labs(y = 'Percent', x = 'Attrition', title = 'Attrition by Education', fill = 'Attrition')+
  theme_light() +
  theme(legend.position = 'none')

g2 <- ggplot(data = general_data_merged_with_categories, aes(x = factor(Attrition), group = EnvironmentSatisfaction)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = 'count', position = 'stack') +
  facet_grid(~EnvironmentSatisfaction) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(round((..prop..),2)), y = ..prop..), stat = 'count', vjust = -.2, size = 3) +
  labs(y = 'Percent', x = 'Attrition', title = 'Environment satisfaction vs Attrition', fill = 'Attrition')+
  theme_light() +
  theme(legend.position = 'none')

g3 <- ggplot(data = general_data_merged_with_categories, aes(x = factor(Attrition), group = JobInvolvement)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = 'count', position = 'stack') +
  facet_grid(~JobInvolvement) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(round((..prop..),2)), y = ..prop..), stat = 'count', vjust = -.2, size = 3) +
  labs(y = 'Percent', x = 'Attrition', title = 'Job involvement vs Attrition', fill = 'Attrition')+
  theme_light() +
  theme(legend.position = 'none')

g4 <- ggplot(data = general_data_merged_with_categories, aes(x = factor(Attrition), group = JobSatisfaction)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = 'count', position = 'stack') +
  facet_grid(~JobSatisfaction) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(round((..prop..),2)), y = ..prop..), stat = 'count', vjust = -.2, size = 3) +
  labs(y = 'Percent', x = 'Attrition', title = 'Job Satisfaction vs Attrition', fill = 'Attrition')+
  theme_light() +
  theme(legend.position = 'none')

g5 <- ggplot(data = general_data_merged_with_categories, aes(x = factor(Attrition), group = PerformanceRating)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = 'count', position = 'stack') +
  facet_grid(~PerformanceRating) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(round((..prop..),2)), y = ..prop..), stat = 'count', vjust = -.2, size = 3) +
  labs(y = 'Percent', x = 'Attrition', title = 'Performance Rating vs Attrition', fill = 'Attrition')+
  theme_light() +
  theme(legend.position = 'none')

g6 <- ggplot(data = general_data_merged_with_categories, aes(x = factor(Attrition), group = WorkLifeBalance)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = 'count', position = 'stack') +
  facet_grid(~WorkLifeBalance) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(round((..prop..),2)), y = ..prop..), stat = 'count', vjust = -.2, size = 3) +
  labs(y = 'Percent', x = 'Attrition', title = 'Work-life balance vs Attrition', fill = 'Attrition')+
  theme_light()	+
  theme(legend.position = 'none')

plot_grid(g1,g2,g3,g4,g5,g6, nrow = 3)

# Plots: part 2

# Plots of Attrition vs Age, Average working hours, Monthly income, Gender, Marital status, and Department.

g7 <-  ggplot(data = general_data_merged_with_categories, aes(x = factor(Attrition), y = Age, fill = Attrition)) +
  geom_boxplot() +
  xlab('')+
  theme(legend.position = 'none')

g8 <- ggplot(data = general_data_merged_with_categories, aes(x = factor(Attrition), y = AvrWorkHrs, fill = Attrition)) +
  geom_boxplot()+
  xlab('')+
  theme(legend.position = 'none')

g9 <- ggplot(data = general_data_merged_with_categories, aes(x = factor(Attrition), y = log(MonthlyIncome), fill = Attrition)) +
  geom_boxplot()+
  xlab('')+
  ylab('Log of Monthly Income')+
  theme(legend.position = 'none')

g10 <- ggplot(data = general_data_merged_with_categories, aes(x = factor(Attrition), group = Gender)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = 'count', position = 'stack') +
  facet_grid(~Gender) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(round((..prop..),2)), y = ..prop..), stat = 'count', vjust = -.2, size = 3) +
  labs(y = 'Percent', x = 'Attrition', title = 'Gender vs Attrition', fill = 'Attrition')+
  theme_light()	+
  theme(legend.position = 'none')

g11 <- ggplot(data = general_data_merged_with_categories, aes(x = factor(Attrition), group = MaritalStatus)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = 'count', position = 'stack') +
  facet_grid(~MaritalStatus) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(round((..prop..),2)), y = ..prop..), stat = 'count', vjust = -.2, size = 3) +
  labs(y = 'Percent', x = 'Attrition', title = 'Marital Status vs Attrition', fill = 'Attrition')+
  theme_light()	+
  theme(legend.position = 'none')

g12 <- ggplot(data = general_data_merged_with_categories, aes(x = factor(Attrition), group = Department)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = 'count', position = 'stack') +
  facet_grid(~Department) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(round((..prop..),2)), y = ..prop..), stat = 'count', vjust = -.2, size = 3) +
  labs(y = 'Percent', x = 'Attrition', title = 'Department vs Attrition', fill = 'Attrition')+
  theme_light()	+
  theme(legend.position = 'none')

plot_grid(g7,g8,g9,g10,g11,g12, nrow = 3)