# Libraries
#------------------------------------

library(plyr)
library(dplyr)
library(ggplot2)
library(caret)
library(car)
library(cowplot)
library(gridExtra)
library(corrplot)
library(MASS)
library(ROCR)
library(ROSE)
library(rpart)
library(rpart.plot)

# Reading files
#------------------------------------

general_data_raw <- read.csv('D:/DS_PROJECTS/HR Analytics Case Study/datasets_42363_72602_general_data.csv', 
                             header = TRUE, sep = ',')

survey_data_raw <- read.csv('D:/DS_PROJECTS/HR Analytics Case Study/datasets_42363_72602_employee_survey_data.csv',
                            header = TRUE,sep = ',')

manager_survey_data_raw <- read.csv('D:/DS_PROJECTS/HR Analytics Case Study/datasets_42363_72602_manager_survey_data.csv',
                                    header = TRUE, sep = ',')

in_time_raw <- read.csv('D:/DS_PROJECTS/HR Analytics Case Study/in_time.csv',
                        header = TRUE, sep = ',')

out_time_raw <- read.csv('D:/DS_PROJECTS/HR Analytics Case Study/out_time.csv',
                         header = TRUE, sep = ',')

# Exploring the dataframes

str(survey_data_raw)

str(manager_survey_data_raw)

str(general_data_raw)

#str(in_time_raw)

#str(out_time_raw)

# Checking for missing values
#------------------------------------

sapply(survey_data_raw, function(x) sum(is.na(x)))/nrow(survey_data_raw)*100
sapply(manager_survey_data_raw, function(x) sum(is.na(x)))/nrow(manager_survey_data_raw)*100
sapply(general_data_raw, function(x) sum(is.na(x)))/nrow(general_data_raw)*100

# Data treatment
#------------------------------------

in_time <- sapply(in_time_raw, function(x) as.POSIXlt(x, origin = '1970-01-01', '%y-%m-%d %H:%M:%S'))
in_time <- as.data.frame(in_time)

out_time <- sapply(out_time_raw, function(x) as.POSIXlt(x, origin = '1970-01-01', '%y-%m-%d %H:%M:%S'))
out_time <- as.data.frame(out_time)

# Removing the first column since it has no value in the data

in_time <- in_time[,c(2:ncol(in_time))]

out_time <- out_time[,c(2:ncol(out_time))]

# Computing difference between out_time and in_time

hours_worked_raw <- out_time - in_time

# We want to keep the records even if there was one employee that went to work

hours_worked_raw <- hours_worked_raw[,colSums(is.na(hours_worked_raw)) < nrow(hours_worked_raw)]

# Transforming the resuls to numeric so we can take the average

hours_worked_raw <- sapply(hours_worked_raw, function(x) as.numeric(x))

hours_worked_raw <- as.data.frame(hours_worked_raw)

# Taking the average of working hours per employee
AvrWorkHrs <- apply(hours_worked_raw, 1, mean, na.rm = TRUE) # 1 Is to indicate rows

# Creating ID column so it can be merged with the rest of the datasets

id_vector <- 1:nrow(hours_worked_raw)

AverageWorkedHours <- data.frame(EmployeeID = id_vector, AvrWorkHrs = AvrWorkHrs)

# Before merging we take a look if each observation is from a different employee

setdiff(survey_data_raw$EmployeeID, manager_survey_data_raw$EmployeeID)
setdiff(manager_survey_data_raw$EmployeeID, general_data_raw$EmployeeID)
setdiff(general_data_raw$EmployeeID, AverageWorkedHours$EmployeeID)

# Since all of them are complete we can merge them

general_data_merged <- inner_join(general_data_raw,manager_survey_data_raw, by = 'EmployeeID') %>%
  inner_join(., survey_data_raw, by = 'EmployeeID') %>%
  inner_join(., AverageWorkedHours, by = 'EmployeeID')

# Droping values that have the same value for all observations

same_values <- nearZeroVar(general_data_merged, names = TRUE)
general_data_merged <- general_data_merged %>%
  dplyr::select(-c(c('EmployeeID', same_values)))

# Now we check the structure of the new dataframe and the missing values. And evaluate if they are 
#significant in terms of the total observations

str(general_data_merged)

sapply(general_data_merged, function(x) sum(is.na(x)))/nrow(general_data_merged)*100

# We have a very small proportion of missing values per feature. We translate those in terms of observations

flag_nulls <- ifelse(rowSums(is.na(general_data_merged)) == 0, 0, 1)
sum(flag_nulls)/nrow(general_data_merged)*100 

# Since missing values represent 2.5% of the observations we remove them (just for this case, one should 
#be careful when dropping missing values).

general_data_merged <- general_data_merged[rowSums(is.na(general_data_merged)) == 0,]


