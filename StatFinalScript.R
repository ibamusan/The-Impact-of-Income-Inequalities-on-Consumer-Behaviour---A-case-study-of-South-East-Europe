# Calling libraries and reading data #####
library(haven)
library(dplyr)
library(purrr)
library(readxl)  
library(ggplot2)
library(skimr)
library(broom)
library(stargazer)

setwd("C:/Users/USER/Desktop/StatGrpAss")
data <- read_dta("grpdataset.dta")

read_excel("pnsedata.xlsx")

summary(data)

skim(data)
#### Data Restriction and Transformation####

#check for NA values.
data %>%
  map(~sum(is.na(.)))

#Variavle selections
Main_data <- data %>% 
  select(P425r, A093r, A094r, A121r, A049r, Gorx, income, expenditure) %>%
  mutate(Income_level = case_when(income >= 1 & income <= 350 ~ "Low", income >350 & income <= 600 ~ "Middle", TRUE ~ "High"), Expenditure = ceiling(expenditure)) %>% 
  rename(SHH_Income = P425r, Work_pattern = A093r, Job_position = A094r, House_type = A121r, Region = Gorx, HH_size = A049r) %>% 
  filter(Region == 8, Work_pattern != 4) %>% 
  mutate(House_type =  case_when(House_type >= 1 &  House_type <= 2 ~ "Rented", TRUE ~ "Owned"), SHH_Income =  case_when(SHH_Income == 1 ~ "Earned", TRUE ~ "Other"), Job_position = case_when(
    Job_position == 1 ~ "Higher_managerial",
    Job_position == 2 ~ "Intermediate_occupation",
    Job_position == 3 ~ "Manual_occupations",
    TRUE ~ "Never_worked"
  ))

summary(Main_data)

#Check for NA values
Main_data%>%
  map(~sum(is.na(.)))

#variable called cid was created
Main_data <- Main_data %>% 
  mutate(cid = row_number())

#Define each variable 
Main_data$SHH_Income <- as.numeric(ifelse(Main_data$SHH_Income == "Earned",1,0))
Main_data$House_type <- as.factor(ifelse(Main_data$House_type == "Rented",1,0))
Main_data$Work_pattern <- as.factor(Main_data$Work_pattern)
Main_data$Job_position <- as.factor(Main_data$Job_position)
Main_data$HH_size <- as.factor(Main_data$HH_size)
Main_data$Income_level <- as.factor(Main_data$Income_level)
Main_data$Expenditure <- as.numeric(Main_data$Expenditure)

table(Main_data$SHH_Income)
table(Main_data$House_type)
### Visualization ####
#plot showing relationship between income level and expenditure.
ggplot(Main_data, aes(Income_level, Expenditure)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(x = "Income Level", y = "Expenditure") +
  labs(title = "Expenditure by Income Level")

#plot showing relationship between Source of Income and expenditure.
ggplot(Main_data, aes(factor(SHH_Income), Expenditure)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(x = "Income Level", y = "Expenditure") +
  labs(title = "Expenditure by Income Level") +
  scale_x_discrete(labels = c("Earned Income", "Other Income"))

#plot showing relationship between source of income and expenditure.
ggplot(Main_data, aes(Income_level, Expenditure, fill = factor(SHH_Income))) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(x = "Income Level", y = "Expenditure") +
  labs(title = "Expenditure by Source of Income") +
  scale_fill_discrete(name = "Source of Income", labels = c("Earned", "Other"))

#plot showing relationship between source of income and expenditure.
ggplot(Main_data, aes(Income_level, Expenditure, fill = factor(SHH_Income))) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(x = "Income Level", y = "Expenditure") +
  labs(title = "Expenditure by Source of Income") +
  scale_fill_discrete(name = "Source of Income", labels = c("Earned", "Other"))

#plot showing relationship between Work-pattern and expenditure.
ggplot(Main_data, aes(Income_level, Expenditure, fill = factor(Work_pattern))) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(x = "Income Level", y = "Expenditure") +
  labs(title = "Expenditure by Source of Income") +
  scale_fill_discrete(name = "Work Pattern", labels = c("Full-time", "Part-time", "Unemployed"))

#Regression Analysis ####

# This build the regression model for model 2 using both household and work related variables.
model1 <- lm(Expenditure ~ Income_level + SHH_Income + factor(Work_pattern) + factor(Job_position) + House_type + factor(HH_size), data = Main_data)

summary(model1)

# This build the regression model for model 2 using work related variables.
model2 <- lm(Expenditure ~ Income_level + factor(Work_pattern) + factor(Job_position), data = Main_data)

summary(model2)

# This build the regression model for model 3 using household related variables.
model3 <- lm(Expenditure ~ Income_level + SHH_Income + House_type + factor(HH_size), data = Main_data)

summary(model3)

#Prediction analysis for Model 1####

# Set the random seed
set.seed(123)

# Split the data into training and testing sets
train <- Main_data %>% sample_frac(0.75)
test  <- anti_join(Main_data, train, by = "cid")

# Build the linear regression model
model1 <- lm(data = train, Expenditure ~  + Income_level + factor(Work_pattern) + factor(Job_position) + House_type + factor(HH_size))

summary(model1)

# This generate residuals for training data
train <- augment(model1, train)

ggplot(data = train) + geom_point(aes(y = .fitted, x = Income_level), alpha = 0.1) + 
  geom_point(aes(y = Expenditure, x = Income_level), alpha = 0.1, color = "red") +
  theme_minimal()

# This generate residuals for training data
test <- augment(model1, newdata = test)

# This generate RMSE value for both tarin and test model
test %>% summarise(RMSE = sqrt(mean(.resid^2))) #this has RMSE of 253
train %>% summarise(RMSE = sqrt(mean(.resid^2)))

summary(train$Expenditure)


# Predictive Analysis for Model 2 ####

#this solve the problem of random selection in splitting the data
set.seed(123) 

# Split the data into training and testing sets
train2a <- Main_data %>% sample_frac(0.75)
test2a  <- anti_join(Main_data, train2a, by = "cid")

# Build the linear regression model
model2 <- lm(data = train2a, Expenditure ~ Income_level + factor(Work_pattern) + factor(Job_position))

summary(model2)

# This generate residuals for training data
train2 <- augment(model2, train2a)

ggplot(data = train2) + geom_point(aes(y = .fitted, x = Income_level), alpha = 0.1) + 
  geom_point(aes(y = Expenditure, x = Income_level), alpha = 0.1, color = "red") +
  theme_minimal()

#This generate residuals for testing data
test2 <- augment(model2, newdata = test2a)

# This generate RMSE value for both tarin and test model
test2 %>% summarise(RMSE = sqrt(mean(.resid^2))) #this has RMSE of 247
train2 %>% summarise(RMSE = sqrt(mean(.resid^2)))

summary(train$Expenditure)

# predictive analysis for Model3 #####

# This Set the random seed 
set.seed(123)

# this Split the data into training and testing sets
train3 <- Main_data %>% sample_frac(0.75)
test3  <- anti_join(Main_data, train3, by = "cid")

# This Build the linear regression model
model3 <- lm(data = train3, Expenditure ~ Income_level + SHH_Income + House_type + factor(HH_size))

summary(model3)

# This generate residuals for training data
train3 <- augment(model3, train3)

ggplot(data = train3) + geom_point(aes(y = .fitted, x = Income_level), alpha = 0.1) + 
  geom_point(aes(y = Expenditure, x = Income_level), alpha = 0.1, color = "red") +
  theme_minimal()

# This generate residuals for testing data
test3 <- augment(model3, newdata = test3)

# This generate RMSE value for both tarin and test model
test3 %>% summarise(RMSE = sqrt(mean(.resid^2)))
train3 %>% summarise(RMSE = sqrt(mean(.resid^2)))


