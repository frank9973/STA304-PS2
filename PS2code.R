library(janitor)
library(tidyverse)

# Load the data dictionary and the raw data and correct the variable names
raw_data <- read_csv("gss.csv")
dataset <- raw_data %>% select(caseid, age, total_children, sex, feelings_life, marital_status, average_hours_worked, income_respondent, 
                            self_rated_health)
dataset$self_rated_health <- as.character(dataset$self_rated_health)
dataset$self_rated_health[dataset$self_rated_health == "Don't know"] <- 0
dataset$self_rated_health[dataset$self_rated_health == "Poor"] <- 1
dataset$self_rated_health[dataset$self_rated_health == "Fair"] <- 2
dataset$self_rated_health[dataset$self_rated_health == "Good"] <- 3
dataset$self_rated_health[dataset$self_rated_health == "Very good"] <- 4
dataset$self_rated_health[dataset$self_rated_health == "Excellent"] <- 5
#dataset$income_respondent <- as.character(data$income_respondent)
#dataset$income_respondent[dataset$income_respondent == "Less than $25,000"] <- 25000
#dataset$income_respondent[dataset$income_respondent == "$25,000 to $49,999"] <- 49999
#dataset$income_respondent[dataset$income_respondent == "$50,000 to $74,999"] <- 74999
#dataset$income_respondent[dataset$income_respondent == "$75,000 to $99,999"] <- 99999
#dataset$income_respondent[dataset$income_respondent == "$125,000 and more"] <- 125000
#create a multiple linear model (income_respondent and feelings_life)
model1 <- lm(feelings_life ~ age + self_rated_health, data = dataset)
model2 <- lm(feelings_life ~ age * self_rated_health, data = dataset)
#show a box plot for income_respondent and feelings_life
dataset$age[dataset$age <= 20] <- "0-20"
dataset$age[20.1 <= dataset$age  & dataset$age<= 40] <- "20-40"
dataset$age[40.1 <= dataset$age  & dataset$age<= 60] <- "40-60"
dataset$age[60.1 <= dataset$age  & dataset$age<= 80] <- "60-80"
dataset$age[80.1 <= dataset$age] <- "80-100"
irplot <- dataset %>% select(caseid, age, feelings_life)
boxplot(irplot$feelings_life ~ irplot$age)
#show a box plot for self_rated_health and feelings_life
srplot <- dataset %>% select(caseid, self_rated_health, feelings_life)
srplot <-srplot[order(srplot$self_rated_health),]
boxplot(srplot$feelings_life ~ srplot$self_rated_health)

#dataset$average_hours_worked <- as.character(dataset$average_hours_worked)
#dataset$average_hours_worked[dataset$average_hours_worked == "0.1 to 29.9 hours"] <- 0.1
#dataset$average_hours_worked[dataset$average_hours_worked == "30.0 to 40.0 hours"] <- 30
#dataset$average_hours_worked[dataset$average_hours_worked == "40.1 to 50.0 hours"] <- 40
#dataset$average_hours_worked[dataset$average_hours_worked == "50.1 hours and more"] <- 50

#create a single linear model (total_children and feelings_life)
#model1 <- lm(feelings_life ~ total_children, data = dataset)
#create a multiple linear model (average_hours_worked and feelings_life)
#model2 <- lm(feelings_life ~ average_hours_worked, data = dataset)
#show a box plot for total_children and feelings_life
#tcplot <- dataset %>% select(caseid, total_children, feelings_life)
#tcplot <-tcplot[order(tcplot$total_children),]
#boxplot(tcplot$feelings_life ~ tcplot$total_children)
#show a box plot for average_hours_worked and feelings_life
#ahplot <- dataset %>% select(caseid, average_hours_worked, feelings_life)
#ahplot <-ahplot[order(ahplot$average_hours_worked),]
#boxplot(ahplot$feelings_life ~ ahplot$average_hours_worked)

#create a multiple linear model (income_respondent and feelings_life)
#model3 <- lm(feelings_life ~ income_respondent + self_rated_health, data = dataset)
#create a multiple linear model (self_rated_health and feelings_life)
#model4 <- lm(feelings_life ~ self_rated_health, data = dataset)
#show a box plot for income_respondent and feelings_life
#irplot <- dataset %>% select(caseid, income_respondent, feelings_life)
#irplot <-irplot[order(irplot$income_respondent),]
#boxplot(irplot$feelings_life ~ irplot$income_respondent)
#show a box plot for self_rated_health and feelings_life
#srplot <- dataset %>% select(caseid, self_rated_health, feelings_life)
#srplot <-srplot[order(srplot$self_rated_health),]
#boxplot(srplot$feelings_life ~ srplot$self_rated_health)

#dataset$marital_status <- as.character(data$marital_status)
#dataset$marital_status[dataset$marital_status == "Less than $25,000"] <- 25000
#dataset$marital_status[dataset$marital_status == "$25,000 to $49,999"] <- 49999
#dataset$marital_status[dataset$marital_status == "$50,000 to $74,999"] <- 74999
#dataset$marital_status[dataset$marital_status == "$75,000 to $99,999"] <- 99999
#dataset$marital_status[dataset$marital_status == "$125,000 and more"] <- 125000

#create a multiple linear model (marital_status)
#model5 <- lm(feelings_life ~ marital_status, data = dataset)
#show a box plot for marital_status and feelings_life
#msplot <- dataset %>% select(caseid, marital_status, feelings_life)
#msplot <-msplot[order(msplot$marital_status),]
#boxplot(msplot$feelings_life ~ msplot$marital_status)

#summary these models
summary(model1)
summary(model2)
#summary(model3)
#summary(model4）