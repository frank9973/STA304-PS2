library(janitor)
library(tidyverse)
library(ggplot2)

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
dataset$age[dataset$age <= 20] <- "0-20"
dataset$age[20.1 <= dataset$age  & dataset$age<= 40] <- "20-40"
dataset$age[40.1 <= dataset$age  & dataset$age<= 60] <- "40-60"
dataset$age[60.1 <= dataset$age  & dataset$age<= 80] <- "60-80"
dataset$age[80.1 <= dataset$age] <- "80-100"

model1 <- lm(feelings_life ~ age + self_rated_health, data = dataset)
model2 <- lm(feelings_life ~ age * self_rated_health, data = dataset)


dataset$feelings_life[dataset$feelings_life == 1] <- "1-2"
dataset$feelings_life[dataset$feelings_life == 2] <- "1-2"
dataset$feelings_life[dataset$feelings_life == 3] <- "3-4"
dataset$feelings_life[dataset$feelings_life == 4] <- "3-4"
dataset$feelings_life[dataset$feelings_life == 5] <- "5-6"
dataset$feelings_life[dataset$feelings_life == 6] <- "5-6"
dataset$feelings_life[dataset$feelings_life == 7] <- "7-8"
dataset$feelings_life[dataset$feelings_life == 8] <- "7-8"
dataset$feelings_life[dataset$feelings_life == 9] <- "9-10"
dataset$feelings_life[dataset$feelings_life == 10] <- "9-10"
#show a box plot for income_respondent and feelings_life
dd <- dataset %>% filter(age == "0-20")
dd <- dd[order(dd$self_rated_health), ]
ggplot(dd, aes(self_rated_health)) + geom_bar(aes(fill = feelings_life))
dd <- dataset %>% filter(age == "20-40")
dd <- dd[order(dd$self_rated_health), ]
ggplot(dd, aes(self_rated_health)) + geom_bar(aes(fill = feelings_life))
dd <- dataset %>% filter(age == "40-60")
dd <- dd[order(dd$self_rated_health), ]
ggplot(dd, aes(self_rated_health)) + geom_bar(aes(fill = feelings_life))
dd <- dataset %>% filter(age == "60-80")
dd <- dd[order(dd$self_rated_health), ]
ggplot(dd, aes(self_rated_health)) + geom_bar(aes(fill = feelings_life))
#irplot <- dataset %>% select(caseid, age, feelings_life)
#boxplot(irplot$feelings_life ~ irplot$age)
#show a box plot for self_rated_health and feelings_life
#srplot <- dataset %>% select(caseid, self_rated_health, feelings_life)
#srplot <-srplot[order(srplot$self_rated_health),]
#boxplot(srplot$feelings_life ~ srplot$self_rated_health)

#summary these models
summary(model1)
summary(model2)
#summary(model3)
#summary(model4）