insurance=read.csv("insurance.csv", stringsAsFactors = TRUE)
str(insurance)
summary(insurance$expenses)
hist(insurance$expenses)
cor(insurance[c("age", "bmi", "children", "expenses")])
pairs(insurance[c("age", "bmi", "children", "expenses")])
install.packages("psych")
library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "expenses")])

install.packages("stats")
library(stats)
ins_model=lm(expenses ~ age + children + bmi + sex +smoker + region, data = insurance)
ins_model

summary(ins_model)


insurance$age2=insurance$age^2
insurance$bmi30= ifelse(insurance$bmi >= 30, 1, 0)
ins_model2=lm(expenses ~ age + age2 + children + bmi + sex +bmi30*smoker + region, data = insurance)
summary(ins_model2)