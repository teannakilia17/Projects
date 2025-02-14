## Assignment 1
#Load libraries--------------
library(car) #VIF
library(MASS) #StepAIC

#Import data-----------
expenditure <-read.table("expenditure.txt")

#Creating a vector of names for readability
names(expenditure)<- c("general", "mental", "bmi", "income", "age",
                       "gender", "ethnicity", "education", "region", "hypertension",
                       "hyperlipidemia", "dvisit", "ndvisit", "dvexpend", "ndvexpend")

#Question 1----------------------
#Let us start by fitting our model of choice, the multiple linear regression, using
##expenditure on doc visits as our dependent variable and a subset of regressors from our data set
lm1.fit <- lm(dvexpend ~ general + mental + bmi + income + age + gender +
                education + hypertension + hyperlipidemia + 
                dvisit, data = expenditure) 
vif(lm1.fit)

#Question 2----------------------
#Here we obtain a summary so we can perform statistical analysis 
summary(lm1.fit) 

par(mfrow = c(2,2))
plot(lm1.fit)


#Appendix----------------------
#Forward Selection
lm2.fit <- lm(dvexpend ~1, data=expenditure)
stepAIC(lm2.fit,~.)
stepAIC(lm2.fit, ~dvexpend + general + mental + bmi + income + age + gender +
          education + hypertension + hyperlipidemia + 
          dvisit, data = expenditure)

#According to forward selection, the model include these variables: 
lm3.fit <- lm(dvexpend ~ dvisit + income + general + mental + 
                gender + bmi, data=expenditure) 
summary(lm3.fit)
par(mfrow = c(2,2))
plot(lm3.fit) #similar to lm1.fit

#Residual plot
residuals <- residuals(lm1.fit)
observation_order <- seq(1, length(residuals)) #we wish to check independence of residuals so we plot against obs order
plot(observation_order, residuals, 
     xlab = "Observation Order", 
     ylab = "Residuals", 
     main = "Residuals vs Observation Order",
     pch = 16, # to use solid dots for residuals
     col = "black") #black for the points