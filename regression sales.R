library(readxl)
library(MASS)
data <- read_excel("predict.me.xlsx")
str(data)
clean.data <- data
rownames(clean.data)<- data$Year
clean.data <- clean.data[-1,-1]
str(clean.data)
full.model <- lm(Sales~ Hurricanes+ACE+NEW.HOUSE+RMI+LAG+Louisian_Events, data = clean.data)
summary(full.model)
step.model <- stepAIC(full.model, direction = "both")
stuff <- summary(step.model)
stuff$sigma
stuff

predict(step.model, clean.data)
clean.data$Sales
step.model$anova

regression.residuals <- rstandard(step.model)
qqnorm(regression.residuals, 
          ylab="Standardized Residuals", 
       xlab="Normal Scores", 
          main="Sales") 
shapiro.test(regression.residuals)

regression.residuals<- list(regression.residuals)
sqrt(var(regressin.residuals))
regressin.residuals <- as.vector(regression.residuals)
regressin.residuals

simple.regression <-lm(Sales ~ NEW.HOUSE, data  = clean.data)
summary(simple.regression)
simple.regressio
stint <- summary(simple.regression)
stint$sigma
simple.regression$residuals  
