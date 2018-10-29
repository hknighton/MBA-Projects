
####
library(nnet)
library(NeuralNetTools)
library(e1071)
library(arules)
library(kernlab)
library(randomForest)
library(tree)
library(stats)
library(ROCR)

data <- read.csv("Kickstarter.original.csv", header = T)
set.seed(1)
data_less <- sample(rownames(data), dim(data)[1]*.2)
str(data)
data.update <- data[data_less,-13]
str(data)
table(data$state)
### correct state to  be a binary variable
data.update$state <- as.character(data.update$state)
data.update$state[data.update$state == "canceled"] <- "failed"
data.update$state[data.update$state == "suspended"] <- "failed"
table(data.update$state)
levels(data.update$state)
class(data.update$state)
#### drop the live 
live_drop <- which(data.update$state == "live")
data.update <- data.update[-live_drop,]
table(data.update$state)
class(data.update$state)
data.update$state <- as.factor(data.update$state)
levels(data.update$state)
### check data 

str(data.update$state)
table(data.update$category)
str(data.update$main_category)
table(data.update$main_category)
mean(data.update$Campaign_Length)



##### Exploring Data used Tableau for ease of use
### AVG Backers for each state
#### Top 20 Categories
#### Country 
### AVG Goal 







#### divide data 60% training 40% validation
set.seed(1)
train.index <- sample(rownames(data.update), dim(data.update)[1]*.6)
valid.index <- setdiff(rownames(data.update),train.index)
train <-  data.update[train.index,]
valid <- data.update[valid.index, ]


##### Model 1 Logistic Regression
model_1 <- glm(state~ goal+main_category+currency+country+Campaign_Length+ backers, data = train, family = "binomial" )
summary(model_1)
significant.variables <- summary(model_1)$coeff[-1,4] < 0.01
names(significant.variables)[significant.variables == TRUE]
significant.variables

### predictions
prediction_model1 <- predict(model_1,valid[,-8], type = "response")
actual_1 <- valid$state
prediction_1<- rep(0,length(prediction_model1))
prediction_1[prediction_model1 > .5] = "successful"
prediction_1[prediction_model1< .5] = "failed"
table(actual_1,prediction_1)
mean(actual_1 != prediction_1)
plot(model_1)
### Evaluate


valid$m1_score <- predict(model_1,type='response',valid)

m1_pred <- prediction(valid$m1_score, valid$state)

m1_perf <- performance(m1_pred,"tpr","fpr")

### Graph
par(mfrow=c(1,1))

plot(m1_perf, lwd=2, colorize=TRUE, main="ROC m1: Logistic Regression Performance")
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)
#### Variable Importance


#### Model 2 Random Forest

library(randomForest)
m2 <- randomForest(state ~ goal+ Campaign_Length+ currency + country+ backers, data = train)
summary(m2)
plot(m2)
varImpPlot(m2, main="Random Forest: Variable Importance")
### predicitons
m2_predict <- predict(m2,valid[,-8], type = "response")
table(m2_predict,actual_1)
mean(actual_1 != m2_predict)
# score test data
valid$m2_score <- predict(m2,type='prob',valid)

m2_pred <- prediction(valid$m2_score[,2],valid$state)

m2_perf <- performance(m2_pred,"tpr","fpr")
m2_perf <- performance(m2_predict,"tpr","fpr")
m2_perfromance <- performance(m2_pred, measure = "prec", x.measure = "rec")
plot(m2_perf, lwd=2, colorize=TRUE, main="ROC m2: Traditional Recursive Partitioning")
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)

### Model 3 tree
library(tree)
install.packages("tree")
tree1 <- tree(state ~ goal  + Campaign_Length + country+ backers , data = train)
summary(tree1)
plot(tree1)
text(tree1 ,pretty =0)
#### Predict
predict_3 <- predict(tree1,valid)
prediction_3<- rep(0,length(valid))
prediction_3[predict_3[,2] > .5] = "successful"
prediction_3[predict_3[,2]< .5] = "failed"

##### EValuate 

valid$m3_score <- predict_3[,2]
m3_pred <- prediction(valid$m3_score,valid$state)
m3_perf <- performance(m3_pred,"tpr","fpr")
m3_perfromance <- performance(m3_pred, measure = "prec", x.measure = "rec")
plot(m3_perf, lwd=2, colorize=TRUE, main="ROC m3: simple tree")
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)


### Results
table(actual_1,prediction_3)
mean(actual_1 != prediction_1)


##4 Model 4 Neural Net
Mod_4 <- nnet(state ~ goal+ Campaign_Length  + country + backers, data = train, hidden = 3, algorithm = "backdrop", act.fct = "logistic", size = 15 )
summary(Mod_4)
Mod_4$wts

prediction_mod4 <- predict(Mod_4, valid[,-8], type = "raw" )
prediciton_model4 <- rep(0, length(actual_1))
prediciton_model4[prediction_mod4 > .5] = "successful"
prediciton_model4[prediction_mod4 < .5] = "failed"
table(prediciton_model4,actual_1)
mean(actual_1 != prediciton_model4)

#### Evaluate
m4_pred <- prediction(predict(Mod_4, newdata=valid, type="raw"),valid$state)
m4_perf <- performance(m4_pred,"tpr","fpr")
plot(m4_perf,lwd=2, colerize=TRUE, main="m4:ROC - Neural Network")
abline(a=0,b=1)
#### plot nnet
# get the weights and structure in the right format
wts <- neuralweights(Mod_4)
wts
struct <- wts$struct
wts <- unlist(wts$wts)
plotnet(Mod_4, struct=struct)


