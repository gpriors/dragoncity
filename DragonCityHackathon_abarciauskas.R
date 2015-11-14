##################################################################
# WELCOME TO THE BCNANALYTICS AND SOCIAL POINT DATA HACKATHON
# In this file you will find the code for the baseline model
################################################################

####################################################################
#Reading in the data
setwd('train_test')
dragoncity_hackathon_train <- read.csv(file="dragon_city_hackathon_train_final.csv", header=TRUE,sep=",")
dragoncity_hackathon_test <- read.csv(file="dragon_city_hackathon_test_final.csv", header=TRUE,sep=",")


####################################################
## fit model
modelfit.baseline <- glm(churn ~ num_sessions+played_day2
                +breedings+has_login_error
                +reach_lvl_3+lvl_ups+attacks+device_group
                +device_age,data=dragoncity_hackathon_train,family=binomial())
summary(modelfit.baseline)

#modelfit.all <- glm(churn ~ .,data=dragoncity_hackathon_train,family=binomial())
#summary(modelfit.all)






####################################################
##Creating the predictions of the model
predictions.baseline <- predict(modelfit, newdata = dragoncity_hackathon_test, type = "response")
write.table(predictions.baseline, 'baseline_submission.csv', row.names=F, quote=F, col.names=F) 

####################################################
# #Checking the roc and auc
# We suggest to check the AUC  in a crossvalidated set internally as
# an alternativeway to check the quality of your model instead
# of just relying on the public leaderboard feedback

# You can use the auc function of the pROC package for it

install.packages("ROCR")
library(ROCR)
ROCRpred = prediction(predictions, train$churn)
as.numeric(performance(ROCRpred, "auc")@y.values)

soft.threshold <- function(x,lambda){
  sign(x) * max(c(abs(x) - lambda , 0))
}

lasso.reg <- function(y, X, lambda) {
  max.iter <- 10
  # Number of explanatory variables
  P <- ncol(X)

  #beta <- solve(t(X)%*%X,t(X)%*%y)
  # Get coefficients from glm 
  data <- as.data.frame(cbind(y, X))
  beta <- glm(y ~ 0 + X, data = data, family=binomial())$coefficients
  # compare with as.vector(modelfit.baseline$coefficients)
  beta <- as.vector(beta)
  beta.prev <- beta

  # Do until convergence
  for (iter in 1:max.iter) {
    for (i in 1:P)
      # For evey coefficient, define y_i(-k) = y_i - sum(beta[,setdiff(beta, i)]%*%)
      y.k <- y - X[,setdiff(1:P,i)] %*% beta[setdiff(1:P,i)]
      x.k <- X[,i]
      cov <- sum(y.k * x.k)
      var <- sum(x.k * x.k)

      # cov/var is just the least squares kth coefficient
      beta[i] <- soft.threshold(cov/var , lambda/(2*var))
      if (sum((beta - beta.prev)**2) < 1e-6) { return(beta) }

      beta.prev <- beta
  }

  beta
}

# X are all the variables we include in the regression
# y is churn
vars <- c('num_sessions', 'played_day2', 'breedings', 'has_login_error', 'reach_lvl_3', 'lvl_ups', 'attacks', 'device_group', 'device_age')
X <- dragoncity_hackathon_train[,vars]
X <- model.matrix(~., data =X)
y <- dragoncity_hackathon_train[,'churn']
lambda <- 0.1

lasso.coefs <- lasso.reg(y, X, lambda)
predictions <- inv.logit(X%*%lasso.coefs)

library(ROCR)
ROCRpred = prediction(predictions, dragoncity_hackathon_train$churn)
as.numeric(performance(ROCRpred, "auc")@y.values)
