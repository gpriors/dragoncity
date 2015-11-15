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
soft.threshold <- function(x,lambda){
  sign(x) * max(c(abs(x) - lambda , 0))
}

lasso.reg <- function(y, X, lambda, max.iter) {
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
      print(paste('beta is', beta))
      print(paste('beta.prev is', beta.prev))
      if (!is.na(beta) & !is.na(beta.prev) & (sum((beta - beta.prev)**2) < 1e-6)) { return(beta) }

      beta.prev <- beta
  }

  beta
}

library(ROCR)


vars <- c("payer", "num_sessions",
"max_level_reached", "reach_lvl_3", "cash_spent",   
"spents",  "dollar_gross", "transactions", 
"dragons_lvl_up","lvl_ups", "breedings", "login_errors", 
"has_login_error", "number_dragons","facebook_connected", 
"number_goals", "played_day2", "attacks",
"attacks_wins", "last_cash",  "last_food", 
"last_gold", "dragons_bought","has_dragons_bought", 
"num_sessions_1d", "num_sessions_2d")

X <- dragoncity_hackathon_train[,vars]
X <- model.matrix(~., data =X)
y <- dragoncity_hackathon_train[,'churn']


lambda <- 8
lasso.coefs <- lasso.reg(y, X, lambda, 1000)
res <- X%*%lasso.coefs
predictions <- inv.logit(res)
ROCRpred = prediction(predictions, dragoncity_hackathon_train$churn)
auc <- as.numeric(performance(ROCRpred, "auc")@y.values)

# Remove vars abs(val) < 0.01
lasso.results <- cbind(colnames(X), lasso.coefs)
lasso.results.subset <- subset(lasso.results, abs(lasso.coefs) > 0.009999)
vars <- subset(lasso.results, abs(lasso.coefs) > 0.009999)[,1]

















#lambda.range <- append(seq(0,1,0.05), seq(0,10,1))
lambda.range <- seq(0,10,1)
aucs <- matrix(ncol=2, dimnames = list(1, c('auc', 'lambda')))

for (lambda in lambda.range) {
  print(paste0('lambda = ', lambda))
  lasso.coefs <- lasso.reg(y, X, lambda, 10)
  res <- X%*%lasso.coefs
  predictions <- inv.logit(res)
  ROCRpred = prediction(predictions, dragoncity_hackathon_train$churn)
  auc <- as.numeric(performance(ROCRpred, "auc")@y.values)
  print(paste0('auc = ', auc))

  if (i == 1) {
    aucs[i,] <- list(auc, lambda)
  } else {
    aucs <- rbind(aucs, list(auc, lambda))
  }
}


vars <- c("log_num_sessions","played_day2","is_marketing_install","has_cs_ticket","dragons_lvl_up","breedings","has_login_error","number_goals","number_dragons","device_age","max_level_reached","dollar_gross","facebook_connected","attacks_wins","num_sessions_1d","has_dragons_bought","dragons_bought","last_cash","last_gold","last_food","reach_lvl_3","lvl_ups","attacks")
varlength <- length(vars)
aucs <- matrix(ncol=2, dimnames = list(1, c('auc', 'model')))

for (i in 1:varlength) {
  vars_subset <- vars[setdiff(1:varlength,i)]
  model.formula <- formula(paste("churn ~", paste(vars_subset, collapse = "+")))
  modelfit <- glm(model.formula, data=dragoncity_hackathon_train,family=binomial())
  predictions <- predict(modelfit, newdata = dragoncity_hackathon_train, type = "response")
  ROCRpred = prediction(predictions, dragoncity_hackathon_train$churn)
  auc <- as.numeric(performance(ROCRpred, "auc")@y.values)

  if (i == 1) {
    aucs[i,] <- list(auc, as.character(model.formula)[3])
  } else {
    aucs <- rbind(aucs, list(auc, as.character(model.formula)[3]))
  }
}

# 0.7231925 => "num_sessions + reach_lvl_3 + cash_spent + spents + dollar_gross + transactions + lvl_ups + breedings + number_cs_ticket + has_cs_ticket + login_errors + has_login_error + number_dragons + facebook_connected + number_goals + played_day2 + attacks + attacks_wins + last_cash + last_food + last_gold + dragons_bought + has_dragons_bought + num_sessions_1d"

