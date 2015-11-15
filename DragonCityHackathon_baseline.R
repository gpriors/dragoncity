##################################################################
# WELCOME TO THE BCNANALYTICS AND SOCIAL POINT DATA HACKATHON
# In this file you will find the code for the baseline model
################################################################

####################################################################
#Reading in the data

dragoncity_hackathon_train <- read.csv(file="dragon_city_hackathon_train_final.csv", header=TRUE,sep=",")
dragoncity_hackathon_test <- read.csv(file="dragon_city_hackathon_test_final.csv", header=TRUE,sep=",")

####################################################
## fit model
modelfit <- glm(churn ~ num_sessions_2d+played_day2
                +breedings+has_login_error
                +reach_lvl_3+lvl_ups+attacks+device_group
                +device_age,data=dragoncity_hackathon_train,family=binomial())
summary(modelfit)

modelfit2 <- randomForest()

####################################################
##Creating the predictions of the model
predictions <- predict(modelfit, newdata = train, type = "response")
write.table(predictions, 'baseline_submission.csv', row.names=F, quote=F, col.names=F) 

####################################################
# #Checking the roc and auc
# We suggest to check the AUC  in a crossvalidated set internally as
# an alternativeway to check the quality of your model instead
# of just relying on the public leaderboard feedback

# You can use the auc function of the pROC package for it
install.packages("pROC")
library(pROC)
auc(pred, real)

install.packages("ROCR")
library(ROCR)
predictions <- predict(modelfit, newdata = dragoncity_hackathon_train, type = "response")
ROCRpred = prediction(predictions, dragoncity_hackathon_train$churn)
as.numeric(performance(ROCRpred, "auc")@y.values)
