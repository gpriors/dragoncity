all.vars <- c("user_id", "date_register", "date_register_ios",  
"date_last_logged", "date_last_logged_ios","register_ip_country",
"register_version", "register_source_ios", "is_marketing_install",
"register_device",  "device_group",  "device_age",   
"payer",   "num_sessions",  "churn",  
"max_level_reached", "reach_lvl_3", "cash_spent",   
"spents",  "dollar_gross", "transactions", 
"dragons_lvl_up","lvl_ups", "breedings", 
"number_cs_ticket", "has_cs_ticket", "login_errors", 
"has_login_error", "number_dragons","facebook_connected", 
"number_goals", "played_day2",   "attacks",
"attacks_wins", "last_cash",  "last_food", 
"last_gold", "dragons_bought","has_dragons_bought", 
"num_sessions_1d", "num_sessions_2d")

all.continuous.vars <- c("num_sessions", "max_level_reached", "reach_lvl_3", "cash_spent",   
"spents",  "dollar_gross", "transactions", 
"dragons_lvl_up","lvl_ups", "breedings", 
"number_cs_ticket", "has_cs_ticket", "login_errors", 
"has_login_error", "number_dragons","facebook_connected", 
"number_goals", "played_day2",   "attacks",
"attacks_wins", "last_cash",  "last_food", 
"last_gold", "dragons_bought","has_dragons_bought", 
"num_sessions_1d", "num_sessions_2d")

num_sessions_2d <- dragoncity_hackathon_train[,'num_sessions_2d']
num_sessions_2d[is.na(num_sessions_2d <- num_sessions_2d)] <- 0
dragoncity_hackathon_train$num_sessions_2d <- num_sessions_2d

cols <- paste(all.continuous.vars, collapse = " + ")
formula <- formula(paste('num_sessions ~', cols))

sessions_model <- glm(num_sessions ~ lvl_ups,data=dragoncity_hackathon_train[,all.continuous.vars],family=poisson())

vars_subset <- c("num_sessions",
"max_level_reached", "reach_lvl_3", "cash_spent",   
"spents",  "dollar_gross", "transactions", 
"dragons_lvl_up","lvl_ups", "breedings", 
"number_cs_ticket", "has_cs_ticket", "login_errors", 
"has_login_error", "number_dragons","facebook_connected", 
"number_goals", "played_day2",   "attacks",
"attacks_wins", "last_cash",  "last_food", 
"last_gold", "dragons_bought","has_dragons_bought", 
"num_sessions_1d", "num_sessions_2d")

# Maybe include interaction of day 1 and day 2?
# All btw -0.19 and -0.3, used num_sessions_2d by intuition
set <- c('num_sessions', 'num_sessions_1d', 'num_sessions_2d')
# model.all <- glm(churn ~ num_sessions, data = dragoncity_hackathon_train, family = binomial())
# model.first <- glm(churn ~ num_sessions_1d, data = dragoncity_hackathon_train, family = binomial())
# model.second <- glm(churn ~ num_sessions_2d, data = dragoncity_hackathon_train, family = binomial())

# attacks_wins much greater impact -0.26 vs -0.69
set <- c('attacks', 'attacks_wins')
# model.attacks <- glm(churn ~ attacks, data = dragoncity_hackathon_train, family = binomial())
# model.attacks_wins <- glm(churn ~ attacks_wins, data = dragoncity_hackathon_train, family = binomial())

set <- c('has_cs_ticket', 'number_cs_ticket')
# neither are stat sig
# model.has_cs_ticket <- glm(churn ~ has_cs_ticket, data = dragoncity_hackathon_train, family = binomial())
# model.number_cs_ticket <- glm(churn ~ number_cs_ticket, data = dragoncity_hackathon_train, family = binomial())

set <- c("number_goals", "max_level_reached", "spents", "dragons_lvl_up", "lvl_ups", "breedings", "number_dragons")
# dragons_lvl_up biggest var
# for (i in set) {
#   print(paste("modelling", i))
#   model <- glm(dragoncity_hackathon_train[,'churn'] ~ dragoncity_hackathon_train[,i], family = binomial())
#   print(summary(model))
# }

set <- c("cash_spent", "dollar_gross")
# removed both as having very little impact
# model.cash_spent <- glm(churn ~ cash_spent, data = dragoncity_hackathon_train, family = binomial())
# model.dollar_gross <- glm(churn ~ dollar_gross, data = dragoncity_hackathon_train, family = binomial())

# model.reach_lvl_3 <- glm(churn ~ reach_lvl_3, data = dragoncity_hackathon_train, family = binomial())
# model.dragons_lvl_up <- glm(churn ~ dragons_lvl_up, data = dragoncity_hackathon_train, family = binomial())
# summary(model.reach_lvl_3)
# summary(model.dragons_lvl_up)

library(ROCR)

vars_subset <- c("reach_lvl_3", "transactions", "login_errors", 
"has_login_error", "facebook_connected", "played_day2",
"attacks_wins", "last_cash",  "last_food", 
"last_gold", "dragons_bought","has_dragons_bought", "num_sessions_2d", "device_group")

#model.formula <- formula(paste("churn ~", paste(vars_subset, collapse = "+")))
modelfit <- glm(churn ~ reach_lvl_3 + transactions + login_errors + has_login_error + 
    facebook_connected + played_day2 + attacks_wins + last_cash + 
    last_food + last_gold + dragons_bought + has_dragons_bought + 
    num_sessions_2d + device_group, data = dragoncity_hackathon_train,family=binomial())
#dragoncity_hackathon_train <- model.matrix(~., data =dragoncity_hackathon_train)

predictions <- predict(modelfit, newdata = dragoncity_hackathon_train, type = "response")
ROCRpred = prediction(predictions, dragoncity_hackathon_train$churn)
auc <- as.numeric(performance(ROCRpred, "auc")@y.values)

modelfit <- glm(churn ~ num_sessions+played_day2
                +breedings+has_login_error
                +reach_lvl_3+lvl_ups+attacks+device_group
                +device_age,data=dragoncity_hackathon_train,family=binomial())
summary(modelfit)


correlations <- cor(dragoncity_hackathon_train[,vars_subset])

correlated_vars <- list()

for (i in 1:ncol(correlations)) {
  print(paste("idx is", i))
  var <- colnames(correlations)[i]
  print(paste("var is", var))
  varcorrelations <- correlations[var,]
  for (corridx in 1:length(varcorrelations)) {
    corr <- varcorrelations[corridx]
    print(paste('corr is', corr))
    rowname <- names(corr[1])
    print(paste("rowname is", rowname))
    if (length(corr) > 0 && !is.na(corr)) {
      val <- corr[[1]]
      if (rowname != var & val > 0.75) {
        
          correlated_vars[var] <- paste(correlated_vars[var], rowname)
        
      }
    }
  }
}


