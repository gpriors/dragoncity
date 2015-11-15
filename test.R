dragoncity_hackathon_train$breedings_per_session <- dragoncity_hackathon_train$breedings/dragoncity_hackathon_train$num_sessions
dragoncity_hackathon_train$proportion_secondday_sessions <- dragoncity_hackathon_train$num_sessions_2d/dragoncity_hackathon_train$num_sessions

modelfit <- glm(churn ~  max_level_reached + max_level_reached:num_sessions +
  num_sessions + reach_lvl_3 + num_sessions:reach_lvl_3,
  dragoncity_hackathon_train, family = binomial())

predictions <- predict(modelfit, newdata = dragoncity_hackathon_train, type = "response")
ROCRpred = prediction(predictions, dragoncity_hackathon_train$churn)
(auc <- as.numeric(performance(ROCRpred, "auc")@y.values))