results

summary(sessions_model)

Call:
glm(formula = num_sessions ~ reach_lvl_3, family = poisson(), 
    data = dragoncity_hackathon_train[, all.continuous.vars])

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.2153  -0.4757  -0.2245   0.1485  15.7884  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) 0.216426   0.001893   114.3   <2e-16 ***
reach_lvl_3 0.795225   0.002190   363.1   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for poisson family taken to be 1)

    Null deviance: 684912  on 524965  degrees of freedom
Residual deviance: 537389  on 524964  degrees of freedom
AIC: 1818172

Number of Fisher Scoring iterations: 5

modelfit <- glm(churn ~ num_sessions + max_level_reached, data = dragoncity_hackathon_train, family = binomial())

predictions <- predict(modelfit, newdata = dragoncity_hackathon_train, type = "response")
ROCRpred = prediction(predictions, dragoncity_hackathon_train$churn)
(auc <- as.numeric(performance(ROCRpred, "auc")@y.values))

summary(modelfit)

dragoncity_hackathon_train <- read.csv(file="dragon_city_hackathon_train_final.csv", header=TRUE,sep=",")

levels <- unique(dragoncity_hackathon_train[,'max_level_reached'])
matrix_levels <- matrix(NA, nrow = length(levels), ncol = 2)
dimnames(matrix_levels)[[2]] <- c('level', 'percentage_churn')

for (idx in 1:length(levels)) {
  l <- levels[idx]
  # == or gte?
  subset <- subset(dragoncity_hackathon_train, max_level_reached == l)
  percentage_churn <- sum(subset$churn)/nrow(subset)
  matrix_levels[idx,] <- c(l, percentage_churn)
}


matrix_levels <- matrix_levels[order(matrix_levels[,'level']),]

plot(matrix_levels, ylim = c(0,1), main = 'Percentage Churn by Max Level Reached', 
  col = 'darkolivegreen4', pch = 19, bg = 'darkolivegreen2')

dragoncity_hackathon_train$time_in_days <- as.Date(dragoncity_hackathon_train$date_last_logged) - as.Date(dragoncity_hackathon_train$date_register)
churned <- subset(dragoncity_hackathon_train, churn == 1)

churned$bins <- cut(as.numeric(churned$time_in_days), breaks=c(-1,0,1,2,3,4,5,19,37,63,200), labels=c("0-1","1-2","2-3","3-4", "4-5", "5-19", "37-63", "63+"))

cuts <- apply(churned, 2, cut, c(-Inf,seq(0.5, 1, 0.1), Inf), labels=0:6)