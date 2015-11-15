library(plyr)
library(ROCR)

counts <- count(dragoncity_hackathon_train[,'register_ip_country'])
heavy_use_countries <- subset(counts, freq > 1000)

dragoncity_hackathon_train.subset <- subset(dragoncity_hackathon_train, register_ip_country %in% heavy_use_countries[,'x'])

for (country in heavy_use_countries[,'x']) {

  dragoncity_hackathon_train.subset <- subset(dragoncity_hackathon_train, register_ip_country == country)

  modelfit <- glm(churn ~ log(num_sessions)+played_day2+is_marketing_install+has_cs_ticket
                  +dragons_lvl_up+breedings+has_login_error+number_goals+number_dragons
                  +device_age+max_level_reached + dollar_gross+facebook_connected+attacks_wins
                  +num_sessions_1d+has_dragons_bought+dragons_bought+last_cash+last_gold
                  +last_food
                  +reach_lvl_3+lvl_ups+attacks, dragoncity_hackathon_train.subset, family = binomial())

  predictions <- predict(modelfit, newdata = dragoncity_hackathon_train.subset, type = "response")
  ROCRpred = prediction(predictions, dragoncity_hackathon_train.subset$churn)
  auc <- as.numeric(performance(ROCRpred, "auc")@y.values)

  print(paste("auc for country", country, ":", auc))
}


mobile <- read.csv('mobile.csv', stringsAsFactors = FALSE)
mobile <- mobile[,c('Country.Name','Country.Code','X2014','X2009')]
mobile$growth_rate <- (mobile[,'X2014']/mobile[,'X2009'] - 1)*100
mobile <- mobile[with(mobile, order(-growth_rate)),]

country_codes <- read.csv('country_codes.csv', stringsAsFactors = FALSE)
alpha2 <- 'ISO3166.1.Alpha.2'
alpha3 <- 'ISO3166.1.Alpha.3'
mobile$alpha2 <- rep(NA, nrow(mobile))

for (idx in 1:nrow(mobile)) {
  code <- mobile[idx,'Country.Code']
  country <- country_codes[country_codes[,'ISO3166.1.Alpha.3']== code,]
  code <- country['ISO3166.1.Alpha.2'][[1]]

  print(code)
  #if (length(code) > 1) {
    mobile[idx,'alpha2'] <- code
  #}
}

mobile$auc <- rep(NA, nrow(mobile))

for (country in unique(dragoncity_hackathon_train[,'register_ip_country'])) {

  dragoncity_hackathon_train.subset <- subset(dragoncity_hackathon_train, register_ip_country == country)

  modelfit <- glm(churn ~ log(num_sessions)+played_day2+is_marketing_install+has_cs_ticket
                  +dragons_lvl_up+breedings+has_login_error+number_goals+number_dragons
                  +device_age+max_level_reached + dollar_gross+facebook_connected+attacks_wins
                  +num_sessions_1d+has_dragons_bought+dragons_bought+last_cash+last_gold
                  +last_food
                  +reach_lvl_3+lvl_ups+attacks, dragoncity_hackathon_train.subset, family = binomial())

  predictions <- predict(modelfit, newdata = dragoncity_hackathon_train.subset, type = "response")
  ROCRpred = prediction(predictions, dragoncity_hackathon_train.subset$churn)
  auc <- as.numeric(performance(ROCRpred, "auc")@y.values)

  mobile[mobile[,'alpha2'] == country,'auc'] <- auc

  print(paste("auc for country", country, ":", auc))
}
