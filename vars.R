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

all.vars <- setdiff(all.vars,c('user_id', 'register_ip_country'))

