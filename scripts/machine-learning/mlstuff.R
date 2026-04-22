library(caret)
library(dplyr)
library(rsample)
apps = read.csv('../Downloads/appstore_games.csv')
apps_type = sapply(apps[1, ], typeof)

apps_2 = apps[, c('Average.User.Rating', 'User.Rating.Count', 'Price')] %>%
  na.omit()
histogram(apps_2$Average.User.Rating)

doml_pretrain = function(apps_2, strata, target_col) {
  set.seed(1234)
  apps_2 = drop_na(apps_2)
  colnames(apps_2)[grep(target_col, colnames(apps_2))] = 'target'
  if (strata == target_col) {
    strata = 'target'
  }
  app_split = apps_2 %>%
    initial_split(prop = 0.8, strata = strata)
  
  app_train = training(app_split)
  app_test = testing(app_split)
  fit_lm_bt = train(target ~ ., method = "lm", data = app_train, trControl = trainControl(method = 'boot'))
  # fit_rf_bt = train(target ~ ., method = "rf", data = app_train, trControl = trainControl(method = 'boot'))
  library(yardstick)
  
  results = app_train %>%
    mutate('Linear regression' = predict(fit_lm_bt, app_train))#,
           # 'Random forest' = predict(fit_rf_bt, app_train))
  
  # Evaluate the performance
  
  metrics(results, truth = target, estimate = 'Linear regression')
  #metrics(results, truth = 'target', estimate = 'Random forest')
  
}
# 
# apps_2 = apps
# 
# # Make one-hot encoding (with or without in.app.purchases)
# apps_2$In.app.Purchases[sapply(apps$In.app.Purchases, FUN = identical, '')] = 0
# apps_2$In.app.Purchases[!sapply(apps$In.app.Purchases, FUN = identical, '')] = 1
# 
# apps_2 = apps_2 %>%
#   mutate(
#     Age.Rating = as.factor(Age.Rating),
#     Languages = as.factor(Languages),
#     Primary.Genre = as.factor(Primary.Genre)
#   )
# 
# apps_3 = apps_2[sapply(apps_2[1, ], typeof) != 'character'] %>%
#   select(-ID, -Languages) # ID is numeric, but useless
# 
# dummies = dummyVars("Average.User.Rating ~ .", data = apps_3)
# apps_4 = predict(dummies, newdata = apps_3) %>%
#   as.data.frame()
# 
# app_train = doml_pretrain(apps_2 = apps_4, strata = 'Average.User.Rating')
# fit_lm_bt = train(Average.User.Rating ~ ., method = "lm", data = app_train, trControl = trainControl(method = 'boot'))
# doml_posttrain(app_train = app_train, fit_lm_bt = fit_lm_bt, target = 'Average.User.Rating')

####
# for (row in 1:nrow(apps)) {
#   apps_2$In.app.Purchases[row] = strsplit(apps$In.app.Purchases[row], split = ',') %>%
#     sapply(as.numeric) %>%
#     mean()
# }
# drop
# head(complete.cases(apps_2))

doml_pretrain(apps_2 = apps_2, strata = 'Average.User.Rating', target_col = 'Average.User.Rating')
