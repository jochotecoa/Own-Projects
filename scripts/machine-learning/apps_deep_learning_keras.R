splitXy = function(df,y){
  x = df[, -y]
  y2 = df[, y]
  return(list(x,y2))
}

library(keras)
library(dplyr)
library(rsample)

# The Boston Housing Prices dataset ---------------------------------------

apps = read.csv('../Downloads/appstore_games.csv')
y = 'Average.User.Rating'
apps_2 = apps[, c('Average.User.Rating', 'User.Rating.Count', 'Price')] %>%
  na.omit()
target = grep(colnames(apps_2), y)
apps_split = apps_2 %>%
  initial_split(prop = 0.8, strata = y)

train_data = training(apps_split); test_data = testing(apps_split)

c(train_x, train_y_real) %<-% splitXy(df = train_data, y = target)
c(test_x, test_y_real) %<-% splitXy(df = test_data, y = target)

# c(train_x, train_y_real) %<-% boston_housing$train
# c(test_x, test_y_real) %<-% boston_housing$test

paste0("Training features: ", ncol(train_x), ", observations: ", nrow(train_x))


train_df


# Normalize features ------------------------------------------------------

# It's recommended to normalize features that use different scales and 
# ranges. Although the model might converge without feature normalization, 
# it makes training more difficult, and it makes the resulting model more 
# dependant on the choice of units used in the input.

# Test data is *not* used when calculating the mean and std.

# Normalize training data
train_x <- scale(train_x) 

# Use means and standard deviations from training set to normalize test set
col_means_train <- attr(train_x, "scaled:center") 
col_stddevs_train <- attr(train_x, "scaled:scale")
test_x <- scale(test_x, center = col_means_train, scale = col_stddevs_train)

train_x[1, ] # First training sample, normalized


# Create the model --------------------------------------------------------


build_model <- function() {
  
  # Keras Model composed of a linear stack of layers
  model <- keras_model_sequential() %>% 
    # Add a densely-connected NN layer to an output
    layer_dense(units = 64, activation = "relu",
                input_shape = dim(train_x)[2]) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    # Average squared difference between the estimated/actual values
    loss = "mse",
    # Iterative method for optimizing an objective function
    optimizer = optimizer_rmsprop(),
    # Metrics to be evaluated by the model during training and testing. 
    metrics = list("mean_absolute_error")
  )
  
  model
}

model <- build_model()
model %>% summary()


# Train the model ---------------------------------------------------------


# Display training progress by printing a single dot for each completed epoch.
print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)    

epochs <- 500

early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)

# Fit the model and store training stats
model <- build_model()
history <- model %>% fit(
  train_x,
  train_y_real,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 1,
  callbacks = list(early_stop)
)


# Evaluate the model ------------------------------------------------------

c(loss, mae) %<-% (model %>% evaluate(test_x, test_y_real, verbose = 0))

paste0("Mean absolute error on test set: ", print(mae))

# Predict -----------------------------------------------------------------
test_predictions <- model %>% predict(test_x)





