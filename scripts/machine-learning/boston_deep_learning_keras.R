library(keras)

# The Boston Housing Prices dataset ---------------------------------------

boston_housing <- dataset_boston_housing()

c(train_data, train_labels) %<-% boston_housing$train
c(test_data, test_labels) %<-% boston_housing$test

paste0("Training features: ", ncol(train_data), ", observations: ", nrow(train_data))

library(tibble)

column_names <- c('CRIM', 'ZN', 'INDUS', 'CHAS', 'NOX', 'RM', 'AGE', 
                  'DIS', 'RAD', 'TAX', 'PTRATIO', 'B', 'LSTAT')
train_df <- as_tibble(train_data)
colnames(train_df) <- column_names

train_df


# Normalize features ------------------------------------------------------

# It's recommended to normalize features that use different scales and 
# ranges. Although the model might converge without feature normalization, 
# it makes training more difficult, and it makes the resulting model more 
# dependant on the choice of units used in the input.

# Test data is *not* used when calculating the mean and std.

# Normalize training data
train_data <- scale(train_data) 

# Use means and standard deviations from training set to normalize test set
col_means_train <- attr(train_data, "scaled:center") 
col_stddevs_train <- attr(train_data, "scaled:scale")
test_data <- scale(test_data, center = col_means_train, scale = col_stddevs_train)

train_data[1, ] # First training sample, normalized


# Create the model --------------------------------------------------------


build_model <- function() {
  
  # Keras Model composed of a linear stack of layers
  model <- keras_model_sequential() %>% 
    # Add a densely-connected NN layer to an output
    layer_dense(units = 64, activation = "relu",
                input_shape = dim(train_data)[2]) %>%
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

epochs <- 100

early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)

# Fit the model and store training stats
model <- build_model()
history <- model %>% fit(
  train_data,
  train_labels,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 1,
  callbacks = list(early_stop)
)


# Evaluate the model ------------------------------------------------------

c(loss, mae) %<-% (model %>% evaluate(test_data, test_labels, verbose = 0))

paste0("Mean absolute error on test set: $", sprintf("%.2f", mae * 1000))

# Predict -----------------------------------------------------------------
test_predictions <- model %>% predict(test_data)





