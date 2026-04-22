# LSTM Time-Series Forecasting for COVID Cases
source('scripts/utils/functions.R')
forceLibrary(c("dplyr", "keras", "tensorflow", "ggplot2"))

print("Fetching full OWID data...")
owid_url <- "https://catalog.ourworldindata.org/garden/covid/latest/compact/compact.csv"
data <- read.csv(owid_url, stringsAsFactors = FALSE)

# Handle column name differences in the new catalog
if (!"location" %in% colnames(data) & "country" %in% colnames(data)) {
  data <- data %>% rename(location = country)
}
if (!"iso_code" %in% colnames(data) & "code" %in% colnames(data)) {
  data <- data %>% rename(iso_code = code)
}

# Focus on one country for simplicity, e.g., United States
df <- data %>%
  filter(location == "United States") %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  select(date, new_cases_smoothed) %>%
  filter(!is.na(new_cases_smoothed))

# Scale data
max_val <- max(df$new_cases_smoothed)
min_val <- min(df$new_cases_smoothed)
df$scaled_cases <- (df$new_cases_smoothed - min_val) / (max_val - min_val)

# Prepare sliding window
window_size <- 30
X <- c()
y <- c()

for (i in 1:(nrow(df) - window_size)) {
  X <- rbind(X, df$scaled_cases[i:(i + window_size - 1)])
  y <- c(y, df$scaled_cases[i + window_size])
}

# Reshape for LSTM: [samples, time steps, features]
X <- array(X, dim = c(nrow(X), window_size, 1))

# Train-test split
split_idx <- floor(0.8 * nrow(X))
X_train <- X[1:split_idx, , , drop=FALSE]
y_train <- y[1:split_idx]
X_test <- X[(split_idx + 1):nrow(X), , , drop=FALSE]
y_test <- y[(split_idx + 1):length(y)]

# Define model
print("Defining Keras Sequential model...")
model <- keras_model_sequential() %>%
  layer_lstm(units = 50, input_shape = c(window_size, 1)) %>%
  layer_dense(units = 1)

model %>% compile(
  loss = "mean_squared_error",
  optimizer = optimizer_adam()
)

print("Model architecture defined.")
print("Note: To run training, uncomment the fit function. Keras/TensorFlow must be configured in your R environment via reticulate.")
# history <- model %>% fit(X_train, y_train, epochs = 20, batch_size = 32, validation_split = 0.1, verbose = 1)
