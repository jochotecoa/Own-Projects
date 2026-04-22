
# Step 1. Load in the data. -----------------------------------------------


library(tidyverse)
library(reshape2)

housing = read.csv('https://raw.githubusercontent.com/ageron/handson-ml/master/datasets/housing/housing.csv')
head(housing)
summary(housing)
par(mfrow=c(2,5))
colnames(housing)
# melt structures the data.frame into the typical ggplot one
# facet_wrap divides the plot into subplots, each representing each column
#   free_x means that every plot has its own x scale
ggplot(data = melt(housing), mapping = aes(x = value)) + 
  geom_histogram(bins = 30) + 
  facet_wrap(~variable, scales = 'free_x')


# Step 2. Clean the data --------------------------------------------------

# Impute missing values ---------------------------------------------------

housing$total_bedrooms[is.na(housing$total_bedrooms)] = 
  median(housing$total_bedrooms, na.rm = T)

# Fix the total columns - make them means ---------------------------------

housing = housing %>%
  mutate(mean_bedrooms = total_bedrooms/households, 
         mean_rooms = total_rooms/households) %>%
  select(-c(total_rooms, total_bedrooms))


# Turn categoricals into booleans -----------------------------------------

cat_housing = model.matrix(~housing$ocean_proximity) %>%
  .[, -1]
head(cat_housing)


# Scale the numerical variables -------------------------------------------

housing_num = housing %>%
  select(-ocean_proximity, -median_house_value)
head(housing_num)

scaled_housing_num = scale(housing_num)
head(scaled_housing_num)


# Merge the altered numerical and categorical dataframes ------------------

cleaned_housing = cbind(cat_housing, scaled_housing_num, 
                        median_house_value = housing$median_house_value) %>%
  as.data.frame()
head(cleaned_housing)


# Step 3. Create a test set of data ---------------------------------------

set.seed(1738)

n = nrow(cleaned_housing)
train_fraction = 0.8
size = n * train_fraction

sample = sample.int(n, size)
train = cleaned_housing[sample, ]
test = cleaned_housing[-sample, ]
stopifnot(nrow(train) + nrow(test) == nrow(cleaned_housing))
stopifnot(ncol(train) == ncol(test), ncol(test) == ncol(cleaned_housing))


# Step 4. Test some predictive models. ------------------------------------

library(boot)

# Give me a generalized linear model: does not expect linear changes in Y
# due to linear changes in X
colnames(cleaned_housing)
glm_house = glm(data = cleaned_housing,
                formula = median_house_value~median_income+mean_rooms+
                  population)

(k_fold_cv_error = cv.glm(cleaned_housing , glm_house, K=5))$delta
(glm_cv_rmse = sqrt(k_fold_cv_error$delta)[1])
