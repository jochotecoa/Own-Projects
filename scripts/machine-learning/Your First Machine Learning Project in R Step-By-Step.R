# install.packages('caret', dependencies=c("Depends", "Suggests"))
library(caret)
library(dplyr)
data("iris")
dataset = iris



# 2.3. Create a Training Dataset ------------------------------------------


# create a list of 80% of the rows in the original dataset we can use for training
training_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- dataset[-training_index,]
# use the remaining 80% of data to training and testing the models
dataset <- dataset[training_index,]

# 3. Summarize Dataset ----------------------------------------------------


# dimensions of dataset
dim(dataset)

# list types for each attribute
sapply(dataset, class)

# take a peek at the first 5 rows of the data
head(dataset)

# list the levels for the class
levels(dataset$Species)

dataset$Species %>% 
  table() %>% 
  cbind(., 
        prop.table(.))

# summarize attribute distributions
summary(dataset)

# 4. Visualize Dataset ----------------------------------------------------


par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(dataset[,i], main=colnames(dataset)[i])
}

par(mfrow= c(1,1))
plot(dataset$Species)

x = dataset[, 1:4]
y = dataset[, 5]

featurePlot(x, y, plot = 'ellipse')

# box and whisker plots for each attribute
featurePlot(x=x, y=y, plot="box")

# density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

# 5. Evaluate Some Algorithms ---------------------------------------------


# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# a) linear algorithms
set.seed(7)
fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)

# summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

