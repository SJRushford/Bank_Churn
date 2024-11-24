##############################################################################
## Bank Churn
##
## Scott Rushford
#############################################################################


setwd("~/My Docs/R_Rstudio/Projects/Bank Churn")

citation()
version$version.string

# https://www.kaggle.com/datasets/gauravtopre/bank-customer-churn-dataset

# Load required libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(knitr)
library(tinytex)


# Data url
churn_url = "https://raw.githubusercontent.com/SJRushford/Bank_Churn/refs/heads/main/Bank%20Customer%20Churn%20Prediction.csv"

# Read Data
churn <- as_tibble(read_csv(url(churn_url)))


str(churn)
mean(churn$credit_card)
mean(churn$active_member)
mean(churn$churn)

# Data frame consists of 10,000 rows and 12 columns
# customer ID, Credit Score, Country, Gender, Age, Tenure, Balance,
# Number of Products, Credit Card, Active Member, Estimated Salary and Churn
# Country is a factor of 3 levels: France, Germany, Spain
# Gender is a factor of 2 levels: Male, Female
# Credit Card is a factor of 2 levels represented by 0 = no card, 1 = card
# with a mean of 0.7055
# Active member is a factor of 2 levels represented by 0 = not active
# 1 = active with a mean of 0.5151
# Churn is a factor of 2 levels represented by 0 = customer, 1 = churn
# with a mean of 0.2037

# Correlation Analysis - change character columns to numeric.
churn_CA <- churn %>% 
  mutate(country = replace(country, country == "France", 1),
         country = replace(country, country == "Germany", 2), 
         country = replace(country, country == "Spain", 3))
         
churn_CA <- churn_CA %>% 
  mutate(gender = replace(gender, gender == "Female", 1), 
         gender = replace(gender, gender == "Male", 0))

churn_CA <- churn_CA %>% 
  mutate_if(is.character, as.numeric)

str(churn_CA)
CCA <- cor(churn_CA)
 
summary(CCA)

# Load library corrplot to plot the Correlation Analysis
# reference required
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
library(corrplot)
corrplot(CCA, method = "color")

# According to the plot churn is mostly associated with gender, age and 
# balance

# Principal Component Analysis (PCA)
# Normalize the data for PCA

churn_norm <- as_tibble(scale(churn_CA))

# Remove customer ID as its purpose is to identify a customer only

churn_norm <- churn_norm %>% 
  select(-customer_id)

churn_pca <- prcomp(churn_norm)
summary(churn_pca)
biplot(churn_pca, scale = 0)

# Rename columns for simplicity
churn <- churn %>% 
  rename(id = customer_id, score = credit_score, products = products_number,
         card = credit_card, active = active_member, salary = estimated_salary)

# [@soetewey][@irizarry2019]

# Data Visualization
# Box plots
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
library(gridExtra)

bp1 <- boxplot(score ~ churn, data = churn)
bp1
mean(churn$score)

# mean credit score is 650.53
bp2 <- boxplot(age ~ churn, data = churn)
bp2
mean(churn$age)
# mean age is 38.92 years

bp3 <- boxplot(tenure ~ churn, data = churn)
bp3
mean(churn$tenure)
# mean tenure is 5.01 years

bp4 <- boxplot(balance ~ churn, data = churn)
bp4
mean(churn$balance)
# mean bank balance is 76,485.89

bp5 <- boxplot(products ~ churn, data = churn)
bp5
mean(churn$products)
# mean products is 1.53

bp6 <- boxplot(salary ~ churn, data = churn)
bp6
mean(churn$salary)
# mean salary is 100,090.20

# Histograms
h1 <- churn %>% 
  ggplot(aes(score)) +
  geom_histogram(bins = 10, col = "black", fill = "red")


h2 <- churn %>% 
  ggplot(aes(age)) +
  geom_histogram(bins = 10, col = "black", fill = "red")


h3 <- churn %>% 
  ggplot(aes(tenure)) +
  geom_histogram(bins = 5, col = "black", fill = "red")

h4 <- churn %>% 
  filter(balance > 0) %>% 
  ggplot(aes(balance)) +
  geom_histogram(bins = 10, col = "black", fill = "red")
  

h5 <- churn %>% 
  ggplot(aes(products)) +
  geom_histogram(bins = 4, col = "black", fill = "red")

h6 <- churn %>% 
  ggplot(aes(salary)) +
  geom_histogram(bins = 10, col = "black", fill = "red")

grid.arrange(h1, h2, h3, h4, h5, h6, nrow = 2, ncol = 3)

# Bar Graphs
b1 <- churn %>% 
  ggplot(aes(country)) +
  geom_bar(col = "black", fill = "red")


b2 <- churn %>% 
  ggplot(aes(gender)) +
  geom_bar(col = "black", fill = "red")

b3 <- churn %>% 
  ggplot(aes(active)) +
  geom_bar(col = "black", fill = "red")

b4 <- churn %>% 
  ggplot(aes(churn)) +
  geom_bar(col = "black", fill = "red")

grid.arrange(b1, b2, b3, b4, nrow = 2, ncol = 2)

# Plots
p1 <- churn %>% 
  ggplot(aes(score,churn)) +
  geom_smooth(method = "gam")

p2 <- churn %>% 
  ggplot(aes(age,churn)) +
  geom_smooth(method = "gam")

p3 <- churn %>% 
  ggplot(aes(tenure,churn)) +
  geom_smooth(method = "gam")

p4 <- churn %>% 
  ggplot(aes(balance,churn)) +
  geom_smooth(method = "gam")

p5 <- churn %>% 
  ggplot(aes(products,churn)) +
  geom_smooth(method = lm)

p6 <- churn %>% 
  ggplot(aes(card,churn)) +
  geom_smooth(method = lm)

p7 <- churn %>% 
  ggplot(aes(active,churn)) +
  geom_smooth(method = lm)

p8 <- churn %>% 
  ggplot(aes(salary,churn)) +
  geom_smooth(method = "gam")


grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, nrow = 2, ncol = 4)

# Determine churn accuracy using random models.
# Test picking at random
set.seed(3)
random_n <- rbinom(10000, 1, 0.5)
score_random_n <- bind_cols(as.numeric(random_n),as.numeric(churn$churn)) %>% 
  rename(predict = ...1, test = ...2) %>% 
  mutate(score = if_else(predict == test, 1, 0))
  
score_n <- sum(score_random_n$score)/10000

# Using an equal number of 0s and 1s predicted churn to a 50.02 %
# accuracy.

# Test based on the actual proportions of 0s (79.63%) and 1s (20.37%) in 
# the churn column

set.seed(3)
sum(churn$churn)
p <- mean(churn$churn)
random_p <- rbinom(10000, 1, p)
score_random_p <- bind_cols(as.numeric(random_p),as.numeric(churn$churn)) %>% 
  rename(predict = ...1, test = ...2) %>% 
  mutate(score = if_else(predict == test, 1, 0))

mean(random_p)
score_p <- sum(score_random_p$score)/10000

scores <- data.frame(model=c('random_n', 'random_p'),
                             score=c(0.5002, 0.6733),
                     PPV=c(0,0), NPV=c(0, 0))

print(scores)
#  Generating a random vector consisting of 79.63% 0 and 
# 20.37% 1 predicted churn at an accuracy level of 67.33%

# Test various machine learning algorithms and determine which is the most 
# accurate
# Prepare the churn data set for classification.
churn_cdp <- churn %>% 
  select(-id, -salary) %>% 
  mutate_at(c('country', 'gender', 'card', 'active', 'churn'), as.factor) %>% 
  mutate(across(where(is.numeric), scale))

str(churn_cdp)
            
# Create training, test and validation sets 60:20:20 @caret
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
train_index <- createDataPartition(y = churn_cdp$churn, times = 1, p = 0.6, list = FALSE)
train_churn <- churn_cdp[train_index,]
test_index <- createDataPartition(y = train_churn$churn, times = 1, p = 0.5, list = FALSE)
test_churn <- train_churn[test_index, ]
val_churn <- train_churn[-test_index, ]

str(train_churn)

#[@caret]

# Set training control for optimizing parameters.
control <- trainControl(method = "cv", number = 10, p = .9)

# KNN
# Test for optimal value for k

set.seed(3)
churn_knn <- train(churn ~ ., method = "knn", 
               data = churn_cdp, tuneGrid = data.frame(k = seq(11, 71, 2)))
ggplot(churn_knn, highlight = TRUE)

churn_knn$bestTune
churn_knn$finalModel

# K is optimized at 33.

set.seed(3)
churn_knn3 <- knn3(churn ~ ., k = 33, data = train_churn)
pred_knn3 <- predict(churn_knn3, test_churn, type = "class")
cm_knn3 <- confusionMatrix(pred_knn3, test_churn$churn)
cm_knn3

# Plot kNN https://www.geeksforgeeks.org/contour-of-knn-model-in-ggplot-using-r/
# Define the grid limits use age and balance as they ranked highest in
# the PCA

x_min <- min(train_churn$age) - 1
x_max <- max(train_churn$age) + 1
y_min <- min(train_churn$balance) - 1
y_max <- max(train_churn$balance) + 1

# Create a grid of values
grid <- expand.grid(age = seq(x_min, x_max, by = 0.1),
                    balance = seq(y_min, y_max, by = 0.1))

# Base plot with original data points
base_plot <- ggplot(train_churn, aes(x = age, y = balance, 
                                     color = churn)) +
  geom_point(size = 2) +
  labs(title = "k-NN Decision Boundaries",
       x = "age",
       y = "balance") +
  theme_minimal()
base_plot


# Validate 
set.seed(3)
val_knn3 <- predict(churn_knn3, val_churn, type = "class")
cm_knn3_val <- confusionMatrix(val_knn3, val_churn$churn)
cm_knn3_val

comp_knn3 <- data.frame(Model=c('Test', 'Validate'),
                        Accuracy=c(cm_knn3$overall["Accuracy"],
                                   cm_knn3_val$overall["Accuracy"]),
                        CI_Lower=c(cm_knn3$overall["AccuracyLower"],
                               cm_knn3_val$overall["AccuracyLower"]),
                        CI_Upper=c(cm_knn3$overall["AccuracyUpper"],
                                   cm_knn3_val$overall["AccuracyUpper"]),
                        PPV=c(cm_knn3$byClass['Pos Pred Value'],
                           cm_knn3_val$byClass['Pos Pred Value']), 
                     NPV=c(cm_knn3$byClass['Neg Pred Value'],
                           cm_knn3_val$byClass['Neg Pred Value']))


print(comp_knn3)
scores <- scores %>%
  add_row(model = 'knn', score = cm_knn3_val$overall["Accuracy"],
          PPV = cm_knn3_val$byClass['Pos Pred Value'], 
          NPV = cm_knn3_val$byClass['Neg Pred Value'])

# Classification Trees
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
library(rpart)

# Optimize parameters
set.seed(4)
churn_ct <- train(churn ~ .,method = "rpart",
              tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
              data = churn_cdp)

ggplot(churn_ct, hightlight = TRUE)
churn_ct$bestTune


# Train
set.seed(3)
churn_rpart <- rpart(churn ~ ., cp = 0.004166667, data = train_churn)
printcp(churn_rpart)

# map tree book 479 [@adler]
if(!require(maptree)) install.packages("maptree", repos = "http://cran.us.r-project.org")
library(maptree)

draw.tree(churn_rpart, cex = 0.5, nodeinfo = TRUE)
# root node error = 0.20369
set.seed(4)
pred_rpart <- predict(churn_rpart, test_churn, type = "class")
cm_rpart <- confusionMatrix(pred_rpart, test_churn$churn)
cm_rpart


# Validate
set.seed(4)
val_rpart <- predict(churn_rpart, val_churn, type = "class")
cm_rpart_val <- confusionMatrix(val_rpart, val_churn$churn)
cm_rpart_val


# Compare Test and Validation 
comp_rpart <- data.frame(Model=c('Test', 'Validate'),
                        Accuracy=c(cm_rpart$overall["Accuracy"],
                                   cm_rpart_val$overall["Accuracy"]),
                        CI_Lower=c(cm_rpart$overall["AccuracyLower"],
                               cm_rpart_val$overall["AccuracyLower"]),
                        CI_Upper=c(cm_rpart$overall["AccuracyUpper"],
                                   cm_rpart_val$overall["AccuracyUpper"]),
                        PPV=c(cm_knn3$byClass['Pos Pred Value'],
                           cm_knn3_val$byClass['Pos Pred Value']), 
                     NPV=c(cm_knn3$byClass['Neg Pred Value'],
                           cm_knn3_val$byClass['Neg Pred Value']))


print(comp_rpart)

# Add Accuracy PPV and NPV
scores <- scores %>%
  add_row(model = 'ctree', score = cm_rpart_val$overall["Accuracy"],
          PPV = cm_rpart_val$byClass['Pos Pred Value'], 
          NPV = cm_rpart_val$byClass['Neg Pred Value'])


#SVM
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(kernlab)) install.packages("kernlab", repos = "http://cran.us.r-project.org")
library(e1071)
library(kernlab)

# Optimize svm parameters radial method
set.seed(4)
churn_svmtr <- train(churn ~ ., method = "svmRadial", data = churn_cdp,
                     trControl = control)
churn_svmtr$bestTune
ggplot(churn_svmtr, highlight = TRUE)

# Train
set.seed(3)
churn_svm <- svm(churn ~ ., sigma = 0.06574296, C = 1, data = train_churn)
summary(churn_svm)



# Test
set.seed(3)
pred_svm <- predict(churn_svm, test_churn, type = "class")
cm_svm <- confusionMatrix(pred_svm, test_churn$churn)
cm_svm

# Validate
set.seed(4)
val_svm <- predict(churn_svm, val_churn, type = "class")
cm_svm_val <- confusionMatrix(val_svm, val_churn$churn)
cm_svm_val

# Compare Test and Validate
comp_svm <- data.frame(Model=c('Test', 'Validate'),
                        Accuracy=c(cm_svm$overall["Accuracy"],
                                   cm_svm_val$overall["Accuracy"]),
                        CI_Lower=c(cm_svm$overall["AccuracyLower"],
                               cm_svm_val$overall["AccuracyLower"]),
                        CI_Upper=c(cm_svm$overall["AccuracyUpper"],
                                   cm_svm_val$overall["AccuracyUpper"]),
                        PPV=c(cm_svm$byClass['Pos Pred Value'],
                           cm_svm_val$byClass['Pos Pred Value']), 
                     NPV=c(cm_svm$byClass['Neg Pred Value'],
                           cm_svm_val$byClass['Neg Pred Value']))


print(comp_svm)

# Add SVM Accuracy, PPV NPV to scores
scores <- scores %>%
  add_row(model = 'svm', score = cm_svm_val$overall["Accuracy"],
          PPV = cm_svm_val$byClass['Pos Pred Value'], 
          NPV = cm_svm_val$byClass['Neg Pred Value'])


# load library for random forest
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")
library(randomForest)

# use cross validation to optimize parameter

library(Rborist)
set.seed(3)
churn_rf <- train(churn ~ ., method = "Rborist",
      tuneGrid = data.frame(predFixed = 2, minNode = c(140, 200)),
                            data = churn_cdp)
churn_rf$bestTune
# minNode = 140

# Optimize mtry using tuneRF - found while reviewing randomForest
# package.

x <- churn %>% 
  select(-churn)

rf_tune <- tuneRF(x, churn_cdp$churn, mtryStart = 1 , ntreeTry=200, stepFactor=1, improve=0.05,
       trace=TRUE, plot=TRUE, doBest=FALSE)
prf1 <- plot(churn_rf)
prf1
plot(rf_tune)

# Create model with optimized parameters
set.seed(3)
churn_rft <- randomForest(churn ~ ., predFixed = 2, minNode = 140, mtry = 4,
                          ntree = 200, data = train_churn)
summary(churn_rft)

# Test model 
set.seed(3)
pred_rf <- predict(churn_rft, test_churn, type = "class")
cm_rf <- confusionMatrix(pred_rf, test_churn$churn)
cm_rf


# Validate
set.seed(4)
val_rf <- predict(churn_rft, val_churn, type = "class")
cm_rf_val <- confusionMatrix(val_rf, val_churn$churn)
cm_rf_val

# Compare Test and Validate
comp_rf <- data.frame(Model=c('Test', 'Validate'),
                        Accuracy=c(cm_rf$overall["Accuracy"],
                                   cm_rf_val$overall["Accuracy"]),
                        CI_Lower=c(cm_rf$overall["AccuracyLower"],
                               cm_rf_val$overall["AccuracyLower"]),
                        CI_Upper=c(cm_rf$overall["AccuracyUpper"],
                                   cm_rf_val$overall["AccuracyUpper"]),
                        PPV=c(cm_rf$byClass['Pos Pred Value'],
                           cm_rf_val$byClass['Pos Pred Value']), 
                     NPV=c(cm_rf$byClass['Neg Pred Value'],
                           cm_rf_val$byClass['Neg Pred Value']))


print(comp_rf)


scores <- scores %>%
  add_row(model = 'rf', score = cm_rf_val$overall["Accuracy"],
          PPV = cm_rf_val$byClass['Pos Pred Value'], 
          NPV = cm_rf_val$byClass['Neg Pred Value'])

# Visualize the results

s1 <-  scores %>% 
  ggplot(aes(reorder(model,score), score)) +
  geom_bar(stat = 'identity', col = 'black', fill = 'red') +
  labs(x = 'Model' , y = 'Score', title = 'Model Accuracy')

s2 <-  scores %>% 
  filter(PPV > 0) %>% 
  ggplot(aes(reorder(model, PPV), PPV)) +
  geom_bar(stat = 'identity', col = 'black', fill = 'red') +
  labs(x = 'Model' , y = 'PPV', title = 'Model PPV')

s3 <-  scores %>% 
  filter(NPV > 0) %>% 
  ggplot(aes(reorder(model, NPV), NPV)) +
  geom_bar(stat = 'identity', col = 'black', fill = 'red') +
  labs(x = 'Model' , y = 'NPV', title = 'Model NPV')

grid.arrange(s1, s2, s3, ncol = 3)

score_graph
str(scores)



