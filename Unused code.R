# Neural Networks
library(neuralnet)
# Optimize parameters
set.seed(6)
churn_nnop <- train(churn ~ ., method = "nnet", data = churn,
                     trControl = control)
churn_nnop$bestTune
ggplot(churn_nnop, highlight = TRUE)

# Train
set.seed(3)
churn_nn <- nnet(churn ~ ., size = 5, decay = 0.1, data = train_churn)
summary(churn_nn)

# Test
set.seed(3)
pred_nn <- predict(churn_nn, test_churn, type = "class")

# Check NN accuracy
score_nn <- bind_cols(as.numeric(pred_nn),as.numeric(test_churn$churn)) %>% 
  rename(predict = ...1, test = ...2) %>% 
  mutate(test = replace(test, test == 1, 0)) %>% 
  mutate(test = replace(test, test == 2, 1)) %>% 
  mutate(score = if_else(predict == test, 1, 0))
         
sum(score_nn$score)/1001

# compute accuracy = 0.862

# Validate on the full data set 0.8681
set.seed(4)
val_nn <- predict(churn_nn, churn, type = "class")

score_nn_val <- bind_cols(as.numeric(val_nn),as.numeric(churn_cdp$churn)) %>% 
  rename(predict = ...1, test = ...2) %>% 
  mutate(test = replace(test, test == 1, 0)) %>% 
  mutate(test = replace(test, test == 2, 1)) %>% 
  mutate(score = if_else(predict == test, 1, 0))

modelnnscore <- sum(score_nn_val$score)/10000
modelnnscore
scores <- scores %>%
  add_row(model = 'nn', score = modelnnscore)

# https://stackoverflow.com/questions/51250283/ggplot2-confusion-matrix-conditional-fill
cm1 <- data.frame(cm_knn3_val$table) %>%
  rename(Actual = Reference) %>% 
  group_by(Actual) %>% 
  mutate(
    total = sum(Freq),
    frac_fill = if_else(Prediction == Actual, Freq / total, 0),
    frac = Freq / total * frac_fill) %>%
  mutate(frac_directed = if_else(Prediction == 1, frac_fill * -1, frac_fill)) %>%
  ggplot(aes(Prediction, Actual, fill = frac_directed)) +
  geom_tile(color = "black") +
  geom_text(aes(label = str_c(Freq, ", ", round(frac * 100), "%")), size = 8) +
  scale_fill_gradient2(low = "red", mid = "white", high = "green") +
  scale_x_discrete(position = "top") +
  labs(title = 'KNN Confusion Matrix')
cm1

cm2 <- data.frame(cm_rpart_val$table) %>%
  rename(Actual = Reference) %>% 
  group_by(Actual) %>% 
  mutate(
    total = sum(Freq),
    frac_fill = if_else(Prediction == Actual, Freq / total, 0),
    frac = Freq / total * frac_fill) %>%
  mutate(frac_directed = if_else(Prediction == 1, frac_fill * -1, frac_fill)) %>%
  ggplot(aes(Prediction, Actual, fill = frac_directed)) +
  geom_tile(color = "black") +
  geom_text(aes(label = str_c(Freq, ", ", round(frac * 100), "%")), size = 8) +
  scale_fill_gradient2(low = "red", mid = "white", high = "green") +
  scale_x_discrete(position = "top") +
  labs(title = 'Class. Tree Confusion Matrix')
cm2

cm3 <- data.frame(cm_svm_val$table) %>%
  rename(Actual = Reference) %>% 
  group_by(Actual) %>% 
  mutate(
    total = sum(Freq),
    frac_fill = if_else(Prediction == Actual, Freq / total, 0),
    frac = Freq / total * frac_fill) %>%
  mutate(frac_directed = if_else(Prediction == 1, frac_fill * -1, frac_fill)) %>%
  ggplot(aes(Prediction, Actual, fill = frac_directed)) +
  geom_tile(color = "black") +
  geom_text(aes(label = str_c(Freq, ", ", round(frac * 100), "%")), size = 8) +
  scale_fill_gradient2(low = "red", mid = "white", high = "green") +
  scale_x_discrete(position = "top") +
  labs(title = 'SVM Confusion Matrix')
cm3

cm4 <- data.frame(cm_rf_val$table) %>%
  rename(Actual = Reference) %>% 
  group_by(Actual) %>% 
  mutate(
    total = sum(Freq),
    frac_fill = if_else(Prediction == Actual, Freq / total, 0),
    frac = Freq / total * frac_fill) %>%
  mutate(frac_directed = if_else(Prediction == 1, frac_fill * -1, frac_fill)) %>%
  ggplot(aes(Prediction, Actual, fill = frac_directed)) +
  geom_tile(color = "black") +
  geom_text(aes(label = str_c(Freq, ", ", round(frac * 100), "%")), size = 8) +
  scale_fill_gradient2(low = "red", mid = "white", high = "green") +
  scale_x_discrete(position = "top") +
  labs(title = 'Random Forest Confusion Matrix')
cm4

grid.arrange(cm1, cm2, cm3, cm4, nrow = 2, ncol = 2)