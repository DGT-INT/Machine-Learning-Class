library(tidyverse)
library(caret)
library(glmnet)
library(rpart)
library(pROC)
library(MLmetrics)

credit_final <- read_csv("credit_final.csv")

credit_final <- credit_final |>
  mutate(across(where(is.character), as.factor))

credit_final <- credit_final |>
  mutate(Approval = ifelse(ever_defaulted == 0, 1, 0))

credit_final <- credit_final |>
  drop_na()

credit_final$Approval <- as.numeric(credit_final$Approval)

set.seed(123)
train_index <- createDataPartition(credit_final$Approval, p = 0.8, 
                                   list = FALSE)
train_data <- credit_final[train_index, ]
test_data <- credit_final[-train_index, ]

#------------Regression Models----------------------
# Linear Regression---------------------------------

lm_model <- lm(Approval ~ ., data = train_data)
lm_preds <- predict(lm_model, newdata = test_data)
lm_class <- ifelse(lm_preds > 0.5, 1, 0)

# Ridge Regression----------------------------------

x_train <- model.matrix(Approval ~ ., data = train_data)[, -1]
y_train <- train_data$Approval

x_test <- model.matrix(Approval ~ ., data = test_data)[, -1]
y_test <- test_data$Approval

ridge_model <- cv.glmnet(x_train, y_train, alpha = 0)
ridge_preds <- as.numeric(predict(ridge_model, s = ridge_model$lambda.min, 
                                  newx = x_test))

ridge_class <- ifelse(ridge_preds > 0.5, 1, 0)

# Decision Tree Regression-------------------------------

tree_model <- rpart(Approval ~ ., data = train_data)
tree_preds <- predict(tree_model, newdata = test_data)
tree_class <- ifelse(tree_preds > 0.5, 1, 0)

# Evaluation--------------------------------------------------

evaluate_model <- function(preds, probs, truth, model_name) {
  cat("====", model_name, "====\n")
  print(confusionMatrix(factor(preds, levels = c(0,1)), 
                        factor(truth, levels = c(0,1))))
  ROC_obj <- roc(truth, probs)
  cat("AUC:", auc(ROC_obj), "\n\n")
}

evaluate_model(lm_class, lm_preds, test_data$Approval, "Linear Regression")
evaluate_model(ridge_class, ridge_preds, 
               test_data$Approval, "Ridge Regression")
evaluate_model(tree_class, tree_preds, 
               test_data$Approval, "Decision Tree Regression")

#------------------------------------------------------------------------

credit_final$Approval <- ifelse(credit_final$ever_defaulted == 1, 0, 1)
credit_final <- credit_final |> drop_na()
credit_final <- credit_final |>
  mutate(across(where(is.character), as.factor))


set.seed(123)
train_index <- createDataPartition(credit_final$Approval, 
                                   p = 0.8, list = FALSE)
train_data <- credit_final[train_index, ]
test_data <- credit_final[-train_index, ]

#------------Classification Models------------------
# Logistic Regression-----------------------------------------------

library(glmnet)

x_train <- model.matrix(Approval ~ ., data = train_data)[, -1]
y_train <- as.numeric(as.character(train_data$Approval))

train_data$Approval <- factor(train_data$Approval, levels = c(0, 1))
test_data$Approval  <- factor(test_data$Approval,  levels = c(0, 1))

lasso_model <- cv.glmnet(
  x_train,
  y_train,
  alpha = 1,
  family = "binomial",
  type.measure = "auc",
  nfolds = 5,
  lambda.min.ratio = 1e-3,
  maxit = 2e5)

ridge_model <- cv.glmnet(x_train, y_train, alpha = 0, family = "binomial")  
ridge_probs <- predict(ridge_model, newx = x_test, s = "lambda.min", type = "response")
ridge_preds <- ifelse(ridge_probs > 0.5, 1, 0)

x_test <- model.matrix(Approval ~ ., data = test_data)[, -1]
lasso_probs <- predict(lasso_model, s = lasso_model$lambda.min, newx = x_test, type = "response")
lasso_preds <- ifelse(lasso_probs > 0.5, 1, 0)

# Random Forest---------------------------------------------------------

library(randomForest)

RF_model <- randomForest(Approval ~ ., data = train_data, ntree = 100)
RF_preds <- predict(RF_model, newdata = test_data)  
RF_probs <- predict(RF_model, newdata = test_data, type = "prob")[,2]  

# Gradient Boosting----------------------------------------------------

library(gbm)

train_data$Approval <- as.numeric(as.character(train_data$Approval))
GBM_model <- gbm(
  Approval ~ ., 
  data = train_data, 
  distribution = "bernoulli", 
  n.trees = 100,
  interaction.depth = 3,
  shrinkage = 0.1,
  cv.folds = 5,
  verbose = FALSE
)

best_iter <- gbm.perf(GBM_model, method = "cv")  
GBM_probs <- predict(GBM_model, newdata = test_data, n.trees = best_iter, 
                     type = "response")
GBM_preds <- ifelse(GBM_probs > 0.5, 1, 0)

# Evaluation--------------------------------------------------

evaluate_model <- function(preds, probs, truth, model_name) {
  cat("====", model_name, "====\n")
  print(confusionMatrix(factor(preds, levels = c(0,1)), truth, positive = "1"))
  ROC_obj <- roc(as.numeric(as.character(truth)), as.numeric(probs))
  cat("AUC:", auc(ROC_obj), "\n\n")
}

evaluate_model(lasso_preds, lasso_probs, test_data$Approval,
               "Lasso Logistic Regression")
evaluate_model(ridge_preds, ridge_probs, test_data$Approval, 
               "Ridge Logistic Regression")
evaluate_model(RF_preds, RF_probs, test_data$Approval, "Random Forest")
evaluate_model(GBM_preds, GBM_probs, test_data$Approval, "Gradient Boosting")
#----------------------------------------------------------------------------

y_true <- as.numeric(as.character(test_data$Approval))
y_true <- test_data$Approval

ROC_lasso <- roc(y_true, as.vector(lasso_probs))
ROC_ridge <- roc(y_true, as.vector(ridge_probs))
ROC_RF     <- roc(y_true, RF_probs)
ROC_gbm    <- roc(y_true, GBM_probs)

ROC_data <- data.frame(
  FPR = c(1 - ROC_lasso$specificities,
          1 - ROC_ridge$specificities,
          1 - ROC_RF$specificities,
          1 - ROC_gbm$specificities),
  TPR = c(ROC_lasso$sensitivities,
          ROC_ridge$sensitivities,
          ROC_RF$sensitivities,
          ROC_gbm$sensitivities),
  Model = factor(rep(c("Lasso", "Ridge", "Random Forest", "GBM"),
                     times = c(length(ROC_lasso$sensitivities),
                               length(ROC_ridge$sensitivities),
                               length(ROC_RF$sensitivities),
                               length(ROC_gbm$sensitivities))))
)

ggplot(ROC_data, aes(x = FPR, y = TPR, color = Model)) +
  geom_line(size = 1.2) +
  geom_abline(linetype = "dashed", color = "black") +
  labs(
    title = "ROC Curves for Classification Models",
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")



#•	Evaluation: Compare models using accuracy, precision, recall, F1-score, and ROC-AUC. Analyze feature importance and model interpretability.





#•	Bias and Fairness Checks: Assess whether sensitive attributes (e.g., gender) influence predictions unfairly.









