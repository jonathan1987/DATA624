suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(caret)))
suppressWarnings(suppressMessages(library(kableExtra)))
suppressWarnings(suppressMessages(library(elasticnet)))
suppressWarnings(suppressMessages(library(e1071)))
suppressWarnings(suppressMessages(library(glmnet)))

load(file = "train.rda")
load(file = "test.rda")

# use train() with 5-fold, repeated 5 times and find out the minimum RMSE
set.seed(123)
train_control <- trainControl(method = "repeatedcv",
                              number = 5,
                              repeats = 5,
                              search = "random")

PH_enet_model <- train(PH ~.,
                       data = train,
                       method = "glmnet",
                       preProcess = c("center", "scale"),
                       tuneLength = 25,
                       trControl = train_control)

# extract best RMSE value along with alpha, lambda
PH_enet_model$results[which.min(PH_enet_model$results[, "RMSE"]), ] %>%
  kable() %>% kable_styling(fixed_thead = T)

# see the RMSE between predicted PH in the training data and the actual PH
enet_pred_PH <- predict(PH_enet_model, newdata = train)
rmse_train_enet <- postResample(enet_pred_PH, train$PH)

# make predictions on the test dataset on PH
enet_pred_test_PH <- round(predict(PH_enet_model, newdata = test), 2)
write.csv(enet_pred_test_PH, "prediction-PH-enet.csv", col.names = "PH")