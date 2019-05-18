suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(caret)))
suppressWarnings(suppressMessages(library(kableExtra)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(leaps)))
suppressWarnings(suppressMessages(library(glmnet)))

load(file = "train.rda")
load(file = "test.rda")

summary(train)
str(train)
dim(train)

# go through every subset and possible combination of features for a linear model
lm_student_train <- regsubsets(PH ~ ., data = train,
                               nvmax = 32, method = "forward")

# let's loop through each one to see what is the best model from calculating
# the RMSE's from the subsets

# store RMSE's from each subset model
rmse <- rep(NA,29)
train.matrix <- model.matrix(PH ~ ., data=train)

for (i in 1:29){
  coef_i <- coef(lm_student_train, id = i) # get the coefficients of the model
  pred_i <- train.matrix[, names(coef_i)] %*% coef_i # training data model predictions
  rmse[i] <- sqrt(mean((pred_i - train$PH)^2))
}
# return the number of features that has the smalles RMSE from the best subset
# of linear models

# plot the RMSE for each number of features for each model

ggplot(data = data.frame(features=seq(1,29), rmse=rmse),
       aes(x=features,y=rmse)) +
  geom_point() +
  geom_line() +
  xlab("Number of Features") +
  ylab("") +
  ggtitle("RMSE for various features") +
  theme_bw() +
  theme_classic()

print(paste("Number of features that make the best model and rmse: ",
            which.min(rmse), sep = " "))

# list the rmse as well
print(paste("Smallest RMSE: ", round(rmse[which.min(rmse)], 4), sep = " "))

# list the coefficients of the model.
#coef(lm_student_train, which.min(rmse)) %>% kable() %>%
#  kable_styling(position = "center")

# recreate the model with the given coefficients
# this is so we can use the predict function to predict the PH levels in the test
# dataset
features <- names(coef(lm_student_train, which.min(rmse)))[6:(ncol(train)-2)] # take out the intercept

# formula to use to properly pass to predict() function
form <- as.formula(paste("PH", paste("Brand.Code +" ,
                                     paste(features, collapse = " + "))
                         ,sep = " ~ "))

lm_PH <- lm(form, data = train)
pred_PH_regsubsets <- round(predict(lm_PH, test), 2)
write.table(pred_PH_regsubsets,
            "predicted-PH-regsubsets.csv",
            col.names = c("PH"),
            row.names = F)

# Variable Importance
varImp_regsubsets <- varImp(lm_PH)
varImp_regsubsets <- data.frame(Features=rownames(varImp_regsubsets),
                                Importance=varImp_regsubsets$Overall)
varImp_regsubsets <- varImp_regsubsets[order(-varImp_regsubsets$Importance),]

varImp_regsubsets %>% kable() %>% kable_styling()
