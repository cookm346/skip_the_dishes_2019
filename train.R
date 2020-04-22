data <- read.csv("train.csv", row.names = 1)
# test <- read.csv("test.csv", row.names = 1)

set.seed(12345)
inds <- sample(nrow(data))
explore <- data[inds[1:10000], ]
train <- data[inds[10001:66000], ]
validation <- data[inds[66001:80000], ]

# install.packages("caret")
library(caret)

tc <- trainControl(method='cv', number=10)

#run the model
plsmodel <- train(food_prep_time_minutes ~ ., data=data, method='pls', 
                  metric='RMSE', trControl=tc,
                  tuneGrid = data.frame(ncomp = c(240, 245, 250))) #tuneLength=250
plot(plsmodel)
plsmodel

results <- resamples(list(PLS=plsmodel, PLS2=plsmodel))
summary(results)

#saveRDS of final model
# saveRDS(plsmodel, "PLSall.rds")
# model <- readRDS("PLSall.rds")


MAE(predict(plsmodel, validation), validation$food_prep_time_minutes)
RMSE(predict(plsmodel, validation), validation$food_prep_time_minutes)
R2(predict(plsmodel, validation), validation$food_prep_time_minutes)


#get final predictions
preds <- data.frame(order_id = rownames(test), 
                    food_prep_time_minutes = predict(model, test))

write.csv(preds, "Cook____skipthedishes_food_prep_time_prediction_file.csv", row.names = FALSE)

