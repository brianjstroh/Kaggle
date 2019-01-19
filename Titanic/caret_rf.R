library(caret)

seed=1
load("x_train")
load("x_test")
load("y_train")
train_rf<-cbind(x_train,Survived = as.factor(y_train$Survived))

set.seed(seed)
control <- trainControl(method="repeatedcv", number=50, repeats=10)
tunegrid <- expand.grid(.mtry=c(1:15))
rf_mine<-train(Survived~.,data = train_rf, method = "rf", metric = "Accuracy", tuneGrid = tunegrid, trControl=control)
print(rf_mine)

#final accuracy of 83.58%

#make predictions on test data
rf_predictions<-as.numeric(predict(rf_mine,newdata = x_test))-1

#clean
#clean up formatting for submission
test_data<-read.csv("test.csv")
write.csv(
      cbind(PassengerId = test_data$PassengerId,data.frame(Survived = rf_predictions)),
      file = "Titanic_submission_V4.csv",
      row.names = FALSE)