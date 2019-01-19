library(caret)
library(deepboost)

seed=1
load("x_train")
load("x_test")
load("y_train")
train_deepboost<-cbind(x_train,Survived = as.factor(y_train$Survived))

set.seed(seed)
deepboost_mine<-deepboost(Survived~.,data = train_deepboost,tree_depth = 50000, num_iter = 1000)
print(deepboost_mine)

#public accuracy of 80.38%

#make predictions on test data
deepboost_predictions<-as.numeric(predict(deepboost_mine,newdata = x_test))-1

#clean
#clean up formatting for submission
test_data<-read.csv("test.csv")
write.csv(
      cbind(PassengerId = test_data$PassengerId,data.frame(Survived = deepboost_predictions+1)),
      file = "Titanic_submission_V6.csv",
      row.names = FALSE)