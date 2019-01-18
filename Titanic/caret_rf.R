library(caret)
set.seed(314159)

load("x_train")
load("x_test")
load("y_train")
train_rf<-cbind(x_train,Survived = as.factor(y_train$Survived))

control <- trainControl(method="repeatedcv", number=10, repeats=30)
mtry <- sqrt(ncol(train_rf))
tunegrid <- expand.grid(.mtry=mtry)

rf_default<-train(Survived~.,data = train_rf, method = "rf", metric = "Accuracy", tuneGrid=tunegrid, trControl=control)
print(rf_default)
