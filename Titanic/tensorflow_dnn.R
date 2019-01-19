library(tensorflow)
library(keras)
library(dplyr)

seed=314159
load("x_train")
load("x_test")
load("y_train")

#set seed for reproducibility
set.seed(seed)
use_session_with_seed(seed)

#defining the model and layers
model <- keras_model_sequential() %>% 
      layer_dense(units = 19, activation = "relu", input_shape = c(19)) %>%
      layer_dropout(.3)%>%
      layer_dense(units = 9, activation = "relu") %>%
      layer_dropout(.3)%>%
      layer_dense(units = 9, activation = "relu") %>%
      layer_dense(units = 9, activation = "relu") %>%
      layer_dense(units = 1, activation = "sigmoid")

#define loss and optimizer
model %>% compile(
      optimizer = optimizer_adam(),
      loss = "binary_crossentropy",
      metrics = c("accuracy")
)

#fit the model
train_log <- model %>% fit(
      as.matrix(x_train),
      as.vector(y_train$Survived),
      epochs = 150,
      validation_split = .25
)

#make final predictions
predictions_test <- predict_classes(model,as.matrix(x_test))


#clean up formatting for submission
test_data<-read.csv("test.csv")
write.csv(
      cbind(test_data,data.frame(Survived = predictions_test)),
      file = "Titanic_submission_V6.csv",
      row.names = FALSE)
