library(caret)
library(dplyr)

unzip("Titanic_Raw_Data.zip")

train_data<-read.csv("train.csv")
test_data<-read.csv("test.csv")

#I'm combining the two datasets for the purpose of data cleaning and feature creation so that both sets can run through the final model.
#I'll split the test set back to what is was at the end of this script.

new_data<- rbind(
                  cbind(train_data,Dataset = rep("train",nrow(train_data))),
                  cbind(test_data,Dataset = rep("test",nrow(test_data)),
                        Survived = rep(0,nrow(test_data)))
                  )

summary(new_data)

#--------------------Cleaning Approach--------------------------------------------
#Passenger ID, Name, and Ticket appear to be useless for machine learning, 
#although Last Name may be valuable in terms of families surviving together.

#Categorical variables PClass, Sex, and Embarked will need to be one-hot encoded.

#Cabin, although mostly blank, may offer valuable data in terms of its lettering.
#I'll also one-hot encode this variable.

#Age also has many NAs that I'll replace with the average of all other ages.

#Finally, I'll scale each of the numeric variables and save the cleaned dataset.
#--------------------------------------------------------------------------------

#-----------------------------Cleaning-------------------------------------------
new_data[order(new_data$Embarked)[1:2],]
#There are two ' ' embarquement values. 
#These records otherwise look valid, so I'll replace ' ' with the mode of Embark.

new_data[order(new_data$Embarked)[1:2], ncol(new_data)-1] =factor('S')
#This will drop the unused level so it doesn't feed to dummyVars
new_data$Embarked<-factor(new_data$Embarked)
summary(new_data$Embarked)

#Replacing missing ages with the mean age.
mean_age<-mean(new_data$Age[complete.cases(new_data$Age)])
new_data$Age[!complete.cases(new_data$Age)] = mean_age
summary(new_data$Age)

#Extracting the letter of the first cabin for each person
cabin_letter<-factor(substr(new_data$Cabin,1,1))
summary(cabin_letter)
new_data<-cbind(new_data,cabin_letter)

#Replacing the NA Fare with the average Fare
mean_fare<-mean(new_data$Fare[complete.cases(new_data$Fare)])
new_data$Fare[!complete.cases(new_data$Fare)] = mean_fare
summary(new_data$Fare)
#-------------------------------------------------------------------------------


#-----------------------------One Hot Encoding----------------------------------
new_data<-cbind(new_data,
                 predict(
                       dummyVars(~factor(Pclass),
                                 data = new_data, 
                                 fullRank = TRUE),
                       new_data)
                 )
new_data<-cbind(new_data,
                 predict(
                       dummyVars(~Embarked,
                                 data = new_data, 
                                 fullRank = TRUE),
                       new_data)
)

new_data<-cbind(new_data,
                 predict(
                       dummyVars(~cabin_letter,
                                 data = new_data, 
                                 fullRank = TRUE),
                       new_data)
)


new_data<-cbind(new_data,
                 predict(
                       dummyVars(~Sex,
                                 data = new_data, 
                                 fullRank = TRUE),
                       new_data)
)
#-------------------------------------------------------------------------------------

names(new_data)
#Center and scale the variables that are not one-hot encoded
for (i in c(6:8,10)){
      new_data[,i]<-(new_data[,i]-mean(new_data[,i]))/ sd(new_data[,i])
      print(summary(new_data[,i]))
}

feature_order = names(new_data)[c(6:8,10,15:27)]

#Select only the features we still need
x_test = select(filter(new_data,Dataset == "test"),feature_order)

#Shuffle dataframe to avoid sequencing coincidences
shuffle_train<-sample(filter(new_data,Dataset == "train"))
x_train = select(shuffle_train,feature_order)
y_train = select(shuffle_train,Survived)

summary(x_train)

save(x_train, file = "x_train")
save(y_train, file = "y_train")
save(x_test, file = "x_test")
