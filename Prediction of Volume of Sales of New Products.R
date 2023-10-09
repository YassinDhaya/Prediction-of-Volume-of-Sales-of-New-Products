existing_products <- read.csv("C:/Users/yassin/Desktop/salesproject/existingproductattributes2017.csv")
#import the new products dataset

new_products <- read.csv("C:/Users/yassin/Desktop/salesproject/newproductattributes2017.csv")
#load the Caret package again - try to always do this so that it will be activated for the task

library(caret)
## Loading required package: lattice
## Loading required package: ggplot2
#dumify the data - typical datasets don’t contain only numeric values. Most data will contain a mixture of numeric and nominal data. Dumifying helps to incorporate both (numeric and nominal data) for developing regression models and making predictions.

#How to dumify the data - convert categorical variables (factor and character variables) to binary variables using the process below

#dumify the data - step1 - create a new dataframe made up of dummy variables from the exisiting products data

newDataFrame <- dummyVars(" ~ .", data = existing_products)
#next integrate the dummy variables df called newdataframe into the existing products dataframe and assign all to a new name called ready dataframe

readyData <- data.frame(predict(newDataFrame, newdata = existing_products))
#cross-check to ensure there are no nominal variables-check the structure

str(readyData)
#Check for missing data - all the columns/sections with NA’s

summary(readyData)
#delete all columns with missing data

readyData$ProductHeight <- NULL
readyData$BestSellersRank <- NULL
#Find the correlation between the relevant independent variables and the dependent variable

corrData <- cor(readyData)
#call the data

corrData
#visualize the correlation matrix using a heat map

install.packages(“corrplot”)

#load corrplot- a correllation matrix heatwave creator

library(corrplot)
## corrplot 0.84 loaded
#call the corrplot matrix for the data

corrplot(corrData)
#blue (cooler) colors show a positive relationship and red (warmer) colors indicate more negative relationships #create training and test sets after allowing for creation of random numbers using set seed

set.seed(123)
#assign names and calculate the taining size and test size

trainSize <- round(nrow(readyData)*0.7)
testSize <- round(nrow(readyData)- trainSize)
#check training and test size

trainSize
testSize
#train the dataset

training_indices<-sample(seq_len(nrow(readyData)),size =trainSize)
#Assign the training and test data into the names trainingset and testsize

trainSet<-readyData[training_indices,]
testSet<-readyData[-training_indices,]
#run linear regression model

readydata_LM<-lm(Volume ~ ., trainSet)
#check the outcome of the linear regression model

readydata_LM
#get a summary of the content of the finding

summary(readydata_LM)
#randomize the dataset

set.seed(123)
#assign names and calculate the taining size and test size

trainSize <- round(nrow(readyData)*0.7)
testSize <- round(nrow(readyData)- trainSize)
#check training and test size

trainSize
testSize
#train the dataset

training_indices<-sample(seq_len(nrow(readyData)),size =trainSize)
#Assign the training and test data into the names trainingset and testsize

trainSet<-readyData[training_indices,]
testSet<-readyData[-training_indices,]
#10 fold cross validation

fit_Control <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
#train Random Forest Regression model with a tuneLenght = 1 (trains with 1 mtry value for RandomForest)

readydata_rf <- train(Volume ~ ., data = trainSet, method = "rf", trControl=fit_Control, tuneLength = 1, importance = T)
#training results

readydata_rf
#see asummary of the model created

summary(readydata_rf)
#predict the findings

readydata_rf_predict<-predict(object = readydata_rf, newdata=testSet, na.action = na.pass)
#to see all the predictions

readydata_rf_predict
#this tells the most important independent variables

varImp(readydata_rf)
#Error check 1 - test of testSet result

postResample(testSet$Volume, readydata_rf_predict)
#Error check 2 - test of trainSet result

readydata_rf_predict2 <- predict(object = readydata_rf, 
                                 newdata = trainSet)


postResample(testSet$Volume, readydata_rf_predict2)
#Error check 3 - confusion matrix - not working confusionMatrix(table(testSet$Volume, readydata_rf_predict))

#because the confusion matrix directly is giving an error, then use the union function to unify it

U <- union(readydata_rf_predict, testSet$Volume)
#create a model that incorporates the both items as factor and adds the unifying model done earlier readydata_conf_matrix <- table(factor(readydata_KNN_predict, U), factor(testSet$Volume, U))

#run the confusion matrix confusionMatrix(readydata_conf_matrix)

#KNN

#randomize the dataset

set.seed
#assign names and calculate the taining size and test size

trainSize <- round(nrow(readyData)*0.7)
testSize <- round(nrow(readyData)- trainSize)
#check training and test size

trainSize
testSize
#train the dataset

training_indices<-sample(seq_len(nrow(readyData)),size =trainSize)
#Assign the training and test data into the names trainingset and testsize

trainSet<-readyData[training_indices,]
testSet<-readyData[-training_indices,]
#10 fold cross validation

fit_Control <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
#train Random Forest Regression model with a tuneLenght = 1 (trains with 1 mtry value for RandomForest)

readydata_KNN <- train(Volume ~ ., data = trainSet, method = "knn", trControl=fit_Control, tuneLength = 1)
#training results

readydata_KNN
#this helps you to see all the data

summary(readydata_KNN)
#predict the findings

readydata_KNN_predict<-predict(object = readydata_KNN, newdata=testSet, na.action = na.pass)
#to see all the predictions

readydata_KNN_predict
#this tells the most important independent variables

varImp(readydata_KNN)
#Error check 1 - test of testSet result

postResample(testSet$Volume, readydata_KNN_predict)
#Error check 2 - test of trainSet result

readydata_KNN_predict2 <- predict(object = readydata_KNN, 
                                  newdata = trainSet)

postResample(testSet$Volume, readydata_KNN_predict2)
#Error check 3 - confusion matrix - not yet working confusionMatrix(table(testSet$Volume, readydata_KNN_predict))

#because the confusion matrix directly is giving an error, then use the union function to unify it

U <- union(readydata_KNN_predict, testSet$Volume)
#create a model that incorporates the both items as factor and adds the unifying model done earlier

readydata_conf_matrix <- table(factor(readydata_KNN_predict, U), factor(testSet$Volume, U))
#run the confusion matrix

confusionMatrix(readydata_conf_matrix)
#svm

#this helps to randomize the dataset

set.seed(123)
#assign names and calculate the taining size and test size

trainSize <- round(nrow(readyData)*0.7)
testSize <- round(nrow(readyData)- trainSize)
#check training and test size

trainSize
testSize
#train the dataset

training_indices<-sample(seq_len(nrow(readyData)),size =trainSize)
#Assign the training and test data into the names trainingset and testsize

trainSet<-readyData[training_indices,]
testSet<-readyData[-training_indices,]
#10 fold cross validation

fit_Control <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
#train Random Forest Regression model with a tuneLenght = 1 (trains with 1 mtry value for RandomForest)

readydata_svm <- caret::train(Volume ~ ., 
                              data = trainSet, 
                              method = 'svmLinear', 
                              trControl=fit_Control)
#training results - run the findings

readydata_svm
summary(readydata_svm)
#predict the findings

readydata_svm_predict<-predict(object = readydata_svm, newdata=testSet, na.action = na.pass)
#to see all the predictions

readydata_svm_predict
#this tells the most important independent variables

varImp(readydata_svm)
#Error check 1 - test of testSet result

postResample(testSet$Volume, readydata_svm_predict)
#Error check 2 - test of trainSet result

readydata_svm_predict2 <- predict(object = readydata_svm, 
                                  newdata = trainSet)
postResample(testSet$Volume, readydata_svm_predict2)
#Error check 3 - confusion matrix - not yet working round(prop.table(confusionMatrix(testSetVolume,readydatasvmpredict)
table))

confusionMatrix(table(testSet$Volume, readydata_svm_predict))

#because the confusion matrix directly is giving an error, then use the union function to unify it

U <- union(readydata_svm_predict, testSet$Volume)
#create a model that incorporates the both items as factor and adds the unifying model done earlier

readydata_conf_matrix <- table(factor(readydata_svm_predict, U), factor(testSet$Volume, U))
#run the confusion matrix

confusionMatrix(readydata_conf_matrix)
#Error check 4 - ggplot (this can be used to compare all the final results - this is done at the end of the process)

#the tested models are then run on the other data set

new_products <- read.csv("C:/Users/gebruiker/Desktop/Ubiqum_1/newproductattributes2017.csv")
#call the caret folder

library(caret)
#next integrate the dummy variables df called newdataframe into the existing products dataframe and assign all to a new name called ready dataframe

readyData_newprod <- data.frame(predict(newDataFrame, newdata = new_products))
#cross-check to ensure there are no nominal variables check the structure

str(readyData_newprod)
#Check for missing data

summary(readyData_newprod)
#delete all missing data

readyData_newprod[is.na(readyData_newprod)] <- 0
#Find the correlation between the relevant independent variables and the dependent variable

corrData <- cor(readyData_newprod)
## Warning in cor(readyData_newprod): the standard deviation is zero
#call the correlation of the dataset

corrData
#note: Correlation values fall within -1 and 1 with variables have string positive relationships having correlation values closer to 1 and strong negative relationships with values closer to -1.

#correlation matrix using a heat map

install.packages(“corrplot”)

library(corrplot)
#Do a plot tof the data

corrplot(corrData)
#predict the findings for the new product rf - trained model name (is known as the object) and the dataset is meant to be inside the bracket

readydata_rf_predict_newprod<-predict(object = readydata_rf, newdata=readyData_newprod, na.action = na.pass)
#to see all the predictions

readydata_rf_predict_newprod
#predict the findings for the new product KNN - trained model name and the dataset is meant to be inside the bracket

readydata_KNN_predict_newprod<-predict(object = readydata_KNN, newdata=readyData_newprod, na.action = na.pass)
#to see all the predictions

readydata_KNN_predict_newprod
#predict the findings for the new product SVM - trained model name and the dataset is meant to be inside the bracket

readydata_svm_predict_newprod<-predict(object = readydata_svm, newdata=readyData_newprod, na.action = na.pass)
#to see all the predictions

readydata_svm_predict_newprod
#steps to add data/findings to the dataset. How: new prod file is assigned into a new file named new prod plus predictions #first we add for rnadom forest

newprod_pluspredictionsrf <- readyData_newprod
#Then u add the final predictions from each of the models (rf)

newprod_pluspredictionsrf$pred <- predict(object = readydata_rf, newdata = readyData_newprod)
#here we add the predicted rf data to the predictions column in the new prod plus prediction file

newprod_pluspredictionsrf$predictions <- readydata_rf_predict_newprod
#Create a csv file and write it to your hard drive. Note: You may need to use your computer’s search function to locate your output file

write.csv(newprod_pluspredictionsrf, file="newprod_pluspredictionsrf.csv", row.names = TRUE)
#steps to add data/findings to the dataset. How: new prod file is assigned into a new file named new prod plus predictions #next we do same for KNN

newprod_pluspredictionsKNN <- readyData_newprod
#Then u add the final predictions from each of the models (KNN)

newprod_pluspredictionsKNN$pred <- predict(object = readydata_KNN, newdata = readyData_newprod)
#here we add the predicted KNN data to the predictions column in the new prod plus prediction file

newprod_pluspredictionsKNN$predictions <- readydata_KNN_predict_newprod
#Create a csv file and write it to your hard drive. Note: You may need to use your computer’s search function to locate your output file

write.csv(newprod_pluspredictionsKNN, file="newprod_pluspredictionsKNN.csv", row.names = TRUE)
#steps to add data/findings to the dataset. How: new prod file is assigned into a new file named new prod plus predictions #next we do same for svm

newprod_pluspredictionssvm <- readyData_newprod
#Then u add the final predictions from each of the models (svm)

newprod_pluspredictionssvm$pred <- predict(object = readydata_svm, newdata = readyData_newprod)
#here we add the predicted KNN data to the predictions column in the new prod plus prediction file

newprod_pluspredictionssvm$predictions <- readydata_svm_predict_newprod
#Create a csv file and write it to your hard drive. Note: You may need to use your computer’s search function to locate your output file

write.csv(newprod_pluspredictionssvm, file="newprod_pluspredictionssvm.csv", row.names = TRUE)
#run dplyr

library(dplyr)
## 
## Attaching package: 'dplyr'
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
library(magrittr)
#since the product type was dumifyed, we can use the product number to identify the product types and then assign colours to the product types in the graph so we can identify the outliers. here we used dplyr package. so we merge product type and number under the exisitng products and name it product type and then use the procut type in the graph to identify each data point.

testSet <- testSet %>%
  left_join(existing_products %>%
              select(ProductNum, ProductType), 
            by = "ProductNum")
#here we use ggplot to visualize the findings in the testSet.The focus is on the test set because we can to see the level of errors that may have occured. geom_abline is used to draw a regression line across. for it you must put the intercepts and slope. color is used to identify the product types. This is for rf(random forest)

rf_plot <- ggplot(data = testSet) +
  geom_point(mapping = aes(x = readydata_rf_predict, y = testSet$Volume, col = ProductType)) +
  geom_abline(slope = 1, intercept = 0) + 
  labs(title = "rf model error with all variables") + 
  theme(legend.position="bottom", legend.title = element_blank())
#call the function

rf_plot
#here we use ggplot to visualize the findings in the testSet.The focus is on the test set because we can to see the level of errors that may have occured. geom_abline is used to draw a regression line across. for it you must put the intercepts and slope. color is used to identify the product types. This is for KNN

KNN_plot <- ggplot(data = testSet) +
  geom_point(mapping = aes(x = readydata_KNN_predict, y = testSet$Volume, col = ProductType)) + 
  geom_abline(slope = 1, intercept = 0) +
  labs(title = "KNN model error with all variables") + 
  theme(legend.position="bottom", legend.title = element_blank())
#call the function

KNN_plot
#here we use ggplot to visualize the findings in the testSet.The focus is on the test set because we can to see the level of errors that may have occured. geom_abline is used to draw a regression line across. for it you must put the intercepts and slope. color is used to identify the product types. This is for svm

svm_plot <- ggplot(data = testSet) +
  geom_point(mapping = aes(x = readydata_svm_predict, y = testSet$Volume, col = ProductType)) + 
  geom_abline(slope = 1, intercept = 0) +
  labs(title = "svm model error with all variables") + 
  theme(legend.position="bottom", legend.title = element_blank())
#call the function

svm_plot
#Reviews Vs Volume of Sales

Review_Vol_of_sale <- ggplot(data = testSet) +
  geom_point(mapping = aes(x = testSet$PositiveServiceReview, y = readydata_svm_predict, col = ProductType)) + 
  labs(title = "Positive Service Reviews VS Volume of Sale") + 
  theme(legend.position="bottom", legend.title = element_blank())
Review_Vol_of_sale
NReview_Vol_of_sale <- ggplot(data = testSet) +
  geom_point(mapping = aes(x = testSet$NegativeServiceReview, y = readydata_svm_predict, col = ProductType)) + 
  labs(title = "Negative Service Reviews VS Volume of Sale") + 
  theme(legend.position="bottom", legend.title = element_blank())
NReview_Vol_of_sale
x5star_Review_Vol_of_sale <- ggplot(data = testSet) +
  geom_point(mapping = aes(x = testSet$x5StarReviews, y = readydata_svm_predict, col = ProductType)) + 
  labs(title = "x5star_Review VS Volume of Sale") + 
  theme(legend.position="bottom", legend.title = element_blank())
x5star_Review_Vol_of_sale
