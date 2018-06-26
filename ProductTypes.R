####Objecctive####
#Objective: Understand how specific product types perform against each other


####Install and Load Packages####
library(caret)
install.packages("corrplot")
library(corrplot)

####Import Data####
#Training dataset
ExisProd <- read.csv("existingproductattributes2017.2.csv")
#Testing dataset
NewProd <- read.csv("newproductattributes2017.2.csv")
str(ExisProd)

####Dummify Data####
#As this is a regresssion problem, need to dummify non-numeric data.
NewExisProd <- dummyVars("~.", data = ExisProd)
ReadyExisProd <- data.frame(predict(NewExisProd, newdata = ExisProd))
ReadyExisProd
str(ExisProd)
str(ReadyExisProd)
summary(ReadyExisProd)


####Remove Missing Values####
#BestSellersRank has empty values and is being removed
ReadyExisProd$BestSellersRank <- NULL


####Correlation####
#Determine correlation between attributes
corrData <- cor(ReadyExisProd)
corrData


#Plot out correlation
corrplot(corrData)

#Remove star review atributes because of their high correlation with dependent variable Volume
ReadyExisProd$x5StarReviews <- NULL
ReadyExisProd$x4StarReviews <- NULL
ReadyExisProd$x3StarReviews <- NULL
ReadyExisProd$x2StarReviews <- NULL
ReadyExisProd$x1StarReviews <- NULL


####Linear Regression####

inTrain <- createDataPartition(y=ReadyExisProd$Volume, p=.75, list=FALSE) #Creates training set equal to random 75% of original dataset; testing set is other 25%
training <- ReadyExisProd[inTrain,] #Creates training set
testing <- ReadyExisProd[-inTrain,] #Creates testing set
set.seed(123)
ExisLM <- lm(Volume ~ .,training)
ExisLM
summary(ExisLM)


####Random Forest####
#Install and load randomForest pacakge and create training and data sets (also to be used for Support Vector Machine)
install.packages("randomForest")
library(randomForest)
trainSize <- round(nrow(ReadyExisProd)*0.7) #Probably should have used 75% as in training set just to be consistent
testSize <- nrow(ReadyExisProd)-trainSize
trainSize
testSize
training_indices <- sample(seq_len(nrow(ReadyExisProd)),size = trainSize)
trainSet <- ReadyExisProd[training_indices,]
testSet <- ReadyExisProd[-training_indices,]


#Create and run randomForest model
rfModel <- randomForest(Volume~., trainSet, ntree = 500) #Can add additional variable in code: do.trace=T, to return the mean squard error and % variability for each individual tree; default is 500 trees (ntree)
rfModel



#Make predictions on randomForest model
rfPredict <- predict(rfModel,testSet)
rfPredict
#attributes(rfModel)# Returns all possible functions that R can return in the following format: 'NameofModel'$'NameofFunction': rfModel$mse, rfModel$rsq
#RFTuning <- tuneRF(trainSet[,-18], trainSet[,18], stepFactor = 0.5, plot = TRUE, ntreeTry = 500, trace = TRUE, improve = 0.1)



####Support Vector Machine####
#Introduce e1071 package for support vector machine model
library(e1071)

#Create model for support vector machine
svmmodel <- svm(Volume~., data = trainSet, kernel = "linear", cost = .1, scale = FALSE) #Creates support vector machine model
svmmodel

#Make predictions on support vector machines
svmPred <- predict(svmmodel,testSet)
svmPred
#svmtuned <- tune(svm, Volume~., data = trainSet, kernel ="linear", ranges = list(cost=c(0.001,0.01,0.1,1,10))) #Tunes model and determines best cost parameter (based on what you give it)

summary(svmPred)


#Call prediction metrics file to compare model predicted sales to actual sales
PredMetrics <- read.csv("Prediction Metrics-RandomForest.csv") 

#Print Support Vector Machine sales predictions of existing product dataset to PredMetrics dataset
output1 <- PredMetrics
output1$predictions5 <- svmPred
write.csv(output1, file="C2.T3output1.csv", row.names = TRUE)

#Print RandomForest sales predictions of existingproductattributes dataset to PredMetrics dataset
output2 <- PredMetrics
output2$predictions5SVM <- rfPredict
write.csv(output2, file="C2.T3output2.csv", row.names = TRUE)


####Testing Dataset####
#Dummify variables in newproductattributes dataset
NewNewProd <- dummyVars("~.", data = NewProd)
ReadyExisProd2 <- data.frame(predict(NewNewProd, newdata = NewProd))
ReadyExisProd2
str(NewProd)
str(ReadyExisProd2)
summary(ReadyExisProd2)


#Remove attributes in newproductattributes that were also removed in existingproductingattributes
ReadyExisProd2$x5StarReviews <- NULL
ReadyExisProd2$x4StarReviews <- NULL
ReadyExisProd2$x3StarReviews <- NULL
ReadyExisProd2$x2StarReviews <- NULL
ReadyExisProd2$x1StarReviews <- NULL

#Predict using randomForest (preferred model)
finalPred <- predict(rfModel, ReadyExisProd2)
finalPred

#Write predictions to new CSV file which also takes all existing data from ReadyExisProd2 (newproductattributes data)
output <- ReadyExisProd2
output$predictions <- finalPred
write.csv(output, file="C2.T3output.csv", row.names = TRUE)
