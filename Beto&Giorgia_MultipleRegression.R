getwd()

rm(list = ls(all = TRUE))

#BETO'S PATH
#setwd("~/Google Drive/DATA ANALYSTICS/2 PREDICTING CUSTOMER PREFERENCES/3 MULTIPLE REGRESSION IN R/task2-3-profitability-BetoMarin")

#GIORGIA'S PATH
setwd("C:/Users/giorgia/Documents/Giorgia Felline/UBIQUM/2. PREDICTING CUSTOMERS PREFERENCES/Task3_MultipleRegression/task2-3-profitability-Jorj91")
getwd()

#install.packages("corrplot")
#install.packages("caret")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("dplyr")
#install.packages("plotly")
#install.packages("randomForest")
#install.packages("gbm")
#install.packages("plot3D")


library("rpart")
library("rpart.plot")
library("corrplot")
library("caret")
library("dplyr")
library("plotly")
library("randomForest")
library("gbm")
library("plot3D")

#importing the existing product dataset (GIORGIA'S filename)
existingprod <- read.csv("existingproductattributes2017.2.csv", header=TRUE, sep=",")
summary(existingprod)
str(existingprod)

#importing the existing product dataset (BETO's filename)
#existingprod <- read.csv("existingprod.csv", header=TRUE, sep=",")
#summary(existingprod)
#str(existingprod)

#creating dummy variables for the Product Type (12 levels)
existingproddummy <- dummyVars("~ProductType", data = existingprod)
existingproddummy
readyData <- data.frame(predict(existingproddummy, newdata = existingprod))
readyData
head(readyData)
#attempt to do a for loop to join the two datasets (existingprod & readyData)

#for (i in 1:(ncol(readydata))) {
# existingprod$[] <- readyData$[i]

#}

#adding the column ProductNum in readyDATA (dataset with dummies) in order to do the merge 
#with the original dataset existingprod based on this key.
readyData$ProductNum <- existingprod$ProductNum
head(readyData)

#merging the two datasets readyData and existingprod
existingtotal <- merge(readyData,existingprod,by="ProductNum")
head(existingtotal)

#TO DO: model with all the variables (dummyvars included) and see what happens. Use dataset "existingtotal"

#removing Product Type column
existingtot <- existingtotal [,-14]
head(existingtot)

#removing BestSellersRank
existingtot$BestSellersRank <- NULL
head(existingtot)

####
#plotting producttype vs volume (we can see that there are two outliers with respect to volume)
qplot(existingprod$ProductType, existingprod$Volume, 
      main = "Sales Volume across different Product Types", 
      xlab = "Product Type", ylab = "Sales Volume", col = "red")
#?qplot
#TO DO: center the title of the plot and remove the legend


#plotly (interactive plot): producttype vs volume (same conclusions as before)
typevolume <- plot_ly(data = existingprod, x = ~ProductType, y = ~Volume, type = "scatter", mode = "markers", color = I("mediumseagreen"))%>%
  layout(title = "Sales Volume across different Product Types")
typevolume
?plot_ly

#plotly showing the perfect correlation between x5 and Volume
x5Volume <- plot_ly(data = existingprod, x = ~x5StarReviews, y = ~Volume, type = "scatter", mode = "markers",color = I("mediumseagreen"))%>%
  layout(title = "Perfect Linear Relationship between X5StarReviews and Volume")

x5Volume
add_lines(x5Volume)

#N.B. to write tilde on a Italian keyboard, type alt + 126 (right hand numbers) -> ~

#TO DO: scattermatrix among all predictors and label Volume
# histograms and barplot of all variables 

head(existingprod) #18 variables

par(mfrow = c(3, 6))
for(i in 1:(ncol(existingprod))){
  cl = existingprod[,i]
  if (is.numeric( cl )) 
  {hist( cl, xlab= names(existingprod)[i], main = (paste('Frequency of',names(existingprod)[i])))}
  else if (is.factor(cl))
  {barplot(table(cl),main = (paste('Frequency of',names(existingprod)[i])))}}


###

#correlation matrix among all the variables
par(mfrow = c(1,1))
correxistingtot <- cor(existingtot) 
correxistingtot
#visualizing the correlation matrix with a heatmap
corrplot(correxistingtot, title = "Correlation Matrix", tl.cex = 0.8, type = "upper",tl.col = "blue2")
?corrplot
#TO DO: Center the title!

#based on the correlation matrix, select the variables which are more correlated with the label Volume

#N.B. choosing a threshold for the correlation index equal to |0.85|, we select
#from the correlation matrix only the variable x4StarReviews (0.87)
#N.B. the correlation matrix only looks for linear relationships. 
#we will need a decision tree to search for non linear relationships.

#removing variable x5starReviews because it has correlation 1 with the dependent variable Volume (very unrealistic)
#and this would bring to overfit out model.

existingprodsintype <- read.csv("existingprodsintype.csv", header=TRUE, sep=",")
head(existingprodsintype)
existingprodsintype$x5StarReviews <- NULL
head(existingprodsintype)


#building a decision tree to capture also non linear relationships among variables
tree <- rpart(Volume~., data=existingprodsintype, cp=0.001)
rpart.plot(tree)
#according to this decisional tree, the most relevant variables are:
#x4StarReviews, PositiveServiceReview (by descending order of importance)


#decision tree with the original dataset, to prove that keeping variable X5 brings to overfitting
tree1 <- rpart(Volume~., data=existingprod, cp=0.001)
rpart.plot(tree1)
#this only considers x5 for the splitting!

#addding the variable productType to the dataset existingsintype
existingprodsintype$ProductType <- existingtotal$ProductType
head(existingprodsintype)

#proving that producttype is not relevant because it does not appear in the tree
tree_pt <- rpart(Volume~., data=existingprodsintype, cp=0.001)
rpart.plot(tree_pt)
#therefore, we can delete the product type because it is not relevant in predicting the volume


#CONCLUSION from CORRELATION MATRIX AND DECISION TREE: 
#x4 star Reviews and PositiveServiceReview are the most relevant
#variables in order to predict the sales volume (by descending order).

#reloading data. this cointains 15 variables.
existingprodsintype <- read.csv("existingprodsintype.csv", header=TRUE, sep=",")
head(existingprodsintype)

#correlation matrix among all the variables
correxistingprodsintype <- cor(existingprodsintype) 
correxistingprodsintype
#visualizing the correlation matrix with a heatmap
corrplot(correxistingprodsintype,title = "Correlation Matrix", tl.cex = 0.8, type = "upper",tl.col = "blue4")

#Afer a deep thougth we have decided to take out the following variables

existingprodsintype$Price <- NULL
existingprodsintype$x5StarReviews <- NULL
existingprodsintype$x3StarReviews <- NULL
existingprodsintype$x2StarReviews <- NULL
existingprodsintype$x1StarReviews <- NULL
existingprodsintype$NegativeServiceReview <- NULL
existingprodsintype$Recommendproduct <- NULL
existingprodsintype$ShippingWeight <- NULL
existingprodsintype$ProductHeight <- NULL
existingprodsintype$ProductWidth <- NULL
existingprodsintype$ProductDepth <- NULL
existingprodsintype$ProfitMargin <- NULL

head (existingprodsintype)
#AT THE END, WE ONLY DEAL WITH THE FOLLOWING VARIABLES: X4, POSITIVESERVICEREVIEW, VOLUME

#take out the outliers and normalize!!!

# Here we identify the outliers

outlier_values <- boxplot.stats(existingprodsintype$Volume)$out
outlier_values

par(mfrow = c(1, 1))
boxplot(existingprodsintype$Volume, main="Outliers in Sales Volume", boxwex=0.2)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

#Here we use a function from a package called dplyr to make take out the outliers. 
#The point is that this is a filter and
#it doesn't delete the rows from the dataset. 
#It creates a new dataset that we call in this case filteroutliers

head(existingprodsintype)
filteroutliers<-filter(existingprodsintype, Volume!=7036 & Volume!=11204)
#from now on we will work with the dataset "filteroutliers"
?filter



#############IGNORE THIS CODE UNTIL THE NEXT #############
#We create the partition in training and test sets


head(filteroutliers)
set.seed(123)
existingprodtrainindex <- createDataPartition(
  y = filteroutliers$Volume,
  ## the outcome data are needed
  p = .75,
  ## The percentage of data in the
  ## training set
  list = FALSE
)

trainexisting <- filteroutliers[ existingprodtrainindex,]
testexisting  <- filteroutliers[-existingprodtrainindex,]

#I check that I don't have outliers
nrow(trainexisting)
nrow(testexisting)

outlier_values <- boxplot.stats(filteroutliers$Volume)$out
boxplot(filteroutliers$Volume, main="outlier", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

#Normalizing and training the LINEAR REGRESSION
set.seed(123)
linearmodelnorm <- train(Volume~.,data=filteroutliers,method="lm",
                         preProcess=c("center","scale"), metric="RMSE")

linearmodelnorm
#performance on the training: 
#RMSE      Rsquared   MAE     
#321.0796  0.7500007  198.7543


#We apply the linear regression to the test set
set.seed(123)
Predictlm <- predict(linearmodelnorm, newdata = testexisting, metric="RMSE")
Predictlm

head(testexisting)
?postResample
postResample(Predictlm, testexisting$Volume)
#evaluating the performance on the test set
#RMSE    Rsquared         MAE 
#458.1982447   0.5519506 278.1005511 
#...una mierda!

str(filteroutliers)

#Apply SVM
set.seed(123)
trctrlsvm103 <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
#set.seed(123)
set.seed(123)
svm_Linear103 <- train(Volume ~., data = trainexisting, method = "svmLinear",
                       trControl=trctrlsvm103,
                       preProcess = c("center", "scale"),
                       tuneLength = 10)
svm_Linear103

#  RMSE      Rsquared   MAE     
#221.4499  0.8847243  127.1359

trctrlsvm206 <- trainControl(method = "repeatedcv", number = 20, repeats = 6)
set.seed(123)

svm_Linear206 <- train(Volume ~., data = trainexisting, method = "svmLinear",
                       trControl=trctrlsvm206,
                       preProcess = c("center", "scale"),
                       tuneLength = 10)
svm_Linear206

#RMSE      Rsquared   MAE     
#188.5187  0.9408436  134.5126

#set.seed(123)
#svm_Linear_Grid <- train(Volume ~., data = trainexisting, method = "svmLinear",
#                           trControl=trctrlsvm103,
#                           preProcess = c("center", "scale"),
#                           tuneGrid = grid,
#                           tuneLength = 10)
#
#> svm_Linear_Grid

test_pred_svm103 <- predict(svm_Linear103, newdata = testexisting, metric="RMSE")
test_pred_svm103

postResample(test_pred_svm103, testexisting$Volume)

#RMSE    Rsquared         MAE 
#481.2416698   0.5366656 257.2789558

set.seed(123)
trctrlsvm206 <- trainControl(method = "repeatedcv", number = 20, repeats = 6)

svm_Linear206 <- train(Volume ~., data = trainexisting, method = "svmLinear",
                       trControl=trctrlsvm206,
                       preProcess = c("center", "scale"),
                       tuneLength = 10)
svm_Linear206

test_pred_svm206 <- predict(svm_Linear206, newdata = testexisting, metric="RMSE")
test_pred_svm206

postResample(test_pred_svm206, testexisting$Volume)

#APPLY RANDOM FOREST

rf300trees <- train(Volume~., data = trainexisting, method = "rf",trControl = trctrlsvm103, metric = "RMSE", ntree = 300)

rf300trees

#resultrf300<- data.frame(testpred_rf300 <- predict(rf300trees, newdata = testexisting, metric="RMSE"))

#resultrf300<- data.frame(testpred_rf300 <- predict(rf300trees, newdata = testexisting, metric="RMSE"))

#colnames(resultrf300$testpred_rf300....predict.rf300trees..newdata...testexisting..) <- resultrf300$Predictions

testpred_rf300 <- predict(rf300trees, newdata = testexisting, metric="RMSE")

rfpredictions <- testpred_rf300
rfpredictions
actual <- testexisting$Volume

plotpredandactual <- plot(predandactual$actual, predandactual$rfpredictions)

predandactual <- data.frame(rfpredictions, actual)

predandactual

predandactual$errors<-predandactual$rfpredictions-predandactual$actual



postResample(testpred_rf300, testexisting$Volume)

#RMSE         Rsquared         MAE 
#249.1708406   0.8762659 148.4440222

rf600trees <- train(Volume~., data = trainexisting, method = "rf",trControl = trctrlsvm103, metric = "RMSE", ntree = 600)

rf600trees

testpred_rf600 <- predict(rf600trees, newdata = testexisting, metric="RMSE")
testpred_rf600

postResample(testpred_rf600, testexisting$Volume)

#RMSE          Rsquared         MAE 
#249.9573519   0.8757835 149.2772859 

#PRUEBA DE RANDOM FOREST CON LA DATASET ORIGNAL SIN ALGUNAS DE LAS COLUMNAS

existingprodpruebarf <- read.csv("existingprod.csv", header=TRUE, sep=",")
head(existingprodpruebarf)

existingprodpruebarf$ShippingWeight <- NULL
existingprodpruebarf$ProductHeight <- NULL
existingprodpruebarf$ProductWidth <- NULL
existingprodpruebarf$ProductDepth <- NULL
existingprodpruebarf$ProfitMargin <- NULL
existingprodpruebarf$ProductNum <- NULL
existingprodpruebarf$x5StarReviews <- NULL
existingprodpruebarf$BestSellersRank <- NULL
existingprodpruebarf$ProductType <- NULL


head(existingprodpruebarf)
summary(existingprodpruebarf)


filteroutliersprueba<-filter(existingprodpruebarf, Volume!=7036 & Volume!=11204)

summary(filteroutliersprueba)

set.seed(123)
existingprodpruebarfindex <- createDataPartition(
  y = filteroutliersprueba$Volume,
  ## the outcome data are needed
  p = .75,
  ## The percentage of data in the
  ## training set
  list = FALSE
)

trainexistingprueba <- filteroutliersprueba[ existingprodpruebarfindex,]
testexistingprueba  <- filteroutliersprueba[-existingprodpruebarfindex,]

#I check that I don't have outliers
nrow(trainexistingprueba)
nrow(testexistingprueba)

rf600treesoridf <- train(Volume~., data = trainexistingprueba, method = "rf",trControl = trctrlsvm103, metric = "RMSE", ntree = 600)

rf600treesoridf
str(rf600treesoridf)


str(filteroutliersprueba)
testpred_rf600prueba <- predict(rf600treesoridf, newdata = testexistingprueba, metric="RMSE")
testpred_rf600prueba

postResample(testpred_rf600prueba, testexistingprueba$Volume)
#RMSE           Rsquared         MAE 
#272.5164137   0.8459636 156.9387037 

treeprueba <- rpart(Volume~., data=filteroutliersprueba, cp=0.001)
rpart.plot(treeprueba)

#PRUEBA APPLY GMB

BMG <- train(Volume ~., data = trainexisting, method = "gbm",
             trControl=trctrlsvm103,
             preProcess = c("center", "scale"),
             tuneLength = 10)

BMG

summary(filteroutliersprueba)


hist(filteroutliers$Volume)

# APPLY THE BESTO MODEL (SVM) TO THE NEW PRODUCTS 

newproducts <- read.csv("newproductattributes2017.2.csv", header=TRUE, sep=",")

newproducts$Price <- NULL
newproducts$x5StarReviews <- NULL
newproducts$x3StarReviews <- NULL
newproducts$x2StarReviews <- NULL
newproducts$x1StarReviews <- NULL
newproducts$NegativeServiceReview <- NULL
newproducts$Recommendproduct <- NULL
newproducts$ShippingWeight <- NULL
newproducts$ProductHeight <- NULL
newproducts$ProductWidth <- NULL
newproducts$ProductDepth <- NULL
newproducts$ProfitMargin <- NULL
newproducts$ProductType <- NULL
newproducts$BestSellersRank<-NULL
newproducts$ProductNum<-NULL

head(newproducts)
finalPred <- predict(rf300trees, newdata = newproducts, metric="RMSE")
finalPred

NewProductsAndPredictions <- read.csv("newprod.csv", header=TRUE, sep=",")

NewProductsAndPredictions$VolumeFinalPredictions <- finalPred

NewProductsAndPredictions$Volume<-NULL

write.csv(NewProductsAndPredictions, file="C2.T3output.csv", row.names = TRUE)







########20 Noviembre 2018

#we will work on the dataset filteroutliers, made of 78 obs and 3 variables
head(filteroutliers)

#creating the partition, the training and the test set
set.seed(123)
existingprodtrainindex <- createDataPartition(
  y = filteroutliers$Volume,
  ## the outcome data are needed
  p = .75,
  ## The percentage of data in the
  ## training set
  list = FALSE
)

trainexisting <- filteroutliers[ existingprodtrainindex,]
testexisting  <- filteroutliers[-existingprodtrainindex,]

#I check that I don't have outliers
nrow(trainexisting)  #60 obs in the training
nrow(testexisting)   #18 obs in the test

#CROSS VALIDATION with 5 folds.

trctrl5F <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
library("caret")
#APPLY RANDOM FOREST
set.seed(123)
rf300trees5F <- train(Volume~., data = trainexisting, method = "rf",trControl = trctrl5F, metric = "RMSE", ntree = 300)

rf300trees5F


#RMSE      Rsquared   MAE    
#155.0392  0.9513453  84.3125

#Tuning parameter 'mtry' was held constant at a value of 2

#PREDICTION

testpred_rf3005F <- predict(rf300trees5F, newdata = testexisting, metric="RMSE")

rfpredictions5F <- testpred_rf3005F
rfpredictions5F

#RMSE Rsquared MAE
postResample(testpred_rf3005F, testexisting$Volume)

#RMSE    Rsquared         MAE 
#249.2465430   0.8781915 149.9128494 

actual <- testexisting$Volume
actual

plotpredandactualRF5F <- plot(actual, rfpredictions5F)


#this is going to be the dataset with prediction from all models that we run and the actual values of volume
predandactual5F <- data.frame(rfpredictions5F, actual)

predandactual5F

predandactual5F$errorsRF<-predandactual5F$rfpredictions5F-predandactual5F$actual

predandactual5F


#LINEAR REGRESSION 5 FOLDS
set.seed(123)
linearmodelnorm5F <- train(Volume~.,data=trainexisting, method="lm",trControl = trctrl5F,
                           preProcess=c("center","scale"), metric="RMSE")

linearmodelnorm5F

#RMSE      Rsquared   MAE     
#246.3502  0.8176289  141.1287


#We apply the linear regression to the test set

Predictlm5F <- predict(linearmodelnorm5F, newdata = testexisting, metric="RMSE")
Predictlm5F

postResample(Predictlm5F, testexisting$Volume)
#RMSE    Rsquared         MAE 
#473.6796236   0.5465173 272.2051194 
#...una mierda!

plotpredandactualLM5F <- plot(actual, Predictlm5F)


#this is going to be the dataset with prediction from all models that we run and the actual values of volume

predandactual5F$lmpredictions5F <-Predictlm5F

predandactual5F

predandactual5F$errorsLM<-predandactual5F$lmpredictions5F-predandactual5F$actual

predandactual5F



#SVM  Linear 5 FOLDS

#training the model
set.seed(123)
svm_Linear5F <- train(Volume ~., data = trainexisting, method = "svmLinear",
                      trControl=trctrl5F,
                      preProcess = c("center", "scale"))
svm_Linear5F

#RMSE    Rsquared   MAE     
#241.14  0.8101649  121.1336
#Tuning parameter 'C' was held constant at a value of 1

summary(svm_Linear5F)

#test the model
test_pred_svm5F <- predict(svm_Linear5F, newdata = testexisting, metric="RMSE")
test_pred_svm5F

postResample(test_pred_svm5F, testexisting$Volume)

#    RMSE      Rsquared         MAE 
#481.2416698   0.5366656 257.2789558 

predandactual5F$SVMpredictions5F <-test_pred_svm5F

predandactual5F

predandactual5F$errorsSVM<-predandactual5F$SVMpredictions5F-predandactual5F$actual

predandactual5F


#TO DO: CHANGE THE PARAMETERS OF THE SVM, BUT BEFORE...STUDY!!! :D

#WE DO THE TEST OF USING THE DATASET WITH 9 VARIABLES TO SHOW THAT 
#   THE RESULT IS WORST THAN WITH 3 VARIABLES
set.seed(123)
trctrl5F <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(123)
library(caret)
RF9VAR5F <- train(Volume~., data = trainexistingprueba, method = "rf",trControl = trctrl5F, metric = "RMSE", ntree = 300)

RF9VAR5F

#mtry  RMSE      Rsquared   MAE      
#2     211.6713  0.8963419  119.33554
#5     194.0643  0.9186650  100.62006
#8     181.9683  0.9273106   94.98213

#RMSE was used to select the optimal model using the smallest value.
#The final value used for the model was mtry = 8.


RF9VAR5FPREDICTION <- predict(RF9VAR5F, newdata = testexistingprueba, metric="RMSE")
RF9VAR5FPREDICTION

postResample(RF9VAR5FPREDICTION, testexistingprueba$Volume)


#RESULTS RF3VAR5F
#RMSE    Rsquared         MAE 
#273.1988763   0.8480071 158.1469259 

#therefore we have proved that random forest works better with just 3 variables
#instead of considering 9 variables (the unuseful variables only bring noise to our model.)


#LOVELY 3D PLOT

x <- filteroutliers$x4StarReviews
y <- filteroutliers$PositiveServiceReview
z <- filteroutliers$Volume

fit <- lm(z ~ x + y)

grid.lines = 78
x.pred <- seq(min(x), max(x), length.out = grid.lines)
#x.pred <- seq(min(x), max(x))
#x.pred
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)
# fitted points for droplines to surface
fitpoints <- predict(fit)
# scatter plot with regression plane
scatter3D(x, y, z, pch = 18, cex = 2, bty = "g",
          theta = 30, phi = -20, ticktype = "detailed",
          xlab = "4STARS", ylab = "POSREV", zlab = "VOLUME",  
          surf = list(x = x.pred, y = y.pred, z = z.pred,  
                      facets = NA, fit = fitpoints, main = "LM"))

#SVMRADIAL 5 FOLDS
set.seed(123)
trctrl5F <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

#training the model
set.seed(123)
svm_radial5F <- train(Volume ~., data = trainexisting, method = "svmRadial",
                      trControl=trctrl5F, tuneLength=10,
                      preProcess = c("center", "scale"))
svm_radial5F

#Support Vector Machines with Radial Basis Function Kernel 
#
#60 samples
#2 predictor
#
#Pre-processing: centered (2), scaled (2) 
#Resampling: Cross-Validated (5 fold, repeated 3 times) 
#Summary of sample sizes: 48, 48, 48, 48, 48, 48, ... 
#Resampling results across tuning parameters:
#  
#  C       RMSE      Rsquared   MAE     
#0.25  304.4547  0.8014496  159.9144
#0.50  276.6008  0.8291646  148.5885
#1.00  264.5064  0.8376942  143.9761
#2.00  255.4845  0.8447417  139.9856
#4.00  252.4130  0.8399188  136.3747
#8.00  257.7725  0.8236617  141.8613
#16.00  271.4505  0.8014997  151.1801
#32.00  293.1908  0.7814759  161.9557
#64.00  315.7511  0.7686871  169.4358
#128.00  331.5912  0.7580524  178.0689
#
#Tuning parameter 'sigma' was held constant at a value of 6.185969
#RMSE was used to select the optimal model using the smallest value.
#The final values used for the model were sigma = 6.185969 and C = 4.

summary(svm_Linear5F)

#test the model
test_pred_svmRADIAL5F <- predict(svm_radial5F, newdata = testexisting, metric="RMSE")
test_pred_svmRADIAL5F

postResample(test_pred_svmRADIAL5F, testexisting$Volume)



predandactual5F$SVMRADIALpredictions5F <-test_pred_svmRADIAL5F

predandactual5F

predandactual5F$errorsSVMRADIAL<-predandactual5F$SVMRADIALpredictions5F-predandactual5F$actual

colnames(predandactual5F)[6] <- "SVMLINEARpredictions5F"

predandactual5F

colnames(predandactual5F)[7] <- "errorsSVMLINEAR"

predandactual5F[,c(1,3,4,5,6,7,8,9,2)]

#final prediction
#BETO'S NAME FILE
#NewProductsAndPredictions <- read.csv("newprod.csv", header=TRUE, sep=",")

#GIORGIA'S NAME FILE
NewProductsAndPredictions <- read.csv("newproductattributes2017.2.csv", header=TRUE, sep=",")


str(NewProductsAndPredictions)

finalPredRF <- predict(rf300trees5F, newdata = newproducts, metric="RMSE")
finalPredRF

NewProductsAndPredictions$VolumeFinalPredictions <- finalPredRF

NewProductsAndPredictions$Volume<-NULL

write.csv(NewProductsAndPredictions, file="C2.T3output.csv", row.names = TRUE)




#####beto's night code
NewProductsAndPredictions <- read.csv("newproductattributes2017.2.csv", header=TRUE, sep=",")

str(NewProductsAndPredictions)

finalPredRF <- predict(rf300trees5F, newdata = newproducts, metric="RMSE")
finalPredRF

NewProductsAndPredictions$VolumeFinalPredictions <- finalPredRF

NewProductsAndPredictions$Volume<-NULL
NewProductsAndPredictions

#round the volume from a real to a integer number (THEY ARE UNITS TO SELL!!)
NewProductsAndPredictions$VolumeFinalPredictions<-round(NewProductsAndPredictions$VolumeFinalPredictions)
NewProductsAndPredictions

write.csv(NewProductsAndPredictions, file="C2.T3output.csv", row.names = TRUE)

#creating an aggregation table with the sum of volume, grouping by producttype
aggregation <- aggregate(NewProductsAndPredictions$VolumeFinalPredictions, by=list(Category=NewProductsAndPredictions$ProductType),FUN=sum)
colnames(aggregation)[colnames(aggregation)=="Category"] <- "Product_Type"
colnames(aggregation)[colnames(aggregation)=="x"] <- "Sales_Volume_Sum_Predicted"
aggregation
#ordering the aggregation table by SALES VOLUME SUM PREDICTED, descending order
aggregation[order(aggregation$Sales_Volume_Sum_Predicted,decreasing = TRUE), ]

#computing profitability
NewProductsAndPredictions$Profitability <- (NewProductsAndPredictions$Price * NewProductsAndPredictions$ProfitMargin * NewProductsAndPredictions$VolumeFinalPredictions)

write.csv(NewProductsAndPredictions, file="C2.T3outputwithprofit.csv", row.names = TRUE)

##########################sorting by#######################################
#NewProductsAndPredictionsSort <-data.frame(NewProductsAndPredictionsSort)   ???


#ordering by protifability in descending order
NewProductsAndPredictions[order(NewProductsAndPredictions$Profitability,decreasing = TRUE), ]
#ordering by sales volume by descending order
NewProductsAndPredictions[order(NewProductsAndPredictions$VolumeFinalPredictions,decreasing = TRUE), ]
write.csv(NewProductsAndPredictions, file="C2.T3outputwithprofitsort.csv", row.names = TRUE)
