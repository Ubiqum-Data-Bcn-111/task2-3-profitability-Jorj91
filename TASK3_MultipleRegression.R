getwd()

rm(list = ls(all = TRUE))

setwd("C:/Users/giorgia/Documents/Giorgia Felline/UBIQUM/2. PREDICTING CUSTOMERS PREFERENCES/Task3_MultipleRegression/task2-3-profitability-Jorj91")


getwd()

install.packages("corrplot")
install.packages("caret")
install.packages("rpart")
install.packages("rpart.plot")


library("rpart")
library("rpart.plot")
library("corrplot")
library("caret")

#importing the existing product dataset
existingprod <- read.csv(file="C:/Users/giorgia/Documents/Giorgia Felline/UBIQUM/2. PREDICTING CUSTOMERS PREFERENCES/Task3_MultipleRegression/task2-3-profitability-Jorj91/existingproductattributes2017.2.csv", header=TRUE, sep=",")
summary(existingprod)
str(existingprod)

#creating dummy variables for the Product Type (12 levels)
existingproddummy <- dummyVars("~ProductType", data = existingprod)
existingproddummy
readyData <- data.frame(predict(existingproddummy, newdata = existingprod))
readyData

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

#removing Product Type column
existingtot <- existingtotal [,-14]
head(existingtot)
str(existingtot)
summary(existingtot)

#removing BestSellersRank
existingtot$BestSellersRank <- NULL

#correlation matrix among all the variables
correxistingtot <- cor(existingtot) 
correxistingtot
#visualizing the correlation matrix with a heatmap
corrplot(correxistingtot)

#based on the correlation matrix, select the variables which are more correlated with the label Volume
#the correlation matrix only looks for linear relationships. we will need a decision tree to search for
#non linear relationships.

#removing variable x5starReviews because it has correlation 1 with the dependent variable Volume (very unrealistic)
#and this would bring to overfit out model.
existingtot$x5StarReviews <- NULL

#building a decision tree to capture also non linear relationships among variables
tree<- rpart(Volume~., data=existingtot, cp=0.001)
rpart.plot(tree)
#we can notice that x4 star Reviews, PositiveServiceReview and x4StarReviews are the most relevant
#variable in order to predict the sales volume (by descending order).

