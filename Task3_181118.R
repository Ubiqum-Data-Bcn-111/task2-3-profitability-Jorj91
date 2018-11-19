getwd()

rm(list = ls(all = TRUE))

#setwd("~/Google Drive/DATA ANALYSTICS/2 PREDICTING CUSTOMER PREFERENCES/3 MULTIPLE REGRESSION IN R/task2-3-profitability-BetoMarin")
setwd("C:/Users/giorgia/Documents/Giorgia Felline/UBIQUM/2. PREDICTING CUSTOMERS PREFERENCES/Task3_MultipleRegression/task2-3-profitability-Jorj91")
getwd()

#install.packages("corrplot")
#install.packages("caret")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("dplyr")


library("rpart")
library("rpart.plot")
library("corrplot")
library("caret")
library("dplyr")
library("plotly")

#importing the existing product dataset
existingprod <- read.csv("existingproductattributes2017.2.csv", header=TRUE, sep=",")
summary(existingprod)
str(existingprod)

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
?qplot
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
correxistingtot <- cor(existingtot) 
correxistingtot
#visualizing the correlation matrix with a heatmap
corrplot(correxistingtot, title = "Correlation Matrix", tl.cex = 0.8, type = "upper",tl.col = "blue4")
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

filteroutliers<-filter(existingprodsintype, Volume!=7036 & Volume!=11204)
#from now on we will work with the dataset "filteroutliers"


#We create the partition in training and test sets

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

linearmodelnorm <- train(Volume~.,data=filteroutliers,method="lm",
                       preProcess=c("center","scale"), metric="RMSE")

linearmodelnorm
#performance on the training: 
#RMSE      Rsquared   MAE     
#321.0796  0.7500007  198.7543


#We apply the linear regression to the test set

Predictlm <- predict(linearmodelnorm, newdata = testexisting, metric="RMSE")
Predictlm

head(testexisting)
?postResample
postResample(Predictlm, testexisting$Volume)
#evaluating the performance on the test set
#RMSE    Rsquared         MAE 
#458.1982447   0.5519506 278.1005511 
#...una mierda!

