#...........................BEGIN SCRIPT........................................
library(readxl)
data <- read_excel("cleaned data.xlsx") # load data
# discretize deal status code 
for (i in 1:nrow(data)){
  data$`Deal Status Code`[i] <- if (data$`Deal Status Code`[i] == "Won") 1 else 0 
}
# convert binary characters to numeric values
data$`Deal Status Code` <- as.numeric(data$`Deal Status Code`)
# convert dates to integers
data$`Deal Date` <- as.integer(as.Date(data$`Deal Date`)) 
# convert solution type to number
data$`Solution Type` <- as.numeric(gsub("Solution ", "", data$`Solution Type`)) 
# remove non-numerical attributes
data <- data[,c(3,4,9,10)]
# the normalization function is created
# note this scales values between 0 and 1 (inclusive)
nor <- function(x) {(x -min(x))/(max(x)-min(x))}
data.nor <- as.data.frame(lapply(data[,c(1:3)], nor))
data.nor$`Deal Status Code` <- data$`Deal Status Code`
# Split data into training/testing
library(caTools)
set.seed(94)
spl = sample.split(data$`Deal Status Code`, 
                   SplitRatio = 0.7) # preserves ratio of win/loss automatically
deal.train = subset(data, spl == TRUE)
deal.test = subset(data, spl == FALSE)
# Build KNN Model
# the deal status column of dataset needed to compare prediction from training dataset
# also convert ordered factor to normal factor
data_target <- deal.train$`Deal Status Code`
# the actual values of 2nd column of testing dataset to compare it with values that will be predicted
test_target <- deal.test$`Deal Status Code`
# run knn function
library(class)
pr <- knn(deal.train,deal.test,cl=data_target,k=5) # start with k=5
# create the confusion matrix
tb <- table(pr,test_target)
print(tb) # to view matrix
# check the accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tb)
s <- seq(from = 3, to = 25, by = 2)
a <- rep(0, length(s))
counter <- 1
for (i in s){
  pr <- knn(deal.train,deal.test,cl=data_target,k=i)
  tb <- table(pr,test_target)
  a[counter] <- accuracy(tb)
  counter <- counter + 1
}
# plot results
plot(c(.647,.669,.667,.667,.66,.656,.656,.66,.653)~seq(from = 3, to = 19, by = 2), 
     xlab = "k", ylab = "Accuracy", cex = 1.5, pch = 19, xaxp = c(3,19,8), 
     yaxp = c(.64,.67,15), col = "violet", cex.axis = 1.5, cex.lab = 2)
text(5,.667,"66.9%")
#............................END SCRIPT.........................................
#
# deal status logistic 2.R
#
#...........................BEGIN SCRIPT........................................
# Load data, perform PCA, then split the data
library(readxl)
data <- read_excel("cleaned data.xlsx")
# discretize deal status code 
for (i in 1:nrow(data)){
  data$`Deal Status Code`[i] <- if (data$`Deal Status Code`[i] == "Won") 1 else 0 
}
# convert binary characters to numeric values
data$`Deal Status Code` <- as.numeric(data$`Deal Status Code`)
# convert dates to integers
data$`Deal Date` <- as.integer(as.Date(data$`Deal Date`)) 
# convert solution type to number
data$`Solution Type` <- as.numeric(gsub("Solution ", "", data$`Solution Type`)) 
# remove non-numerical attributes
data <- data[,c(3,4,9,10)]
# PCA with "FactoMineR" library
library("FactoMineR")
data.pca <- PCA(data[,-4], scale.unit = TRUE, ncp = 5, graph = TRUE)
# does not include deal status
print(data.pca) # print eigenvalues, percentage of variance, and cumulative
# percentage of variance
# scree plot with "factoextra" library
library("factoextra")
fviz_eig(data.pca, addlabels = TRUE, ylim = c(0, 50))
# see that PCA does not help much
# Split data into training/testing
library(caTools)
set.seed(95)
spl = sample.split(data$`Deal Status Code`, 
                   SplitRatio = 0.7) # preserves ratio of win/loss automatically
deal.train = subset(data, spl == TRUE)
deal.test = subset(data, spl == FALSE)
# Build a Logistic Regression Model
DealLog = glm(`Deal Status Code` ~ `Solution Type`
              + `Deal Date` + `Deal Cost`, data = deal.train, family=binomial)
summary(DealLog) 
# see that only solution type is significant
a <- DealLog$coefficients[2]
b <- DealLog$coefficients[1]
# make predictions with testing set
predictTest = predict(DealLog, newdata = deal.test, type="response")
# Confusion matrix for threshold of 0.5
t <- table(deal.test$`Deal Status Code`, predictTest > 0.35)
print(t)
# find optimal threshold
s <- seq(from = min(predictTest),
         to = max(predictTest), by = 0.01)
accuracy <- rep(0,length(s))
counter <- 1
for (i in s){
  t <- table(deal.test$`Deal Status Code`, predictTest > i)
  accuracy[counter] <- (t[1,1]+t[2,2])/sum(t)
  counter <- counter + 1
}
threshold <- s[which.max(accuracy)]
# plot results
plot(s,accuracy,xlab = "Threshold", ylab = "Accuracy")
p <- colors()
plot(deal.test$`Deal Status Code`~ deal.test$`Solution Type`, 
     pch = 16, ylim = c(-0.1,1.1), ylab = "Probability", 
     xlab = "Solution Type", 
     col = p[(deal.test$`Deal Status Code`+1)*50])

lines((exp(b+a*deal.train$`Solution Type`)/(1+exp(b+a*deal.train$`Solution Type`))
)~deal.train$`Solution Type`,col="black")
#............................END SCRIPT.........................................
#
# deal status tree.R
#
#...........................BEGIN SCRIPT........................................
# Load then Split the data
library(caTools, readxl)
set.seed(95)
data <- read_excel("cleaned data.xlsx")
spl = sample.split(data$`Original Record Number`, 
                   SplitRatio = 0.7) # preserves ratio of win/loss automatically
deal.train = subset(data, spl == TRUE)
deal.test = subset(data, spl == FALSE)
# Check percentage of deals in each status bucket of training data
table(deal.train$`Deal Status Code`)/nrow(deal.train)
table(deal.test$`Deal Status Code`)/nrow(deal.test)
# Decision Tree for deal status
library(rpart)
library(rpart.plot)
# tree depth of 10 seems to be most accurate
dataTree = rpart(`Deal Status Code` ~ `Client Category`+ Sector + Location 
                 + `VP Name`, data=deal.train, method="class", 
                 control = rpart.control(maxdepth = 10, cp=0.00005))
prp(dataTree, cex = .75, yesno = 1, split.border.col = 0, lty = 2, faclen = 1, 
    nn.col = "red", nn.cex = 1, xcompact = TRUE, ycompact = TRUE, yspace = .1, 
    gap = 10, compress = TRUE, nspace = 2, split.space = .1)
# Make predictions
PredictTest = predict(dataTree, newdata = deal.test, type = "class")
t <- table(deal.test$`Deal Status Code`, PredictTest)
print(t)
acc <- (t[1,1]+t[2,2])/sum(t)
#............................END SCRIPT.........................................
#
#
# Naive Bayes
#
#...........................BEGIN SCRIPT........................................
library(e1071)
library(naivebayes)
library(caret)
str(train_m)
Deal_fnl_mod_num$`Deal Status Code` <- as.factor(Deal_fnl_mod_num$`Deal Status Code`)
str(Deal_fnl_mod_num)
nb = naive_bayes(`Deal Status Code`~.,data = train_m,usekernel = T)
nb
plot(nb)
pred_nr <- predict(nb,test_m)
mat_nbm <- confusionMatrix(as.factor(test_m$`Deal Status Code`),as.factor(pred_nr))
mat_nbm
library(ROCR)
actual <- test_m$`Deal Status Code`
pred_lor =prediction(as.numeric(pred_nr),as.numeric(actual))
ROC_lor = performance(pred_lor,"tpr","fpr")
plot(ROC_lor,colorize=T,lwd =2)
abline(a=0,b=1)
AUC = performance(pred_lor,measure = "auc")
print(AUC@y.values)
#............................END SCRIPT.........................................
#
# Decision Tree
#
#...........................BEGIN SCRIPT........................................
spl2 <- sample.split(Deal_fnl_mod$`Deal Status Code`,SplitRatio = 0.7)
train_m <- subset(Deal_fnl_mod,spl2=="TRUE")
test_m <- subset(Deal_fnl_mod,spl2=="FALSE")
mod_dt <- rpart(`Deal Status Code`~.,data = train_m,method = "class")
rpart.plot(mod_dt,cex = 0.5, fallen.leaves = F,type = 4)
p8 <- predict(mod_dt,newdata = test_m)
library(caret)
dt_m <- confusionMatrix(p8,as.factor(test_m$`Deal Status Code`))
p8 <- predict(mod_dt,newdata = test_m,type = "class")
dt_m <- confusionMatrix(p8,as.factor(test_m$`Deal Status Code`),mode = "prec_recall")
dt_m
#............................END SCRIPT.........................................
#
# Logistic Regression
#
#...........................BEGIN SCRIPT........................................
Deal_fnl_mod$`Client Category` <- as.factor(Deal_fnl_mod$`Client Category`)
Deal_fnl_mod$`Solution Type` <- as.factor(Deal_fnl_mod$`Solution Type`)
Deal_fnl_mod$Sector <- as.factor(Deal_fnl_mod$Sector)
Deal_fnl_mod$Location <- as.factor(Deal_fnl_mod$Location)
Deal_fnl_mod$`VP Name` <- as.factor(Deal_fnl_mod$`VP Name`)
Deal_fnl_mod$`Manager Name` <- as.factor(Deal_fnl_mod$`Manager Name`)
Deal_fnl_mod$`Deal Status Code` <- as.factor(Deal_fnl_mod$`Deal Status Code`)
set.seed(50)
train <- subset(Deal_fnl_mod, split == "TRUE")
test <- subset(Deal_fnl_mod, split == "FALSE")
logr = glm(`Deal Status Code`~., data = train, family = "binomial")
p4 <- predict(logr,type = "response",newdata = test)
CM <- table(Deal_fnl_mod$`Deal Status Code`,p4>0.5)
CM
acc_logr <- sum(diag(CM))/sum(CM)
acc_logr
install.packages("ROCR")
library(ROCR)
1611/(1611+484)
624/(624+263)
Rocpred <- prediction(p4,test_m$`Deal Status Code`)
ROCperf <- performance(Rocpred,"tpr","fpr")
plot(ROCperf,colorize=TRUE)
as.numeric(performance(Rocpred,"auc")@y.values)
#............................END SCRIPT.........................................
#
# Random Forest
#
#...........................BEGIN SCRIPT........................................
spl2 <- sample.split(Deal_fnl_mod$`Deal Status Code`,SplitRatio = 0.7)
train_m <- subset(Deal_fnl_mod,spl2=="TRUE")
test_m <- subset(Deal_fnl_mod,spl2=="FALSE")
library(randomForest)
set.seed(222)
test_m <- na.omit(test_m)
rf <- randomForest(`Deal Status Code`~`Deal Cost`+Sector+`Solution Type`,data =train_m)
print(rf)
head(rf)
str(Deal_fnl_mod_num)
library(caret)
p3= predict(rf,test_m)
library(ROCR)
act = test_m$`Deal Status Code`
pred_lor = prediction(as.numeric(p3),as.numeric(act))
ROC_lor = performance(pred_lor,"tpr","fpr")
plot(ROC_lor,colorize=T,lwd =2)
abline(a=0,b=1)
AUC = performance(pred_lor,measure = "auc")
print(AUC@y.values)
#............................END SCRIPT.........................................
#
#___________________________END R SCRIPTS_______________________________________
