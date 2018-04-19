#Wine Quality Dataset - White Wine
rm(list=ls())
getwd()

#importing dataset
ww <- winequality.white

#exploring properties of the white wine dataset
attach(ww) 
str(ww) #all variables are numerical/continuous/or an integer, potentially change quality column to a ordinal?
dim(ww) #~4900 rows in dataset, 12 columns
sum(is.na(ww)) #no NA's in the dataset
summary(ww) #physiochemical statistics; mean quality looks to be 6

#visualizations to start off with - looking at distributions/spread of each variable
require(ggplot2)
ggplot(ww, aes(ww$quality)) + geom_bar(stat="count") + ggtitle("Quality") +xlab("Quality")
  #A lot of medium quality white wines (qualit of either 5 or 6); there are very few execellent and poor white wines
ggplot(ww, aes(x=factor(0),y=ww$quality)) + geom_boxplot(outlier.size=1) + xlab(" ") +
  ggtitle("Quality") + scale_x_discrete(breaks = NULL) + coord_flip()

ggplot(ww, aes(x=factor(0),y=fixed.acidity)) + geom_boxplot(outlier.size=1) + xlab(" ") +
  ggtitle("Fixed Acidity") + scale_x_discrete(breaks = NULL) + coord_flip()
ggplot(ww, aes(x=factor(0),y=volatile.acidity)) + geom_boxplot(outlier.size=1) + xlab(" ") +
  ggtitle("Volatile Acidity") + scale_x_discrete(breaks = NULL) + coord_flip()
  #Alot of outliers for volatile acidity - white wine typically has lower acidity
ggplot(ww, aes(x=factor(0),y=citric.acid)) + geom_boxplot(outlier.size=1) + xlab(" ") +
  ggtitle("Citric Acid") + scale_x_discrete(breaks = NULL) + coord_flip()
#citric acid is also low - maybe due to white wines being more sweet and fruity
ggplot(ww, aes(x=factor(0),y=residual.sugar)) + geom_boxplot(outlier.size=1) + xlab(" ") +
  ggtitle("Residual Sugar") + scale_x_discrete(breaks = NULL) + coord_flip()
ggplot(ww, aes(x=factor(0),y=chlorides)) + geom_boxplot(outlier.size=1) + xlab(" ") +
  ggtitle("Chlorides") + scale_x_discrete(breaks = NULL) + coord_flip()
  #A lot of outliers for chlorides, salt level? 
ggplot(ww, aes(x=factor(0),y=free.sulfur.dioxide)) + geom_boxplot(outlier.size=1) + xlab(" ") +
  ggtitle("Free Sulfur Dioxide") + scale_x_discrete(breaks = NULL) + coord_flip()
ggplot(ww, aes(x=factor(0),y=total.sulfur.dioxide)) + geom_boxplot(outlier.size=1) + xlab(" ") +
  ggtitle("Total Sulfur Dioxide") + scale_x_discrete(breaks = NULL) + coord_flip()
ggplot(ww, aes(x=factor(0),y=density)) + geom_boxplot(outlier.size=1) + xlab(" ") +
  ggtitle("Density") + scale_x_discrete(breaks = NULL) + coord_flip()
  #density of wine determined by concentration of sugar, alcohol, & other solids
ggplot(ww, aes(x=factor(0),y=pH)) + geom_boxplot(outlier.size=1) + xlab(" ") +
  ggtitle("pH") + scale_x_discrete(breaks = NULL) + coord_flip()
  #low pH for white wine (~3.0)
ggplot(ww, aes(x=factor(0),y=sulphates)) + geom_boxplot(outlier.size=1) + xlab(" ") +
  ggtitle("Sulphates") + scale_x_discrete(breaks = NULL) + coord_flip()
ggplot(ww, aes(x=factor(0),y=alcohol)) + geom_boxplot(outlier.size=1) + xlab(" ") +
  ggtitle("Alcohol") + scale_x_discrete(breaks = NULL) + coord_flip()

#Observations:
#there are a lot of wines with a quality of either 5, 6, or 7. Very few wines are ranked as "poor" or "execellent"
#Everything except for alcohol had alot of outliers
#don't think I will remove any outliers because it might affect dataset

#Looking at how each variable varies with Quality
ggplot(ww, aes(quality,fixed.acidity, group=quality)) + geom_boxplot(outlier.size = 1) +
  xlab("Quality") + ylab("Fixed Acidity")
ggplot(ww, aes(quality,volatile.acidity, group=quality)) + geom_boxplot(outlier.size = 1) +
  xlab("Quality") + ylab("Volatile Acidity")
ggplot(ww, aes(quality,citric.acid, group=quality)) + geom_boxplot(outlier.size = 1) +
  xlab("Quality") + ylab("Citric Acid")
ggplot(ww, aes(quality,residual.sugar, group=quality)) + geom_boxplot(outlier.size = 1) +
  xlab("Quality") + ylab("Residual Sugar")
ggplot(ww, aes(quality,chlorides, group=quality)) + geom_boxplot(outlier.size = 1) +
  xlab("Quality") + ylab("Chlorides")
ggplot(ww, aes(quality,free.sulfur.dioxide, group=quality)) + geom_boxplot(outlier.size = 1) +
  xlab("Quality") + ylab("Free Sulfur Dioxide")
ggplot(ww, aes(quality,total.sulfur.dioxide, group=quality)) + geom_boxplot(outlier.size = 1) +
  xlab("Quality") + ylab("Total Sulfur Dioxide")
ggplot(ww, aes(quality,density, group=quality)) + geom_boxplot(outlier.size = 1) +
  xlab("Quality") + ylab("Density")
ggplot(ww, aes(quality,pH, group=quality)) + geom_boxplot(outlier.size = 1) +
  xlab("Quality") + ylab("pH")
ggplot(ww, aes(quality,sulphates,group=quality)) + geom_boxplot(outlier.size = 1) +
  xlab("Quality") + ylab("Sulphates")
ggplot(ww, aes(quality,alcohol,group=quality)) + geom_boxplot(outlier.size = 1) +
  xlab("Quality") + ylab("Alcohol")

#Observation
#clear indication that a high alcohol content equates to a high quality wine


if(!(require(ggpubr))) install.packages("ggpubr")
ggscatter(ww, x = "residual.sugar", y = "density", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Residual Sugar", ylab = "Density")


#test for normality - variables do not follow a normal distribution bc all p-values are less than 0
shapiro.test(ww$fixed.acidity)
shapiro.test(ww$volatile.acidity)
shapiro.test(ww$citric.acid)
shapiro.test(ww$residual.sugar)
shapiro.test(ww$chlorides)
shapiro.test(ww$free.sulfur.dioxide)
shapiro.test(ww$total.sulfur.dioxide)
shapiro.test(ww$density)
shapiro.test(ww$pH)
shapiro.test(ww$sulphates)
shapiro.test(ww$alcohol)
shapiro.test(ww$quality)


#looking at multicollinearity - calculating spearman rank correlation along with associated p values
library("Hmisc")
wcorr <- rcorr(as.matrix(ww),type = "spearman")
wcorr
#Overall not many features are correlated with each other, and not highly either
#pH and fixed acidity negatively correlated = -0.42
#density highly pos. correlated with residual sugar = 0.72
#alcohol negatively correlated with residual sugar = -0.45
#chlorides negatively correlated with alcohol = -0.57
#free sulfur dioxide correlated with total sulfur dioxide = 0.62
#density is negatively correlated with alcohol = -0.82
#quality correlated with alcohol = 0.44

#Trying out PCA
library("factoextra")
#apply PCA
ww.pca <- prcomp(ww[,1:11], center=TRUE, scale. = TRUE)
fviz_eig(ww.pca) #visualizing eigenvalues
#analyzing results: standards deviations & rotations (loadings)
print(ww.pca)
#importance of the PC's -
#at least 93% of the variance is explained by the first 8 PC's, at least 97% of the varince explained by first 9PC's
summary(ww.pca)

#turn predictor variable into ordinal categorical variable
ww$quality <- as.factor(ww$quality) #turn into categorical
is.ordered(quality) #checking to see if ordered - no
ww$quality <- as.ordered(ww$quality)
ww$quality #checked that quality was an ordered factor

#train test split - doing a 75/25 split
set.seed(100)
r.ww <- dim(ww)[1]
#75% to train
train.rate = 0.75
#remainder to test
test.rate = r.ww*(1.-train.rate)
#construct random set of training indices
train.ind <- sample(1:r.ww, train.rate*r.ww, replace=FALSE)
#construct random set of testing indices
test.ind <- setdiff(1:r.ww,train.ind)
#build train dataset
train <- subset(ww[train.ind,], select = c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar","chlorides",
                                           "free.sulfur.dioxide", "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol")) 
#build test dataset
test <- subset(ww[test.ind,], select = c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", "chlorides",
                                         "free.sulfur.dioxide", "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol"))
#labels for train dataset
train.lbls <- ww$quality[train.ind]
#labels for test dataset
test.lbls <- ww$quality[test.ind]


#First Model - kNN
set.seed(1)
if(!(require(gmodels))) install.packages("gmodels")
require(class)
knn_pred <- knn(train = train, test = test, train.lbls, k=61)
summary(knn_pred)
CrossTable(knn_pred,test.lbls, chisq = FALSE)
#Accuracy
(116+437+12)/nrow(test) #very low accuracy

#Second Model - Random Forest
set.seed(1)
if(!(require(randomForest))) install.packages("randomForest")
train.new <- cbind(train, train.lbls) #rebuilding original train dataset with data & labels
names(train.new)[names(train.new) == 'train.lbls'] <- 'quality'
rf_mod <- randomForest(quality~., data=train.new)
rf_mod
importance(rf_mod) #predictor importance
rf_pred <- predict(rf_mod, newdata = test)
table(rf_pred, test.lbls)
(11+240+426+116+14)/nrow(test)


#trying to tune RF model using caret package
set.seed(1)
#grid search
if(!(require(caret))) install.packages("caret")
if(!(require(e1071))) install.packages("e1071")
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tunegrid <- expand.grid(.mtry=c(1:15)) #searching through mtry values of 1 through 15
rf_gridsearch <- train(quality~., data=train.new, method="rf", tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch) #optimal mtry value = 2
plot(rf_gridsearch)
#rerunning RF Classifier with optimal mtry value
rf_mod2 <- randomForest(quality~.,data=train.new, ntree=500, mtry=2)
rf_mod2
rf_pred2 <- predict(rf_mod2, newdata=test)
table(rf_pred2,test.lbls)
(10+244+432+114+14)/nrow(test) #accuracy

#Third Model - SVM Classifier
set.seed(1)
svm_mod <- svm(quality~., train.new)
summary(svm_mod)
svm_pred <- predict(svm_mod, test)
table(svm_pred, test.lbls)
(2+210+428+54)/nrow(test)

#trying to tune SVM model
svm_tune <- tune(svm, train.x=train, train.y = train.lbls, kernel="radial",
                 ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
svm_tune #cost of 10 and gamma of 1
svm_mod_tuned <- svm(quality~.,train.new, kernel="radial", cost=10, gamma=1)
summary(svm_mod_tuned)
svm_tuned_pred <- predict(svm_mod_tuned,test)
table(svm_tuned_pred,test.lbls)
(7+226+438+105+15)/nrow(test) #increased accuracy of SVM
