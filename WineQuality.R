#Wine Quality Dataset - White Wine

#importing dataset
ww <- winequality.white

#exploring properties of the white wine dataset
attach(ww) 
str(ww) #all variables are numerical/continuous/or an integer, potentially change quality column to a ordinal?
dim(ww) #~4900 rows in dataset, 12 columns
sum(is.na(ww)) #no NA's in the dataset
summary(ww) #physiochemical statistics; mean quality looks to be 6

#visualizations to start off with
ggplot(ww, aes(ww$quality)) + geom_histogram(binwidth = 1) + ggtitle("Quality")
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
ggplot(ww, aes(x=factor(0),y=residual.sugar)) + geom_boxplot(outlier.size=1) + xlab(" ") +
  ggtitle("Residual Sugar") + scale_x_discrete(breaks = NULL) + coord_flip()
  #citric acid is also low - maybe due to white wines being more sweet and fruity
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

#Everything except for alcohol had outliers
#don't think I will remove any outliers

#test for normality - data is not normal bc all p-values less than 0
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


install.packages("ggpubr")
library("ggpubr")
ggscatter(ww, x = "alcohol", y = "quality", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Alcohol", ylab = "Quality")
  #Not a linear relationship
ggscatter(ww, x = "residual.sugar", y = "density", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Residual Sugar", ylab = "Density")


#looking at multicollinearity
library("Hmisc")
wcorr <- rcorr(as.matrix(ww),type = "spearman")
wcorr
#pH and fixed acidity negatively correlated = -0.42
#density highly pos. correlated with residual sugar = 0.72
#alcohol negatively correlated with residual sugar = -0.45
#chlorides negatively correlated with alcohol = -0.57
#free sulfur dioxide correlated with total sulfur dioxide = 0.62
#density is negatively correlated with alcohol = -0.82
#quality correlated with alcohol = 0.44



#Regression Approach: SVM, NN, regression tree, train and test
#REC Curve = metric
#metrics: MAD, confusion matrix




