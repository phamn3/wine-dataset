#Wine Dataset

#importing dataset
wine1 <- wine1

attach(wine1)
summary(wine1)

set.seed(1)
#apply kmeans, removing first column. choosing k=3 because there are 3 cultivars
wclass <- kmeans(wine1[,-1], centers = 3, iter.max = 1000)
wclass

tbl <- table(wine1[,1], wclass$cluster)
tbl

#plotting kmeans results
plot(wine1[c("Ash", "Alcalinity.of.ash")], col = wclass$cluster)

#plotting actual cluster results
plot(wine1[c("Ash", "Alcalinity.of.ash")], col = Class)

