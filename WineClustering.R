#Wine Dataset

#importing dataset
wine1 <- wine1

attach(wine1)
str(wine1) #get data types of all attributes

#plotting some visualizations
ggplot(wine1, aes(Class, Alcohol, group=Class)) + geom_boxplot() + ggtitle("Alcohol")
ggplot(wine1, aes(Class, Malic.Acid, group=Class)) + geom_boxplot() + ggtitle("Malic Acid")
ggplot(wine1, aes(Class, Ash, group=Class)) + geom_boxplot() + ggtitle("Ash")
ggplot(wine1, aes(Class, Alcalinity.of.ash, group=Class)) + geom_boxplot() + ggtitle("Alcalinity of Ash")
ggplot(wine1, aes(Class, Magnesium, group=Class)) + geom_boxplot() + ggtitle("Magnesium")
ggplot(wine1, aes(Class, Total.phenols, group=Class)) + geom_boxplot() + ggtitle("Total Phenols")
ggplot(wine1, aes(Class, Flavanoids, group=Class)) + geom_boxplot() + ggtitle("Flavanoids")
ggplot(wine1, aes(Class, Nonflavanoid.phenols, group=Class)) + geom_boxplot() + ggtitle("Nonflavanoid Phenols")
ggplot(wine1, aes(Class, Proanthocyanins, group=Class)) + geom_boxplot() + ggtitle("Proanthocyanins")
ggplot(wine1, aes(Class, Color.intensity, group=Class)) + geom_boxplot() + ggtitle("Color Intensity")
ggplot(wine1, aes(Class, Hue, group=Class)) + geom_boxplot() + ggtitle("Hue")
ggplot(wine1, aes(Class, OD280.OD315.of.diluted.wines, group=Class)) + geom_boxplot() + ggtitle("OD280/OD315 of Diluted Wines")
ggplot(wine1, aes(Class, Proline, group=Class)) + geom_boxplot() + ggtitle("Proline")

#Observations
#Generally a low margin of difference amongst attributes is observed between the 3 cultivars (i.e seen in boxplot for Ash, Alcalinity of Ash, Magnesium, etc.)
#Are able to differentiate better on other properties (Total Phenols, Flavanoids, Color Intensity, Proline, etc.)
#Class 1 for the Malic Acid attribute contains a lot of outliers
#May be difficult to create distinguished clusters after running algorithms

#Food for thought on other iterations: Do feature selection before running kmeans??

set.seed(1)
######K-means: outliers might be a problem here
#removing first column. choosing k=3 because there are 3 cultivars
wclass <- kmeans(wine1[,-1], centers = 3, iter.max = 1000)
wclass

tbl <- table(wine1[,1], wclass$cluster)
tbl #resulting contingency table didn't produce great results

#some visualizations
#plotting kmeans results
ggplot(wine1, aes(Ash, Alcalinity.of.ash)) + geom_point(colour = wclass$cluster) +
  ggtitle("Kmeans Results: Ash vs. Alcalinity of ash")
#plotting actual cluster results
ggplot(wine1, aes(Ash, Alcalinity.of.ash)) + geom_point(colour = wine1$Class) +
  ggtitle("True Results: Ash vs. Alcalinity of ash")

#cluster graph
install.packages("factoextra")
library(factoextra)
fviz_cluster(wclass, data = wine1, ellipse.type = "convex", palette = "jco",
             ggtheme = theme_minimal())
#Cluster 1 and Cluster 2 are well defined, but Cluster 3 seems to overlap alot with 1 and 2



#### PAM Algorithm aka K-medoids clustering
install.packages("cluster")
library(cluster)
wpam <- pam(wine1[,-1], k=3)
wpam

tbl1 <- table(wine1[,1], wpam$clustering)
tbl1 #same contingency table as kmeans??

#PAM algorithm actually performed worse than kmeans algorithm
#PAM has less than 50% accuracy vs. kmeans which had ~75% accuracy
#accuracy computed manually by adding up number of True Pos/Total Count * 100
#visualize in cluster plot
fviz_cluster(wpam, ggtheme = theme_minimal())



#####Hierarchical clustering
#Example 1
dwine <- dist(as.matrix(wine1))
hwine <- hclust(dwine)
plot(hwine)

install.packages("viridisLite")
install.packages("viridis")
fviz_dend(hwine, k_colors = c("#2E9FDF", "#00AFBB", "#E7B800"),
          rect = TRUE, show_labels = FALSE)


          