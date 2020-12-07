## Tutorial for the CCrac Package :


Load all the packages needed

```r
library(ggradar)
library(scales)
library(dplyr)
library(purrrlyr)
library(factoextra)
library(stats)
library(maxmatching)
library(igraph)
library(ggalluvial)
library(CCarac)
```

load the dataset Iris and proceed a k-means clustering


```r
data("iris")
head(iris)
```

```
##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
## 1          5.1         3.5          1.4         0.2  setosa
## 2          4.9         3.0          1.4         0.2  setosa
## 3          4.7         3.2          1.3         0.2  setosa
## 4          4.6         3.1          1.5         0.2  setosa
## 5          5.0         3.6          1.4         0.2  setosa
## 6          5.4         3.9          1.7         0.4  setosa
```

```r
# Remove species column (5) and scale the data
iris.scaled <- scale(iris[, -5])
true_class = iris[,5]
# methods from the package stats
library(stats)
# (kmeans method)
km.res <- kmeans(iris.scaled, 3, nstart = 10)
```

Apply our basic summary function from our package at first, to visualize the clustering results. We just have to call the function and write the object that we want to summarize, which is the output of the k-means algorithm, and the data that the k-means algorithm took for entry :


```r
clust_summary_viz(km.res, data = iris.scaled)
```

![](README_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

As you can see, it gives us a graphic output of the summary of the km.res object, linked to the original datas that we used for our kmeans. It is simply a way for us to visualize directly the informations provided by our initial object and our data.

Let's visualize the clustering in 3D, by calling the clust_viz_3d function :



```r
clust_viz_3d(km.res, data = iris.scaled)
```

In our Iris dataset, we do have the true features of the observation, that is why we can compute our external measures. If you do not have the true features, you can skip the external part of this tutorial.
We can find a contingency table of the true classification and the clustering results, that's what we are going to do with the clust_class_viz function, by computing the clustering object first, and then the true features :



```r
clust_class_viz(km.res, true_class)
```

![](README_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


Now, let's go ahead for the matching measures, by calling the matching measures function with the same parameters as before. Just as an example, let's take a look to the parameters of the next two functions. In fact, they are giving the same results, we can compute any one you want, as long as the features are provided. Indeed, if you are writing the object, the script will automatically find the clusters inside the object if those exists. It means that in contrary, you can write object = NULL if you are writing by yourself the clusters as the third parameter. It gives :


```r
#matching_measures(object = NULL, iris[,5], km.res$cluster)
matching_measures(km.res,iris[,5])
```

```
## $`Purity of Clustering`
## [1] 0.8333333
## 
## $`Maximum Matching`
## [1] 0.8333333
## 
## $`Clusters Precision`
## Cluster 1 Cluster 2 Cluster 3 
## 1.0000000 0.7659574 0.7358491 
## 
## $`Clusters Recall`
##    1    2    3 
## 1.00 0.72 0.78 
## 
## $`Clusters F-measure`
## Cluster 1 Cluster 2 Cluster 3 
## 1.0000000 0.7422680 0.7572816 
## 
## $`F-measure`
## [1] 0.8331832
```

We can now watch the purity, the maximum matching and the F-measure of the clusters, as the precision and the recall are done to calculate the F-measure. 

The 0.83 purity means that 83% of the observations grouped by the clustering algorithm belongs to the same partitions (in terms of initial features).

The maximum matching is the same as the purity measure, except that only one cluster can match one unique partition. 

The maximum matching measure is the fraction of points in a cluster Ci from the majority partition, except that in this case, one cluster can match only one unique partition


The 0.83 F-measure is the harmonical mean of the precision and the recall. By itself, the F-measure is not really meaningful, because it represent the combination of several metrics. Of course, the closest is the F-measure to 1, to better it is, but We need to observe the precision and the recall to really analyze the results, depending on the goal that we want to achieve.

 

We can do the same thing for external entropy measures :


```r
entropy_measures(km.res, iris[,5])
```

```
## $`Conditional Cluster Entropy`
## Cluster 1 Cluster 2 Cluster 3 
## 0.0000000 0.7849921 0.8329458 
## 
## $`Conditional Entropy`
## [1] 0.5402717
## 
## $`Normalized mutual information`
## [1] 0.659487
## 
## $`Variation of information`
## [1] 1.078811
```

About the conditional entropy, the more a cluster's members are split into several partitions, the higher the conditional entropy. A perfect clustering compared to the real features gives a conditional entropy of 0. This is a probability measure.

The normalized mutual information is quantifying the amount of shared informations between the clustering and the partitioning. It is a measure of the dependence between the observed joint probability and the expected joint probability, with a independence assumption. The value is in the range [0,1], with a value of 1 for the perfect clustering case.


The variation of information is pretty linked to the mutual information, we will actually use the results of the normalized mutual information to compute it. The lower the score, the better the clustering.





Now, it's time to go for our internal measures, which don't need the true features to be working. First, we'll be interested about uni-variate measures. Let's start with the Khi-2 statistic, which is going to give us the Cramer's V. It we will be compute by calling the Khi2 function, with two different qualitative data (of same length) that we want to compare. In this particular case, we will compare the clustering features with the true features, but just as an example. Of course, it doesn't mean that you need to have the true features to compute your Khi-2 test, of course not. You could compare any qualitative value with another, it could be a comparison with the clustering features and any qualitative variable. In our case, it is :


```r
Khi2(iris[,5],km.res$cluster)
```

```
## [1] 0.790855
## [1] "There is a liaison between these twos"
```

![](README_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

We can see in the results that our Cramer's V (0.79) is indeed significative, there is a liaison between the two variables.



Now, let's use the graphique.MoyCon function to understand how the variable have contributes to the formation of the groups. We also give you a threshold that you can set. For example, if you are setting the threshold at 80%, you could keep only the variables that are superior or equals to 80%, and say that this is the most relevant variables for the formation of the groups.


```r
obj <- Univarie.MoyCon(iris[,-5],km.res$cluster)
graphique.MoyCon(obj,seuil=70)
```

![](README_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

In this case, we see for example that 93.13% of the informations contained by the variable Petal.Length have been used to form the groups.



Let's see now see what we can tell about the variable means by clusters. For that, we are going to call the function clust_radar_viz, by directly declaring the data and the cluster parameters. That's why we just have to tell our function that data is equal to NULL, because we already provide it the clusters. Indeed, the object is needed only if we want to find automatically the clusters. In this case, we provide directly our clusters with the argument km.res$cluster. 

```r
clust_radar_viz(object = NULL, data = iris.scaled, clusters=km.res$cluster)
```

```
## Warning: `funs()` is deprecated as of dplyr 0.8.0.
## Please use a list of either functions or lambdas: 
## 
##   # Simple named list: 
##   list(mean = mean, median = median)
## 
##   # Auto named with `tibble::lst()`: 
##   tibble::lst(mean, median)
## 
##   # Using lambdas
##   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_warnings()` to see where this warning was generated.
```

![](README_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

The graph is giving us some interesting insights about the means of the variables by clusters. Indeed, we can see that depending the the cluster, the means of the different variables are not equivalent at all. For example, in comparison to the others, the cluster number 3 is characterized by a significantly larger mean of the Sepal Width, with a really small mean for the three other variables. It means that in the cluster 3, you will be most likely to find flowers with larger and smaller sepals.


Now, let's find out the liaison between our quantitative variables to our clusters by calling the TestQuanti function with the parameters 


```r
TestQuanti(iris[,-5],km.res$cluster)
```

```
## [1] "Test ANOVA for Sepal.Length"
##              Df Sum Sq Mean Sq F value Pr(>F)    
## col           2  76.46   38.23   218.6 <2e-16 ***
## Residuals   147  25.71    0.17                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## [1] "Test ANOVA for Sepal.Width"
##              Df Sum Sq Mean Sq F value Pr(>F)    
## col           2  14.74   7.372    79.9 <2e-16 ***
## Residuals   147  13.56   0.092                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](README_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```
## [1] "Test ANOVA for Petal.Length"
##              Df Sum Sq Mean Sq F value Pr(>F)    
## col           2  427.8  213.90   860.6 <2e-16 ***
## Residuals   147   36.5    0.25                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](README_files/figure-html/unnamed-chunk-11-2.png)<!-- -->

```
## [1] "Test ANOVA for Petal.Width"
##              Df Sum Sq Mean Sq F value Pr(>F)    
## col           2  75.95   37.98   525.7 <2e-16 ***
## Residuals   147  10.62    0.07                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](README_files/figure-html/unnamed-chunk-11-3.png)<!-- -->![](README_files/figure-html/unnamed-chunk-11-4.png)<!-- -->

We find our ANOVA test the p-value is <5%, which means that the variable that we are interested about is correlated to the categorical data, that is why We can watch the graph now. The boxplot show us the distribution of the quantitative variable throughout the categorical variable. Besides that, the second one shows the distribution of the quantitative variable where we can find that whether the trend is following the normal distribution. And the last one shows the density of each categorical value for the quantitative variables. In facts, we can find 4 different tests output and 4 different pages of graphs, because we asked your function to compute 4 different quantitative variables (iris[,5]==iris[,0:4]) to the categorical one.


Finally, we can't end this tutorial with the last internal measures, which is now multivariate. Indeed, we are going to measure how well the clusters are separated to each others, and how compact they are. Let's call the dunn_index function with the clustering object first, and then the data used for our k-means. 


```r
dunn_index(km.res, data = iris.scaled)
```

```
## [1] 0.02649665
```


We can see that the Dunn index is around 0.02, which is pretty low. Remember that the Dunn index is between ]0 and +Inf[. We will surely have a better value of the Dunn index by increasing k. By fact, it is notable that this measure is kind of insensitive, and not perfectly efficient by itself in this case to judge our clustering quality.


