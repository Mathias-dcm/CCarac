#' Title visual contingency table of the true classification (partitioning) and the clustering results
#'
#' @param object : Clustering result from usual methods of clustering
#' @param features : True Partition (Classification) of the observation
#' @param clusters : Possibility to display a visual contingency table from just clustering results
#'
#' @return a GGalluvial graph
#' @author Mohamed Al Mahdi Tantaoui
#'
#' @examples :
#' # depencies :
#' #library("ggplot2")
#' #library("ggalluvial")
#' # features : vector of factors
#' # clusters : vector of factors
#' # Data preparation
#' # +++++++++++++++
#' # data("iris")
#' # head(iris)
#' # # Remove species column (5) and scale the data
#' iris.scaled <- scale(iris[, -5])
#' # true_class = iris[,5]
#' # # methods from the package stats
#' # library(stats)
#' # # (kmeans method)
#' # km.res <- kmeans(iris.scaled, 5, nstart = 10)
#' # clust_class_viz(km.res, true_class)
#' # # ------------------------------------------------------------------------------
#' # # methods from the package cluster
#' # library(cluster)
#' # # (pam method)
#' # pam.res = pam(iris.scaled, 3)
#' # clust_class_viz(pam.res, true_class)
#' # # (clara method)
#' # clara.res = clara(iris.scaled, 3)
#' # clust_class_viz(clara.res, true_class)
#' # # (fanny method)
#' # fanny.res = fanny(iris.scaled, 3)
#' # clust_class_viz(fanny.res, true_class)
#' # # ------------------------------------------------------------------------------
#' # # methods from the package fpc
#' # library(fpc)
#' # # (dbscan method)
#' # dbscan.res = dbscan(iris.scaled, eps = 0.2)
#' # clust_class_viz(dbscan.res, true_class)
#' # # dbscan method don't provide information, clusters are
#' # # required for external measures computation
#' # # ------------------------------------------------------------------------------
#' # # methods from the package mclust
#' # library(mclust)
#' # # Mclust method
#' # Mclust.res = Mclust(iris.scaled,4)
#' # clust_class_viz(Mclust.res, true_class)
#' # # ------------------------------------------------------------------------------
#' # # methods from the package FactoMineR
#' # library(FactoMineR)
#' # # HCPC method
#' # HCPC.res = HCPC(data.frame(iris.scaled),3)
#' # clust_class_viz(HCPC.res,true_class)
#' # # ------------------------------------------------------------------------------
#' # # methods from the package factoextra
#' # library(factoextra)
#' # # hkmeans method
#' # hkmeans.res = hkmeans(iris.scaled,3)
#' # clust_class_viz(hkmeans.res,true_class)
#' # ------------------------------------------------------------------------------
#' # using the two arguments of the function : features, clusters
#' #clust_class_viz(object = NULL,features = true_class, clusters=HCPC.res$call$X$clust)
#' @export
clust_class_viz = function(object, features=NULL, clusters=NULL){

  if(inherits(object, c("hkmeans", "eclust", "kmeans"))){

    # input : objects from the package factoextra (hkmeans and eclust methods)
    clusters = object$cluster
    clusters = as.factor(clusters)
    # verifying that the features and cluster vectors have the same length
    if( length(clusters) != length(features) ) stop("the two arguments features and object clusters must have the same length")

  }else if( inherits(object, "kmeans") ){

    # input : object from the package stats (kmeans method)
    clusters = object$cluster
    clusters = as.factor(clusters)
    # verifying that the features and cluster vectors have the same length
    if( length(clusters) != length(features) ) stop("the two arguments features and kmeans object clusters must have the same length")

  }else if(inherits(object, "Mclust")){

    # input : object from the package mclust (Mclust method)
    clusters = object$classification
    clusters = as.factor(clusters)
    # verifying that the features and cluster vectors have the same length
    if( length(clusters) != length(features) ) stop("the two arguments features and object clusters must have the same length")

  }else if(inherits(object, "HCPC")) {

    # input : object from the package FactoMineR (HCPC method)
    clusters = object$call$X$clust
    clusters = as.factor(clusters)
    # verifying that the features and cluster vectors have the same length
    if( length(clusters) != length(features) ) stop("the two arguments features and object clusters must have the same length")

  }else if(inherits(object, "dbscan")){

    # input : object from the package fpc (dbscan method)
    if( is.null(features) | is.null(clusters) ) stop("the dbscan method don't provides cluster information, please use
                                                   the two arguments 'features' and 'clusters'." )

  }else if( inherits(object, "pam") | inherits(object,"clara") | inherits(object, "fanny")){

    # input : object from the package cluster (pam, clara and fanny methods)
    clusters = object$clustering
    clusters = as.factor(clusters)
    # verifying that the features and cluster vectors have the same length
    if( length(clusters) != length(features) ) stop("the two arguments features and object clusters must have the same length")

  }else if(!is.null(object$data) & !is.null(object$cluster)){
    # Any object containing data and cluster elements
    data <- object$data
    cluster <- object$cluster
    clusters = as.factor(clusters)
  }else if( !is.null(clusters) & !is.null(features) ){

    # verifying that the features and cluster vectors have the same length
    if( length(clusters) != length(features) ){
      stop("the two arguments features and clusters must have the same length")
    }
  }else{ stop("Can't handle an object of class ", class(object)) }

  # contingency table : number of observations common to the clusters and the label classes
  contingency = table(clusters, features)

  #   maximum matching visualization
  p = ggplot(data = data.frame(contingency),aes(axis1 = clusters, axis2 = features, y = Freq)) +
    scale_x_discrete(limits = c("Clusters", "True Classes"), expand = c(.2, .05)) +
    xlab(" Clusters and True Classes Matches ") +
    geom_alluvium(aes(fill = features)) +
    geom_stratum() +
    geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
    theme_minimal() +
    ggtitle("Clusters and True Classes Mapping")


  return(p)

}
