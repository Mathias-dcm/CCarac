#' Title : Entropy based Measures (external measures)
#'
#' @param object : Clustering result from usual methods of clustering
#' @param features : True Partition (Classification) of the observation
#' @param clusters : Possibility to compute the matching Based measures from just clustering results
#'
#' @return Entropy Based Measures
#' @author Mohamed Al Mahdi Tantaoui
#'
#' @examples :
#' # # # depencies :
#'# library(maxmatching)
#'# library(igraph)
#'# # # ------------------------------------------------------------------------------
#'# # # Data preparation
#'# # ------------------------------------------------------------------------------
#'# data("iris")
#'# head(iris)
#'# Remove species column (5) and scale the data
#'# iris.scaled <- scale(iris[, -5])
#'# true_class = iris[,5]
#'# methods from the package stats
#'# library(stats)
#'# (kmeans method)
#'# km.res <- kmeans(iris.scaled, 3, nstart = 10)
#'# entropy_measures(km.res, true_class)
#'# ------------------------------------------------------------------------------
#'# methods from the package cluster
#'# library(cluster)
#'# (pam method)
#'# pam.res = pam(iris.scaled, 3)
#'# matching_measures(pam.res, true_class)
#'# (clara method)
#'# clara.res = clara(iris.scaled, 3)
#'# matching_measures(clara.res, true_class)
#'# (fanny method)
#'# fanny.res = fanny(iris.scaled, 3)
#'# matching_measures(fanny.res, true_class)
#'# ------------------------------------------------------------------------------
#'# methods from the package fpc
#'# library(fpc)
#'# (dbscan method)
#'# dbscan.res = dbscan(iris.scaled, eps = 0.2)
#'# matching_measures(dbscan.res, true_class)
#'# dbscan method don't provide information, clusters are
#'# required for external measures computation
#'# ------------------------------------------------------------------------------
#'# methods from the package mclust
#'# library(mclust)
#'# Mclust method
#'# Mclust.res = Mclust(iris.scaled,4)
#'# matching_measures(Mclust.res, true_class)
#'# ------------------------------------------------------------------------------
#'# methods from the package FactoMineR
#'# library(FactoMineR)
#'# HCPC method
#'# HCPC.res = HCPC(data.frame(iris.scaled),3)
#'# matching_measures(HCPC.res,true_class)
#'# ------------------------------------------------------------------------------
#'# methods from the package factoextra
#'# library(factoextra)
#'# hkmeans method
#'# hkmeans.res = hkmeans(iris.scaled,3)
#'# matching_measures(hkmeans.res,true_class)
#'# ------------------------------------------------------------------------------
#'# using the two arguments of the function : features, clusters
#'# matching_measures(object = NULL,features = true_class, clusters=HCPC.res$call$X$clust)
#' @export
entropy_measures = function(object, features=NULL, clusters=NULL){
  # inspecting the input object
  # ------------------------------------------------------------------------------
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
    if( is.null(features) | is.null(clusters) ) stop("the dbscan method don't provides cluster information, please use the two arguments 'features' and 'clusters'." )

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
  # ------------------------------------------------------------------------------

  # computing entropy based measures for clustering
  # ------------------------------------------------------------------------------
  # contingency table : number of observations common to the clusters and the label classes
  contingency = table(clusters, features)

  # dividing each element of the contingency table by the sum of his row
  contingency_ratio_sum = contingency / rowSums(contingency)

  # preparing for computing the entropy of each cluster
  transformation = -log(contingency_ratio_sum, base = 2) * contingency_ratio_sum

  # replacing the NaN values (log results) by 0 (we chose 0 for its sum neutrality)
  transformation[is.nan(transformation)] <- 0

  conditional_cluster_entropy = rowSums(transformation)
  names(conditional_cluster_entropy) = sprintf("Cluster %s", seq(1:length(conditional_cluster_entropy)))

  # the total number of observations
  n = sum(contingency)

  # computing the conditional entropy of the clustering method
  conditional_entropy = sum(conditional_cluster_entropy * rowSums(contingency) / n)

  # computing the cluster entropy
  cluster_proportions = rowSums(contingency)/n
  cluster_entropy = -sum(log(cluster_proportions, base=2) * cluster_proportions)

  # computing the partitioning entropy
  partition_proportions = colSums(contingency)/n
  partition_entropy = -sum(log(partition_proportions, base=2) * partition_proportions)

  # computing the mutual information
  sum_ratio = t( (t(contingency) * n)/colSums(contingency) ) /rowSums(contingency)
  information_matrix = ((contingency/n) * log(sum_ratio, base = 2))
  information_matrix[is.nan(information_matrix)] <- 0
  mutual_information = sum(information_matrix)

  # computing normalized mutual information
  nmi = mutual_information / sqrt(cluster_entropy * partition_entropy)

  # Variation of information
  vi = cluster_entropy + partition_entropy - 2 * mutual_information

  # organizing the output (entropy based measures) in a list
  # ------------------------------------------------------------------------------
  entropyMeasures = list()

  # Cluster Conditional Entropy output
  entropyMeasures[["Conditional Cluster Entropy"]] = conditional_cluster_entropy

  # Conditional Entropy Output
  entropyMeasures[["Conditional Entropy"]] = conditional_entropy

  # Normalized mutual information matrix
  entropyMeasures[["Normalized mutual information"]] = nmi

  # Variation of information
  entropyMeasures[["Variation of information"]] = vi




  # returning the conditional entropy based measures of the clustering method
  return(entropyMeasures)

}



















