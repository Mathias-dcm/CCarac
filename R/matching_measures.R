# external validation measures : Matching Based Measures
#' Title : external validation measures : Matching Based Measures
#'
#' @param object : Clustering result from usual methods of clustering
#' @param features : True Partition (Classification) of the observation
#' @param clusters : Possibility to compute the matching Based measures from just clustering results
#'
#' @return external validation measures : Matching Based Measures
#' @author Mohamed Al Mahdi Tantaoui
#'
#' @examples :
#'
#'# # depencies :
#'# library(maxmatching)
#'# library(igraph)
#'# # # # ------------------------------------------------------------------------------
#'# # # # Data preparation
#'# # # ------------------------------------------------------------------------------
#'# data("iris")
#'# head(iris)
#'# # Remove species column (5) and scale the data
#'# iris.scaled <- scale(iris[, -5])
#'# true_class = iris[,5]
#'# # methods from the package stats
#'# library(stats)
#'# # (kmeans method)
#'# km.res <- kmeans(iris.scaled, 3, nstart = 10)
#'#matching_measures(km.res, true_class)
#'# # ------------------------------------------------------------------------------
#'# # methods from the package cluster
#'# library(cluster)
#'# # (pam method)
#'# pam.res = pam(iris.scaled, 3)
#'# matching_measures(pam.res, true_class)
#'# # (clara method)
#'# clara.res = clara(iris.scaled, 3)
#'# matching_measures(clara.res, true_class)
#'# # (fanny method)
#'# fanny.res = fanny(iris.scaled, 3)
#'# matching_measures(fanny.res, true_class)
#'# # ------------------------------------------------------------------------------
#'# # methods from the package fpc
#'# library(fpc)
#'# # (dbscan method)
#'# dbscan.res = dbscan(iris.scaled, eps = 0.2)
#'# matching_measures(dbscan.res, true_class)
#'# # dbscan method don't provide information, clusters are
#'# # required for external measures computation
#'# # ------------------------------------------------------------------------------
#'# # methods from the package mclust
#'# library(mclust)
#'# # Mclust method
#'# Mclust.res = Mclust(iris.scaled,4)
#'# matching_measures(Mclust.res, true_class)
#'# # ------------------------------------------------------------------------------
#'# # methods from the package FactoMineR
#'# library(FactoMineR)
#'# # HCPC method
#'# HCPC.res = HCPC(data.frame(iris.scaled),3)
#'# matching_measures(HCPC.res,true_class)
#'# # ------------------------------------------------------------------------------
#'# # methods from the package factoextra
#'# library(factoextra)
#'# # hkmeans method
#'# hkmeans.res = hkmeans(iris.scaled,3)
#'# matching_measures(hkmeans.res,true_class)
#'# # ------------------------------------------------------------------------------
#'# # using the two arguments of the function : features, clusters
#'# matching_measures(object = km.res,features = iris[,5], clusters=NULL)
#'#
#'# matching_measures(km.res,iris[,5])
#' @export
matching_measures = function(object, features=NULL, clusters=NULL){
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

  # computing matching based measures for clustering
  # ------------------------------------------------------------------------------
  # contingency table : number of observations common to the clusters and the label classes
  contingency = table(clusters, features)

  # compute the max of each row
  max_rows = apply(contingency, 1, max)

  # number of observations
  n = length(features)

  # compute the purity of the clustering method
  purity = sum(max_rows)/n

  # compute the maximum matching
  contingency_df = as.data.frame(contingency)

  # treat the contingency table as a complete weighted bipartite graph
  contingency_graph = igraph::graph_from_data_frame(contingency_df, directed = F, vertices = NULL)
  igraph::E(contingency_graph)$weight = contingency_df$Freq
  # plot(contingency_graph, layout = layout_with_graphopt)

  # computing the matching weight of the contingency graph
  matching_weight = maxmatching::maxmatching(contingency_graph, weighted = F)$matching_weight

  # computing the maximum matching of clustering
  maximum_matching = matching_weight/n

  # computing the precision of the clusters
  # maximum of the rows
  max_precision = apply(contingency, 1, max)

  # rows sum
  sum_precision = apply(contingency,1,sum)

  # precision of clusters
  precision = max_precision / sum_precision
  names(precision) = sprintf("Cluster %s", seq(1:nrow(contingency)))

  # computing the recall of the clusters
  # sum of columns of the tables
  sum_col_recall = apply(contingency,2,sum)

  # dividing each element by the columns sum
  max_sum_ratio = t( t(contingency) / sum_col_recall )

  # computing the recall of each  cluster
  recall = apply(max_sum_ratio,1,max)

  # computing the F-measure of each clusters
  f_measure_cluster = (2 * precision * recall)/(precision + recall)
  names(f_measure_cluster) = sprintf("Cluster %s", seq(1:nrow(contingency)))

  # computing the F-measure
  f_measure = mean(f_measure_cluster)
  # ------------------------------------------------------------------------------


  # organizing the output(external measures) in a list
  # ------------------------------------------------------------------------------
  externalMeasures = list()

  # Purity output
  externalMeasures[["Purity of Clustering"]] = purity

  # Maximum Matching output
  externalMeasures[["Maximum Matching"]] = maximum_matching

  # Precision of clusters Output
  externalMeasures[["Clusters Precision"]] = precision

  # Recall of clusters Output
  externalMeasures[["Clusters Recall"]] = recall

  # F-measure of clusters Output
  externalMeasures[["Clusters F-measure"]] = f_measure_cluster

  # F-measure Output
  externalMeasures[["F-measure"]] = f_measure
  # ------------------------------------------------------------------------------

  return(externalMeasures)

}






