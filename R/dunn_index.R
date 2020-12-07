#' Title Dunn Index
#'
#' @param object : Clustering result from usual methods of clustering
#' @param data : the data which was used to compute the clustering methods results
#' @param clusters : : Possibility to compute the matching Based measures from just clustering results
#'
#' @return the Dunn Index
#' @author Mohamed Al Mahdi Tantaoui
#'
#' @examples
#'# # depencies :
#'# devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)
#'# library(ggradar)
#'# library(scales)
#'# library(dplyr)
#'# library(purrrlyr)
#'# data("iris")
#'# head(iris)
#'# # Remove species column (5) and scale the data
#'# iris.scaled <- scale(iris[, -5])
#'# true_class = iris[,5]
#'# # methods from the package stats
#'# library(stats)
#'# # (kmeans method)
#'# km.res <- kmeans(iris.scaled, 3, nstart = 10)
#'# #dunn_index(km.res, data = iris.scaled)
#'# # ------------------------------------------------------------------------------
#'# # methods from the package cluster
#'# library(cluster)
#'# # (pam method)
#'# pam.res = pam(iris.scaled, 5)
#'# #dunn_index(pam.res)
#'# # (clara method)
#'# clara.res = clara(iris.scaled, 3)
#'# dunn_index(clara.res)
#'# # (fanny method)
#'# fanny.res = fanny(iris.scaled, 3)
#'# #dunn_index(fanny.res)
#'# # ------------------------------------------------------------------------------
#'# # methods from the package fpc
#'# library(fpc)
#'# # (dbscan method)
#'# dbscan.res = dbscan(iris.scaled, eps = .4, MinPts = 4)
#'# #dunn_index(object = dbscan.res, data = iris[,1:4])
#'# # dbscan method don't provide information, clusters are
#'# # required for external measures computation
#'# # ------------------------------------------------------------------------------
#'# # methods from the package mclust
#'# library(mclust)
#'# # Mclust method
#'# Mclust.res = Mclust(iris.scaled,4)
#'# #dunn_index(Mclust.res)
#'# # ------------------------------------------------------------------------------
#'# # methods from the package FactoMineR
#'# library(FactoMineR)
#'# # HCPC method
#'# HCPC.res = HCPC(data.frame(iris.scaled),3)
#'# dunn_index(HCPC.res)
#'# # ------------------------------------------------------------------------------
#'# # methods from the package factoextra
#'# library(factoextra)
#'#
#'# # hkmeans method
#'# hkmeans.res = hkmeans(iris.scaled,3)
#'#dunn_index(hkmeans.res)
#'# ------------------------------------------------------------------------------
#'# using the two arguments of the function : labels, clusters
#'#dunn_index(object = NULL, data = iris.scaled, clusters=km.res$cluster)
#' @export
dunn_index = function(object, data=NULL,clusters = NULL){

  # inspecting the input object
  # ------------------------------------------------------------------------------
  if(inherits(object, c("hkmeans", "eclust"))){

    # input : objects from the package factoextra (hkmeans and eclust methods)
    clusters = object$cluster
    clusters = as.factor(clusters)
    data = object$data


  }else if( (inherits(object, "kmeans") & !inherits(object, "eclust"))| inherits(object, "dbscan") ){

    clusters = object$cluster
    clusters = as.factor(clusters)
    # input : object from the package stats and fpc (kmeans and dbscan method)
    if(is.null(data)) stop("data is required for computing the Dunn Index for  kmeans/dbscan clusters")


  }else if(inherits(object, "Mclust")){

    # input : object from the package mclust (Mclust method)
    clusters = object$classification
    clusters = as.factor(clusters)
    data = object$data

  }else if(inherits(object, "HCPC")) {

    # input : object from the package FactoMineR (HCPC method)
    clusters = object$call$X$clust
    clusters = as.factor(clusters)
    data = object$data.clust[,-ncol(object$data.clust)]

  }else if(inherits(object, "hcut")){

    # input : objects from the package factoextra (hcut method)
    if(inherits(object$data, "dist")){
      if(is.null(data)) stop("The option 'data' is required for an object of class hcut." )
    }
    else data <- object$data
  }else if( inherits(object, "pam") | inherits(object,"clara") | inherits(object, "fanny")){

    # input : object from the package cluster (pam, clara and fanny methods)
    clusters = object$clustering
    clusters = as.factor(clusters)
    data <- object$data


  }else if( !is.null(clusters) & !is.null(data) ){

    # verifying that the labels and cluster vectors have the same length
    if( length(clusters) != nrow(data) ){
      stop("the two arguments data and clusters must have the same number of rows")
    }

    clusters = as.factor(clusters)

  }else{ stop("Can't handle an object of class ", class(object)) }
  # ------------------------------------------------------------------------------

  # computing the number of clusters
  clust_number = length(levels(clusters))

  # attributing the cluster to each observation
  data.cluster <- cbind.data.frame(data, cluster = as.integer(clusters))

  # computing the observation index of each cluster
  index = list()
  for (i in 1:clust_number) {

    index[[i]] = which( data.cluster[,ncol(data.cluster)] == i, arr.ind = TRUE )

  }

  # computing the proximity matrix
  proximity = dist(data)
  proximity = as.matrix(proximity)

  # initializing the minimum distance matrix of clusters
  min_matrix = matrix(ncol=clust_number, nrow=clust_number)

  for (i in 1:clust_number) {
    for (j in 1:clust_number) {

      # computing the minimum distance between two clusters
      min_matrix[i,j] = min( proximity[ index[[i]], index[[j]] ] )

    }

  }
  # initializing the maximum intra-distance of clusters
  max_distance = c()

  for (i in 1:clust_number) {

    # computing the maximum distance between observation in the same cluster
    max_distance = append(max_distance,max(proximity[ index[[i]], index[[i]] ]))
  }

  # computing Dunn
  diag(min_matrix) = Inf
  dunn = min(min_matrix)/max(max_distance)

  return(dunn)


}





