#' Title 3D reprenstation of the Clusters
#'
#' @param object : Clustering result from usual methods of clustering
#' @param data : the data which was used to compute the clustering methods results
#' @param clusters : : Possibility to display in 3D the clustering  from just the vector of clustering results
#' @param stand : Boolean used to skip the scaling procedure for the HCPC method results
#'
#' @return 3D representation of the clustering results
#' @author Mohamed Al Mahdi Tantaoui
#'
#' @examples :
#'
#'# depencies :
#'# library(ggplot2)
#'# library(plotly)
#'# library(dplyr)
#'# library(factoextra)
#'# library(stats)
#'#
#'# data("iris")
#'# head(iris)
#'# # Remove species column (5) and scale the data
#'# iris.scaled <- scale(iris[, -5])
#'# true_class = iris[,5]
#'# # methods from the package stats
#'# library(stats)
#'# # (kmeans method)
#'# km.res <- kmeans(iris.scaled, 3, nstart = 10)
#'# clust_viz_3d(km.res, data = iris.scaled)
#'# # ------------------------------------------------------------------------------
#'# # methods from the package cluster
#'# library(cluster)
#'# # (pam method)
#'# pam.res = pam(iris.scaled, 5)
#'# clust_viz_3d(pam.res)
#'# # (clara method)
#'# clara.res = clara(iris.scaled, 3)
#'# clust_viz_3d(clara.res)
#'# # (fanny method)
#'# fanny.res = fanny(iris.scaled, 3)
#'# clust_viz_3d(fanny.res)
#'# # ------------------------------------------------------------------------------
#'# # methods from the package fpc
#'# library(fpc)
#'# # (dbscan method)
#'# dbscan.res = dbscan(iris.scaled, eps = .4, MinPts = 4)
#'# clust_viz_3d(object = dbscan.res, data = iris.scaled)
#'# # dbscan method don't provide information, clusters are
#'# # required for external measures computation
#'# # ------------------------------------------------------------------------------
#'# # methods from the package mclust
#'# library(mclust)
#'# # Mclust method
#'# Mclust.res = Mclust(iris.scaled,4)
#'# clust_viz_3d(Mclust.res)
#'# # ------------------------------------------------------------------------------
#'# # methods from the package FactoMineR
#'# library(FactoMineR)
#'# # HCPC method
#'# HCPC.res = HCPC(data.frame(iris.scaled),3)
#'# clust_viz_3d(HCPC.res)
#'# # ------------------------------------------------------------------------------
#'# # methods from the package factoextra
#'# library(factoextra)
#'# # hkmeans method
#'# hkmeans.res = hkmeans(iris.scaled,3)
#'# clust_viz_3d(hkmeans.res)
#'# # ------------------------------------------------------------------------------
#'# # using the two arguments of the function : labels, clusters
#'# clust_viz_3d(object = NULL, data = iris.scaled, clusters=km)
#' @export
clust_viz_3d = function(object, data=NULL,clusters = NULL, stand = TRUE){

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
    if(is.null(data)) stop("data is required for plotting kmeans/dbscan clusters")


  }else if(inherits(object, "Mclust")){

    # input : object from the package mclust (Mclust method)
    clusters = object$classification
    clusters = as.factor(clusters)
    data = object$data

  }else if(inherits(object, "HCPC")) {

    # input : object from the package FactoMineR (HCPC method)
    clusters = object$call$X$clust
    clusters = as.factor(clusters)
    data <- res.hcpc <- object
    stand = FALSE

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

  # scaling the data before dimensionality reduction
  if(stand) data <- scale(data)

  # boolean of performing the PCA
  pca_performed <- FALSE

  # Prepare the data for plotting
  # ++++++++++++++++++++++++
  # PCA is performed depending on the number of variables
  if(inherits(data, c("matrix", "data.frame"))){
    if(ncol(data)>3){
    pca <- stats::prcomp(data, scale = FALSE, center = FALSE)
    ind <- facto_summarize(pca, element = "ind", result = "coord", axes = 1:3)
    eig <- get_eigenvalue(pca)[1:3,2]

    }else if(ncol(data) == 3){

      # PCA is not performed
      ind <- as.data.frame(data, stringsAsFactors = TRUE)
      ind <- cbind.data.frame(name = rownames(ind), ind, stringsAsFactors = TRUE)

    }
    else{
      stop("The dimension of the data < 3! No plot.")
    }
  }
  else if(inherits(data, "HCPC")){
    ind <- res.hcpc$call$X[, c(1:3, ncol(res.hcpc$call$X))]
    colnames(ind) <- c("Dim.1", "Dim.2", "Dim.3", "clust")
    ind <- cbind.data.frame(name = rownames(ind), ind, stringsAsFactors = TRUE)

    label_coord <- ind
    eig <- get_eigenvalue(res.hcpc$call$t$res)[1:3,2]

  }
  else stop("A data of class ", class(data), " is not supported.")

  plot.data <- cbind.data.frame(ind, cluster = clusters, stringsAsFactors = TRUE)

  p <- plot_ly(plot.data, x=~Dim.1, y=~Dim.2, z=~Dim.3, color=~clusters) %>% add_markers(size=2)
  print(p)
  return(p)


}

















