#' Title Variables means by clusters
#'
#' @param object : Clustering result from usual methods of clustering
#' @param data : the data which was used to compute the clustering methods results
#' @param clusters  Possibility to display a radar of the clustering from just clustering results and the original data
#'
#' @return a radar plot
#' @author Mohamed Al Mahdi Tantaoui
#'
#' @examples :
#'
#'# depencies :
#'# devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)
#'# library(devtools)
#'# library(digest)
#'# library(ggradar)
#'# library(scales)
#'# library(dplyr)
#'# library(purrrlyr)
#'#
#'# data("iris")
#'# head(iris)
#'# # Remove species column (5) and scale the data
#'# iris.scaled <- scale(iris[, -5])
#'# true_class = iris[,5]
#'# # methods from the package stats
#'# library(stats)
#' # (kmeans method)
#'# km.res <- kmeans(iris.scaled, 3, nstart = 10)
#'# #clust_radar_viz(km.res, data = iris[,1:4])
#'# # ------------------------------------------------------------------------------
#'# # methods from the package cluster
#'# library(cluster)
#'# # (pam method)
#'# pam.res = pam(iris.scaled, 5)
#'# clust_radar_viz(pam.res)
#'# # (clara method)
#'# clara.res = clara(iris.scaled, 3)
#'# #clust_radar_viz(clara.res)
#'# # (fanny method)
#'# fanny.res = fanny(iris.scaled, 3)
#'# #clust_radar_viz(fanny.res)
#'# # ------------------------------------------------------------------------------
#'# # methods from the package fpc
#'# library(fpc)
#'# # (dbscan method)
#'# dbscan.res = dbscan(iris.scaled, eps = .4, MinPts = 4)
#'# #clust_radar_viz(object = dbscan.res, data = iris[,1:4])
#'# # dbscan method don't provide information, clusters are
#'# # required for external measures computation
#'# # ------------------------------------------------------------------------------
#'# # methods from the package mclust
#'# library(mclust)
#'# # Mclust method
#'# Mclust.res = Mclust(iris.scaled,4)
#'# #clust_radar_viz(Mclust.res)
#'# # ------------------------------------------------------------------------------
#'# # methods from the package FactoMineR
#'# library(FactoMineR)
#'# # HCPC method
#'# HCPC.res = HCPC(data.frame(iris.scaled),3)
#'# #clust_radar_viz(HCPC.res)
#'# # ------------------------------------------------------------------------------
#'# # methods from the package factoextra
#'# library(factoextra)
#'#
#'# # hkmeans method
#'# hkmeans.res = hkmeans(iris.scaled,3)
#'# #clust_radar_viz(hkmeans.res)
#'# # ------------------------------------------------------------------------------
#'# # using the two arguments of the function : labels, clusters
#'# # clust_radar_viz(object = NULL, data = iris.scaled, clusters=km.res$cluster)
#' @export
clust_radar_viz = function(object, data=NULL,clusters = NULL){

  # inspecting the input object
  # ------------------------------------------------------------------------------
  if(inherits(object, c("hkmeans", "eclust"))){

    # input : objects from the package factoextra (hkmeans and eclust methods)
    clusters = object$cluster
    clusters = as.factor(clusters)
    data = object$data


  }else if( (inherits(object, "kmeans") & !inherits(object, "eclust"))| inherits(object, "dbscan") ){

    clusters = object$cluster
    # clusters = as.factor(clusters)
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

  # preparing data for plotting
  plot.data <- cbind.data.frame(data, cluster = as.factor(clusters))


  radar.data = plot.data %>% slice_rows("cluster") %>% dmap(mean)

  # creating the radar plot
  plotdata <- radar.data %>%
    filter(as.character(cluster) %in% levels(cluster)) %>%
    select(cluster,colnames(data)) %>%
    rename(group = cluster) %>%
    mutate_at(vars(-group),
              funs(rescale))

  # plotting the radar
  ggradar(plotdata,
          grid.label.size = 4,
          axis.label.size = 4,
          group.point.size = 5,
          group.line.width = 1.5,
          legend.text.size= 10)

}
