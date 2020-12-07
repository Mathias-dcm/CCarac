#' Title Khi2
#'
#' @param df : First qualitative variable
#' @param col : Second qualitative variable
#'
#' @return the Kramer's V
#' @author Mohamed Al Mahdi Tantaoui
#'
#' @examples
#'# # depencies :
#'# library(cowplot)
#'# library(ggplot2)
#'####### Package
#'#--install.packages("ggplot2")
#'#install.packages("cowplot")
#'#######Library
#'# library(ggplot2)
#'# library(cowplot)
#'######## Recuperer les données et kmeans ######
#'# data(iris)
#'# dat <- subset(iris, Species != "setosa")
#'# spe <- iris[,5]
#'# res.km <- kmeans(scale(iris[, -5]), 3, nstart = 25)
#'# clus<-res.km$cluster
#'############################# Khi 2 - V Cramer  ############################
#' @export
Khi2 <- function(df,col){
  col=factor(col)
  data<-data.frame(df,col)
  #We perform a chi2 test here
  khi2<-chisq.test(df, col, correct = FALSE, simulate.p.value=TRUE)
  #We extract the test
  khi2 <- as.numeric(khi2$statistic)
  #Got the number of column
  n= length(df)
  #Get the size of df and col
  p = length(levels(as.factor(df)))
  q = length(levels(as.factor(col)))
  #Calculate the V-Cramer
  m=min(p-1,q-1)
  V=sqrt(khi2/(n*m))
  print(V)
  #Case of each V-Cramer
  if (V < 0.5){
    print('There is no liason between these twos')} #When V is too low
  else{ print('There is a liason between these twos') } #When V is significant

  plot = ggplot(data, aes(x = df, fill = col)) +
    geom_bar(position = "fill") +
    labs(y = "proportion")
  print(plot)
}
