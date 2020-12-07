#' Title Test between quantitative and qualitative variables
#'
#' @param df : quantitative(s) variable(s)
#' @param col : qualitative variable
#'
#' @return ANOVA or Student test
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
#'#library(ggplot2)
#'#library(cowplot)
#'######## Recuperer les données et kmeans ######
#'# data(iris)
#'# dat <- subset(iris, Species != "setosa")
#'# spe <- iris[,5]
#'# res.km <- kmeans(scale(iris[, -5]), 3, nstart = 25)
#'# clus<-res.km$cluster
#'###### Test ########
#'#print(TestQuanti(iris[,-5],res.km$cluster))
#'#TestQuanti(iris[,-5],res.km$cluster)
#'######################## Student vs ANOVA #################################
#' @export
TestQuanti <- function (df,col){
  col<-factor(col)
  library(cowplot)
  numcol=ncol(df)
  data<-data.frame(df,col)
  if (is.null(numcol)){
    if (nlevels(factor(col))<3){
      if(var.test(df~col)$p.value<0.05){
        print("Student Test")
        test<-t.test(df~col,var.equal=TRUE)
        method <- "Student Test"
        pvalue <-t.test(df~col,var.equal=TRUE)$p.value
        print(test)
      }
      else{
        print("Welch Test")
        test<-t.test(df~col,var.equal=FALSE)
        method <- "Welch Test"
        pvalue <-t.test(df~col,var.equal=FALSE)$p.value
        print(test)
      }
    }
    else{
      print(paste0("Test ANOVA"))
      test<-summary(aov(df~col))
      method <- "ANOVA"
      pvalue <-summary(aov(df~col))[[1]][["Pr(>F)"]]
      print(test)
    }
    plot=ggplot(data,aes(x=col,y=df))+geom_boxplot(aes(color=col))+
      scale_color_manual(values= c("#00AFBB","#E7B800","#FC4E07"))+
      labs(title=paste0(method," for p-value = ",pvalue),y=col)
    histogram = ggplot(data,aes(x=df))+
      geom_histogram(aes(y=..density..), colour="black", fill="white")+
      geom_density(alpha=.2, fill="#FF6666")+
      labs(x=col)
    cdplot= ggplot(data,aes_string(df,fill=col))+
      geom_density(position='fill',alpha=0.5)+
      labs(x=col)
    figure <- plot_grid(plot,histogram,cdplot, nrow=2, ncol=2)
    print(figure)
  }
  else{
    for (i in 1:numcol){
      names=colnames(df)[i]
      if (nlevels(factor(col))<3){
        if(var.test(df[,i]~col)$p.value<0.05){
          print("Student Test")
          test<-t.test(df[,i]~col,var.equal=TRUE)
          method <- "Student Test"
          pvalue <-t.test(df[,i]~col,var.equal=TRUE)$p.value
          print(test)
        }
        else{   #print("Welch Test")
          test<-t.test(df[,i]~col,var.equal=FALSE)
          method <- "Welch Test"
          pvalue <-t.test(df[,i]~col,var.equal=FALSE)$p.value}
        print(test)
      }
      else{   names=colnames(df)[i]
      print(paste0("Test ANOVA for ",names))

      test<-summary(aov(df[,i]~col))
      method <- "ANOVA"
      pvalue <-summary(aov(df[,i]~col))[[1]][["Pr(>F)"]]
      print(test)
      }
      plot=ggplot(data,aes(x=col,y=df[,i]))+geom_boxplot(aes(color=col))+
        scale_color_manual(values= c("#00AFBB","#E7B800","#FC4E07"))+
        labs(title=paste0(method," for ",names," p-value = ",pvalue),y=names)
      histogram = ggplot(data,aes(x=df[,i]))+
        geom_histogram(aes(y=..density..), colour="black", fill="white")+
        geom_density(alpha=.2, fill="#FF6666")+
        labs(x=names)
      cdplot= ggplot(data,aes_string(df[,i],fill=col))+
        geom_density(position='fill',alpha=0.5)+
        labs(x=names)
      figure <- plot_grid(plot,histogram,cdplot, nrow=2, ncol=2)
      print(figure)
    }
  }

}
