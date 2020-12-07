#' Title Conditional mean of the variables to the classes
#'
#' @param obj : equals to another function "Univarie.MoyCon(data,cluster)" with the data at first, and then classes in second
#' @param seuil : Treshold
#'
#' @return Conditional mean
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
#'# res.km <- kmeans(scale(iris[, -5]), 3, nstart = 25)
#'# clus<-res.km$cluster
#'####### Moy Con ########
#'# obj <- Univarie.MoyCon(iris[,-5],res.km$cluster)
#'# print(class(obj))
#'# print(obj)
#'# graphique.MoyCon(obj,seuil=70)
#'############################ MOYENNE CONDITIONELLE##################################
#'#Recuperer la moyenne condition pour savoir l'importance de chaque variable
#'@export
stat.comp <- function(x,y){
  #nombre de groupes et observation
  K <- length(unique(y))
  n <- length(x)
  #La moyenne
  m <- mean(x)
  #Variabilité globale
  TSS <- sum((x-m)^2)
  #Effectifs conditionnels
  nk <- table(y)
  #Moyennes conditionnelles
  mk <- tapply(x,y,mean)
  #Variabilité expliquée
  BSS <- sum(nk * (mk - m)^2)
  #Moyennes + prop. variance expliquée
  result <- c(mk,100.0*BSS/TSS)
  #Nommer les élements du vecteur
  names(result) <- c(paste("G",1:K),"% epl.")
  #Renvoyer le vecteur résultat
  return(result)
}


################ On interge dans un S3 ########################
#Création de la classe UNIVARIE.MOYCON

#Constructeur de données
#'@export
Univarie.MoyCon <- function(donnees,cluster){
  #contr?le - data.frame
  ok <- is.data.frame(donnees)
  if (!ok){
    stop("Ce n'est pas un data frame")
  }
  #controle du type de chaque variable
  nb_ok <- sum(sapply(donnees,is.numeric))
  if (nb_ok < ncol(donnees)){
    stop("Il y a des variables non numériques dans le data frame")
  }
  #ok - on peut y aller
  instance <-list()
  instance$X <- donnees
  tab <- data.frame(sapply(donnees,stat.comp,y=cluster))
  instance$moy <- tab["% epl.",]
  class(instance) <- "MoyCon"
  return(instance)
}

### Print l'objet #####
#'@export
print.MoyCon <- function(object){
  #affichage de la liste des variables
  cat("Variables : ", colnames(object$X),"\n")
  #affichage des valeurs propres
  #cat("Moyenne condition : ",object$moy)
  print('Moyenne condition:')
  print(object$moy)
}

#######Graphique##########
#'@export
graphique.MoyCon<-function(object,seuil=80){#Laisser les choix de seuil à l'utilisateur
  library(ggplot2)
  lastline <-object$moy
  #Transposer ligne à colonne
  tab <-data.frame(t(lastline))
  #AJouter colonne de variable
  tab[,2] <-rownames(tab)
  #Renommer les colonnes
  names(tab)=c("Pourcentage","Name")
  #Graphique
  ggplot(tab, aes(x =Name, y = Pourcentage, color=Name)) +
    geom_bar(stat="identity",fill="white")+
    geom_hline(yintercept=seuil, linetype="dashed", color = "red")+
    ylim(0, 100)+
    geom_text(aes(label= round(Pourcentage,digits=2)), vjust=1.8, color="#62AEAE", size=3.5)+
    labs(x = "X variable", y = "Cumulative %")
}

