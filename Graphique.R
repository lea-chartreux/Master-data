#######
#Régression
####
library(ggplot2)
library(tidyr)
library(dplyr)
iris = Droite_d_ajustement
class(iris)
class(iris$NbAtt)


ggplot(iris, aes(x= NB, y = NbAct)) +
  geom_boxplot() +
  ggtitle("Comparaison des distributions entre les deux groupes")+
  scale_color_viridis_d()


library(ggpubr)
ggqqplot(iris, x = "NbAct",
         ggtheme = theme_bw())

library(reshape2)
data_long <- iris %>%
  pivot_longer(cols = c(NbAct, NbAtt), names_to = "Variable", values_to = "Nombre")

ggplot(data_long, aes(x = NbAct, y = NbAtt)) +
  geom_line() +
  geom_point() +
  labs(title = "Nombre d'habitants et d'animaux par maison",
       x = "Maison",
       y = "Nombre") +
  theme_minimal()



data_long <- iris %>%
  pivot_longer(cols = c(NbAct, NbAtt), names_to = "Variable", values_to = "Nombre")

ggplot(iris, aes(x = NbAct, y = NbAtt)) +
  geom_point()+
  geom_smooth(method="lm", se=FALSE, color="red")



  labs(title = "Nombre d'habitants et d'animaux par maison",
       x = "Maison",
       y = "Nombre") +
  theme_minimal()
  
  
  
  library(ggplot2)
  
  library(ggplot2)
  
  # Création des données
  set.seed(42)
  data <- data.frame(
    Groupe = rep(c("Groupe 1", "Groupe 2"), each = 100),
    Valeur = c(rnorm(100, mean = 50, sd = 10),  # Groupe 1 : Moyenne 50, écart-type 10
               rnorm(100, mean = 55, sd = 12))  # Groupe 2 : Moyenne 55, écart-type 12
  )
  
  # Tracé du boxplot
  ggplot(data, aes(x = Groupe, y = Valeur, fill = Groupe)) +
    geom_boxplot() +
    ggtitle("Comparaison des distributions (Boxplot)") +
    theme_minimal()
  
  
  
  groupe1 <- iris$NbAtt
  groupe2 <- iris$NbAct
  
  # Tracé des boîtes à moustaches
  boxplot(groupe1, groupe2,
          names = c("Groupe 1", "Groupe 2"),
          col = c("lightblue", "lightgreen"),
          main = "Comparaison des distributions",
          ylab = "Valeurs",
          scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))  

wilcox.test(iris$NbAtt, iris$NbAct, paired = TRUE) 


ggplot(iris, aes(x = NB, y = NbAct)) +
  geom_point(size = 1) +
  labs(title = "Distribution du nombre de caractère au sein des noms de document en fonction de leur numéro",
       x = "Numéro associé au document",
       y = "Nombre de caractère des noms de documents") +
        theme_bw()

ggplot(iris, aes(x = NB, y = NbAct)) +
  geom_point(size = 1) +
  theme_bw()


# theme_classic2()# la part des individus qui s'écarte de la valeur de référence
# écart brut de cette valeur
# nuage de points en rond proportionnel 


#I count the occurence of each couple of values. Eg : number of time a=1 and b=1, number of time a=1 and b=2 etc...
AA <- xyTable(iris$x,iris$y,iris$Size)

#Now I can plot this ! I represent the dots as big as the couple occurs often
coeff_bigger <- iris$Size
iris<-VF
#cex=AA$x*coeff_bigger
par(mar = c(4, 6, 10, 6))
,mar = c(5.1, 4.1, 2.1, 4.1)
plot(iris$x , iris$y , axes = FALSE,cex=iris$fr*30, col=rgb(0.1,0.16,0.6), pch=16, xlab= "La part des individus qui s'écarte de la valeur de référence" , ylab=" ", xlim=c(-10,10) , ylim=c(0,0.1))
axis(1)
# Ajouter l'axe des ordonnées (Y) au centre (x = 0) sans chiffres ni traits
abline(v = 0, col = "black")




text(AA$x , AA$y )
class(AA$size)
#Note : It's easy to make a function that will compute this kind of plot automaticaly :
represent_discrete_variable <- function(var1, var2 , coeff_bigger){
  AA=xyTable(var1,var2)
  plot(AA$x , AA$y , cex=AA$size*coeff_bigger  , pch=16 , col="chocolate1" , xlab= "value of a" , ylab="value of b" )
  text (AA$x , AA$y , AA$number )
}



# Histogramme

ggplot(iris, aes(x=Size)) + geom_histogram()














library(ggplot2)

# Exemple de données
df <- data.frame(x = -10:10, y = (-10:10)^2)

# Graphique avec l'axe Y au milieu
ggplot(iris, aes(x = x, y = y)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  scale_x_continuous(position = "bottom") +
  #scale_y_continuous(sec.axis = dup_axis()) +  # Ajoute un axe secondaire
  theme(axis.title.y = element_text(hjust = 0.5))  # Centrer le titre de l'axe Y



# Chargement des données
data(iris)

# Définir x et y comme deux colonnes de iris
x <- iris$Sepal.Length  # Exemple d'axe X
y <- iris$Sepal.Width   # Exemple d'axe Y

# Comptabilisation des occurrences de chaque paire (x, y)
AA <- xyTable(x, y)

# Coefficient d'agrandissement des points
coeff_bigger <- 2

# Tracer le nuage de points avec taille proportionnelle à la fréquence
plot(AA$x, AA$y, pch = 16, 
     cex = AA$number * coeff_bigger,  # Taille des points proportionnelle
     col = rgb(0, 0, 1, 0.5), 
     xlab = "Longueur du Sépale", 
     ylab = "Largeur du Sépale",
     main = "Nuage de points proportionnel",
     xlim = c(min(x), max(x)), 
     ylim = c(min(y), max(y)))



