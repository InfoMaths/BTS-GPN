library(tidyverse)

################ Chapitre 1 ###########
age <- c(28,48,47,71,22,80,48,30,31,18,22,33,44,55,66,77,88,99,10,20)
depense <- c(100,200,150,300,200,400,250,150,200,250,300,350,400,450,500,550,600,650,700,750)
df <- data.frame(age, depense)
df
summary(df)
str(df)
head(df)
tail(df)
View(df)

################ la base de données msleep ###########
data(msleep)
head(msleep)
tail(msleep)
summary(msleep)
str(msleep)
View(msleep)
dim(msleep)
mean(msleep$sleep_total)      # moyenne
median(msleep$sleep_total)    # médiane
max(msleep$sleep_total)       # le maximum
min(msleep$sleep_total)       # le minimum
sd(msleep$sleep_total)        # Ecart type ou mesure de dispersion
var(msleep$sleep_total)       # Variance
quantile(msleep$sleep_total)  # les quantiles
range(msleep$sleep_total)     # la plage
IQR(msleep$sleep_total)       # l'écart interquartile
table(msleep$vore)            # table de fréquence
table(msleep$vore, msleep$conservation) # table de contingence
table(msleep$vore, msleep$conservation, useNA = "ifany") # table de contingence
table(msleep$vore, msleep$conservation, useNA = "always") # table de contingence
table(msleep$vore, msleep$conservation, useNA = "no") # table de contingence
sum(msleep$sleep_total > 8)   # Frequency pour les animaux qui dorment plus de 8 heures
mean(msleep$sleep_total > 8)  # la moyenne des animaux qui dorment plus de 8 heures
sd(msleep$sleep_total > 8)    # l'écart type des animaux qui dorment plus de 8 heures
mean(msleep$sleep_rem, na.rm=TRUE)       # la moyenne de la durée du someil paradoxal
sd(msleep$sleep_rem, na.rm=TRUE)         # l'écart type de la durée du someil paradoxal
cor(msleep$sleep_total, msleep$sleep_rem, use = "complete.obs") # la corrélation entre la durée du someil total et la durée du someil paradoxal

################ Données Catégorielles ###########
## les variables catégorielles sont des variables qui prennent des valeurs comme vore et conservation
table(msleep$vore)            # table de fréquence
table(msleep$conservation)    # table de fréquence
table(msleep$vore, msleep$conservation) # table de contingence
proportions(table(msleep$vore)) # proportion des animaux par régime alimentaire
proportions(table(msleep$conservation)) # proportion des animaux par conservation
proportions(table(msleep$vore, msleep$conservation)) # proportion des animaux par régime alimentaire et conservation
round(proportions(table(msleep$vore, msleep$conservation)),2) # proportion des animaux par régime alimentaire et conservation
round(proportions(table(msleep$vore, msleep$conservation), margin = 1),2) # proportion des animaux par régime alimentaire et conservation
round(proportions(table(msleep$vore, msleep$conservation), margin = 2),2) # proportion des animaux par régime alimentaire et conservation
round(prop.table(table(msleep$vore, msleep$conservation)),2) # proportion des animaux par régime alimentaire et conservation

################ Nuage de points ###########
plot(msleep$sleep_total, msleep$sleep_rem)
plot(msleep$sleep_total, msleep$sleep_rem, col = "blue", pch=16) 

ggplot(data=msleep) + aes(x=sleep_total, y=sleep_rem)+
               geom_point(col='blue', pch=16)

#Nuge de points avec la moyenne et une grille
plot(msleep$sleep_total, msleep$sleep_rem, col='blue',pch = 16)
grid(col='green')
abline(h=mean(msleep$sleep_rem, na.rm=TRUE), col='red')
abline(v=mean(msleep$sleep_total, na.rm=TRUE), col='red')

#Nuge de points avec la moyenne et une grille
ggplot(data=msleep) + aes(x=sleep_total, y=sleep_rem )+
               geom_point(col= "darkgreen", size = 2,pch=16)+
               geom_vline(xintercept = mean(msleep$sleep_total, na.rm=TRUE), col='red')+
               geom_hline(yintercept = mean(msleep$sleep_rem, na.rm=TRUE), col='red')+
               theme_minimal()

## 1)  Créez un nuage de points avec le temps de sommeil total sur l'axe des x 
## et le temps d'éveil sur l'axe des y
ggplot(data=msleep) + aes(x=sleep_total, y=awake)+
               geom_point(col= "khaki", size = 1,pch=16)+
               theme_minimal()
## 2)  Créez un nuage de points avec la taille du cerveau  sur l'axe des x
## et le temps de sommeil totalsur l'axe des y
ggplot(data=msleep) + aes(x=brainwt, y=sleep_total)+
               geom_point(col= "darkgreen", size = 1,pch=16)+
               labs(X="Taille du cerveau", 
                    Y="Temps de sommeil total")+
               theme_minimal()
## 3)  Créez un nuage de points avec le temps de sommeil total sur l'axe des x
## et le temps de sommeil paradoxal sur l'axe des y
ggplot(data=msleep) + aes(x=sleep_total, y=sleep_rem)+
               geom_point(col= "darkgreen", size = 1,pch=16)+
               labs(X="Temps de sommeil total", 
                    Y="Temps de sommeil paradoxal")+
               theme_minimal()
## 4) Comparaison de groupes
ggplot(msleep) + aes(brainwt, sleep_total,col=vore) + 
     geom_point() +
     labs(x = "Brain weight (logarithmic scale)",
       y = "Total sleep time") +
     scale_x_log10() +
     facet_wrap(~ vore)
## 5) Comparaison de groupes
ggplot(msleep) + aes(brainwt, sleep_total,col=conservation) + 
     geom_point() +
     labs(x = "Brain weight (logarithmic scale)",
       y = "Total sleep time") +
     scale_x_log10() +
     facet_wrap(~ conservation)
## 6) Comparaison de groupes
ggplot(msleep) + aes(brainwt, sleep_total,col=conservation) + 
     geom_point() +
     labs(x = "Brain weight (logarithmic scale)",
       y = "Total sleep time") +
     scale_x_log10() +
     facet_wrap(~ conservation) +
     theme_classic()
#################### Boxplot ####################
boxplot(sleep_total ~ vore, data=msleep, col=hcl.colors(6)) ## Boxplot la libraire standard    
grid() ## Ajouter une grille

ggplot(data=msleep) + aes(x=vore, y=sleep_total,col=vore )+  ## la librairie GGPLOT2
               geom_boxplot(fill=rainbow(5))+
               theme_classic()

## 1) Créez un boxplot avec le temps de sommeil total en fonction du régime alimentaire
ggplot(data=msleep) + aes(x=vore, y=sleep_total,col=vore )+  ## la librairie GGPLOT2
               geom_boxplot(fill=rainbow(5))+
               theme_classic()
## 2) Créez un boxplot avec le temps de sommeil total en fonction de la conservation
ggplot(data=msleep) + aes(x=conservation, y=sleep_total,col=conservation )+  ## la librairie GGPLOT2
               geom_boxplot(fill='khaki')+
               theme_classic()
## 3) Créez un boxplot avec le temps de sommeil paradoxal en fonction du régime alimentaire
ggplot(data=msleep) + aes(x=vore, y=sleep_rem,col=vore )+  ## la librairie GGPLOT2
               geom_boxplot(fill=rainbow(5))+
               theme_classic()

################## Histogrammes ####################
hist(msleep$sleep_total, col='blue', breaks=10) ## Histogramme la libraire standard
grid() ## Ajouter une grille

ggplot(data=msleep) + aes(x=sleep_total)+  ## la librairie GGPLOT2
               geom_histogram(fill='blue',col='red', bins=10)+
               theme_minimal()

## 1) Créez un histogramme avec le temps de sommeil total
ggplot(data=msleep) + aes(x=sleep_total)+  ## la librairie GGPLOT2
               geom_histogram(fill='blue',col='red', bins=10)+
               theme_minimal()
## 2) Créez un histogramme avec le temps de sommeil paradoxal 
ggplot(data=msleep) + aes(x=sleep_rem)+  ## la librairie GGPLOT2
               geom_histogram(fill='blue',col='red', bins=10)+
               theme_minimal()
## 3) Créez un histogramme avec la taille du cerveau
ggplot(data=msleep) + aes(x=brainwt)+  ## la librairie GGPLOT2
               geom_histogram(fill='blue',col='red', bins=10)+
               theme_minimal()

################## Densité ####################
plot(density(msleep$sleep_total),lwd=2, col='blue') ## Densité la libraire standard
grid() ## Ajouter une grille

ggplot(data=msleep) + aes(x=sleep_total)+  ## la librairie GGPLOT2
               geom_density(fill='blue',col='red')+
               theme_minimal()

## 1) Créez un graphique de densité avec le temps de sommeil pardoxal
ggplot(data=msleep) + aes(x=sleep_rem)+  ## la librairie GGPLOT2
               geom_density(fill='blue',col='red')+
               theme_minimal()
## 2) Créez un graphique de densité avec le temps de sommeil total  
ggplot(data=msleep) + aes(x=sleep_total)+  ## la librairie GGPLOT2
               geom_density(fill='blue',col='red')+
               theme_minimal()
## 3) Créez un graphique de densité avec la taille du cerveau
ggplot(data=msleep) + aes(x=brainwt)+  ## la librairie GGPLOT2
               geom_density(fill='blue',col='red')+
               theme_minimal()

################## Graphiques à barres ####################
barplot(table(msleep$vore), col='green') ## Graphique à barres la libraire standard
grid() ## Ajouter une grille

ggplot(data=msleep) + aes(x=vore)+  ## la librairie GGPLOT2
               geom_bar(fill=rainbow(5))+
               theme_minimal()

## 1) Créez un graphique à barres avec le régime alimentaire
ggplot(data=msleep) + aes(x=vore)+  ## la librairie GGPLOT2
               geom_bar(fill=rainbow(5))+
               theme_minimal()
## 2) Créez un graphique à barres avec la conservation
ggplot(data=msleep) + aes(x=conservation)+  ## la librairie GGPLOT2
               geom_bar(fill=hcl.colors(7))+
               theme_minimal()
## 3) Créez un graphique à barres avec la durée du sommeil total
ggplot(data=msleep) + aes(x=sleep_total)+  ## la librairie GGPLOT2
               geom_bar(fill='blue')+
               theme_minimal()

################## Graphiques à secteurs ####################
pie(table(msleep$vore), col=rainbow(5)) ## Graphique à secteurs la libraire standard
grid() ## Ajouter une grille








