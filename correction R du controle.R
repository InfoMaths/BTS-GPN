##Exercice 1: Loi Binomiale
## Soit X une variable aléatoire suivant une loi binomiale 
## de paramètre  n= 5 et p=0.4
#1) Expliquer que veut dire cette hypothèse

#2) Calculer  P(X=1),   P(X<=3)     et P(X>4)
### calcul de P(X=1)
dbinom(1,5,0.4) =0.2592
##P(X=1)=0.2592

### calcul de P(X<=3)=P(X=0)+P(X=1)+P(X=2)+P(X=3)
sum(dbinom(0:3,5,0.4)) ## ou
pbinom(3,5,0.4)
## P(X<=3)=0.91296
### calcul de P(X>4)=1-P(X<=4)
sum(dbinom(5,5,0.4))
## P(X>4)=0.01024

#) Tracer la fonction de masse de probabilité de X 
x=0:5
y=dbinom(x,5,0.4)
plot(x,y,type="h",col="blue",lwd=2)

###3) Calculer l'espérance et la variance de X
## Espérance de X
n=5
p=0.4
#E(X)=n*p=5*0.4=2
##Variance de X
#Var(X)=n*p*(1-p)=5*0.4*0.6=1.2

#4) 4) Si X suit une loi binomiale B(2, 6) ,  
# avec P(X=0)=16/25   alors donner la valeur de p 
#P(X=0)=(1-p)^2=16/25 et donc p=1-sqrt(16/25)=1/5

##Exercice 2: Loi Normale
## On suppose que la hauteur des arbres dans une forêt est modélisée 
## par une variable aléatoire  X suivant une loi normale 
## de moyenne m=10 mètres  et d’écart-type  s=2 mètres
#1) Calculer P(X<=11)
pnorm(11,10,2)
## P(X<=11)=0.6914625

## 2) Calculer P(X>9)
1-pnorm(9,10,2)
## P(X>9)=0.6914625

## 3) Calculer P(8<=X<=12)
pnorm(12,10,2)-pnorm(8,10,2)
## P(8<=X<=12)=0.3829249
##) graphique de la loi normale et l'aire calculée
x=seq(5,15,0.01)
y=dnorm(x,10,2)
plot(x,y,type="l",col="blue",lwd=2)
x1=seq(8,12,0.01)
y1=dnorm(x1,10,2)
polygon(c(8,x1,12),c(0,y1,0),col="skyblue")

## 4) Calculer le quantile d'ordre 0.975 de X P(X<=a)=0.975
qnorm(0.975,10,2)
## quantile d'ordre 0.975 de a = 13.9193

## 5) Calculer le quantile d'ordre 0.0.025 de X P(X<=b)=0.025
qnorm(0.025,10,2)
## quantile d'ordre 0.025 de b = 6.080072

##6)  Calculer la P(a<=X<=b)
pnorm(13.9193,10,2)-pnorm(6.080072,10,2)
## P(6.080072<=X<=13.9193)=0.95

##Exercice 3 Estmation et intervalles de confiance
##Un collectionneur de papillons a prélevé 12 cocons d’une variété de papillons
## et les a pesés. Il obtient, sur cet échantillon de 12 cocons
## les mesures suivantes en grammes
mesures <- c(1.0, 0.5, 0.6, 0.9, 0.5 , 0.9, 0.8, 0.6 ,0.6, 0.5, 0.6, 0.5)
##1) Donner uen estimation ponctuelle la moyenne de la masse des cocons
mean(mesures)
## estimation ponctuelle de la moyenne = 0.66 grammes

##2) Donner une estimation ponctuelle de la variance de la masse des cocons
var(mesures)
## estimation ponctuelle de la variance = 0.033 grammes

##3) Donner un intervalle de confiance de niveau 95% de la moyenne de la masse des cocons

## Intervalle de confiance de la moyenne
alpha=0.05
n=length(mesures)
moy=mean(mesures)
sigma=0.5
IC_inf=moy-qnorm(1-alpha/2)*sigma/sqrt(n)
IC_sup=moy+qnorm(1-alpha/2)*sigma/sqrt(n)
IC_inf
## IC_inf=0.3837
IC_sup
## IC_sup=0.9495

## l'intervaale de confiance de la moyenne de la masse des cocons est [0.3837,0.9495]


