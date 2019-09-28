---
  title: "Visualisation de donnees"
author: "MBALLO Aboubacry"
date: " aoÃfÂ»t 2019"
output:
  word_document: default
html_document: default
pdf_document:
  keep_tex: yes
---
  ```{r, message=FALSE, warning=FALSE}
library(readr)
library(ggplot2)
library(scatterplot3d)
library(rgl)
library(caret)
library(h2o)
library(xgboost)
library(xlsx)
library(tidyverse)
library(tidyr)
library(dplyr)
```

```{r,include=FALSE}
options(tinytex.verbose = TRUE)
```

donnees <- read.xlsx("C:/Users/hp/Desktop/edacyProjets/faketalents.xlsx", sheetIndex = 1)
donnees <- data.frame(donnees)
str(donnees)
print(head(donnees))
donnees2<-na.omit(donnees)
summary(donnees2)
str(donnees2)

# change type of columns
donnees2$Formation <- factor(donnees2$Formation)
donnees2Etablissement <- factor(donnees2$Etablissement)
donnees2$Statut <- factor(donnees2$Statut)
donnees2$Sexe <- factor(donnees2$Sexe)
donnees2$statut_professionnel <- factor(donnees2$statut_professionnel)
donnees2$acquis <- factor(donnees2$acquis)
donnees2$dernier_diplome <- factor(donnees2$dernier_diplome)

sum(is.na(donnees2))

dim(donnees2)
table(donnees2$Etablissement)
table(donnees2$acquis)
table(donnees2$statut_professionnel)

## Exploration des donnees


# target
ggplot(data = donnees2) +
  geom_histogram(mapping = aes(x = freq_connexion), binwidth = 1) +
  labs(title = "Distribution de la freq de connexion des talents", x = "freq_connexion")

summary(donnees2$freq_connexion)
sd(donnees2$freq_connexion)

#predictors 
ggplot(data = donnees2) +
  geom_boxplot(mapping = aes(x = Sexe, y = acquis)) +
  coord_flip()

ggplot(data = lycees_bac_data) +
  geom_boxplot(mapping = aes(x = Secteur, y = taux_reussite_bac)) +
  labs(title = "Statut de l'?tablissement et taux de r?ussite", x = "Statut public ou priv?", 
       y = "taux de r?ussite")

ggplot(data = lycees_bac_data) +
  geom_boxplot(mapping = aes(x = Structure.Pedagogique, y = taux_reussite_bac, fill = Secteur)) +
  labs(title = "Offre de formation de l'?tablissement, statut et taux de r?ussite",
       x = "offre de formation", y = "taux de r?ussite")

ggplot(data = lycees_bac_data) +
  geom_boxplot(mapping = aes(x = Secteur, y = taux_reussite_bac, fill = Structure.Pedagogique)) +
  labs(title = "Offre de formation de l'?tablissement, statut et taux de r?ussite",
       x = "Statut public ou priv?", y = "taux de r?ussite")

ggplot(data = lycees_bac_data) +
  geom_violin(mapping = aes(x = Secteur, y = taux_reussite_bac, fill = Structure.Pedagogique)) +
  labs(title = "Distribution des variables Statut et Offre de formation")

ggplot(data = lycees_bac_data) +
  geom_point(mapping = aes(x = nombre_presents_bac, y = taux_reussite_bac, color = Secteur)) +
  geom_smooth(mapping = aes(x = nombre_presents_bac, y = taux_reussite_bac)) +
  labs(title = "Nombre d'?l?ves pr?sents au bac, statut et taux de r?ussite",
       x = "Nombre d'?l?ves pr?sents au bac", y = "taux de r?ussite")

ggplot(data = lycees_bac_data) +
  geom_point(mapping = aes(x = effectifs_rentree, y = taux_reussite_bac, color = Secteur)) +
  geom_smooth(mapping = aes(x = effectifs_rentree, y = taux_reussite_bac)) +
  labs(title = "Effectifs en terminale, statut et taux de r?ussite",
       x = "Effectifs en terminale", y = "taux de r?ussite")

ggplot(data = lycees_bac_data) +
  geom_point(mapping = aes(x = nombre_presents_bac, y = taux_reussite_bac, color = Structure.Pedagogique)) +
  geom_smooth(mapping = aes(x = nombre_presents_bac, y = taux_reussite_bac)) +
  labs(title = "Nombre d'?l?ves pr?sents au bac, offre de formation et taux de r?ussite",
       x = "Nombre d'?l?ves pr?sents au bac", y = "taux de r?ussite")

ggplot(data = lycees_bac_data) +
  geom_point(mapping = aes(x = effectifs_rentree, y = taux_reussite_bac, color = Structure.Pedagogique)) +
  geom_smooth(mapping = aes(x = effectifs_rentree, y = taux_reussite_bac)) +
  labs(title = "Effectifs en terminale, offre de formation et taux de r?ussite",
       x = "Effectifs en terminale", y = "taux de r?ussite")




















 donnees2$acquis <- factor(donnees2$acquis)

boxplot(donnees2$Sexe,donnees2$moyenne,donnees2$Etablissement,donnees2$age)

#checking outliers
ggplot(donnees2, aes(x=tauxPart, y=acquis, color=Sexe)) + geom_point() 
###Average length & width by universite
temp_df <- donnees2 %>% group_by(Etablissement) %>% summarize(mean(age),mean(moyenne),mean(tauxPart),mean(visiPlat))
kable(temp_df,align = 'c',col.names = c('Etablissement','Avg age','Avg moyenne','Avg tauxPart','Avg visitPlats')) 
temp_df


plot(donnees2$moyenne,donnees2$acquis)
ggplot(donnees2, aes(x=moyenne, y=acquis, color=Sexe)) + geom_point() + labs(title="Scatterplot", x="moyenne", y="acquis
")




#change type of columns

donnees$acquis <- factor(donnees$acquis)
donnees$statut_professionnel <- factor(donnees$statut_professionnel) 
#donnees$statut<- factor(donnees$statut) 
donnees$Formation <- factor(donnees$Formation) 
donneessdernier_diplome <- factor(donnees$dernier_diplome) 
donnees$acquis <- factor(donnees$acquis) 


summary(donnees)
sum(is.na(donnees)) # no NAs

###Scatter plot moyenne & visitPlat:
ggplot(donnees, aes(x=moyenne, y=acquis, color=Sexe)) + geom_point() + labs(title="Scatterplot", x="moyenne", y="acquis")

#histogramme de distribution du taux d'acquisition de talents 
#ggplot(data = donnees) +
  #geom_histogram(mapping = aes(x = acquis), binwidth = 1) +
  #labs(title = "Distribution du taux d'acquisition de talents", x = "acquis")

###Plotting all numeric features

ggpairs(data=donnees, rows= 1:50, columns=14:19,title="Talents caract Features",colour='Sexe')

###Including Density in plots

ggpairs(data=donnees,
        columns=16:19, 
        upper = list(continuous = "density"),
        lower = list(combo = "facetdensity"),
        colour = "acquis")

#Trying pairs to get distinct colours for each cluster
pairs(donnees[,16:19], col=donnees$acquis) 


### algorithme de prediction: classification avec arbres de decision 
### pour predire le taux d'acquisition des nouveaux talents

#chargement des packages de tidyverse et du jeu de donnees

library(tidyverse)
donnees<- data.frame(donnees)
donnees
#fractionement de donnees avec une echelle de 0.8 et training data
#creation de jeu test

## Prediction univariée: avec la variable tauxPart

train <- donnees %>% sample_frac(0.8) 
test <- anti_join(donnees, train)
# creation du modele de classification avec rpart et le jeu de donnees test
model <- rpart(acquis ~ tauxPart, data = test, method = "class")

#utilisation de la matrice de confusion pour estimer la qualité du modele

###La sensibilité: est la capacité d'un modèle à prédire un positif 
#quand la donnée est réellement positive, 
#et la spécificité: sa capacité inverse, celle de prédire un négatif lorsqu'il y a vraiment un négatif. 


# on commence à predire le taux d'acquisit avec tauxPart
#ensuite on poursuit avec visitPlat
#et enfin avec la frequence de connexion


library(rpart)
library(caret)
tree <- rpart(acquis ~ tauxPart, data = test, method = "class") 
test$prediction <- predict(tree, train, type = "class")
conf <- confusionMatrix(data = test$prediction, reference = test$acquis)
Sensitivity<-conf$byClass["Sensitivity"]
Sensitivity
Specificity <-conf$byClass["Specificity"]
Specificity 

#prediction avec visitPlat

tree <- rpart(acquis ~ visiPlat, data = test, method = "class") 
test$prediction <- predict(tree, train, type = "class")
conf <- confusionMatrix(data = test$prediction, reference = test$acquis)
Sensitivity<-conf$byClass["Sensitivity"]
Sensitivity
Specificity <-conf$byClass["Specificity"]
Specificity 

#prediction avec freq_connexion

tree <- rpart(acquis ~ freq_connexion, data = test, method = "class") 
test$prediction <- predict(tree, train, type = "class")
conf <- confusionMatrix(data = test$prediction, reference = test$acquis)
Sensitivity<-conf$byClass["Sensitivity"]
Sensitivity
Specificity <-conf$byClass["Specificity"]
Specificity 

###modele multivarié avec les trois vars tauxPar visitPlat et freq_connex

tree <- rpart(acquis ~ tauxPart + visiPlat + freq_connexion, data = test, method = "class") 
test$prediction <- predict(tree, train, type = "class")
conf <- confusionMatrix(data = test$prediction, reference = test$acquis)
Sensitivity<-conf$byClass["Sensitivity"]
Sensitivity
Specificity <-conf$byClass["Specificity"]
Specificity 
 






