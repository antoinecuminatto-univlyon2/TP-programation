# 1 installer le library
library(readxl)
pokemon = readxl::read_excel(path = "L:/BUT/SD/Promo 2025/acuminatto/R/TD3/pokemon.xlsx", 
                   sheet = "pokemon")
#2 info essentiel
dim(pokemon) #enssemble
ncol(pokemon)#colonne
nrow(pokemon)#ligne

#3 résumer des données
summary(pokemon) #remarque :  En R, il est important que les variables qualitatives soient de type factor.

#4 transformer en type factor
#chaque ligne correspond à une variable qualitative
pokemon$is_legendary <-as.factor(pokemon$is_legendary)
pokemon$generation <-as.factor(pokemon$generation)
pokemon$type <-as.factor(pokemon$type)

#5 nouveau résumer
summary(pokemon)

#EXERCICE 2
#1 crer une nouvelle colonne en fonction d'une déja existante
mediane = median(pokemon$attack)
pokemon$attack_group = ifelse(pokemon$attack >= mediane, "attack+","attack-")
pokemon$attack_group <-as.factor(pokemon$attack_group)
summary(pokemon$attack_group)

#2 nouvelle colonne mais avec condition "ou" 
pokemon$water_fire = ifelse(pokemon$type %in% c("water","fire"), "yes","no")
pokemon$water_fire <-as.factor(pokemon$water_fire)
summary(pokemon$water_fire)

#3 colonne avec plusieur condition à respecter
q1 = quantile(pokemon$attack, probs = 0.75)
q2 = quantile(pokemon$defense, probs = 0.75)
q3 = quantile(pokemon$speed, probs = 0.75)
pokemon$best = ifelse(pokemon$attack > q1 & pokemon$defense > q2 & pokemon$speed > q3, "yes", "no")
pokemon$best <-as.factor(pokemon$best)
summary(pokemon$best)

#4 la fonction "is.na" pour voir les choses sans valeurs
requete = subset(pokemon, is.na(weight_kg))
View(requete)
#5 inverse, voir ce qui n'ont pas rien
requete = subset(pokemon, !is.na(weight_kg))
View(requete)

#6 PS variable et objet c'est la même chose
med_weight = median(pokemon$weight_kg, na.rm = TRUE)
pokemon$weight_kgNa = ifelse(is.na(pokemon$weight_kg), med_weight, pokemon$weight_kg)
med_height = median(pokemon$height_m,  na.rm = TRUE)
pokemon$height_mNA = ifelse(is.na(pokemon$height_m), med_height, pokemon$height_m)

#7 la fonction "cut"
pokemon$weight_group = cut(pokemon$weight_kg, breaks = 3, labels = c("léger", "moyen", "lourd"))

#8 tranches au debut puis plus a la fin 
pokemon$height_m_group = cut(pokemon$height_m, breaks = c(0,1,2,3, max(pokemon$height_m, na.rm = TRUE)))

#9 tranches avec les quartiles
pokemon$defense_group = cut(pokemon$defense,
                            breaks = quantile(pokemon$defense,na.rm = TRUE),
                            include.lowest = TRUE)
summary(pokemon$defense_group)



#EXERCICE 3 
#1 moyenne d'agrégation, de classe, etc, ici : attack par type
aggregate(x = attack ~ type, 
          data = pokemon,
          FUN = function(x) mean(x))

#2 médiane avec plusieur regroupement
aggregate(x = attack ~ generation + type, 
          data = pokemon,
          FUN = function(x) median(x))

#3 effectif 
aggregate(x = pokedex_number ~ type,
          data = pokemon,
          FUN = function(x) length(x))

#4 plusieur calcul dans la même aggrégation
aggregate(x = speed ~ generation + type,
          data = pokemon,
          FUN = function(x) c(moy = mean(x),
                              med = median(x),
                              effectif = length(x)
                              ))
          