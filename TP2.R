#EXERCICE 1
fao = read.csv(file = "C:/Users/acuminatto/Downloads/fao.csv", header = TRUE, sep = ";", dec = ",")
nrow(fao)
summary(fao)
#EXERCICE 2
#1
mean(fao$Dispo_alim)
#2
sum(fao$Population, na.rm=TRUE) #na.rm permet de sauter les valeurs null
#3
sd(fao$Export_viande, na.rm = TRUE)
sd(fao$Import_viande, na.rm = TRUE)
#4
median(fao$Prod_viande, na.rm = TRUE)
#5
quantile(fao$Dispo_alim)
#6
quantile(fao$Import_viande, seq(0,1,0.01))
#EXERCICE 3
#1
order(fao$Population, decreasing = FALSE)
fao1 =fao[ order(fao$Population, decreasing = FALSE), ]
fao1 = fao1[1:5, ]
View(fao1)
#2
order(fao$Population, decreasing = TRUE)
fao1 =fao[ order(fao$Population, decreasing = TRUE), ]
fao1 = fao1[1:5, ]
View(fao1)
#3  
order(fao$Prod_viande, decreasing = TRUE)
fao2 =fao[ order(fao$Prod_viande, decreasing = TRUE), ]
fao2 = fao2[1:5, ]
View(fao2)
#4
order(fao$Import_viande, decreasing = TRUE)
fao3 =fao[ order(fao$Import_viande, decreasing = TRUE), ]
fao3 = fao3[1:5, ]
View(fao3)
#5
resultat = subset(fao, fao$Dispo_alim>=2300)
View(resultat)
#6
resultat = subset(fao, fao$Dispo_alim > 3500  & Import_viande > 1000)
View(resultat)
#7
resultat = subset(fao, fao$Nom %in% c("France","Belgique"))
View(resultat)
# EXERCICE 4
#1
fao$Part_export<-fao$Export_viande/fao$Prod_viande
#2
fao$dispo_alim_pays = fao$Dispo_alim*fao$Population
#3
write.table(x = fao, file = "ExportTp2.csv")
#4
dispo_alim_mondiale = sum(fao$dispo_alim_pays, na.rm=TRUE)
dispo_alim_mondiale
#5
dispo_alim_mondiale/2300

#EXERCICE 5
#1
plot(x = fao$Prod_viande,
     y = fao$Export_viande, 
     main = "Pays : Prod_viande / Export_viande")
#2
cor(x = fao$Prod_viande,
    y = fao$Export_viande)
#3
matriceCor = cor(fao[ , - 1] , use = "complete.obs")
matriceCor = round(matriceCor , 2)
View(matriceCor)
#4
install.packages("corrplot")
#5
library(corrplot) #je charge mon package pour pouvoir utiliser ses fonctionalités
corrplot(matriceCor, method="circle")