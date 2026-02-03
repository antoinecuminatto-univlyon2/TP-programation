install.packages()

#EXERCICE 1
getwd()
setwd("L:/BUT/SD/Promo 2025/acuminatto/R/TD2/dataset")
bodies_karts = read.csv("L:/BUT/SD/Promo 2025/acuminatto/R/TD2/dataset/bodies_karts.csv", header = TRUE, dec = ",", sep = ";")
drivers = read.csv("L:/BUT/SD/Promo 2025/acuminatto/R/TD2/dataset/drivers.csv", header = TRUE, dec = ",", sep = ";")
gliders = read.csv("L:/BUT/SD/Promo 2025/acuminatto/R/TD2/dataset/gliders.csv", header = TRUE, dec = ",", sep = "|")
tires = read.csv("L:/BUT/SD/Promo 2025/acuminatto/R/TD2/dataset/tires.csv", header = TRUE, dec = ",", sep = "\t")
View(tires)
dim(bodies_karts)
dim(tires)
dim(gliders)
dim(drivers)

#EXERCICE 2
summary(bodies_karts)
summary(tires)
summary(gliders)
summary(drivers)

plot(x = drivers$Weight,
     y = drivers$Acceleration, 
     main = "Drivers : Weight / Acceleration")
cor(drivers$Weight, drivers$Acceleration)

Cova = cov(drivers$Weight, drivers$Acceleration)
ox = sd(drivers$Weight, FALSE)
oy = sd(drivers$Acceleration, FALSE)
corr = Cova / (ox * oy)
corr

determin = corr^2
determin
#6
matrciedrive = cor(drivers[ , -1]) #-1 car on veut toute les colonnes sauf la 1ere
matrciedrive = round(matrciedrive , 2)
View(matrciedrive)


#7
install.packages("corrplot" )
library(corrplot)
corrplot(matrciedrive, method="circle")
#8
matrciebodies = cor(bodies_karts[ , -1]) 
matrciebodies = round( matrciebodies, 2)
corrplot(matrciebodies, method="circle")

matrciegliders =  cor(gliders[ , -1]) 
matrciegliders = round(matrciegliders , 2)
#impossible de faire gliders

matrcietires =  cor(tires[ , -1]) 
matrcietires = round(matrcietires , 2)
corrplot(matrcietires, method="circle")


#EXERCICE 3
#1
resultat = drivers[ , c("Driver" , "Weight")]
View(resultat)
#2
resultat2 = drivers[ 1:10, c("Driver" , "Acceleration")]
View(resultat2)
#3
resultat3 = drivers[ , -c(2, 3)]
View(resultat3)
#4
resultat4 = drivers[ , c("Driver" , "Acceleration", "Weight")]
View(resultat4)
#5
resultat5 = drivers[ c(3,12,32) , ]
View(resultat5)
#6
resultat6 = drivers[ c(32,3,12) , ]
View(resultat6)
#8
resultat8 = drivers[ order(drivers$Weight), c("Driver" , "Weight")]
View(resultat8)
#9

resultat9 = drivers[ order(drivers$Acceleration, drivers$Weight, decreasing = c(TRUE, FALSE)), c("Driver" , "Acceleration", "Weight")]
View(resultat9)

#EXERCICE 4
topDriver = subset(x = drivers, # l'objer ou on va enlever ce que l'on veut pas
                   subset = Acceleration == max(Acceleration), #La condition a vérifier, ici l'acceleration 
                   select = c("Driver","Acceleration")) #ce qu'on affiche, donc les drivers avec leurs accélération que si il ont le max
View(topDriver)

topglider = subset(x = gliders, # l'objer ou on va enlever ce que l'on veut pas
                   subset = Acceleration == max(Acceleration), #La condition a vérifier, ici l'acceleration 
                   select = c("Glider","Acceleration")) #ce qu'on affiche, donc les drivers avec leurs accélération que si il ont le max
View(topglider)

toptires = subset(x = tires, # l'objer ou on va enlever ce que l'on veut pas
                   subset = Acceleration == max(Acceleration), #La condition a vérifier, ici l'acceleration 
                   select = c("Tire","Acceleration")) #ce qu'on affiche, donc les drivers avec leurs accélération que si il ont le max
View(toptires)

topBody = subset(x = bodies_karts,
                 subset = Acceleration == max(Acceleration), 
                 select = c("Body","Acceleration"))
View(topBody)