###################################################
## Guatemala, julio del 2020
## Data Science 
## Alejandro Tejada 17584
## Diego Sevilla 17238
## Jose Cifuentes CARNET
## Oscar Juarez CARNET
## Jesus Belches
## Proyecto No1: Limpieza de datos
## Catedrático: Lynette García
###################################################



##-----------------ZONA LIBRERIAS---------------------------
library(dplyr)
library(rpart)
library(tree)
library(rpart.plot)
library(dummies)
library(nortest)
library(caret)
library(corrplot)
library(e1071)
library(nnet)
library(RWeka)
library(neural)
library(dummy)
library(neuralnet)
library(stringi)
##------------------FIN ZONA LIBRERIAS------------------

getwd()
setwd("C:/Users/josea/Desktop/Universidad/2020/DataScience/Proyecto1/DataScienceProject1/CSV")

AVerapaz = read.csv("altaVerapaz.csv",stringsAsFactors = FALSE, na.strings = TRUE, strip.white = TRUE,sep = ";", encoding="UTF-8" )
BVerapaz = read.csv("bajaVerapaz.csv",stringsAsFactors = FALSE, na.strings = TRUE, strip.white = TRUE,sep = ";" , encoding="UTF-8" )
Chima = read.csv("chimaltenango.csv",stringsAsFactors = FALSE, na.strings = TRUE, strip.white = TRUE,sep = ";", encoding="UTF-8"  )
Chiqui = read.csv("chiquimula.csv",stringsAsFactors = FALSE, na.strings = TRUE, strip.white = TRUE,sep = ";" , encoding="UTF-8" )
Progreso = read.csv("elProgreso.csv",stringsAsFactors = FALSE, na.strings = TRUE, strip.white = TRUE,sep = ";" , encoding="UTF-8" )
Escuintla = read.csv("escuintla.csv",stringsAsFactors = FALSE, na.strings = TRUE, strip.white = TRUE,sep = ";" , encoding="UTF-8" )
Guatemala = read.csv("guatemala.csv",stringsAsFactors = FALSE, na.strings = TRUE, strip.white = TRUE,sep = ";" , encoding="UTF-8" )
Huehue = read.csv("huehue.csv",stringsAsFactors = FALSE, na.strings = TRUE, strip.white = TRUE,sep = ";" ,encoding="UTF-8")
Izabal = read.csv("izabal.csv",stringsAsFactors = FALSE, na.strings = TRUE, strip.white = TRUE,sep = ";" , encoding="UTF-8")
Jalapa = read.csv("jalapa.csv",stringsAsFactors = FALSE, na.strings = TRUE, strip.white = TRUE,sep = ";" , encoding="UTF-8" )
Jutiapa = read.csv("jutiapa.csv",stringsAsFactors = FALSE, na.strings = TRUE, strip.white = TRUE,sep = ";" ,encoding="UTF-8" )
Peten = read.csv("peten.csv",stringsAsFactors = FALSE, na.strings = TRUE, strip.white = TRUE,sep = ";" , encoding="UTF-8" )
Quetza = read.csv("quetzaltenango.csv",stringsAsFactors = FALSE, na.strings = TRUE, strip.white = TRUE,sep = ";" , encoding="UTF-8" )
Quiche = read.csv("quiche.csv",stringsAsFactors = FALSE, na.strings = TRUE, strip.white = TRUE,sep = ";" , encoding="UTF-8" )
Reta = read.csv("retalhuleu.csv",stringsAsFactors = FALSE, na.strings = TRUE, strip.white = TRUE,sep = ";" , encoding="UTF-8" )
Sacate = read.csv("sacatepequez.csv",stringsAsFactors = FALSE, na.strings = TRUE, strip.white = TRUE,sep = ";" , encoding="UTF-8" )
SMarcos = read.csv("sanMarcos.csv",stringsAsFactors = FALSE, na.strings = TRUE, strip.white = TRUE,sep = ";" , encoding="UTF-8" )
SRosa= read.csv("santaRosa.csv",stringsAsFactors = FALSE, na.strings = TRUE, strip.white = TRUE,sep = ";" , encoding="UTF-8" )
Solola = read.csv("solola.csv",stringsAsFactors = FALSE, na.strings = TRUE, strip.white = TRUE,sep = ";" , encoding="UTF-8" )
Suchi = read.csv("suchitepequez.csv",stringsAsFactors = FALSE, na.strings = TRUE, strip.white = TRUE,sep = ";", encoding="UTF-8")
Toto = read.csv("totonicapan.csv",stringsAsFactors = FALSE, na.strings = TRUE, strip.white = TRUE,sep = ";",encoding="UTF-8")
Zacapa = read.csv("zacapa.csv",stringsAsFactors = FALSE, na.strings = TRUE, strip.white = TRUE,sep = ";" , encoding="UTF-8" )

#Unimos toda la data
FULLDATASET <- rbind(AVerapaz, BVerapaz,Chima,Chiqui,Progreso,Escuintla,Guatemala,Huehue,Izabal,Jalapa,Jutiapa,Peten,
                     Quetza,Quiche,Reta,Sacate,SMarcos,SRosa,Solola,Suchi,Toto, Zacapa )

Encoding(FULLDATASET$ESTABLECIMIENTO)
unique(FULLDATASET$DEPARTAMENTO)


str(FULLDATASET)
str(FULLDATASET)


