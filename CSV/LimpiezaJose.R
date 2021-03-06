###################################################
## Guatemala, julio del 2020
## Data Science 
## Alejandro Tejada 17584
## Diego Sevilla 17238
## Jose Cifuentes CARNET
## Oscar Juarez CARNET
## Jesus Belches
## Proyecto No1: Limpieza de datos
## Catedr�tico: Lynette Garc�a
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
##setwd("C:/Users/Diego Sevilla/Documents/UVG Semestres/Repositorios/8vo Semestre/Data science/DataScienceProject1/CSV")

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


## Revisamos que esten los departamentos
unique(FULLDATASET$DEPARTAMENTO)

##Quitamos las tildes
FULLDATASET$ESTABLECIMIENTO<-stri_trans_general(FULLDATASET$ESTABLECIMIENTO,"Latin-ASCII")
FULLDATASET$MODALIDAD<-stri_trans_general(FULLDATASET$MODALIDAD,"Latin-ASCII")
FULLDATASET$DIRECTOR<-stri_trans_general(FULLDATASET$DIRECTOR,"Latin-ASCII")
FULLDATASET$DIRECCION<-stri_trans_general(FULLDATASET$DIRECCION,"Latin-ASCII")


##Se reemplazan espacios en blanco por NA en cada columna
FULLDATASET$TELEFONO[FULLDATASET$TELEFONO==""] <- NA
FULLDATASET$DISTRITO[FULLDATASET$DISTRITO==""] <- NA
FULLDATASET$DEPARTAMENTO[FULLDATASET$DEPARTAMENTO==""] <- NA
FULLDATASET$MUNICIPIO[FULLDATASET$MUNICIPIO==""] <- NA
FULLDATASET$ESTABLECIMIENTO[FULLDATASET$ESTABLECIMIENTO==""] <- NA
FULLDATASET$DIRECCION[FULLDATASET$DIRECCION==""] <- NA
FULLDATASET$DIRECTOR[FULLDATASET$DIRECTOR==""] <- NA
FULLDATASET$SUPERVISOR[FULLDATASET$SUPERVISOR==""] <- NA
FULLDATASET$NIVEL[FULLDATASET$NIVEL==""] <- NA
FULLDATASET$AREA[FULLDATASET$AREA==""] <- NA
FULLDATASET$STATUS[FULLDATASET$STATUS==""] <- NA
FULLDATASET$MODALIDAD[FULLDATASET$MODALIDAD==""] <- NA
FULLDATASET$JORNADA[FULLDATASET$JORNADA==""] <- NA
FULLDATASET$PLAN[FULLDATASET$PLAN==""] <- NA
FULLDATASET$DEPARTAMENTAL[FULLDATASET$DEPARTAMENTAL==""] <- NA
FULLDATASET$SUPERVISOR[FULLDATASET$SUPERVISOR==""] <- NA

##Se reemplazan "-" o "." etc por NA en direccion
FULLDATASET$DIRECCION[FULLDATASET$DIRECCION=="-"] <- NA
FULLDATASET$DIRECCION[FULLDATASET$DIRECCION=="--"] <- NA
FULLDATASET$DIRECCION[FULLDATASET$DIRECCION=="---"] <- NA
FULLDATASET$DIRECCION[FULLDATASET$DIRECCION=="."] <- NA

##Se reemplazan "-" o "0" etc por NA en telefono
FULLDATASET$TELEFONO[FULLDATASET$TELEFONO=="-"] <- NA
FULLDATASET$TELEFONO[FULLDATASET$TELEFONO=="0"] <- NA

##Se reemplazan "-" o "0" etc por NA en supervisor
FULLDATASET$SUPERVISOR[FULLDATASET$SUPERVISOR=="------------------------ ---------------------------"] <- NA

##Se reemplazan "-" o "0" etc por NA en director
FULLDATASET$DIRECTOR[FULLDATASET$DIRECTOR=="0"] <- NA
FULLDATASET$DIRECTOR[FULLDATASET$DIRECTOR==","] <- NA
FULLDATASET$DIRECTOR[FULLDATASET$DIRECTOR=="."] <- NA
FULLDATASET$DIRECTOR[FULLDATASET$DIRECTOR==".."] <- NA
FULLDATASET$DIRECTOR[FULLDATASET$DIRECTOR==".--"] <- NA
FULLDATASET$DIRECTOR[FULLDATASET$DIRECTOR=="-"] <- NA
FULLDATASET$DIRECTOR[FULLDATASET$DIRECTOR=="--"] <- NA
FULLDATASET$DIRECTOR[FULLDATASET$DIRECTOR=="---"] <- NA
FULLDATASET$DIRECTOR[FULLDATASET$DIRECTOR=="----"] <- NA
FULLDATASET$DIRECTOR[FULLDATASET$DIRECTOR=="-----"] <- NA
FULLDATASET$DIRECTOR[FULLDATASET$DIRECTOR=="------"] <- NA
FULLDATASET$DIRECTOR[FULLDATASET$DIRECTOR=="-------"] <- NA
FULLDATASET$DIRECTOR[FULLDATASET$DIRECTOR=="--------"] <- NA
FULLDATASET$DIRECTOR[FULLDATASET$DIRECTOR=="---------"] <- NA
FULLDATASET$DIRECTOR[FULLDATASET$DIRECTOR=="----------"] <- NA

	

	

View(FULLDATASET)
##Se cuentan NA en cada columna
sum(is.na(FULLDATASET$TELEFONO))        #1780
sum(is.na(FULLDATASET$DISTRITO))        #323
sum(is.na(FULLDATASET$DEPARTAMENTO))
sum(is.na(FULLDATASET$MUNICIPIO))
sum(is.na(FULLDATASET$ESTABLECIMIENTO)) #1
sum(is.na(FULLDATASET$DIRECCION))       #94
sum(is.na(FULLDATASET$DIRECTOR))        #3383
sum(is.na(FULLDATASET$SUPERVISOR))      #325
sum(is.na(FULLDATASET$NIVEL)) 
sum(is.na(FULLDATASET$AREA)) 
sum(is.na(FULLDATASET$STATUS))
sum(is.na(FULLDATASET$MODALIDAD))
sum(is.na(FULLDATASET$JORNADA))
sum(is.na(FULLDATASET$PLAN))
sum(is.na(FULLDATASET$DEPARTAMENTAL))
sum(is.na(FULLDATASET$SUPERVISOR))      #325


#EMPIEZa LIMPIEZA JOSe
View(FULLDATASET$DEPARTAMENTO)
JOSE<-FULLDATASET[which(FULLDATASET$DEPARTAMENTO == "BAJA VERAPAZ" 
                       | FULLDATASET$DEPARTAMENTO == "SAN MARCOS"
                       | FULLDATASET$DEPARTAMENTO == "QUETZALTENANGO"
                       | FULLDATASET$DEPARTAMENTO == "OTONICAPAN"
                       | FULLDATASET$DEPARTAMENTO == "SOLOLA"),]

View(JOSE)
#Con esto identificamos los nombres menores a x longitud
for (variable in JOSE) {
  if(nchar(variable)<14)
  {
    print(variable)
  }
}

#Con esto revisamos la primera palabra del nombre
lista<-c();
for (variable in JOSE) {
  lista<-c(lista,substr(variable, 0, regexpr(' ', variable)))
}

lista2<-as.data.frame(lista)
dplyr::distinct(lista2)


FULLDATASET$ESTABLECIMIENTO[FULLDATASET$ESTABLECIMIENTO == "CPUM LICEO 'SAN LUIS'"] <- "COLEGIO PRIVADO URBANO MIXTO LICEO 'SAN LUIS'"
FULLDATASET$ESTABLECIMIENTO[FULLDATASET$ESTABLECIMIENTO == "CPMI DE CIENCIAS COMERCIALES 'EL ADELANTO'"] <- "COLEGIO PARTICULAR MIXTO DE CIENCIAS COMERCIALES 'EL ADELANTO'"
FULLDATASET$ESTABLECIMIENTO[FULLDATASET$ESTABLECIMIENTO == "ENBI OXLAJUJ NO�OJ"] <- "ESCUELA NORMAL BILING�E INTERCULTURAL OXLAJUJ NO�OJ"
FULLDATASET$ESTABLECIMIENTO[FULLDATASET$ESTABLECIMIENTO == "INSTITUTO PRIVADO MIXTO DE EDUC. BASICA Y BACHILLERATO POR MADUREZ"] <- "INSTITUTO PRIVADO MIXTO DE EDUCACION BASICA Y BACHILLERATO POR MADUREZ"
	
View(FULLDATASET)
#FIN jOSE
