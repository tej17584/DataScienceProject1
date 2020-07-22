###################################################
## Guatemala, julio del 2020
## Data Science 
## Alejandro Tejada 17584
## Diego Sevilla 17238
## Jose Cifuentes CARNET
## Oscar Juarez CARNET
## Jesus Belches
## Proyecto No1: Limpieza de datos
## Catedr?tico: Lynette Garc?a
###################################################

install.packages("rJava")

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
library(stringr)
##------------------FIN ZONA LIBRERIAS------------------

getwd()
setwd("C:/Users/josea/Desktop/Universidad/2020/DataScience/Proyecto1/DataScienceProject1/CSV")
#setwd("C:/Users/josea/Desktop/Universidad/2020/DataScience/Proyecto1/DataScienceProject1/CSV")
setwd("/home/paul/Documents/Semestre2/dataScience/DataScienceProject1/CSV")
#setwd("C:/Users/Diego Sevilla/Documents/UVG Semestres/Repositorios/8vo Semestre/Data science/DataScienceProject1/CSV")
# setwd("C:/Users/Oscar/Desktop/UVG/Semestre8/DataScience/DataScienceProject1/CSV")

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


	

#View(FULLDATASET)
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

View(FULLDATASET)

#removemos duplicados

###JOSE
#LIMPIEZA ESPECIFICA BAJA VERAPAZ, SAN MARACOS, QUETZALTENANGO, TOTONICAPAN, SOLOLA------------------------------------------------------------------------------------------------------
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
FULLDATASET$ESTABLECIMIENTO[FULLDATASET$ESTABLECIMIENTO == "ENBI OXLAJUJ NO?OJ"] <- "ESCUELA NORMAL BILING?E INTERCULTURAL OXLAJUJ NO?OJ"
FULLDATASET$ESTABLECIMIENTO[FULLDATASET$ESTABLECIMIENTO == "INSTITUTO PRIVADO MIXTO DE EDUC. BASICA Y BACHILLERATO POR MADUREZ"] <- "INSTITUTO PRIVADO MIXTO DE EDUCACION BASICA Y BACHILLERATO POR MADUREZ"

View(FULLDATASET)
#####------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


##PAUL
## LIMPIEZA ESPECIFICA Pete, quiche, alta verapaz , hueheu--------------------------------------------------------------------------------------------------------------------------------

AltaRechazados = c(
"16-01-0666-46", "16-03-0076-46", "16-04-0041-46", "16-04-0046-46", "16-04-0072-46", #16-01-0672-46 *ICA* 
"16-04-8321-46", "16-01-1259-46", "16-01-0558-46", "16-09-7312-46", "16-01-0664-46", "16-13-0225-46", "16-03-9884-46")

QuicheRechazados = c(
"14-01-0210-46", "14-15-0235-46","14-10-0078-46","14-10-0079-46","14-01-0090-46","14-19-0084-46","14-20-0141-46",
"14-20-0158-46",#14-13-1096-46 *EORM*
"14-20-0124-46","14-20-0126-4","14-01-0181-46","14-01-0242-46","14-21-0024-46","14-04-0048-46","14-15-0215-46v","14-12-0122-46",
"14-01-0110-46","14-01-0193-46","14-20-0143-46")

HuehueRechazados = c(
"13-26-0206-46","13-01-7148-46","13-01-0348-46","13-01-0368-46","13-01-0369-46","13-02-0209-46","13-02-0210-46","13-26-6879-46",
"13-01-0162-46","13-01-7111-46","13-02-0217-46","13-30-0017-46","13-08-0055-46","13-03-0048-46","13-01-0354-46","13-23-0035-46",
"13-01-0158-46","13-27-0068-46","13-25-0041-46","13-25-0046-46","13-25-0057-46","13-19-0057-46","13-01-6778-46","13-01-7142-46",
"13-01-1592-46","13-04-7122-46","13-01-0290-46","13-01-0218-46",#13-26-0182-46 *INED*
"13-17-0067-46","13-26-0162-46","13-26-0197-46","13-26-0201-46","13-12-0081-46","13-08-0045-46","13-08-0047-46","13-08-0053-46",
"13-17-0059-46","13-12-0113-46","13-01-1502-46","13-07-0064-46","13-12-0075-46","13-01-0244-46","13-01-0246-46","13-07-0029-46",
"13-01-0171-46","13-01-0268-46","13-01-0275-46","13-03-0065-46","13-19-0041-46","13-09-0059-46","13-15-0051-46","13-01-7082-46",
"13-24-0016-46","13-15-0061-46","13-17-0068-46","13-07-0071-46")

PetenRechazados =  c(
"17-03-3831-46","17-11-0079-46","17-11-0084-46","17-03-0104-46","17-03-0105-46","17-03-4112-46","17-04-0098-46","17-05-0321-46",
"17-07-0045-46","17-01-3821-46","17-03-3996-46","17-10-0284-46","17-14-0083-46","17-13-0136-46","17-07-0048-46","17-13-0118-46",
"17-05-0210-46","17-10-0353-46","17-05-0284-46","17-01-0081-46","17-14-0078-46","17-01-0083-46","17-05-0250-46","17-05-0301-46",
"17-09-0318-46","17-08-0069-46","17-12-0002-46","17-10-0271-46","17-10-0207-46","17-10-0249-46","17-13-0126-46","17-05-4057-46",
"17-10-0179-46","17-05-0305-46","17-05-0306-46","17-10-0179-46","17-05-0306-46","17-05-0305-46","17-02-0011-46","17-01-0125-46",
"17-01-3998-46","17-13-0107-46","17-03-0025-46","17-01-0039-46","17-01-0130-46","17-10-0251-46","17-10-0346-46","17-05-1308-46",
"17-10-3566-46","17-01-0136-46","17-12-0140-46","17-03-0084-46","17-12-0134-46","17-03-0115-46","17-08-0001-46","17-14-0044-46",
"17-03-0098-46","17-05-0232-46","17-03-2834-46")

FULLDATASET2 <- FULLDATASET[!FULLDATASET$CODIGO %in% AltaRechazados, ] 
FULLDATASET2 <- FULLDATASET[!FULLDATASET$CODIGO %in% QuicheRechazados, ] 
FULLDATASET2 <- FULLDATASET[!FULLDATASET$CODIGO %in% HuehueRechazados, ] 
FULLDATASET2 <- FULLDATASET[!FULLDATASET$CODIGO %in% PetenRechazados, ] 

#Resultados es "ICA" 
#FULLDATASET2[which(FULLDATASET2$ESTABLECIMIENTO == "ICA"),]
#FULLDATASET2$ESTABLECIMIENTO[FULLDATASET2$ESTABLECIMIENTO == "ICA"] <- "INSTITUTO NACIONAL DE EDUCACION DIVERSIFICADA"
#Resultados es "EORM"
FULLDATASET2[which(FULLDATASET2$ESTABLECIMIENTO == "EORM"),]
FULLDATASET2$ESTABLECIMIENTO[FULLDATASET2$ESTABLECIMIENTO == "EORM"] <- "ESCUELA OFICIAL RURAL MIXTA"
#Resultados es "INED" 
FULLDATASET2[which(FULLDATASET2$ESTABLECIMIENTO == "INED"),]
FULLDATASET2$ESTABLECIMIENTO[FULLDATASET2$ESTABLECIMIENTO == "INED"] <- "INSTITUTO NACIONAL DE EDUCACION DIVERSIFICADA"

#View(FULLDATASET)
View(FULLDATASET2)

DATAdePAUL <- FULLDATASET2[which( FULLDATASET2$DEPARTAMENTO == "ALTA VERAPAZ" 
                                |FULLDATASET2$DEPARTAMENTO == "QUICHE"
                                | FULLDATASET2$DEPARTAMENTO == "HUEHUETENANGO"
                                | FULLDATASET2$DEPARTAMENTO == "PETEN"
                                ),]

View(DATAdePAUL)
#-----------------------------Fin --------------------------------------------
#Encontramos los duplicados (sin contar el codigo sino que todos los demas campos)
Duplicados<-FULLDATASET[duplicated(FULLDATASET[,2:17]),]
#Eliminamos los duplicados y los volvemos asignar
FULLDATASET<-FULLDATASET[!duplicated(FULLDATASET[,2:17]),]
#####------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


##TEJADA
## LIMPIEZA ESPECIFICA JALAPA, CHIQUIMULA, EL PROGRESO, ZACAPA, IZABAL-------------------------------------------------------------------------------------------------------------------

DATATEJ<-FULLDATASET[which(FULLDATASET$DEPARTAMENTO == "CHIQUIMULA" 
                         | FULLDATASET$DEPARTAMENTO == "JALAPA"
                         | FULLDATASET$DEPARTAMENTO == "EL PROGRESO"
                         | FULLDATASET$DEPARTAMENTO == "ZACAPA"
                         | FULLDATASET$DEPARTAMENTO == "IZABAL"),]


##Se cuentan NA en cada columna
sum(is.na(DATATEJ$TELEFONO))        #67
sum(is.na(DATATEJ$DISTRITO))        #8
sum(is.na(DATATEJ$DEPARTAMENTO))
sum(is.na(DATATEJ$MUNICIPIO))
sum(is.na(DATATEJ$ESTABLECIMIENTO)) #0
sum(is.na(DATATEJ$DIRECCION))       #7
sum(is.na(DATATEJ$DIRECTOR))        #142
sum(is.na(DATATEJ$SUPERVISOR))      #8
sum(is.na(DATATEJ$NIVEL)) 
sum(is.na(DATATEJ$AREA)) 
sum(is.na(DATATEJ$STATUS))
sum(is.na(DATATEJ$MODALIDAD))
sum(is.na(DATATEJ$JORNADA))
sum(is.na(DATATEJ$PLAN))
sum(is.na(DATATEJ$DEPARTAMENTAL))
sum(is.na(DATATEJ$SUPERVISOR))      #8

#Agregamos un numero de linea
DATATEJ$NO_LINEA <- seq.int(nrow(DATATEJ))
#Reordenamos para que este al inicio
DATATEJ<-DATATEJ[,c(18,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]
View(DATATEJ)

#miramos palabras mas cortas con SIGLAS
for (variable in DATATEJ$ESTABLECIMIENTO) {
  if(nchar(variable)<5)
  {
    print(variable)
  }
}

#Resultados es "INED"
DATATEJ[which(DATATEJ$ESTABLECIMIENTO == "INED"),]
DATATEJ$ESTABLECIMIENTO[DATATEJ$ESTABLECIMIENTO == "INED"] <- "INSTITUTO NACIONAL DE EDUCACION DIVERSIFICADA"

#con otra funcion ahora

lista<-c();
for (variable in DATATEJ$ESTABLECIMIENTO) {
  lista<-c(lista,substr(variable, 0, regexpr(' ', variable)))
}

lista2<-as.data.frame(lista)
distinct(lista2)


#PATRON DE "LICEO-----------------------
# create a pattern to use (the same as you would do when using the LIKE operator)
ptn = '^"LICEO.*?'  # gets beige and berry but not blueberry
# execute a pattern-matching function on your data to create an index vector
ndx = grep(ptn, DATATEJ$ESTABLECIMIENTO, perl=T)
# use this index vector to extract the rows you want from the data frome:
selected_rows = DATATEJ[ndx,]

#las filas a cambiar son la 9 y 467
DATATEJ$ESTABLECIMIENTO[DATATEJ$ESTABLECIMIENTO == '"LICEO LA SALLE"'] <- "LICEO LA SALLE"
DATATEJ$ESTABLECIMIENTO[DATATEJ$ESTABLECIMIENTO == '"LICEO EDUCACIONAL MIXTO DEI VERBUM"'] <- "LICEO EDUCACIONAL MIXTO DEI VERBUM"



#PATRON DE "INST.-----------------------
# create a pattern to use (the same as you would do when using the LIKE operator)
ptn2 = '^INST. .*?'  # gets beige and berry but not blueberry
# execute a pattern-matching function on your data to create an index vector
ndx2 = grep(ptn2, DATATEJ$ESTABLECIMIENTO, perl=T)
# use this index vector to extract the rows you want from the data frome:
selected_rows2 = DATATEJ[ndx2,]

#las filas a cambiar son la 73
DATATEJ$ESTABLECIMIENTO[DATATEJ$ESTABLECIMIENTO == "INST. PRIV. MIXTO DE EDUC. DIVERSIFICADA ESCUELA DE CIENCIAS COMERCIALES"] <- 
  "INSTITUTO PRIVADO MIXTO DE EDUCACION DIVERSIFICADA ESCUELA DE CIENCIAS COMERCIALES"


#PATRON DE 'LICEO-----------------------
# create a pattern to use (the same as you would do when using the LIKE operator)
ptn3 = "^'LICEO.*?"  # gets beige and berry but not blueberry
# execute a pattern-matching function on your data to create an index vector
ndx3 = grep(ptn3, DATATEJ$ESTABLECIMIENTO, perl=T)
# use this index vector to extract the rows you want from the data frome:
selected_rows3 = DATATEJ[ndx3,]

DATATEJ$ESTABLECIMIENTO[DATATEJ$ESTABLECIMIENTO == "'LICEO SAN JOSE'"] <- "LICEO SAN JOSE"


#PATRON DE 'LICEO-----------------------
# create a pattern to use (the same as you would do when using the LIKE operator)
ptn4 = "^INED.*?"  # gets beige and berry but not blueberry
# execute a pattern-matching function on your data to create an index vector
ndx4 = grep(ptn4, DATATEJ$ESTABLECIMIENTO, perl=T)
# use this index vector to extract the rows you want from the data frome:
selected_rows4 = DATATEJ[ndx4,]

DATATEJ$ESTABLECIMIENTO[DATATEJ$ESTABLECIMIENTO == "INED PROF. HUGO LEONEL SANCE CETINO"] <- 
  "INSTITUTO NACIONAL DE EDUCACION DIVERSIFICADA PROF. HUGO LEONEL SANCE CETINO"
DATATEJ$ESTABLECIMIENTO[DATATEJ$ESTABLECIMIENTO == "INED ADSCRITO AL INSTITUTO NACIONAL DE EDUCACION BASICA EXPERIMENTAL CON ORIENTACION OCUPACIONAL DR. LUIS PASTEUR"] <- 
  "INSTITUTO NACIONAL DE EDUCACION DIVERSIFICADA ADSCRITO AL INSTITUTO NACIONAL DE EDUCACION BASICA EXPERIMENTAL CON ORIENTACION OCUPACIONAL DR. LUIS PASTEUR"
DATATEJ$ESTABLECIMIENTO[DATATEJ$ESTABLECIMIENTO == "INED TECNICO MORALENSE, ITM"] <- 
  "INSTITUTO NACIONAL DE EDUCACION DIVERSIFICADA TECNICO MORALENSE, ITM"

#removemos la data de las variables
rm(ptn,ptn2,ptn3,ptn4)
rm(ndx,ndx2,ndx3,ndx4)
rm(lista, variable)
rm(selected_rows,selected_rows2,selected_rows3,selected_rows4, lista2)

#eliminamos la columna de numero
DATATEJ$NO_LINEA<-NULL
#LIMPIEZA FINAL
View(DATATEJ)
str(DATATEJ)
#####------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


##DIEGO
## LIMPIEZA ESPECIFICA GUATEMALA, ESCUINTLA, SANTA ROSA, JUTIAPA-----------------------------------------------------------------------------------------------------------------
#View(FULLDATASET)
DATASEV<-FULLDATASET[which(FULLDATASET$DEPARTAMENTO == "GUATEMALA"
                         | FULLDATASET$DEPARTAMENTO == "ESCUINTLA"
                         | FULLDATASET$DEPARTAMENTO == "SANTA ROSA"
                         | FULLDATASET$DEPARTAMENTO == "JUTIAPA"),]
View(DATASEV)

##NAs
sum(is.na(DATASEV$TELEFONO))        #153
sum(is.na(DATASEV$DISTRITO))        #73
sum(is.na(DATASEV$DEPARTAMENTO))
sum(is.na(DATASEV$MUNICIPIO))
sum(is.na(DATASEV$ESTABLECIMIENTO)) 
sum(is.na(DATASEV$DIRECCION))       #28
sum(is.na(DATASEV$DIRECTOR))        #514
sum(is.na(DATASEV$SUPERVISOR))      #73
sum(is.na(DATASEV$NIVEL)) 
sum(is.na(DATASEV$AREA)) 
sum(is.na(DATASEV$STATUS))
sum(is.na(DATASEV$MODALIDAD))
sum(is.na(DATASEV$JORNADA))
sum(is.na(DATASEV$PLAN))
sum(is.na(DATASEV$DEPARTAMENTAL))
sum(is.na(DATASEV$SUPERVISOR))      #73

##SE REVISAN SIGLAS
for (variable in DATASEV$ESTABLECIMIENTO) {
  if(nchar(variable)<5)
  {
    print(variable)
  }
}
#Segun el for no hay siglas en este dataset, pero ya veremos

##SE REVISA LA PRIMERA PALABRA DE CADA NOMBRE DE LOS ESTABLECIMIENTOS
lista<-c();
for (variable in DATASEV$ESTABLECIMIENTO) {
  lista<-c(lista,substr(variable, 0, regexpr(' ', variable)))
}

lista2<-as.data.frame(lista)
distinct(lista2)
#Nos muestra varias inconsistencias como: comillas, palabras incompletas, abreviaturas, etc.

#SE QUITAN COMILLAS
DATASEV$ESTABLECIMIENTO[DATASEV$ESTABLECIMIENTO == "'ESCUELA NORMAL PARTICULAR DEL SUR'"] <- 
  "ESCUELA NORMAL PARTICULAR DEL SUR"
DATASEV$ESTABLECIMIENTO[DATASEV$ESTABLECIMIENTO == "'LICEO VALLE DEL SOL'"] <- 
  "LICEO VALLE DEL SOL"
DATASEV$ESTABLECIMIENTO[DATASEV$ESTABLECIMIENTO == '"LICEO CRISTIANO NAZARETH"'] <- 
  "LICEO CRISTIANO NAZARETH"
DATASEV$ESTABLECIMIENTO[DATASEV$ESTABLECIMIENTO == '"LICEO EL CARMEN"'] <- 
  "LICEO EL CARMEN"
DATASEV$ESTABLECIMIENTO[DATASEV$ESTABLECIMIENTO == '"LICEO TIUCAL"'] <- 
  "LICEO TIUCAL"


##SE CORRIGEN PALABRAS MAL ESCRITAS
DATASEV$ESTABLECIMIENTO[DATASEV$ESTABLECIMIENTO == "INSTITUO MIXTO NOCTURNO EL ANGEL"] <- 
  "INSTITUTO MIXTO NOCTURNO EL ANGEL"
DATASEV$ESTABLECIMIENTO[DATASEV$ESTABLECIMIENTO == "CDOLEGIO CRISTIANO BILINGUE GENERACION DE JOSUE"] <- 
  "COLEGIO CRISTIANO BILINGUE GENERACION DE JOSUE"
DATASEV$ESTABLECIMIENTO[DATASEV$ESTABLECIMIENTO == "INTITUTO TECNOLOGICO DE SUR ORIENTE"] <- 
  "INSTITUTO TECNOLOGICO DE SUR ORIENTE"
DATASEV$ESTABLECIMIENTO[DATASEV$ESTABLECIMIENTO == 'ACADEMIA COMERCIAL E INTITUTO "LOURDES"'] <- 
  'ACADEMIA COMERCIAL E INSTITUTO "LOURDES"'
DATASEV$ESTABLECIMIENTO[DATASEV$ESTABLECIMIENTO == "INSTIITUTO EDUCATIVO ASISTENCIAL EMILIANI Y HOGAR DE HUERFANOS SANTA TERESA"] <-
  "INSTITUTO EDUCATIVO ASISTENCIAL EMILIANI Y HOGAR DE HUERFANOS SANTA TERESA"
DATASEV$ESTABLECIMIENTO[DATASEV$ESTABLECIMIENTO == "CENTRODE ENSENANZA SUPERIOR BILINGUE EN COMPUTACION CESBIC."] <-
  "CENTRO DE ENSENANZA SUPERIOR BILINGUE EN COMPUTACION CESBIC."

##SE ELIMINAN siglas
DATASEV$ESTABLECIMIENTO[DATASEV$ESTABLECIMIENTO == "INST. MIXTO DE DIVERSIFICADO POR COOPERATIVA DE ENSENANZA ANGELINA ACUNA"] <- 
  "INSTITUTO MIXTO DE DIVERSIFICADO POR COOPERATIVA DE ENSENANZA ANGELINA ACUNA"

##CPMI
DATASEV$ESTABLECIMIENTO[DATASEV$ESTABLECIMIENTO == "CPMI EVANGELICO SAMARIA"] <- 
  "COLEGIO PRIVADO MIXTO INSTITUCIONAL EVANGELICO SAMARIA"
DATASEV$ESTABLECIMIENTO[DATASEV$ESTABLECIMIENTO == "CPMI DE CIENCIAS COMERCIALES SAN JOSE"] <- 
  "COLEGIO PRIVADO MIXTO INSTITUCIONAL DE CIENCIAS COMERCIALES SAN JOSE"
DATASEV$ESTABLECIMIENTO[DATASEV$ESTABLECIMIENTO == "CPMI 'SANTA FAMILIA'"] <- 
  "COLEGIO PRIVADO MIXTO INSTITUCIONAL SANTA FAMILIA"
DATASEV$ESTABLECIMIENTO[DATASEV$ESTABLECIMIENTO == "CPMI 'SAN JOSE OBRERO I'"] <- 
  "COLEGIO PRIVADO MIXTO INSTITUCIONAL SAN JOSE OBRERO I"

#INED
DATASEV$ESTABLECIMIENTO[DATASEV$ESTABLECIMIENTO == "INED ULEW KOTZ'I'J (TIERRA DE LAS FLORES)"] <- 
  "INSTITUTO NACIONAL DE EDUCACION DIVERSIFICADA ULEW KOTZ'I'J (TIERRA DE LAS FLORES)"
DATASEV$ESTABLECIMIENTO[DATASEV$ESTABLECIMIENTO == "INED ULEW KOTZ'I'J (TIERRA DE LAS FLORES)"] <- 
  "INSTITUTO NACIONAL DE EDUCACION DIVERSIFICADA ULEW KOTZ'I'J (TIERRA DE LAS FLORES)"
DATASEV$ESTABLECIMIENTO[DATASEV$ESTABLECIMIENTO == "INED EDGAR ARNOLDO MEDRANO"] <- 
  "INSTITUTO NACIONAL DE EDUCACION DIVERSIFICADA EDGAR ARNOLDO MEDRANO"

#CPUM
DATASEV$ESTABLECIMIENTO[DATASEV$ESTABLECIMIENTO == "CPUM COLEGIO MIXTO BILINGUE 'PINEDA'"] <- 
  "COLEGIO PRIVADO URBANO MIXTO COLEGIO BILINGUE 'PINEDA'"

View(DATASEV)
str(DATASEV)
#####------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


##OSCAR
## LIMPIEZA ESPECIFICA SUCHITEPEQUEZ, SACATEPEQUEZ, RETALHULEU, CHIMALTENANGO --------------------

DATA3<-FULLDATASET[which(FULLDATASET$DEPARTAMENTO == "SUCHITEPEQUEZ" 
                         | FULLDATASET$DEPARTAMENTO == "SACATEPEQUEZ"
                         | FULLDATASET$DEPARTAMENTO == "RETALHULEU"
                         | FULLDATASET$DEPARTAMENTO == "CHIMALTENANGO"),]


##Se cuentan NA en cada columna
sum(is.na(DATA3$TELEFONO))        #71
sum(is.na(DATA3$DISTRITO))        #12
sum(is.na(DATA3$DEPARTAMENTO))
sum(is.na(DATA3$MUNICIPIO))
sum(is.na(DATA3$ESTABLECIMIENTO)) #0
sum(is.na(DATA3$DIRECCION))       #3
sum(is.na(DATA3$DIRECTOR))        #248
sum(is.na(DATA3$SUPERVISOR))      #13
sum(is.na(DATA3$NIVEL)) 
sum(is.na(DATA3$AREA)) 
sum(is.na(DATA3$STATUS))
sum(is.na(DATA3$MODALIDAD))
sum(is.na(DATA3$JORNADA))
sum(is.na(DATA3$PLAN))
sum(is.na(DATA3$DEPARTAMENTAL))
sum(is.na(DATA3$SUPERVISOR))      #13

#Agregamos un numero de linea
DATA3$NO_LINEA <- seq.int(nrow(DATA3))
#Reordenamos para que este al inicio
DATA3<-DATA3[,c(18,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]


listaCasosEspeciales <- c(DATA3[ nchar(DATA3$ESTABLECIMIENTO)<10 , 1])
for (variable in listaCasosEspeciales) {
  print(variable)
  print(DATA3[variable,6])
}

# CASOS ESPEC?FICOS A NOTAR
#1. #?NOMBRE?
DATA3[538,6] <- "CENTRO DE EDUCACION EXTRAESCOLAR ESCUELA DE CAFICULTURA ANTIGUA COFEE"

DATA3[633,6] <- "JUDA"

#miramos palabras mas cortas con SIGLAS
for (variable in DATA3$ESTABLECIMIENTO) {
  if(nchar(variable)<10)
  {
    print(variable)
  }
}


# RESULTADOS DEL CICLO FOR

#Resultado 1: "INED"
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "INED", replacement = "INSTITUTO NACIONAL DE EDUCACION DIVERSIFICADA", fixed = TRUE)

#Ahora para ver si tenemos otras siglas integradas a strings
lista<-c()
filas<-c()
for (variable in DATA3$ESTABLECIMIENTO) {
  lista<-c(lista,substr(variable, 0, regexpr(' ', variable)))
}

lista2<-as.data.frame(lista)
dplyr::distinct(lista2)

# REEMPLAZANDO ABREVIACIONES:
#1. INST.
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "INST.", replacement = "INSTITUCION", fixed = TRUE)

#2. EDUC.
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "EDUC.", replacement = "EDUCACION", fixed = TRUE)


# REEMPLAZANDO PALABRAS MAL ESCRITAS:
#1. COELGIO
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "COELGIO", replacement = "COLEGIO", fixed = FALSE)

#2. INSTITITO, INSTIUTO o INTITUTO
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "INSTITITO|INSTIUTO|INTITUTO", replacement = "INSTITUTO", fixed = FALSE)

#3. CENTREO
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "CENTREO", replacement = "CENTRO", fixed = FALSE)


#PATRON DE 'TECNOLOGICO-----------------------
# create a pattern to use (the same as you would do when using the LIKE operator)
ptn = "^'EL*?"  # gets beige and berry but not blueberry
# execute a pattern-matching function on your data to create an index vector
ndx = grep(ptn, DATA3$ESTABLECIMIENTO, perl=T)
# use this index vector to extract the rows you want from the data frome:
selected_rows = DATA3[ndx,c(1,6)]

# Quitamos apostrophes de cada fila
for (row in selected_rows$NO_LINEA) {
  DATA3[row,6] <- gsub("'", '', DATA3[row,6])
}

#removemos la data de las variables
rm(ptn, ndx, selected_rows, lista, variable, lista2, listaCasosEspeciales)

#eliminamos la columna de numero
DATA3$NO_LINEA<-NULL
#LIMPIEZA FINAL
View(DATA3)
str(DATA3)

###SE HACE RBIND A TODOS LOS DATASETS 
FULLDATASET_clean <- rbind( "DATAdeJOSE", DATATEJ, DATAdePAUL ,DATASEV,DATA3)
View(FULLDATASET_clean)

