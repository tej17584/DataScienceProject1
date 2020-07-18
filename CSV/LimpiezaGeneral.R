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
##setwd("C:/Users/josea/Desktop/Universidad/2020/DataScience/Proyecto1/DataScienceProject1/CSV")
setwd("C:/Users/Diego Sevilla/Documents/UVG Semestres/Repositorios/8vo Semestre/Data science/DataScienceProject1/CSV")

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

View(FULLDATASET)

#removemos duplicados

#Encontramos los duplicados (sin contar el codigo sino que todos los demas campos)
Duplicados<-FULLDATASET[duplicated(FULLDATASET[,2:17]),]
#Eliminamos los duplicados y los volvemos asignar
FULLDATASET<-FULLDATASET[!duplicated(FULLDATASET[,2:17]),]







#------------------------TEJADA ZONE----------------------

## LIMPIEZA ESPECIFICA JALAPA, CHIQUIMULA, EL PROGRESO, ZACAPA, IZABAL--------------------

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
##/--------------**--* FINALIZA LIMPIEZA ESPECÍFICA JALAPA, CHIQUIMULA, EL PROGRESO, ZACAPA, IZABAL
#------------------FIN TEJADA ZONE------------------------------


##------------------------SEVILLA ZONE----------------------
## LIMPIEZA ESPECIFICA GUATEMALA, ESCUINTLA, SANTA ROSA, JUTIAPA--------------------
View(FULLDATASET)
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

## FINALIZA LIMPIEZA ESPECIFICA GUATEMALA, ESCUINTLA, SANTA ROSA, JUTIAPA--------------------
##------------------------FIN SEVILLA ZONE----------------------

