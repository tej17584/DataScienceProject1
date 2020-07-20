###################################################
## Guatemala, julio del 2020
## Data Science 
## Alejandro Tejada 17584
## Diego Sevilla 17238
## Jose Cifuentes CARNET
## Oscar Juarez 17315
## Jesus Belches
## Proyecto No1: Limpieza de datos
## Catedr?tico: Lynette Garc?a
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
library(stringr)
##------------------FIN ZONA LIBRERIAS------------------

# setwd("C:/Users/Usuario/Desktop/UVG/Semestre8/DataScience/DataScienceProject1/CSV")

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



## LIMPIEZA ESPECIFICA SUCHITEPEQUEZ, SACATEPEQUEZ, RETALHULEU, CHIMALTENANGO --------------------

DATA3<-FULLDATASET[which(FULLDATASET$DEPARTAMENTO == "SUCHITEPEQUEZ" 
                         | FULLDATASET$DEPARTAMENTO == "SACATEPEQUEZ"
                         | FULLDATASET$DEPARTAMENTO == "RETALHULEU"
                         | FULLDATASET$DEPARTAMENTO == "CHIMALTENANGO"),]


##Se cuentan NA en cada columna
sum(is.na(DATA3$TELEFONO))        #1377
sum(is.na(DATA3$DISTRITO))        #193
sum(is.na(DATA3$DEPARTAMENTO))
sum(is.na(DATA3$MUNICIPIO))
sum(is.na(DATA3$ESTABLECIMIENTO)) #0
sum(is.na(DATA3$DIRECCION))       #49
sum(is.na(DATA3$DIRECTOR))        #3642
sum(is.na(DATA3$SUPERVISOR))      #195
sum(is.na(DATA3$NIVEL)) 
sum(is.na(DATA3$AREA)) 
sum(is.na(DATA3$STATUS))
sum(is.na(DATA3$MODALIDAD))
sum(is.na(DATA3$JORNADA))
sum(is.na(DATA3$PLAN))
sum(is.na(DATA3$DEPARTAMENTAL))
sum(is.na(DATA3$SUPERVISOR))      #195

#Agregamos un numero de linea
DATA3$NO_LINEA <- seq.int(nrow(DATA3))
#Reordenamos para que este al inicio
DATA3<-DATA3[,c(18,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]


listaCasosEspeciales <- c(DATA3[ nchar(DATA3$ESTABLECIMIENTO)<10 , 1])
for (variable in listaCasosEspeciales) {
  print(variable)
  print(DATA3[variable,6])
}

# CASOS ESPECÍFICOS A NOTAR EN DONDE LOS ESTABLECIMIENTOS REPITEN SIGLAS
#1. EODP EODP
DATA3[6777,6] <- "EODP"

#2. EPRM EPRM
DATA3[6759,6] <- "EPRM"

#3. #¿NOMBRE?
# Validado acorde al código de establecimiento y la página del mineduc
DATA3[3857,6] <- "CENTRO DE EDUCACION EXTRAESCOLAR ESCUELA DE CAFICULTURA ANTIGUA COFEE"

#4. EPUM MIXTA (EPUM es ESCUELA PRIVADA URBANA MIXTA) por lo que se repite la palabra Mixta
DATA3[2132,6] <-  "EPUM COLEGIO 'ZARAGOZA' 7A. AVENIDA NORTE NO.10"


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

#Resultado 2: "EORM"
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "EORM", replacement = "ESCUELA OFICIAL RURAL MIXTA", fixed = TRUE)

#Resultado 3: "INEB"
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "INEB", replacement = "INSTITUCION NACIONAL DE EDUCACION BASICA", fixed = TRUE)

#Resultado 4: "EPRM"
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "EPRM", replacement = "ESCUELA PRIVADA RURAL MIXTA", fixed = TRUE)

#Resultado 5: "IEBC"
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "IEBC", replacement = "INSTITUTO DE EDUCACION BASICA COOPERATIVA", fixed = TRUE)

#Resultado 6: "EODP"
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "EODP", replacement = "ESCUELA OFICIAL DE PARVULOS", fixed = TRUE)

#Resultado 7: "EOUM"
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "EOUM", replacement = "ESCUELA OFICIAL URBANA MIXTA", fixed = TRUE)

#Resultado 8: "EOUN"
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "EOUN", replacement = "ESCUELA OFICIAL URBANA DE NIÑAS", fixed = TRUE)

#Resultado 9: "EOPA"
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "EOPA", replacement = "ESCUELA OFICIAL PARA ADULTOS", fixed = TRUE)

#Resultado 10: "EOUV"
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "EOUV", replacement = "ESCUELA OFICIAL URBANA DE VARONES", fixed = TRUE)

#Resultado 11: "COPB"
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "COPB", replacement = "CENTRO OFICIAL DE PREPRIMARIA BILINGUE", fixed = TRUE)

#Resultado 12: "EORN"
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "EORN", replacement = "ESCUELA OFICIAL RURAL DE NI?AS", fixed = TRUE)

#Resultado 13: "EMRM"
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "EMRM", replacement = "ESCUELA MUNICIPAL RURAL MIXTA", fixed = TRUE)

#Resultado 14: "IEEB"
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "IEEB", replacement = "INSTITUTO EXPERIMENTAL DE EDUCACION BASICA", fixed = TRUE)

#Resultado 15: "EMUM"
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "EMUM", replacement = "ESCUELA MUNICIPAL URBANA MIXTA", fixed = TRUE)

#Resultado 16: "EORV"
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "EORV", replacement = "ESCUELA MUNICIPAL URBANA MIXTA", fixed = TRUE)

#Resultado 17: "ERMP"
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "ERMP", replacement = "ESCUELA RURAL MIXTA DE PARVULOS", fixed = TRUE)

#Resultado 18: "CEIN"
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "CEIN", replacement = "CENTRO DE EDUCACION INTEGRAL", fixed = TRUE)

#Resultado 19: "PAIN"
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "PAIN", replacement = "PROYECTO DE ATENCION AL NIÑO Y LA NIÑA", fixed = TRUE)


#Ahora para ver si tenemos otras siglas integradas a strings
lista<-c()
filas<-C()
for (variable in DATA3$ESTABLECIMIENTO) {
  lista<-c(lista,substr(variable, 0, regexpr(' ', variable)))
}

lista2<-as.data.frame(lista)
dplyr::distinct(lista2)


#Resultado 20: "CEEX"
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "CEEX", replacement = "CENTRO DE EDUCACION EXTRAESCOLAR", fixed = TRUE)

#Resultado 21: "CEES"
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "CEES", replacement = "CENTRO DE EDUCACION ESPECIAL", fixed = TRUE)

#Resultado 22: "CPRM"
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "CPRM", replacement = "COLEGIO PRIVADO RURAL MIXTO", fixed = TRUE)

#Resultado 23: "EPPA"
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "EPPA", replacement = "ESCUELA PRIVADA PARA ADULTOS", fixed = TRUE)

#Resultado 24: "IPMB"
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "IPMB", replacement = "INSTITUTO PRIVADO MIXTO DE EDUCACION BASICA", fixed = TRUE)

#Resultado 25: "CPMI"
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "CPMI", replacement = "COLEGIO PARTICULAR MIXTO", fixed = TRUE)

#Resultado 26: "EPUM"
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "EPUM", replacement = "ESCUELA PRIVADA URBANA MIXTA", fixed = TRUE)

#Resultado 27: "EMDP"
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "EMDP", replacement = "ESTUDIOS MULTIDIMENSIONALES DE PAIS", fixed = TRUE)

#Resultado 28: "EPDP"
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "EPDP", replacement = "ESCUELA PRIVADA DE PARVULOS", fixed = TRUE)

#Resultado 29: "EUMP"
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "EUMP", replacement = "ESCUELA URBANA MIXTA DE PARVULOS", fixed = TRUE)

#Resultado 30: "CPUM"
DATA3$ESTABLECIMIENTO <- lapply(DATA3$ESTABLECIMIENTO, gsub, pattern = "CPUM", replacement = "COLEGIO PRIVADO URBANO MIXTO", fixed = TRUE)



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
#4. NUCLE
DATA3[2753,6] <- "NUCLEO FAMILIAR EDUCATIVO PARA EL DESARROLLO N. 317"
#4. COLEGI
DATA3[3238,6] <- "COLEGIO LICEO RURAL MIXTO TULULA"


#PATRON DE "LICEO-----------------------
# create a pattern to use (the same as you would do when using the LIKE operator)
ptn = "^'LICEO.*?"  # gets beige and berry but not blueberry
# execute a pattern-matching function on your data to create an index vector
ndx = grep(ptn, DATA3$ESTABLECIMIENTO, perl=T)
# use this index vector to extract the rows you want from the data frome:
selected_rows = DATA3[ndx,c(1,6)]

print(selected_rows)

# filas a cambiar con el patron
DATA3$ESTABLECIMIENTO[DATA3$ESTABLECIMIENTO == "'LICEO CRISTIANO JERUSALEN'"] <- "LICEO CRISTIANO JERUSALEN"


#PATRON DE 'COLEGIO-----------------------
# create a pattern to use (the same as you would do when using the LIKE operator)
ptn = "^'COLE.*?"  # gets beige and berry but not blueberry
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

##/--------------**--* FINALIZA LIMPIEZA ESPEC?FICA SUCHITEPEQUEZ, SACATEPEQUEZ, RETALHULEU, CHIMALTENANGO