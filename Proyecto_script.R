#Proyecto: Desarrollar un sistema de mineria de datos, utilizando la metodologia CRISP,
# para proponer medidas practicas para disminuir la cantidad de accidentes de transito occurridos.

# Dataset basado en VicStats

library(plotrix)   # graficas
library(psych)
library(plyr)
library(dplyr) 
library(qwraps2)
library(MASS)
library(car)
library(sm)
library(scatterplot3d)
library(rgl)
library(lattice)
require(vcd)
library(epitools)
library(boot)
library(energy)
library(rpart.plot)
library(ggplot2)       # for generating visualizations
library(scales)
library(lubridate)


#Analisis de la base de Personas
persona.dat <- read.table(file = "C:/Users/Brenda Jimenez/Documents/Brenda/Mineria de Datos/Modulo 5/Proyecto/person.csv",
                          dec = ",",sep = ",",header = T)
attach(persona.dat)
#persona.dat
colnames(persona.dat)
summary(persona.dat)


# Se genera una variable auxiliar age1 para poder realizar
#la gráfica de la distribución de la variable age

age1<-Person.p$AGE
summary(age1)
age2<-age1[!is.na(age1)]
summary(age2)
hist(age2,col="white",main="Histograma ",xlab="Edad",ylab="frecuencia absoluta",
     col.main="darkblue",col.axis="red",col.lab="darkgreen",col.sub=" skyblue", freq =FALSE)

lines(density(age2),col="blue",lwd=2,lty=2)
class(age2)


#boxplot(Person.p$AGE,col="white",main="AGE",ylab="", horizontal=T)
boxplot(Person.p$AGE,col="white",main="AGE",ylab="", boxwex=.25, range=1.5,horizontal=T)


#Demás variables cualitativas utilizando el comando table
sexo<-table(Person.p$SEX)
sexo
prop.table(sexo)
#pie(sexo), esta no se ve chida
table_SP<-table(Person.P$SEATING_POSITION)
table_SP
prop.table(table_SP)

table_LS<-table(Person.p$LICENCE_STATE)
table_LS
prop.table(table_LS)

table_HBW<-table(Person.p$HELMET_BELT_WORN)
table_HBW
prop.table(table_RBW)

table_PM<-table(Person.p$PEDEST_MOVEMENT)
table_PM
prop.table(table_PM)

table_IL<-table(Person.p$INJ_LEVEL)
table_IL
prop.table(table_IL)

table_RUT<-table(Person.p$ROAD_USER_TYPE)
table_RUT
prop.table(table_RUT)

table_TAKEN<-table(Person.p$TAKEN_HOSPITAL)
table_TAKEN
prop.table(table_TAKEN)

table_SitPos<-table(Person.p$SEATING_POSITION)
table_SitPos
prop.table(table_SitPos)

table_HELMET<-table(Person.p$HELMET_BELT_WORN)
table_HELMET
prop.table(table_HELMET)

table1<-table((sexo),Person.p$Age.Group)
table1

table(Person$EJECTED_CODE)

#histograma

hist(Person.p$AGE,col=rainbow(6),main="Histograma: ",sub="PEMEX",xlab="consumo de energía",ylab="frecuencia absoluta",
     col.main="darkblue",col.axis="red",col.lab="darkgreen",col.sub=" skyblue ")
q<-quantile(Person.p$AGE,prob=c(seq(0,1,0.1)), na.rm = TRUE)  ### Definimos nosotros el número de intervalos

hist(Person.p$AGE,col=rainbow(10),main="Histograma: consumo de energía",sub="PEMEX",xlab="consumo de energía",ylab="frecuencia absoluta",
     col.main="darkblue",col.axis="red",col.lab="darkgreen",col.sub=" skyblue", breaks.q)

lines(density(Person.p$AGE, na.rm = TRUE), col="red",lwd=8)
class(AGE)

table(Person$SEX)
sexo
prop.table(sexo)
#pie(sexo), esta no se ve chida
barplot(sexo)
table1<-table(Person$SEX,Person$Age.Group)
table1

dimnames(ROAD_USER_TYPE)
#Se realiza el análisis estadístico de las siguientes variables
#Variable Sexo
sexo<-table(Person$SEX)
sexo
prop.table(sexo)
#pie(sexo), esta no se ve chida
barplot(sexo)
table1<-table(Person$SEX,Person$Age.Group)
table1


table(Person$Inj.Level.Desc)
table(Person$TAKEN_HOSPITAL)
table(Person$LICENCE_STATE)
table(Person$PEDEST_MOVEMENT)
table(Person$Road.User.Type.Desc)
table(Person$ROAD_USER_TYPE)
summary(Person$ROAD_USER_TYPE)
table(Person$EJECTED_CODE)


# **** Estadistica descriptiva de las variables 

#Sexo
sexo<-prop.table(table(persona.dat$SEX))
sexo
pie(sexo) #Mejorar
barplot(sexo) #Poner porcentajes


#edad 
dummy<-persona.dat$AGE[persona.dat$AGE<=95 & !is.nan(persona.dat$AGE)]
boxplot(dummy, main="Boxplot",col.main="gray50",horizontal = TRUE)



# *********** Vehiculos 

vehiculo.dat <- read.table(file = "C:/Users/Brenda Jimenez/Documents/Brenda/Mineria de Datos/Modulo 5/Proyecto/vehicle.csv",
                          dec = ",",sep = ",",header = T)
colnames(vehiculo.dat)
attach(vehiculo.dat)
summary(vehiculo.dat) 

## contar NA's
for (Var in names(vehiculo)) {
  missing <- sum(is.na(vehiculo[,Var]))
  if (missing > 0) {
    print(c(Var,missing))
  }
}
##obtener frecuencias
t(table(TRAFFIC_CONTROL))
for (Var in names(vehiculo)) {
  print(table(vehiculo[,Var]))
}
apply(vehiculo[,c(3,15,18:24)], 2, mean, na.rm=TRUE)
apply(vehiculo[,c(3,15,18:24)], 2, sd, na.rm=TRUE)
apply(vehiculo[,c(3,15,18:24)], 2, min, na.rm=TRUE)
apply(vehiculo[,c(3,15,18:24)], 2, max, na.rm=TRUE)
apply(vehiculo[,c(3,15,18:24)], 2, modes, na.rm=TRUE)

#Ano de manufactura del Vehiculo
anoVeh<-vehiculo.dat$VEHICLE_YEAR_MANUF
hist(anoVeh, col='midnightblue', border="gray", labels=FALSE, main="Histograma", sub="", xlab="",  ylab="Frecuencia absoluta",
     col.main="gray50",col.axis="gray40",col.lab="gray40",col.sub="gray40", freq = TRUE )
lines(density(anoVeh),col="red")
boxplot(anoVeh, main="Boxplot",col.main="gray50",horizontal = TRUE)

#numero total de ocupantes
dummy<-vehiculo.dat$TOTAL_NO_OCCUPANTS[vehiculo.dat$TOTAL_NO_OCCUPANTS<=7 & !is.nan(vehiculo.dat$TOTAL_NO_OCCUPANTS)]
hist(dummy, label=TRUE)
boxplot(dummy, main="Boxplot",col.main="gray50",horizontal = TRUE)

#tare weigth 
dummy<-vehiculo.dat$TARE_WEIGHT[vehiculo.dat$TARE_WEIGHT<=3000 & !is.nan(vehiculo.dat$TARE_WEIGHT)]
hist(dummy, label=TRUE)
boxplot(dummy, main="Boxplot",col.main="gray50",horizontal = TRUE)

#Cilindros 
dummy<-vehiculo.dat$NO_OF_CYLINDERS[vehiculo.dat$NO_OF_CYLINDERS<=8 & !is.nan(vehiculo.dat$NO_OF_CYLINDERS)]
hist(dummy, label=TRUE)
boxplot(dummy, main="Boxplot",col.main="gray50",horizontal = TRUE)

# Tipo de vehiculo 
dummy<-vehiculo.dat$VEHICLE_COLOUR_1
table(dummy)
boxplot(dummy, main="Boxplot",col.main="gray50",horizontal = TRUE)

boxplot(x=vehiculo[,3],cex.lab=.5, cex.axis=.5, cex.main=.5, cex.sub=.5)
title("Vehicle_Year_Manuf")
boxplot(x=vehiculo[,15],cex.lab=.5, cex.axis=.5, 
        cex.main=1, cex.sub=.5)
title("Vehicle_Weight")
boxplot(x=vehiculo[,18],cex.lab=.5, cex.axis=.5, cex.main=1, cex.sub=.5)
title("No_Of_Wheels")

boxplot(x=vehiculo[,19],cex.lab=.5, cex.axis=.5, cex.main=.5, cex.sub=.5)
title("No_Of_Cylinders")
par(mfrow=c(2,2))
boxplot(x=vehiculo[,20],cex.lab=.5, cex.axis=.5, cex.main=.5, cex.sub=.5)
title("Seating_Capacity")
boxplot(x=vehiculo[,21],cex.lab=.5, cex.axis=.5, cex.main=.5, cex.sub=.5)
title("Tare_Weight")
##par(mfrow=c(1,3))
boxplot(x=vehiculo[,22],cex.lab=.5, cex.axis=.5, cex.main=.5, cex.sub=.5)
title("Total_No_Occupants")
boxplot(x=vehiculo[,23],cex.lab=.5, cex.axis=.5, cex.main=.5, cex.sub=.5)
title("Carry_Capacity")
boxplot(x=vehiculo[,24],cex.lab=.5, cex.axis=.5, cex.main=.5, cex.sub=.5)
title("Cubic_Capacity")


##par(mfrow=c(2,2))
qna<-!is.na(VEHICLE_YEAR_MANUF)
VEHICLE_YEAR_MANUF1<-VEHICLE_YEAR_MANUF[qna]
alejados<-which(VEHICLE_YEAR_MANUF1<1980 | VEHICLE_YEAR_MANUF1>2018)
VEHICLE_YEAR_MANUF.DEP<-VEHICLE_YEAR_MANUF1[c(-alejados)]
q<-quantile(VEHICLE_YEAR_MANUF.DEP,prob=c(seq(0,1,0.05)))
hist(VEHICLE_YEAR_MANUF.DEP,main="Año de fabricación de autos",xlab="año fabricación",ylab="frecuencia",
     col.main="darkblue",col.axis="red",col.lab="darkgreen",col.sub=" skyblue ", breaks =q)
lines(density(VEHICLE_YEAR_MANUF.DEP),col="blue",lwd=2)


qna<-!is.na(VEHICLE_WEIGHT)
VEHICLE_WEIGHT1<-VEHICLE_WEIGHT[qna]
alejados<-which(VEHICLE_WEIGHT1==0)
VEHICLE_WEIGHT.DEP<-VEHICLE_WEIGHT1[c(-alejados)]
q<-quantile(VEHICLE_WEIGHT.DEP,prob=c(seq(0,1,0.1)))
hist(VEHICLE_WEIGHT.DEP,main="Peso del vehículo",xlab="peso",ylab="frecuencia",
     col.main="darkblue",col.axis="red",col.lab="darkgreen",col.sub=" skyblue ")


qna<-!is.na(NO_OF_WHEELS)
NO_OF_WHEELS1<-NO_OF_WHEELS[qna]
q<-quantile(NO_OF_WHEELS1,prob=c(seq(0,1,0.25)))
hist(NO_OF_WHEELS.DEP,main="Histograma:Número de llantas ",xlab="Número de llantas",ylab="frecuencia absoluta",
     col.main="darkblue",col.axis="red",col.lab="darkgreen",col.sub=" skyblue ")


qna<-!is.na(NO_OF_CYLINDERS)
NO_OF_CYLINDERS1<-NO_OF_CYLINDERS[qna]
q<-quantile(NO_OF_CYLINDERS1,prob=c(seq(0,1,0.25)))
hist(NO_OF_CYLINDERS1,main="Número de cilindros ",xlab="Número de cilindros",ylab="frecuencia absoluta",
     col.main="darkblue",col.axis="red",col.lab="darkgreen",col.sub=" skyblue ")

qna<-!is.na(TOTAL_NO_OCCUPANTS)
TOTAL_NO_OCCUPANTS1<-TOTAL_NO_OCCUPANTS[qna]
q<-quantile(TOTAL_NO_OCCUPANTS1,prob=c(seq(0,1,0.25)))
hist(TOTAL_NO_OCCUPANTS1,main="Ocupantes ",xlab="Número de ocupantes",ylab="frecuencia absoluta",
     col.main="darkblue",col.axis="red",col.lab="darkgreen",col.sub=" skyblue ")


# ****** Accidentes 


accidentes.dat <- read.table(file = "C:/Users/Brenda Jimenez/Documents/Brenda/Mineria de Datos/Modulo 5/Proyecto/accident.csv",
                           dec = ".",sep = ",",header = T)
accidentes.dat <- as.data.frame(accidentes.dat)
colnames(accidentes.dat)
attach(accidentes.dat)
#rename(accidentes.dat, c("beta"="two", "gamma"="three"))
#toupper(names(accidentes.dat))

summary(accidentes.dat)

## Convertimos los espacios en blancos en NA 
accidentes.dat[accidentes.dat == ""] <- NA
## Vemos cuantas son NA
summary(is.na(accidentes.dat))

## Sacamos los estadisticos de las variable numericas
summary(accidentes.dat[,18:23])
#summary(accidentes.dat[,c(1,18:23)]) # si las var numericas estan separadas
# Para la desviacion estandard
sapply(accidentes.dat[,18:23],sd)

# Histograma Numero de vehiculos
table(accidentes.dat$NO_OF_VEHICLES)
qs<-quantile(accidentes.dat$NO_OF_VEHICLES, prob=c(seq(0,1,0.1)), na.rm = TRUE )  ### Definimos nosotros el número de intervalos

accnum<-accidentes.dat$NO_OF_VEHICLES[accidentes.dat$NO_OF_VEHICLES<=5 & !is.nan(accidentes.dat$NO_OF_VEHICLES)]
par(mfrow=c(1,2))

hist(accnum, col='midnightblue', border="gray", labels=FALSE, main="Histograma", sub="",xlim =c(1,5), xlab="Número de Vehículos", ylab="Frecuencia absoluta",
     col.main="gray50",col.axis="gray50",col.lab="gray40",col.sub="gray40", freq = FALSE )
lines(density(accnum),col="red")
boxplot(accidentes.dat$NO_OF_VEHICLES, main="Boxplot",col.main="gray50",horizontal = TRUE)
boxplot(accnum, main="Boxplot",col.main="gray50",horizontal = TRUE)
mtext("Número de vehículos", outer = TRUE, cex = 1.5)
dev.off()
qqnorm(accidentes.dat$NO_OF_VEHICLES, col.main="gray50", col.axis="gray50")



#Numero de personas
pernum<-accidentes.dat$NO_PERSONS[accidentes.dat$NO_PERSONS<=10 & !is.nan(accidentes.dat$NO_PERSONS)]
par(mfrow=c(1,2))
hist(pernum, col='midnightblue', border="gray", labels=true, main="Histograma", sub="", xlab="", ylim = c(0,3), ylab="Frecuencia absoluta",
     col.main="gray50",col.axis="gray40",col.lab="gray40",col.sub="gray40", freq = FALSE )
lines(density(pernum),col="red")
boxplot(pernum, main="Boxplot",col.main="gray50",horizontal = TRUE)

par(mfrow=c(1,2))
hist(pernum, col='midnightblue', border="gray", labels=FALSE, main="Histograma", sub="",xlim =c(1,5), xlab="",ylim=c(0,1.5), ylab="",
     col.main="gray50",col.axis="gray40",col.lab="gray40",col.sub="gray40", freq = FALSE )
lines(density(pernum),col="red")
boxplot(pernum, main="Boxplot",col.main="red",horizontal = TRUE)

# Num de personas Lesionas Inj 2
per1<-accidentes.dat$NO_PERSONS_INJ_2[accidentes.dat$NO_PERSONS_INJ_2<=5 & !is.nan(accidentes.dat$NO_PERSONS_INJ_2)]
hist(per1,col='midnightblue', border="gray", labels=FALSE, main="", sub="",xlim =c(1,5), xlab="", ylab="Frecuencia absoluta",
     col.main="gray50",col.axis="gray40",col.lab="gray40",col.sub="gray40", freq = FALSE )
lines(density(per1),col="red")
boxplot(per1, main="Boxplot",col.main="gray50",horizontal = TRUE)
# Num de personas Inj 3
per2<-accidentes.dat$NO_PERSONS_INJ_3[accidentes.dat$NO_PERSONS_INJ_3<=10 & !is.nan(accidentes.dat$NO_PERSONS_INJ_2)]
hist(per2,col='midnightblue', border="gray", labels=FALSE, main="", sub="",xlim =c(1,5), xlab="", ylab="Frecuencia absoluta",
     col.main="gray50",col.axis="gray40",col.lab="gray40",col.sub="gray40", freq = FALSE )
lines(density(per2),col="red")
boxplot(per2, main="Boxplot",col.main="gray50",horizontal = TRUE)

#Personas killed
perk<-accidentes.dat$NO_PERSONS_KILLED
hist(perk,col='midnightblue', border="gray", labels=TRUE, main="Histograma", sub="",xlim =c(1,3), xlab="", ylim = c(0,0.5), ylab="Frecuencia absoluta",
     col.main="gray50",col.axis="gray40",col.lab="gray40",col.sub="gray40", freq = FALSE )
lines(density(perk),col="red")
boxplot(perk, main="Boxplot",col.main="gray50",horizontal = TRUE)

#Not injured
perok<-accidentes.dat$NO_PERSONS_NOT_INJ[accidentes.dat$NO_PERSONS_NOT_INJ<=10 & !is.nan(accidentes.dat$NO_PERSONS_NOT_INJ)]
hist(perok,col='midnightblue', border="gray", labels=TRUE, main="Histograma", sub="", xlab="", ylab="Frecuencia absoluta",
     col.main="gray50",col.axis="gray40",col.lab="gray40",col.sub="gray40", freq = TRUE )
lines(density(perok),col="red")
boxplot(perok, main="Boxplot",col.main="gray50",horizontal = TRUE)


# Para la fecha Año
accDate <- as.Date(accidentes.dat$ACCIDENTDATE, "%d/%m/%Y")
hist(accDate, "years", freq = TRUE, format = "%Y",col='midnightblue', border="gray", labels=FALSE, main="Histograma", col.main='gray50', xlab = "", ylab='Frecuencia absoluta', col.axis="gray40")
prop.table(table(year(accDate)))
# Para la fecha Mes
barplot(prop.table(table(month(accDate))),col='midnightblue', names.arg = c('Enero','Febrero','Marzo','Abril','Mayo','Junio','Julio','Agosto','Septiembre','Octubre','Noviembre','Diciembre'))
prop.table(table(month(accDate)))

# Dia de la semana
dia<-prop.table(table(accidentes.dat$Day.Week.Description))
dia
barplot(dia, main = 'Día de la semana', col = 'midnightblue')

# Hora del accidente
dummy<- as.POSIXct(accidentes.dat$ACCIDENTTIME, format="%H.%M")
prop.table(table(hour(dummy)))
barplot(prop.table(table(hour(dummy))),col='midnightblue',names.arg = c('00am','1am','2am','3am','4am','5am','6am','7am','8am','9am','10am','11am','12pm','1pm','2pm','3pm','4pm','5pm','6pm','7pm','8pm','9pm','10pm','11pm'))

# Tipo de accidentes
dummy <- accidentes.dat$Accident.Type.Desc
prop.table(table(dummy))
pie(table(dummy),  col = hcl(seq(0, 1000, length = 9)))

# Severidad del accidente
dummy <- accidentes.dat$SEVERITY
prop.table(table(dummy))
pie(table(dummy),  col = hcl(seq(0, 1000, length = 9)),labels = c('Fatal','Grave (Hospitalización)','Otro','Sin lesionados'))

# Limite de velocidad
dummy <- accidentes.dat$SPEED_ZONE
prop.table(table(dummy))
pie(table(dummy),  col = hcl(seq(0, 1000, length = 13)))



# *************** Base LIMPIA *************
# Missings rellenados y Outliers quitados y con Variables de Respuesta
install.packages('PCAmixdata')
library(PCAmixdata)

accidentes.dat <- read.table(file = "C:/Users/Brenda Jimenez/Documents/Brenda/Mineria de Datos/Modulo 5/Proyecto/Choques2_VF.csv",
                             sep = ",",header = T)

dim(accidentes.dat)
attach(accidentes.dat)
colnames(accidentes.dat)

#accidentes.dat$Colision_auto<-factor(accidentes.dat$Colision_auto)
#levels(accidentes.dat$Colision_auto)<-c('Yes','No')
accidentes.dat

summary(accidentes.dat)

#Solo las variables categoricas
accidentes.categoricas<-accidentes.dat[,-c(7)]
summary(accidentes.categoricas)
write.csv(accidentes.categoricas, file="C:/Users/Brenda Jimenez/Documents/Brenda/Mineria de Datos/Modulo 5/Proyecto/Choques2_Cat.csv", quote = FALSE, row.names = FALSE)
# Muestra de la base de solo categoricas
train<-sample(1:350650,100000)
accidentesCat.Sample <-accidentes.categoricas[train,] 
dim(accidentesCat.Sample)
write.csv(accidentesCat.Sample, file="C:/Users/Brenda Jimenez/Documents/Brenda/Mineria de Datos/Modulo 5/Proyecto/Choques2_Cat30%.csv", quote = FALSE, row.names = FALSE)

#install.packages('sampling')
library(sampling)
#tomamos una muestra
train<-sample(1:350650,100000)
accidentes.Sample <-accidentes.dat[train,] 
dim(accidentes.Sample)

#
#library(splitstackshape)
#accidentes.Sample<-stratified(accidentes.dat[1:23],"Severidad" , size = 150000)

summary(accidentes.Sample)
head(accidentes.Sample)

write.csv(accidentes.Sample, file="C:/Users/Brenda Jimenez/Documents/Brenda/Mineria de Datos/Modulo 5/Proyecto/Choques2_Sample30%.csv", quote = FALSE, row.names = FALSE)

#--------------------------------------------   Samples
# Variables cuantitativas
data.quanti <- accidentes.dat[,c(8:13,17:18,24:26,33)]
summary(data.quanti)

# Variables cualiatvas
data.quali <- accidentes.dat[,-c(1:3,6,8:15,17:19,21:22,24:33,36,38:39,41)]
summary(data.quali)
# convertimos a variables categoricas laas que reconoce inicialmente como numericas
#data.quali$POLICE_ATTEND<-factor(data.quali$POLICE_ATTEND)
#levels(data.quali$POLICE_ATTEND)
#levels(data.quali$POLICE_ATTEND)<-c('Yes','No','UnK')
data.quali$SEVERITY<-factor(data.quali$SEVERITY)
levels(data.quali$SEVERITY)
levels(data.quali$SEVERITY)<-c('Fatal', 'Serious','Other','No Injury')
data.quali$HELMET_BELT_WORN<-factor(data.quali$HELMET_BELT_WORN)
levels(data.quali$HELMET_BELT_WORN)<-c('Seatbelt worn', 'Seatbelt not worn','Child restraint worn', 'Child restraint not worn','Seatbelt/restraint not fitted','Crash helmet worn','Crash helmet not worn','Not appropriate','UNK' )

summary(data.quali)

dim(data.quali)
dim(data.quanti)

# Componentes principales para datos quali y quanti
pca<-PCAmix(data.quanti, data.quali, ndim=5, rename.level = TRUE, graph=TRUE)

pca$eig
pca$quali


#Component map with factor scores of the numerical columns
plot(pca,choice="cor")
#Scores of the levels of the categorical variables
plot(pca,choice="levels")
#contributions of the variables
plot(pca,choice="sqload",coloring.var=TRUE)



# Arbol de decision
library(rattle)
rattle()


