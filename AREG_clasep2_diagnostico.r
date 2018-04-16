library(MASS)
library(zoo)
library(lmtest)
library(spam)
library(fields)
library(readxl)
cigarrillos <- read_excel("~/R/FMwd/cigarrillos.xlsx")
attach(cigarrillos)
s<-cigarrillos$state
a<- cigarrillos$age
h<-cigarrillos$hs
i<-cigarrillos$income
b<- cigarrillos$black
f<-cigarrillos$female
p<-cigarrillos$price
v<-cigarrillos$sales

#Modelo
fit<-lm(formula=v~1+a+h+i+b+f+p, data=cigarrillos)
summary(fit)

###Ajuste de modelos###

#Ajuste con 1 variable
fit1<-ajuste.normal(fit,1)
#2 variables
fit2<-ajuste.normal(fit,2)
ajuste.normal(fit,3)
ajuste.normal(fit,4)
ajuste.normal(fit,5)
ajuste.normal(fit,6)
#Candidatos a mejor modelo
fits<-lm(formula=v~-1+a+h+i+b+f+p, data=cigarrillos)
fitadec<-lm(formula=v~1+h+i+b+f+p, data=cigarrillos)
summary(fitadec)
fita4<-lm(formula=v~1+h+i+b+p, data=cigarrillos)
summary(fita4)

###############################ANÁLISIS DE DIAGNÓSTICO##########################

#####Leverage#####

leverage<-Leverage.normal(fit,4, "")
#los puntos obtenidos fueron: 9,2,10,45

#####Extremos#####

extremos<-Residuos.normal(fit,4,"")
#los puntos obtenidos fueron: 2, 8, 10, 24
#puntos por encima de 2: 2,8,24
#puntos por debajo de -2: 10
#El 98% de los datos están entre -2 y 2

#####Influyentes#####

qqplot<-qqplot.normal(fit, 100,0.01,5,"")
#los puntos fuera de las bandas son: 28,29,10,34,8
# Dado que son varios los puntos que están fuera de las bandas,
#es posible que se esté violando el supuesto de normalidad

influencia<-Influence.normal(fit,7,1,3,"")
#los punto con mayor distancia de cook es 2, luego 9 y 12.

#Procedemos a calcular el modelo sin los posibles puntos influyentes.
bptest(fit)
# valor p: 0.048, a un n.s del 5% se rechaza H0
dwtest(fit)
# valor p: 0.83, como es muy grande, la decisión en no rechazar H0
fitsi<-lm(formula=v~1+h+i+b+p, data=cigarrillos, subset=-c(2,9,12))
summary(fitsi)
summary(fita4)
