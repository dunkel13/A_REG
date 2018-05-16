## Regresión logística ###
cangrejos <- read.table("cangrejos.txt", header=TRUE, dec=",")
color<-factor(cangrejos$C)
scol<-factor(cangrejos$S)
capa<-cangrejos$W
y<-cangrejos$Y
cangrejos<-data.frame(color, scol, capa,y)
fit<-glm(formula=y~1+color+scol+capa, family=binomial, data=cangrejos)
summary(fit)
fit2<-glm(formula=y~1+color+scol+capa+color*capa+scol*capa+color*scol, family=binomial, data=cangrejos)
library(MASS)
stepAIC(fit2)
# el modelo con menor AIC es el que la v expl. son color y capa
fita<-glm(formula=y~1+color+capa, family=binomial, data=cangrejos)
fitb<-glm(formula=y~1+scol+capa, family=binomial, data=cangrejos)
summary(fita)
# Al hacer el summary el color resulta no significativo
fitm<-glm(formula=y~1+capa, family=binomial, data=cangrejos)
summary(fitm)
exp(fitm$coefficients[1])
# razón de chances, a medida que aumenta un cm en el ancho de caapa la prob de tener un sat es  
# mayor en un 64% que uno con un cm menos
exp(fitm$coefficients[2])
### punto C ###
# Estadístico de prueba
z=(fitm$coefficients[2]-0)/sqrt(vcov(fitm)[2,2])
# regla de decisión: rechazo H0 a un ns de 100a% 
#### punto D ###
anova(fitm,fita, test="Chisq")
# No se rechaza H0, luego el color no está siendo significativo
### punto E ###
anova(fitm,fitb, test="Chisq")
# No se rechaza H0, luego el scol no está siendo significativo
### punto F ###
#Estimación de una probabilidad
x<-c(1,26)
eta<-sum(x*coef(fitm))
prob<-(exp(eta)/(1+exp(eta)))
prob     
x1<-c(1,27)
eta1<-sum(x1*coef(fitm))
prob1<-(exp(eta1)/(1+exp(eta1)))
prob1
### punto G ###
# Intervalo de confianza
LS<-fitm$coefficients[2]+qnorm(0.975)*sqrt(vcov(fitm)[2,2])
LI<-fitm$coefficients[2]-qnorm(0.975)*sqrt(vcov(fitm)[2,2])
# Intervalo de confianza para la razón de chances, con un confianza del 95% por cada cm en capa
# el chance de tener satelites aumenta entre 34,7% al 100
c(exp(LI), exp(LS))
### punto I ###
# Diagnóstico
residuos<-Residuos.binomial(fitm, 3,"")
qqplot.binomial(fitm, 100, 0.01,3,"")
Leverage.binomial(fitm, 4, "")
rm(list=ls())
