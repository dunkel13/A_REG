######## EJERCICIO EN CLASE 3 ########
######## CODIGO R             ########


######## Cargando los paquetes ########
library(MASS)

###################################################### EJERCICIO 1 #####################################################
########### Lectura de los datos ########
bebes <- read.table("C:\\LuzMarina\\2016-I\\Regresion\\Datos\\bebes.txt", header=T)
edad <- bebes$edad
toxemia <- bebes$toxemia
circ <- bebes$circ
bebes <- data.frame(edad,toxemia,circ)

########### Gráfico de los datos ########
mine <- min(edad)
maxe <- max(edad)
minc <- min(circ)
maxc <- max(circ)
plot(edad[toxemia==0],circ[toxemia==0], xlim=c(mine,maxe),
ylim=c(minc,maxc), cex=0.2, lwd=3, col="blue", xlab="", ylab="")
par(new=T)
plot(edad[toxemia==1],circ[toxemia==1], xlim=c(mine,maxe),
ylim=c(minc,maxc), cex=0.2, lwd=3, col="red", xlab="Edad gestacional (en semanas)",
ylab="Circunferencia de cabeza (en centimetros)")
legend(23,37,c("Ausencia de Toxemia en la madre","Presencia de Toxemia en la madre"), bty="n", col=c("blue","red"), lty=c(1,1))

######## Estimación del modelo de regresión ########
fit <- lm(circ ~ 1+edad+toxemia, data=bebes)
summary(fit)


########### Gráfico del modelo estimado ########
mine <- min(edad)
maxe <- max(edad)
minc <- min(circ)
maxc <- max(circ)
plot(edad[toxemia==0],circ[toxemia==0], xlim=c(mine,maxe),
ylim=c(minc,maxc), cex=0.2, lwd=3, col="blue", xlab="", ylab="")
par(new=T)
plot(edad[toxemia==1],circ[toxemia==1], xlim=c(mine,maxe),
ylim=c(minc,maxc), cex=0.2, lwd=3, col="red", xlab="", ylab="")
par(new=T)
plot(edad[toxemia==0],fitted(fit)[toxemia==0], xlim=c(mine,maxe),
ylim=c(minc,maxc), col="blue", xlab="", ylab="", type="l")
par(new=T)
plot(edad[toxemia==1],fitted(fit)[toxemia==1], xlim=c(mine,maxe),
ylim=c(minc,maxc), col="red", xlab="Edad gestacional (en semanas)",
ylab="Circunferencia de cabeza (en centimetros)", type="l")
legend(23,37,c("Ausencia de Toxemia en la madre",
"Presencia de Toxemia en la madre"), bty="n", col=c("blue","red"), lty=c(1,1))


########### Comparación de modelos ########
fit2 <- lm(circ ~ 1+edad+toxemia+edad*toxemia, data=bebes)
anova(fit, fit2, test="F")



###################################################### EJERCICIO 2 #####################################################
########### Lectura de los datos ########
estudiantes <- read.table("C:\\LuzMarina\\2016-I\\Regresion\\Datos\\ipb.txt", header=T)
ipb <- estudiantes$ipb
grupo <- estudiantes$grupo
sexo <- estudiantes$sexo
estudiantes <- data.frame(ipb,grupo,sexo)

########### Estadísticas de resumen ########
library(spam)
library(fields)

stats(ipb, by=grupo)
stats(ipb, by=sexo)

########### Gráfico de los datos ########
mini <- min(ipb)
maxi <- max(ipb)
par(xaxt="n")
plot(as.integer(grupo)[sexo=="F"], ipb[sexo=="F"], xlab=" ", ylim=c(mini,maxi), xlim=c(0.5,length(levels(grupo))+0.5), ylab=" ", cex=0.2, lwd=3, col="blue")
par(new=T)
plot(as.integer(grupo)[sexo=="M"], ipb[sexo=="M"], xlab=" ", ylim=c(mini,maxi), xlim=c(0.5,length(levels(grupo))+0.5), ylab="Índice de Placa Bacteriana ", cex=0.2, lwd=3, col="red")
lab <- c("Grupo 1","Grupo 2","Grupo 3")
par(xaxt="s")
axis(side=1, at=seq(1,length(levels(grupo)),by=1), label=lab)
legend(0.5,5,c("Femenino","Masculino"), bty="n", col=c("blue","red"), lty=c(1,1))

######## Estimación del modelo de regresión ########
fit <- lm(ipb ~ 1+grupo+sexo, data=estudiantes)
summary(fit)

########### Comparación de modelos ########
fit2 <- lm(ipb ~ 1+sexo, data=estudiantes)
anova(fit2, fit, test="F")

########### Comparación de modelos ########
fit3 <- lm(ipb ~ 1+grupo+sexo+grupo*sexo, data=estudiantes)
anova(fit, fit3, test="F")


###################################################### EJERCICIO 3 #####################################################
##### USAR FUNCIONES ####


######## Cargando los paquetes ########
library(MASS)
library(zoo)
library(lmtest)
####### Lectura de los datos ########
atletas <- read.table("C:\\LuzMarina\\2016-I\\Regresion\\Datos\\atletas.txt", header=T, dec=",")


######## Estimación del modelo de regresión ########
fit <- lm(lbm ~ 1+ht+wt+rcc, data=atletas)
summary(fit)

######## Correlaciones simples ########
Correlaciones(fit,3,1,4,"")

######## Correlaciones parciales ########
Correlaciones.parcial(fit,3,1,1,"")

######## Búsqueda del "mejor" modelo ########
ajuste.normal(fit,1)
ajuste.normal(fit,2)

######## Estimación del modelo de regresión ########
fit <- lm(lbm ~ -1+ht+wt+rcc, data=atletas)
summary(fit)

