hipertension <- read.table("C:/Users/portatil 25/Documents/hipertension.txt", header=T)
tratamiento<- hipertension$Tratamiento
antes<-hipertension$Antes
despues<-hipertension$Despues
diferencia<- antes-despues
hipertensio<-data.frame(tratamientpo, diferencia)
fit<-lm(diferencia)
