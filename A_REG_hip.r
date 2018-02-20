library(readxl)
hipertension <- read_excel("~/R/FMwd/hipertension.xlsx")
View(hipertension)

tratamiento<- hipertension$Tratamiento
antes<-hipertension$Antes
despues<-hipertension$Despues

diferencia<- antes-despues
hipertension<-data.frame(tratamiento, diferencia)
hipertension$tratamiento <- as.integer(hipertension$tratamiento)
for (i in 1:10) {
  if(hipertension[i,1]==2){hipertension[i,1]=1}else{hipertension[i,1]=0}
}
fit<-lm(diferencia ~ 1+tratamiento, data=hipertension)
summary(fit)
#######################
# Pregunta C
alpha<-0.01
n<-length(diferencia)
n
LI<-coef(fit)-qt(1-alpha/2, n-2)*sqrt(diag(vcov(fit)))
LS<-coef(fit)+qt(1-alpha/2, n-2)*sqrt(diag(vcov(fit)))
cbind(LI, LS)
#####################
# Pregunta E
alph<-0.05
T<-qt(1-alph/2, n-2)
t<-coef(fit)[2]/sqrt(vcov(fit)[2,2])
t
#####################
# Pregunta F
alp<-0.1
x<-1
sigma<-sum(fit$residuals^2)/(n-2)
mu<- coef(fit)[1]+coef(fit)[2]*x
var<- sigma + vcov(fit)[1,1]+x*x*vcov(fit)[2,2]+2*x*vcov(fit)[1,2]
LI <- mu - qt(1-alp/2,n-2)*sqrt(var)
LS <- mu + qt(1-alp/2,n-2)*sqrt(var)
cbind(LI, LS)
