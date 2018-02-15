Pacientes <- read.table("C:/Users/portatil 25/Documents/frecuencia.txt", header=T)
frecuencia<- Pacientes$Frecuencia
peso <- Pacientes$Peso
pacientes <- data.frame(frecuencia, peso)
pacientes
##############################################################################
fit<-lm(frecuencia ~ 1 + peso, data=pacientes)
summary(fit)
# con una persona con 0 kg, los latidos en reposo promedio sería de 68.24 ñatidos
# \beta_{2} por cada kg adicional en el peso corporal ,la frecuencia promedio de reposo va aumentar o.76 latidos
# p-valor es muy pequeño
# entre más pequeño bel \sigma el modelo tiene menor variabilidad, y da a entender la distancia de un individuo y el promedio.
# parametro de localización \beta_{1} y \beta{2}
# formula intervalo de confianza: $\beta_{1}+/- t_{\alpha/2}$
alpha<- 0.05
n <- length(frecuencia)
# toma los valores en coef 
LI <- coef(fit)-qt(1-alpha/2,n-2)*sqrt(diag(vcov(fit)))
LS <- coef(fit)+qt(1-alpha/2,n-2)*sqrt(diag(vcov(fit)))
cbind(LI, LS)
# con un 95% de confianza podemos afirmar que, por cada kg adicional en el peso corporal la frecuencia cardiaca en reposo promedio está entre 0.71 y 0.81 latidos
##############################################################################
# pregunta D: mirar la significancia de \beta_{2}
# H0: \beta_{2}= 0 vs H1: \beta_{2} != 0 \alpha=0.05
# \gamma: Rechazar H0 a un nivel de significancia 10*\alpha%
# abs(T)>t_{1-\alpha/2}(n-2) = t_{0.975}(97)=1.9847
##############################################################################
# pregunta E: prueba de hipótesis
# T>t_{1-\alpha}(n-2)=1.667
qt(0.95, 97)
# T= \betae_{2}-\beta_{2}/sqrt(vare(\beta_{2}))=2.4
 0.7629496-0.7/sqrt(vcov(fit)[2,2])
##############################################################################
# pregunta F: intervalo para la media
alpha<- 0.1
n
x<-70
mu <- coef(fit)[1]+coef(fit)[2]*x
var<- vcov(fit)[1,1]+x*x*vcov(fit)[2,2]+2*x*vcov(fit)[1,2]
LI<- mu - qt(1-alpha/2, n-2)*sqrt(var)
LS<- mu + qt(1-alpha/2, n-2)*sqrt(var)
names(LI)
names(LS)
cbind(LI, LS)
# con un 90% de confianza podemos afirmar que, la persona con 70 kgs de peso tienen en promedio una frecuencia cardiaca en reposo entre 120.82 y 122.48 latidos.
##############################################################################
# pregunta G:
alpha<-0.01
x<-85
sigma<-sum(fit$residuals^2)/(n-2)
mu<- coef(fit)[1]+coef(fit)[2]*x
var<- sigma + vcov(fit)[1,1]+x*x*vcov(fit)[2,2]+2*x*vcov(fit)[1,2]
sqrt(sigma)
LI <- mu - qt(1-alpha/2,n-2)*sqrt(var)
LS <- mu + qt(1-alpha/2,n-2)*sqrt(var)
cbind(LI, LS)
