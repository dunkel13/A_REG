# Ejemplo en clase, altura(mts) como variable explicativa del peso en niñas
y<- c(34,34,29,27,40,25,40,34,46,42,47,59)
x<- c(1.35,1.35,1.35,1.35,1.4,1.4,1.4,1.4,1.5,1.5,1.5,1.5)
d<- data.frame(x,y)
colnames(d)<-c("altura (mts)", "peso (kg)")
fit<-lm(y~1+x, data=d)
coef(fit)
# (Intercept)           x 
#   -131.4107    119.6429 
