# (a) Realizamos la muestra simulado de tamaño n=1000 de una distribución N(3,9)
x<-rnorm(n=1000, mean=3, sd=3)
#Calculamos la media y la varianza
meandata<-mean(x)
vardata<-var(x)

# (b) Calculamos y graficamos la función empírica de los datos:
emp<- ecdf(x)
xpts<- seq(min(x)-1, max(x)+1, by=0.01)
plot(xpts, emp(xpts), type="l", main= "Función de distribución empírica \n de los datos (n=1000)", xlab="x", ylab="F(x) empírica")
lines(xpts, pnorm(xpts, mean=3, sd=3), col="blue", lwd=2)

# (c) Olvidemos que conocemos la distribución real. Llamemos data a la muestra simulada
data<-x
#Realicemos muestras bootstrap, y guardémoslas como filas de una matriz:
mat<- matrix(nrow = 1000, ncol = 1000)
for(i in c(1:1000)){
  fila<- sample(data, 1000, replace=TRUE)
  mat[i,]=fila}
#Guardemos las estadísticas pedidas como componentes de vectores con tamaño n=1000:
media<- vector(length = 1000) #Media
expvar<- vector(length = 1000) #Expresión de la varianza {(n-1)s_i^2/var(data)}
cv<- vector(length = 1000) #Coeficiente de variación
for(i in c(1:1000)){
  media[i]<- mean(mat[i,])
  expvar[i]<- (1000-1)*var(mat[i,])/var(data)
  cv[i]<- sqrt(var(mat[i,]))/media[i] 
}
#Realicemos los histogramas correspondientes a cada estadística, y superpongamos su distribución teórica:
#Histograma de la media
pts<-seq(min(media)-0.5,max(media)+0.5, by=0.01)
hist(media, prob=TRUE, xlim=c(min(media)-0.5, max(media)+0.5), ylim=c(0,4.25), main="Histograma de la media (n=1000)", xlab="Media", ylab="Densidad")
lines(pts, dnorm(pts, mean=3, sd=sqrt(9/1000)), col="red", lwd=2)
#Histograma de la expresión de la varianza
pts<-seq(min(expvar)-0.5,max(expvar)+0.5, by=0.01)
hist(expvar, prob=TRUE, xlim=c(min(expvar)-0.5, max(expvar)+0.5), ylim=c(0,0.01),main="Histograma de la expresión de la varianza (n=1000)", xlab="Expresión de la varianza", ylab="Densidad") 
lines(pts, dchisq(pts, 1000-1), col="green", lwd=2)
#Histograma del coeficiente de variación:
hist(cv, prob=TRUE, main="Histograma del coeficiente de variación (n=1000)", xlab="Coeficiente de variación", ylab="Densidad")
#Añadimos ahora el promedio muestral, y el verdadero valor del parámetro:
abline(v=mean(cv), col="red", lwd=2)
abline(v=1, col="blue", lwd=2)
legend(locator(1), c("Media muestral", "Valor real del parámetro"), col=c("red", "blue"),lwd=c(2,2))