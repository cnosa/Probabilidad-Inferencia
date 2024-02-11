################################################################################
#                                                                              #
# #                    Proyecto Parte A: Apartado C.                          ##
#                                                                              #
################################################################################


#Punto a:
#
# Para realizar las m simulaciones, primeramente se le asigna a la variable "data1" una muestra aleatoria generada 
# por "rdexp" y acudiendo al teorema que dice que simular l valores de la función de distribución empírica es 
# equivalente a remuestrear l elementos con reemplazo, se hace uso de la función ya integrada "sample" y dado que 
# se requieren 1000 simulaciones, se utiliza "replicate" que permite replicar el argumento tantas veces como se 
# le pida (en este caso 1000 veces). 
#
n=10
data1<-rdexp(n,3,1)
muestras=replicate(1000,{muestra=sample(data1,n,replace = TRUE)})
#
# Para calcular las estimaciones máximo verosímil y las estimaciones por momentos, se crean los vectores con 
# entradas numéricas "MLE" y "MOM" respectivamente y mediante el "for" se les va asignando a cada entrada la 
# correspondiente estimación de cada una de las 1000 muestras de tamaño 10.
#
MLE <- numeric(1000)
for(i in 1:1000){MLE[i]<-median(muestras[,i])}
MOM <- numeric(1000)
for(i in 1:1000){MOM[i]<-mean(muestras[,i])}
#
#Histograma MLE
hist(MLE,prob=TRUE,xlim=c(min(MLE),max(MLE)),xlab="Estimación ML",ylab="Densidad",main="Histograma de la estimación ML en una simulación de Dob_Exp")
xpts<-seq(min(MLE),max(MLE),by=0.001)
abline(v=3,col="red",lwd=3)
abline(v=mean(MLE),col="blue",lwd=3)
legend(locator(1), c("Verdadero valor","Prom. de m estimaciones"), col=c(2,4),lty=c(1,1),lwd=c(2,2))
#
#Histograma MOM
hist(MOM,prob=TRUE,xlim=c(2.5,max(MOM)),xlab="Estimación MOM",ylab="Densidad",main="Histograma de la estimación de momentos en una simulación de Dob_Exp")
xpts<-seq(min(MOM),max(MOM),by=0.001)
abline(v=3,col="red",lwd=2)
abline(v=mean(MOM),col="blue",lwd=3)
legend(locator(1), c("Verdadero valor","Prom. de m estimaciones"), col=c(2,4),lty=c(1,1),lwd=c(2,2))
#
# Para obtener la tabla con las varianzas y medias de las estimaciones MLE y MOM se almacena la información en la 
# matriz A:
A<-c(var(MLE), var(MOM), mean(MLE), mean(MOM))
dim(A)<-c(2,2)
colnames(A)<-c("Varianza","Promedio")
rownames(A)<-c("MLE","MOM")
#MSE
mseMLE1=(mean(MLE)-3)^2+var(MLE)
mseMOM1=(mean(MOM)-3)^2+var(MOM)
#
#Punto b:
# Para la realización de este apartado se usa el mismo código usado en a) variando en cada caso el valor de n:
#n=50:
n=50
data2<-rdexp(n,3,1)
muestras2=replicate(1000,{muestra=sample(data2,n,replace = TRUE)})
MLE50 <- numeric(1000)
for(i in 1:1000){MLE50[i]<-median(muestras2[,i])}
MOM50 <- numeric(1000)
for(i in 1:1000){MOM50[i]<-mean(muestras2[,i])}
#Histograma MLE50
hist(MLE50,prob=TRUE,xlim=c(min(MLE50),max(MLE50)),xlab="Estimación ML",ylab="Densidad",main="Histograma de la estimación ML en una simulación de Dob_Exp") 
xpts<-seq(min(MLE50),max(MLE50),by=0.001)
abline(v=3,col="red",lwd=3)
abline(v=mean(MLE50),col="blue",lwd=3)
legend(locator(1), c("Verdadero valor","Prom. de m estimaciones"), col=c(2,4),lty=c(1,1),lwd=c(2,2))
#Histograma MOM50
hist(MOM50,prob=TRUE,xlim=c(min(MOM50),max(MOM50)),xlab="Estimación MOM",ylab="Densidad",main="Histograma de la estimación de momentos en una simulación de Dob_Exp") 
xpts<-seq(min(MOM50),max(MOM50),by=0.001)
abline(v=3,col="red",lwd=2)
abline(v=mean(MOM50),col="blue",lwd=3)
legend(locator(1), c("Verdadero valor","Prom. de m estimaciones"), col=c(2,4),lty=c(1,1),lwd=c(2,2))
#Varianza y media de las m estimaciones
A2<-c(var(MLE50), var(MOM50), mean(MLE50), mean(MOM50))
dim(A2)<-c(2,2)
colnames(A2)<-c("Varianza","Promedio")
rownames(A2)<-c("MLE","MOM")
#MSE
mseMLE50=(mean(MLE50)-3)^2+var(MLE50)
mseMOM50=(mean(MOM50)-3)^2+var(MOM50)
#
#n=100:
n=100
data3<-rdexp(n,3,1)
muestras3=replicate(1000,{muestra=sample(data3,n,replace = TRUE)})
MLE100 <- numeric(1000)
for(i in 1:1000){MLE100[i]<-median(muestras3[,i])}
MOM100 <- numeric(1000)
for(i in 1:1000){MOM100[i]<-mean(muestras3[,i])}
#Histograma MLE100
hist(MLE100,prob=TRUE,xlim=c(min(MLE100),max(MLE100)),xlab="Estimación ML",ylab="Densidad",main="Histograma de la estimación ML en una simulación de Dob_Exp") 
xpts<-seq(min(MLE100),max(MLE100),by=0.001)
abline(v=3,col="red",lwd=3)
abline(v=mean(MLE100),col="blue",lwd=3)
legend(locator(1), c("Verdadero valor","Prom. de m estimaciones"), col=c(2,4),lty=c(1,1),lwd=c(2,2))
#Histograma MOM100
hist(MOM100,prob=TRUE,xlim=c(min(MOM100),max(MOM100)),xlab="Estimación MOM",ylab="Densidad",main="Histograma de la estimación de momentos en una simulación de Dob_Exp") 
xpts<-seq(min(MOM100),max(MOM100),by=0.001)
abline(v=3,col="red",lwd=2)
abline(v=mean(MOM100),col="blue",lwd=3)
legend(locator(1), c("Verdadero valor","Prom. de m estimaciones"), col=c(2,4),lty=c(1,1),lwd=c(2,2))
#Varianza y media de las m estimaciones
A3<-c(var(MLE100), var(MOM100), mean(MLE100), mean(MOM100))
dim(A3)<-c(2,2)
colnames(A3)<-c("Varianza","Promedio")
rownames(A3)<-c("MLE","MOM")
#MSE
mseMLE100=(mean(MLE100)-3)^2+var(MLE100)
mseMOM100=(mean(MOM100)-3)^2+var(MOM100)
#
#n=200:
n=200
data4<-rdexp(n,3,1)
muestras4=replicate(1000,{muestra=sample(data4,n,replace = TRUE)})
MLE200 <- numeric(1000)
for(i in 1:1000){MLE200[i]<-median(muestras4[,i])}
MOM200 <- numeric(1000)
for(i in 1:1000){MOM200[i]<-mean(muestras4[,i])}
#Histograma MLE200
hist(MLE200,prob=TRUE,xlim=c(min(MLE200),max(MLE200)),xlab="Estimación ML",ylab="Densidad",main="Histograma de la estimación ML en una simulación de Dob_Exp") 
xpts<-seq(min(MLE200),max(MLE200),by=0.001)
abline(v=3,col="red",lwd=3)
abline(v=mean(MLE200),col="blue",lwd=3)
legend(locator(1), c("Verdadero valor","Prom. de m estimaciones"), col=c(2,4),lty=c(1,1),lwd=c(2,2))
#Histograma MOM
hist(MOM200,prob=TRUE,xlim=c(min(MOM200),max(MOM200)),xlab="Estimación MOM",ylab="Densidad",main="Histograma de la estimación de momentos en una simulación de Dob_Exp") 
xpts<-seq(min(MOM200),max(MOM200),by=0.001)
abline(v=3,col="red",lwd=2)
abline(v=mean(MOM200),col="blue",lwd=3)
legend(locator(1), c("Verdadero valor","Prom. de m estimaciones"), col=c(2,4),lty=c(1,1),lwd=c(2,2))
#Varianza y media de las m estimaciones
A4<-c(var(MLE200), var(MOM200), mean(MLE200), mean(MOM200))
dim(A4)<-c(2,2)
colnames(A4)<-c("Varianza","Promedio")
rownames(A4)<-c("MLE","MOM")
#MSE
mseMLE200=(mean(MLE200)-3)^2+var(MLE200)
mseMOM200=(mean(MOM200)-3)^2+var(MOM200)
#
#n=500:
n=500
data5<-rdexp(n,3,1)
muestras5=replicate(1000,{muestra=sample(data5,n,replace = TRUE)})
MLE500 <- numeric(1000)
for(i in 1:1000){MLE500[i]<-median(muestras5[,i])}
MOM500 <- numeric(1000)
for(i in 1:1000){MOM500[i]<-mean(muestras5[,i])}
#Histograma MLE500
hist(MLE500,prob=TRUE,xlim=c(min(MLE500),max(MLE500)),xlab="Estimación ML",ylab="Densidad",main="Histograma de la estimación ML en una simulación de Dob_Exp")
xpts<-seq(min(MLE500),max(MLE500),by=0.001)
abline(v=3,col="red",lwd=3)
abline(v=mean(MLE500),col="blue",lwd=3)
legend(locator(1), c("Verdadero valor","Prom. de m estimaciones"), col=c(2,4),lty=c(1,1),lwd=c(2,2))
#Histograma MOM500
hist(MOM500,prob=TRUE,xlim=c(min(MOM500),max(MOM500)),xlab="Estimación MOM",ylab="Densidad",main="Histograma de la estimación de momentos en una simulación de Dob_Exp")
xpts<-seq(min(MOM500),max(MOM500),by=0.001)
abline(v=3,col="red",lwd=2)
abline(v=mean(MOM500),col="blue",lwd=3)
legend(locator(1), c("Verdadero valor","Prom. de m estimaciones"), col=c(2,4),lty=c(1,1),lwd=c(2,2))
#Varianza y media de las m estimaciones
A5<-c(var(MLE500), var(MOM500), mean(MLE500), mean(MOM500))
dim(A5)<-c(2,2)
colnames(A5)<-c("Varianza","Promedio")
rownames(A5)<-c("MLE","MOM")
#MSE
mseMLE500=(mean(MLE500)-3)^2+var(MLE500)
mseMOM500=(mean(MOM500)-3)^2+var(MOM500)
#
#n=1000:
n=1000
data6<-rdexp(n,3,1)
muestras6=replicate(1000,{muestra=sample(data6,n,replace = TRUE)})
MLE1000 <- numeric(1000)
for(i in 1:1000){MLE1000[i]<-median(muestras6[,i])}
MOM1000 <- numeric(1000)
for(i in 1:1000){MOM1000[i]<-mean(muestras6[,i])}
#Histograma MLE1000
hist(MLE1000,prob=TRUE,xlim=c(min(MLE1000),max(MLE1000)),xlab="Estimación ML",ylab="Densidad",main="Histograma de la estimación ML en una simulación de Dob_Exp") 
xpts<-seq(min(MLE1000),max(MLE1000),by=0.001)
abline(v=3,col="red",lwd=3)
abline(v=mean(MLE1000),col="blue",lwd=3)
legend(locator(1), c("Verdadero valor","Prom. de m estimaciones"), col=c(2,4),lty=c(1,1),lwd=c(2,2))
#Histograma MOM1000
hist(MOM1000,prob=TRUE,xlim=c(min(MOM1000),max(MOM1000)),xlab="Estimación MOM",ylab="Densidad",main="Histograma de la estimación de momentos en una simulación de Dob_Exp")
xpts<-seq(min(MOM1000),max(MOM1000),by=0.001)
abline(v=3,col="red",lwd=2)
abline(v=mean(MOM1000),col="blue",lwd=3)
legend(locator(1), c("Verdadero valor","Prom. de m estimaciones"), col=c(2,4),lty=c(1,1),lwd=c(2,2))
#Varianza y media de las m estimaciones
A6<-c(var(MLE1000), var(MOM1000), mean(MLE1000), mean(MOM1000))
dim(A6)<-c(2,2)
colnames(A6)<-c("Varianza","Promedio")
rownames(A6)<-c("MLE","MOM")
#MSE
mseMLE1000=(mean(MLE1000)-3)^2+var(MLE1000)
mseMOM1000=(mean(MOM1000)-3)^2+var(MOM1000)
#
#Punto c:
#
#Percentil asociado a 0.5
e_0.5=qdexp(0.5,3,1)
#
#Punto d:
#
n=1000
data7<-rdexp(n,3,1)
muestras7=replicate(1000,{muestra=sample(data7,n,replace = TRUE)})
#
# Análogamente a la construcción del MLE y MOM se construyen Z1000 y M1000 asignandole a cada una su respectiva 
# definición.
Z1000<-numeric(1000)
for(i in 1:1000){Z1000[i]<-sqrt(1000)*(mean(muestras7[,i])-3)}
M1000<-numeric(1000)
for(i in 1:1000){M1000[i]<-sqrt(1000)*(median(muestras7[,i])-3)}
#Histograma Z1000
hist(Z1000,prob=TRUE,xlim=c(min(Z1000),max(Z1000)),xlab="Realizaciones Z1000",ylab="Densidad",main="Histograma de Z_1000 con datos simulados")
xpts<-seq(min(Z1000),max(Z1000),by=0.001)
lines(xpts,dnorm(xpts,0,2),col="red",lwd=2)
#Histograma M1000
hist(M1000,prob=TRUE,xlim=c(min(M1000),max(M1000)),ylim = c(0,1/sqrt(2*pi)),xlab="Realizaciones M1000",ylab="Densidad",main="Histograma de M_1000 con datos simulados")
xpts<-seq(min(M1000),max(M1000),by=0.001)
lines(xpts,dnorm(xpts,0,1),col="red",lwd=2)
#
#Punto e (Bonus):
#Para la construcción de las diferentes modas se exploran diferentes métodos para hallar dicho valor, adicionalmente
#se calculan los valores del promedio y varianza en cada caso para posteriormente comparar la estimación moda con las
#estimaciones ML y MOM
#
#Moda con n=10
Moda1 <- numeric(1000)
for(i in 1:1000){Moda1[i]<- mlv(muestras[,i], method = "naive")}
vm1=var(Moda1)
pm1=mean(Moda1)
mseM1=(pm1-3)^2+vm1
#Moda con n=50
Moda2<-numeric(1000)
for(i in 1:1000){Moda2[i]<-mlv(muestras2[,i], method = "meanshift")}
vm2=var(Moda2)
pm2=mean(Moda2)
mseM2=(pm2-3)^2+vm2
#Moda con n=100
Moda3 <- numeric(1000)
for(i in 1:1000){Moda3[i]<- mlv(muestras3[,i], method = "naive")}
vm3=var(Moda3)
pm3=mean(Moda3)
mseM3=(pm3-3)^2+vm3
#Moda con n=200
Moda4 <- numeric(1000)
for(i in 1:1000){Moda4[i]<- mlv(muestras4[,i], method = "venter")}
vm4=var(Moda4)
pm4=mean(Moda4)
mseM4=(pm4-3)^2+vm4
#Moda con n=500
Moda5<- numeric(1000)
for(i in 1:1000){Moda5[i]<- mlv(muestras5[,i], method = "hsm")}
vm5=var(Moda5)
pm5=mean(Moda5)
mseM5=(pm5-3)^2+vm5
#Moda con n=1000
Moda6<- numeric(1000)
for(i in 1:1000){Moda6[i]<-mlv(muestras6[,i], method = "meanshift")}
vm6=var(Moda6)
pm6=mean(Moda6)
mseM6=(pm6-3)^2+vm6
