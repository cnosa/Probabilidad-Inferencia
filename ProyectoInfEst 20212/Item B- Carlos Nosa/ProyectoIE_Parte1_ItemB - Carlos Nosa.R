#Cálculo de una estimación máximo-verosimil cuando no hay una solución analítica
vr = 3 #Valor real del parámetro

##Ítem a.
{


MLE<-c() #Vector de valores estimados del MLE de las simulaciones
ME<-c() #Vector de valores estimados de la mediana de las simulaciones

ll_cauchy<-function(theta,x){#Función a optimizar con base a los datos generados(x)
  n<-length(x)
  n*log(pi)+ sum(log(1+(x-theta)^2)) #Negativo de la función de log-verosimilitud 
}



for(i in 1:1000){
  sim = rcauchy(n=10,location=vr,scale=1)
  #Simulación de una m.a. de tamaño 10
  
  mle = optim(mean(sim),ll_cauchy,method = "L-BFGS-B",x=sim)$par 
  #Minimización del negativo de la función de log-verosimilitud. Se seleccionó el método 
  #"L-BFGS-B" para poder usar una función bidimensional 
  
  me = median(sim)
  #Mediana de los datos generados
  
  MLE=append(MLE,mle,i)
  ME=append(ME,me,i)
}

#Histograma de las estimaciones por MLE
hist(MLE,breaks=30,probability = T,main="Histograma de las estimaciones por el estimador MLE",xlab="Estimaciones de MLE",
     ylab="Densidad")
abline(v=vr,col="red",lty=2,lwd=3)#Valor verdadero del parámetro
abline(v=mean(MLE),col="blue",lty=1,lwd=2)#Media de las estimaciones
legend("topleft", legend=c("Valor real del parámetro", "Promedio de las estimaciones"),
       col=c("red", "blue"), lty=c(2,1),lwd=c(3,2), cex=0.8,
       box.lty=0)

#Histograma de los valores de la mediana de las simulaciones
hist(ME,breaks=20,probability = T,main="Histograma de los valores de la mediana muestral
     (estimador por analogía)",xlab="Valores de la mediana de la muestra",
     ylab="Densidad",)
abline(v=vr,col="red",lty=2,lwd=3) #Valor verdadero del parámetro
abline(v=mean(ME),col="blue",lty=1,lwd=2) #Media de las estimaciones
legend("topleft", legend=c("Valor real del parámetro", "Promedio de las estimaciones"),
       col=c("red", "blue"), lty=c(2,1),lwd=c(3,2), cex=0.8,
       box.lty=0)

#Promedio y varianza de los estimadores
  #Estimador MLE
Promedio_MLE=mean(MLE)
Varianza_MLE=mean(MLE^2)-mean(MLE)^2
  #Mediana
Promedio_ME=mean(ME)
Varianza_ME=mean(ME^2)-mean(ME)^2

#Error cuadrático medio (MSE) de los estimadores
 #Estimador MLE
MSE_MLE = (Promedio_MLE-vr)^2 + Varianza_MLE
  #Mediana
MSE_ME = (Promedio_ME-vr)^2 + Varianza_ME

#Eficiencia relativa (Estimador MLE,Mediana)
RE = MSE_MLE/MSE_ME
}

##Ítem b.
{
tm_N<-function(N,m){
MLE<-c() #Vector de valores estimados del MLE de cada una de las simulaciones
ME<-c() #Vector de valores estimados de la mediana de cada una de las simulaciones

ll_cauchy<-function(theta,x){#Función a optimizar con base a los datos generados(x)
  n<-length(x)
  n*log(pi)+ sum(log(1+(x-theta)^2)) #Negativo de la función de log-verosimilitud 
} 

for(i in 1:m){
  sim = rcauchy(n=N,location=3,scale=1)
  #Simulación de una m.a. de tamaño 10  
  
  mle = optim(mean(sim),ll_cauchy,method = "L-BFGS-B",x=sim)$par 
  #Minimización del negativo de la función de log-verosimilitud. Se seleccionó el método 
  #"L-BFGS-B" para 
  
  me = median(sim)
  #Mediana de los datos generados
  
  MLE=append(MLE,mle,i)
  ME=append(ME,me,i)
}


par(mfrow=c(1, 2))
#Histograma de las estimaciones por MLE
hist(MLE,breaks=30,probability = T,main="Estimador MLE",xlab="Estimaciones de MLE",
     ylab="Densidad")
abline(v=vr,col="red",lty=2,lwd=3)#Valor verdadero del parámetro
abline(v=mean(MLE),col="blue",lty=1,lwd=2)#Media de las estimaciones
legend("bottomright",legend=c("V. real", "Media"),
       col=c("red", "blue"), lty=c(2,1),lwd=c(3,2), cex=0.8,
       box.lty=0)

#Histograma de los valores de la mediana de las simulaciones
hist(ME,breaks=20,probability = T,main="Mediana",xlab="Valores de la mediana de la muestra",
     ylab="Densidad",)
abline(v=vr,col="red",lty=2,lwd=3) #Valor verdadero del parámetro
abline(v=mean(ME),col="blue",lty=1,lwd=2) #Media de las estimaciones
legend("bottomright", legend=c("V. real", "Media"),
       col=c("red", "blue"), lty=c(2,1),lwd=c(3,2), cex=0.8,
       box.lty=0)


#Promedio y varianza de los estimadores
#Estimador MLE
Promedio_MLE=mean(MLE)
Varianza_MLE=mean(MLE^2)-mean(MLE)^2
#Mediana
Promedio_ME=mean(ME)
Varianza_ME=mean(ME^2)-mean(ME)^2

#Error cuadrático medio (MSE) de los estimadores
#Estimador MLE
B2_MLE = (Promedio_MLE-vr)^2
MSE_MLE = B2_MLE + Varianza_MLE
#Mediana
B2_ME = (Promedio_ME-vr)^2
MSE_ME =  B2_ME + Varianza_ME

#Eficiencia relativa (Estimador MLE,Mediana)
RE = MSE_MLE/MSE_ME


cat("\tEstimador MLE
    \nPromedio=", Promedio_MLE,"
    \nVarianza=", Varianza_MLE,"
    \nSesgo al cuadrado=",B2_MLE,"
    \nMSE=", MSE_MLE,"
    \n\tMediana
    \nPromedio=",Promedio_ME,"
    \nVarianza=",Varianza_ME,"
    \nSesgo al cuadrado=",B2_ME,"
    \nMSE=",MSE_ME,"
    \n\nEficiencia relativa(RE)=",RE)
} #Esta función retorna los histogramas del estimador MLE
#y de la Mediana, además, retorna algunos datos de resumen de estos estimadores 
#N corresponde al tamaño de la muestra y m al número de simulaciones

  
#Cálculo para los diferentes tamaños de muestra con m=1000 simulaciones
tm_N(50,1000)
tm_N(100,1000)
tm_N(200,1000)
tm_N(500,1000)  
tm_N(1000,1000)

}

##Ítem d.
{
VW<-function(N){
  sim = rcauchy(n=N,location=vr,scale=1)
  mle = optim(mean(sim),ll_cauchy,method = "L-BFGS-B",x=sim)$par
  me = median(sim)
  c(sqrt(N)*(mle-vr),sqrt(N)*(me-vr))
} #Esta función retorna realizaciones de V_N y W_N

V_1000<-c()
W_1000<-c()

for(i in 1:1000){
  V = VW(1000)[1]
  W = VW(1000)[2]
  V_1000=append(V_1000,V,i)
  W_1000=append(W_1000,W,i)
} #m simulaciones

#Histograma de V
hist(V_1000,breaks=20,probability=T,main="Histograma de V de tamaño 1000",xlab="V",ylab="Densidad")
curve(dnorm(x, mean=0, sd=sqrt(2)), col="red", lwd=2, add=TRUE)
legend("topright", legend="Distribución asintótica",col="red",lty=1,cex=0.8, box.lty=0)

#Histograma de W
hist(W_1000,breaks=20,probability=T,main="Histograma de W de tamaño 1000",xlab="W",ylab="Densidad")
curve(dnorm(x, mean=0, sd=sqrt((pi^2)/4)),col="red", lwd=2, add=TRUE)
legend("topright", legend="Distribución asintótica",col="red",lty=1,cex=0.8, box.lty=0)

}

##Ítem e.
{
fdata<-function(N,m){
  MLE<-c()
  ME<-c()
  MO_1<-c()
  MO_2<-c()
  MO_3<-c()
  
  ll_cauchy<-function(theta,x){#Función a optimizar con base a los datos generados(x)
    n<-length(x)
    n*log(pi)+ sum(log(1+(x-theta)^2)) #Negativo de la función de log-verosimilitud 
  }
  
  for(i in 1:m){
    sim = rcauchy(n=N,location=vr,scale=1)
    #Simulación de una m.a. de tamaño N
  
    mle = optim(mean(sim),ll_cauchy,method = "L-BFGS-B",x=sim)$par 
    #Minimización del negativo de la función de log-verosimilitud
  
    me = median(sim)
    #Mediana de los datos generados
  
    #Moda de los datos generados calculada con distintos métodos
    mo_1 = mlv(sim,method = "shorth")
    mo_2 = mlv(sim,method = "naive",bw=1/2)
    mo_3 = mlv(sim,method = "meanshift")
  
    MLE=append(MLE,mle,i)
    ME=append(ME,me,i)
    MO_1=append(MO_1,mo_1,i)
    MO_2=append(MO_2,mo_2,i)
    MO_3=append(MO_3,mo_3,i)
  }
  Estim <- rbind(MLE,ME,MO_1,MO_2,MO_3)
  
  H<-matrix(1:15,nrow=5,ncol=3) #Matriz de simulaciones
  rownames(H)<-c("MLE","Mediana","Moda(método:shorth)","Moda(método:naive)","Moda(método:meanshift)")
  colnames(H)<-c("Promedio","Varianza","MSE")
  
  for(i in 1:5){
    prom = mean(Estim[i,])
    vari = as.double(mean(Estim[i,]^2) - prom^2)
    mse = as.double((mean(Estim[i,])-vr)^2 + vari)
    x<-c(prom,vari,mse)
    H[i,]<-x
  }
  
  
  H #Imprime matriz de datos
  return(Estim) #Retorna realizaciones de los estimadores(Uso:gráficas)
}
#Esta función calcula tres estadísticas de resumen de los estimadores
#para un tamaño de muestra N y m simulaciones

  
#Cálculo para diferentes tamaños de muestras
fdata(50,1000)
fdata(100,1000)
fdata(200,1000)
fdata(500,1000)
fdata(1000,1000)
  

#Datos
H=fdata(1000,1000)

#Gráfica comparativa entre los tres estimadores(la moda es calculada con tres métodos distintos)
plot(density(H[1,]),main="Estimadores",xlab="x",ylab="Densidad",col="darkblue",lwd=2)
lines(density(H[2,]),col="red",lwd=2)
lines(density(H[3,]),col="green",lwd=2)
lines(density(H[4,]),col="orange",lwd=2)
lines(density(H[5,]),col="purple",lwd=2)
abline(v=vr,lwd=3)
legend("topright", legend=c("MLE", "Me","Mo(shorth)","Mo(naive)","Mo(meanshift)"),
       col=c("darkblue", "red","green","orange","purple"), lwd=c(2,2,2,2,2), cex=0.8)

#Gráfica comparativa entre los distintos métodos de cálculo de la moda
plot(density(H[3,]),main="Moda (diferentes métodos)",xlab="x",ylab="Densidad",col="green",lwd=2)
lines(density(H[4,]),col="orange",lwd=2)
lines(density(H[5,]),col="purple",lwd=2)
abline(v=vr,lwd=3)
legend("topright", legend=c("Mo(shorth)","Mo(naive)","Mo(meanshift)"),
       col=c("green","orange","purple"), lwd=c(2,2,2), cex=0.8)
}

