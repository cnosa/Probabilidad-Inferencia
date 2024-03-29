#C�lculo de una estimaci�n m�ximo-verosimil cuando no hay una soluci�n anal�tica
vr = 3 #Valor real del par�metro

##�tem a.
{


MLE<-c() #Vector de valores estimados del MLE de las simulaciones
ME<-c() #Vector de valores estimados de la mediana de las simulaciones

ll_cauchy<-function(theta,x){#Funci�n a optimizar con base a los datos generados(x)
  n<-length(x)
  n*log(pi)+ sum(log(1+(x-theta)^2)) #Negativo de la funci�n de log-verosimilitud 
}



for(i in 1:1000){
  sim = rcauchy(n=10,location=vr,scale=1)
  #Simulaci�n de una m.a. de tama�o 10
  
  mle = optim(mean(sim),ll_cauchy,method = "L-BFGS-B",x=sim)$par 
  #Minimizaci�n del negativo de la funci�n de log-verosimilitud. Se seleccion� el m�todo 
  #"L-BFGS-B" para poder usar una funci�n bidimensional 
  
  me = median(sim)
  #Mediana de los datos generados
  
  MLE=append(MLE,mle,i)
  ME=append(ME,me,i)
}

#Histograma de las estimaciones por MLE
hist(MLE,breaks=30,probability = T,main="Histograma de las estimaciones por el estimador MLE",xlab="Estimaciones de MLE",
     ylab="Densidad")
abline(v=vr,col="red",lty=2,lwd=3)#Valor verdadero del par�metro
abline(v=mean(MLE),col="blue",lty=1,lwd=2)#Media de las estimaciones
legend("topleft", legend=c("Valor real del par�metro", "Promedio de las estimaciones"),
       col=c("red", "blue"), lty=c(2,1),lwd=c(3,2), cex=0.8,
       box.lty=0)

#Histograma de los valores de la mediana de las simulaciones
hist(ME,breaks=20,probability = T,main="Histograma de los valores de la mediana muestral
     (estimador por analog�a)",xlab="Valores de la mediana de la muestra",
     ylab="Densidad",)
abline(v=vr,col="red",lty=2,lwd=3) #Valor verdadero del par�metro
abline(v=mean(ME),col="blue",lty=1,lwd=2) #Media de las estimaciones
legend("topleft", legend=c("Valor real del par�metro", "Promedio de las estimaciones"),
       col=c("red", "blue"), lty=c(2,1),lwd=c(3,2), cex=0.8,
       box.lty=0)

#Promedio y varianza de los estimadores
  #Estimador MLE
Promedio_MLE=mean(MLE)
Varianza_MLE=mean(MLE^2)-mean(MLE)^2
  #Mediana
Promedio_ME=mean(ME)
Varianza_ME=mean(ME^2)-mean(ME)^2

#Error cuadr�tico medio (MSE) de los estimadores
 #Estimador MLE
MSE_MLE = (Promedio_MLE-vr)^2 + Varianza_MLE
  #Mediana
MSE_ME = (Promedio_ME-vr)^2 + Varianza_ME

#Eficiencia relativa (Estimador MLE,Mediana)
RE = MSE_MLE/MSE_ME
}

##�tem b.
{
tm_N<-function(N,m){
MLE<-c() #Vector de valores estimados del MLE de cada una de las simulaciones
ME<-c() #Vector de valores estimados de la mediana de cada una de las simulaciones

ll_cauchy<-function(theta,x){#Funci�n a optimizar con base a los datos generados(x)
  n<-length(x)
  n*log(pi)+ sum(log(1+(x-theta)^2)) #Negativo de la funci�n de log-verosimilitud 
} 

for(i in 1:m){
  sim = rcauchy(n=N,location=3,scale=1)
  #Simulaci�n de una m.a. de tama�o 10  
  
  mle = optim(mean(sim),ll_cauchy,method = "L-BFGS-B",x=sim)$par 
  #Minimizaci�n del negativo de la funci�n de log-verosimilitud. Se seleccion� el m�todo 
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
abline(v=vr,col="red",lty=2,lwd=3)#Valor verdadero del par�metro
abline(v=mean(MLE),col="blue",lty=1,lwd=2)#Media de las estimaciones
legend("bottomright",legend=c("V. real", "Media"),
       col=c("red", "blue"), lty=c(2,1),lwd=c(3,2), cex=0.8,
       box.lty=0)

#Histograma de los valores de la mediana de las simulaciones
hist(ME,breaks=20,probability = T,main="Mediana",xlab="Valores de la mediana de la muestra",
     ylab="Densidad",)
abline(v=vr,col="red",lty=2,lwd=3) #Valor verdadero del par�metro
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

#Error cuadr�tico medio (MSE) de los estimadores
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
} #Esta funci�n retorna los histogramas del estimador MLE
#y de la Mediana, adem�s, retorna algunos datos de resumen de estos estimadores 
#N corresponde al tama�o de la muestra y m al n�mero de simulaciones

  
#C�lculo para los diferentes tama�os de muestra con m=1000 simulaciones
tm_N(50,1000)
tm_N(100,1000)
tm_N(200,1000)
tm_N(500,1000)  
tm_N(1000,1000)

}

##�tem d.
{
VW<-function(N){
  sim = rcauchy(n=N,location=vr,scale=1)
  mle = optim(mean(sim),ll_cauchy,method = "L-BFGS-B",x=sim)$par
  me = median(sim)
  c(sqrt(N)*(mle-vr),sqrt(N)*(me-vr))
} #Esta funci�n retorna realizaciones de V_N y W_N

V_1000<-c()
W_1000<-c()

for(i in 1:1000){
  V = VW(1000)[1]
  W = VW(1000)[2]
  V_1000=append(V_1000,V,i)
  W_1000=append(W_1000,W,i)
} #m simulaciones

#Histograma de V
hist(V_1000,breaks=20,probability=T,main="Histograma de V de tama�o 1000",xlab="V",ylab="Densidad")
curve(dnorm(x, mean=0, sd=sqrt(2)), col="red", lwd=2, add=TRUE)
legend("topright", legend="Distribuci�n asint�tica",col="red",lty=1,cex=0.8, box.lty=0)

#Histograma de W
hist(W_1000,breaks=20,probability=T,main="Histograma de W de tama�o 1000",xlab="W",ylab="Densidad")
curve(dnorm(x, mean=0, sd=sqrt((pi^2)/4)),col="red", lwd=2, add=TRUE)
legend("topright", legend="Distribuci�n asint�tica",col="red",lty=1,cex=0.8, box.lty=0)

}

##�tem e.
{
fdata<-function(N,m){
  MLE<-c()
  ME<-c()
  MO_1<-c()
  MO_2<-c()
  MO_3<-c()
  
  ll_cauchy<-function(theta,x){#Funci�n a optimizar con base a los datos generados(x)
    n<-length(x)
    n*log(pi)+ sum(log(1+(x-theta)^2)) #Negativo de la funci�n de log-verosimilitud 
  }
  
  for(i in 1:m){
    sim = rcauchy(n=N,location=vr,scale=1)
    #Simulaci�n de una m.a. de tama�o N
  
    mle = optim(mean(sim),ll_cauchy,method = "L-BFGS-B",x=sim)$par 
    #Minimizaci�n del negativo de la funci�n de log-verosimilitud
  
    me = median(sim)
    #Mediana de los datos generados
  
    #Moda de los datos generados calculada con distintos m�todos
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
  rownames(H)<-c("MLE","Mediana","Moda(m�todo:shorth)","Moda(m�todo:naive)","Moda(m�todo:meanshift)")
  colnames(H)<-c("Promedio","Varianza","MSE")
  
  for(i in 1:5){
    prom = mean(Estim[i,])
    vari = as.double(mean(Estim[i,]^2) - prom^2)
    mse = as.double((mean(Estim[i,])-vr)^2 + vari)
    x<-c(prom,vari,mse)
    H[i,]<-x
  }
  
  
  H #Imprime matriz de datos
  return(Estim) #Retorna realizaciones de los estimadores(Uso:gr�ficas)
}
#Esta funci�n calcula tres estad�sticas de resumen de los estimadores
#para un tama�o de muestra N y m simulaciones

  
#C�lculo para diferentes tama�os de muestras
fdata(50,1000)
fdata(100,1000)
fdata(200,1000)
fdata(500,1000)
fdata(1000,1000)
  

#Datos
H=fdata(1000,1000)

#Gr�fica comparativa entre los tres estimadores(la moda es calculada con tres m�todos distintos)
plot(density(H[1,]),main="Estimadores",xlab="x",ylab="Densidad",col="darkblue",lwd=2)
lines(density(H[2,]),col="red",lwd=2)
lines(density(H[3,]),col="green",lwd=2)
lines(density(H[4,]),col="orange",lwd=2)
lines(density(H[5,]),col="purple",lwd=2)
abline(v=vr,lwd=3)
legend("topright", legend=c("MLE", "Me","Mo(shorth)","Mo(naive)","Mo(meanshift)"),
       col=c("darkblue", "red","green","orange","purple"), lwd=c(2,2,2,2,2), cex=0.8)

#Gr�fica comparativa entre los distintos m�todos de c�lculo de la moda
plot(density(H[3,]),main="Moda (diferentes m�todos)",xlab="x",ylab="Densidad",col="green",lwd=2)
lines(density(H[4,]),col="orange",lwd=2)
lines(density(H[5,]),col="purple",lwd=2)
abline(v=vr,lwd=3)
legend("topright", legend=c("Mo(shorth)","Mo(naive)","Mo(meanshift)"),
       col=c("green","orange","purple"), lwd=c(2,2,2), cex=0.8)
}

