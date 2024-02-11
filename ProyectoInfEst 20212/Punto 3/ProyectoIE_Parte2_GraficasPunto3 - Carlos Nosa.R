#Importamos datos desde un archivo Excel
library(readxl)

#Por medio de esta función obtenga la ruta del archivo que contiene los datos
file.choose() 


#Complete con la ruta del archivo para extraer los datos
ruta_excel<- 
  

excel_sheets(ruta_excel)


#Datos correspondientes a la cobertura promedio
datacobprom<- read_excel(ruta_excel,range="B2:B666",col_names = FALSE)
hcobprom<- c()
for(i in seq(1,length(datacobprom[[1]]),1)){
  hcobprom = append(hcobprom,as.numeric(datacobprom[[1]][i]))
}

#Datos correspondientes a la longitud promedio 
datalongprom<- read_excel(ruta_excel,range="B667:B1331",col_names = FALSE)
hlongprom<- c()
for(i in seq(1,length(datalongprom[[1]]),1)){
  hlongprom = append(hlongprom,as.numeric(datalongprom[[1]][i]))
}




grap<-function(posib,h,t){
  
  x<-seq(0.05,0.95,0.05)
  h0<-matrix(h,nrow=35)
  
  tm5<-c();tm10<-c();tm50<-c();tm100<-c();tm200<-c();tm500<-c();tm1000<-c()
  
  for(i in 1:19){
    h1<-matrix(h0[,i],nrow=5)
    h2<-matrix(h1[posib,])
    tm5 = append(tm5,h2[5,1])
    tm10 = append(tm10,h2[1,1])
    tm50 = append(tm50,h2[6,1])
    tm100 = append(tm100,h2[2,1])
    tm200 = append(tm200,h2[4,1])
    tm500 = append(tm500,h2[7,1])
    tm1000 = append(tm1000,h2[3,1])
  }
  colores = rainbow(7)
  
  titulo = paste(paste(t,"para la posibilidad"),posib)
  plot(x,tm5,type="l",col=colores[1],lwd = 2,main=titulo,xlab="Valores del parámetro p",ylab=t,xlim=c(0.05,0.95),ylim=c(0,1))
  lines(x,tm10,type="l",col=colores[2],lwd = 2)
  lines(x,tm50,type="l",col=colores[3],lwd = 2)
  lines(x,tm100,type="l",col=colores[4],lwd = 2)
  lines(x,tm200,type="l",col=colores[5],lwd = 2)
  lines(x,tm500,type="l",col=colores[6],lwd = 2)
  lines(x,tm1000,type="l",col=colores[7],lwd = 2)
  legend("bottomright",legend=c("n=5","n=10","n=50","n=100","n=200","n=500","n=1000"),lwd = 2,col=colores,cex=0.7,box.lty=0)
}
#Esta función grafica el conjunto de datos asociados a la cobertura promedio
# y la longitud promedio para cada uno de las cinco posibilidades con parámetros:
# posib: Número de la posibilidad que se desea graficar
# h: Conjuntos de datos para graficar
# t: Título para la gráfica

# Nota: Esta función esta diseñada para un conjunto de datos muy específico



#Gráficas para la cobertura promedio
grap(1,hcobprom,"Cobertura promedio")
grap(2,hcobprom,"Cobertura promedio")
grap(3,hcobprom,"Cobertura promedio")
grap(4,hcobprom,"Cobertura promedio")
grap(5,hcobprom,"Cobertura promedio")


#Gráficas para la longitud promedio de los intervalos
grap(1,hlongprom,"Longitud promedio")
grap(2,hlongprom,"Longitud promedio")
grap(3,hlongprom,"Longitud promedio")
grap(4,hlongprom,"Longitud promedio")
grap(5,hlongprom,"Longitud promedio")

