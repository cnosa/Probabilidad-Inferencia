#Importamos algunas librerías que necesitaremos
library(readxl)
library(RColorBrewer)

#Por medio de esta funci?n obtenga la ruta del archivo que contiene los datos
file.choose() 


#Reemplace #Nombre# por la ruta del archivo para extraer los datos
ruta_excel<-"C:\\Users\\beima\\Downloads\\Resultados Algoritmo - ProyectoIE_Parte2 - Pablo Gonzalez.xlsx"

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
#Cociente
#
cocprom<-c()
for(i in seq(1,length(datalongprom[[1]]),1)){
  cocprom = append(cocprom,as.numeric((hcobprom[i])/(hlongprom[i])))
}

#Creamos una función que nos ayude a graficar los datos. En este caso:
#"n" es el tamaño de muestra
#"h" indica los datos que representaremos (eficiencia o longitud promedio) 
#"t" es el framengo del Título del gráfico: "(t) para n=(n)"
grapmuestra<-function(n,h,t){
  
  x<-seq(0.05,0.95,0.05)
  h0<-matrix(h,nrow=35)
  
  pos1<-c();pos2<-c();pos3<-c();pos4<-c();pos5<-c();m<-integer()
  
  #Correspondencia de tamaños de muestra con columnas (específica para la base de datos extraida del punto 1)
  if(n==5) {m=5}
  else if(n==10) {m=1}
  else if(n==50) {m=6}
  else if(n==100) {m=2}
  else if(n==200) {m=4}
  else if(n==500) {m=7}
  else if(n==1000) {m=3}
  else {return("Valor de n errado")}
  
  for(i in 1:19){
    h1<-matrix(h0[,i],nrow=5)
    h2<-c(h1[,m]) 
    pos1 = append(pos1,h2[1])
    pos2 = append(pos2,h2[2])
    pos3 = append(pos3,h2[3])
    pos4 = append(pos4,h2[4])
    pos5 = append(pos5,h2[5])
 
  }
  colores = brewer.pal(5,"Set2")
  
  titulo = paste(paste(t,"para n="),n)
  plot(x,pos1,type="l",col=colores[1],lwd = 2,main=titulo,xlab="Valores del par�metro p",ylab=t,xlim=c(0.05,0.95), ylim=c(min(c(min(pos1),min(pos2),min(pos3),min(pos4),min(pos5)))-0.01, max(c(max(pos1),max(pos2),max(pos3),max(pos4),max(pos5)))+0.01))
  lines(x,pos2,type="l",col=colores[2],lwd = 2)
  lines(x,pos3,type="l",col=colores[3],lwd = 2)
  lines(x,pos4,type="l",col=colores[4],lwd = 2)
  lines(x,pos5,type="l",col=colores[5],lwd = 2)
  legend("bottomright",legend=c("Posib 1", "Posib 2", "Posib 3", "Posib 4", "Posib 5" ),lwd = 2,col=colores,cex=0.7,box.lty=0)
}
# Nota: Esta función esta diseñada para un conjunto de datos muy específico

###########################################################

#Gráficas para la cobertura promedio
grapmuestra(5,hcobprom,"Cobertura promedio")
grapmuestra(50,hcobprom,"Cobertura promedio")
grapmuestra(200,hcobprom,"Cobertura promedio")
grapmuestra(1000,hcobprom,"Cobertura promedio")


#Gráficas para la longitud promedio de los intervalos
grapmuestra(5,hlongprom,"Longitud promedio")
grapmuestra(50,hlongprom,"Longitud promedio")
grapmuestra(200,hlongprom,"Longitud promedio")
grapmuestra(1000,hlongprom,"Longitud promedio")


#Gráficas para coc promedio de los intervalos
grapmuestra(5,cocprom,"Cociente promedio")
grapmuestra(50,cocprom,"Cociente promedio")
grapmuestra(200,cocprom,"Cociente promedio")
grapmuestra(1000,cocprom,"Cociente promedio")

