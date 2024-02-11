#Punto 6
#a
#Hacemos los calculos para obtener los límites correspondientes de nuestro intervalo de confianza

p=0.8
z=pnorm(0.95)
limitinf=(p+z^2/20)/(1+z^2/10)-z*(sqrt(p*(1-p)/10+z^2/400))/(1+z^2/10)
limitsup=(p+z^2/20)/(1+z^2/10)+z*(sqrt(p*(1-p)/10+z^2/400))/(1+z^2/10)#Determinamos el alpha escogido:
alpha=0.01
#b
#Método para realizar un intervalo de confianza para (b), mediante la posibilidad 4:
muestra<-vector(length=100)
for(i in 0:100){
  if (i<=10){muestra[i]=1}
  else {muestra[i]=0}
}
#Calculamos la estimación puntual de la proporción:
mean(muestra)
#Guardamos en un vector 1000 estimaciones bootstrap de la proporción:
medias.bootstrap<-vector(length=1000)
for(i in 0:100){
medias.bootstrap[i]<-mean(sample(muestra, replace=TRUE))
}
limite.inferior <- quantile(x = medias.bootstrap, probs = alpha/2)
limite.superior <- quantile(x = medias.bootstrap, probs = 1 - alpha/2)
#Por ultimo, mostramos los resultados obtenidos:
limite.inferior
limite.superior