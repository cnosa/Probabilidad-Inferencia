# Importamos las librerias necesaria para almacenar los resultados y para exportarlos como Excel (.xlsx).
library(hash)
library(writexl)

## Punto 2.
# Numero de simulaciones
m <- 1000

# Valores p y n de los cuales podemos escoger
valores.parametro <- seq(from = 0.05, to = 0.95, by = 0.05)
valores.n <- c(5, 10, 50, 100, 200, 500, 1000)

# Intervalos de confianza del 95%
alpha <- 0.05

# 1000 Muestras Bootstrap
B <- 1000

# Variable para almacenar resultados de cobertura promedio y longitud promedio
H <- hash()

# Percentil 1 - alpha/2
z.score <- qnorm(p = 1 - alpha/2)

for (p in valores.parametro) {
  for (n in valores.n) {
    # Variables necesarias para el literal c3.
    cantidad.intervalos.con.parametro.posib1 <- 0
    longitudes.intervalos.con.parametro.posib1 <- c()
      
    cantidad.intervalos.con.parametro.posib2 <- 0
    longitudes.intervalos.con.parametro.posib2 <- c()
    
    cantidad.intervalos.con.parametro.posib3 <- 0
    longitudes.intervalos.con.parametro.posib3 <- c()
      
    cantidad.intervalos.con.parametro.posib4 <- 0
    longitudes.intervalos.con.parametro.posib4 <- c()
      
    cantidad.intervalos.con.parametro.posib5 <- 0
    longitudes.intervalos.con.parametro.posib5 <- c()
    
    # Linea para saber en que combinación de valores p y n se encuentra actualmente.
    print(paste0("--------- Para valor de parametro = ", p, " y tamaño de muestra n = ", n, " ---------"))
    
    for (iter in 1:m) {
      
      # Linea para saber en que iteracion se encuentra actualmente.
      if (iter %in% c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)) {
        print(paste0("Iteracion numero ", iter))
      }
      
      # Muestra aleatoria de una distribución Bernoulli de tamaño n con parámetro p
      muestra.aleatoria <- rbinom(n = n, size = 1, prob = p)
      
      # Media muestral de la muestra aleatoria original
      media.muestral <- mean(muestra.aleatoria)
      
      # --------------------------------- Posibilidad 1 ---------------------------------
      primera.expresion <- ( media.muestral + (z.score^2)/(2*n) )/( 1 + (z.score^2)/(n) )
      segunda.expresion <- ( sqrt( (media.muestral*(1-media.muestral))/(n) + (z.score^2)/(4*n^2) ) )/( 1 + (z.score^2)/(n) )
      
      limite.inferior <- primera.expresion - z.score * segunda.expresion
      limite.superior <- primera.expresion + z.score * segunda.expresion
      
      # Si el verdadero valor del parámetro esta dentro del intervalo, se añade +1 a la cantidad de intervalos que contienen
      # al parámetro para esta posibilidad, y se almacena la longitud de dicho intervalo.
      if (limite.inferior <= p && p <= limite.superior) {
        cantidad.intervalos.con.parametro.posib1 <- cantidad.intervalos.con.parametro.posib1 + 1
        longitudes.intervalos.con.parametro.posib1 <- c(longitudes.intervalos.con.parametro.posib1, ( limite.superior - limite.inferior ))
      }
      # ---------------------------------------------------------------------------------
      
      
      # --------------------------------- Posibilidad 2 ---------------------------------
      limite.inferior <- media.muestral - z.score * sqrt((media.muestral * (1-media.muestral))/n)
      limite.superior <- media.muestral + z.score * sqrt((media.muestral * (1-media.muestral))/n)
      
      # Si el verdadero valor del parámetro esta dentro del intervalo, se añade +1 a la cantidad de intervalos que contienen
      # al parámetro para esta posibilidad, y se almacena la longitud de dicho intervalo.
      if (limite.inferior <= p && p <= limite.superior) {
        cantidad.intervalos.con.parametro.posib2 <- cantidad.intervalos.con.parametro.posib2 + 1
        longitudes.intervalos.con.parametro.posib2 <- c(longitudes.intervalos.con.parametro.posib2, ( limite.superior - limite.inferior ))
      }
      # ---------------------------------------------------------------------------------
      
      
      # --------------------------------- Posibilidad 3 ---------------------------------
      limite.inferior <- sin(asin(sqrt(media.muestral)) - (z.score)/(2*sqrt(n)))^2
      limite.superior <- sin(asin(sqrt(media.muestral)) + (z.score)/(2*sqrt(n)))^2
      
      # Si el verdadero valor del parámetro esta dentro del intervalo, se añade +1 a la cantidad de intervalos que contienen
      # al parametro para esta posibilidad, y se almacena la longitud de dicho intervalo.
      if (limite.inferior <= p && p <= limite.superior) {
        cantidad.intervalos.con.parametro.posib3 <- cantidad.intervalos.con.parametro.posib3 + 1
        longitudes.intervalos.con.parametro.posib3 <- c(longitudes.intervalos.con.parametro.posib3, ( limite.superior - limite.inferior ))
      }
      # ---------------------------------------------------------------------------------
      
      
      # --------------------------------- Posibilidad 4 ---------------------------------
      estimaciones.bootstrap <- numeric(length = n)
      
      # Calcular y almacenar B = 1000 muestras bootstrap a partir de la muestra original.
      for (i in 1:B) {
        muestra.bootstrap <- sample(muestra.aleatoria, replace=TRUE)
        estimaciones.bootstrap[i] <- mean(muestra.bootstrap)
      }
      
      limite.inferior <- quantile(x = estimaciones.bootstrap, probs = alpha/2)
      limite.superior <- quantile(x = estimaciones.bootstrap, probs = 1 - alpha/2)
      
      # Si el verdadero valor del parámetro esta dentro del intervalo, se añade +1 a la cantidad de intervalos que contienen
      # al parámetro para esta posibilidad, y se almacena la longitud de dicho intervalo.
      if (limite.inferior <= p && p <= limite.superior) {
        cantidad.intervalos.con.parametro.posib4 <- cantidad.intervalos.con.parametro.posib4 + 1
        longitudes.intervalos.con.parametro.posib4 <- c(longitudes.intervalos.con.parametro.posib4, ( limite.superior - limite.inferior ))
      }
      # ---------------------------------------------------------------------------------
      
      
      # --------------------------------- Posibilidad 5 ---------------------------------
      estimaciones.bootstrap.empirico <- numeric(length = n)
      
      # Calcular y almacenar B = 1000 muestras bootstrap a partir de la muestra original.
      for (i in 1:B) {
        muestra.bootstrap <- sample(muestra.aleatoria, replace=TRUE)
        estimaciones.bootstrap.empirico[i] <- ( mean(muestra.bootstrap) - media.muestral )
      }
      
      limite.inferior <- ( media.muestral - quantile(x = estimaciones.bootstrap.empirico, probs = 1 - alpha/2) )
      limite.superior <- ( media.muestral - quantile(x = estimaciones.bootstrap.empirico, probs = alpha/2))
      
      # Si el verdadero valor del parámetro esta dentro del intervalo, se añade +1 a la cantidad de intervalos que contienen
      # al parámetro para esta posibilidad, y se almacena la longitud de dicho intervalo.
      if (limite.inferior <= p && p <= limite.superior) {
        cantidad.intervalos.con.parametro.posib5 <- cantidad.intervalos.con.parametro.posib5 + 1
        longitudes.intervalos.con.parametro.posib5 <- c(longitudes.intervalos.con.parametro.posib5, ( limite.superior - limite.inferior ))
      }
      # ---------------------------------------------------------------------------------
      
    }
    
    "
    En la siguiente porcion de código, se almacena en un diccionario/hash llamado H la cobertura promedio y 
    la longitud promedio para cada posibilidad y para cada combinación del valor del parametro (p) y el 
    tamaño de muestra (n).
    
    Para acceder a cada cobertura promedio y longitud promedio obtenido en cierta posibilidad con cierto valor
    de parametro (p) y tamaño de muestra (n) se utiliza H[[llave]] donde llave es de la forma:
    
    llave = Cob. Promedio con param = [p] y n = [n] en posibilidad [numero de posibilidad], (para acceder a la cob. promedio)
    llave = Long. Promedio con param = [p] y n = [n] en posibilidad [numero de posibilidad], (para acceder a la long. promedio)
    "
    
    cobprom1 <- paste0("Cob. Promedio con param = ", p, " y n = ", n, " en posibilidad 1")
    longprom1 <- paste0("Long. Promedio con param = ", p, " y n = ", n, " en posibilidad 1")
    
    H[[cobprom1]] <- cantidad.intervalos.con.parametro.posib1/1000
    H[[longprom1]] <- sum(x = longitudes.intervalos.con.parametro.posib1)/cantidad.intervalos.con.parametro.posib1
    
    cobprom2 <- paste0("Cob. Promedio con param = ", p, " y n = ", n, " en posibilidad 2")
    longprom2 <- paste0("Long. Promedio con param = ", p, " y n = ", n, " en posibilidad 2")
    
    H[[cobprom2]] <- cantidad.intervalos.con.parametro.posib2/1000
    H[[longprom2]] <- sum(x = longitudes.intervalos.con.parametro.posib2)/cantidad.intervalos.con.parametro.posib2
    
    cobprom3 <- paste0("Cob. Promedio con param = ", p, " y n = ", n, " en posibilidad 3")
    longprom3 <- paste0("Long. Promedio con param = ", p, " y n = ", n, " en posibilidad 3")
    
    H[[cobprom3]] <- cantidad.intervalos.con.parametro.posib3/1000
    H[[longprom3]] <- sum(x = longitudes.intervalos.con.parametro.posib3)/cantidad.intervalos.con.parametro.posib3
    
    cobprom4 <- paste0("Cob. Promedio con param = ", p, " y n = ", n, " en posibilidad 4")
    longprom4 <- paste0("Long. Promedio con param = ", p, " y n = ", n, " en posibilidad 4")
    
    H[[cobprom4]] <- cantidad.intervalos.con.parametro.posib4/1000
    H[[longprom4]] <- sum(x = longitudes.intervalos.con.parametro.posib4)/cantidad.intervalos.con.parametro.posib4
    
    cobprom5 <- paste0("Cob. Promedio con param = ", p, " y n = ", n, " en posibilidad 5")
    longprom5 <- paste0("Long. Promedio con param = ", p, " y n = ", n, " en posibilidad 5")
    
    H[[cobprom5]] <- cantidad.intervalos.con.parametro.posib5/1000
    H[[longprom5]] <- sum(x = longitudes.intervalos.con.parametro.posib5)/cantidad.intervalos.con.parametro.posib5
    
  }
}

# Este algoritmo se demora corriendo aproximadamente 4 horas con 8GB de memoria RAM.

# --------- Exportar resultados ---------
# Convertir a dataframe
a<-keys(H)
b<-values(H)
d<-cbind(a,b)
L<-as.data.frame(d)

# Exportar como archivo excel
result.dataframe <- data.frame(lapply(L, as.character), stringsAsFactors=FALSE)
write_xlsx(result.dataframe, "C:\\Users\\Pablo\\Desktop\\Proyecto-R\\resultados-algoritmo.xlsx")