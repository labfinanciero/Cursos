
# Montecarlo flujo de caja ----
## Segundo nivel ----
### Tercer nivel ----
#### Cuarto nivel ----
##### Quinto nivel ----
###### Sexto nivel ----

# Nivelatorio de R

## Tour por Rstudio ----
### Principales paneles ----
#### Editor ----
#### Consola ----
#### Enviroment ----
#### History ----
#### Plots ----
#### Help ----
#### Files ----

## Ejecutar comandos desde el editor ----
### Cntrl + Enter ----
### Seleccionar las líneas ----
### Cntrl + alt + r Ejecuta todo el programa ----

## Operadores ----
### Aritméticos ----
### De asignación ----

## Estructuras de datos ----
### Vectores ----
### Matrices ----
### Data Frames ----


## Lista con fuciones requeridas ----

## Instalar paquetes ----

## Cargar paquetes ----

#### Crear funcioens


## Ejercicios ----
# Vídeo de doctor strege.
### Flujo de caja ----
library(ggplot2)
vpn_simulados <- function (iteracion) {
 inversion <- runif(1, 1000, 2000)
 tasa_cv <- runif(1, 0, 0.12)
 ventas_1 <- runif(1, 700, 1200)
 costo_variable <- runif(1, 0.25, 0.55)
 costo_fijo <- rnorm(1, 250, 50)
 inflacion <- rnorm(1, 0.05, 0.01)
 costo_k <- 0.1
 
 # vf = vp * (1 + i)^n
 
 ingresos_proyectados <- ventas_1 * (1 + tasa_cv) ^ (0:4) # Precedencias, operaciones con vectores. 
 costos_proyectados <- costo_fijo * (1 + inflacion) ^ (0:4)
 costos_var_proyectados <- ingresos_proyectados * costo_variable
 utilidad_bruta_proyectada <- ingresos_proyectados - costos_proyectados - costos_var_proyectados
 
 # El del año 1 se lleva a cero
 # vp = vf / (1 + i) ^ n
 vpn <- -inversion + sum(utilidad_bruta_proyectada / (1 + costo_k) ^ (1:5))
return(vpn) 
}


a <- 1:10000
resultado <- sapply(a, vpn_simulados)
hist(resultado, probability = TRUE, nclass = ceiling(sqrt(length(a))))
summary(resultado)[1]
abline(v = summary(resultado)[3], col = "salmon", lwd = 4, lty = 3)
abline(v = summary(resultado)[4], col = "royalblue", lwd = 4, lty = 3)

boxplot(resultado, horizontal = TRUE)
abline(v = summary(resultado)[4], col = "royalblue", lwd = 2, lty = 3)

sum(resultado > 0) / length(a)
resultado_data <- data.frame(x = resultado)
ggplot(data = resultado_data, aes(x)) + geom_boxplot() + geom_jitter(aes(y = 0), height = 0.01, alpha = 0.1)


### Póker ----
#### Probabilidad de par ----
#pintas <- c("Corazón_", "Diamante_", "Picas_", "Trébol_")
#mazo <- paste0(pintas, cartas)

cartas <- 2:14
cartas <- rep(cartas, each = 4)

un_par <- function (iter, cartas) {
 mano <- sample(cartas, 52, replace = FALSE)[1:5]
 mano <- unique(mano)
  if (length(mano) == 4) {
    return(1)
  } else {
    return(0)
  }
}

it <- 100000
sum(sapply(1:it, un_par, cartas)) / it * 100


#### Dos pares. 

cartas <- 2:14
cartas <- rep(cartas, each = 4)

dos_pares <- function (iter, cartas) {
 mano <- sample(cartas, 52, replace = FALSE)[1:5]
 if (length(unique(mano)) == 3) {
   mano <- sort(mano)
 } else {
  return(0)
 }
}

it <- 10000
sum(sapply(1:it, un_par, cartas)) / it * 100
