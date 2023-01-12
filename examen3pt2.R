# EXAMEN PARCIAL 3 PARTE 2  MODELAJE DE ENFERMEDADES INFECCIOSAS}

### REBECA VEGA ESCAMILLA ###


#A) Considera un modelo SIR sin demografía en donde la población se conserva.
#Escribe y resuelve las ecuaciones diferenciales para valores de R0=beta/gamma =
#1.75, 2, 2.5
 ### CON R0 = 1.75 ###

library(deSolve) #Cargo mi libreria

SIR <- function(t, estado, parametros) { #Voy a crear una  función
  with(as.eq(c(estado, parametros)), { #para despues hacer las ecuaciones
    dS <- -b*S*I
    dI <- b*S*I -d*I
    dR <- d*I
    eq(c(dS, dI, dR)) #  ecuaciones del modelo
  })
}
#en el modelo no se incluye la tasa de nacimiento y muerte porque si la poblacion se conserva estas se eliminan en las ecuaciones

parametros <- c(b = 9.2, d = 5.25) #pongo los parametros
estado <- c(S = 10^6, I = 1, R = 0) #pongo mis condiciones iniciales
t <- seq(0, 3, by = 0.00001) 
out <- ode(estado, t, SIR, parametros) 

## Grafica del modelo ##
matplot(out[ , 1], out[ , 2:4], type = "l", xlab = "time", ylab = "Población",
        main = "SIR R0 = 1.75", lwd = 2) 
legend("topright", c("Susceptible", "Infectado","Recuperado"), col = 1:3,lty=1:3,cex=0.5)

### CON R0=2 ###

SIRMOD <- function(t, estado, parametros) { #creo una nueva función
  with(as.eq(c(estado, parametros)), { #defino las ecuaciones
    dS <- -b*S*I
    dI <- b*S*I -d*I
    dR <- d*I
    eq(c(dS, dI, dR)) #pongo las ecuaciones d emi modelo
  })
}


parametros <- c(b = 400, d = 200) #Establecer los parametros
estado <- c(S = 10^6, I = 1, R = 0) #establecer las condiciones iniciales
t <- seq(0, 1, by = 0.0001) 
out <- ode(estado, t, SIRMOD, parametros) 

## Grafica del modelo ##
matplot(out[ , 1], out[ , 2:4], type = "l", xlab = "time", ylab = "Población",
        main = "SIR R0 = 2", lwd = 2) 
legend("topright", c("Susceptible", "Infectado","Recuperado"), col = 1:3,lty=1:3,cex=0.5)

### CON RO= 2.5 ###

MODSIR <- function(t, estado, parametros) { #creo una nueva función
  with(as.eq(c(estado, parametros))) { #defino las ecuaciones
    dS <- -b*S*I
    dI <- b*S*I -d*I
    dR <- d*I
    eq(c(dS, dI, dR)) 
}}
#en el modelo no se incluye la tasa de nacimiento y muerte porque si la poblacion se conserva estas se eliminan en las ecuaciones

parametros <- c(b = 370, d = 145) #pongo mis parametros
estado <- c(S = 10^6, I = 1, R = 0) #pongo mis condiciones
t <- seq(0, 3, by = 0.0001) 
out <- ode(estado, t, MODSIR, parametros)

## Grafica del modelo ##
matplot(out[ , 1], out[ , 2:4], type = "l", xlab = "time", ylab = "Población",
        main = "SIR R0 = 2.5", lwd = 2) 
legend("topright", c("Susceptible", "Infectado","Recuperado"), col = 1:3,lty=1:3,cex=0.5)


#B) Considera la condición inicial (S=10^6, I=1, R=0) # Ver arriba je

#C) Considera los datos de la pandemia de H1N1 en México. Escoge valores de beta y 
#gamma que den los valores de R0 de arriba pero que ajusten mejor los datos.
#Sugerencia utiliza un ciclo for para variar los valores.

inf <- read.csv("dinfluenza.csv") # creo mi objeto con mi matriz 
inf

hist(inf$Cases, col= rainbow(8) ) # observo mis datos

