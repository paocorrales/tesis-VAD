library(reshape2)
library(lubridate)
library(data.table)

#=========================================================================================#
# read.vad es una funcion que lee los archivos con los datos de un vad para una determinada
# fecha. Recibe la ubicación de los archivos y devuelve un dataframe con los datos.
#=========================================================================================#

read.vad <- function(path){
  
  filename <- (Sys.glob(path))
  datetime <- ymd_hms(stringi::stri_sub(filename, from = 24, to = 44))
  
  for (i in 1:length(filename)){
    temp <- read.csv(filename[i], sep = ";", dec = ".", na.strings = "-9999")
    date_time <- datetime[i]
    temp$date_time <- date_time
    if (i == 1){
      temp2 <- temp
    } else {
      temp2 <- rbind(temp2, temp)
    }
  }
  temp2$di <- temp2$di
  temp2$u <- -temp2$spd * sin(temp2$di*pi/180)
  temp2$v <- -temp2$spd * cos(temp2$di*pi/180)
  return(temp2)
}
  

#=========================================================================================#
# loess.vad aplica loess a spd para corregir inconsistencias temporales
# Si hay NAs no agrega nada, de hecho se pieren algunos datos vecinos.
#=========================================================================================#

lowess.vad <- function(dataframe, span = 0.06, delta = 0){
  
  # Convierte mis datos en data.table
  vad.dt <- as.data.table(dataframe)
  
  # Calculo el loess para cada altura. Span pequeño para que la regresión sea mas localizada.
  vad.dt[, spd_smooth := lowess(spd ~ date_time, f = span, delta = delta)$y, by = ht]
  
  return(as.data.frame(vad.dt))
  
}


#=========================================================================================#
# ri.j calcula el número de Richarson bulk jet según Banta 2003
# 
#=========================================================================================#
t_i <- 27.7
t_f <- 22.3
p_i <- 1002.6
p_f <- 1003.3
u_max <- 16.51094
z_max <- 0.3


ri.j <- function(t_i, t_f, p_i, p_f, u_max, z_max){
  g <- 9.8
  
  tita_i <- t_i*(p_i/1000)^(-0.286)
  tita_f <- t_f*(p_f/1000)^(-0.286)
  
    tita_m <- (tita_i + tita_f)/2
  delta_z <- z_max - 10
  ri<- (g * (tita_f - tita_i)/delta_z) / (tita_m * (u_max/z_max)^2)
  return(ri)
}

#=========================================================================================#
# t.tita calcula tita a partir de T y la presión
# 
#=========================================================================================#
