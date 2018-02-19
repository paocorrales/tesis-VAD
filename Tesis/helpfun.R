library(reshape2)
library(lubridate)
library(data.table)

#=========================================================================================#
# read.vad es una funcion que lee los archivos con los datos de un vad para una determinada
# fecha. Recibe la ubicación de los archivos y devuelve un dataframe con los datos.
#=========================================================================================#

read.vad <- function(path, lowess = TRUE){
  
  filename <- (Sys.glob(path))
  file <- basename(filename)
  datetime <- ymd_hms(stringi::stri_sub(file, from = 5, to = 25))
  
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
  if (lowess == TRUE){
    temp2 <- lowess.vad(temp2)
    temp2$di <- ConvertLongitude(temp2$di, 180)
    temp2$u <- -temp2$spd_smooth * sin(temp2$di*pi/180)
    temp2$v <- -temp2$spd_smooth * cos(temp2$di*pi/180)
  } else{
    temp2$di <- ConvertLongitude(temp2$di, 180)
    temp2$u <- -temp2$spd * sin(temp2$di*pi/180)
    temp2$v <- -temp2$spd * cos(temp2$di*pi/180)
  }
  temp2$date_time <- round_date(temp2$date_time, "minute")
  setDT(temp2)
  
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
# read.sup es una funcion que lee los archivos con los datos de superficie y los procesa 
# para que sean utilizables. Se encarga de las unidades, de agregar la altura, etc.
#=========================================================================================#

read.sup <- function(path){
  sup <- read.csv(path, sep = ",", na.strings = " ")
  
  # Ahora hay que trabajar sobre el archivo para que las variable de interés estén en las unidades 
  # que corresponden y tengan el nivel correspondiente
  
  sup$intensidad <- sup$intensidad * 0.5144 #Paso de nudos a m/s
  sup$direccion <- sup$direccion * 10 #Paso del formato synop a angulos entre 0 y 360
  sup$ht <- 0.01 #El viento se mide a 10 metros, la temperatura a dos. 
  sup$r <- exp(17.625*sup$tempd/(243.04 + sup$tempd))/exp(17.625*sup$temp/(243.04 + sup$temp))
  sup$es <- 6.11*exp(-2500000/461*(1/(sup$temp+273.15)-1/273.15))
  sup$e <- sup$r*sup$es
  sup$w <- 0.622*sup$e/(sup$presion_estacion-sup$e)
  sup$q <- sup$w/(sup$w+1)
  
  # Ahora la fecha y hora, miedo.
  sup$fecha <- dmy_hm(paste0(sup$fecha, " - ", sup$hora.utc, ":00"))
  
  sup
}

#=========================================================================================#
# convert.sup es una funcion que toma un data.frame con datos de superficie y lo organiza  
# para que sea combinablecon los vad.
#=========================================================================================#

convert.sup <- function(sup){
  sup <- sup[, .(fecha, ht, direccion, intensidad)]
  sup$u <- - sup$intensidad * sin(sup$direccion*pi/180)
  sup$v <- - sup$intensidad * cos(sup$direccion*pi/180)
  
  cnames <- c("X", "ht", "spd", "rmse1", "rmse2", "rmse3", "di", "rings", "date_time", "u", "v")
  
  sup <- data.frame(X = NA, 
                           ht = sup$ht, 
                           spd = sup$intensidad,
                           rmse1 = 0.514444/2,
                           rmse2 = 0.514444/2,
                           rmse3 = 0.514444/2,
                           di = sup$direccion,
                           rings = NA,
                           date_time = sup$fecha,
                           u = sup$u,
                           v = sup$v, 
                           spd_smooth = sup$intensidad)
  sup
}

#=========================================================================================#
# read.radar es una funcion que lee los archivos cfradial del radar para analizar dBZ y su 
# gradiente.
#=========================================================================================#


read.radar <- function(file, varname = 'dBZ') {
  radar <- nc_open(file)
  azimut <- ncvar_get(radar, 'azimuth')   
  elevacion <- ncvar_get(radar, 'elevation') 
  rango <- ncvar_get(radar, 'range')      
  dbz <- ncvar_get(radar, varname)
  datetime <- radar$dim$time$units %>% 
    strsplit(., "since") %>% 
    .[[1]] %>% 
    .[2] %>% 
    ymd_hms(tz = "UTC")
  second(datetime) <- 0
  dimnames(dbz) <- list(rango = rango, azimut = azimut)
  radar <- setDT(melt(dbz, value.name = "dbz")) 
  radar[, date := datetime]
  
  radar[, elevacion := elevacion, by = rango]
  R <- 6371.0*1000 #km
  Rp <- 4*R/3
  radar[, ht := sqrt((rango^2)+(Rp^2)+2*rango*Rp*sin(pi*elevacion/180))-Rp ]
  radar[elevacion < 6 & ht < 5000]
  
  
  ht_reg <- seq(0, 5000, by = 50)
  radar[, NNa := sum(!is.na(dbz)), by = .(azimut, elevacion, date)]
  reg <- radar[NNa > 3, .(ht_reg = ht_reg,
                          dbz_reg = approx(ht, dbz, xout = ht_reg)$y), 
               by = .(azimut, elevacion, date)] %>% 
    .[elevacion < 6, .(dbz = mean(dbz_reg, na.rm = T)), 
      by = .(ht_reg, date, elevacion)] 
  reg[, ddbz := metR:::.derv(dbz, ht_reg), by = .(elevacion, date)]
  reg
}

#=========================================================================================#
# ri.j calcula el número de Richarson bulk jet según Banta 2003
# acepta parámetros con unidades el sistema internacional
#=========================================================================================#
# t_i <- 27.7+273.15
# t_f <- 22.3+273.15
# p_i <- 1002.6
# p_f <- 1003.3
# u_max <- 16.51094
# z_max <- 300


ri <- function(t_i, t_f, p_i, p_f, u_max, z_max){
  g <- 9.8
  
  tita_i <- t_i*(p_i/1000)^(-0.286)
  tita_f <- t_f*(p_f/1000)^(-0.286)
  
  tita_m <- (tita_i + tita_f)/2
  ri<- (g * (tita_i - tita_f)/z_max) / (tita_m * (u_max/z_max)^2)
  return(ri)
}

#=========================================================================================#
# crea un data frame para trabajar con el número de Richarson bulk jet según Banta 2003
# requiere datos iniciales.
#=========================================================================================#

create.ri <- function(vad, date.sup, temp.sup, p.sup, ti, pi){
  ri_x <- vad %>% 
    group_by(date_time) %>% 
    summarise(u_max = max(spd_smooth, na.rm = T), 
              z_max = ht[which.max(spd_smooth)]) %>% 
    as.data.table()
  
  ri_x[, `:=`(t_i = ti + 273.15, 
              p_i = pi,
              t_f = approx(date.sup, temp.sup, xout = ri_x$date_time)$y + 273.15,
              p_f = approx(date.sup, p.sup, xout = ri_x$date_time)$y)]
  
  
  ri_x[, ri := ri(t_i, t_f, p_i, p_f, u_max, z_max*1000)]
  ri_x
}

#=========================================================================================#
# inside:
# Crop del dominio global para obtener un dominio circular de radio 40km centrado en el radar.
# Mantiene solo los puntos dentro del dominio
# 
# rrgange y azimuth.
# Convierten lat y lon en grilla del radar. Devuelve el rango y el azimuth.
#=========================================================================================#



inside <- function(lon, lat, lon0 = -60.537289, lat0 = -31.848438, r = 40000) {
  m <- matrix(c(lon, lat), ncol = 2)
  d <- geosphere::distGeo(m, c(lon0, lat0))
  d <= r
}

rrange <- function(lon, lat, lon0 = -60.537289, lat0 = -31.848438) {
  m <- matrix(c(lon, lat), ncol = 2)
  d <- geosphere::distGeo(m, c(lon0, lat0))
  d
}

azimuth <- function(lon, lat, lon0 = -60.537289, lat0 = -31.848438) {
  m <- matrix(c(lon, lat), ncol = 2)
  d <- geosphere::bearingRhumb(m, c(lon0, lat0))
  d <- ConvertLongitude(d - 180, 180)
}

#=========================================================================================#
# mode:
# Calcula la moda de x
#=========================================================================================#
 
mode <- function(x, ...) {
  if (length(x) > 1) {
    d <- density(x, ...)
    x[x %~% d$x[which.max(d$y)]]
  } else {
    x
  }
}


#=========================================================================================#
# Coeficientes de difusividad y perfil de u after Ulke 2000
# Cambia de acuerdo a la estabilidad.
#=========================================================================================#

kh.ulke <- function(z, h, L, ust, k = 0.4){
  # k <- 0.4

    khz <- vector(length = length(L))
    s <- sign(L) > 0
    khz[s] <- k*ust[s]*h[s]*(z[s]/h[s])*(1 - z[s]/h[s])*(1 + 9.2*z[s]/L[s])^(-1)
    khz[!s] <- k*ust[!s]*h[!s]*(z[!s]/h[!s])*(1 - z[!s]/h[!s])*(1 - 13*z[!s]/L[!s])^(1/2)  
    khz
}

km.ulke <- function(z, h, L, ust, k = 0.4){
  
  kmz <- vector(length = length(L))
  s <- sign(L) > 0
  kmz[s] <- k*ust[s]*h[s]*(z[s]/h[s])*(1 - z[s]/h[s])*(1 + 6.9*z[s]/L[s])^(-1)
  kmz[!s] <- k*ust[!s]*h[!s]*(z[!s]/h[!s])*(1 - z[!s]/h[!s])*(1 - 22*z[!s]/L[!s])^(1/4)
  kmz
}

u.ulke <- function(z, h, L, ust, k = 0.4, z0 = 0.05){
  # z0 <- 0.05 #Stull
  ustz <- ust*(1 - z/h)
  mu = (1 - 22*z/L)^(1/4)
  mu0 = (1 - 22*z0/L)^(1/4)
  
  uz <- vector(length = length(L))
  s <- sign(L) > 0
  uz[s] = (ust[s]/k)*(log(z[s]/z0) - (1 - 6.9*h[s]/L[s])*((z[s] - z0)/h[s]) - 
                        3.45 * (h[s]/L[s]) *(z[s]^2/h[s]^2 - z0^2/h[s]^2))
  uz[!s] = (ust[!s]/k)*(log(z[!s]/z0) + 
                          log((1 + mu0[!s]^2)*(1 + mu0[!s])^2/((1 + mu[!s]^2)*(1 + mu[!s])^2)) + 
                          2*(atan(mu[!s]) - atan(mu0[!s])) + 
                          2*L[!s]/(33*h[!s])*(mu[!s]^3 - mu0[!s]^3))  
  uz
}

#=========================================================================================#
# errores:
# rms, rre, bias, rmse, rmsens
#=========================================================================================#

rms <- function(p, p_ref){
  resta <- p - p_ref
  n <- length(p)
  rms <- sqrt(sum(resta^2, na.rm = T)/n)
  rms
}

rre <- function(p, p_ref){
  resta <- p - p_ref
  n <- length(p)
  rre = sqrt(sum(resta^2, na.rm = T)/sum(p_ref^2, na.rm = T))
  rre
}


bias.f <- function(mod, obs){
  
  N <- length(mod)
  resta <- mod - obs
  bias <- sum(resta, na.rm = T)/N
  bias
}

rmse.f <- function(mod, obs){
  
  N <- length(mod)
  resta <- mod - obs
  rmse <- sqrt(sum(resta^2, na.rm = T)/N)
  rmse
}


rmsens.f <- function(mod, obs){
  
  N <- length(mod)
  bias <- bias.f(mod, obs)
  resta <- mod - obs - bias
  rmsens <- sqrt(sum(resta^2, na.rm = T)/N)
  rmsens
}

bias.c <- function(dif.circ){
  bias <- sum(dif.circ, na.rm = T)/length(dif.circ)
  bias
}

rmse.c <- function(dif.circ){
  
  rmse <- sqrt(sum(dif.circ^2, na.rm = T)/length(dif.circ))
  rmse
}

rmsens.c <- function(dif.circ){

  bias <- bias.c(dif.circ)
  resta <- dif.circ - bias
  rmsens <- sqrt(sum(resta^2, na.rm = T)/length(dif.circ))
  rmsens
}

geom_label_contour2 <- function(...) {
  list(geom_label_contour(fill = "white", label.r = unit(0, "lines"),
                          label.padding = unit(0.04, "lines"), color = NA, ...),
       geom_text_contour(..., rotate = FALSE))
}
