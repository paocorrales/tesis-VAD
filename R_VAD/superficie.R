#Incorporando datos de superficie

sup <- read.csv("../../Superficie/parana2016.csv", sep = ";", na.strings = " ")

# Ahora hay que trabajar sobre el archivo para que las variable de interés estén en las unidades 
# que corresponden y tengan el nivel correspondiente

sup$intensidad <- sup$intensidad * 0.5144 # Paso de nudos a m/s
sup$direccion <- sup$direccion * 10 #Paso del formato synop a angulos entre 0 y 360
sup$ht <- 0.01

# Ahora la fecha y hora, miedo.
sup$fecha <- dmy_hm(paste0(sup$fecha, " - ", sup$hora.utc, ":00"))

keep <- c("fecha", "ht", "direccion", "intensidad")
sup <- sup[keep]
sup$u <- sup$intensidad * cos(sup$direccion*pi/180)
sup$v <- sup$intensidad * sin(sup$direccion*pi/180)

cnames <- c("X", "ht", "spd", "rmse1", "rmse2", "rmse3", "di", "rings", "date_time", "u", "v")

superficie <- data.frame(X = NA, 
                         ht = sup$ht, 
                         spd = sup$intensidad,
                         rmse1 = NA,
                         rmse2 = NA,
                         rmse3 = NA,
                         di = sup$direccion,
                         rings = NA,
                         date_time = sup$fecha,
                         u = sup$u,
                         v = sup$v, 
                         spd_smooth = sup$intensidad)


vad_20160114$date_time <- round_date(vad_20160114$date_time, "minute")
vad_20160114 <- rbind(subset(superficie, day(date_time) == 14), vad_20160114)

vad_20160113$date_time <- round_date(vad_20160113$date_time, "minute")
vad_20160113 <- rbind(subset(superficie, day(date_time) == 13), vad_20160113)
