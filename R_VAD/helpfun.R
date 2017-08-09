library(reshape2)
library(lubridate)


# read.vad es una funcion que lee los archivos con los datos de un vad para una determinada
# fecha. Recibe la ubicación de los archivos y devuelve un dataframe con los datos.

read.vad <- function(path){
  
  filename <- (Sys.glob(path))
  datetime <- ymd_hms(stringi::stri_sub(filename, from = 21, to = 41))
  
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
  

