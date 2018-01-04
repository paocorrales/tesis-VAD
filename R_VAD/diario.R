library(ggplot2)
library(reshape2)
library(lubridate)
library(RadioSonde)
library(gridBase)
library(grid)
library(magrittr)
library(data.table)
library(zoo)
library(dplyr)
library(mutate)
source("geom_arrow.R")
source("helpfun.R")


#Leo los datos con la funcion read.vad

vad_20160114 <- read.vad("../../20160114_240/vda*") #%>% lowess.vad()
vad_20170120 <- read.vad("../../20170120_240/vda*") %>% lowess.vad()
vad_20170121 <- read.vad("../../20170121_240/vda*") %>% lowess.vad()
vad_20170129 <- read.vad("../../20170129_240/vda*") %>% lowess.vad()
vad_20160116 <- read.vad("../../20160116_240/vda*") %>% lowess.vad()
vad_20160123 <- read.vad("../../20160123_240/vda*") %>% lowess.vad()
vad_20160122 <- read.vad("../../20160122_240/vda*") ##%>% lowess.vad()
vad_20160113 <- read.vad("../../20160113_240/vda*") #%>% lowess.vad()
vad_20160115 <- read.vad("../../20160115_240/vda*")


#Plots

dia <- rbind(vad_20160113, vad_20160114)
dia <- vad_20160114
perfiles <- subset(dia, ht < 1.0 & minute(date_time) == 00 & hour(date_time) %in% c(06,18))
tiempos <- subset(dia, minute(date_time) == 0)

# Campo de viento con errores
ggplot(dia, aes(date_time, ht)) + 
  geom_contour(aes(z = spd_smooth, color = ..level..), binwidth = 1) +
 # geom_contour(data = vad_20160114, aes(z = spd)) +
  scale_color_distiller(name = "Velocidad", type = "seq", palette = 8, direction = 1) + 
  geom_point(data = subset(dia, rmse1 > 0.5), aes(size = rmse1), shape = 1, color = "black") +
  geom_point(data = subset(dia, rmse2 > 0.5), aes(size = rmse2), shape = 4, color = "grey25") +
  ylim(c(0,2)) + xlab("Tiempo") + 
  ylab("Altura (km)") + 
  labs(title = paste0("Campo de viento para el ", as.Date(dia$date_time[1]))) +
  theme_minimal()
ggsave(paste0("Campo_", as.Date(dia$date_time[1]), ".png"), device = "png", path = "fig")

# Perfiles característicos
ggplot(perfiles, aes(ht, spd, color = as.factor(hour(date_time)))) + 
  geom_line() + 
  coord_flip() +
  scale_color_brewer(palette = "Set1", name="Tiempo", type = "qual") + 
  ylab("Viento (m/s)") + 
  xlab("Altura (Km)") + 
  labs(title = paste0("Perfiles de viento para el ", as.Date(dia$date_time[1]))) +
  theme_minimal()
#ggsave(paste0("Perfiles_", as.Date(dia$date_time[1]), ".png"), device = "png", path = "fig")

# Dirección del viento

ggplot(subset(tiempos, !is.na(spd)), aes(date_time, ht)) +
  #geom_arrow(data =  tiempos1, aes(mag = spd, angle = di), color = "red") +
  geom_arrow(aes(mag = spd, angle = di, color = spd)) +
  geom_point(aes(size = ifelse(rmse1 > 0.5, rmse1, NA)), shape = 1, color = "black") +
  geom_point(aes(size = ifelse(rmse2 > 0.5, rmse2, NA)), shape = 4, color = "grey25") +
  scale_size_continuous(range = c(0, 5), guide = "none") +
  scale_color_distiller(palette = "Oranges", name="Velocidad", direction = 1) +
  xlab("Tiempo (hora UTC)") + ylab("Altura (km)") +
  ylim(c(0,2)) + 
  scale_x_datetime(date_breaks = "2 hour", date_labels = "%H z") +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = paste0("Dirección del viento para el ", as.Date(dia$date_time[1]))) +
  theme_minimal() 
ggsave(paste0("Dirección_", as.Date(dia$date_time[1]), ".png"), device = "png", path = "fig")

# Hodógrafa
ggplot(perfiles, aes(u, v,  color = as.factor(hour(date_time)))) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(data=subset(perfiles, ht != 0.1), size = 1) +
  geom_point(data=subset(perfiles, ht == 0.1), shape = 17, size = 3) +
  geom_path() +
  scale_color_brewer(palette = "Set1", name="Hora", type = "qual") + 
  xlab("u (m/s)") + ylab("v (m/s)") +
  labs(title = paste0("Hodógrafa para el ", as.Date(dia$date_time[1]), "en distintos tiempos")) +
  #xlim(c(-10,14)) +
  #ylim(c(-10,14)) +
  coord_equal() +
  theme_minimal() 

ggsave(paste0("Hodografa_horaria_", as.Date(dia$date_time[1]), ".png"), device = "png", path = "fig")

# Hodógrafa

ggplot(subset(tiempos, ht %in% c(0.01, 0.3, 1.0)), aes(u, v,  color = as.factor(ht))) +
  geom_point(aes(x = ifelse(hour(date_time) != 0, u, NA)), size = 1) +
  geom_point(aes(x = ifelse(hour(date_time) == 0, u, NA)), shape = 17, size = 3) +
  geom_path() +
  scale_color_brewer(palette = "Set1", name="Nivel", type = "qual") + 
  xlab("u (m/s)") + ylab("v (m/s)") +
  labs(title = paste0("Hodógrafa para el ", as.Date(dia$date_time[1]), "para distintos niveles")) +
  #xlim(c(-12,10)) +
  #ylim(c(-14,10)) +
  coord_equal() +
  theme_minimal() 
ggsave(paste0("Hodografa_nivel_", as.Date(dia$date_time[1]), ".png"), device = "png", path = "fig")


#============================================
#Viento por niveles
ggplot(subset(dia, ht < 2.5), aes(date_time, spd)) + geom_line(color = "#FF3300") + facet_wrap(~ht) + 
  xlab("Tiempo (hora UTC)") + ylab("Viento (m/s)") + 
  scale_x_datetime(date_breaks = "6 hour", date_labels = "%H") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45))

#Anillos por niveles
ggplot(subset(dia, ht < 1.7), aes(date_time, rings)) + geom_point(color = "#FF6600") + facet_wrap(~ht) + 
  xlab("Tiempo (hora UTC)") + ylab("Anillos usados") + 
  scale_x_datetime(date_breaks = "6 hour", date_labels = "%H") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45))

#Error por niveles
gdata <- melt(vad_23_240, id.vars)
ggplot(subset(vad_23_240, ht < 1.7), aes(date_time, rmse)) + 
  geom_line(aes(color = "#fdbf6f")) + 
  scale_color_manual(values = c("#fdbf6f", "#ff7f00"),
                     labels = c("rmse1", "rmse2")) +
  geom_line(aes(date_time, rmse2, color = "#ff7f00")) +
  facet_wrap(~ht) + 
  scale_x_datetime(date_breaks = "6 hour", date_labels = "%H") +
  xlab("Tiempo (hora UTC)") +
  ylab("rmse (m/s)") + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 45))


#Perfil de viento para un par de tiempos
ggplot(subset(vad_20160114, ht < 1.5 & date_time == date_time[1125] | date_time == date_time[2550]), aes(ht, spd, color = as.factor(date_time))) + 
  geom_line() + 
  coord_flip() +
  scale_color_brewer(palette = "Oranges", name="Tiempo") + 
  ylab("Viento (m/s)") + xlab("Altura (Km)") + 
  theme_minimal()
#Perfil rmse para un par de tiempos
ggplot(subset(vad_14_240, ht < 1.5 & date_time == date_time[1125] | date_time == date_time[1200]), aes(ht, rmse, color = as.factor(date_time))) + 
  geom_line() + 
  coord_flip() +
  scale_color_brewer(palette = "Oranges", name="Tiempo") + 
  ylab("Viento (m/s)") + xlab("Altura (Km)") + 
  theme_minimal()

#Viento

temp <- subset(vad_20160114, minute(date_time) == 0)
#temp$di <- 270 - temp$di

ggplot(temp, aes(date_time, ht)) + 
  # geom_point() +
  # geom_spoke(aes(angle = di*pi/180, radius = 100*spd, color = spd))
  geom_arrow(aes(mag = spd, angle = di, color = spd)) +
  geom_point(data = subset(temp, rmse > 0.5), aes(size = rmse), shape = 1, color = "black") +
  geom_point(data = subset(temp, rmse2 > 0.5), aes(size = rmse2), shape = 4, color = "grey25") +
  scale_size_continuous(range = c(0, 10), guide = "none") +
  scale_color_distiller(palette = "Oranges", name="Velocidad", direction = 1) +
  xlab("Tiempo (hora UTC)") + ylab("Altura (km)") +
  ylim(c(0,2)) + 
  scale_x_datetime(date_breaks = "2 hour", date_labels = "%H:%m") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45))


#Perfil de viento para un tiempo
perfil <- read.csv("../20160114_240/elev_vda-2016-01-14T06:30:04Z_INTA_Parana.csv", sep = ";", na.strings = "-9999")
ggplot(perfil, aes(ht, spd, color = as.factor(elev))) + 
  geom_line() + 
  coord_flip() +
  xlim(c(0,1.5)) +
  scale_color_brewer(palette = "Oranges", name="Tiempo") + 
  ylab("Viento (m/s)") + xlab("Altura (Km)") + 
  theme_minimal()



#Comparación

ggplot(subset(vad_23_120, ht < 1.5), aes(date_time, spd)) + geom_line(color = "#FF3300") + 
  geom_line(data = subset(vad_23_240, ht < 1.5), color = "#FF9900") +
  facet_wrap(~ht) + 
  xlab("Tiempo") + ylab("Viento") + theme_minimal()




#Viento
perfil <- vad_14_240
perfil$di <- 360-perfil$di
perfil$time <- 1
with(perfil, plot(perfil$time, perfil$ht, pch=16))

vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)
# Plot
for (i in 1:nrow(perfil)) {
  pushViewport(viewport(
    x=unit(perfil$time[i], "native"),
    y=unit(perfil$ht[i], "native"), 
    angle=360-perfil$di[i]))
  wind_barb(perfil$spd[i])
  popViewport()
}

popViewport(3)

wind_barb <- function(x, mlength=0.1, wblength=0.025) {
  
  # Calculate which / how many barbs
  # any triangles (50)
  fif <- floor(x /50)
  # and then look for longer lines for remaining speed (10)
  tn <- floor( (x - fif* 50)/10)
  # and then look for shorter lines for remaining speed (5)
  fv <- floor( (x - fif* 50 - tn* 10)/5)
  
  # Spacing & barb length
  yadj <- 0.5+mlength
  dist <- (yadj-0.5) / 10    
  xadj <- 0.5+wblength
  xfadj <- 0.5+wblength/2        
  
  # Create grobs
  main_grob <- linesGrob(0.5, c(0.5, yadj ))
  
  # 50 windspeed
  if(fif != 0) {
    fify <- c(yadj, yadj-dist*seq_len(2* fif) )
    fifx <- c(0.5, xadj)[rep(1:2, length=length(fify))]
    fif_grob <- pathGrob(fifx, fify, gp=gpar(fill="black"))
  } else {
    fif_grob <- NULL
    fify <- yadj+dist
  }
  
  # Ten windspeed
  if(tn != 0) {
    tny <- lapply(seq_len(tn) , function(x) min(fify) - dist*c(x, x-1))  
    tn_grob <- do.call(gList, 
                       mapply(function(x,y) 
                         linesGrob(x=x, y=y, gp=gpar(fill="black")),
                         x=list(c(0.5, xadj)), y=tny, SIMPLIFY=FALSE))
  } else {
    tn_grob <- NULL
    tny <- fify
  }
  
  # Five windspeed
  if(fv != 0) {
    fvy <- lapply(seq_len(fv) , function(x) min(unlist(tny)) -dist* c(x, x-0.5))
    fv_grob <- do.call(gList, 
                       mapply(function(x,y) 
                         linesGrob(x=x, y=y, gp=gpar(fill="black")),
                         x=list(c(0.5, xfadj)), y=fvy, SIMPLIFY=FALSE))
  } else {
    fv_grob <- NULL
  }    
  
  # Draw    
  #grid.newpage()
  grid.draw(gList(main_grob, fif_grob, tn_grob, fv_grob))
}
