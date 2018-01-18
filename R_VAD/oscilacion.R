# Modelo de oscilación inercial after Van de Wiel et al
# 
# Usa el modelo de Blackadar conservando la fricción y sumando Ekman


# Perfil inicial: 20/21 hora local del día anterior
# Uso 21 UTC osea 18 local
u0 <- subset(vad_20160113, hour(date_time) == 22 & minute(date_time) == 0)$u
v0 <- subset(vad_20160113, hour(date_time) == 22 & minute(date_time) == 0)$v

# Perfil de equilibrio, usando la aproximación de Ekman y viento geostrífico
# sacado de la galera. 
f <- 4*pi*sin(-32*pi/180)/(24*3600)
ht <- subset(vad_20160113, hour(date_time) == 22 & minute(date_time) == 0)$ht*1000
ug <- -7  + 6*ht/2000
vg <- 5  - 5*ht/2000

gama <- 1/80 # Según el paper REVISAR
k <- 0.5
gama <- 1 / sqrt(2*k/abs(f))

ue <- ug - ug*exp(-gama*ht)*cos(gama*ht) - vg*exp(-gama*ht)*sin(gama*ht)
ve <- vg - vg*exp(-gama*ht)*cos(gama*ht) + ug*exp(-gama*ht)*sin(gama*ht)

# Perfil modelizado

#t <- vad_20160114$date_time - ymd_hms("2016-01-13 21:00:00")
# 21 UTC / 3UTC / 6 UTC
t <- 8*3600
u <- ue + (v0 - ve)*sin(f*t) + (u0 - ue)*cos(f*t)
v <- ve + (v0 - ve)*cos(f*t) - (u0 - ue)*sin(f*t)

spd_io <- sqrt(u^2 + v^2)


with(subset(vad_20160114, date_time == date_time[1048]), plot(spd_smooth, ht*1000, type = "l"))
points(spd_io, ht)

plot(spd_io, ht, type = "b")
with(subset(vad_20160114, date_time == date_time[1048]), lines(spd_smooth, ht*1000))


