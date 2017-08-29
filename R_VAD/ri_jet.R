# Estudio de la turbulencia aproximando el número de Richarson

ri_20160114 <- as.data.frame(unique(vad_20160114$date_time))

ri_20160114 <- vad_20160114 %>% 
  group_by(date_time) %>% 
  summarise(u_max = max(spd.smooth, na.rm = T), 
            z_max = ht[which.max(spd.smooth)])

ri_20160114$t_i <- 27.7+273.15
ri_20160114$p_i <- 1003.3

rep(subset(sup, day(sup$fecha) ==14)$temp, each = 6)


temp_surf <- approxfun(sup$hora.utc, sup$temp)
ri_20160114$t_f <- approx(sup$fecha, sup$temp, xout = ri_20160114$date_time)$y +273
ri_20160114$p_f <- approx(sup$fecha, sup$presion_estacion, xout = ri_20160114$date_time)$y

ri_20160114$ri <- with(ri_20160114, ri.j(t_i, t_f, p_i, p_f, u_max, z_max*1000))

ggplot(dia, aes(date_time, ht)) + 
  geom_contour(aes(z = spd, color = ..level..), binwidth = 1) +
  scale_color_distiller(name = "Velocidad", type = "seq", palette = 8, direction = 1) + 
  geom_point(data = ri_20160114, aes(date_time, z_max), color = "blue")
  
ggplot(ri_20160114, aes(date_time, ri)) +
  geom_point() +
  geom_point(aes(y = u_max), color = "red") +
  geom_point(aes(y = z_max), color = "blue")
