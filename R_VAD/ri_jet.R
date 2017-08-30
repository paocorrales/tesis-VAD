# Estudio de la turbulencia aproximando el número de Richarson

ri_20160114 <- as.data.frame(unique(vad_20160114$date_time))

ri_20160114 <- vad_20160114 %>% 
  group_by(date_time) %>% 
  summarise(u_max = max(spd_smooth, na.rm = T), 
            z_max = ht[which.max(spd_smooth)])

ri_20160114$t_i <- 27.7+273.15 #21 UTC 13/01/2017
ri_20160114$p_i <- 1003.3

temp_surf <- approxfun(sup$hora.utc, sup$temp)
ri_20160114$t_f <- approx(sup$fecha, sup$temp, xout = ri_20160114$date_time)$y + 273
ri_20160114$p_f <- approx(sup$fecha, sup$presion_estacion, xout = ri_20160114$date_time)$y

ri_20160114$delta_tita <- with(ri_20160114, (t_i*(p_i/1000)^(-0.286)) - (t_f*(p_f/1000)^(-0.286)))

ri_20160114$ri <- with(ri_20160114, ri.j(t_i, t_f, p_i, p_f, u_max, z_max*1000))

ggplot(dia, aes(date_time, ht)) + 
  geom_contour(aes(z = spd, color = ..level..), binwidth = 1) +
  scale_color_distiller(name = "Velocidad", type = "seq", palette = 8, direction = 1) + 
  geom_point(data = ri_20160114, aes(date_time, z_max), color = "blue")
  
# Ri_j a lo largo del tiempo durante el máximo de viento observado
# 
ggplot(subset(ri_20160114, hour(date_time) %in% c(4,5,6,7,8)), aes(date_time, ri)) +
  geom_point() +
  geom_hline(yintercept = 0.25) +
  ylab("Ri_j") +
  xlab("Tiempo (Hora UTC)") +
  theme_minimal()

ggplot(subset(ri_20160114, hour(date_time) %in% c(4,5,6,7,8)), aes(date_time, u_max)) +
  geom_point() +
  #geom_hline(yintercept = 0.25) +
  ylab("Umax (m/s)") +
  xlab("Tiempo (Hora UTC)") +
  theme_minimal()
  
ggplot(subset(ri_20160114, hour(date_time) %in% c(4,5,6,7,8)), aes(date_time, delta_tita/(z_max*1000))) +
  geom_point() +
  #geom_hline(yintercept = 0.25) +
  ylab("dtita/dz (K/m)") +
  xlab("Tiempo (Hora UTC)") +
  theme_minimal()

ggplot(subset(ri_20160114, hour(date_time) %in% c(4,5,6,7,8)), aes(date_time, z_max*1000)) +
  geom_point() +
  #geom_hline(yintercept = 0.25) +
  ylab("Zmax (m)") +
  xlab("Tiempo (Hora UTC)") +
  theme_minimal()

#================================================================================
#

ri_20160123 <- as.data.frame(unique(vad_20160123$date_time))

selectmax <- function(spd, ht) {
  m <- which.max(spd)
  if (length(m) == 0) {
    r <- NA
  } else {
    r <- ht[m]
  }
  return(r)
}

ri_20160123 <- vad_20160123 %>% 
  group_by(date_time) %>% 
  summarise(u_max = max(spd_smooth, na.rm = T), 
            z_max = selectmax(spd_smooth, ht))


ri_20160123$t_i <- 34.2+273.15
ri_20160123$p_i <- 998.8

temp_surf <- approxfun(sup$hora.utc, sup$temp)
ri_20160123$t_f <- approx(sup$fecha, sup$temp, xout = ri_20160123$date_time)$y + 273
ri_20160123$p_f <- approx(sup$fecha, sup$presion_estacion, xout = ri_20160123$date_time)$y

ri_20160123$delta_tita <- with(ri_20160123, (t_i*(p_i/1000)^(-0.286)) - (t_f*(p_f/1000)^(-0.286)))

ri_20160123$ri <- with(ri_20160123, ri.j(t_i, t_f, p_i, p_f, u_max, z_max*1000))

ggplot(dia, aes(date_time, ht)) + 
  geom_contour(aes(z = spd, color = ..level..), binwidth = 1) +
  scale_color_distiller(name = "Velocidad", type = "seq", palette = 8, direction = 1) + 
  geom_point(data = ri_20160123, aes(date_time, z_max), color = "blue")

# Ri_j a lo largo del tiempo durante el máximo de viento observado
# 
ggplot(subset(ri_20160123, hour(date_time) %in% c(0,1,2,3,4,5,6,7,8)), aes(date_time, ri)) +
  geom_point() +
  geom_hline(yintercept = 0.25) +
  ylab("Ri_j") +
  xlab("Tiempo (Hora UTC)") +
  theme_minimal()

ggplot(subset(ri_20160123, hour(date_time) %in% c(0,1,2,3,4,5,6,7,8)), aes(date_time, u_max)) +
  geom_point() +
  #geom_hline(yintercept = 0.25) +
  ylab("Umax (m/s)") +
  xlab("Tiempo (Hora UTC)") +
  theme_minimal()

ggplot(subset(ri_20160123, hour(date_time) %in% c(0,1,2,3,4,5,6,7,8)), aes(date_time, delta_tita/(z_max*1000))) +
  geom_point() +
  #geom_hline(yintercept = 0.25) +
  ylab("dtita/dz (K/m)") +
  xlab("Tiempo (Hora UTC)") +
  theme_minimal()

ggplot(subset(ri_20160123, hour(date_time) %in% c(0,1,2,3,4,5,6,7,8)), aes(date_time, z_max*1000)) +
  geom_point() +
  #geom_hline(yintercept = 0.25) +
  ylab("Zmax (m)") +
  xlab("Tiempo (Hora UTC)") +
  theme_minimal()



