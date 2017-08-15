# Intento de corregir inconsistencias temporales. 

# Loess parece funcionar

group_by(vad_20170120, ht) %>% 
  mutate(dv = c(NA, NA, diff(diff(spd)))) %>% 
  mutate(dvs = as.numeric(scale(dv))) %>% 
  ggplot(aes(date_time, spd)) +
  geom_line() + #geom_smooth(span = 0.2) +
  facet_wrap(~ht, scales = "free_y")


vad_20160114n %>% 
  group_by(ht) %>% 
  mutate(spd.smooth = loess.smooth(date_time, spd, span = 0.12, evaluation = length(spd))$y)

# Convierte mis datos en data.table
vad.dt <- as.data.table(vad_20160114n)

# Calculo el loess para cada altura. Span pequeño para que la regresión sea mas localizada.
vad.dt[, spd.smooth := loess.smooth(date_time, spd, span = 0.06, evaluation = .N)$y, 
       by = ht]
vad.dt[, spd.smooth := loess(spd ~ as.numeric(date_time), span = 0.75, evaluation = .N, 
                             na.action = na.exclude)$y, by = ht]

# Cuenta NA en una ventana de 6 datos (equivalente a una hora)
vad.dt[, na.acum := zoo::rollsum(is.na(spd), k = 6, fill = NA), by = ht]

ggplot(vad.dt, aes(date_time)) +
  #geom_col(aes(y = na.acum)) +
  geom_line(aes(y = spd)) +
  geom_line(aes(y = spd.smooth), color = "red") +
  facet_wrap(~ht) +
  ylab("Velocidad (m/s)") + 
  theme_minimal()

ggplot(subset(vad.dt, ht < 2.0), aes(date_time, ht)) + 
  # meteoR::stat_contour_fill(aes( z = spd.smooth)) +
  geom_contour(aes(z = ifelse(na.acum <= 4, spd.smooth, NA)), color = "red", binwidth = 1) +
  scale_colour_gradient(name = "Velocidad", low = "grey", high = "black") + 
  # geom_tile(fill = "white", data = vad.dt[na.acum >= 5]) +
  geom_contour(aes(z = spd, color = ..level..), binwidth = 1) +
  geom_point(aes(size = ifelse(rmse2 >0.5, rmse2, NA)), shape = 1) +
  ylab("Altura (Km)") +
  xlab("Tiempo") + 
  theme_minimal()

test <- vad_20170120 %>% 
  group_by(ht) %>% 
  filter(ht == 1.8) %>% 
  mutate(spd.input = approx(date_time, spd, xout = date_time)$y)

ggplot(subset(test, ht == 1.8), aes(date_time, spd)) +
  geom_line() + geom_point()
