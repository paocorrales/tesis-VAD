---
title: "Prueba dBZ"
author: "Paola Corrales"
date: "January 15, 2018"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

# librerías
library(metR)
library(ggplot2)
library(ggforce)
library(lubridate)
library(magrittr)
library(dplyr)
library(dtplyr)
library(viridis)
library(directlabels)
library(data.table)
library(patchwork)
library(ncdf4)
#source("geom_contourlabel.R")
source("helpfun.R")
```

```{r}
radar <- nc_open("seudocache/cfrad.20160114_060005.000_to_20160114_060431.001_INTA_Parana_SUR.nc")
azimut <- ncvar_get(radar, 'azimuth')   
elevacion <- ncvar_get(radar, 'elevation') 
rango <- ncvar_get(radar, 'range')      
V <- ncvar_get(radar, 'V')            
Vda <- ncvar_get(radar, 'Vda')
dbz <- ncvar_get(radar, 'dBZ')
Vda[Vda > 99999] <- NA
elev <- unique(elevacion)

R <- 6371.0*1000 #km
Rp <- 4*R/3
propagation <- expand.grid(r = rango, tita = elev)
ht <- with(propagation, sqrt((r^2)+(Rp^2)+2*r*Rp*sin(pi*tita/180))-Rp)

dimnames(V) <- list(rango = rango, azimut = azimut)
radar <- setDT(melt(V, value.name = "V")) 
radar[, elevacion := elevacion, by = rango]
radar[, ht := sqrt((rango^2)+(Rp^2)+2*rango*Rp*sin(pi*elevacion/180))-Rp ]

radar[, Vda := c(Vda)]
radar[, dbz := c(dbz)]

ggplot(radar[azimut %between% c(210, 212)], aes(ht, rango)) + 
  geom_point(size = 0.1) + 
  coord_flip()


radar[elevacion == elev[3] & !is.na(dbz), ] %>%
  RepeatLon(., colname = "azimut") %>% 
  ggplot(aes(azimut, rango/1000)) + 
  geom_path(data = map[rango <= 300*1000], aes(group = group), color = "darkgray") +
  geom_point(aes(color = dbz, size = rango)) +
  #scale_color_divergent() +
  scale_size_area(max_size = 0.1) +
  scale_x_continuous(name = NULL, labels = NULL, breaks = seq(0, 359, 90)) +
  scale_y_continuous(name = "Distancia al centro (Km)", breaks = c(40, 120, 180, 240), 
                     limits = c(0, 300), expand = c(0, 0)) + 
  guides(size = "none") +
  coord_polar() +
  theme_minimal() + theme(legend.position = "bottom")
```

```{r}
# Interpolo a una grilla regular
# 
ht_reg <- seq(0, 50000, by = 10)
reg <- radar[, .(ht_reg = ht_reg,
                 dbz_reg = approx(ht, dbz, xout = ht_reg)$y), 
        by = .(azimut, elevacion)] %>% 
  .[, mean(dbz_reg, na.rm = T), 
    by = .(ht_reg, elevacion)] %>% 
  ggplot(aes(elevacion, ht_reg)) +
  geom_tile(aes(z = V1))


```

