library(ncdf4)
library(reshape2)
library(lubridate)
library(ggplot2)
library(data.table)

REA_20160114_nc <- nc_open("reanalisis/20160114.nc")
hgt <- ncvar_get(REA_20160114_nc, 'hgt')
time <- ncvar_get(REA_20160114_nc, 'time')
lon <- ncvar_get(REA_20160114_nc, 'lon')
lat <- ncvar_get(REA_20160114_nc, 'lat')
lev <- ncvar_get(REA_20160114_nc, 'level')
time <- as.POSIXct(time*3600, tz = "GTM", origin="1800-01-01 00:00:00")

REA_20160114_nc <- metR::ReadNetCDF("reanalisis/20160114.nc")
dimnames(hgt) <- list(lat, lon, lev, date = as.character(time))
hgt_20160114 <- melt(hgt, value.name = "hgt", varnames = c("lat", "lon", "lev", "date"))
hgt_20160114$date <- ymd_hms(hgt_20160114$date)

ggplot(subset(hgt_20160114, day(date) == 14 & lev == 1000), aes(lon, lat)) + 
  geom_contour(aes(z = hgt, color = ..level..)) +
  facet_wrap(~hour(date))
