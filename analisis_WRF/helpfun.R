# Crop del dominio global para obtener un dominio circular de radio 40km centrado en el radar.

inside <- function(lon, lat, lon0 = -60.537289, lat0 = -31.848438, r = 40000) {
  m <- matrix(c(lon, lat), ncol = 2)
  d <- geosphere::distGeo(m, c(lon0, lat0))
  d <= r
}

# Calcula la moda de x  
mode <- function(x, ...) {
  if (length(x) > 1) {
    d <- density(x, ...)
    x[x %~% d$x[which.max(d$y)]]
  } else {
    x
  }
}


# Coeficientes de difusividad Ulke 2000

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