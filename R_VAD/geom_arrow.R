
geom_arrow <- function(aes, direction = -1, start = -90, ...) {
    # geom para graficar flechas más fácilmente.
    # aes requeridos :
    #  * mag (magnitud) y angle (ángulo en radianes)
    #  * vx (velocidad en dirección x) y vy (velocidad en dirección y)
    #
    # Otros parámetros:
    #   direction: dirección del ángulo. 1 para antihorario, -1 para horario
    #   start: ángulo de inicio. 0 para empezar desde el eje x, -90 para
    #   el ángulo meteorológico
    #   ... : otros parámetros para geom_text
    if (!is.null(aes$angle) & !is.null(aes$mag)) {
        angle <- deparse(aes$angle)
        aes.angle <- paste0(start,  "+", direction, "*", angle)
        geom_text(aes_string(size = aes$mag,
                             angle = aes.angle,
                             color = aes$colour),
                  label = "\u27f6", ...)
    } else if (!is.null(aes$vy) & !is.null(aes$vx)) {
        aes.angle <- paste0("atan2(", aes$vy, ", ", aes$vx, ")*180/pi")
        aes.size <- paste0("sqrt(", aes$vx, "^2 + ", aes$vy, "^2)")
        geom_text(aes_string(size = aes.size, angle = aes.angle,
                             color = aes$colour), label = "\u27f6", ...)
    } else {
        stop("geom_arrow needs either angle and mag or vx and vy")
    }
}
