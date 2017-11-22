#distances
mc.euclideanDistance = function(p1, p2) sqrt(sum((p1 - p2) ^ 2))
mc.getDistances = function(points, u, distFunction) apply(points, 1, distFunction, u)

#plot
plot.limits = function(arr, deviation = 0) {
    c(min(arr) - deviation, max(arr) + deviation)
}

plot.functionGradient <- function(x, y, col, n = 500) {
    plot(x, y, type = "n", xlab = "", ylab = "")
    e <- par('usr')
    len = length(x)
    pal <- if (!is.function(col)) colorRampPalette(col)(n) else col(n)
    pal <- pal[as.numeric(cut(y, n))]
    # plot rectangles to simulate colour gradient
    x_prev = x[1]
    for (i in 1:(len - 1)) {
        x_next = x[i] + (x[i + 1] - x[i]) / 2
        rect(x_prev, max(y), x_next, min(y), col = pal[i], border = NA)
        x_prev = x_next
    }
    rect(x_prev, max(y), x[len], min(y), col = pal[i], border = NA)
    # plot white polygons representing the inverse of the area of interest
    polygon(c(min(x), x, max(x), rev(x)),
          c(e[4], ifelse(y > 0, y, 0),
            rep(e[4], length(y) + 1)), col = 'white', border = NA)
    polygon(c(min(x), x, max(x), rev(x)),
          c(e[3], ifelse(y < 0, y, 0),
            rep(e[3], length(y) + 1)), col = 'white', border = NA)
    box()
}

#time
time.format = function(time, precision = 2, ms = F) {
    if (!is.na(time["elapsed"]))
        time = time["elapsed"]

    unit = "s"
    if (ms) {
        unit = "ms"
        time = time * 1000
    }
        
    paste("Time=", round(time, precision), unit, sep = "")
}