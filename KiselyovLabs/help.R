#distances
mc.euclideanDistance = function(p1, p2) sqrt(sum((p1 - p2) ^ 2))
mc.getDistances = function(points, u, distFunction) apply(points, 1, distFunction, u)

#plot
plot.limits = function(arr, deviation = 0) {
    c(min(arr) - deviation, max(arr) + deviation)
}

plot.polygonGradient <- function(x, y, col, n = 500) {
    # x, y: the x and y coordinates
    # col: a vector of colours (hex, numeric, character), or a colorRampPalette
    # n: the vertical resolution of the gradient
    # ...: further args to plot()
    plot(x, y, type = "n", xlab = "", ylab = "")
    e <- par('usr')
    height <- diff(e[3:4]) / (n - 1)
    y_up <- seq(0, e[4], height)
    y_down <- seq(0, e[3], - height)
    y_all <- c(rev(y_down), y_up)
    ncolor <- length(y_all)
    pal <- if (!is.function(col)) colorRampPalette(col)(ncolor) else col(ncolor)
    # plot rectangles to simulate colour gradient
    sapply(seq_len(n),
         function(i) {
             rect(min(x), y_all[i], max(x), y_all[i] + height, col = pal[i], border = NA)
         })
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