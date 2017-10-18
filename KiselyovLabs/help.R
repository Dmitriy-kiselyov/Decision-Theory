#distances
mc.euclideanDistance = function(p1, p2) sqrt(sum((p1 - p2) ^ 2))
mc.getDistances = function(points, u, distFunction) apply(points, 1, distFunction, u)

#plot
plot.limits = function(arr, deviation = 0) {
    c(min(arr) - deviation, max(arr) + deviation)
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