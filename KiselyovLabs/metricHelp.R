#distances
mc.euclideanDistance = function(p1, p2) sqrt(sum((p1 - p2) ^ 2))
mc.getDistances = function(points, u, distFunction) apply(points, 1, distFunction, u)

#plot
plot.limits = function(arr, deviation = 0) {
    c(min(arr) - deviation, max(arr) + deviation)
}