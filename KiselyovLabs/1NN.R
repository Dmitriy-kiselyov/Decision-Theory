#help fields and functions
dist = function(p1, p2) sqrt(sum((p1 - p2) ^ 2))

closestGroup = function(u) {
    irisDist = apply(petals, 1, dist, u)
    minIndex = max(which(irisDist == min(irisDist))) #last index, because there are equal dots in irises (lst dots are drawn on plot)
    name = iris[minIndex, 5]
    return (name)
}

#init global values
irisNames = c("setosa", "versicolor", "virginica")
petals = iris[, 3:4]
xlim = c(1, 7)
ylim = c(-1, 3)
#petals = iris[, 1:2]
#xlim = c(4, 8)
#ylim = c(1.5, 4.5)
step = 0.1

#draw plot
irisColors = c("red", "green3", "blue")
names(irisColors) = irisNames

plot(petals, bg = irisColors[iris$Species], pch = 21, asp = 1, xlim = xlim, ylim = ylim) #iris
#legend("bottomright", irisNames, pch = 21, pt.bg = irisColors)

#main method
for (x in seq(xlim[1], xlim[2], step)) {
    for (y in seq(ylim[1], ylim[2], step)) {
        u = c(x, y)
        groupName = closestGroup(u)
        #draw new point
        points(u[1], u[2], col = irisColors[groupName], pch = 20) #u
    }
}