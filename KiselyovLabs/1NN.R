irisNames = c("setosa", "versicolor", "virginica")

#count distances

#help fields and functions
petals = iris[, 3:4]

dist = function(p1, p2) sqrt(sum((p1 - p2) ^ 2))

closestGroup = function(u) {
    irisDist = apply(petals, 1, dist, u)
    minIndex = max(which(irisDist == min(irisDist))) #last index, because there are equal dots in irises (lst dots are drawn on plot)
    name = iris[minIndex, 5]
    return (name)
}

#draw plot
irisColors = c("red", "green3", "blue")
names(irisColors) = irisNames

plot(iris[, 3:4], bg = irisColors[iris$Species], pch = 21, asp = 1) #iris
#legend("bottomright", irisNames, pch = 21, pt.bg = irisColors)

#main method
for (x in seq(1, 7, 0.1)) {
    for (y in seq(-1, 3, 0.1)) {
        u = c(x, y)
        groupName = closestGroup(u)
        #draw new point
        points(u[1], u[2], col = irisColors[groupName], pch = 20) #u
    }
}