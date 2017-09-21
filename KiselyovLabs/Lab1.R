u = c(2, 2) #some random point

irisNames = c("setosa", "versicolor", "virginica")

#count means points for each group
irisMean = function(name) {
    irisByName = iris[iris$Species == name, 3:4]
    c(mean(irisByName[, 1]), mean(irisByName[, 2]))
}

irisMeans = t(sapply(irisNames, irisMean))

#count distances
dist = function(p1, p2)(p1[1] - p2[1]) ^ 2 + (p1[2] - p2[2]) ^ 2
irisDist = apply(irisMeans, 1, dist, u)

#print closest group of flowers to point u
irisNames[which.min(irisDist)]

#draw plot
irisColors = c("red", "green3", "blue")
names(irisColors) = irisNames

plot(iris[, 3:4], col = irisColors[iris$Species], pch = 20, asp = 1) #iris
legend("bottomright", irisNames, pch = 20, col = irisColors)

points(u[1], u[2], col = "black", pch = 20) #u

points(irisMeans[, 1], irisMeans[, 2], col = "black", bg = "yellow", pch = 21) #mean point for each group