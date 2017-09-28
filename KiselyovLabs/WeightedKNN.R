#help fields and functions
dist = function(p1, p2) sqrt(sum((p1 - p2) ^ 2))

#Weight function
w = function(i) {
    return((k + 1 - i) / k)
}

#Weighted KNN method
closestGroup = function(u, k) {
    irisDist = apply(petals, 1, dist, u)
    names(irisDist) = iris[, 5]
    minKDist = sort(irisDist)[1:k]

    #count weights
    sumByName = function(name, arr) {
        sum(arr[names(arr) == name])
    }

    wei = replace(minKDist, T, 1:k)
    wei = w(wei)
    wei = sapply(irisNames, sumByName, wei)

    return(names(which.max(wei)))
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
k = 10

#draw plot
irisColors = c("red", "green3", "blue")
names(irisColors) = irisNames

plot(petals, bg = irisColors[iris$Species], pch = 21, asp = 1, xlim = xlim, ylim = ylim) #iris
#legend("bottomright", irisNames, pch = 21, pt.bg = irisColors)

#main method
for (x in seq(xlim[1], xlim[2], step)) {
    for (y in seq(ylim[1], ylim[2], step)) {
        u = c(round(x, 1), round(y, 1)) #use round to aviod cases 0.1 + 0.2 = 0.3000000004
        if (any(apply(petals, 1, function(v) all(v == u)))) next
        #if trainig set contains u (O(n) - too long)
        groupName = closestGroup(u, k)
        #draw new point
        points(u[1], u[2], col = irisColors[groupName], pch = 20) #u
    }
}