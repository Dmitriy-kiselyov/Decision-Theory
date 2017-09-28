irisNames = c("setosa", "versicolor", "virginica")

#count distances

#help fields and functions
petals = iris[, 3:4]

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

#draw plot
irisColors = c("red", "green3", "blue")
names(irisColors) = irisNames

plot(petals, bg = irisColors[iris$Species], pch = 21, asp = 1, xlim = c(1, 7), ylim = c(-1, 3)) #iris
#legend("bottomright", irisNames, pch = 21, pt.bg = irisColors)

#main method
k = 10
for (x in seq(1, 7, 0.1)) {
    for (y in seq(-1, 3, 0.1)) {
        u = c(round(x, 1), round(y, 1)) #use round to aviod cases 0.1 + 0.2 = 0.3000000004
        if (any(apply(petals, 1, function(v) all(v == u)))) next
        #if trainig set contains u (O(n) - too long)
        groupName = closestGroup(u, k)
        #draw new point
        points(u[1], u[2], col = irisColors[groupName], pch = 20) #u
    }
}