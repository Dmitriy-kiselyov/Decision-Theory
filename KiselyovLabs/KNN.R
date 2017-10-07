#help functions
source("metricHelp.R")

#KNN
mc.KNN = function(sortedDistances, k) {
    minKDistances = sortedDistances[1:k]
    minKClasses = names(minKDistances)
    bestClass = names(which.max(table(minKClasses)))
    return(bestClass)
}

#LOO
mc.LOO.KNN = function(points, classes) {
    n = dim(points)[1]
    looY = rep(0, n-1) #n-1 because one element in sample always missing

    for (i in 1:n) {
        u = points[i,]
        teachSample = points[-i,]

        distances = mc.getDistances(points, u, mc.euclideanDistance)[-i]
        names(distances) = classes[-i]
        sortedDistances = sort(distances)

        for (k in 1:n-1) {
            bestClass = mc.KNN(sortedDistances, k)
            looY[k] = looY[k] + ifelse(bestClass == classes[i], 0, 1)
        }
    }

    looY = looY / n
    return(looY)
}

#DRAWINGS
mc.draw.LOO.KNN = function(points, classes) {
    looY = mc.LOO.KNN(points, classes)

    #draw
    plot(1:length(looY), looY, type = "l", main = "LOO для KNN", xlab = "K", ylab = "LOO")
    k.opt = which.min(looY)
    points(k.opt, looY[k.opt], pch = 19, col = "red")
    text(k.opt, looY[k.opt], labels = paste("K=", k.opt, sep = ""), pos = 3, col = "red", family = "mono")

    return(k.opt) #for future use
}

mc.draw.KNN = function(points, classes, colors, k, xlim, ylim, step) {
    uniqueClasses = unique(classes)
    names(colors) = uniqueClasses
    plot(points, bg = colors[classes], pch = 21, asp = 1, xlim = xlim, ylim = ylim, main = "Карта классификации по KNN") #known data
    #legend("topright", legend = uniqueClasses, pch = 20, pt.bg = colors, xpd = T)

    #guess
    for (x in seq(xlim[1], xlim[2], step)) {
        for (y in seq(ylim[1], ylim[2], step)) {
            u = c(round(x, 1), round(y, 1)) #use round to aviod cases 0.1 + 0.2 = 0.3000000004
            if (any(apply(points, 1, function(v) all(v == u)))) next #do not classify known points

            distances = mc.getDistances(points, u, mc.euclideanDistance)
            names(distances) = classes
            bestClass = mc.KNN(sort(distances), k)

            #draw new point
            points(u[1], u[2], col = colors[bestClass], pch = 21) #u
        }
    }
}

#test
main = function() {
    petals = iris[, 3:4]
    petalNames = iris[, 5]

    #draw
    par(mfrow = c(1, 2))
    k.opt = mc.draw.LOO.KNN(petals, petalNames)
    mc.draw.KNN(petals, petalNames, colors = c("red", "green3", "blue"), k = k.opt, xlim = plot.limits(petals[, 1], 0.2), ylim = plot.limits(petals[, 2], 0.2), step = 0.1)
}