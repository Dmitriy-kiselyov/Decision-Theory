#help functions
source("metricHelp.R")
source("kernelHelp.R")

#KERNEL
mc.PW.kernel = mc.kernel.Q #use this kernel

#PW
mc.PW = function(distances, classes, u, h) {
    distances = distances / h
    distances = mc.PW.kernel(distances)

    sumByName = function(name, arr) sum(arr[names(arr) == name])
    classes = unique(classes)
    wei = sapply(classes, sumByName, distances)
    names(wei) = classes

    return(names(which.max(wei)))
}

#LOO
mc.LOO.PW = function(points, classes, hLimits) {
    n = dim(points)[1]
    looY = rep(0, length(hLimits))

    for (i in 1:n) {
        u = points[i,]
        teachSample = points[-i,]
        distances = mc.getDistances(teachSample, u, mc.euclideanDistance)
        names(distances) = classes[-i]

        for (j in 1:length(hLimits)) {
            h = hLimits[j]
            bestClass = mc.PW(distances, classes[-i], u, h)
            looY[j] = looY[j] + ifelse(bestClass == classes[i], 0, 1)
        }
    }

    looY = looY / n
    return(looY)
}

#DRAWINGS
mc.draw.LOO.PW = function(points, classes, hLimits) {
    looY = mc.LOO.PW(points, classes, hLimits)

    #draw
    plot(hLimits, looY, type = "l", main = "LOO для PW", xlab = "h", ylab = "LOO")

    h.opt = hLimits[which.min(looY)]
    loo.min = looY[which.min(looY)]

    points(h.opt, loo.min, pch = 19, col = "red")
    text(h.opt, loo.min, labels = paste("h=", h.opt, sep = ""), pos = 3, col = "red", family = "mono")

    return(h.opt) #for future use
}

mc.draw.PW = function(points, classes, colors, h, xlim, ylim, step) {
    uniqueClasses = unique(classes)
    names(colors) = uniqueClasses
    plot(points, bg = colors[classes], pch = 21, asp = 1, xlim = xlim, ylim = ylim, main = "Карта классификации по PW") #known data
    #legend("topright", legend = uniqueClasses, pch = 20, pt.bg = colors, xpd = T)

    #guess
    for (x in seq(xlim[1], xlim[2], step)) {
        for (y in seq(ylim[1], ylim[2], step)) {
            u = c(round(x, 1), round(y, 1)) #use round to aviod cases 0.1 + 0.2 = 0.3000000004
            if (any(apply(points, 1, function(v) all(v == u)))) next #do not classify known points

            distances = mc.getDistances(points, u, mc.euclideanDistance)
            names(distances) = classes
            bestClass = mc.PW(distances, classes, u, h)

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
    h.opt = mc.draw.LOO.PW(petals, petalNames, hLimits = seq(0.1, 1, 0.1))
    mc.draw.PW(petals, petalNames, colors = c("red", "green3", "blue"), h = h.opt, xlim = plot.limits(petals[, 1], 0.2), ylim = plot.limits(petals[, 2], 0.2), step = 0.1)
}