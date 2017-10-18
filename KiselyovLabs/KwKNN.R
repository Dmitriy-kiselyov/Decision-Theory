#help functions
source("help.R")

#KNN
mc.KwKNN.w = function(i, k) return((k + 1 - i) / k)

mc.KwKNN = function(sortedDistances, k) {
    minKDistances = sortedDistances[1:k]

    #use weight function
    sumByName = function(name, arr) {
        sum(arr[names(arr) == name])
    }
    wei = mc.KwKNN.w(1:k, k)
    names(wei) = names(minKDistances)
    wei = sapply(unique(names(wei)), sumByName, wei)

    return(names(which.max(wei)))
}

#LOO
mc.LOO.KwKNN = function(points, classes) {
    n = dim(points)[1]
    looY = rep(0, n-1) #n-1 because one element in sample always missing

    for (i in 1:n) {
        u = points[i,]
        teachSample = points[-i,]

        distances = mc.getDistances(points, u, mc.euclideanDistance)[-i]
        names(distances) = classes[-i]
        sortedDistances = sort(distances)

        for (k in 1:n - 1) {
            bestClass = mc.KwKNN(sortedDistances, k)
            looY[k] = looY[k] + ifelse(bestClass == classes[i], 0, 1)
        }
    }

    looY = looY / n
    return(looY)
}

#DRAWINGS
mc.draw.LOO.KwKNN = function(points, classes) {
    time = system.time(looY <- mc.LOO.KwKNN(points, classes))

    #draw
    plot(1:length(looY), looY, type = "l", main = "LOO для взвешенного KNN", sub = time.format(time), font.sub = 3, cex.sub = 0.8, xlab = "K", ylab = "LOO")
    k.opt = which.min(looY)
    points(k.opt, looY[k.opt], pch = 19, col = "red")
    text(k.opt, looY[k.opt], labels = paste("K=", k.opt, ", Loo=", round(looY[k.opt], 3), sep = ""), pos = 3, col = "red", family = "mono")

    return(k.opt) #for future use
}

mc.draw.KwKNN = function(points, classes, colors, k, xlim, ylim, step) {
    uniqueClasses = unique(classes)
    names(colors) = uniqueClasses
    plot(points, bg = colors[classes], pch = 21, asp = 1, xlim = xlim, ylim = ylim, main = "Карта классификации по взвешенному KNN") #known data
    #legend("topright", legend = uniqueClasses, pch = 20, pt.bg = colors, xpd = T)

    #guess
    time = Sys.time()

    xSeq = seq(xlim[1], xlim[2], step)
    ySeq = seq(ylim[1], ylim[2], step)
    for (x in xSeq) {
        for (y in ySeq) {
            u = c(round(x, 1), round(y, 1)) #use round to aviod cases 0.1 + 0.2 = 0.3000000004
            if (any(apply(points, 1, function(v) all(v == u)))) next #do not classify known points

            distances = mc.getDistances(points, u, mc.euclideanDistance)
            names(distances) = classes
            bestClass = mc.KwKNN(sort(distances), k)

            #draw new point
            points(u[1], u[2], col = colors[bestClass], pch = 21) #u
        }
    }

    time = Sys.time() - time
    time = time / length(xSeq) / length(ySeq) #average
    title(sub = paste(time.format(time, precision = 0, ms = T), " for each point", sep = ""), font.sub = 3, cex.sub = 0.8)
}

#test
main = function() {
    petals = iris[, 3:4]
    petalNames = iris[, 5]

    #draw
    par(mfrow = c(1, 2), xpd = NA)
    k.opt = mc.draw.LOO.KwKNN(petals, petalNames)
    mc.draw.KwKNN(petals, petalNames, colors = c("red", "green3", "blue"), k = k.opt, xlim = plot.limits(petals[, 1], 0.2), ylim = plot.limits(petals[, 2], 0.2), step = 0.1)
}