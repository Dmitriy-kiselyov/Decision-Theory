#help functions
source("help.R")
source("kernelHelp.R")

#KERNEL
mc.PW.kernel = NULL #use this kernel

#PW
mc.PW = function(distances, classes, u, h) {
    distances = distances / h
    distances = mc.PW.kernel(distances)

    sumByName = function(name, arr) sum(arr[names(arr) == name])
    classes = unique(classes)
    wei = sapply(classes, sumByName, distances)
    names(wei) = classes

    if (max(wei) == 0) return("no")

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
    hs = NULL
    loos = NULL

    for (kernelName in names(mc.kernels)) {
        mc.PW.kernel <<- mc.kernels[[kernelName]] #set kernel

        time = system.time(looY <- mc.LOO.PW(points, classes, hLimits))

        #draw
        plot(hLimits, looY, type = "l", main = paste("LOO для PW (ядро: ", kernelName, ")", sep = ""), sub = time.format(time), font.sub = 3, cex.sub = 0.8, xlab = "h", ylab = "LOO")

        h.opt = hLimits[which.min(looY)]
        loo.min = looY[which.min(looY)]

        points(h.opt, loo.min, pch = 19, col = "red")
        text(h.opt, loo.min, labels = paste("h=", h.opt, ", Loo=", round(loo.min, 3), sep = ""), pos = 3, col = "red", family = "mono")

        hs = c(hs, h.opt)
        loos = c(loos, loo.min)
    }

    bestIndex = which.min(loos)
    mc.PW.kernel = mc.kernels[bestIndex] #set best kernel as default

    return(hs[bestIndex]) #for future use
}

mc.draw.PW = function(points, classes, colors, h, xlim, ylim, step) {
    uniqueClasses = unique(classes)
    names(colors) = uniqueClasses
    plot(points, bg = colors[classes], pch = 21, asp = 1, xlim = xlim, ylim = ylim, main = "Карта классификации по PW") #known data

    #add 'no' class
    colors = c(colors, "no" = "yellow")

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
            bestClass = mc.PW(distances, classes, u, h)

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
    par(xpd = NA)
    layout(matrix(c(1, 2, 5, 5, 3, 4, 5, 5), 2, 4, byrow = T))
    h.opt = mc.draw.LOO.PW(petals, petalNames, hLimits = seq(0.1, 2, 0.05))
    mc.draw.PW(petals, petalNames, colors = c("red", "green3", "blue"), h = h.opt, xlim = plot.limits(petals[, 1], 0.2), ylim = plot.limits(petals[, 2], 0.2), step = 0.1)
}