#install.packages("plotrix")
require("plotrix")
source("help.R")
source("kernelHelp.R")

mc.PF.kernel = mc.kernel.T

mc.PF = function(distances, classes, potentials, h) {
    wei = potentials * mc.PF.kernel(distances / h)
    names(wei) = classes

    sumByName = function(name, arr) sum(arr[names(arr) == name])
    classes = unique(classes)
    wei = sapply(classes, sumByName, wei)

    max.i = which.max(wei)
    if (wei[max.i] == 0) return("no")

    classes[max.i]
}

mc.PF.potentials = function(points, classes, h, mistakes.limit, log = F) {
    n = dim(points)[1]
    potentials = rep(0, n)

    mistakes.made = mistakes.limit + 1
    while (mistakes.made > mistakes.limit) {
        #Select parameters
        flag = T
        while (flag) {
            i = sample(1:n, 1)
            u = points[i,]
            distances = mc.getDistances(points, u, mc.euclideanDistance)

            if (mc.PF(distances, classes, potentials, h) != classes[i]) {
                potentials[i] = potentials[i] + 1
                flag = F
            }
        }

        #Calculate mistake
        mistakes.made = 0
        for (i in 1:n) {
            u = points[i,]
            distances = mc.getDistances(points, u, mc.euclideanDistance)

            if (mc.PF(distances, classes, potentials, h) != classes[i]) {
                mistakes.made = mistakes.made + 1
                if (!log && mistakes.made > mistakes.limit) break #save time
            }
        }

        if (log) {
            print(mistakes.made)
            print(potentials)
        }    
    }

    return(potentials)
}

mc.draw.PF = function(points, classes, potentials, h, colors, xlim, ylim, step) {
    #draw irises
    uniqueClasses = unique(classes)
    names(colors) = uniqueClasses
    plot(points, bg = colors[classes], pch = 21, asp = 1, xlim = xlim, ylim = ylim)
    title(main = "Карта классификации по потенциальным функциям")

    #draw potentials
    densities = potentials / max(potentials)
    n = length(potentials)
    for (i in 1:n) {
        x = points[i, 1]
        y = points[i, 2]
        rad = h[i]
        d = densities[i] / 2 #all are transparent
        color = adjustcolor(colors[classes[i]], d)

        if (d != 0) draw.circle(x, y, rad, 50, border = color, col = color)
    }

    #draw irised on top
    points(points, bg = colors[classes], pch = 21)

    #draw classified objects
    time = Sys.time()
    
    colors = c(colors, "no" = "yellow") #add 'no' class

    xSeq = seq(xlim[1], xlim[2], step)
    ySeq = seq(ylim[1], ylim[2], step)
    for (x in xSeq) {
        for (y in ySeq) {
            u = c(round(x, 1), round(y, 1)) #use round to aviod cases 0.1 + 0.2 = 0.3000000004
            if (any(apply(points, 1, function(v) all(v == u)))) next #do not classify known points

            distances = mc.getDistances(points, u, mc.euclideanDistance)
            bestClass = mc.PF(distances, classes, potentials, h)

            #draw new point
            points(u[1], u[2], col = colors[bestClass], pch = 18) #u
        }
    }

    time = Sys.time() - time
    time = time / length(xSeq) / length(ySeq) #average
    title(sub = paste(time.format(time, precision = 0, ms = T), " for each point", sep = ""), font.sub = 3, cex.sub = 0.8)
}

#test function
main = function() {
    points = iris[, 3:4]
    classes = iris[, 5]
    n = dim(points)[1]

    h = rep(1, n)

    time = system.time(potentials <- mc.PF.potentials(points, classes, h = h, 4, log = T))
    print(time.format(time))

    xlim = plot.limits(points[, 1], 0.2)
    ylim = plot.limits(points[, 2], 0.2)
    mc.draw.PF(points, classes, potentials, h, colors = c("red", "green3", "blue"), xlim, ylim, 0.1)
}