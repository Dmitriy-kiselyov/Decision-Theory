source("help.R")
source(file = "KNN.R", encoding = "UTF-8")

mc.STOLP.w = function(points, classes, u, classes.from) {
    #KNN
    k = 6
    distances = mc.getDistances(points, u, mc.euclideanDistance)
    names(distances) = classes
    distances = sort(distances)[1:k]

    #compute only this
    ind = which(sapply(names(distances), function(v) any(v == classes.from)))
    if (length(ind) == 0) return(0)
    distances = distances[ind]

    max(table(names(distances)))
}

mc.STOLP.margin = function(points, classes, u, class) {
    m1 = mc.STOLP.w(points, classes, u, class)
    m2 = mc.STOLP.w(points, classes, u, classes[-which(classes == class)])
    m1 - m2
}

mc.STOLP.margins = function(points, classes) {
    n = length(classes)
    margins = rep(0, n)
    for (i in 1:n) margins[i] = mc.STOLP.margin(points, classes, points[i, ], classes[i])

    return(margins)
}

mc.draw.STOLP.margins = function(margins) {
    n = length(margins)
    margins = sort(margins)
    colors = colorRampPalette(c("red", "green"))

    plot.polygonGradient(1:n, margins, colors)
    lines(1:n, margins, lwd = 3, col = "blue")
    lines(c(1, n), c(0, 0), col = "grey", lwd = 2)
    title(main = "График упорядоченных по возрастанию отступов (KNN при k=6)", ylab = "Отступ (М)", xlab = "Объекты выборки")

    ox = seq(0, 150, 5)
    axis(side = 1, at = ox)
    sapply(ox, function(x) abline(v = x, col = "grey", lty = 3))
}

mc.STOLP = function(points, classes, noise.bound, mistakes.limit) {
    #classes may be factor, should convert to array
    classes = as.array(levels(classes))[classes]
    n = length(classes)

    #Remove all M < noise
    for (i in 1:n) {
        if (i > n) break
        if (mc.STOLP.margin(points, classes, points[i, ], classes[i]) <= noise.bound) {
            points = points[-i,]
            classes = classes[-i]
            n = n - 1
        }
    }

    #Choose one represantative from each class
    etalones = data.frame()
    etalones.classes = c()
    for (class in unique(classes)) {
        ind = which(classes == class)
        margins = sapply(ind, function(i) mc.STOLP.margin(points, classes, points[i,], class))
        max.i = ind[which.max(margins)]
        print(margins)

        etalones = rbind(etalones, points[max.i,])
        etalones.classes = c(etalones.classes, class)
        points = points[-max.i,]
        classes = classes[-max.i]
        n = n - 1
    }
    names(etalones) = names(points)

    #Choose objects until mistake is satisfying
    while (n > 0) {
        margins = c()
        margins.i = c()
        for (i in 1:n) {
            m = mc.STOLP.margin(etalones, etalones.classes, points[i,], classes[i])
            if (m <= 0) {
                margins = c(margins, m)
                margins.i = c(margins.i, i)
            }
        }

        print(margins)

        if (length(margins) <= mistakes.limit) break

        min.i = margins.i[which.min(margins)]
        etalones = rbind(etalones, points[min.i,])
        etalones.classes = c(etalones.classes, classes[min.i])
        points = points[-min.i,]
        classes = classes[-min.i]
        n = n - 1
    }

    #return
    list(points.etalones = etalones, classes.etalones = etalones.classes, points.rest = points, classes.rest = classes)
}

mc.draw.STOLP = function(points.etalones, classes.etalones, points.rest, classes.rest, colors) {
    uniqueClasses = unique(classes.etalones)
    names(colors) = uniqueClasses

    plot(points.rest, col = colors[classes.rest], pch = 21, asp = 1, main = "STOLP для KNN")
    points(points.etalones, bg = colors[classes.etalones], pch = 21)
}

#STOLP
main = function() {
    points = iris[, 3:4]
    classes = iris[, 5]

    time = system.time(attach(mc.STOLP(points, classes, -3, 5)))

    #draw STOLP
    mc.draw.STOLP(points.etalones, classes.etalones, points.rest, classes.rest, c("red", "green3", "blue"))
    title(sub = time.format(time, precision = 2), font.sub = 3, cex.sub = 0.8)

    #draw classification
    xlim = plot.limits(points[,1], 0.2)
    ylim = plot.limits(points[, 2], 0.2)

    par(mfrow = c(1, 2), xpd = NA)
    mc.draw.KNN(points.etalones, classes.etalones, c("red", "green3", "blue"), k = 6, xlim = xlim, ylim = ylim, step = 0.1, title = F)
    title(main = "Карта классификации KNN на выборке отфильтрованной STOLP")
    mc.draw.KNN(points, classes, c("red", "green3", "blue"), k = 6, xlim = xlim, ylim = ylim, step = 0.1)
}

#MARGINS
main2 = function() {
    points = iris[, 3:4]
    classes = iris[, 5]
    margins = mc.STOLP.margins(points, classes)
    mc.draw.STOLP.margins(margins)
}