library(MASS)

## Восстановление центра нормального распределения
estimateMu = function(points) {
    rows = dim(points)[1]
    cols = dim(points)[2]
    mu = matrix(NA, 1, cols)
    for (col in 1:cols) {
        mu[1, col] = mean(points[, col])
    }
    return(mu)
}

## Восстановление ковариационной матрицы нормального распределения
estimateCovarianceMatrix = function(points, mu) {
    rows = dim(points)[1]
    cols = dim(points)[2]
    covar = matrix(0, cols, cols)
    for (i in 1:rows) {
        covar = covar + (t(points[i,] - mu) %*% (points[i,] - mu)) / (rows - 1)
    }
    return(covar)
}

## Получение коэффициентов разделяющей поверхности
getPlugInDiskriminantCoeffs = function(mu1, sigma1, mu2, sigma2) {
    # Уравнение : a*x1^2 + b*x1*x2 + c*x2^2 + d*x1 + e*x2 + f = 0
    invSigma1 = solve(sigma1)
    invSigma2 = solve(sigma2)
    f = log(abs(det(sigma1))) - log(abs(det(sigma2))) + mu1 %*% invSigma1 %*% t(mu1) - mu2 %*% invSigma2 %*% t(mu2);
    alpha = invSigma1 - invSigma2
    a = alpha[1, 1]
    b = 2 * alpha[1, 2]
    c = alpha[2, 2]
    beta = invSigma1 %*% t(mu1) - invSigma2 %*% t(mu2)
    d = -2 * beta[1, 1]
    e = -2 * beta[2, 1]
    return(c("x^2" = a, "xy" = b, "y^2" = c, "x" = d, "y" = e, "1" = f))
}

server = function(input, output) {
    generateData = function() {
        n1 = input$n1
        n2 = input$n2

        covar1 = matrix(c(input$Ex1, 0, 0, input$Ey1), 2, 2)
        covar2 = matrix(c(input$Ex2, 0, 0, input$Ey2), 2, 2)
        mu1 = c(input$Mx1, input$My1)
        mu2 = c(input$Mx2, input$My2)
        xy1 = mvrnorm(n1, mu1, covar1)
        xy2 = mvrnorm(n2, mu2, covar2)

        list("xy1" = xy1, "xy2" = xy2)
    }

    drawPoints = function(xy1, xy2) {
        x = rbind(cbind(xy1, 1), cbind(xy2, 2))
        colors = c("blue", "green")
        plot(x[, 1], x[, 2], pch = 21, bg = colors[x[, 3]], asp = 1, xlab = "X", ylab = "Y")
    }

    estimateCoeffs = function(xy1, xy2) {
        mu1 = estimateMu(xy1)
        mu2 = estimateMu(xy2)
        covar1 = estimateCovarianceMatrix(xy1, mu1)
        covar2 = estimateCovarianceMatrix(xy2, mu2)

        #выводим итоговую матрицу
        output$covar1 = renderTable({
            colnames(covar1) = c(NA, NA)
            covar1
        }, include.colnames = FALSE)
        output$covar2 = renderTable({
            colnames(covar2) = c(NA, NA)
            covar2
        }, include.colnames = FALSE)

        coeffs = getPlugInDiskriminantCoeffs(mu1, covar1, mu2, covar2)
        coeffs
    }

    output$plot = renderPlot({
        #Создаем тестовые данные
        data = generateData()
        xy1 = data$xy1
        xy2 = data$xy2

        #Рисуем точки
        drawPoints(xy1, xy2)

        # Поиск параметров
        coeffs = estimateCoeffs(xy1, xy2)

        # Рисуем дискриминантую функцию
        x = y = seq(-10, 20, len = 100)
        z = outer(x, y, function(x, y) coeffs["x^2"] * x ^ 2 + coeffs["xy"] * x * y + coeffs["y^2"] * y ^ 2 + coeffs["x"] * x + coeffs["y"] * y + coeffs["1"])
        contour(x, y, z, levels = 0, drawlabels = FALSE, lwd = 3, col = "red", add = TRUE)

    })
}