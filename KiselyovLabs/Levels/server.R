norma = function(x, y, mu, sigma) {
    x = matrix(c(x, y), 1, 2)

    k = 1 / sqrt((2 * pi) ^ 2 * det(sigma))
    e = exp(-0.5 * (x - mu) %*% solve(sigma) %*% t(x - mu))
    k * e
}

server = function(input, output) {
    output$plot = renderPlot({
        #Создаем тестовые данные
        mu = matrix(c(input$Mx, input$My), 1, 2)
        sigma = matrix(c(input$Ex, 0, 0, input$Ey), 2, 2)

        # Рисуем дискриминантую функцию
        minX = mu[1] - sigma[1, 1] - 2
        maxX = mu[1] + sigma[1, 1] + 2
        minY = mu[2] - sigma[2, 2] - 2
        maxY = mu[2] + sigma[2, 2] + 2

        x = seq(minX, maxX, len = 100)
        y = seq(minY, maxY, len = 100)
        z = outer(x, y, function(x, y) {
            sapply(1:length(x), function(i) norma(x[i], y[i], mu, sigma))
        })

        from = 0.001
        to = 0.2
        
        add = F
        for (level in seq(from, to, by = 0.005)) {
            contour(x, y, z, levels = level, drawlabels = T, lwd = 1, col = "black", add = add, asp = 1)
            add = T
        }
    })
}