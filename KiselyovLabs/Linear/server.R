library(MASS)

# Нормализация обучающей выборки
linear.normalize = function(xl) {
    cols = dim(xl)[2]
    for (i in 1:(cols - 1)) {
        xl[, i] = (xl[, i] - mean(xl[, i])) / sd(xl[, i])
    }

    return(xl)
}

# Добавляем w0 = -1 как третью колонку
linear.prepare = function(xl) {
    rows = dim(xl)[1]
    cols = dim(xl)[2]
    xl = cbind(xl[, 1:(cols - 1)], rep(-1, rows), xl[, cols])
}

# Функции потерь
linear.adaline.L = function(m) {
    (m - 1) ^ 2
}

linear.perceptron.L = function(m) {
    max(-m, 0)
}

linear.logit.L = function(m) {
    log2(1 + exp(-m))
}

# Привила обновления весов
linear.adaline.update = function(w, eta, xi, yi) {
    w - eta * (sum(w * xi) - yi) * xi
}

linear.perceptron.update = function(w, eta, xi, yi) {
    w + eta * yi * xi
}

linear.logit.update = function(w, eta, xi, yi) {
    sigmoid = function(z) {
        1 / (1 + exp(-z))
    }

    w + eta * xi * yi * sigmoid(-sum(w * xi) * yi)
}

server = function(input, output) {
    generateData = reactive({
        n1 = input$n1
        n2 = input$n2

        covar1 = matrix(c(input$Ex1, 0, 0, input$Ey1), 2, 2)
        covar2 = matrix(c(input$Ex2, 0, 0, input$Ey2), 2, 2)
        mu1 = c(input$Mx1, input$My1)
        mu2 = c(input$Mx2, input$My2)
        xy1 = mvrnorm(n1, mu1, covar1)
        xy2 = mvrnorm(n2, mu2, covar2)

        list("xy1" = xy1, "xy2" = xy2)
    })

    drawPoints = function(xl) {
        colors = c("blue", "white", "green")
        plot(xl[, 1], xl[, 2], pch = 21, bg = colors[xl[, 4] + 2], asp = 1, xlab = "X", ylab = "Y")
    }

    # Стохастический градиент
    linear = function(xl, L, update) {
        #default
        eta = 1 / 5
        lambda = 1 / length(xl)

        rows = dim(xl)[1]
        cols = dim(xl)[2]
        w = rep(1 / 2, cols - 1)

        # initialize Q
        Q = 0
        for (i in 1:rows) {
            margin = sum(w * xl[i, 1:(cols - 1)]) * xl[i, cols]
            Q = Q + L(margin)
        }
        Q.prev = Q

        repeat {
            # select the error objects
            margins = rep(0, rows)
            for (i in 1:rows) {
                xi = xl[i, 1:(cols - 1)]
                yi = xl[i, cols]
                margins[i] = sum(w * xi) * yi
            }
            errorIndecies = which(margins <= 0)

            #stop algorithm if all objects are divided correctly
            if (length(errorIndecies) == 0) {
                output$log = renderText({
                    "Найден точный ответ"
                });
                break;
            }

            # select random index from the errors            
            i = sample(errorIndecies, 1)
            xi = xl[i, 1:(cols - 1)]
            yi = xl[i, cols]

            # calculate error
            margin = sum(w * xi) * yi
            error = L(margin)
            w = update(w, eta, xi, yi)

            # Calculate new Q
            Q = (1 - lambda) * Q + lambda * error

            #exit if Q is stable
            if (abs(Q.prev - Q) < 0.01) {
                output$log = renderText({
                    "Точный ответ не найден"
                });
                break;
            }
            Q.prev = Q
        }

        return(w)
    }

    L = list("ada" = linear.adaline.L, "per" = linear.perceptron.L, "log" = linear.logit.L)
    update = list("ada" = linear.adaline.update, "per" = linear.perceptron.update, "log" = linear.logit.update)

    output$plot = renderPlot({
        #Создаем тестовые данные
        data = generateData()
        xy1 = data$xy1
        xy2 = data$xy2
        xl = rbind(cbind(xy1, -1), cbind(xy2, +1))

        # Нормализация данных
        xl = linear.normalize(xl)
        xl = linear.prepare(xl)

        #Рисуем точки
        drawPoints(xl)

        # Поиск разделяющей поверхности
        w = linear(xl, L[[input$algo]], update[[input$algo]])

        # Рисуем разделяющую поверхность
        abline(a = w[3] / w[2], b = -w[1] / w[2], lwd = 3, col = "red")
    })
}