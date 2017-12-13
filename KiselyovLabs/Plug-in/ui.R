makeRow = function(label, id1, id2, min, max, value, value2 = value) {
    fluidRow(
        column(2, h5(label)),
        column(5,
            sliderInput(
                inputId = id1,
                label = NULL,
                min = min,
                max = max,
                value = value
            )
        ),
        column(5,
            sliderInput(
                inputId = id2,
                label = NULL,
                min = min,
                max = max,
                value = value2
            )
        )
    )
}

ui <- fluidPage(
    titlePanel("Plug-in"),

    sidebarLayout(
    
        sidebarPanel(

            fluidRow(
                column(2),
                column(5, h2("Класс 1", style = "color: blue")),
                column(5, h2("Класс 2", style = "color: green"))
            ),

            makeRow("Кол-во", "n1", "n2", 5, 1000, 50),

            makeRow("Разброс по X", "Ex1", "Ex2", 1, 10, 5),

            makeRow("Разброс по Y", "Ey1", "Ey2", 1, 10, 5),

            makeRow("Отклонение X", "Mx1", "Mx2", -10, 10, 0, 10),

            makeRow("Отклонение Y", "My1", "My2", -10, 10, 0),

            fluidRow(
                column(2, h5("Итоговая ковариационнная матрица")),
                column(5, tableOutput("covar1"), align = "center"),
                column(5, tableOutput("covar2"), align = "center")
            )

        ),

        mainPanel(

            plotOutput(outputId = "plot", height = "600px")

        )
    
    )
 
)