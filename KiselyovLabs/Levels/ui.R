makeJoinedRow = function(label, id, min, max, value) {
    fluidRow(
        column(2, h5(label)),
        column(10,
            sliderInput(
                inputId = id,
                label = NULL,
                min = min,
                max = max,
                value = value
            )
        )
    )
}

ui <- fluidPage(
    titlePanel("Линии уровня"),

    sidebarLayout(
    
        sidebarPanel(

            makeJoinedRow("Разброс по X", "Ex", 1, 10, 1),

            makeJoinedRow("Разброс по Y", "Ey", 1, 10, 1),

            makeJoinedRow("Отклонение X", "Mx", -10, 10, 0),

            makeJoinedRow("Отклонение Y", "My", -10, 10, 0)

        ),

        mainPanel(

            plotOutput(outputId = "plot", height = "600px")

        )
    
    )
 
)