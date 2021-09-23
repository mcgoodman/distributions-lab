# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(tidyverse)
library(shiny)

theme_set(theme_minimal() + theme(text = element_text(size = 16)))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Lab: Exploring Distributions"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "dist",
                label = "Statistical Distribution",
                choices = c(
                    "Normal", "Poisson", "Binomial", "Beta", "Pareto"
                )),
            uiOutput("params"),
            uiOutput("xlim"),
            fluidPage(
                sliderInput("sample_size", "Sample size", 1, 500, 10, 1),
                sliderInput("n_samples", "Number of samples", 10, 1000, 50, 10)
            )
        ),


        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
             tabPanel("Density Function", plotOutput("density"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    source("distributions.R")

    dist <- reactive({
         switch(
            input$dist,
            "Normal" = normal,
            "Poisson" = poisson,
            "Binomial" = binomial,
            "Beta" = beta,
            "Pareto" = pareto
        )
    })

    output$params <- renderUI({
        if (length(dist()$pars) == 1){
            fluidPage(
                sliderInput("par1", dist()$par[1], min = dist()$scale[[1]][1],
                            max = dist()$scale[[1]][2], value = dist()$scale[[1]][3],
                            step = dist()$scale[[1]][4])
            )
        } else if (length(dist()$pars) > 1){
            fluidPage(
                sliderInput("par1", dist()$par[1], min = dist()$scale[[1]][1],
                            max = dist()$scale[[1]][2], value = dist()$scale[[1]][3],
                            step = dist()$scale[[1]][4]),
                sliderInput("par2", dist()$par[2], min = dist()$scale[[2]][1],
                            max = dist()$scale[[2]][2], value = dist()$scale[[2]][3],
                            step = dist()$scale[[2]][4])
            )
        }
    })

#    output$xlim <- renderUI({
#        if(input$dist == "Beta" | input$dist == "Binomial" | input$dist == "Poisson") {
#            fluidPage()
#        } else {
#            fluidPage(
#                numericInput("xmin", "x minimum", dist()$xlim[1], min = dist()$xlim[3], max = dist()$xlim[4]),
#                numericInput("xmax", "x maximum", dist()$xlim[2], min = dist()$xlim[3], max = dist()$xlim[4])
#            )
#        }
#    })

    output$xlim <- renderUI({
        if(input$dist == "Normal") {
            fluidPage(
                numericInput("xmin", "x minimum", dist()$xlim[1], min = dist()$xlim[3], max = dist()$xlim[4]),
                numericInput("xmax", "x maximum", dist()$xlim[2], min = dist()$xlim[3], max = dist()$xlim[4])
            )
        } else if (input$dist == "Pareto") {
            fluidPage(
                numericInput("xmax", "x maximum", dist()$xmax, min = 1)
            )
        } else {
            fluidPage()
        }
    })

    output$density <- renderPlot({
        if (length(dist()$pars) == 1){
            dist()$plot(input$par1)
        } else if (input$dist == "Pareto") {
            dist()$plot(input$par1, input$par2, xmax = input$xmax)
        } else {
            dist()$plot(input$par1, input$par2, xlim = c(input$xmin, input$xmax))
        }
    })

}

# Run the application
shinyApp(ui = ui, server = server)
