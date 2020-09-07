#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Fit! That! Line!"),
    
    plotOutput('line_and_fit'),
    
    hr(),
    
    fluidRow(
        column(4,
               h4("Simulation Parameters"),
               sliderInput("n", "Sample Size", value = 25, 5, 100),
               numericInput("a", "Intercept", 0, -10, 10),
               numericInput("b", "Slope", 1, -10, 10),
               numericInput("rsd", "Residual SD", 3, 0.1, 10)
        ),
        
        column(4,
               h4("Objective function - function to minimize"),
               br(),
               "Can use n, yhat (fitted value of y), y, x, resid_sd",
               textInput("fn", "Func:", "sum((yhat-y)^2)"),
               br(),
               "sum((yhat-y)^2) is least squares regression"),
        
        column(4,
               h4("Properties of Objective Function"),
               "Bias",
               "SE")
    ),
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #the simulated data reactive
    dat <- reactive({
        x <- rnorm(input$n)
        y <- rnorm(input$n, input$a + input$b*x, input$rsd)
        data.frame(x = x,
                   y = y)
    })
    
    obj <- reactive({

        #the objective function
        obj_fn <- function(pars){
            
            #variables for objective
            x <- dat()$x
            y <- dat()$y
            n <- input$n
            
            a <- pars[1]
            b <- pars[2]
            
            yhat <- a+b*x
            resid_sd <- sd(yhat-y)
            #sum((yhat-y)^2)
            sum(eval(parse(text = input$fn)))
        }
        
        #the fit
        fit <- optim(c(0,0), obj_fn)
        
        fit$par
    })
    
    #make that plot
    output$line_and_fit <- renderPlot({
        pars <- obj()
        
        ggplot(data = dat(),
               aes(x = x, y = y)) +
            geom_point(size = 2.1) +
            theme_classic() +
            geom_abline(intercept = pars[1],
                        slope = pars[2])
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
