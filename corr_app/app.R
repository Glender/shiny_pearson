#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(MASS)

ui <- fixedPage(
    
    # Correlation plot
    titlePanel("Strength of Pearson's Correlation Coefficient:"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            # Input: sample size
            sliderInput(
                "n", "Sample size:",
                min = 10, max = 10000,
                value = 10, step = 5,
                animate = animationOptions(interval = 200, loop = FALSE)
            ),
            
            # Input: give correlation coefficient
            sliderInput(
                "r", "Pearson's correlation coefficient (r):",
                min = -1,
                max = 1,
                value = 0.1,
                step = .01,
                animate = animationOptions(interval = 200, loop = FALSE)
            ),
            # Input x, y and main title
            textInput("x", "Name of the x-axis:"),
            textInput("y", "Name of the y-axis:"),
            textInput("main", "Name of the main title:"),
            
            checkboxGroupInput(
                "margins", "Margins of Error:", 
                c("95% Confidence Intervals", "95% Prediction Intervals")
            )
        
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("plot", width = "600px", height = "600px"),
            verbatimTextOutput("summary_data")
        )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    get_data <- reactive({
        # create correlated data
        data <-  mvrnorm(n=input$n, mu=c(0, 0), Sigma=matrix(c(1, input$r, input$r, 1), nrow=2), empirical=TRUE)
        colnames(data) <- c("X", "Y")
        data <- as.data.frame(data)
    })
    model_data <- reactive({
        data <- get_data()
        model <- lm(Y~X, data=data)
        summary(model)
    })
        
    # create plot
    plot_data <- reactive({
        
        data <- get_data()
        plot(data, 
             col = "#75AADB", 
             pch = 16, 
             xlab = input$x, ylab = input$y, main = input$main,
             xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5))
        
        model <- lm(Y~X, data=data)
        abline(model, col = "black", lwd = 2)
        
        # add text
        text(-2.25, 2.4, 
             paste(
                 "r = ", as.character(input$r), 
                 "\nn = ", as.character(input$n),
                 "\n p = ", as.character(round(summary(model)$coefficients[2,4], 4))
             )
        )
        #
        xseq <- data.frame(X=seq(-3,3,length=100))
        
        # plot confidence intervals
        if("95% Confidence Intervals" %in% input$margins){
            ci.band <- predict(model,newdata=xseq,interval="confidence",level=0.95)
            lines(xseq[,1],ci.band[,2],lty=2)
            lines(xseq[,1],ci.band[,3],lty=2)
        }
        
        # plot prediction intervals
        if("95% Prediction Intervals" %in% input$margins){
            pi.band <- predict(model,newdata=xseq,interval="prediction",level=0.95)
            lines(xseq[,1],pi.band[,2],lty=2,col="darkgray")
            lines(xseq[,1],pi.band[,3],lty=2,col="darkgray")
        }
        
        # create legend
        legend("topright",
               legend=c("Fit","95% CI","95% PI"),
               lty=c(1,2,2),
               col=c("black","black","darkgray"),
               lwd=c(2,1,1)
               )
    })
    
    output$plot <- renderPlot({
        plot_data()
    })
    
    output$summary_data <- renderPrint({
        model_data()
    }) 
    
}

# Run the application 
shinyApp(ui = ui, server = server)
