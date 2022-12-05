## Factors Contributing to Crime in 1960
#' This Shiny application is my project for DATA 824
#' The data used for this project can be found on my GitHub account

Crime <- read.delim("C:/Users/broph/Dropbox/School/2022/Fall 2022/STAT 824 Visualization/Final Project/CrimeUSA1960.txt")

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Simple Regression Application (Dataset: Crime in United States in 1960)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("outcome", label = h3("Select response variable:"),
                  choices = list("Percentage of Males" = "M",
                                 "Southern State" = "So",
                                 "Mean Education" = "Ed",
                                 "Expenditure on Police Protection (1960)" = "Po1",
                                 "Expenditure on Police Protection (1959)" = "Po2",
                                 "Labor Force Participation" = "LF",
                                 "Number of Males per Females" = "M.F",
                                 "State Population (100,000)" = "Pop",
                                 "Percentage Nonwhite in Population" = "NW",
                                 "Unemployment Rate of Urban Males (14-24)" = "U1",
                                 "Unemployment Rate of Urban Males (35-39)" = "U2",
                                 "Family Income" = "Wealth",
                                 "Inequality" = "Ineq",
                                 "Probability of Imprisonment" = "Prob",
                                 "Average Time in State Prison" = "Time",
                                 "Crime Rate" = "Crime"), selected = 1),
      
      selectInput(
        "IndVar",
        label = h3("Select independent variable:"),
        choices = list("Percentage of Males" = "M",
                       "Southern State" = "So",
                       "Mean Education" = "Ed",
                       "Expenditure on Police Protection (1960)" = "Po1",
                       "Expenditure on Police Protection (1959)" = "Po2",
                       "Labor Force Participation" = "LF",
                       "Number of Males per Females" = "M.F",
                       "State Population (100,000)" = "Pop",
                       "Percentage Nonwhite in Population" = "NW",
                       "Unemployment Rate of Urban Males (14-24)" = "U1",
                       "Unemployment Rate of Urban Males (35-39)" = "U2",
                       "Family Income" = "Wealth",
                       "Inequality" = "Ineq",
                       "Probability of Imprisonment" = "Prob",
                       "Average Time in State Prison" = "Time",
                       "Crime Rate" = "Crime"), selected = 1)
      ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Scatterplot", plotOutput("scatterplot")), # Plot
                  tabPanel("Histogram/Density Plots", # Plots of distributions
                           fluidRow(
                             column(6, plotOutput("distribution1")),
                             column(6, plotOutput("distribution2")))
                  ),
                  tabPanel("Model Information", verbatimTextOutput("summary")), # Regression output
                  tabPanel("Data", DT::dataTableOutput('tbl')) # Data as datatable
                  
      )
    )
  ))


# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  # Linear Regression Output
  output$summary <- renderPrint({
    fit <- lm(Crime[,input$outcome] ~ Crime[,input$IndVar])
    names(fit$coefficients) <- c("Intercept", input$IndVar)
    summary(fit)
  })
  
  # Data output
  output$tbl = DT::renderDataTable({
    DT::datatable(Crime, options = list(lengthChange = FALSE))
  })
  
  
  # Scatterplot output
  output$scatterplot <- renderPlot({
    plot(Crime[,input$IndVar], Crime[,input$outcome], main="Scatterplot",
         xlab=input$IndVar, ylab=input$outcome, pch=16)
    abline(lm(Crime[,input$outcome] ~ Crime[,input$IndVar]), col="dark green")
    lines(lowess(Crime[,input$outcome] ~ Crime[,input$IndVar]), col="purple")
    legend(x = "topleft", box.col = "brown",
           bg ="light blue", box.lwd = 1, title="Legend", 
           legend=c("Linear Regression", "LOESS Regression"), 
           fill = c("dark green","purple"),
           adj = c(0, 0.3),cex=1)
    
  }, height=700)

  # Histogram output var 1
  output$distribution1 <- renderPlot({
    hist(Crime[,input$outcome], main="", xlab=input$outcome, prob = TRUE)
    lines(density(Crime[,input$outcome]),
          lwd = 2,
          col = "chocolate3")
    abline(v=mean(Crime[,input$outcome]),col="blue",lwd=4, lty = "dashed")
  }, height=600, width=600)
  
  # Histogram output var 2
  output$distribution2 <- renderPlot({
    hist(Crime[,input$IndVar], main="", xlab=input$IndVar, prob = TRUE)
    lines(density(Crime[,input$IndVar]),
          lwd = 2,
          col = "chocolate3")
    abline(v=mean(Crime[,input$IndVar]),col="blue",lwd=4, lty = "dashed")
  }, height=600, width=600)
  
}

shinyApp(ui = ui, server = server)