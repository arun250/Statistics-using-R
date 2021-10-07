library(shiny)
library(shinythemes)
ui <- fluidPage(
  theme = shinytheme("flatly"),
  h2("HSE Statistics"),
  navbarPage(
    "Statistical Concepts:",
    tabPanel(
      "Distribution of Random Variables",
      
      tabsetPanel(
        tabPanel(
          "Explanation",
          icon = icon(" fa-align-center"),
          br(),
          wellPanel(
            tags$p(
              strong("Normal Distribution:"),
              br(),
              "The Gaussian or normal probability was first described by Abraham de Moivre in 1733
              but bears the name of Karl Friedrich Gauss. It is the most important and widely used
              continuous probability distribution, especially in the analysis of data, because the distributions
              of several sample statistics tend towards a normal distribution as the sample size increases.",
              br(),
              strong("Exponential Distribution:"),
              br()
              ,
              "If a random variable x is distributed exponentially, then the reciprocal of x, y = 1 / x
              follows a Poisson distribution and vice versa.If the number of occurrences has Poisson distribution,
              the span between occurrences has exponential distribution.
              Its cumulative distribution function is as follows:
              
              ",
              br(),
              "P(X<=x)= 1-e^(-",
              HTML("&lambda;"),
              "t)",
              br(),
              strong("Uniform Distribution:"),
              br(),
              "The discrete uniform distribution, where all elements of a finite set are equally likely. This is
              supposed to be the distribution of a balanced coin,
              an unbiased die, a casino roulette or a well-shuffled deck.
              Also, one can use measurements of quantum states to generate uniform random variables.",
              br(),
              strong("Binomial Distribution:"),
              br(),
              "The Binomial Distribution is a very simple discrete probability distribution because it models a
              situation in which a single trial of some process or experiment can
              result in only one of two mutually exclusive outcomes.",
              br(),
              strong("Log Normal Distribution:"),
              br(),
              "The log-normal distribution is the probability distribution of a random variable whose logarithm
              is normally distributed. Let X be a random variable with
              a normal distribution, then Y=exp(X) has a log-normal distribution. In other words, if Y is log-normally distributed,
              then X=log(Y) is normally distributed. When a random variable
              represents a process that is the resultant of multiplicative product of many small effects. The logarithm of such a
              random variable can be expected to follow a normal distribution.
              Hence, if the variable is transformed to the log domain.",
              br(),
              strong("Recommended"),
              tags$ul(
                tags$li("Dalgaard, P. (2008): Introductory Statistics with R. Springer"),
                tags$li(
                  "Kleiber, C. and Zeileis, A. (2008): Applied Econometrics with R,
                  Springer"
                ),
                tags$li("Adler, J. (2010): R in a Nutshell. O'Reiley."),
                tags$li(
                  "Petzoldt, T. (2017) Data Analysis with R { Selected Topics and
                  Examples.}"
                )
                ),
              strong("Note:"),
              br(),
              "This web application is developed with",
              a("Shiny.", href = "http://www.rstudio.com/shiny/", target = "_blank"),
              br(),
              "This application is created as part of study project called",
              strong("Web based Teaching Application for HSE Statistics"),
              br(),
              strong("Authors:"),
              "Arun, Amjed, Alfonso",
              br(),
              strong("Supervisor:"),
              "Dr.Thomas Petzoldt"
                )
          )
      ),
      tabPanel(
        "Main",
        icon = icon("fas fa-chart-bar"),
        br(),
        sidebarPanel(
          id = "sidepanel",
          shinyjs::useShinyjs(),
          selectInput(
            "Distribution",
            "Select Distribution Type",
            choices = c(
              "Normal Distribution",
              "Exponential Distribution",
              "Uniform Distribution",
              "Binomial Distribution",
              "Lognormal Distribution"
            )
          ),
          sliderInput (
            "sampleSize",
            "Select Sample Size :",
            min = 0,
            max = 5000 ,
            value = 2500,
            step = 100
          ),
          conditionalPanel(
            condition = "input.Distribution == 'Normal Distribution'",
            textInput("Mean", "Enter the Mean", 180),
            textInput("sd", "Enter Standard Deviation", 1),
            textInput("Breaks", "Enter the number of Histogram columns", 20)
          ),
          conditionalPanel(
            condition = "input.Distribution == 'Exponential Distribution'",
            textInput("Lamda", "Enter Exponential Lamda", 1),
            textInput("Breaks", "Enter the number of Histogram columns", 20)
          ),
          conditionalPanel(
            condition = "input.Distribution == 'Binomial Distribution'",
            textInput("Trials", "Enter Number of Trials", 100),
            textInput("Prob", "Enter Probability", 0.5),
            textInput("Breaks", "Enter the number of Histogram columns", 20)
          ),
          conditionalPanel(
            condition = "input.Distribution == 'Lognormal Distribution'",
            textInput("Mean1", "Enter the mean", 10),
            textInput("sd1", "Enter Standard Deviation", 1),
            textInput("Breaks", "Enter the number of Histogram columns", 20)
          ),
          conditionalPanel(
            condition = "input.Distribution == 'Uniform Distribution'",
            textInput("min", "Enter the minimum value", 1),
            textInput("max", "Enter the maximum value", 50),
            textInput("Breaks", "Enter the number of Histogram Columns", 20)
          )
          ,
          actionButton("reset", "Reset", icon("fas fa-undo"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
        ),
        mainPanel(
          splitLayout(
            style = "border: 1px solid silver:",
            cellWidths = 400,
            cellHeights = 400,
            cellArgs = list(style = "padding: 6px")
            ,
            plotOutput("hist"),
            plotOutput("box")
          )
        )
        
      )
        )
    )
  )
  )
server <- function(input, output) {
  observeEvent(input$reset, {
    shinyjs::reset("sidepanel")
  })
  output$hist <- renderPlot({
    distype <- input$Distribution
    size <- input$sampleSize
    if (distype == "Normal Distribution") {
      rendervec <-
        rnorm(size,
              mean = as.numeric(input$Mean),
              sd = as.numeric(input$sd))
    }
    else if (distype == "Exponential Distribution") {
      rendervec <- rexp(size, rate = 1 / as.numeric(input$Lamda))
    }
    else if (distype == "Binomial Distribution") {
      rendervec <-
        rbinom (n = as.numeric(input$Trials),
                size,
                prob = as.numeric(input$Prob))
    }
    else if (distype == "Lognormal Distribution") {
      rendervec <-
        exp((rnorm(
          size,
          mean = as.numeric(input$Mean1),
          sd = as.numeric(input$sd1)
        )))
    }
    else{
      rendervec <-
        runif(size,
              max = as.numeric(input$max),
              min = as.numeric(input$min))
    }
    hist(
      rendervec,
      col = "blue",
      main = input$Distribution,
      xlab = "Random Variable",
      xlim = c(min(rendervec), max(rendervec)),
      labels = TRUE ,
      breaks = as.numeric(input$Breaks)
    )
  })
  output$box <- renderPlot({
    distype <- input$Distribution
    size <- input$sampleSize
    if (distype == "Normal Distribution") {
      rendervec <-
        rnorm(size,
              mean = as.numeric(input$Mean),
              sd = as.numeric(input$sd))
    }
    else if (distype == "Exponential Distribution") {
      rendervec <- rexp(size, rate = 1 / as.numeric(input$Lamda))
    }
    else if (distype == "Binomial Distribution") {
      rendervec <-
        rbinom (n = as.numeric(input$Trials),
                size,
                prob = as.numeric(input$Prob))
    }
    else if (distype == "Lognormal Distribution") {
      rendervec <-
        exp((rnorm(
          size,
          mean = as.numeric(input$Mean1),
          sd = as.numeric(input$sd1)
        )))
    }
    else{
      rendervec <-
        runif(size,
              max = as.numeric(input$max),
              min = as.numeric(input$min))
    }
    boxplot(rendervec, main = input$Distribution)
  })
  output$plot <- renderPlot({
    distype <- input$Distribution
    size <- input$sampleSize
    if (distype == "Normal Distribution") {
      rendervec <-
        rnorm(size,
              mean = as.numeric(input$Mean),
              sd = as.numeric(input$sd))
      p <- c(seq(min(rendervec), max(rendervec), length = 1000))
      plot(p,
           dnorm(p, mean = mean(p), sd = sd(p)),
           type = "l",
           col = "blue")
    }
    else if (distype == "Exponential Distribution") {
      rendervec <- rexp(size, rate = 1 / as.numeric(input$Lamda))
      p <- c(seq(min(rendervec), max(rendervec), length = 1000))
      plot(p,
           dexp(p, rate = 1 / as.numeric(inpit$Lamda)),
           type = "l",
           col = "blue")
    }
    else if (distype == "Binomial Distribution") {
      rendervec <-
        rbinom (n = as.numeric(input$Trials),
                size,
                prob = as.numeric(input$Prob))
      p <- c(seq(min(rendervec), max(rendervec), length = 1000))
      plot(p,
           dbinom(p, size, prob = as.numeric(input$Prob)),
           type = "l",
           col = "blue")
    }
    else if (distype == "Lognormal Distribution") {
      rendervec <-
        exp((rnorm(
          size,
          mean = as.numeric(input$Mean1),
          sd = as.numeric(input$sd1)
        )))
      p <- c(seq(min(rendervec), max(rendervec), length = 1000))
      plot(p, exp(dnorm(
        p, mean = mean(p), sd = sd(p)
      )), type = "l", col = "blue")
    }
    else{
      rendervec <-
        runif(size,
              max = as.numeric(input$max),
              min = as.numeric(input$min))
      p <- c(seq(min(rendervec), max(rendervec), length = 1000))
      plot(p,
           dunif(p, max = max(p), min = min(p)),
           type = "l",
           col = "blue")
    }
  })
  output$text <- renderPrint({
    distype <- input$Distribution
    size <- input$sampleSize
    if (distype == "Normal Distribution") {
      rendervec <-
        rnorm(size,
              mean = as.numeric(input$Mean),
              sd = as.numeric(input$sd))
    }
    else if (distype == "ExponentialDistribution") {
      rendervec <- rexp(size, rate = 1 / as.numeric(input$Lamda))
    }
    else if (distype == "Binomial Distribution") {
      rendervec <-
        rbinom (n = as.numeric(input$Trials),
                size,
                prob = as.numeric(input$Prob))
    }
    else if (distype == "Lognormal Distribution") {
      rendervec <-
        exp((rnorm(
          size,
          mean = as.numeric(input$Mean1),
          sd = as.numeric(input$sd1)
        )))
    }
    else{
      rendervec <-
        runif(size,
              max = as.numeric(input$max),
              min = as.numeric(input$min))
    }
    summary(rendervec)
  })
}
shinyApp(ui = ui, server = server)
