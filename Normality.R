library(shiny)
library(psych)
library(beeswarm)
library(DescTools)
library(DT)
library(tidyverse)
if (!require("ggplot2"))
  install.packages("ggplot2")
if (!require("shinyBS"))
  install.packages("shinyBS")

ui<-fluidPage(
  titlePanel("Normality Test"),
  
    tabsetPanel(
      tabPanel("Information of t-test",icon = icon("fas fa-info-circle"),
               column(1),
               column(5,br(),br(),br(),
                      withMathJax(p("This application allows users to perform the ", code("Normality Test",style="color:navy"), " by means of a visual part and a numeric part."),
                                  p("The", code("Graphical Methods",style="color:navy")," in this application includes Q-Q Plot and Hist.",
                                    code("QQ Plot:",style="color:navy"),
                                    "If the data is normally distributed, the points in the Q-Q normal plot lie on a straight diagonal line. You can add this line to your Q-Q plot with the comand Q-Q line(x), where x is the vector of values. When the deviations from the straight line are minimal, this indicates a normal distribution.",
                                    code("Histogram:",style="color:navy"),"Plotting a histogram of the variable of interest will give an indicator of the shape of the distribution. A density curve smoothes out the histogram and can be added to the graph"),
                                  p("On the other hand the are also specific methods for testing normality but these should be used in conjunction with either a histogram or a Q-Q plot.", "The", code("Kolmogorov-Smirnov Test",style="color:navy"), " and the", code("Shapiro-Wilk Test",style="color:navy"),
                                    " should be preferred as it is generally more sensitive."),
                                  p("For smaller samples, non-normality is less likely to be detected but the Shapiro-Wilk test should be preferred as it is generally more sensitive"),
                                  p("For larger samples (i.e. more than one hundred), the normality tests are overly conservative and the assumption of normality might be rejected too easily"),
                                  p(code("Kolmogorov-Smirnov Test",style="color:navy"), "The One-Sample Kolmogorov-Smirnov Test procedure compares the observed cumulative distribution
                                    45
                                    function for a variable with a specified theoretical distribution, which may be normal, uniform, Poisson, or exponential. The Kolmogorov-Smirnov Z is computed from the largest difference (in absolute value) between the observed and theoretical cumulative distribution functions. This goodness-of-fit test tests whether the observations could reasonably have come from the specified distribution."),
                                  p("The Kolmogorov-Smirnov test assumes that the parameters of the test distribution are specified in advance. This procedure estimates the parameters from the sample. The sample mean and sample standard deviation are the parameters for a normal distribution, the sample minimum and maximum values define the range of the uniform distribution, the sample mean is the parameter for the Poisson distribution, and the sample mean is the parameter for the exponential distribution. The power of the test to detect departures from the hypothesized distribution may be seriously diminished. For testing against a normal distribution with estimated parameters, consider the adjusted K-S Lilliefors test (available in the Explore procedure."),
                                  p(code("Shapiro-Wilk Test",style="color:navy"), "Here the Null hypothesis states that the data is normally distributed. If p> 0.05, normality can be assumed. The p value tells you what the chances are that the sample comes from a normal distribution. The lower this value, the smaller the chance. Statisticians tipically use a value of 0.05 as a cutoff, so when the p-value is lower than 0.05, you can conclude that the sample deviates from nomrality"))),
               column(5,br(),br(),br(),
                      strong('List of Packages Used'), br(),
                      code('library(shiny)'),br(),
                      code('library(psych)'),br(),
                      code('library(beeswarm)'),br(),
                      code('library(DescTools)'),br(),
                      br(),
                      strong('Note'),
                      br(),
                      "This web application is developed with",
                      a("Shiny.", href = "http://www.rstudio.com/shiny/", target = "_blank"),
                      br(),
                      "This application is created as part of study project called",
                      strong("Web based Teaching Application for HSE Statistics"),
                      br(),
                      strong("Authors:"),
                      "Arun,Amjed,Alfonso",
                      br(),
                      strong("Supervisor:"),"Dr.Thomas Petzoldt",br()
                      
                      ,
                      wellPanel(
                        code("Graphical Methods:",style="color:navy"),
                        p(HTML("Q-Q Plot"),p(),
                          p("histogram"), HTML("</ul>")),
                        code("Numerical Methods:",style="color:navy"),
                        p(HTML("kolmogorov-Smirnov.test"),p(),
                          p("Shapiro-Wilk.test"), HTML("</ul>"))))),
      tabPanel("Normality Test - Given Data",icon = icon("fas fa-database"),br(),sidebarPanel(
        strong(p("Data input:")),width =4 ,
        tags$textarea(id="textarea.in", rows=30, cols=15,
                      "44\n32\n37\n44\n40\n44\n45\n36\n37\n42\n38\n47\n26\n41\n27\n39\n47\n41\n36\n44\n39\n49\n40\n25\n33\n39\n36\n45\n32\n51\n47\n39\n43\n38\n42\n35\n29\n33\n40\n37\n38\n34\n36\n43\n38\n39\n26\n30\n41\n35\n30\n39\n46\n39\n43\n46\n33\n48\n44\n45\n42\n45\n46\n53\n47\n47\n51\n33\n40\n43\n48\n37\n31\n40\n40\n49\n43\n49\n46\n38\n40\n51\n40\n44\n45\n53\n46\n42\n44\n46\n29\n38")
      ),mainPanel(
               h3("Visual Inspection Normality Test"),
               
               br(),
               h3("Q-Q plot"),
               plotOutput("qqPlot", width="100%"),
               br(),
               h3("Histogram"),
               plotOutput("distPlot",width = "100%"),
               h3("Numeric Inspection
                  Test of normality"),
               numericInput("numdigits", "Number of decimal points:", 1),
               br(),
               verbatimTextOutput("textarea.out"),
               br(),
               verbatimTextOutput("testnorm.out"),
               br(),
               p(strong('"Within this analysis we can determine if the sample is normal distrubuted or not, to review the analysis of each result, the information is provided in the third panel"')),
               verbatimTextOutput("code") )),
      tabPanel("Normality Test - Uploaded Data",icon = icon("fas fa-upload"),br(),sidebarPanel(
               fileInput("uploaded_file", "Choose CSV File",
                         multiple = TRUE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               # Horizontal line ----
               tags$hr(),
               # Input: Checkbox if file has header ----
               checkboxInput("header", "Header", TRUE),
               # Input: Select separator ----
               radioButtons("sep", "Separator",
                            choices = c(Semicolon = ";",
                                        Comma = ",",
                                        Tab = "\t"),
                            
                            selected = ","),
               # Horizontal line ----
               tags$hr(),
               # Select variables to display ----
               uiOutput("checkbox")),mainPanel(
               id = "dataset",
               tabPanel("FILE", DT::dataTableOutput("rendered_file")),plotOutput("makeboxPlot"))),
      tabPanel("Results Uploaded Data",icon = icon("fas fa-chart-bar"),mainPanel(
               h3("Visual Inspection Normality Test"),
               br(),
               h3("Q-Q plot"),
               plotOutput("qqPlot1", width="100%"),
               br(),
               h3("Histogram"),
               plotOutput("distPlot1"),
               h3("Numeric Inspection
                  Test of normality"),
               numericInput("numdigits1", "Number of decimal points:", 1),
               br(),
               verbatimTextOutput("a.out"),
               br(),
               verbatimTextOutput("testnorm1.out")), column(1))))


server<-shinyServer(function(input, output) {
  options(warn=-1)
  bs <- reactive({
    x <- input$textarea.in
    #x <- df()[,1]
    x <- as.numeric(unlist(strsplit(x, "[\n, \t]")))
    result <- describe(x)[2:13]
    row.names(result) <- ""
    ndigits <- input$numdigits # decimal points
    ci <- round(MeanCI(x, conf.level=0.95), ndigits)
    print(result, digits=ndigits) # rounding to the specified number of digits
    cat("\n", "[95% confidence intervals of the mean]", "\n")
    return(ci)})
  makedistPlot <- function(){
    x <- input$textarea.in
    #x <- df()[,1]
    print(str(x))
    x <- as.numeric(unlist(strsplit(x, "[\n, \t]")))
    x <- x[!is.na(x)]
    simple.bincount <- function(x, breaks) {
      nx <- length(x)
      nbreaks <- length(breaks)
      counts <- integer(nbreaks - 1)
      for (i in 1:nx) {
        lo <- 1
        37
        hi <- nbreaks
        if (breaks[lo] <= x[i] && x[i] <= breaks[hi]) {
          while (hi - lo >= 2) {
            new <- (hi + lo) %/% 2
            if(x[i] > breaks[new])
              lo <- new
            else
              hi <- new}
          counts[lo] <- counts[lo] + 1}}
      return(counts)}
    nclass <- nclass.FD(x)
    breaks <- pretty(x, nclass)
    counts <- simple.bincount(x, breaks)
    counts.max <- max(counts)
    h <- hist(x, las=1, breaks="FD", xlab= "Red vertical line shows the mean.",
              ylim=c(0, counts.max*1.2), main="", col = "#FF9900")
    rug(x)
    abline(v = mean(x, na.rm=T), col = "#c00000", lwd = 2)
    xfit <- seq(min(x, na.rm=T), max(x, na.rm=T))
    yfit <- dnorm(xfit, mean = mean(x, na.rm=T), sd = sd(x, na.rm=T))
    yfit <- yfit * diff(h$mids[1:2]) * length(x)
    lines(xfit, yfit, col = "yellowgreen", lwd = 2)}
  output$distPlot <- renderPlot({
    print(makedistPlot())})
  makeboxPlot <- function(){
    x <- input$textarea.in
    #x <- df()[,1]
    x <- as.numeric(unlist(strsplit(x, "[\n, \t]")))
    boxplot(x, horizontal=TRUE, xlab= "Mean and +/-1 SD are displayed in yellow.")
    points(mean(x, na.rm=T), 0.9, pch = 18, col = "#FF9900", cex = 2)
    arrows(mean(x, na.rm=T), 0.9, mean(x, na.rm=T) + sd(x, na.rm=T), length = 0.1, angle = 45, col = "#FF9900")
    
    arrows(mean(x, na.rm=T), 0.9, mean(x, na.rm=T) - sd(x, na.rm=T), length = 0.1, angle = 45, col = "#FF9900")
    if (input$beeswarm == F) {
      NULL
    } else {
      beeswarm(x, horizontal=TRUE, col = 4, pch = 16, add = TRUE)
    } }
  output$boxPlot <- renderPlot({
    print(makeboxPlot())})
  testnorm <- reactive({
    x <- input$textarea.in
    #x <- df()[,1]
    x <- as.numeric(unlist(strsplit(x, "[\n, \t]")))
    list(ks.test(scale(x), "pnorm"), shapiro.test(x)) })
  makeqqPlot <- function(){
    x <- input$textarea.in
    #x <- df()[,1]
    x <- as.numeric(unlist(strsplit(x, "[\n, \t]")))
    qqnorm(x, las=1)
    qqline(x, col=2) }
  output$qqPlot <- renderPlot({
    print(makeqqPlot())})
  info <- reactive({
    info1 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")
    info2 <- paste("It was executed on ", date(), ".", sep = "")
    cat(sprintf(info1), "\n")
    cat(sprintf(info2), "\n") })
  output$info.out <- renderPrint({
    info() })
  output$textarea.out <- renderPrint({
    bs()})
  output$testnorm.out <- renderPrint({
    
    testnorm() })
  output$qqplot = renderPlot({
    if((input$datformat==1 & !input$usedata) | (input$sampdat==1 & input$usedata))
    {
      dat=unlist(data())
      dat1=data.frame(x=as.numeric(as.character(dat)))
      ggplot(data=dat1, aes(sample=x)) + stat_qq(geom="point",color="navy",shape=1) +
        theme_bw() + theme(text=element_text(size=15)) + ggtitle("Q-Q Plot")
    } else if((input$datformat==2 & !input$usedata) | (input$sampdat!=1 & input$usedata))
    {
      dat=data()
      dat1=data.frame(x=dat[[1]],y=dat[[2]])
      if(length(unique(dat[[1]])) > length(unique(dat[[2]])))
      {
        dat1$x=as.numeric(as.character(dat1$x))
        ggplot(data=dat1, aes(sample=x)) + stat_qq(aes(color=factor(y)),geom="point",shape=1) + theme_bw() +
          theme(text=element_text(size=15)) + ggtitle("Q-Q Plot") + facet_wrap(~y) +
          scale_color_manual(name="", values=c("navy","gold2")) + guides(color=FALSE)
      } else
      {
        dat1$y=as.numeric(as.character(dat1$y))
        ggplot(data=dat1, aes(sample=y)) + stat_qq(aes(color=factor(x)),geom="point",shape=1) + theme_bw() +
          theme(text=element_text(size=15)) + ggtitle("Q-Q Plot") + facet_wrap(~x) +
          scale_color_manual(name="", values=c("navy","gold2")) + guides(color=FALSE)
      }
    } else if((input$datformat==3 & !input$usedata) | (input$sampdat!=1 & input$usedata))
    {
      dat=data()
      dat1=data.frame(x=c(as.numeric(as.character(dat[[1]])),as.numeric(as.character(dat[[2]]))),
                      y=c(rep(names(dat)[1],length(dat[[1]])),rep(names(dat)[2],length(dat[[2]]))))
      
      ggplot(data=dat1, aes(sample=x)) + stat_qq(aes(color=factor(y)),geom="point",shape=1) + theme_bw() +
        theme(text=element_text(size=15)) + ggtitle("Q-Q plot") + facet_wrap(~y) +
        scale_color_manual(name="", values=c("navy","gold2")) + guides(color=FALSE) } })
  # Read file ----
  df <- reactive({
    req(input$uploaded_file)
    read.csv(input$uploaded_file$datapath,
             header = input$header,
             sep = input$sep) })
  # Dynamically generate UI input when data is uploaded ----
  output$checkbox <- renderUI({
    checkboxGroupInput(inputId = "select_var",
                       label = "Select variables",
                       choices = names(df()))})
  # Select columns to print ----
  df_sel <- reactive({
    req(input$select_var)
    df_sel <- df() %>% select(input$select_var)})
  # Print data table ----
  output$rendered_file <- DT::renderDataTable({
    df_sel()})
  options(warn=-1)
  bss <- reactive({
    #x <- input$textarea.in
    a <- df()[,1]
    #x <- as.numeric(unlist(strsplit(x, "[\n, \t]")))
    result1 <- describe(a)[2:13]
    row.names(result1) <- ""
    ndigits1 <- input$numdigits1 # decimal points
    ci1 <- round(MeanCI(a, conf.level=0.95), ndigits1)
    print(result1, digits=ndigits1) # rounding to the specified number of digits
    
    cat("\n", "[95% confidence intervals of the mean]", "\n")
    return(ci1)})
  makedistPlot1 <- function(){
    #x <- input$textarea.in
    a <- df()[,1]
    print(str(a))
    #x <- as.numeric(unlist(strsplit(x, "[\n, \t]")))
    a <- a[!is.na(a)]
    simple.bincount1 <- function(a, breaks1) {
      nx1 <- length(a)
      nbreaks1 <- length(breaks1)
      counts1 <- integer(nbreaks1 - 1)
      for (i in 1:nx1) {
        lo1 <- 1
        hi1 <- nbreaks1
        if (breaks1[lo1] <= a[i] && a[i] <= breaks1[hi1]) {
          while (hi1 - lo1 >= 2) {
            new1 <- (hi1 + lo1) %/% 2
            if(a[i] > breaks1[new1])
              lo1 <- new1
            else
              hi1 <- new1}
          counts1[lo1] <- counts1[lo1] + 1}}
      return(counts1)}
    nclass1 <- nclass.FD(a)
    breaks1 <- pretty(a, nclass1)
    counts1 <- simple.bincount1(a, breaks1)
    counts.max1 <- max(counts1)
    h1 <- hist(a, las=1, breaks1="FD", xlab= "Red vertical line shows the mean.",
               ylim=c(0, counts.max1*1.2), main="", col = "#FF9900")
    rug(a)
    abline(v = mean(a, na.rm=T), col = "#c00000", lwd = 2)
    
    xfit1 <- seq(min(a, na.rm=T), max(a, na.rm=T))
    yfit1 <- dnorm(xfit1, mean = mean(a, na.rm=T), sd = sd(a, na.rm=T))
    yfit1 <- yfit1 * diff(h1$mids[1:2]) * length(a)
    lines(xfit1, yfit1, col = "yellowgreen", lwd = 2)}
  output$distPlot1 <- renderPlot({
    print(makedistPlot1())})
  makeboxPlot1 <- function(){
    #x <- input$textarea.in
    a <- df()[,1]
    #x <- as.numeric(unlist(strsplit(x, "[\n, \t]")))
    boxplot1(a, horizontal=TRUE, xlab= "Mean and +/-1 SD are displayed in yellow.")
    points(mean(a, na.rm=T), 0.9, pch = 18, col = "#FF9900", cea = 2)
    arrows(mean(a, na.rm=T), 0.9, mean(a, na.rm=T) + sd(a, na.rm=T), length = 0.1, angle = 45, col = "#FF9900")
    arrows(mean(a, na.rm=T), 0.9, mean(a, na.rm=T) - sd(a, na.rm=T), length = 0.1, angle = 45, col = "#FF9900")
    if (input$beeswarm == F) {
      NULL
    } else {
      beeswarm(a, horizontal=TRUE, col = 4, pch = 16, add = TRUE)}}
  output$boxPlot1 <- renderPlot({
    print(makeboxPlot1()) })
  testnorm1 <- reactive({
    #x <- input$textarea.in
    a <- df()[,1]
    #a <- as.numeric(unlist(strsplit(a, "[\n, \t]")))
    list(ks.test(scale(a), "pnorm"), shapiro.test(a))})
  makeqqPlot1 <- function(){
    #x <- input$textarea.in
    a <- df()[,1]
    #a <- as.numeric(unlist(strsplit(a, "[\n, \t]")))
    qqnorm(a, las=1)
    qqline(a, col=2)}
  
  output$qqPlot1 <- renderPlot({
    print(makeqqPlot1())})
  infoo <- reactive({
    info11 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")
    info12 <- paste("It was executed on ", date(), ".", sep = "")
    cat(sprintf(info11), "\n")
    cat(sprintf(info12), "\n")})
  output$infoo.out <- renderPrint({
    infoo()})
  output$a.out <- renderPrint({
    bss()})
  output$testnorm1.out <- renderPrint({
    testnorm1()})})

shinyApp(ui, server)

