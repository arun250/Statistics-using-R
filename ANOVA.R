#libraries used in this application
library(shiny)
library(beeswarm)
library(ggplot2)
library(plotly)
library(dplyr)
library(datasets)
library(purrr)
#example data
data(iris)
data(mtcars) 
uvals <- sapply(mtcars, function(x) {
  length(unique(x))
})
mtcars <- map_if(mtcars, uvals < 4, as.factor) %>% as.data.frame()
#plotting theme for ggplot2
.theme <- theme(
  axis.line = element_line(colour = 'gray', size = .75),
  panel.background = element_blank(),
  plot.background = element_blank()
)
# Define UI for application
ui <- fluidPage(# Application title
  titlePanel(h2(
    strong("Web based Teaching Application for HSE Statistics")
  )),
  br(),
  tabsetPanel(
    tabPanel((strong("Information about ANOVA")),
             icon = icon("fas fa-info-circle"),
             br(),
             
             column(1),
             column(12, br(), wellPanel(h2((
               strong("ANOVA")
             )),
             withMathJax(
               p(
                 "This application allows users to perform an",
                 strong("ANOVA"),
                 "test. An Analysis of Variance",
                 strong("ANOVA"),
                 " test is a way to find out if survey or experiment results are significant.
                 In other words, they help you to figure out if you need to reject the null
                 hypothesis or accept the alternate hypothesis. Basically, you're testing
                 groups to see if there's difference between them. ANOVA methods are mostly
                 based on linear models."
               ),
               p(
                 "Types of Tests:",
                 br(),
                 tags$ul(tags$li("One-way ANOVA"), tags$li("Two-way ANOVA")),
                 
                 strong("One-way ANOVA:"),
                 br(),
                 "
                 A one-way ANOVA is used to compare two means from two independent groups using
                 the F-distribution. In this case, the null hypothesis for the test is that the
                 two means are equal. Therefore, a significant result means that accept the
                 alternate hypothesis."
               ),
               p(
                 "
                 Example 1: A group of psychiatric patients are trying three different therapies:
                 counselling, medication and biofeedback. You want to see if one therapy is bette
                 than the others.",
                 br(),
                 "
                 Example 2: You want to study the height of people according to their weight.
                 You could split contestants into categories (obese, overweight and normal)
                 and measure their height on a measuring scale.",
                 br(),
                 
                 strong("Limitations:"),
                 "   one-way ANOVA test will tell you difference between the two-groups. But it
                 won't tell you which groups are different.",
                 br(),
                                  strong("Post-Hoc test:"),
                 "  In order to know which groups a difference in means had, you need to approach a
                 post hoc test.Example: Fisher LSD-Test, Tukey HSD-Test, Duncan, Dunnett, Sheffe,
                 Newman-Keuls",
                 br(),
                 br(),
                 strong("Two-way ANOVA:"),
                 br(),
                 "
                 A Two-way ANOVA is an extension of the one-way ANOVA. It is used to find whether
                 there is an interaction between the two independent variables on the dependent variables.",
                 br(),
                 "
                 Example 1: You might be studying interaction between the physical activity level and gender
                 on heart beat level in children, where the independent variables are physical
                 activity(low/moderate/high), gender (male/female) and heart beat levels are your dependent variable.",
                 br(),
                 "
                 Example 2: You want to find out if there is an interaction between income and gender for
                 anxiety level at the job interviews, where the gender (male, female) and income (Low, middle, high)
                 are the independent variables and the anxiety levels are your dependent variables." ,
                 br(),
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
                 br(),
                 strong("Note:"),
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
                 strong("Supervisor:"),
                 "Dr.Thomas Petzoldt"
                 
                   )
               )))
             
             
    ),
    tabPanel(
      strong("Data Exploration"),
      icon = icon("fas fa-database"),
      br(),
      
      sidebarPanel (
        # Input: Select what to display
        selectInput(
          "dataset",
          "Data:",
          choices = list(iris = "iris", mtcars = "mtcars"),
          selected = NULL
        ),
        selectInput("variable", "Variable:", choices = NULL),
        selectInput("group", "Group:", choices = NULL)
      ),
      
      # output
      mainPanel(plotOutput("plot"),
                verbatimTextOutput("aov4"))
    )
    ,
    tabPanel(
      strong("User Data"),
      icon = icon("fas fa-upload"),
      br(),
      sidebarLayout(
        # Sidebar panel for inputs
        sidebarPanel(
          # Input: Select a file ----
          fileInput(
            "uploaded_file",
            "Choose CSV File",
            multiple = TRUE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")
          ),
          
          uiOutput("checkbox"),
          uiOutput("checkbox2")
          
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(DT::dataTableOutput("rendered_file"), br())
        
      )
      
    ),
    tabPanel(
      strong("Visualize Data"),
      icon = icon("fas fa-chart-bar"),
      br(),fluidRow(column(9,offset=2,
      plotlyOutput("boxplot1",width = "100%"),br(),
      verbatimTextOutput("aov"),
      br(),
      verbatimTextOutput("aov1")
    ))
    
  )))

# Define server logic required to define the output
server <- function(input, output, session) {
  observe({
    var.opts <- colnames(get(input$dataset))
    updateSelectInput(session, "variable", choices = var.opts)
    updateSelectInput(session, "group", choices = var.opts)
  })
  
  
  get_data <- reactive({
    get(input$dataset)
  })
  
  
  output$plot <- renderPlot({
    obj <- get_data()
    p <- ggplot(obj,
                aes_string(
                  x 		= input$variable,
                  y 		= input$group,
                  fill 	= input$variable # let type determine plotting
                )) + geom_boxplot()
    print(p)
  })
  
  
  output$aov4 <- renderPrint({
    obj <- get_data()
    
    ## double [[]] important to make x and v a vector, not a single column data frame
    x <- obj[[input$variable]]
    v <- obj[[input$group]]
    ret <- anova(lm(v ~ x))
    print(ret)
  })
  
  
  
  # Read file ----
  df <- reactive({
    req(input$uploaded_file, cancelOutput = FALSE)
    # get(input$uploaded_file,inherits = FALSE)
    # if (is.null(uploaded_file)){return(NULL)}
    read.csv(input$uploaded_file$datapath)
    
  })
  
  # Dynamically generate UI input when data is uploaded ----
  output$checkbox <- renderUI({
    selectInput(inputId = "select_var",
                label = "Select variables",
                choices = names(df()))
    
  })
  
  output$checkbox2 <- renderUI({
    selectInput(inputId = "select_group",
                label = "Select Group",
                choices = names(df()))
  })
  
  # Print data table ----
  output$rendered_file <- DT::renderDataTable({
    df()
    
  })
  output$boxplot1 <- renderPlotly({
    p <-
      ggplot(na.omit(df()),
             aes(
               x = na.omit(df())[, (input$select_var)],
               y = na.omit(df())[, (input$select_group)],
               fill = na.omit(df())[, input$select_var]
             )) +
      geom_boxplot(na.rm = TRUE) + ggtitle("Boxplot") +
      xlab(input$select_var) + ylab(input$select_group) + scale_fill_discrete(name =
                                                                                input$select_group)
    ggplotly(p) %>% layout(autosize = TRUE)# %>% style(hoverinfo = "none")
  })
  
  output$aov <- renderPrint({
    if (is.null(input$uploaded_file$datapath)) {
      return()
    }
    anova(lm((df()[, (input$select_group)]) ~ (df()[, (input$select_var)]), data = df()))
  })
  
  output$aov1 <- renderPrint({
    if (is.null(input$uploaded_file$datapath)) {
      return()
    }
    anova(lm(df()[, (input$select_group)] ~ df()[, (input$select_var)], data = df()))
    TukeyHSD(aov(lm(df()[, (input$select_group)] ~ df()[, (input$select_var)], data = df())))
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
