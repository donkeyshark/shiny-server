###########################################
## New tax plan calculator
## Author: Travis Trail
## Date: 12/15/2017
#
###########################################


library(shiny)
library(data.table)

# Define UI for application 
ui <- fluidPage(
  
  # Application title
  titlePanel("New Tax Plan Calculator"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      helpText("Filing status, income, & dependents"),
      selectInput("status", label = "Filing Status",
                  choices = c("Single", "Married - Filing Jointly"),
                  selected = "Single"),
      numericInput("children", "Number of Children", "0"),
      numericInput("exemptions", "Total Number of Personal Exemptions", "1"),
      numericInput("income", "Gross Income", "0"),
      helpText("Deductions"),
      numericInput("mtg", "Mortgage Interest Paid", "0"),
      numericInput("salt", "State & Local Taxes Paid", "0"),
      numericInput("property", "Property Taxes Paid", "0"),
      numericInput("charity", "Charitable Contributions", "0"),
      numericInput("medical", "Medical Expenses", "0"),
      numericInput("other", "Other Qualifying Deductions", "0")
      #verbatimTextOutput("value")
    ), position = "left",
    
    # Show a plot of the generated distribution
    mainPanel(
      
      #textOutput("text1"),
      htmlOutput("text3"),
      htmlOutput("text4"),
      div(tableOutput("table"), style = "font-size: 100%; width: 100%"),
      #div(htmlOutput("text3"), style="color:#0000FF")
      htmlOutput("text2"),
      tags$div(class="header", checked=NA,
               tags$p("Note: The tax tables and deduction info used in these calculations are taken directly from Congress's tax plan proposal document. 
                      They are not taken from some random partisan website. See the link below for the official document if you wish to read 
                      it (warning: it is 1097 pages long)."),
               tags$a(href="http://docs.house.gov/billsthisweek/20171218/CRPT-115HRPT-466.pdf", "New tax plan proposal")
               )
      )
    )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  options(scipen=999)
  
  #output$text1 <- renderText({paste("You have selected", input$status)})
  output$text2 <- renderUI({ HTML(paste(" ", " ", sep="<br/>")) })
  
  
  # New forumula for Single
  new.single <- function(ti) {
    if (ti <= 9525) { 
      return (ti*0.1)
    } else if (ti <= 38700) {
      return (952.5 + (ti - 9525) * 0.12)
    } else if (ti <= 82500) {
      return (4453.5 + (ti - 38700) * 0.22)
    } else if (ti <= 157500) {
      return (14089.5 + (ti - 82500) * 0.24)
    } else if (ti <= 200000) {
      return (32089.5 + (ti - 157500) * 0.32)
    } else if (ti <= 500000) {
      return (45689.5 + (ti - 200000) * 0.35)
    } else 
      return (150689.5 + (ti - 500000) * 0.37)
  }
  
  # New forumula for Married Filing Jointly
  new.mfj <- function(ti) {
    if (ti <= 19050) { 
      return (ti*0.1)
    } else if (ti <= 77400) {
      return (1905 + (ti - 19050) * 0.12)
    } else if (ti <= 165000) {
      return (8907 + (ti - 77400) * 0.22)
    } else if (ti <= 315000) {
      return (28179 + (ti - 165000) * 0.24)
    } else if (ti <= 400000) {
      return (64179 + (ti - 315000) * 0.32)
    } else if (ti <= 600000) {
      return (91379 + (ti - 400000) * 0.35)
    } else 
      return (161379 + (ti - 600000) * 0.37)
  }
  
  # Old forumula for Single
  old.single <- function(ti) {
    if (ti <= 9525) { 
      return (ti*0.1)
    } else if (ti <= 38700) {
      return (952.5 + (ti - 9525) * 0.15)
    } else if (ti <= 93700) {
      return (5328.75 + (ti - 38700) * 0.25)
    } else if (ti <= 195450) {
      return (19078.75 + (ti - 93700) * 0.28)
    } else if (ti <= 424950) {
      return (47568.75 + (ti - 195450) * 0.33)
    } else if (ti <= 426700) {
      return (123303.75 + (ti - 424950) * 0.35)
    } else 
      return (123916.25 + (ti - 426700) * 0.396)
  }
  
  # Old forumula for Married Filing Jointly
  old.mfj <- function(ti) {
    if (ti <= 19050) { 
      return (ti*0.1)
    } else if (ti <= 77400) {
      return (1905 + (ti - 19050) * 0.15)
    } else if (ti <= 156150) {
      return (10657.5 + (ti - 77400) * 0.25)
    } else if (ti <= 237950) {
      return (30345 + (ti - 156150) * 0.28)
    } else if (ti <= 424950) {
      return (53249 + (ti - 237950) * 0.33)
    } else if (ti <= 480050) {
      return (114959 + (ti - 424950) * 0.35)
    } else 
      return (134244 + (ti - 480050) * 0.396)
  }
  
  status <- reactive({ input$status })
  kids <- reactive({ as.numeric(input$children) })
  exempts <- reactive({ as.numeric(input$exemptions) })
  income <- reactive({ as.numeric(input$income) })
  mtg <- reactive({ as.numeric(input$mtg) })
  salt <- reactive({ as.numeric(input$salt) })
  prop <- reactive({ as.numeric(input$property) })
  charity <- reactive({ as.numeric(input$charity) })
  med <- reactive({ as.numeric(input$medical) })
  other <- reactive({ as.numeric(input$other) })
  
  
  old.itemized <- reactive({ salt() + mtg() + prop() + med() + charity() + other() })
  new.itemized <- reactive({ min(10000, salt() + prop() ) + mtg() + med() + charity() + other() })
  old.standard <- reactive({ ifelse(status() == "Single", 6350, 12700) })
  new.standard <- reactive({ ifelse(status() == "Single", 12000, 24000) })
  old.taxable <- reactive({ max(0, income() - max(old.standard(), old.itemized() ) - 4050*exempts() ) })
  new.taxable <- reactive({ max(0, income() - max(new.standard(), new.itemized() )) })  
  old.tax.due <- reactive({ ifelse(status() == "Single", 
                                   old.single(old.taxable() ) - 1000*kids(), 
                                   old.mfj(old.taxable() ) - 1000*kids() ) })
  new.tax.due <- reactive({ ifelse(status() == "Single", 
                                   new.single(new.taxable() ) - 2000*kids(), 
                                   new.mfj(new.taxable() ) - 2000*kids() ) })
  
  tax.tbl <- reactive({
    row.text <- c("Gross Income", "Allowable Itemized Deductions", "Standard Deduction", 
                  "Personal Exemptions", "Total Deductions", "Taxable Income Amount", "Child Tax Credit", "Taxes Due", "After Tax Income")
    old.values <- c(income(), old.itemized(), old.standard(), 4050*exempts(), max(old.standard(), old.itemized() ) + 4050*exempts(), 
                    old.taxable(), 1000*kids(), old.tax.due(), income() - old.tax.due() )
    new.values <- c(income(), new.itemized(), new.standard(), "N/A", max(new.standard(), new.itemized() ), new.taxable(),
                    2000*kids(), new.tax.due(), income() - new.tax.due() )
    tbl.out <- cbind(row.text, old.values, new.values)
    colnames(tbl.out) <- c(" ", "Old Plan", "New Plan")
    return(data.table(tbl.out))
  })
  
  output$text3 <- renderText({ paste("<h3>Estimated net difference for your return is</h3>")})
  output$text4 <- renderText({ ifelse((new.tax.due()-old.tax.due()) <= 0,
                                      paste("<font color=\"#229954\"><b><h1>", 
                                            "$", old.tax.due()-new.tax.due(), "</h1><h4>",
                                            " ($", round((old.tax.due()-new.tax.due())/12, 2), " per month)",
                                            "</h4></b></font>", sep=""),
                                      paste("<font color=\"#FF0000\"><b><h1>", 
                                            "-$", new.tax.due()-old.tax.due(), "</h1><h4>",
                                            " (-$", round((new.tax.due()-old.tax.due())/12, 2), " per month)",
                                            "</h4></b></font>", sep="")) })
  
  output$table <- renderTable({ tax.tbl() }, bordered = T, align = 'r')
}

# Run the application
shinyApp(ui = ui, server = server)

