# Created by: Hunter Tiner [HuLaTin@gmail.com]
# https://hulatin.shinyapps.io/SPOD_Viz_App/

library(shiny)
library(shinydashboard)
library(plotly)
library(readr)
library(DT)
library(zoo)  # For moving average calculation

# Custom CSS for horizontal scrolling
custom_css <- "
  .dataTables_wrapper {
    overflow-x: auto;
  }
"

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(
    title = "UALR IOST Visualization App",
    titleWidth = 350  # Make the title responsive to display size
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload & Control Data", tabName = "control_tab", icon = icon("upload")),
      menuItem("Plot", tabName = "plot_tab", icon = icon("line-chart")),
      menuItem("Data Preview", tabName = "data_preview_tab", icon = icon("table")),
      menuItem("Summary", tabName = "summary_tab", icon = icon("list-alt")),
      menuItem("Log", tabName = "log_tab", icon = icon("clipboard"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(custom_css))),  # Include the custom CSS
    tabItems(
      tabItem(tabName = "control_tab",
              fluidRow(
                box(title = "Upload CSV", status = "primary", solidHeader = TRUE, width = 12,
                    fileInput("file1", "Choose CSV File",
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                    checkboxInput("header", "Header", TRUE)
                ),
                box(title = "Select Columns", status = "primary", solidHeader = TRUE, width = 12,
                    uiOutput("column_selector")),
                box(title = "Smoothing & Normalization", status = "primary", solidHeader = TRUE, width = 12,
                    checkboxInput("smoothing", "Apply Moving Average Smoothing", FALSE),
                    numericInput("window_size", "Window Size for Moving Average", value = 5, min = 1),
                    checkboxInput("normalize", "Normalize Data", FALSE)
                )
              )
      ),
      tabItem(tabName = "plot_tab",
              fluidRow(
                box(title = "Plot", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("plot", height = "700px"))
              ),
              fluidRow(
                box(title = "Time Filter", status = "primary", solidHeader = TRUE, width = 12,
                    uiOutput("time_slider"))
              )
      ),
      tabItem(tabName = "data_preview_tab",
              fluidRow(
                box(title = "Data Preview", status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("dataTable"))
              )
      ),
      tabItem(tabName = "summary_tab",
              fluidRow(
                box(title = "Summary Statistics", status = "primary", solidHeader = TRUE, width = 12,
                    verbatimTextOutput("summary"))
              )
      ),
      tabItem(tabName = "log_tab",
              fluidRow(
                box(title = "Log", status = "primary", solidHeader = TRUE, width = 12,
                    verbatimTextOutput("log"))
              )
      )
    ),
    fluidRow(
      column(width = 12, align = "center",
             p(HTML('Created by: Hunter Tiner, M.Sc. [HuLaTin]<br><a href="https://github.com/HuLaTin" target="_blank">GitHub Profile</a>'), 
               style = "font-size: 12px; color: gray;")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  log <- reactiveVal("")
  
  observeEvent(input$file1, {
    log(paste(log(), "File uploaded:", input$file1$name, "\n", sep = "\n"))
  })
  
  data <- reactive({
    req(input$file1)
    read_csv(input$file1$datapath, col_names = input$header)
  })
  
  output$column_selector <- renderUI({
    req(data())
    cols <- names(data())[-1] # Exclude the first column
    selectInput("selected_columns", "Columns", choices = cols, selected = cols, multiple = TRUE, selectize = TRUE)
  })
  
  output$dataTable <- renderDT({
    req(data())
    datatable(data(), options = list(scrollX = TRUE))  # Enable horizontal scrolling
  })
  
  output$summary <- renderPrint({
    req(data())
    summary(data())
  })
  
  output$log <- renderText({
    log()
  })
  
  output$time_slider <- renderUI({
    req(data())
    df <- data()
    df[[1]] <- as.POSIXct(df[[1]], format="%Y-%m-%d %H:%M:%S")
    sliderInput("time_range", "Select Time Range:",
                min = min(df[[1]], na.rm = TRUE),
                max = max(df[[1]], na.rm = TRUE),
                value = c(min(df[[1]], na.rm = TRUE), max(df[[1]], na.rm = TRUE)),
                timeFormat = "%Y-%m-%d %H:%M:%S")
  })
  
  output$plot <- renderPlotly({
    req(data(), input$selected_columns, input$time_range)
    
    df <- data()
    
    # Convert the first column to date/time if not already
    df[[1]] <- as.POSIXct(df[[1]], format="%Y-%m-%d %H:%M:%S")
    
    # Filter data based on time range
    df <- df[df[[1]] >= input$time_range[1] & df[[1]] <= input$time_range[2], ]
    
    # Filter columns based on selection
    selected_df <- df[, c(names(df)[1], input$selected_columns), drop = FALSE]
    
    # Apply moving average if smoothing is enabled
    if (input$smoothing) {
      window_size <- input$window_size
      for (i in 2:ncol(selected_df)) {
        selected_df[[i]] <- rollmean(selected_df[[i]], k = window_size, fill = NA)
      }
    }
    
    # Apply normalization if enabled
    if (input$normalize) {
      for (i in 2:ncol(selected_df)) {
        selected_df[[i]] <- (selected_df[[i]] - min(selected_df[[i]], na.rm = TRUE)) / 
          (max(selected_df[[i]], na.rm = TRUE) - min(selected_df[[i]], na.rm = TRUE))
      }
    }
    
    # Create the plot
    p <- plot_ly(selected_df, x = ~selected_df[[1]])
    for(i in 2:ncol(selected_df)) {
      p <- add_lines(p, y = selected_df[[i]], name = names(selected_df)[i])
    }
    p <- layout(p, title = "Time Series Plot",
                xaxis = list(title = "Time"),
                yaxis = list(title = "Values"))
    p
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
