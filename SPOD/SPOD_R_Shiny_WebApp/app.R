# Load required libraries
library(shiny)
library(shinydashboard)
library(plotly)
library(readr)
library(DT)
library(zoo)  # For moving average calculation
library(shinyjs)  # For using HTML tags and CSS

# Custom CSS for horizontal scrolling
custom_css <- "
  .dataTables_wrapper {
    overflow-x: auto;
  }
  .github-icon {
    display: inline-block;
    margin-left: 5px;
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
      menuItem("Scatter Plot", tabName = "scatter_plot_tab", icon = icon("diagram-project")),
      menuItem("Data Preview", tabName = "data_preview_tab", icon = icon("table")),
      menuItem("Summary", tabName = "summary_tab", icon = icon("list-alt")),
      menuItem("Log", tabName = "log_tab", icon = icon("clipboard"))
    )
  ),
  dashboardBody(
    useShinyjs(),
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
                ),
                box(title = "Scatter Plot", status = "primary", solidHeader = TRUE, width = 12,
                    checkboxInput("scatter_enable", "Enable Scatter Plot", FALSE),
                    uiOutput("x_column_selector"),
                    uiOutput("y_column_selector")
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
      tabItem(tabName = "scatter_plot_tab",
              fluidRow(
                box(title = "Scatter Plot", status = "primary", solidHeader = TRUE, width = 12,
                    uiOutput("scatter_plot_ui"))
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
             p(HTML('Created by: Hunter Tiner, M.Sc.
                    <a href="https://github.com/HuLaTin" target="_blank">
                    <i class="fab fa-github github-icon"></i> [HuLaTin]</a>'), 
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
  
  output$x_column_selector <- renderUI({
    req(input$scatter_enable, data())
    cols <- names(data())[-1] # Exclude the first column
    selectInput("x_column", "X Column", choices = cols, selected = cols[1], selectize = TRUE)
  })
  
  output$y_column_selector <- renderUI({
    req(input$scatter_enable, data())
    cols <- names(data())[-1] # Exclude the first column
    selectInput("y_column", "Y Column", choices = cols, selected = cols[2], selectize = TRUE)
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
  
  output$scatter_plot_ui <- renderUI({
    if (!input$scatter_enable || is.null(input$x_column) || is.null(input$y_column)) {
      h4("Please enable the scatter plot and select the columns for X and Y axes.")
    } else {
      plotlyOutput("scatter_plot", height = "700px")
    }
  })
  
  output$scatter_plot <- renderPlotly({
    req(input$scatter_enable, input$x_column, input$y_column, data())
    
    scatter_df <- data()
    
    max_abs_x <- max(abs(scatter_df[[input$x_column]]), na.rm = TRUE)
    max_abs_y <- max(abs(scatter_df[[input$y_column]]), na.rm = TRUE)
    max_abs <- max(max_abs_x, max_abs_y)
    
    p <- plot_ly(scatter_df, x = ~scatter_df[[input$x_column]], y = ~scatter_df[[input$y_column]], type = 'scatter', mode = 'markers')
    p <- layout(p, title = "Scatter Plot",
                xaxis = list(title = input$x_column, range = c(-max_abs, max_abs)),
                yaxis = list(title = input$y_column, range = c(-max_abs, max_abs)),
                shapes = list(
                  list(type = 'line', x0 = -max_abs, x1 = max_abs, y0 = 0, y1 = 0, line = list(color = 'black', dash = 'dash')),
                  list(type = 'line', x0 = 0, x1 = 0, y0 = -max_abs, y1 = max_abs, line = list(color = 'black', dash = 'dash'))
                ))
    p
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
