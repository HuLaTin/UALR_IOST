library(plotly)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(shiny)

# Define UI
options(shiny.maxRequestSize = 150*1024^2)
ui <- fluidPage(theme = shinytheme("cerulean"),
                
                # Application title
                titlePanel("Sensor Data Analysis"),
                
                # Sidebar layout
                sidebarLayout(
                  sidebarPanel(
                    fileInput(inputId = "file1", label = "Upload CSV File",
                              multiple = FALSE,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                    radioButtons("smooth", "Smoothing Method",
                                 c("None" = "none",
                                   "Moving Average" = "ma",
                                   "LOESS" = "loess")),
                    numericInput("window", "Moving Average Window Size", value = 5, min = 1),
                    uiOutput("column_selector"),
                    width = 3
                  ),
                  
                  mainPanel(
                    tabsetPanel(type = "tabs",
                                tabPanel(
                                  titlePanel("Plot"),
                                  plotlyOutput("sensorPlot", height = 720)
                                ),
                                tabPanel(
                                  titlePanel("Summary"),
                                  verbatimTextOutput("summary")
                                ),
                                tabPanel(
                                  titlePanel("Log"),
                                  verbatimTextOutput("log")
                                ),
                                tabPanel(
                                  titlePanel("Data Preview"),
                                  tableOutput("dataPreview")
                                )
                    ),
                    width = 9
                  )
                )
)

# Define server logic
server <- function(input, output, session) {
  
  data <- reactive({
    req(input$file1)
    log_messages <- ""
    
    # Read the raw contents of the file for debugging
    raw_contents <- tryCatch({
      readLines(input$file1$datapath)
    }, error = function(e) {
      log_messages <<- paste(log_messages, "Error reading file: ", e$message, sep = "\n")
      output$log <- renderPrint(log_messages)
      return(NULL)
    })
    
    if (is.null(raw_contents)) {
      log_messages <<- paste(log_messages, "Raw file contents are NULL.", sep = "\n")
      output$log <- renderPrint(log_messages)
      return(NULL)
    }
    
    log_messages <<- paste(log_messages, "Raw file contents:\n", paste(raw_contents[1:min(5, length(raw_contents))], collapse = "\n"), sep = "\n")
    
    # Initialize empty dataframe to store valid rows
    df <- data.frame()
    
    for (line in raw_contents) {
      tryCatch({
        temp_df <- read.csv(text = line, header = FALSE, stringsAsFactors = FALSE)
        if (ncol(temp_df) == 18) {
          df <- rbind(df, temp_df)
        } else {
          log_messages <<- paste(log_messages, "Skipped line due to incorrect number of fields: ", line, sep = "\n")
        }
      }, error = function(e) {
        log_messages <<- paste(log_messages, "Error processing line: ", line, sep = "\n")
      })
    }
    
    if (nrow(df) == 0) {
      log_messages <<- paste(log_messages, "No valid data rows after processing.", sep = "\n")
      output$log <- renderPrint(log_messages)
      return(NULL)
    }
    
    # Update column names to match your CSV file structure
    log_messages <<- paste(log_messages, "Original columns: ", paste(colnames(df), collapse = ", "), sep = "\n")
    
    expected_colnames <- c("Time", "A", "B", "C", "D", "E", "F", "Average", 
                           "Open_X", "Closed_X", "Open_Y", "Closed_Y", 
                           "Open_Radius", "Closed_Radius", "Open_Deg", 
                           "Closed_Deg", "Open_Radian", "Closed_Radian")
    
    if (length(colnames(df)) != length(expected_colnames)) {
      log_messages <<- paste(log_messages, "Column count mismatch. Expected columns: ", paste(expected_colnames, collapse = ", "), sep = "\n")
      output$log <- renderPrint(log_messages)
      return(NULL)
    }
    
    colnames(df) <- expected_colnames
    
    log_messages <<- paste(log_messages, "Updated columns: ", paste(colnames(df), collapse = ", "), sep = "\n")
    
    # Remove rows with missing Time values
    df <- df[!(is.na(df$Time) | df$Time == ""), ]
    log_messages <<- paste(log_messages, "Rows after removing missing Time values: ", nrow(df), sep = "\n")
    
    # Set Time to POSIXct
    df$Time <- tryCatch({
      as.POSIXct(df$Time, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
    }, error = function(e) {
      log_messages <<- paste(log_messages, "Error converting Time to POSIXct: ", e$message, sep = "\n")
      output$log <- renderPrint(log_messages)
      return(NULL)
    })
    
    if (is.null(df$Time)) {
      log_messages <<- paste(log_messages, "Time conversion resulted in NULL values.", sep = "\n")
      output$log <- renderPrint(log_messages)
      return(NULL)
    }
    
    log_messages <<- paste(log_messages, "Time column converted to POSIXct.", sep = "\n")
    
    output$log <- renderPrint(log_messages)
    df
  })
  
  smoothed_data <- reactive({
    df <- data()
    if (is.null(df)) return(NULL)
    
    smooth_method <- input$smooth
    
    smooth_df <- df
    selected_cols <- input$selected_columns
    
    if (is.null(selected_cols) || length(selected_cols) == 0) return(smooth_df)
    
    if (smooth_method == "ma") {
      window <- input$window
      for (col in selected_cols) {
        smooth_df[[col]] <- stats::filter(df[[col]], rep(1/window, window), sides = 2)
      }
    } else if (smooth_method == "loess") {
      for (col in selected_cols) {
        smooth_df[[col]] <- predict(loess(df[[col]] ~ df$Time))
      }
    }
    
    smooth_df
  })
  
  output$column_selector <- renderUI({
    df <- data()
    if (is.null(df)) return(NULL)
    selectInput("selected_columns", "Select Columns to Plot:", 
                choices = names(df)[2:ncol(df)], 
                selected = names(df)[2:7],  # Default to columns A-F
                multiple = TRUE)
  })
  
  output$dataPreview <- renderTable({
    df <- data()
    if (is.null(df)) return(NULL)
    head(df)
  })
  
  output$sensorPlot <- renderPlotly({
    df <- smoothed_data()
    if (is.null(df)) {
      output$log <- renderPrint("Data is NULL")
      return(NULL)
    }
    
    selected_cols <- input$selected_columns
    if (is.null(selected_cols) || length(selected_cols) == 0) {
      output$log <- renderPrint("No columns selected")
      return(NULL)
    }
    
    # Create the plot
    p <- plot_ly()
    for (col in selected_cols) {
      if (!is.null(df[[col]]) && !all(is.na(df[[col]]))) {
        p <- p %>% add_trace(x = df$Time, y = df[[col]], name = col, mode = 'lines', type = 'scatter')
      } else {
        output$log <- renderPrint(paste("Column", col, "is NULL or has no data"))
      }
    }
    
    if (length(selected_cols) > 0) {
      p %>% layout(
        xaxis = list(title = 'Time'),
        yaxis = list(title = 'Values'),
        showlegend = TRUE
      )
    } else {
      p <- plot_ly()
    }
  })
  
  output$summary <- renderPrint({
    df <- data()
    if (is.null(df)) return("No data available")
    
    selected_cols <- input$selected_columns
    if (is.null(selected_cols) || length(selected_cols) == 0) return("No columns selected.")
    
    summary(df[, selected_cols, drop = FALSE])
  })
}

# Run the application
shinyApp(ui = ui, server = server)
