library(shiny)
library(lubridate)
library(dplyr)

ui <- fluidPage(
  titlePanel("ELISA Timepoints Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      dateInput("start_day", "Start Day:", value = Sys.Date()),
      textInput("start_time", "Start Time (HH:MM, 24-hour):", value = "08:00"),
      
      tags$br(),
      h4("Enter duration and transfer time for each step:"),
      
      # Step inputs
      lapply(c("CAB", "Block", "Agn", "DAB", "Conj", "Sub", "Stop/Read"), function(step) {
        tagList(
          tags$br(),
          h4(strong(step)),
          fluidRow(
            column(4, numericInput(paste0(step, "_hr"), "Hr:",
                                   value = ifelse(step == "CAB", 3,
                                           ifelse(step == "Block", 2,
                                           ifelse(step == "Agn", 18,
                                           ifelse(step == "DAB", 3,
                                           ifelse(step == "Conj", 0,
                                           ifelse(step == "Sub", 0, 0)))))), 
                                   min = 0)),
            column(4, numericInput(paste0(step, "_min"), "Min:",
                                   value = ifelse(step == "Conj", 45,
                                           ifelse(step == "Sub", 10,
                                           ifelse(step == "Stop/Read", 5, 0))),
                                   min = 0, max = 59)),
            if (!(step %in% c("Sub", "Stop/Read")))
              column(4, numericInput(paste0(step, "_transfer"),
                                     "Transfer (min):", value = 5, min = 0))),
          tags$hr())}),
      actionButton("calc", "Calculate Timings")),
    mainPanel(h3("Step Schedule"),  tableOutput("schedule"))))

server <- function(input, output, session) {
  observeEvent(input$calc, {
    
    # Combine date and time
    start_datetime <- as.POSIXct(
      paste(input$start_day, input$start_time),
      format = "%Y-%m-%d %H:%M",
      tz = Sys.timezone())
    
    # Step names
    steps <- c("CAB", "Block", "Agn", "DAB", "Conj", "Sub", "Stop/Read")
    
    # Prepare durations
    durations_min <- sapply(steps, function(s) input[[paste0(s,"_hr")]]*60 + input[[paste0(s,"_min")]])
    
    # Prepare transfers, set 0 for Sub and Stop/Read
    transfers_min <- sapply(steps, function(s) {
      if (s %in% c("Sub", "Stop/Read")) {0
      } else {
        input[[paste0(s,"_transfer")]]
      }})
    
    # Create empty table
    df <- data.frame(Step = character(), Duration = character(), Start_Time = character(), End_Time = character(), stringsAsFactors = FALSE)
    
    current_time <- start_datetime
    
    for(i in seq_along(steps)) {
      # Step
      step_name <- steps[i]
      step_duration <- durations_min[i]
      
      df <- rbind(df, data.frame(
        Step = step_name,
        Duration = sprintf("%02d hr %02d min", step_duration %/% 60, step_duration %% 60),
        Start_Time = format(current_time, "%A, %d %b %Y @ %H:%M"),  # Include day of week
        End_Time = format(current_time + minutes(step_duration), "%A, %d %b %Y @ %H:%M"),
        stringsAsFactors = FALSE))
      
      # Advance current_time by step duration
      current_time <- current_time + minutes(step_duration)
      
      # **Only advance time for transfer, do NOT add a row**
      transfer_time <- transfers_min[i]
      current_time <- current_time + minutes(transfer_time)
    }
    
    output$schedule <- renderTable(df, bordered = TRUE, striped = TRUE, hover = TRUE, sanitize.text.function = function(x) x)
  })}

shinyApp(ui, server)
