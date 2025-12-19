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
      
      do.call(tagList, lapply(
        c("CAB", "Block", "Agn", "DAB", "Conj", "Sub", "Stop/Read"),
        function(step) {
          tagList(
            tags$br(),
            h4(strong(step)),
            fluidRow(
              
              # Hours
              column(4, numericInput(paste0(step, "_hr"), "Hr:",
                  value = ifelse(step == "CAB", 3,
                          ifelse(step == "Block", 2,
                          ifelse(step == "Agn", 18,
                          ifelse(step == "DAB", 3,
                          ifelse(step == "Conj", 0,
                          ifelse(step == "Sub", 0, 0)))))),
                  min = 0)),
              
              # Minutes
              column(4, numericInput(paste0(step, "_min"), "Min:",
                  value = ifelse(step == "Conj", 45,
                          ifelse(step == "Sub", 10,
                          ifelse(step == "Stop/Read", 5, 0))),
                  min = 0, max = 59)),
              
              # Transfer time (not needed for CAB, Sub, Stop/Read)
              if (!(step %in% c("CAB", "Sub", "Stop/Read")))
                column(4, numericInput(paste0(step, "_transfer"), "Transfer (min):",
                    value = 5, min = 0))),
            tags$hr()
          )})),
      
      actionButton("calc", "Calculate Timings")),
    
    mainPanel(
      h3("Step Schedule"),
      tableOutput("schedule"))))


server <- function(input, output, session) {
  
  observeEvent(input$calc, {
    
    # Combine date and time
    start_datetime <- as.POSIXct(
      paste(input$start_day, input$start_time),
      format = "%Y-%m-%d %H:%M",
      tz = Sys.timezone())
    
    # Step names
    steps <- c("CAB", "Block", "Agn", "DAB", "Conj", "Sub", "Stop/Read")
    
    # Step durations
    durations_min <- sapply(steps,
      function(s) input[[paste0(s, "_hr")]]*60 + input[[paste0(s, "_min")]])
    
    # Transfer times
    transfers_min <- sapply(steps,
      function(s) {
        transfer_val <- input[[paste0(s, "_transfer")]]
        if (is.null(transfer_val)) 0 else transfer_val})
    
    # Build schedule table
    df <- data.frame(Step = character(),
                     Duration = character(),
                     Start_Time = character(),
                     End_Time = character(),
                     stringsAsFactors = FALSE)
    
    current_time <- start_datetime
    
    for (i in seq_along(steps)) {
      step_name <- steps[i]
      step_duration <- durations_min[i]
      
      step_start <- current_time
      step_end   <- current_time + minutes(step_duration)
      
      df <- rbind(df,
        data.frame(
          Step = step_name,
          Duration = sprintf("%02d hr %02d min", step_duration %/% 60, step_duration %% 60),
          Start_Time = format(step_start, "%A, %d %b %Y @ %H:%M"),
          End_Time = format(step_end, "%A, %d %b %Y @ %H:%M"),
          stringsAsFactors = FALSE))
      
      # Move time forward
      current_time <- step_end + minutes(transfers_min[i])}
    
    output$schedule <- renderTable(df,
      bordered = TRUE,
      striped = TRUE,
      hover = TRUE,
      sanitize.text.function = function(x) x)})}

shinyApp(ui, server)
