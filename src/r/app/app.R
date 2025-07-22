# app.R

library(shiny)

ui <- fluidPage(
  titlePanel("Terminal Interface"),
  sidebarLayout(
    sidebarPanel(
      textInput("command", "Enter Terminal Command:", value = "ls"),
      actionButton("run", "Run Command")
    ),
    mainPanel(
      verbatimTextOutput("output")
    )
  )
)

server <- function(input, output, session) {
  result <- eventReactive(input$run, {
    cmd <- input$command

    # Optional: restrict to safe commands
    allowed <- c("ls", "pwd", "whoami", "cat", "echo", "date")
    if (!any(startsWith(cmd, allowed))) {
      return("Command not allowed.")
    }

    tryCatch({
      system(cmd, intern = TRUE)
    }, error = function(e) {
      paste("Error:", e$message)
    })
  })

  output$output <- renderText({
    paste(result(), collapse = "\n")
  })
}

shinyApp(ui, server)
