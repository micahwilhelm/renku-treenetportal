# app.R

library(shiny)

ui <- fluidPage(
  titlePanel("Server File Browser"),
  sidebarLayout(
    sidebarPanel(
      textInput("dir", "Directory Path:", value = getwd()),
      actionButton("browse", "Browse")
    ),
    mainPanel(
      verbatimTextOutput("currentDir"),
      tableOutput("fileList")
    )
  )
)

server <- function(input, output, session) {
  current_dir <- reactiveVal(getwd())

  observeEvent(input$browse, {
    req(input$dir)
    if (dir.exists(input$dir)) {
      current_dir(input$dir)
    } else {
      showNotification("Directory does not exist", type = "error")
    }
  })

  output$currentDir <- renderText({
    paste("Current Directory:", current_dir())
  })

  output$fileList <- renderTable({
    files <- list.files(current_dir(), full.names = TRUE)
    data.frame(
      Name = basename(files),
      Type = ifelse(file.info(files)$isdir, "Directory", "File"),
      Size = file.info(files)$size,
      Modified = file.info(files)$mtime
    )
  })
}

shinyApp(ui, server)
