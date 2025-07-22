#' This is the main user interface used for the forsm. We have to use a separate
#' `renderUI` for that reason because we reactivly using the log-in page. This page 
#' only appears if the user successfully logs in to the main page. Therefore, we also require the
#' `logged` to be TRUE.

output$ui_adata_vis <- renderUI({
  # keep connection open during long downloads (https://tickets.dominodatalab.com/hc/en-us/articles/360058494751-Increasing-the-timeout-for-Shiny-Server)
  tags$head(
    tags$style(HTML("
      /* CSS for shinytip */
      .shinytip, #export {
        z-index: 9999 !important;
      }
    ")),
    HTML(
      "
          <script>
          var socket_timeout_interval
          var n = 0
          $(document).on('shiny:connected', function(event) {
          socket_timeout_interval = setInterval(function(){
          Shiny.onInputChange('count', n++)
          }, 15000)
          });
          $(document).on('shiny:disconnected', function(event) {
          clearInterval(socket_timeout_interval)
          });
          </script>
          "
    )
  )
  
  req(user_con$logged)
  
  fluidPage(
    
    titlePanel("TreeNet Data Portal"),
    
    # Side panel ----
    sidebarPanel(
      width = 2,
      textInput("command", "Enter Terminal Command:", value = "ls"), #testing
      actionButton("run", "Run Command"), #testing
      radioButtons("dataqual_id", choiceNames = c("L1","L2","LM"), choiceValues = c("L1","L2","LM"), selected = "LM", inline = T,
                   label = shinytip::tip(
                     strong("Data quality", icon("question-circle")),
                     "'L1' means time-aligned data available to the current date. 'L2' means automatically cleaned data available to the current date. 'LM' means manually cleaned data, which is L2 with additional expert processsing available to the end of last year. ",
                     position = getOption("shinytip.position", "right"),
                     length = getOption("shinytip.length", "l"),
                     bg = getOption("shinytip.bg", "black"),
                     fg = getOption("shinytip.fg", "white"),
                     size = getOption("shinytip.size", "12px"),
                     click = getOption("shinytip.click", FALSE),
                     animate = getOption("shinytip.animate", TRUE),
                     pointer = getOption("shinytip.pointer", TRUE))
                   ),
      
      shinyWidgets::pickerInput(
        "tree_genus_species",
        "Species",
        multiple             = T,
        choices              = NULL,
        selected             = NULL,
        width                = "100%",
        options              = shinyWidgets::pickerOptions(
          header             = "",
          liveSearch         = T,
          actionsBox         = T,
          noneSelectedText   = "Select species",
          selectedTextFormat = "count > 2",
          countSelectedText  = "{0} species selected",
          deselectAllText    = "Select none",
          selectAllText      = "Select all"
        )
      ),
      
      shinyWidgets::pickerInput(
        "site_name",
        "Site",
        multiple             = T,
        choices              = NULL,
        selected             = NULL,
        width                = "100%",
        options              = shinyWidgets::pickerOptions(
          header             = "",
          liveSearch         = T,
          liveSearchNormalize= T,
          actionsBox         = T,
          noneSelectedText   = "Select sites",
          selectedTextFormat = "count > 3",
          countSelectedText  = "{0} sites selected",
          deselectAllText    = "Select none",
          selectAllText      = "Select all"
        )
      ),
      
      # selectInput("variable_id", "Variable:", choices = NULL, multiple = TRUE, selected = NULL),
      dateRangeInput("daterange_id", "Date range", start = Sys.Date()-30, end = Sys.Date(), min = "1997-01-01",  max = Sys.Date()),
       shinytip::tip(
        actionButton("export", "Export selected data", icon = icon("download")),
        "Highlight the time series of interest in the right table. You can select a range of rows at once by holding the shift key on the second click.",
        position = getOption("shinytip.position", "right"),
        length = getOption("shinytip.length", "l"),
        bg = getOption("shinytip.bg", "black"),
        fg = getOption("shinytip.fg", "white"),
        size = getOption("shinytip.size", "12px"),
        click = getOption("shinytip.click", FALSE),
        animate = getOption("shinytip.animate", TRUE),
        pointer = getOption("shinytip.pointer", TRUE)
        ),
      uiOutput("ui_admin_panel")
    ),
    
    # Main panel ----
    mainPanel(
      width = 10,
      verbatimTextOutput("output"), #testing
      shinytip::tip(
        strong("Available data", icon("question-circle")),
        "The time series available in the table below are the ones that match your selection criteria.",
        position = getOption("shinytip.position", "right"),
        length = getOption("shinytip.length", "l"),
        bg = getOption("shinytip.bg", "black"),
        fg = getOption("shinytip.fg", "white"),
        size = getOption("shinytip.size", "12px"),
        click = getOption("shinytip.click", FALSE),
        animate = getOption("shinytip.animate", TRUE),
        pointer = getOption("shinytip.pointer", TRUE)),
      DT::dataTableOutput("table_out"),
      
      # actionButton("view", "6. Select data in table", icon = icon("chart-line")),
      # downloadButton("export_out", "7. Export displayed data", icon = icon("download")),
      uiOutput("plot_out"),
      textOutput("keepAlive") #Keep connection open
    ),
  )
})