#' This is a module which will serve as log in and log out page
#' 


# 1. UI -------------------------------------------------------------------
m_ui_login <- function( id ) {
  
  ns <- shiny::NS(id)
version <- "2025-07-08 11:02"
  fluidRow( id = ns("panel_login"),
            
            column( width = 3, offset = 4, align = "center",
                    
                    div( class = "panel-body",
                         
                         h3("Welcome to TreeNet"),
                         
                         div( id = "panel-body",
                           p("To access our data portal, please authenticate with your user credentials below."),
                           
                           br(),
                           
                           textInput( inputId = ns("user_id"), label = "Username:", width = "100%"), #value = "tnreader"),
                           
                           passwordInput( inputId = ns("user_pwd"), label = "Password:", width = "100%"), #value = "Iku8Gem3"),
                           
                           br(),
                           
                           actionButton( inputId = ns("b_login"), label = "Login", width = "100%", class = "btn-primary" ),
                           
                           br()
                         )
                    ),
                    
                    div( id = ns("result_auth") ),
                    br(),
                    p(paste0("App last updated on ",version))
            )
  )
  
}


m_ui_logout <- function( id ) {
  #' @description module that allows user to log out from the application. The 
  #' button will appear only after successful log in. Otherwise, it will be hidden.
  #' @param id is the unique environment (namespace) needed for the modules
  
  ns <- shiny::NS(id)
  
  shiny::div(class = "pull-right",
             shinyjs::hidden(
               shiny::actionButton(
                 inputId = ns("b_logout"),
                 label = "Logout",
                 class = "btn-warning",
                 icon = icon("sign-out-alt")
               )
             )
  )
}



# 2. Server ---------------------------------------------------------------
m_s_login <- function(input, output, session, log_out = NULL) {
  
  ns <- session$ns
  jns <- function(x) {
    paste0("#", ns(x))
  }
  
  db_con <- reactiveValues( logged = FALSE, cc = NULL, scope = NULL, username = NULL)
  
  shiny::observeEvent( log_out(), {
    
    # disconnect from DB
    if( db_con$logged ){
      DBI::dbDisconnect( db_con$cc )
      isolate({users$count <- users$count - 1})
    }
    
    db_con$logged = FALSE
    
    # clean the log-in
    shiny::updateTextInput(session, "user_id", value = "")
    shiny::updateTextInput(session, "user_pwd", value = "")
  })
  
  
  shiny::observeEvent( db_con$logged, ignoreInit = TRUE, {
    # if the user successfully logged, hide the log-in panel
    shinyjs::toggle(id = "panel_login")
  })


shiny::observeEvent(input$b_login, {
  removeUI(selector = jns("msg_usr"))

  user <- tolower(input$user_id)
  pwd  <- input$user_pwd

  # Try getting the scope first
  scope <- try(get_tn_scope(user, pwd), silent = TRUE)

  if (inherits(scope, "try-error") || is.null(scope)) {
    db_con$logged <- FALSE
    db_con$cc     <- NULL
    db_con$scope  <- NULL
    db_con$username <- NULL

    insertUI(
      selector = jns("result_auth"),
      ui = div(
        id = ns("msg_usr"), 
        class = "alert alert-danger", 
        icon("exclamation-triangle"), 
        HTML("Authentication failed. Invalid username or password. Please contact the <a href='mailto:micah.wilhelm@wsl.ch?subject=TreeNet Portal credentials error'>database administrator</a>.")
      )
    )

    shiny::updateTextInput(session, "user_id", value = "")
    shiny::updateTextInput(session, "user_pwd", value = "")
    return(NULL)
  }

  # If scope was valid, proceed to connect
  conn <- try(dbConnect_tn(username = user, password = pwd, db_host = 'pgdbtreenet', db_name = 'treenet'), silent = TRUE)

  if (inherits(conn, "try-error") || is.null(conn)) {
    db_con$logged <- FALSE
    db_con$cc     <- NULL

    shiny::showNotification("Database connection failed after multiple attempts.", type = "error")
    return(NULL)
  }

  # Success
  db_con$cc       <- conn
  db_con$scope    <- scope
  db_con$username <- user
  db_con$logged   <- TRUE

  req(users$count)
  isolate({ users$count <- users$count + 1 })
})



  # shiny::observeEvent( input$b_login, {
  #   # main log-in. If successful we return the credentials. If not, we remove 
    
  #   removeUI(selector = jns("msg_usr"))
    
  #   db_con$cc <- try( dbConnect_tn( username = tolower(input$user_id), password = input$user_pwd, db_host = 'pgdbtreenet', db_name = 'treenet'), TRUE)
  #   db_con$scope <- get_tn_scope( username = tolower(input$user_id), password = input$user_pwd)
  #   db_con$username <-input$user_id
    
  #   if( "try-error" %in% class(db_con$cc)){
      
  #     db_con$logged = FALSE
      
  #     insertUI( selector = jns("result_auth"),
  #               ui = div( id = ns("msg_usr"), 
  #                         class = "alert alert-danger", 
  #                         icon("exclamation-triangle"), 
  #                         HTML("Username or password are incorrect!. Please contact the database <a href='mailto:micah.wilhelm@wsl.ch?subject=TreeNet Portal credentials error'>administrator</a>."))
  #     )
      
  #     # clean the log-in
  #     shiny::updateTextInput(session, "user_id", value = "")
  #     shiny::updateTextInput(session, "user_pwd", value = "")
      
  #   } else {
  #     req(users$count)
  #     isolate({users$count <- users$count + 1})
  #     db_con$logged = TRUE
      
  #   }
    
  # })
  
  # 7. Concurrent user limitation ---------------------------------------------------
  shiny::observeEvent( users$count, {
    removeUI(selector = jns("msg_usr"))
    shinyjs::enable("b_login")
    
    if (users$count == 0) {
      insertUI( selector = jns("result_auth"),
                ui = div( id = ns("msg_usr"), 
                          class = "alert alert-info", 
                          icon("smile"), 
                          HTML("There are no users logged on.")))
    } else if (users$count <= 5) {
      insertUI( selector = jns("result_auth"),
                ui = div( id = ns("msg_usr"), 
                          class = "alert alert-warning", 
                          icon("info-circle"), 
                          HTML(paste0("There are ", users$count, " users are logged on. <br>Server response may be slower."))))
    } else {
      shinyjs::disable("b_login")
      insertUI( selector = jns("result_auth"),
                ui = div( id = ns("msg_usr"), 
                          class = "alert alert-danger", 
                          icon("exclamation-triangle"), 
                          HTML(paste0("There are ", users$count, " users logged on. <br>This is the maximum load for our server. <br>Please try again later."))))
    }
  })
  
  return( db_con )
  
}

m_s_logout <- function(input, output, session, active) {
  
  shiny::observeEvent(active(), ignoreInit = TRUE, {
    shinyjs::toggle(id = "b_logout", anim = FALSE)
    
  })
  
  # return reactive logout button tracker
  shiny::reactive({ input$b_logout })
}
