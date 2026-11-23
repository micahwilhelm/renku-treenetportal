server <- function(input, output, session) {
    
    #users <- reactiveValues(count = 0)
      # ensure shared users reactive (near top of server)
if (!exists("users") || !inherits(users, "reactivevalues")) {
  users <- reactiveValues(count = 0, list = character(0))
}

    #source('scripts/0_log_in_out.R', local = T)

    # Login module ------------------------------------------------------------
    
    # call the log-out module with reactive trigger to hide/show
    logout_init <- callModule(m_s_logout,  id = "logout", active = reactive(user_con$logged))
    
    # call log-in module supplying data frame, user and password cols
    # and reactive trigger
    user_con <- callModule(m_s_login, id = "login", log_out = reactive(logout_init()))
    
    
    

    # 1. Main user interface page ---------------------------------------------
    source('scripts/1_ui_adata_vis.R', local=TRUE)


    # 2. Get the main data from the databse -----------------------------------
    source('scripts/2_server_adata_vis.R', local=TRUE)

    # 3. Admin panel ----------------------------------------------------------
  
  # render the user list only for admin
output$ui_admin_panel <- renderUI({
  if (!is.null(user_con$username) && user_con$username == "micah.wilhelm@wsl.ch") {
    ul_items <- isolate(users$list)
    if (length(ul_items) == 0) {
      div(class = "panel panel-default",
          div(class = "panel-heading", "Logged-on users"),
          div(class = "panel-body", "No users logged on")
      )
    } else {
      div(class = "panel panel-default",
          div(class = "panel-heading", "Logged-on users"),
          div(class = "panel-body",
              tags$ul(lapply(ul_items, tags$li)),
              actionButton("admin_disconnect", "Disconnect all users")
          )
      )
    }
  } else {
    NULL
  }
})

# keep users$list in sync with your login module
observeEvent(user_con$logged, ignoreInit = TRUE, {
  if (isTRUE(isolate(user_con$logged))) {
    uname <- isolate(user_con$username)
    if (!is.null(uname) && nzchar(uname)) {
      users$list <- unique(c(isolate(users$list), uname))
    }
  } else {
    uname <- isolate(user_con$username)
    users$list <- setdiff(isolate(users$list), uname)
  }
  users$count <- length(isolate(users$list))
})
  
observeEvent(input$admin_disconnect, {session$close()})


# Session end -------------------------------------------------------------
# remove user on session end
session$onSessionEnded(function() {
  if (isolate(user_con$logged)) {
    DBI::dbDisconnect(isolate(user_con$cc))
    isolate({
      users$list <- setdiff(users$list, isolate(user_con$username))
      users$count <- length(users$list)
    })
  }
})
    # Set this to "force" instead of TRUE for testing locally (without Shiny Server)
    session$allowReconnect(TRUE)
}