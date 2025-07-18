server <- function(input, output, session) {
    
    #users <- reactiveValues(count = 0)

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
    output$ui_admin_panel <- renderUI({
    if (!is.null(user_con$username) && user_con$username == "wilhelm") {
      actionButton("admin_disconnect", "Disconnect all users")
    }})
    observeEvent(input$admin_disconnect, {session$close()})

    # Session end -------------------------------------------------------------
    session$onSessionEnded(function(){
      if( isolate(user_con$logged )){
        DBI::dbDisconnect( isolate(user_con$cc ))
        isolate({users$count <- users$count - 1})
      }
      # stopApp()
    })
    # Set this to "force" instead of TRUE for testing locally (without Shiny Server)
    session$allowReconnect(TRUE)
  }