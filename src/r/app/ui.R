ui <- fluidPage(
    
    shinyjs::useShinyjs(),
    tags$script(HTML("
    	$(document).on('keypress', function(e) {
    	  if (e.which === 13 && $('#login-user_pwd').is(':focus')) {
    	    $('#login-b_login').click();
    	  }
    	});
    	")),
    # add logout button UI 
    m_ui_logout(id = "logout"),
    # add login panel UI function
    m_ui_login(id = "login"),
    uiOutput("ui_adata_vis")
  )


# Deploy app
# system(paste0("sed -i '9s/.*/version <- \"",format(lubridate::now(), "%Y-%m-%d %H:%M"),"\"/' ~/treenet/treenetportal/scripts/0_log_in_out.R")); system("cp -rf ~/treenet/treenetportal/* ~/ShinyApps/treenetportal/")