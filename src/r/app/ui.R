ui <- fluidPage(
    
    shinyjs::useShinyjs(),
    # add logout button UI 
    m_ui_logout(id = "logout"),
    # add login panel UI function
    m_ui_login(id = "login"),
    uiOutput("ui_adata_vis")
  )


# Deploy app
# system(paste0("sed -i '9s/.*/version <- \"",format(lubridate::now(), "%Y-%m-%d %H:%M"),"\"/' ~/treenet/treenetportal/scripts/0_log_in_out.R")); system("cp -rf ~/treenet/treenetportal/* ~/ShinyApps/treenetportal/")