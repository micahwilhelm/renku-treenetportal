ui <- shiny::fluidPage(
  shinyjs::useShinyjs(),
  waiter::useWaiter(),
  # loading indicator over the data table while it is being recalculated (data checks, date / time zone changes)
  waiter::autoWaiter(
    id = "table_out",
    html = htmltools::tagList(waiter::spin_wave(), htmltools::h4("Loading data …")),
    color = "rgba(51, 62, 72, 0.75)"
  ),
  # add logout button UI
  m_ui_logout(id = "logout"),
  # add login panel UI function
  m_ui_login(id = "login"),
  uiOutput("ui_adata_vis")
)

# Run app
# shiny::runApp(".", launch.browser = T) #function(url) system2("xdg-open", url))


# Deploy app
# system(paste0("sed -i '9s/.*/version <- \"",format(lubridate::now(), "%Y-%m-%d %H:%M"),"\"/' ~/treenet/treenetportal/scripts/0_log_in_out.R")); system("cp -rf ~/treenet/treenetportal/* ~/ShinyApps/treenetportal/")