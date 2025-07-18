#' Once the user logged-in, retrieve all the necessary data for the analysis.
#' We first test if there is a connection, and later after isolating the connection
#' we get all the needed data for the analysis. We assign them with `<<-` so the data
#' are also available afterward, outside of this observe part. 


# 1.  Get the configuration information data ------------------------------
observe({
  
  req(user_con$scope, user_con$logged)
  
  # Connect to SQLite instead of PostgreSQL
  db_conn <<- DBI::dbConnect(RSQLite::SQLite(), "/home/shiny/work/forestcast/treenet.sqlite")
  scope <- user_con$scope

  # Replace tbl_tn with DBI::dbReadTable or dplyr::tbl for SQLite
  info.df <<- dplyr::tbl(db_conn, 'view_metadata') %>%
    dplyr::collect() %>% 
    dplyr::filter(., rlang::eval_tidy(rlang::parse_expr(scope)))

  import.df <<- dplyr::tbl(db_conn, 'import_log') %>%
    dplyr::collect()
  DBI::dbDisconnect(db_conn)

  tmp.choices <- unique(info.df$tree_genus_species) %>% sort()
  shinyWidgets ::updatePickerInput(session, inputId = "tree_genus_species", choices = tmp.choices[!tmp.choices %in% "-999 -999"]) # [!tmp.choices %in% "NA NA"]
  tmp.choices <- unique(info.df$site_name) %>% sort()
  shinyWidgets ::updatePickerInput(session, inputId = "site_name", choices = tmp.choices) 

}) 

# 2.  Reactive filtering of variables -------------------------------------
table_out_rows_debounce <- reactive(input$table_out_rows_selected) %>% debounce(2000)
dataqual_id_debounce    <- reactive(input$dataqual_id) %>% debounce(2000)

# DATA QUALITY
dataqual.tbl <- reactive({ 
  req(user_con$logged, dataqual_id_debounce())
  if (dataqual_id_debounce() == "LM") {
    table.name <- "data_dendro_lm"
  } else if (dataqual_id_debounce() == "L2") {
    table.name <- "data_dendro_l2"
  } else if (dataqual_id_debounce() == "L1") {
    table.name <- "data_all_l1"
  }
    # info.df
    import.df %>%
      dplyr::filter(table_name == table.name) %>% 
      dplyr::collect() %>% 
      dplyr::mutate(db_start = import_from,
                    db_stop  = import_until) %>% 
      dplyr::left_join(., info.df, by = "series_id") #dplyr::select(info.df, -c(series_start,series_stop)), by = "series_id")

})

available.tbl <- reactive({
  req(dataqual.tbl())

  filter(dataqual.tbl(), 
           tree_genus_species %in% input$tree_genus_species,
           site_name %in% input$site_name)
  })

# AVAILABLE SPECIES & SITEs
# observe({
#   req(dataqual.tbl())
#   df <- dataqual.tbl()
  
#   # Get current selections
#   selected_species <- input$tree_genus_species
#   selected_site    <- input$site_name

#   # Start with all choices
#   species_choices <- sort(unique(df$tree_genus_species))
#   site_choices    <- unique(df$site_name)

#   # Filter choices based on the other input
#   if (!is.null(selected_site) && length(selected_site) > 0) {
#     species_choices <- sort(unique(df$tree_genus_species[df$site_name %in% selected_site]))
#   }
#   if (!is.null(selected_species) && length(selected_species) > 0) {
#     site_choices <- sort(unique(df$site_name[df$tree_genus_species %in% selected_species]))
#   }

#   # Update pickers (use shinyWidgets::updatePickerInput if you use pickerInput)
#   shinyWidgets::updatePickerInput(session, "tree_genus_species", choices = species_choices, selected = selected_species)
#   shinyWidgets::updatePickerInput(session, "site_name",     choices = site_choices,    selected = selected_site)
# })

observe({
  req(dataqual.tbl())
  df <- dataqual.tbl()
  
  # All possible choices
  all_species <- sort(unique(df$tree_genus_species))
  all_sites   <- sort(unique(df$site_name))
  
  # Get current selections
  selected_species <- input$tree_genus_species
  selected_site    <- input$site_name

  # Which species are enabled given selected site(s)?
  enabled_species <- all_species
  if (!is.null(selected_site) && length(selected_site) > 0) {
    enabled_species <- sort(unique(df$tree_genus_species[df$site_name %in% selected_site]))
  }
  species_disabled <- !(all_species %in% enabled_species)

  # Which sites are enabled given selected species?
  enabled_sites <- all_sites
  if (!is.null(selected_species) && length(selected_species) > 0) {
    enabled_sites <- sort(unique(df$site_name[df$tree_genus_species %in% selected_species]))
  }
  sites_disabled <- !(all_sites %in% enabled_sites)

  # Update pickers with disabled options
  shinyWidgets::updatePickerInput(
    session, "tree_genus_species",
    choices = all_species,
    selected = selected_species,
    choicesOpt = list(disabled = species_disabled)
  )
  shinyWidgets::updatePickerInput(
    session, "site_name",
    choices = all_sites,
    selected = selected_site,
    choicesOpt = list(disabled = sites_disabled)
  )
})


# FILTERED
filtered.tbl <- reactive({ 
  req(input$daterange_id)
  # if (is.null(input$daterange_id)) {
  # filter(species.tbl(), 
  #        site_name %in% input$site_name %>% 
  #   dplyr::arrange(measure_point) 
  # } else {
  filter(available.tbl(), 
         db_start <= lubridate::ymd_hm(input$daterange_id[2], truncated = 2),
         # is.na(series_stop) | 
         db_stop >= lubridate::ymd_hm(input$daterange_id[1], truncated = 2)) %>% #CLEANED 1997-2023: 342(db) 573(series)     RAW 1997-2024: 612 (db/series) 
    dplyr::arrange(measure_point) 
  # }
}) #%>% 
# debounce(400)


# DATE RANGE
dates <- reactiveValues(
  start = lubridate::ymd("2020-12-01"),
  end   = lubridate::ymd("2020-12-31")
)

observeEvent(input$daterange_id, {
  req(input$daterange_id[[1]],input$daterange_id[[2]])
  start <- lubridate::ymd(input$daterange_id[[1]])
  end   <- lubridate::ymd(input$daterange_id[[2]])
  if (start >= end){
    showNotification("The start date cannot be after the end date!", type = "error")
    updateDateRangeInput(
      session = session, 
      inputId =  "daterange_id", 
      start   = dates$start,
      end     = dates$end
    )
  } else {
    dates$start <- input$daterange_id[[1]]
    dates$end   <- input$daterange_id[[2]]
  }
}, ignoreInit = TRUE)

observe({
  req(input$dataqual_id)
  if (input$dataqual_id == "LM") {
    date.max     <- lubridate::floor_date(Sys.Date(), "years")-1
    date.start   <- lubridate::ceiling_date(Sys.Date(), "years")-lubridate::years(2)
    date.end     <- date.start+30
  } else {
    date.max     <- Sys.Date()
    date.start   <- date.max-30
    date.end     <- date.max
  }
  updateDateRangeInput(
    session = session,
    inputId = "daterange_id",
    start   = date.start,
    end     = date.end,
    max     = date.max
  )
  
  # if( diff( c(input$daterange_id[1], input$daterange_id[2]), unit = 'days') > 180 ) {
  #   updateDateRangeInput(session, inputId = "daterange_id",
  #                        start = as.POSIXct(input$daterange_id[2]) - as.difftime(180, unit="days"),
  #                        end = input$daterange_id[2],
  #                        min = "2001-01-01",  max = Sys.Date()+1)
  # 
  #   showNotification("The date range is too large. Maximum 180 days are allowed!", type = 'error')
  # }
})



# 3. A data table visualization -------------------------------------------
displayed.tbl <- eventReactive(filtered.tbl(),{
  filtered.tbl() %>%
    dplyr::select(series_id, measure_point, sensor_class, series_start, series_stop, site_xcor, site_ycor, site_altitude) %>% 
    dplyr::distinct()
})

output$table_out <- DT::renderDataTable({
  # req(input$tree_genus_species, input$site_name, input$daterange_id)
  toDT(displayed.tbl()) 
})  

# 4. Get the data ---------------------------------------------------------
observeEvent(input$send, {
  
  info.sel <- filtered.tbl()[input$table_out_rows_selected, ] %>% 
    # dplyr::select(series_id, measure_point, sensor_name, site_name, sensor_class) %>% 
    as.data.frame()
  

  job.id <- uuid::UUIDgenerate()
  params_file <- paste0("~/treenet/treenetportal/exports/params_", job.id, ".rds")
  # log_out     <- paste0("exports/out_", job.id, ".log")
  # log_err     <- paste0("exports/err_", job.id, ".log")
  
  params <- list(
    recipient = input$recipient,
    dataqual  = input$dataqual_id,
    daterange = input$daterange_id,
    metadata  = info.sel,
    jobid     = job.id
  )
  saveRDS(params, params_file)

  
  system2(
    command = "nohup",
    args = c("Rscript", "scripts/filesender.R", params_file),
    stdout = F,
    stderr = F,
    wait = FALSE
  )
})

# 5. View the data ---------------------------------------------------
# observe({
#   shinyjs::disable("view")
#   updateActionButton("view", session=session, "6. Select available data", icon = icon("chart-line"))
#   req(displayed.tbl(), input$table_out_rows_selected)
#   shinyjs::enable("view")
#   updateActionButton("view", session=session, "6. View selected data", icon = icon("chart-line"))
# })
# 
# output$plot_out <- renderUI({
#   req(data.tbl(), input$send)#, table_out_rows_debounce())
#   if (is.data.frame(data.tbl())) {
#     withProgress(message = 'Displaying the data', value = 0.8, {
#       # print(data.tbl())
#       data.df <- data.tbl() %>%
#         dplyr::select(ts, series, value) %>%
#         tidyr::pivot_wider(names_from = series, values_from = value)  %>%
#         dplyr::bind_rows(dplyr::tibble(ts = as.POSIXct(isolate(input$daterange_id))))
#       
#       data.df <- xts::xts(x = dplyr::select(data.df, -ts),
#                           order.by = dplyr::select(data.df, ts) %>% dplyr::pull(ts))
#       
#       dygraph_group <- dplyr::distinct(data.tbl(), series, sensor_class)
#       # print(dygraph_group)
#       dygraph_group <- setNames(dygraph_group$series, dygraph_group$sensor_class)
#       
#       # output results
#       res_vis <- list()
#       
#       # Plot the results
#       i <- 1
#       for (n in unique(names(dygraph_group))){
#         
#         vis_variables <- dygraph_group[names(dygraph_group) %in% n]
#         data_dygraph <- data.df[, colnames(data.df) %in% vis_variables]
#         
#         if( !all(is.na(data_dygraph)) ){
#           res_vis[[i]] <-  dygraphs::dygraph(data_dygraph, main = n, group = 'main_stuck_plot', width = "100%", height = "20vh") %>%
#             dygraphs::dyLegend(show = "follow", labelsSeparateLines = TRUE, width = 550) %>%
#             dygraphs::dyOptions(connectSeparatedPoints = TRUE)
#           i = i + 1
#         }
#         
#       }
#       res_vis <- htmltools::tagList(res_vis)
#       
#       res_vis
#     })
#   }
# })

# 6. Export selected data ---------------------------------------------------
observe({
  shinyjs::disable("export")
  # updateActionButton("export", session=session, "6. Select available data", icon = icon("download"))
  req(displayed.tbl(), input$table_out_rows_selected)
  shinyjs::enable("export")
  # updateActionButton("export", session=session, "6. Export selected data", icon = icon("download"))
})

iv <- InputValidator$new()
iv$add_rule("recipient", sv_required())
iv$add_rule("recipient", sv_email())
iv$enable()

observe({
  if (iv$is_valid()) {
    shinyjs::enable("send")
  } else {
    shinyjs::disable("send")
  }
})
  
observeEvent(input$export, {
  showModal(
    modalDialog(
      title = "Export Selected Data",
      "Please enter an email address to receive the data download link.",
      textInput( inputId = "recipient", label = "Recipient:", width = "100%", value = user_con$username, placeholder = "Your email address"),
      easyClose = F,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("send", "Send")
      )
    ))
})
observeEvent(input$send, {
  req(input$recipient)
  # req(data.tbl(),input$recipient)
  # filesender(data.tbl() %>% dplyr::select(series_id=series, measure_point, ts, value),
  #            input$recipient)
    removeModal()
    showModal(
      modalDialog(
      title = "Export Request Received",
      paste0("Please check the inbox of ",input$recipient,"."),
      br(),
      "Once the export is complete, you will receive an email from filesender@switch.ch with a download link for the timeseries data and the corresponding request metadata.",
      br(),
      "You may now close this message and logout of the data portal.",
      easyClose = F,
      footer = modalButton("Close"),
    ))
})

# observe({
#   shinyjs::disable("export_out")
#   req(data.tbl(), table_out_rows_debounce())
#   shinyjs::enable("export_out")
# })
# 
# output$export_out <- downloadHandler(
#   filename = function() {paste0("tn_", lubridate::now(), ".csv")},
#   content = function(file) {
#     withProgress(message = 'Exporting the data', value = 0.5, {
#       
#       # if (!is.null(input$plot_out_date_window)) {
#       #   export.tbl <- export.tbl %>%
#       #     filter(ts <= lubridate::ymd_hm(input$plot_out_date_window[[2]], truncated = 2),
#       #            ts >= lubridate::ymd_hm(input$plot_out_date_window[[1]], truncated = 2)) %>%
#       #     dplyr::select(-site_name, -sensor_class)
#       # } else {
#       # }
#       
#       write.csv(data.tbl() %>% dplyr::select(series_id=series, measure_point, ts, value), file, row.names = FALSE)
#     })
#   })

# 7. Keep connection open ---------------------------------------------------
output$keepAlive <- renderText({
  req(input$count)
  paste("keep alive ", input$count)
})



