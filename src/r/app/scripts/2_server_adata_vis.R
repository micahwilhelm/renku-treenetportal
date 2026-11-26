#' Server: data selection and export (cleaned & reorganized)
#'
#' Responsibilities:
#'  - load metadata (info.df) once user logged in
#'  - expose reactive tables: dataqual.tbl, available.tbl, window_rows, filtered.tbl
#'  - update VirtualSelect inputs (tree_genus_species, site_name, variable_name)
#'  - export selected data via filesender.R

# Per-session state. This file is sourced with local = TRUE, so these live in the session's server
# environment; the `<<-` assignments below update them there instead of creating process-wide globals that
# other users' sessions would overwrite.
db_conn <- NULL
info.df <- NULL

# Debounced dataqual and choice trackers (single definitions)
dataqual_debounce <- reactive(input$dataqual) %>% debounce(500)
# The date range fires on every edit while typing / picking dates; wait until it settles, so that the
# data-availability check (window_rows) and the table only run once per period.
daterange_debounce <- reactive(input$daterange) %>% debounce(800)
prev_species <- reactiveVal(character())
prev_sites <- reactiveVal(character())
job_id <- reactiveVal(NULL)

# 1. Load configuration / metadata and set initial choices ------------------
observe({
  req(user_con$scope, user_con$logged)

  # populate DB connection and metadata
  db_conn <<- user_con$cc
  scope <- user_con$scope
  info.df <<- tbl_tn(db_conn, 'public', 'view_metadata') %>%
    dplyr::collect() %>%
    dplyr::filter(., rlang::eval_tidy(rlang::parse_expr(scope)))

  # compute species choices with meteorological code allowed only for L0/L1
  tmp_species <- sort(unique(info.df$tree_genus_species))
  species_choices <- setdiff(tmp_species, "-999 -999")

  # update species select only when changed
  if (!identical(species_choices, prev_species())) {
    prev_species(species_choices)
    freezeReactiveValue(input, "tree_genus_species")
    shinyWidgets::updateVirtualSelect(
      session = session,
      inputId = "tree_genus_species",
      choices = species_choices,
      selected = intersect(isolate(input$tree_genus_species), species_choices)
    )
  }

  # sites: update only when changed
  tmp_sites <- sort(unique(info.df$site_name))
  if (!identical(tmp_sites, prev_sites())) {
    prev_sites(tmp_sites)
    freezeReactiveValue(input, "site_name")
    shinyWidgets::updateVirtualSelect(
      session = session,
      inputId = "site_name",
      choices = tmp_sites,
      selected = intersect(isolate(input$site_name), tmp_sites)
    )
  }
})

# 2. Variable visibility toggling by dataqual -------------------------------
observeEvent(
  dataqual_debounce(),
  {
    if (dataqual_debounce() %in% c("L0", "L1")) {
      shinyWidgets::updateVirtualSelect(
        inputId = "variable_name",
        session = session,
        disabledChoices = character(0)
      )
    } else if (dataqual_debounce() == "L3") {
      shinyWidgets::updateVirtualSelect(
        inputId = "variable_name",
        session = session,
        disabledChoices = c(
					"tree branch radius change",
					"tree root radius change",
					"tree xylem radius change",
					"tree reference radius change",
					"tree stem carbon dioxide",
					"tree stem carbon dioxide soil",
					"tree stem temperature",
					"tree sap flow",
					"tree sap flow alpha inner",
					"tree sap flow alpha outer",
					"tree sap flow beta inner",
					"tree sap flow beta outer",
					"tree sap flow heat velocity inner",
					"tree sap flow heat velocity outer",
					"tree sap flow pulse energy",
					"tree sap flow pulse time",
					"tree sap flow swc inner",
					"tree sap flow swc outer",
					"tree sap flow temp outer",
					"tree sap flow tmax inner",
					"tree sap flow tmax max inner upstream",
					"tree sap flow tmax outer",
					"tree sap flow tmax outer upstream",
					"tree sap flow vhc inner",
					"tree sap flow vhc outer",
          "air pressure",
          "net solar radiation",
          "ozone concentration",
          "photosynthetically active radiation",
          "precipitation",
          "precipitation interval",
          "wind direction",
          "wind speed",
          "soil dielectric permittivity",
          "soil electrical conductivity",
          "soil volumetric water content"
        )
      )
    } else {
      shinyWidgets::updateVirtualSelect(
        inputId = "variable_name",
        session = session,
        disabledChoices = c(
          "air pressure",
          "air temperature",
          "net solar radiation",
          "ozone concentration",
          "photosynthetically active radiation",
          "precipitation",
          "precipitation interval",
          "relative humidity",
          "wind direction",
          "wind speed",
          "soil dielectric permittivity",
          "soil electrical conductivity",
          "soil temperature",
          "soil volumetric water content",
          "soil water potential"
        )
      )
    }
  },
  ignoreInit = TRUE
)

# 3. DATA QUALITY table (select correct import_log table/coverage) ----------
# view_coverage and import_log are downloaded once per session and level and kept here (this file is
# sourced with local = TRUE, so the cache belongs to the session). Switching back to a level that was
# already opened is instant; the data are refreshed when the user logs in again / reloads the page.
dataqual_cache <- new.env(parent = emptyenv())
coverage_cache <- new.env(parent = emptyenv())

dataqual.tbl <- reactive({
  req(user_con$logged, dataqual_debounce())
  dq <- dataqual_debounce()
  if (!is.null(dataqual_cache[[dq]])) return(dataqual_cache[[dq]])

  # Dendrometer series are processed into separate "_dendro_" tables (and
  # tracked by separate view_coverage columns) for L2/L3/LM; all other series
  # use the "_all_"/plain ones. L0/L1 raw data isn't split by sensor_class,
  # and LM only exists for dendrometers.
  table_all    <- switch(dq,
    L0 = "data_all_l0", L1 = "data_all_l1",
    L2 = "data_all_l2", L3 = "data_all_l3",
    LM = NA_character_,
    stop("unsupported dataqual")
  )
  table_dendro <- switch(dq,
    L0 = "data_all_l0",    L1 = "data_all_l1",
    L2 = "data_dendro_l2", L3 = "data_dendro_l3",
    LM = "data_dendro_lm"
  )
  coverage_all    <- switch(dq,
    L0 = "coverage_l0", L1 = "coverage_l1",
    L2 = "coverage_l2", L3 = "coverage_l3",
    LM = NA_character_
  )
  coverage_dendro <- switch(dq,
    L0 = "coverage_l0",        L1 = "coverage_l1",
    L2 = "coverage_dendro_l2", L3 = "coverage_dendro_l3",
    LM = "coverage_dendro_lm"
  )

  # only keep series that actually have data at this quality level, per
  # view_coverage, instead of assuming every series in view_metadata does.
  # view_coverage also has its own series_start/series_stop columns; drop
  # them here so they don't collide with view_metadata's below.
  # the same table serves all levels, so it is downloaded only once
  if (is.null(coverage_cache$long)) {
    coverage_cache$long <- tbl_tn(db_conn, 'public', 'view_coverage') %>%
      dplyr::collect() %>%
      dplyr::select(series_id, dplyr::starts_with("coverage_")) %>%
      tidyr::pivot_longer(
        dplyr::starts_with("coverage_"),
        names_to  = "coverage_col",
        values_to = "coverage_pct"
      )
  }
  covg_long <- coverage_cache$long

  info.lvl <- info.df %>%
    dplyr::mutate(
      is_dendro    = sensor_class == "Dendrometer",
      table_name   = dplyr::if_else(is_dendro, table_dendro, table_all),
      coverage_col = dplyr::if_else(is_dendro, coverage_dendro, coverage_all)
    ) %>%
    dplyr::filter(!is.na(table_name)) %>%
    dplyr::left_join(covg_long, by = c("series_id", "coverage_col")) %>%
    dplyr::filter(!is.na(coverage_pct), coverage_pct > 0) %>%
    dplyr::select(-is_dendro, -coverage_col, -coverage_pct)

  table_names <- unique(stats::na.omit(c(table_all, table_dendro)))

  out <- tbl_tn(db_conn, 'public', 'import_log') %>%
    dplyr::filter(table_name %in% table_names) %>%
    dplyr::collect() %>%
    dplyr::mutate(db_start = first_date, db_stop = last_date) %>%
    dplyr::inner_join(info.lvl, by = c("series_id", "table_name"))

  dataqual_cache[[dq]] <- out
  out
})

observe({
  req(dataqual_debounce())
  # "today" is the date in the selected time zone; isolate: changing the zone must not reset the user's date range
  today <- lubridate::today(tzone = tn_valid_tz(isolate(input$timezone)))
  if (dataqual_debounce() == "LM" | dataqual_debounce() == "L3") {
    date.max <- lubridate::floor_date(today, "years") - 1
    date.start <- lubridate::ceiling_date(today, "years") -
      lubridate::years(2)
    date.end <- date.start + 30
  } else {
    date.max <- today
    date.start <- date.max - 30
    date.end <- date.max
  }
  updateDateRangeInput(
    session = session,
    inputId = "daterange",
    start = date.start,
    end = date.end,
    max = date.max
  )
})

# 4. Available table (apply user selections safely) -------------------------
available.tbl <- reactive({
  req(dataqual.tbl())
  df <- dataqual.tbl()

  # allowed visible species (never show meteorological)
  allowed_visible_species <- setdiff(
    sort(unique(df$tree_genus_species)),
    "-999 -999"
  )

  sel_species <- if (
    !is.null(input$tree_genus_species) && length(input$tree_genus_species) > 0
  ) {
    intersect(input$tree_genus_species, allowed_visible_species)
  } else {
    allowed_visible_species
  }

  sel_site <- if (!is.null(input$site_name) && length(input$site_name) > 0) {
    intersect(input$site_name, sort(unique(df$site_name)))
  } else {
    sort(unique(df$site_name))
  }

  sel_variable <- if (
    !is.null(input$variable_name) && length(input$variable_name) > 0
  ) {
    intersect(input$variable_name, sort(unique(df$variable_name)))
  } else {
    sort(unique(df$variable_name))
  }

  df %>%
    dplyr::filter(
      # include rows matching the visible selection OR meteorological code when L0/L1
      (tree_genus_species %in% sel_species) |
        (dataqual_debounce() %in%
          c("L0", "L1", "L3") &
          tree_genus_species == "-999 -999"),
      site_name %in% sel_site,
      variable_name %in% sel_variable
    )
})

# 5. Keep VirtualSelect choices in sync (idempotent updates) -----------------
observe({
  req(dataqual.tbl())
  df <- dataqual.tbl()

  # compute possible choices depending on current dataqual
  all_species <- sort(unique(df$tree_genus_species))
  all_sites <- sort(unique(df$site_name))

  new_species <- setdiff(sort(unique(df$tree_genus_species)), "-999 -999")
  new_sites <- all_sites

  # preserve intersection with current selection when computing 'selected'
  current_species_sel <- intersect(
    isolate(input$tree_genus_species),
    new_species
  )
  current_site_sel <- intersect(isolate(input$site_name), new_sites)

  if (!identical(new_species, prev_species())) {
    prev_species(new_species)
    freezeReactiveValue(input, "tree_genus_species")
    shinyWidgets::updateVirtualSelect(
      session = session,
      inputId = "tree_genus_species",
      choices = new_species,
      selected = current_species_sel
    )
  }

  if (!identical(new_sites, prev_sites())) {
    prev_sites(new_sites)
    freezeReactiveValue(input, "site_name")
    shinyWidgets::updateVirtualSelect(
      session = session,
      inputId = "site_name",
      choices = new_sites,
      selected = current_site_sel
    )
  }
})

# 6. Filtered table by date range ------------------------------------------
# import_log only stores one [min, max] range per series (and view_coverage is derived
# from it), so it can list series whose table holds no rows in the selected period; a
# download of those fails with "no data available". For the levels below the listing is
# therefore confirmed against the data tables themselves (tn_series_with_rows(); it first
# probes only the edges of each series' range, so long periods stay fast).
exact_check_levels <- c("L0", "L1", "L2", "L3", "LM")

# The time zone selected in the dropdown (falls back to the default before the UI exists).
selected_tz <- reactive(tn_valid_tz(input$timezone))

# TRUE where the [db_start, db_stop] range of a series overlaps the selected period. The period runs
# from 00:00 of the first day to the END of the last day in time zone tz (tn_period_window()), the same
# window the export cuts and tn_series_with_rows() probes; comparing against 00:00 of the last day would
# drop series whose data starts later on that day.
overlaps_period <- function(db_start, db_stop, daterange, tz) {
  win <- tn_period_window(daterange[1], daterange[2], tz)
  db_start < win$hi & db_stop >= win$lo
}

# (series_id, table_name) pairs that really have rows in the selected period. Depends only
# on data quality and date range, so it runs once per period and changing the species /
# site / variable selection stays instant. NULL = not checked (level skipped or check failed).
# Series the check could not finish in time are kept (they may have data).
window_rows <- reactive({
  req(daterange_debounce(), dataqual.tbl())
  if (!dataqual_debounce() %in% exact_check_levels) return(NULL)

  tz <- selected_tz()
  cand <- dataqual.tbl() %>%
    dplyr::filter(overlaps_period(db_start, db_stop, daterange_debounce(), tz)) %>%
    dplyr::group_by(series_id, table_name) %>%
    dplyr::summarise(db_start = min(db_start), db_stop = max(db_stop), .groups = "drop")

  ok <- tryCatch(
    tn_series_with_rows(db_conn, cand, daterange_debounce()[1], daterange_debounce()[2], tz),
    error = function(e) {
      message(Sys.time(), " window_rows check failed: ", conditionMessage(e))
      showNotification(
        "Could not verify data availability for this period. Some listed series may have no data.",
        type = "warning"
      )
      NULL
    }
  )
  if (is.null(ok)) return(NULL)
  if (anyNA(ok)) {
    showNotification(
      "Data availability could not be fully verified for this period. Some listed series may have no data.",
      type = "warning"
    )
  }
  cand[is.na(ok) | ok, c("series_id", "table_name"), drop = FALSE]
})

filtered.tbl <- reactive({
  req(daterange_debounce())
  out <- available.tbl() %>%
    dplyr::filter(overlaps_period(db_start, db_stop, daterange_debounce(), selected_tz()))

  rows <- window_rows()
  if (!is.null(rows)) {
    out <- dplyr::semi_join(out, rows, by = c("series_id", "table_name"))
  }

  dplyr::arrange(out, measure_point)
})

# 7. Data table output -----------------------------------------------------
displayed.tbl <- eventReactive(list(filtered.tbl(), selected_tz()), {
  tz <- selected_tz()
  filtered.tbl() %>%
    dplyr::transmute(
      series_id,
      # measure_point,
      variable_name,
      tree_genus_species,
      site_name,
      # first and last row of this series in the selected data quality level (db_start / db_stop, from
      # import_log), i.e. what can be downloaded; not the sensor's installation period from view_metadata.
      # Shown in the selected time zone.
      first_date = format(db_start, "%Y-%m-%d %H:%M", tz = tz),
      last_date  = format(db_stop,  "%Y-%m-%d %H:%M", tz = tz)
    ) %>%
    dplyr::distinct()
})

output$table_out <- DT::renderDataTable({
  tryCatch(
    toDT(displayed.tbl()),
    error = function(e){
      msg <- paste(Sys.time(), "renderDataTable error:", conditionMessage(e), sep = " ")
      writeLines(msg, con = "shiny-error.log", sep = "\n", useBytes = TRUE)
      stop(e)  # preserves original behavior so client still sees error response
    }
  )
})

# Select / deselect all toggle -------------------------------------------
# Acts on the rows currently shown in the table (respects the table's search filter).
table_has_rows <- reactive({
  n <- tryCatch(nrow(displayed.tbl()), error = function(e) 0L)
  isTRUE(n > 0) && length(input$table_out_rows_all) > 0
})

all_rows_selected <- reactive({
  table_has_rows() && all(input$table_out_rows_all %in% input$table_out_rows_selected)
})

observe({
  shinyjs::toggle("select_all_rows", condition = table_has_rows())
  if (all_rows_selected()) {
    updateActionButton(session, "select_all_rows", label = "Deselect all", icon = icon("square"))
  } else {
    updateActionButton(session, "select_all_rows", label = "Select all", icon = icon("check-double"))
  }
})

observeEvent(input$select_all_rows, {
  shown    <- input$table_out_rows_all
  selected <- input$table_out_rows_selected
  rows <- if (all_rows_selected()) setdiff(selected, shown) else union(selected, shown)
  DT::selectRows(DT::dataTableProxy("table_out"), if (length(rows)) rows else NULL)
})

# 8. Export handler --------------------------------------------------------
observeEvent(input$send, {
    waiter::waiter_show(
      html = tagList(
        spin_fading_circles(),
        "Submitting export request..."
      )
    )
  info.sel <- filtered.tbl()[input$table_out_rows_selected, , drop = FALSE] %>%
    as.data.frame()

  job_id(uuid::UUIDgenerate())
  params_file <- file.path("exports", paste0("params_", job_id(), ".rds"))

  params <- list(
    recipient = input$recipient,
    dataqual = input$dataqual,
    daterange = input$daterange,
    timezone = selected_tz(),
    metadata = info.sel,
    jobid = job_id()
  )

  saveRDS(params, params_file)

  system2(
    command = "nohup",
    args = c("Rscript", "scripts/filesender.R", params_file),
    stdout = TRUE,
    stderr = TRUE,
    wait = FALSE
  )
})

observeEvent(input$send, {
  req(input$recipient)
  waiter::waiter_hide()
  removeModal()

  # Path to the logfile (must match filesender.R logic)
  # retrieve the job.id used for this export
  logfile <- file.path("exports", paste0(job_id(), ".log"))
  error_msg <- NULL

  if (file.exists(logfile) && file.info(logfile)$size > 0) {
    error_msg <- paste(readLines(logfile), collapse = "<br>")
  }

  showModal(modalDialog(
    title = "Export Request Received",
    if (!is.null(error_msg)) {
      HTML(paste0("<b>Error:</b><br>", error_msg))
    } else {
      tagList(
        p("Your export request has been received and is being processed."),
        br(),
        HTML(paste0(
          "Once the export completes, an email will be sent to ",
          input$recipient,
          " from <b>filesender@switch.ch</b> containing a download link for the timeseries data and its metadata. Links expire after 20 days."
        ))
      )
    },
    easyClose = FALSE,
    footer = modalButton("Close")
  ))
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
#         dplyr::bind_rows(dplyr::tibble(ts = as.POSIXct(isolate(input$daterange))))
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
  req(displayed.tbl(), input$table_out_rows_selected)
  shinyjs::enable("export")
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
  showModal(modalDialog(
    title = "Export Selected Data",
    textInput(
      inputId = "recipient",
      label = "Recipient:",
      width = "100%",
      value = user_con$username,
      placeholder = "Your email address"
    ),
    easyClose = FALSE,
    footer = tagList(modalButton("Cancel"), actionButton("send", "Send"))
  ))
})


# 10. Keep connection alive (heartbeat) -----------------------------------
output$keepAlive <- renderText({
  req(input$count)
  paste("keep alive ", input$count)
})

# 11. import_log driven updates (idempotent; duplicate-safe) ----------------
observeEvent(dataqual.tbl(), {
  # ensure import_log-driven updates also respect L0/L1 meteorological inclusion
  req(dataqual.tbl())
  df <- dataqual.tbl()

  new_species <- setdiff(sort(unique(df$tree_genus_species)), "-999 -999")
  new_sites <- sort(unique(df$site_name))

  if (!identical(new_species, prev_species())) {
    prev_species(new_species)
    freezeReactiveValue(input, "tree_genus_species")
    shinyWidgets::updateVirtualSelect(
      session = session,
      inputId = "tree_genus_species",
      choices = new_species,
      selected = intersect(isolate(input$tree_genus_species), new_species)
    )
  }

  if (!identical(new_sites, prev_sites())) {
    prev_sites(new_sites)
    freezeReactiveValue(input, "site_name")
    shinyWidgets::updateVirtualSelect(
      session = session,
      inputId = "site_name",
      choices = new_sites,
      selected = intersect(isolate(input$site_name), new_sites)
    )
  }
})

# Ensure choices + selections refresh when dataqual changes
observeEvent(dataqual_debounce(), {
  req(dataqual.tbl())
  df <- dataqual.tbl()

  new_species <- setdiff(sort(unique(df$tree_genus_species)), "-999 -999")
  new_sites <- sort(unique(df$site_name))

  # update stored choices and UI (clear selection so filtering uses available choices)
  prev_species(new_species)
  prev_sites(new_sites)

  freezeReactiveValue(input, "tree_genus_species")
  shinyWidgets::updateVirtualSelect(
    session = session,
    inputId = "tree_genus_species",
    choices = new_species,
    selected = character(0)
  )

  freezeReactiveValue(input, "site_name")
  shinyWidgets::updateVirtualSelect(
    session = session,
    inputId = "site_name",
    choices = new_sites,
    selected = character(0)
  )
}, ignoreInit = TRUE)



