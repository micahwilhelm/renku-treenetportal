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
			width = 3,

			radioButtons(
				"dataqual",
				choices = tn_levels_available(),   # Renku: only the levels present in the Parquet export
				selected = if ("LM" %in% tn_levels_available()) "LM" else tn_levels_available()[1],
				inline = T,
				label = shinytip::tip(
					strong("Data quality", icon("question-circle")),
					paste(
						"L0 - Raw data",
						"  Unprocessed measurements",
						"  Tree & environmental data",
						"  Up to today",
						"",
						"L1 - Time-aligned",
						"  Interpolated to common timestamps",
						"	 Tree & environmental data",
						"  Up to today",
						"",
						"L2 - Auto-cleaned",
						"  Automated outlier & jump removal",
						"  Tree data only",
						"  Up to today",
						"",
						"L3 - Gap-filled",
						"  Cleaned & gap-filled by neural networks",
						"  Stem radius change & environmental data",
						"  2000 to 2025",
						"",
						"LM - Manually cleaned",
						"  Expert-reviewed & corrected.",
						"  Tree data only",
						"  Through end of last year",

						sep = "\n"
					),
					position = "bottom-left",
					length = getOption("shinytip.length", "xl"),
					bg = getOption("shinytip.bg", "black"),
					fg = getOption("shinytip.fg", "white"),
					size = getOption("shinytip.size", "12px"),
					click = getOption("shinytip.click", FALSE),
					animate = getOption("shinytip.animate", TRUE),
					pointer = getOption("shinytip.pointer", TRUE),
					`data-balloon-break` = NA
				)
			),

			shinyWidgets::virtualSelectInput(
				inputId = "variable_name",
				label = "Variable",
				choices = list(
					"Tree variables" = c(
						"tree stem radius change",
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
						"tree sap flow vhc outer"
					),
					"Environmental variables" = c(
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
				),
				multiple = T,
				search = T,
				searchGroup = T,
				updateOn = "close",
				width = "100%",
				zIndex = 100,
				disableOptionGroupCheckbox = F
				# showValueAsTags = F
			),

			shinyWidgets::virtualSelectInput(
				inputId = "tree_genus_species",
				label = "Species",
				choices = NULL,
				selected = NULL,
				multiple = T,
				search = T,
				searchGroup = T,
				updateOn = "close",
				width = "100%",
				zIndex = 100
			),

			shinyWidgets::virtualSelectInput(
				inputId = "site_name",
				label = "Site",
				choices = NULL,
				selected = NULL,
				multiple = T,
				search = T,
				searchGroup = T,
				updateOn = "close",
				width = "100%",
				zIndex = 100
			),

			# selectInput("variable_id", "Variable:", choices = NULL, multiple = TRUE, selected = NULL),
			shinyWidgets::virtualSelectInput(
				inputId = "timezone",
				label = shinytip::tip(
					strong("Time zone", icon("question-circle")),
					paste(
						"All dates in the app and in the exported files use this time zone:",
						"  the date range (whole days in this zone),",
						"  the first and last date in the table,",
						"  the timestamps and dates in the exported files.",
						"Stored data is UTC. Etc/GMT-1 is UTC+1 all year (no daylight saving).",
						sep = "\n"
					),
					position = "bottom-left",
					length = getOption("shinytip.length", "xl"),
					bg = getOption("shinytip.bg", "black"),
					fg = getOption("shinytip.fg", "white"),
					size = getOption("shinytip.size", "12px"),
					click = getOption("shinytip.click", FALSE),
					animate = getOption("shinytip.animate", TRUE),
					pointer = getOption("shinytip.pointer", TRUE),
					`data-balloon-break` = NA
				),
				choices = OlsonNames(),
				selected = tn_default_tz,
				multiple = FALSE,
				search = TRUE,
				width = "100%",
				zIndex = 100
			),

			dateRangeInput(
				"daterange",
				"Date range",
				start = Sys.Date() - 30,
				end = Sys.Date(),
				min = "1997-01-01",
				max = Sys.Date()
			),
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
			width = 9,
			div(
				style = "display: flex; align-items: center; gap: 10px; margin-bottom: 6px;",
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
					pointer = getOption("shinytip.pointer", TRUE)
				),
				# Toggles between "Select all" and "Deselect all"; shown only when the table has rows
				shinyjs::hidden(
					actionButton("select_all_rows", "Select all", icon = icon("check-double"), class = "btn-sm")
				)
			),
			DT::dataTableOutput("table_out"),
			# actionButton("view", "6. Select data in table", icon = icon("chart-line")),
			# downloadButton("export_out", "7. Export displayed data", icon = icon("download")),
			uiOutput("plot_out"),
			textOutput("keepAlive") #Keep connection open
		),
	)
})