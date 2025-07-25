#!/usr/bin/env Rscript

# Load required libraries
suppressPackageStartupMessages({
	library(dplyr)
	library(lubridate)
})

# Read parameters
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) stop("No parameter file passed.")
paramsfile <- args[1]

if (!file.exists(paramsfile)) stop("File does not exist: ", paramsfile)
params <- readRDS(paramsfile)

# Unpack parameters
recipient <- params$recipient
dataqual  <- params$dataqual
daterange <- params$daterange
metadata  <- params$metadata
jobid     <- params$jobid
timezone  <- "Etc/GMT-1"

# Define timestamped export datafile and logfile
timestamp <- format(now(), "%Y%m%d_%H%M%S")
datafile  <- paste0("/tmp/tn_timeseries_", dataqual, "_", daterange[1],"_", daterange[2],"_",jobid,".csv")
metafile  <- paste0("/tmp/tn_metadata_",   dataqual, "_", daterange[1],"_", daterange[2],"_",jobid,".csv")
logfile   <- paste0("/tmp/", jobid, ".log")

# Extract the data
extract_sqlite <- function(
    data_format = NULL,
    series_id   = NULL,
    from        = NULL,
    to          = NULL,
    tz          = "Etc/GMT-1",
    db_path     = NULL
) {
  
  # Load functions ------------------------------------------------------------
  Sys.setenv(TZ = tz)
  
  # Function to set the time zone in the database connection
  set_db_timezone <- function(con, tz) {
    DBI::dbExecute(con, paste0("SET TIME ZONE '", tz, "'"))
    current_tz <- DBI::dbGetQuery(con, "SHOW timezone")
    if (current_tz != tz) {
      stop("Error setting the database time zone.")
    }
  }
  
  
  # Download series -----------------------------------------------------------
  # specify format
  
  if (data_format == "L0")  db_table  <- "data_all_l0" 
  if (data_format == "L1")  db_table  <- "data_all_l1"
  if (data_format == "L2")  db_table  <- "data_dendro_l2"
  if (data_format == "LM")  db_table  <- "data_dendro_lm"
  
  # format time window
  if (length(from) == 0) {
    from <- "1970-01-01"
  }
  from <- as.POSIXct(as.character(from), format = "%Y-%m-%d", tz = "Etc/GMT-1")
  if (length(to) == 0) {
    to <- lubridate::today() %>% as.character()
  }
  to <- as.POSIXct(as.character(to), format = "%Y-%m-%d", tz = "Etc/GMT-1") +86399
  
  # download series
  # options(warn = -1)
  
  # find unique meta_series take first start and last stop date per series_id
  # Assume all other metadata is identical in other rows
  
  # specify time window
  db_time <- paste0(db_table, ".ts BETWEEN '",
                    format(start, "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-1"),
                    "' AND '",
                    format(stop, "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-1"), "'")
  
  
  con  <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  
  # Set the database time zone to UTC (server default)
  # set_db_timezone(con, "UTC")
  
  query <- paste0("SELECT * FROM ", db_table,
                  " WHERE series_id in (", paste0(series_id, collapse=", "),")",
                  paste0(c("",  db_time), collapse=" AND "), ";")
  
  foo <- sqldf::sqldf(query, connection=con)
  invisible(DBI::dbDisconnect(con))
  
  # df <- foo %>%
  #   dplyr::select_if(!(names(.) %in% "insert_date")) %>%
  #   transform(ts = lubridate::force_tz(ts, tzone = "Etc/GMT-1")) %>%  # Force the timezone to Etc/GMT-1
  #   transform(ts = lubridate::with_tz(ts, tzone = tz)) %>%  # Convert to desired timezone
  # # dplyr::arrange(ts) %>%
  #   # dplyr::distinct() %>%
  #   transform(value = as.numeric(value))
  
  return(foo)
}

# run the benchmark
source("scripts/benchmark_logger.R")
result <- benchmark_and_log({
  timeseries <- extract_sqlite(
    data_format = dataqual,
    series_id   = metadata$series_id,
    from        = daterange[1],
    to          = daterange[2],
    tz          = timezone,
    db_path     = "/home/shiny/work/forestcast/treenet.sqlite"
  ) 
})

if (is.null(timeseries)) {
	message("Sorry. There are no data available from the specified series and period.", con = logfile)
} else {
  message("data extracted")
	# metadata <- metadata %>% 
	# 	mutate(data_format = dataqual, extracted_from = daterange[1], extracted_until = daterange[2], timezone = timezone) %>% 
	# 	dplyr::select(measure_point, current_sensor=sensor_name, sensor_class, series_start, series_stop, site_xcor, site_ycor, site_altitude)
	metadata <- metadata %>% 
		rename(current_sensor = sensor_name) %>% 
		mutate(data_format = dataqual, extracted_from = daterange[1], extracted_until = daterange[2], timezone = timezone) %>% 
		select(-c(table_name,import_from,import_until,import_nrows,import_date,db_start,db_stop,sensor_data_source)) %>% #tree_name,tree_xcor,tree_ycor,tree_altitude,genus_species
		relocate(series_id, data_format, extracted_from, extracted_until, timezone)
	
	data.table::fwrite(
		timeseries,
		datafile,
		row.names = F
	)
	data.table::fwrite(
		metadata,
		metafile,
		row.names = F
	)
	
	# Send file via Python script (filesender.py)
	exportfiles <- paste0("/tmp/tn_download_",jobid,".zip") #paste(shQuote(c(datafile,metafile)), collapse = " ")
	zip::zip(zipfile = exportfiles, files = c(datafile, metafile))
	command <- sprintf(
		"nohup python3 scripts/filesender.py %s -r %s >> %s 2>&1",
		exportfiles, recipient, logfile
	)
	system(command, wait = F)
	message("files sent")

	# # Remove logfile if empty (no errors/output)
	# if (file.exists(logfile) && file.info(logfile)$size == 0) {
	# 	file.remove(logfile)
	# }
	# 
	# # Clean up after sending
	# if (file.exists(datafile)) {
	# 	file.remove(datafile)
	# }
	# if (file.exists(metafile)) {
	# 	file.remove(metafile)
	# }
	if (file.exists(paramsfile)) {
		file.remove(paramsfile)
	}
}
