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
timeseries <- extract_sqlite(
    data_format = dataqual,
    series_id   = metadata$series_id,
    from        = daterange[1],
    to          = daterange[2],
    tz          = timezone,
    db_path     = "/home/shiny/work/forestcast/treenet.sqlite"
) 

if (is.null(timeseries)) {
	message <- "Sorry. There are no data available from the specified series and period."
	writeLines(message, con = logfile)
} else {
	# metadata <- metadata %>% 
	# 	mutate(data_format = dataqual, extracted_from = daterange[1], extracted_until = daterange[2], timezone = timezone) %>% 
	# 	dplyr::select(measure_point, current_sensor=sensor_name, sensor_class, series_start, series_stop, site_xcor, site_ycor, site_altitude)
	metadata <- metadata %>% 
		rename(current_sensor = sensor_name) %>% 
		mutate(data_format = dataqual, extracted_from = daterange[1], extracted_until = daterange[2], timezone = timezone) %>% 
		select(-c(table_name,import_from,import_until,import_nrows,import_date,db_start,db_stop,sensor_data_source)) %>% #tree_name,tree_xcor,tree_ycor,tree_altitude,genus_species
		relocate(series_id, data_format, extracted_from, extracted_until, timezone)
	
	write.csv(
		timeseries %>% rename(series_id = series),
		datafile,
		row.names = F
	)
	write.csv(
		metadata,
		metafile,
		row.names = F
	)
	
	# Send file via Python script (filesender.py)
	exportfiles <- paste(shQuote(c(datafile,metafile)), collapse = " ")
	command <- sprintf(
		"nohup python3 scripts/filesender.py %s -r %s >> %s 2>&1",
		exportfiles, recipient, logfile
	)
	system(command, wait = T)

	# Remove logfile if empty (no errors/output)
	if (file.exists(logfile) && file.info(logfile)$size == 0) {
		file.remove(logfile)
	}
	
	# Clean up after sending
	if (file.exists(datafile)) {
		file.remove(datafile)
	}
	if (file.exists(metafile)) {
		file.remove(metafile)
	}
	if (file.exists(paramsfile)) {
		file.remove(paramsfile)
	}
}
