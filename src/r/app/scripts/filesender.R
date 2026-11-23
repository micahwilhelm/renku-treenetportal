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
source("scripts/tn_timezone.R")
source("scripts/tn_parquet.R")   # Renku: data are read from the Parquet export, not PostgreSQL
timezone  <- tn_valid_tz(params$timezone)   # chosen in the app; the default for parameter files from before the dropdown

# Define timestamped export datafile and logfile
timestamp <- format(now(), "%Y%m%d_%H%M%S")
metafile <- paste0("exports/tn_metadata_",   dataqual, "_", daterange[1],"_", daterange[2],"_",jobid,".csv")
datafile <- paste0("exports/tn_timeseries_", dataqual, "_", daterange[1],"_", daterange[2],"_",jobid,".csv")
logfile  <- paste0("exports/", jobid, ".log")

# Read the data
# The period is whole days in the chosen time zone: from 00:00 of the first day up to 00:00 after the last day.
# Renku: each series is read from the exported table it was listed from (metadata$table_name, from import_log);
# the Parquet export stores true UTC instants, so they only need to be shown in the chosen zone.
win <- tn_period_window(daterange[1], daterange[2], timezone)
dataseries <- lapply(unique(metadata$table_name), function(tn) {
	tn_read_series(tn, metadata$series_id[metadata$table_name == tn], win$lo, win$hi)
}) |>
	dplyr::bind_rows()
if (nrow(dataseries) == 0) {
	dataseries <- NULL   # nothing inside the period: same message as no data at all
} else {
	dataseries <- dataseries |>
		dplyr::mutate(ts = lubridate::with_tz(ts, timezone)) |>
		dplyr::arrange(series_id, ts)
}

if (is.null(dataseries)) {
	message <- "Sorry. There are no data available from the specified series and period."
	writeLines(message, con = logfile)
} else {
  # dataset_id is only present in the L3 and LM tables; capture one value per
  # series_id (dendro and non-dendro alike) before any dendro splitting below,
  # then resolve it to the human-readable dataset_name from the 'dataset' table
  if ("dataset_id" %in% colnames(dataseries)) {
    dataset_id_map <- dataseries |>
      dplyr::distinct(series_id, .keep_all = TRUE) |>
      dplyr::select(series_id, dataset_id)

    # Renku: the 'dataset' table is part of the Parquet export
    dataset_lookup <- tryCatch(
      tn_read_index("dataset") |> dplyr::select(dataset_id, dataset_name),
      error = function(e) data.frame(dataset_id = integer(0), dataset_name = character(0))
    )

    dataset_id_map <- dataset_id_map |>
      dplyr::left_join(dataset_lookup, by = "dataset_id") |>
      dplyr::select(series_id, dataset_name)
  } else {
    dataset_id_map <- data.frame(series_id = integer(0), dataset_name = character(0))
  }
  dataseries <- dplyr::select(dataseries, -dplyr::any_of("dataset_id"))

  # for dataqual == "L3", download_treenet already selects dendrometer series
  # from data_dendro_l3 (which carries the L2-style columns max/twd/gro_yr/
  # gro_start/gro_end/frost/flags) and environmental series from data_all_l3
  # (value/flag), so no further ad-hoc processing is needed here; just split
  # the combined download into the two output schemas.
dendrometers <- metadata |>
	dplyr::filter(sensor_class == "Dendrometer")

if (dataqual == "L3" && nrow(dendrometers) > 0) {
	dendroseries <- dataseries |>
		dplyr::filter(series_id %in% dendrometers$series_id) |>
		dplyr::select(ts, series_id, value, max, twd, gro_yr, gro_start, gro_end, frost, flags) |>
		dplyr::arrange(series_id, ts)
	dataseries <- dataseries |>
		dplyr::filter(!(series_id %in% dendrometers$series_id)) |>
		dplyr::select(dplyr::any_of(c("ts", "series_id", "value", "flag")))
}
  
	# metadata <- metadata %>%
	# 	mutate(data_format = dataqual, extracted_from = daterange[1], extracted_until = daterange[2], timezone = timezone) %>%
	# 	dplyr::select(measure_point, current_sensor=sensor_name, sensor_class, series_start, series_stop, site_xcor, site_ycor, site_altitude)
	metadata <- metadata %>%
		rename(current_sensor = sensor_name) %>%
		mutate(data_format = dataqual, extracted_from = daterange[1], extracted_until = daterange[2], timezone = timezone) %>%
		# first_date / last_date: first and last row of the series at this data quality level (db_start / db_stop,
		# from import_log), in the export's time zone, replacing the sensor's series_start / series_stop
		# (.env$timezone: the script variable; plain `timezone` would be the column created just above)
		# (import_log's own first_date / last_date columns, if present, are replaced by these formatted ones)
		mutate(first_date = format(db_start, "%Y-%m-%d %H:%M", tz = .env$timezone),
		       last_date  = format(db_stop,  "%Y-%m-%d %H:%M", tz = .env$timezone)) %>%
		relocate(first_date, last_date, .before = series_start) %>%
		select(-c(series_start,series_stop,table_name,db_start,db_stop,sensor_data_source),
		       -any_of(c("import_nrows","import_date")),
		       -any_of(c("import_from","import_until"))) %>% #tree_name,tree_xcor,tree_ycor,tree_altitude,genus_species
		dplyr::left_join(dataset_id_map, by = "series_id") %>%
		relocate(series_id, data_format, dataset_name, extracted_from, extracted_until, timezone)

	# the processed dendrometer data has a different schema (L2 columns) than
	# the rest of the export, so it goes into its own file when present. Decide
	# the filenames before writing anything, so the file we write is the file
	# we later reference in exportfiles/cleanup.
	send_dendrofile <- dataqual == "L3" && exists("dendroseries") && nrow(dendroseries) > 0
    send_datafile   <- exists("dataseries") && nrow(dataseries) > 0

	if (send_dendrofile) {
    	dendrofile <- paste0("exports/tn_timeseries_dendro_", dataqual, "_", daterange[1],"_", daterange[2],"_", jobid,".csv")
    	datafile   <- paste0("exports/tn_timeseries_enviro_", dataqual, "_", daterange[1],"_", daterange[2],"_", jobid,".csv")
		write.csv(
			dendroseries,
			dendrofile,
			row.names = F
		)
	}
	if (send_datafile) {
		write.csv(
			dataseries,
			datafile,
			row.names = F
		)
	}
	write.csv(
		metadata,
		metafile,
		row.names = F
	)

	# Send file via Python script (filesender.py)
	# Renku: the files are zipped into one upload and the transfer is benchmarked (scripts/benchmark_logger.R)
	zipfile <- paste0("exports/tn_download_", jobid, ".zip")
	zip::zip(
		zipfile = zipfile,
		files   = c(metafile, if (send_datafile) datafile, if (send_dendrofile) dendrofile),
		mode    = "cherry-pick"
	)
	command <- sprintf(
		"nohup python3 scripts/filesender.py %s -r %s >> %s 2>&1",
		shQuote(zipfile), shQuote(recipient), logfile
	)
	source("scripts/benchmark_logger.R")
	tryCatch(
		benchmark_and_log({
			system(command, wait = T)
		}),
		# the files have been sent by then; a failed benchmark log must not skip the clean-up below
		error = function(e) message("Benchmark log not written: ", conditionMessage(e))
	)

	# Clean up after sending
	if (file.exists(logfile) && file.info(logfile)$size == 0) {
		file.remove(logfile)
	}
	if (send_datafile && file.exists(datafile)) {
		file.remove(datafile)
	}
	if (send_dendrofile && file.exists(dendrofile)) {
		file.remove(dendrofile)
	}
	if (file.exists(metafile)) {
		file.remove(metafile)
	}
	if (file.exists(zipfile)) {
		file.remove(zipfile)
	}
	if (file.exists(paramsfile)) {
		file.remove(paramsfile)
	}
}
