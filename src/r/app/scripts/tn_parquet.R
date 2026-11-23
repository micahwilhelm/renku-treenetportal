# Renku data layer: the TreeNet database is not reachable from Renku, so the portal reads the daily Parquet
# export in the forestcast-data bucket (written by tndb/cronjob_tndb_export_sdsc_daily.R) instead of PostgreSQL.
#
# Layout of tn_parquet_dir:
#   view_metadata.parquet, import_log.parquet, view_coverage.parquet, dataset.parquet   small index tables
#   data_<level>/data_<level>-<timestamp>.parquet                                         data tables, in chunks
#
# Shared by the app (global.R) and the export (scripts/filesender.R). Everything database-specific in the Renku
# version lives here, so the rest of the code can follow the main treenetportal branch closely.

tn_parquet_dir <- Sys.getenv("TREENET_PARQUET_DIR", "/home/shiny/work/forestcast/treenet-parquet")

# data tables per data quality level: c(all, dendro), as in dataqual.tbl() of 2_server_adata_vis.R
tn_level_tables <- list(
	L0 = c("data_all_l0"),
	L1 = c("data_all_l1"),
	L2 = c("data_all_l2", "data_dendro_l2"),
	L3 = c("data_all_l3", "data_dendro_l3"),
	LM = c("data_dendro_lm")
)

tn_parquet_tables <- function(){
	#' @description names of the data tables that have been exported to Parquet
	dirs <- list.dirs(tn_parquet_dir, full.names = FALSE, recursive = FALSE)
	dirs[startsWith(dirs, "data_")]
}

tn_levels_available <- function(){
	#' @description data quality levels with at least one exported data table, in the order of tn_level_tables
	tabs <- tn_parquet_tables()
	names(Filter(function(x) any(x %in% tabs), tn_level_tables))
}

.tn_index_cache <- new.env(parent = emptyenv())

tn_read_index <- function(table){
	#' @description read one of the small index tables; cached until the file changes (the export rewrites it daily)
	f <- file.path(tn_parquet_dir, paste0(table, ".parquet"))
	if (!file.exists(f)) {
		stop("Missing ", f, ". It is written by the daily TreeNet Parquet export (tndb/cronjob_tndb_export_sdsc_daily.R).")
	}
	key   <- paste(table, file.info(f)$mtime)
	if (!exists(key, envir = .tn_index_cache, inherits = FALSE)) {
		rm(list = ls(.tn_index_cache)[startsWith(ls(.tn_index_cache), paste0(table, " "))], envir = .tn_index_cache)
		assign(key, dplyr::as_tibble(arrow::read_parquet(f)), envir = .tn_index_cache)
	}
	get(key, envir = .tn_index_cache, inherits = FALSE)
}

tn_read_series <- function(table, series_id, lo, hi){
	#' @description rows of the given series with lo <= ts < hi (POSIXct instants) from one exported data table;
	#' NULL if the table was not exported. insert_date is internal and dropped.
	dir <- file.path(tn_parquet_dir, table)
	if (!dir.exists(dir)) return(NULL)
	ids <- as.integer(series_id)
	arrow::open_dataset(dir, format = "parquet", unify_schemas = TRUE) |>
		dplyr::filter(series_id %in% ids, ts >= lo, ts < hi) |>
		dplyr::select(-dplyr::any_of("insert_date")) |>
		dplyr::collect()
}
