#' This is a test application to serve as the login page.
#' The login is based on the connect function
#' This is a minimum example and can be much extended
# 0. Libraries ------------------------------------------------------------

suppressPackageStartupMessages({
	library(shiny)
	library(DBI)
	library(shinyjs)
	library(shinytip)
	library(shinyvalidate)
	library(dplyr)
	library(dygraphs)
	library(DT)
	library(waiter)
})

users <- reactiveValues(count = 0)
source('scripts/tn_timezone.R')
source('scripts/tn_parquet.R')   # Renku: data are read from the Parquet export, not PostgreSQL
source('scripts/0_log_in_out.R')
options(shiny.fullstacktrace = T)
options(shiny.autoreload = F)
options(shiny.trace = F)
options(warn = 0)
# Sys.setenv(TMPDIR = "/home/wilhelm/tmp")
dir.create("tmp", showWarnings = FALSE, recursive = TRUE)
Sys.setenv(TMPDIR = normalizePath("tmp"))
dir.create("exports", showWarnings = FALSE, recursive = TRUE)   # parameter files and export logs

# Renku: the login credentials are a Renku session secret, mounted as a file
tn_creds_file <- Sys.getenv("TREENET_CREDS", "/secrets/login_creds.csv")

parse_validity <- function(x, formats = c("%Y-%m-%d", "%d.%m.%Y", "%Y/%m/%d")){
	# convert the validity strings to Date, trying each format per element; NA if none fits
	x   <- trimws(as.character(x))
	out <- as.Date(rep(NA_character_, length(x)))
	for (f in formats) {
		todo <- is.na(out) & !is.na(x) & nzchar(x)
		if (!any(todo)) break
		out[todo] <- as.Date(x[todo], format = f)
	}
	out
}

read_tn_creds <- function(){
	# read the credentials and add the parsed expiry date as `valid_until`
	# the file is semicolon-delimited so that a limit may contain commas, e.g. %in% c('a','b'); trim_ws is
	# needed because read_delim (unlike read_csv) keeps the tab/space padding used to align the columns
	cred <- readr::read_delim(tn_creds_file, delim=";", trim_ws=TRUE, col_types=readr::cols())
	if (!"validities" %in% names(cred)) {
		warning(tn_creds_file, " has no 'validities' column; all logins are refused.")
		cred$validities <- NA_character_
	}
	cred$valid_until <- parse_validity(cred$validities)
	cred
}

tn_scope_expr <- function(scopes, limits){
	# build the filter expression (a string) for each scope/limit pair
	# - scope 'site_name' with a limit that does not start with an R operator is a comma-separated list
	#   of substrings: a site matches if its name contains any of them (case-insensitive, literal),
	#   e.g. "Neunkirch" or "Davos-Seehornwald, Laegeren"
	# - everything else (e.g. "== 'x'", "%in% c('a','b')", "!= ''", scope 'country') is kept as `scope limit`
	# an element is NA if it grants nothing (no usable substring), so that the caller can drop it (fail closed)
	scopes <- trimws(as.character(scopes))
	limits <- trimws(as.character(limits))
	op_re  <- "^(==|!=|<|>|!|%[^%]*%)"
	vapply(seq_along(scopes), function(i){
		if (identical(scopes[i], "site_name") && !is.na(limits[i]) && !grepl(op_re, limits[i])) {
			subs <- trimws(strsplit(limits[i], ",", fixed = TRUE)[[1]])
			subs <- unique(tolower(subs[nzchar(subs)]))
			if (length(subs) == 0) return(NA_character_)
			paste(sprintf("grepl(%s, tolower(site_name), fixed = TRUE)", vapply(subs, deparse, "")), collapse = " | ")
		} else {
			paste(scopes[i], limits[i])
		}
	}, character(1))
}

get_tn_scope <- function(username=NULL, password=NULL){
	# validate user credentials and specify data access
	# a credential is valid up to and including the date in the `validities` column;
	# a missing or unparsable date is treated as expired (fail closed)
	cred    <- read_tn_creds()
	matched <- cred$usernames %in% username & cred$passwords %in% password
	valid   <- !is.na(cred$valid_until) & cred$valid_until >= Sys.Date()
	mask    <- matched & valid
	
	if (any(matched & !mask)) message("Login refused for '", username, "': credentials expired or validity date missing.")
	
	if (any(mask)) {
		out <- tn_scope_expr(cred$scopes, cred$limits)[mask]
		out <- out[!is.na(out)]
		if (length(out) == 0) return(NULL)
		return(out)
	} else {
		return(NULL)
	}
}

get_tn_expiry <- function(username=NULL, password=NULL){
	# expiry date of credentials that match but have expired; NA if there is no such match
	# (wrong username/password, still valid, or no readable validity date)
	cred    <- read_tn_creds()
	matched <- cred$usernames %in% username & cred$passwords %in% password
	expired <- matched & !is.na(cred$valid_until) & cred$valid_until < Sys.Date()
	if (any(expired)) max(cred$valid_until[expired]) else as.Date(NA)
}

toDT <- function(x){
	#' @description create a data table from the data.frame
	x %>% 
		DT::datatable(escape   = FALSE,
									rownames = F,
									list(dom       = 'Bft',
											 scrollX   = TRUE,
											 # autoWidth = TRUE,
											 scrollY   = "75vh",
											 paging    = FALSE,
											 # pageLength = nrow(x),
											 searching = TRUE,
											 searchHighlight = TRUE,
											 columnDefs = list(list(className = 'dt-left', targets = '_all'))
									), 
									selection = list(mode = 'multiple')#, selected = seq_len(nrow(x)))
									# selection = list(mode = 'multiple', selected = 1)
		)
}


get_connection_string <- function(db_host){
	# support function to retrieve the connection string for a particular service
	# return a named string
	
	# Use switch statement to handle different cases for db_host
	out <- switch(db_host,
								pgdb01       = c('host' = 'dbpg01.wsl.ch',  	'port' = '5433'),
								pgdb01dev    = c('host' = 'dbpg01.wsl.ch',  	'port' = '5434'),
								postgres     = c('host' = 'postgres.wsl.ch',	'port' = '5432'),
								localhost    = c('host' = 'localhost',      	'port' = '5432'),
								pgdbtapp     = c('host' = 'pgdbtapp.wsl.ch',	'port' = '5432'),
								pgdbtreenet  = c('host' = 'pgdbtreenet.wsl.ch',	'port' = '5432'),
								# Default case for handling invalid db_host values
								stop('The db_host is not valid! Please provide a valid db_host.')
	)
	
	return( out )
}

# Renku: a "connection" only records where the Parquet export is. It is a DBIConnection so that the
# DBI::dbDisconnect() calls in server.R and 0_log_in_out.R work unchanged.
setClass("TnParquetConnection", contains = "DBIConnection", slots = c(root = "character"))
setMethod("dbDisconnect", "TnParquetConnection", function(conn, ...) invisible(TRUE))

dbConnect_tn <- function(
  username,
  password,
  db_host = NULL,   # unused, kept for the call in 0_log_in_out.R
  db_name = NULL    # unused
) {
  if (is.null(get_tn_scope(username, password))) {
    shiny::showNotification("Invalid user scope or authentication failed.", type = "error")
    stop("User authentication failed.")
  }
  if (!file.exists(file.path(tn_parquet_dir, "view_metadata.parquet"))) {
    shiny::showNotification("The TreeNet data export is not available in this session.", type = "error")
    stop("No Parquet export found in ", tn_parquet_dir)
  }
  new("TnParquetConnection", root = tn_parquet_dir)
}


get_tn_data <- function(
	conn,
	installation    = NULL,
	variable        = NULL,
	messvar         = NULL,
	messtime_from   = NULL,
	messtime_to     = NULL,
	inserttime_from = NULL,
	inserttime_to   = NULL,
	retrieve        = FALSE
){
	
	if (is.null( c(installation, variable, messvar, messtime_from, messtime_to, inserttime_from, inserttime_to)))  warning('All data are selected. No subsetting is provided!')
	
	# check the connection
	if( !"PqConnection" %in% class( conn ) ){
		conn
		return()
	}
	
	# Get the information on unique selected variables
	messvar_tbl <- tbl_tn( conn, 'public', 'MESSVAR')
	
	# subset required data
	if( !is.null(installation)) messvar_tbl <- dplyr::filter( messvar_tbl, .data$INSTALLATION_ID %in% installation)
	
	if( !is.null(variable)) messvar_tbl <- dplyr::filter( messvar_tbl, .data$VARIABLE_ID %in% variable)
	
	if( !is.null(messvar)) messvar_tbl <- dplyr::filter( messvar_tbl, .data$MESSVAR_ID %in% messvar)
	
	messvar.df <- messvar_tbl %>% 
		dplyr::inner_join( tbl_tn( conn, 'public', 'VARTABLE'), by = "VARTABLE_ID") %>%
		dplyr::select( .data$VARTABLE_NAME, .data$MESSVAR_ID) %>%
		dplyr::collect()
	
	# loop through the tables and retrieve the data
	for ( t_name in unique(messvar.df$VARTABLE_NAME)){
		
		out_t <- try( tbl_tn( conn, 'public', t_name), TRUE)
		
		# check if user have access to the table to continue
		if( !"try-error" %in% class( out_t) ){
			
			messvar_t <- dplyr::filter( messvar.df, .data$VARTABLE_NAME %in% t_name) %>%
				dplyr::pull(.data$MESSVAR_ID)
			
			# subset required data
			if( !is.null(messvar)) out_t <- dplyr::filter( out_t, .data$MESSVAR_ID %in% messvar_t)
			
			if( !is.null(messtime_from) && !is.na(messtime_from)){
				messtime_from <- as.character(messtime_from)
				out_t <- dplyr::filter( out_t, .data$MESSTIME >=  to_date(messtime_from, 'yyyy-mm-dd hh24:mi:ss'))
			}
			
			if( !is.null(messtime_to) && !is.na(messtime_to)){
				messtime_to <- as.character(messtime_to)
				out_t <- dplyr::filter( out_t, .data$MESSTIME <  to_date(messtime_to, 'yyyy-mm-dd hh24:mi:ss'))
			}
			
			if( !is.null(inserttime_from) && !is.na(inserttime_from)){
				inserttime_from <- as.character(inserttime_from)
				out_t <- dplyr::filter( out_t, .data$INSERTTIME >=  to_date(inserttime_from, 'yyyy-mm-dd hh24:mi:ss'))
			}
			
			if( !is.null(inserttime_to) && !is.na(inserttime_to)){
				inserttime_to <- as.character(inserttime_to)
				out_t <- dplyr::filter( out_t, .data$INSERTTIME <  to_date(inserttime_to, 'yyyy-mm-dd hh24:mi:ss'))
			}
			
			# Retrieve/download the results if needed
			if( retrieve ){
				out_t <- dplyr::collect( out_t )
			}
			
			if( exists("out") ){
				out <- dplyr::union_all(out, out_t)
			}else{
				out <- out_t
			}
			
		}
		
	}
	
	if( !exists("out") ){
		warning('No data fullfiled the requirenments!')
		out <- dplyr::tibble()
	}
	
	return( out )
}


tbl_tn <- function(
	conn,
	schema = 'public',   # unused, kept for the calls in 2_server_adata_vis.R
	table = 'view_metadata',
	col = c('*'),
	retrieve = FALSE){
	#' @description Renku: an index table from the Parquet export as a tibble (dplyr verbs and collect() work as on
	#' the database). import_log is limited to the data tables that were exported, so that the app never lists
	#' series it cannot export.

	tbl_out <- tn_read_index(table)
	if (table == "import_log") {
		tbl_out <- dplyr::filter(tbl_out, table_name %in% tn_parquet_tables())
	}
	if (!identical(col, '*')) {
		tbl_out <- dplyr::select(tbl_out, dplyr::all_of(col))
	}
	tbl_out
}

tn_series_with_rows <- function(conn, df, from, to, tz = tn_default_tz, timeout_s = 60){
	#' @description Renku: no exact check. On PostgreSQL this probes the data tables for at least one row per series
	#' in the period; on the Parquet export that means opening every chunk of the table in the bucket (thousands of
	#' files), far too slow for an interactive filter. The table therefore relies on import_log and view_coverage,
	#' and an export of a series without rows in the period reports that no data are available.
	rep(TRUE, nrow(df))
}

# filesender <- function(data,
# 											 recipient, 
# 											 filename = paste0("exports/tn_",recipient,"_",lubridate::now() %>% gsub(" ","-",.) %>% gsub(":","",.), ".csv"), 
# 											 logfile = paste0("exports/",lubridate::today(),".log")) {
# 	write.csv(data, filename, row.names = F)
# 	command <- sprintf(
# 	paste0("nohup python3 scripts/filesender.py ",filename," -r ",recipient ," >> %s 2>&1"),
# 	logfile
# )
# 	system(command, wait = F)
# 
# }


