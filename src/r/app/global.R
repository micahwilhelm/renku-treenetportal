#' This is a test application to serve as the login page.
#' THe login is based on the connect function
#' This is a minimum example and can be much extended
# 0. Libraries ------------------------------------------------------------

suppressPackageStartupMessages({
	#remotes::install_github("daattali/shinytip")
	library(shinytip)
	library(shinyjs)
	library(shinyvalidate)
	library(shinyWidgets)
	library(dplyr)
	library(dygraphs)
	library(DT)
	library(DBI)
	library(sqldf)
	library(RSQLite)
})

users <- reactiveValues(count = 0)
source('scripts/0_log_in_out.R')
options(shiny.fullstacktrace = T)
options(shiny.autoreload = F)
options(shiny.trace = F)
options(warn = 0)

get_tn_scope <- function(username=NULL, password=NULL){
	# validate user credentials and specify data access
	cred <- readr::read_csv("scripts/login_creds.csv", col_types=readr::cols())
	mask    <- cred$usernames %in% username & cred$passwords %in% password
	
	if (any(mask)) {
		out <- paste(cred$scopes, cred$limits)[mask]
		return(out)
	} else {
		return(NULL)
	}
}

toDT <- function(x){
	#' @description create a data table from the data.frame
	x %>% 
		DT::datatable(escape   = FALSE,
									rownames = TRUE,
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

dbConnect_tn <- function(
  username,
  password,
  db_host = 'pgdbtreenet',
  db_name = NULL,
  max_retries = 10,
  retry_delay = 1 # in seconds
) {
  hosts <- c("pgdb01", "pgdb01dev", "postgres", "localhost", "pgdbtapp", "pgdbtreenet")

  # Check db_name requirement
  if (db_host %in% hosts && is.null(db_name)) {
    shiny::showNotification("Error: db_name is required when connecting to this host.", type = "error")
    stop("For the PostgreSQL database, db_name is required!")
  }

  connection_string <- tryCatch(
    get_connection_string(db_host = db_host),
    error = function(e) {
      shiny::showNotification(paste("Error getting connection string:", e$message), type = "error")
      stop("Could not get connection string.")
    }
  )

  # Retry logic
  for (attempt in seq_len(max_retries)) {
    try({
      if (!is.null(get_tn_scope(username, password))) {
        conn <- DBI::dbConnect(RSQLite::SQLite(), "/home/shiny/work/forestcast/treenet.sqlite")

        shiny::showNotification("Database connection established.", type = "message", duration = 3)
        return(conn)
      } else {
        shiny::showNotification("Invalid user scope or authentication failed.", type = "error")
        stop("User authentication failed.")
      }
    }, silent = TRUE)

    # If we got here, the attempt failed
    if (attempt < max_retries) {
      delay <- retry_delay * 2^(attempt - 1) # exponential backoff
      message(sprintf("Connection attempt %d failed. Retrying in %d seconds...", attempt, delay))
      Sys.sleep(delay)
    } else {
      shiny::showNotification("Error: Failed to connect to the database after multiple attempts.", type = "error")
      message("Final attempt failed. Could not connect to the database.")
      return(NULL)
    }
  }
}


dbConnect_tn_old <- function(
	username,
	password,
	db_host = 'pgdbtreenet',
	db_name = NULL
){
	
	hosts     <- c("pgdb01", "pgdb01dev", "postgres", "localhost", "pgdbtapp", "pgdbtreenet")
	
	if( db_host %in% hosts & is.null(db_name)){
		out <- NULL
		stop("For the PostgreSQL database db_name is required!")
	}
	
	connection_string <- get_connection_string(db_host=db_host)
	
	tryCatch(
		expr = {
			
			# if( !is.null(get_tn_scope(username, password)) ){
				out <- DBI::dbConnect(drv      = RPostgres::Postgres(),
															dbname   = db_name,
															host     = connection_string[['host']],
															port     = connection_string[['port']],
															user     = "tnreader",
															password = "Iku8Gem3")
			# }
			
			return( out )
			
		},
		error = function( cond ){
			message( "Error, could not connect to the database \n" )
			message( cond )
			return( NA )
		}
	)
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
	schema = 'public',
	table = 'view_metadata',
	col = c('*'),
	retrieve = FALSE){
	
	if( class(conn) %in% c("OraConnection", "PqConnection")){
		
		tbl_out <- dplyr::tbl( conn, dplyr::sql( paste0( 'select ', paste0(col, collapse = ", "), ' from "', schema, '"."', table, '"') ) )
		
		if( retrieve ){
			dplyr::collect( tbl_out )
		}
		
	}else{
		
		cat('The Database connection is invalid!')
		
		tbl_out <- NULL
		
	}
	
	return(tbl_out)
	
}


