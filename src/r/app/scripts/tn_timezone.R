# Time zone helpers shared by the app (global.R) and the export (scripts/filesender.R).
#
# The database stores true UTC instants (timestamptz). The user picks a time zone in the app; it applies to the
# date range (whole days in that zone), the first_date / last_date shown in the table, and the exported
# timestamps and dates.

tn_default_tz <- "Etc/GMT-1"

tn_valid_tz <- function(tz){
	#' @description tz if it is a known time zone name, else the default (e.g. input$timezone is NULL before the UI exists)
	if (is.character(tz) && length(tz) == 1 && !is.na(tz) && tz %in% OlsonNames()) tz else tn_default_tz
}

tn_day_start <- function(date, tz){
	#' @description 00:00 of the date in time zone tz as an instant; if that local time does not exist
	#' (daylight saving jump at midnight in some zones) the first valid time after it
	lubridate::force_tz(as.POSIXct(format(as.Date(date), "%Y-%m-%d"), tz = "UTC"), tzone = tz, roll_dst = c("post", "post"))
}

tn_period_window <- function(from, to, tz){
	#' @description the selected period as instants: from 00:00 of the first day up to (excluding) 00:00 of the day
	#' after the last day, in time zone tz. A day can be 23, 24 or 25 hours long.
	list(lo = tn_day_start(from, tz), hi = tn_day_start(as.Date(to) + 1, tz))
}

tn_undo_download_tz <- function(ts){
	#' @description treenetproc::download_treenet() up to the 2026-10 fix read true UTC timestamps from the database
	#' but then relabelled them as Etc/GMT-1 (force_tz, not a conversion), so every returned instant was 1 h earlier
	#' than the database's, whatever `tz` was requested. On such a package this restores the database instants
	#' (verified 2026-10-15); on a fixed package it leaves ts alone (see tn_download_shifts()).
	if (!tn_download_shifts()) return(ts)
	lubridate::force_tz(lubridate::with_tz(ts, "Etc/GMT-1"), "UTC")
}

tn_download_shifts <- function(){
	#' @description TRUE if the installed treenetproc still shifts downloads by -1 h. The fixed package declares
	#' `Config/treenetproc/download_tz: 2` in its DESCRIPTION, so the portal works with either version and the order
	#' of deploying the portal and installing the package does not matter.
	is.null(utils::packageDescription("treenetproc")[["Config/treenetproc/download_tz"]])
}
