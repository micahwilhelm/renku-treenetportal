# TreeNet Data Portal <img src="dev/logo/logo.png" align="right" width="80"/>

An R Shiny application to browse and export data from the TreeNet database (PostgreSQL).

The application is running at https://webapps.wsl.ch/treenetportal/

## Getting started

You need valid TreeNet portal credentials (username and password) to log in. They are issued by the database administrator (micah.wilhelm@wsl.ch). Once logged in you can:

1. Select the **data quality** level
2. Select **variables**, **species** and **sites**
3. Select the **time zone** and the **date range**
4. Highlight the time series of interest in the table on the right (hold shift to select a range of rows) and click **Export selected data**

### Data quality levels

| Level | Description | Data | Coverage |
|-------|-------------|------|----------|
| L0 | Raw data | Tree & environmental | up to today |
| L1 | Time-aligned (interpolated to common timestamps) | Tree & environmental | up to today |
| L2 | Auto-cleaned (outlier & jump removal) | Tree only | up to today |
| L3 | Gap-filled by neural networks | Stem radius change & environmental | 2000 to 2025 |
| LM | Manually cleaned (expert-reviewed) | Tree only | end of last year |

### Time zone

The database stores UTC. The time zone chosen in the app (default `Etc/GMT-1`, i.e. UTC+1 all year, no daylight saving) is applied to the date range (whole days in that zone, from 00:00 of the first day to the end of the last day), to the first/last date shown in the table, and to the timestamps in the exported files.

### Table contents

The table lists only series that have data at the selected quality level. For L2, L3 and LM the listing is confirmed against the data tables for the selected period (the import log can be stale), so every listed series returns data. L0/L1 skip this check because their tables are large.

### Export

The export is run by `scripts/filesender.R`, which downloads the selected series with `treenetproc::download_treenet()` and writes CSV files to `exports/`:

- `tn_metadata_<quality>_<from>_<to>_<jobid>.csv`: metadata of the selected series (including the data set name for L3)
- `tn_timeseries_<quality>_...csv`: the time series; dendrometer and environmental series are written to separate `tn_timeseries_dendro_...` / `tn_timeseries_enviro_...` files
- `<jobid>.log`: log of the job

## Access control

Credentials are kept in `scripts/login_creds.csv` (semicolon-delimited) and re-read at every login, so changes take effect as soon as the file is deployed. Columns:

| Column | Meaning |
|--------|---------|
| `usernames`, `passwords` | login |
| `scopes`, `limits` | data access: an R filter expression `scope limit`, e.g. `country == 'Switzerland'` or `site_name %in% c('a','b')`. For `site_name`, a limit without an operator is a comma-separated list of case-insensitive substrings, e.g. `Davos-Seehornwald, Laegeren` |
| `validities` | last valid day (`YYYY-MM-DD`, `DD.MM.YYYY` or `YYYY/MM/DD`). A missing or unparsable date is treated as expired; a user with expired credentials is told the expiry date |

### Adding a user

Both scripts add a user with a random password and e-mail the credentials (via the WSL SMTP relay):

```sh
Rscript scripts/login_add_user.R <email> [--country A,B | --site A,B | --scope S --limit L] \
                                 [--months N | --until YYYY-MM-DD] [--name NAME] [--dry-run] [--no-email] [--yes]
```

Without options the user gets access to all Swiss sites for 6 months. Run with `--help` for all options. `scripts/login_add_user_interactive.R` does the same from a set of variables at the top of the script (for RStudio / VS Code).

### Admin

The admin user sees the list of logged-on users in the side panel and can disconnect all users. The portal limits the number of simultaneous users and warns about slower responses when several are logged on.

## Run locally

Install the packages loaded in `global.R` (`shiny`, `shinyjs`, `shinytip`, `shinyvalidate`, `shinyWidgets`, `waiter`, `dplyr`, `dygraphs`, `DT`, ...) and the `treenetproc` package, provide the database settings in `scripts/config.yml`, and run:

```r
shiny::runApp(".")
```

## Project structure

| File | Purpose |
|------|---------|
| `global.R` | libraries, credential / scope handling, database helpers |
| `ui.R`, `server.R` | app entry points, admin panel |
| `scripts/0_log_in_out.R` | login / logout module |
| `scripts/1_ui_adata_vis.R` | UI of the data selection page |
| `scripts/2_server_adata_vis.R` | server logic: metadata, filtering, table, export |
| `scripts/filesender.R` | export job (download and write CSV files) |
| `scripts/tn_timezone.R` | time zone helpers shared by the app and the export |
| `scripts/login_creds.csv` | credentials, scopes and validity |
| `scripts/login_add_user*.R` | add a user and send the credentials |
