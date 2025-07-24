# benchmark_logger.R

benchmark_and_log <- function(expr_to_benchmark) {
  log_path <- "/home/shiny/work/forestcast/benchmark-log.csv"

  # ---- 1. Benchmark execution ----
  timing <- system.time(eval(expr_to_benchmark))
  elapsed_time <- timing["elapsed"]

  # ---- 2. Collect metadata ----
  timestamp <- as.character(Sys.time())
  cpu_cores <- as.integer(system("nproc", intern = TRUE))
  memory_gb <- as.numeric(system("awk '/MemTotal/ {print $2}' /proc/meminfo", intern = TRUE)) / 1024 / 1024  # in GB

  # ---- 3. Build log entry ----
  entry <- data.frame(
    timestamp   = timestamp,
    cpu_cores   = cpu_cores,
    memory_gb   = memory_gb,
    elapsed_sec = round(elapsed_time, 3),
    stringsAsFactors = FALSE
  )

  # ---- 4. Append or create log file ----
  if (file.exists(log_path)) {
    log_df <- read.csv(log_path, stringsAsFactors = FALSE)
    log_df <- rbind(log_df, entry)
  } else {
    log_df <- entry
  }

  # ---- 5. Save updated log ----
  write.csv(log_df, log_path, row.names = FALSE)

  # ---- 6. Return timing info ----
  return(entry)
}
