# ============================================================
# clean_kelmarsh.R : Kelmarsh wind farm open data -> clean .rds files
#
# Source: Cubico Sustainable Investments, "Kelmarsh wind farm data",
#   Zenodo record 16807551 (Aug 2025 release, CC-BY-4.0), 6 x Senvion
#   MM92 (2.05 MW), 10-min SCADA + event logs, 2016-01-03 .. 2024-12-31.
#
# Inputs (raw_dir, unzipped as downloaded):
#   Kelmarsh_SCADA_<year>_*/Turbine_Data_Kelmarsh_<t>_*.csv  10-min SCADA
#   Kelmarsh_SCADA_<year>_*/Status_Kelmarsh_<t>_*.csv        event/status log
#   Kelmarsh_PMU_*/Device_Data_*, Status_*                   substation PMU
#   Kelmarsh_Grid_Meter_*/Device_Data_*, Status_*            fiscal meter
#   Kelmarsh_WT_static.csv, Kelmarsh_WT_dataSignalMapping.csv
#
# Outputs (out_dir):
#   turbine_static.rds         one row per turbine (hub height, coords, COD)
#   signal_mapping.rds         Greenbyte signal id -> title -> unit
#   scada_10min/scada_10min_<year>.parquet  all signals, one file per year
#                              (query all years lazily with
#                               arrow::open_dataset(scada_dir, unify_schemas = TRUE))
#   scada_10min_core.parquet   2016-2024, turbine x 10-min, key signals only
#   turbine_events.rds         every status/event row, repeats flagged
#   turbine_stops.rds          Status == "Stop" subset of turbine_events
#   pmu_10min.parquet, grid_meter_10min.parquet, device_events.rds
#   column_dictionary.csv      clean name <-> original Greenbyte header
#   cleaning_log.txt           every count this script reports
#
# Pathologies found in the raw files (and what is done about them):
#   1. 2023 + 2024 SCADA files are CUMULATIVE EXPORTS: the year was
#      exported in 81 growing chunks (655, 1310, ... 52560 rows) and
#      all chunks were concatenated, so each timestamp appears up to 81
#      times (2.17M rows for 52.6k intervals, ~2.7 GB per file). Only
#      the FIRST appearance of a timestamp carries values; every later
#      copy is NaN. Verified on 4 files x 4 signals: zero non-NA values
#      in later copies. -> keep first appearance; any non-NA value in a
#      later copy is used to fill a gap and counted in the log.
#   2. Header drift across years: 299 cols (2016-20), 303 (2021-22),
#      312 (2023-24). Acceleration units renamed "mm/ss" -> "mm/s2" in
#      2023. -> names cleaned to snake_case, mm/ss == mm/s2, years
#      stacked with fill (signal absent in a year = NA).
#   3. 2016 starts 2016-01-03 (not 01-01). Interval gaps -> each
#      turbine-year completed to a regular 10-min grid, with
#      src_row_missing = TRUE on inserted rows.
#   4. Status logs: events open at a year boundary appear in BOTH
#      year files (22 events), plus 688 exact duplicate rows within
#      files. -> kept and flagged (dup_cross_year, dup_exact), since
#      the source does not document whether repeats are meaningful.
#      Most Informational rows have end "-" (instantaneous) -> NA end,
#      duration 0. Durations like "211:08:29" (hours > 24) parsed.
#      2016-20 logs lack the two "contract category" columns -> NA.
#   5. Header line is a comment ("# Date and time,...") on line 10 for
#      SCADA/device files; status files have a plain header on line 10.
#
# Physical-range screening (review these choices): values outside the
# bounds below are set to NA and counted in the log. Power is NOT
# screened (negative = self-consumption, which is real).
#   wind speeds 0..40 m/s | directions/angles 0..360 deg (pitch excluded)
#   temperatures -40..150 degC
# In the 2023 test run these caught real sensor faults, not weather:
# T4 bearing/gear-oil temps stuck at 324.4 degC for 7-19 Jul 2023
# (1,598 intervals) and T4/T5 anemometer 2 stuck at -15 m/s.
#
# Engine: data.table (raw 2023/24 files are 2.7 GB each; needs ~8 GB
# free RAM per file). Tested with R 4.4.3 + data.table.
# ============================================================
library(data.table)
library(arrow)        # large tables are written as parquet

raw_dir <- "C:/Users/cantwi/Dropbox/Interruption Management/IM Project/Data/Source Data/Wind Farm Data"
out_dir <- file.path(raw_dir, "Clean")
if (!exists("years")) years <- 2016:2024   # set years <- 2016 first for a quick test
scada_dir <- file.path(out_dir, "scada_10min")   # one parquet per year
dir.create(scada_dir, showWarnings = FALSE, recursive = TRUE)

log_lines <- character()
logf <- function(...) {
  msg <- sprintf(...)
  cat(msg, "\n")
  log_lines <<- c(log_lines, msg)
}

# ---- shared helpers --------------------------------------------------------
clean_names <- function(x) {
  x <- enc2utf8(x)
  x <- gsub("mm/ss", "mm/s2", x, fixed = TRUE)          # 2016-22 spelling
  x <- gsub("\u00b0C", "degc", x)
  x <- gsub("\u00b0", "deg", x)
  x <- gsub("%", "pct", x, fixed = TRUE)
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_|_$", "", x)
}

# Greenbyte header is the commented line that starts "# Date and time"
read_greenbyte_header <- function(f) {
  l <- readLines(f, n = 15, warn = FALSE, encoding = "UTF-8")
  i <- grep("^# Date and time", l)[1]
  if (is.na(i)) stop("no '# Date and time' header in ", basename(f))
  list(line = i, names = names(fread(text = sub("^# ", "", l[i]), header = TRUE)))
}

parse_ts <- function(x) {
  out <- as.POSIXct(x, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  bad <- is.na(out) & !is.na(x) & x != "-" & x != ""
  if (any(bad)) out[bad] <- as.POSIXct(x[bad], tz = "UTC", format = "%Y-%m-%d")
  out
}

# Reads one 10-min Greenbyte file -> data.table(timestamp, <clean cols>)
# and collapses cumulative-export duplicates (pathology 1).
read_greenbyte_10min <- function(f, label) {
  h  <- read_greenbyte_header(f)
  cn <- clean_names(h$names); cn[1] <- "timestamp"
  if (anyDuplicated(cn)) stop("clean_names collision in ", basename(f), ": ",
                              paste(unique(cn[duplicated(cn)]), collapse = ", "))
  x <- fread(f, skip = h$line, header = FALSE, col.names = cn,
             colClasses = list(character = 1), na.strings = c("NaN", "NA", ""),
             showProgress = FALSE)
  n_raw <- nrow(x)
  x[, timestamp := parse_ts(timestamp)]
  if (anyNA(x$timestamp)) stop("unparsed timestamps in ", basename(f))

  keep <- !duplicated(x$timestamp)
  n_filled <- 0L
  if (!all(keep)) {
    kept <- x[keep]
    drop <- x[!keep]
    vcols <- setdiff(names(x), "timestamp")
    pos <- match(drop$timestamp, kept$timestamp)
    for (v in vcols) {
      dv <- drop[[v]]
      has <- which(!is.na(dv))
      if (!length(has)) next
      kv <- kept[[v]][pos[has]]
      fill <- has[is.na(kv)]
      if (length(fill)) {
        set(kept, i = pos[fill], j = v, value = dv[fill])
        n_filled <- n_filled + length(fill)
      }
      conflict <- sum(!is.na(kv) & abs(kv - dv[has]) > 1e-9 * pmax(1, abs(kv)))
      if (conflict) logf("  WARNING %s: %d conflicting duplicate values in %s",
                         label, conflict, v)
    }
    x <- kept
    rm(kept, drop); gc(verbose = FALSE)
  }
  logf("  %-28s raw rows %8d -> unique 10-min rows %6d (dupes dropped %d, gaps filled from dupes %d)",
       label, n_raw, nrow(x), n_raw - nrow(x), n_filled)
  list(data = x, dict = data.table(clean = cn, original = h$names))
}

# Completes a regular 10-min grid between first and last timestamp
complete_grid <- function(x) {
  grid <- data.table(timestamp = seq(min(x$timestamp), max(x$timestamp), by = "10 min"))
  x[, src_row_missing := FALSE]
  x <- x[grid, on = "timestamp"]
  x[is.na(src_row_missing), src_row_missing := TRUE]
  x
}

screen_ranges <- function(x, label) {
  rule <- function(pattern, lo, hi, exclude = NULL) {
    cols <- grep(pattern, names(x), value = TRUE)
    if (!is.null(exclude)) cols <- grep(exclude, cols, value = TRUE, invert = TRUE)
    cols <- cols[!grepl("standard_deviation|stddev|_std_", cols)]
    for (v in cols) {
      bad <- which(!is.na(x[[v]]) & (x[[v]] < lo | x[[v]] > hi))
      if (length(bad)) {
        set(x, i = bad, j = v, value = NA_real_)
        logf("  %s: %-50s %6d values outside [%g, %g] -> NA", label, v, length(bad), lo, hi)
      }
    }
  }
  rule("^wind_speed.*_m_s$|^density_adjusted_wind_speed", 0, 40, exclude = "^long_term")
  rule("^(wind_direction|nacelle_position|yaw_bearing_angle).*_deg$", 0, 360)
  rule("_degc$", -40, 150)
  invisible(x)
}

# ============================================================
# 1. STATIC + SIGNAL MAPPING
# ============================================================
logf("== 1. static data")
static <- fread(file.path(raw_dir, "Kelmarsh_WT_static.csv"), encoding = "UTF-8")
setnames(static, clean_names(names(static)))
static[, turbine := as.integer(sub("Kelmarsh ", "", title))]
static[, commercial_operations_date := as.IDate(commercial_operations_date, format = "%d/%m/%Y")]
setcolorder(static, "turbine")
saveRDS(static, file.path(out_dir, "turbine_static.rds"))
logf("  turbines: %d | rated %s kW | hub heights %s m", nrow(static),
     paste(unique(static$rated_power_kw), collapse = "/"),
     paste(sort(unique(static$hub_height_m)), collapse = "/"))

sig <- fread(file.path(raw_dir, "Kelmarsh_WT_dataSignalMapping.csv"), encoding = "UTF-8")
setnames(sig, clean_names(names(sig)))
saveRDS(sig, file.path(out_dir, "signal_mapping.rds"))
logf("  signal mapping rows: %d", nrow(sig))

# ============================================================
# 2. TURBINE 10-MIN SCADA (one year at a time, all signals)
# ============================================================
core_cols <- c(
  "turbine", "timestamp", "year", "src_row_missing",
  "wind_speed_m_s", "wind_speed_standard_deviation_m_s", "wind_direction_deg",
  "nacelle_position_deg", "density_adjusted_wind_speed_m_s",
  "power_kw", "power_standard_deviation_kw", "power_minimum_kw", "power_maximum_kw",
  "potential_power_default_pc_kw", "turbine_power_setpoint_kw",
  "rotor_speed_rpm", "generator_rpm_rpm",
  "blade_angle_pitch_position_a_deg", "blade_angle_pitch_position_b_deg",
  "blade_angle_pitch_position_c_deg",
  "nacelle_ambient_temperature_degc", "nacelle_temperature_degc",
  "gear_oil_temperature_degc", "front_bearing_temperature_degc",
  "rear_bearing_temperature_degc", "generator_bearing_front_temperature_degc",
  "generator_bearing_rear_temperature_degc", "stator_temperature_1_degc",
  "energy_export_kwh", "energy_theoretical_kwh",
  "lost_production_to_downtime_kwh", "lost_production_to_performance_kwh",
  "lost_production_to_curtailment_total_kwh",
  "time_based_iec_b_2_2_users_view", "time_based_system_avail",
  "data_availability", "capacity_factor")

logf("== 2. turbine SCADA")
scada_files <- list.files(raw_dir, "^Turbine_Data_Kelmarsh_[1-6]_.*\\.csv$",
                          recursive = TRUE, full.names = TRUE)
dict_all <- list(); core_all <- list()
for (yr in years) {
  fy <- scada_files[grepl(sprintf("Kelmarsh_SCADA_%d_", yr), scada_files)]
  if (!length(fy)) { logf("  %d: no files", yr); next }
  yr_list <- list()
  for (f in fy) {
    tb <- as.integer(sub("^Turbine_Data_Kelmarsh_([1-6])_.*", "\\1", basename(f)))
    r  <- read_greenbyte_10min(f, sprintf("%d turbine %d", yr, tb))
    x  <- complete_grid(r$data)
    x[, `:=`(turbine = tb, year = yr)]
    screen_ranges(x, sprintf("%d T%d", yr, tb))
    yr_list[[length(yr_list) + 1]] <- x
    dict_all[[length(dict_all) + 1]] <- r$dict[, year := yr]
    rm(r); gc(verbose = FALSE)
  }
  scada_yr <- rbindlist(yr_list, use.names = TRUE, fill = TRUE); rm(yr_list)
  setcolorder(scada_yr, c("turbine", "timestamp", "year", "src_row_missing"))
  setkey(scada_yr, turbine, timestamp)
  logf("  %d: %d rows | %d grid rows inserted | %d signal cols",
       yr, nrow(scada_yr), sum(scada_yr$src_row_missing), ncol(scada_yr) - 4)
  write_parquet(scada_yr, file.path(scada_dir, sprintf("scada_10min_%d.parquet", yr)), compression = "zstd")
  missing_core <- setdiff(core_cols, names(scada_yr))
  if (length(missing_core)) logf("  %d: core cols absent: %s", yr, paste(missing_core, collapse = ", "))
  core_all[[as.character(yr)]] <- scada_yr[, intersect(core_cols, names(scada_yr)), with = FALSE]
  rm(scada_yr); gc(verbose = FALSE)
}
core <- rbindlist(core_all, use.names = TRUE, fill = TRUE); rm(core_all)
setkey(core, turbine, timestamp)
write_parquet(core, file.path(out_dir, "scada_10min_core.parquet"), compression = "zstd")
logf("  core panel: %d rows x %d cols | %s .. %s", nrow(core), ncol(core),
     format(min(core$timestamp)), format(max(core$timestamp)))
logf("  core NA share: power %.1f%% | wind speed %.1f%%",
     100 * mean(is.na(core$power_kw)), 100 * mean(is.na(core$wind_speed_m_s)))

dict <- rbindlist(dict_all)[, .(years = paste(range(year), collapse = "-")), by = .(clean, original)]
fwrite(dict, file.path(out_dir, "column_dictionary.csv"))

# ============================================================
# 3. TURBINE STATUS / EVENT LOGS
# ============================================================
logf("== 3. turbine events")
parse_duration <- function(d) {
  d[!grepl("^[0-9]+:[0-9]{2}:[0-9]{2}$", d)] <- NA_character_   # "-" = no end
  p <- tstrsplit(d, ":", fixed = TRUE, fill = NA_character_)
  as.numeric(p[[1]]) * 3600 + as.numeric(p[[2]]) * 60 + as.numeric(p[[3]])
}
status_files <- list.files(raw_dir, "^Status_Kelmarsh_[1-6]_.*\\.csv$",
                           recursive = TRUE, full.names = TRUE)
ev <- rbindlist(lapply(status_files, function(f) {
  x <- fread(f, skip = 9, colClasses = "character", encoding = "UTF-8", na.strings = NULL)
  setnames(x, clean_names(names(x)))
  x[, `:=`(turbine   = as.integer(sub("^Status_Kelmarsh_([1-6])_.*", "\\1", basename(f))),
           file_year = as.integer(sub(".*_(20[0-9]{2})-01-0[13]_-_.*", "\\1", basename(f))))]
}), use.names = TRUE, fill = TRUE)
# Repeated rows are KEPT and FLAGGED, not dropped (filter downstream):
#   dup_exact      2nd+ copy of a row identical in every column within
#                  one file (688 rows; all Informational/Warning, mostly
#                  "Wind < start wind", adjacent lines; no Stops)
#   dup_cross_year copy of an event in the later year file when the
#                  event spans New Year and both files list it with the
#                  same start/end (22 events; 4 of them Stops)
# Same start + code but different end (633 "System OK" pairs, one with
# end "-" and one with a real end) are distinct records: not flagged.
ev[, row_in_file := seq_len(.N), by = file_year]
all_cols <- setdiff(names(ev), "row_in_file")
ev[, dup_exact := duplicated(ev, by = all_cols)]
xy_cols <- c("turbine", "timestamp_start", "timestamp_end", "status", "code", "message")
ev[, n_files := uniqueN(file_year), by = xy_cols]
ev[, dup_cross_year := n_files > 1 & file_year > min(file_year), by = xy_cols]
ev[, c("n_files", "row_in_file") := NULL]
logf("  raw rows %d | flagged dup_exact %d | flagged dup_cross_year %d | unflagged %d",
     nrow(ev), sum(ev$dup_exact), sum(ev$dup_cross_year & !ev$dup_exact),
     sum(!ev$dup_exact & !ev$dup_cross_year))

ev[, `:=`(start = parse_ts(timestamp_start),
          end   = parse_ts(timestamp_end),
          code  = as.integer(code))]
ev[, duration_s := parse_duration(duration)]
ev[is.na(end), duration_s := fifelse(status == "Informational", 0, NA_real_)]
ev[, instantaneous := timestamp_end == "-"]
chk <- ev[!is.na(end), sum(abs(as.numeric(end - start, units = "secs") - duration_s) > 1, na.rm = TRUE)]
logf("  duration vs end-start mismatches (>1 s): %d", chk)
logf("  end before start: %d", ev[!is.na(end) & end < start, .N])
for (cc in c("comment", "service_contract_category", "iec_category",
             "global_contract_category", "custom_contract_category"))
  if (cc %in% names(ev)) set(ev, i = which(ev[[cc]] == ""), j = cc, value = NA_character_)
ev[, c("timestamp_start", "timestamp_end", "duration") := NULL]
setcolorder(ev, c("turbine", "start", "end", "duration_s", "instantaneous",
                  "status", "code", "message", "dup_exact", "dup_cross_year"))
setkey(ev, turbine, start)
logf("  by status: %s", ev[, .N, by = status][, paste(status, N, sep = "=", collapse = ", ")])
saveRDS(ev, file.path(out_dir, "turbine_events.rds"))

stops <- ev[status == "Stop"]
logf("  stops: %d (%d flagged as repeats) | open-ended (no end) %d | median %.0f s | IEC: %s",
     nrow(stops), sum(stops$dup_exact | stops$dup_cross_year), sum(is.na(stops$end)), median(stops$duration_s, na.rm = TRUE),
     stops[, .N, by = iec_category][order(-N)][, paste(iec_category, N, sep = "=", collapse = ", ")])
saveRDS(stops, file.path(out_dir, "turbine_stops.rds"))

# ============================================================
# 4. SUBSTATION PMU + GRID METER (10-min) AND THEIR EVENT LOGS
# ============================================================
logf("== 4. PMU / grid meter")
read_device <- function(pattern, label) {
  fs <- list.files(raw_dir, pattern, recursive = TRUE, full.names = TRUE)
  x <- rbindlist(lapply(fs, function(f) read_greenbyte_10min(f, basename(dirname(f)))$data),
                 use.names = TRUE, fill = TRUE)
  x <- unique(x, by = "timestamp")
  x <- complete_grid(x)
  setkey(x, timestamp)
  logf("  %s: %d rows | %s .. %s | %d inserted grid rows", label, nrow(x),
       format(min(x$timestamp)), format(max(x$timestamp)), sum(x$src_row_missing))
  x
}
pmu  <- read_device("^Device_Data_Kelmarsh_PMU_.*\\.csv$", "PMU")
grid <- read_device("^Device_Data_Kelmarsh_Grid_Meter_.*\\.csv$", "grid meter")
write_parquet(pmu,  file.path(out_dir, "pmu_10min.parquet"), compression = "zstd")
write_parquet(grid, file.path(out_dir, "grid_meter_10min.parquet"), compression = "zstd")

dev_ev <- rbindlist(lapply(
  list.files(raw_dir, "^Status_Kelmarsh_(PMU|Grid_Meter)_.*\\.csv$", recursive = TRUE, full.names = TRUE),
  function(f) {
    x <- fread(f, skip = 7, colClasses = "character", encoding = "UTF-8", na.strings = NULL)
    setnames(x, clean_names(names(x)))
    x[, device := sub("^Status_Kelmarsh_(PMU|Grid_Meter)_.*", "\\1", basename(f))]
  }), use.names = TRUE, fill = TRUE)
dev_ev <- unique(dev_ev)
dev_ev[, `:=`(start = parse_ts(timestamp_start), end = parse_ts(timestamp_end),
              duration_s = parse_duration(duration))]
saveRDS(dev_ev, file.path(out_dir, "device_events.rds"))
logf("  device events: %d", nrow(dev_ev))

writeLines(log_lines, file.path(out_dir, "cleaning_log.txt"))
cat("\nDone. Outputs in", out_dir, "\n")
