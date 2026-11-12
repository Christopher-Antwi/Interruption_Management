# ============================================================
# build_kelmarsh_panel.R : clean Kelmarsh outputs -> kelmarsh_panel
#   scada_10min_core.parquet + turbine_events.rds + turbine_static.rds
#   + kelmarsh_stop_classes.csv (the wind "Table A1" rulebook)
#   -> kelmarsh_panel_<width>.parquet   turbine x slot panel
#   -> kelmarsh_episodes.rds            stop episodes from the event log
#   -> kelmarsh_reference_curve.csv     per-turbine reference power curve
#
# ---- THE KNOBS --------------------------------------------------------------
#   PANEL_WIDTH  : the period length (row grain). Any multiple of 10 min:
#                  "10 minutes", "30 minutes", "1 hour", ... "10 hours".
#                  Set it before source() to build another width:
#                    PANEL_WIDTH <- "2 hours"; source("build_kelmarsh_panel.R")
#   BUCKET_WIDTH : concentration-metric grain T_n; raised automatically to
#                  the next whole multiple of PANEL_WIDTH when needed.
# Everything is computed at the native 10 minutes first and then
# aggregated, so every width is built from identical 10-min facts:
#   measurements -> slot means ; energy -> sums
#   stop / Interruption flags -> any() within slot ; stop_share -> mean
#   in-spec flags -> share of valid 10-min intervals in spec, and
#                    <limit>_Pass = 1 when that share >= PASS_SHARE
#   leads / lags -> step = PANEL_WIDTH, within turbine
#
# ---- INTERRUPTIONS (straight from the event log, no SCADA matching) --------
#   1. Every controller "Stop" gets a class from kelmarsh_stop_classes.csv:
#        exogenous: Grid, Environment, Fault (incl. forced outages),
#                   Comm, Requested
#        separate : Manual   (labor / recovery angle)
#        excluded : Routine  (battery & brake tests, cable unwind, flushing)
#   2. Overlapping / touching stops on a turbine chain into one episode;
#      class = first exogenous stop, else Manual, else Routine.
#   3. Episodes starting below CUT_IN wind cost no output: low_wind = TRUE
#      and (DROP_LOW_WIND) kept out of Interruption. Unknown wind kept in.
#   4. A 10-min interval is flagged when an episode overlaps it.
#
# ---- IN-SPEC ("QC") OUTCOMES: three control limits, each on its own ---------
#   Reference curve: per turbine, 0.5 m/s wind bins (IEC 61400-12-1 method
#   of bins), median and SD of power over NORMAL operation = no logged
#   stop, not curtailed (setpoint >= 99% of rated), bins with >= 30 obs.
#     Curve95 : power >= 95% of the reference curve (power-curve warranty
#               convention)
#     Band2SD : |power - reference| <= 2 SD of the bin (statistical
#               control band, the SPC analogue of tablet control limits)
#     Avail97 : IEC time-based availability of the interval >= 0.97
#               (service-contract availability convention)
#   Curve95 / Band2SD are defined when the reference expects > 5% of rated.
#   Perf_Ratio_Ref = power / reference curve (Perf_Ratio uses Greenbyte's
#   default curve, which runs ~7% conservative).
# ============================================================
suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(lubridate)
})

# ---- config ----------------------------------------------------------------
if (!exists("PANEL_WIDTH"))  PANEL_WIDTH  <- "10 minutes"
if (!exists("BUCKET_WIDTH")) BUCKET_WIDTH <- "6 hours"
EPISODE_GAP   <- 0               # seconds; 0 = chain only overlapping/touching stops
CUT_IN        <- 4               # m/s; MM92 cut-in is ~3, 4 avoids edge noise
DROP_LOW_WIND <- TRUE            # keep low-wind stops out of Interruption
EXOG_CLASSES  <- c("Grid", "Environment", "Fault", "Comm", "Requested")
FLAG_CLASSES  <- c(EXOG_CLASSES, "Manual")
N_LEADLAG     <- 6               # leads / lags of the outcomes
PASS_SHARE    <- 0.9             # slot is in spec when >= 90% of its intervals are
CURVE_FRAC    <- 0.95
BAND_SD       <- 2
AVAIL_MIN     <- 0.97

clean_dir  <- "C:/Users/cantwi/Dropbox/Interruption Management/IM Project/Data/Source Data/Wind Farm Data/Clean"
out_dir    <- "C:/Users/cantwi/Dropbox/Interruption Management/IM Project/Data/Source Data/Wind Farm Data/Panel"
class_file <- "C:/Interruption Management/Interruption_Management/IM Project/Data/Regression Panels/Kelmarsh Panel/kelmarsh_stop_classes.csv"
dir.create(out_dir, showWarnings = FALSE)

panel_sec  <- as.numeric(duration(tolower(PANEL_WIDTH)))
bucket_sec <- as.numeric(duration(tolower(BUCKET_WIDTH)))
stopifnot(!is.na(panel_sec), !is.na(bucket_sec), panel_sec %% 600 == 0)
if (bucket_sec %% panel_sec != 0) {
  bucket_sec <- ceiling(bucket_sec / panel_sec) * panel_sec
  message("BUCKET_WIDTH raised to ", bucket_sec / 3600, " hours (whole multiple of ", PANEL_WIDTH, ")")
}
Tn_min <- bucket_sec / 60
floor_to <- function(t, sec) as.POSIXct(floor(as.numeric(t) / sec) * sec,
                                        origin = "1970-01-01", tz = "UTC")

# ---- 1. 10-minute base table ------------------------------------------------
core <- as.data.table(read_parquet(file.path(clean_dir, "scada_10min_core.parquet")))
static <- readRDS(file.path(clean_dir, "turbine_static.rds"))
rated <- setNames(static$rated_power_kw, static$turbine)
setkey(core, turbine, timestamp)

# ---- 2. classify logged stops, chain into episodes --------------------------
classes <- fread(class_file, encoding = "UTF-8")[, .(code, class)]
ev <- readRDS(file.path(clean_dir, "turbine_events.rds"))
stops <- ev[status == "Stop" & dup_cross_year == FALSE & dup_exact == FALSE & !is.na(start)]
rm(ev); invisible(gc(verbose = FALSE))
stops <- merge(stops, classes, by = "code", all.x = TRUE)
unknown <- stops[is.na(class), unique(code)]
if (length(unknown)) {
  message("Codes missing from the rulebook (Fault if IEC Forced outage, else Routine): ",
          paste(unknown, collapse = ", "))
  stops[is.na(class), class := fifelse(iec_category %in% "Forced outage", "Fault", "Routine")]
}
n_open <- sum(is.na(stops$end))
stops[is.na(end), end := start + 600]                # open-ended: one interval
n_neg <- stops[end < start, .N]
stops[end < start, end := start]                     # 1 source row ends before it starts

setorder(stops, turbine, start)
stops[, run_end := cummax(as.numeric(end)), by = turbine]
stops[, episode_id := cumsum(c(TRUE, as.numeric(start[-1]) > head(run_end, -1) + EPISODE_GAP)),
      by = turbine]
stops[, episode_id := .GRP, by = .(turbine, episode_id)]

episodes <- stops[, {
  ex <- which(class %in% EXOG_CLASSES)
  k  <- if (length(ex)) ex[1] else if (any(class == "Manual")) which(class == "Manual")[1] else 1L
  .(ep_start   = min(start),
    ep_end     = as.POSIXct(max(run_end), origin = "1970-01-01", tz = "UTC"),
    n_stops    = .N,
    class      = if (length(ex) || any(class == "Manual")) class[k] else "Routine",
    code       = code[k], message = message[k], iec_category = iec_category[k],
    classes_in_episode = paste(unique(class), collapse = "+"))
}, by = .(turbine, episode_id)]
episodes[, duration_min := as.numeric(difftime(ep_end, ep_start, units = "mins"))]

episodes[, timestamp := floor_to(ep_start, 600)]
episodes <- merge(episodes, core[, .(turbine, timestamp, wind_at_start = wind_speed_m_s)],
                  by = c("turbine", "timestamp"), all.x = TRUE)
episodes[, timestamp := NULL]
episodes[, low_wind := fcoalesce(wind_at_start < CUT_IN, FALSE)]   # unknown wind kept in
episodes[, Interruption := as.integer(class %in% EXOG_CLASSES & !(DROP_LOW_WIND & low_wind))]
for (cl in FLAG_CLASSES) set(episodes, j = paste0("Interruption_", cl),
  value = as.integer(episodes$class == cl & !(DROP_LOW_WIND & episodes$low_wind)))
setcolorder(episodes, c("turbine", "episode_id", "ep_start", "ep_end", "duration_min",
                        "class", "Interruption", "low_wind", "wind_at_start"))
setorder(episodes, turbine, ep_start)

# ---- 3. episode flags at 10 minutes (exact overlap) -------------------------
ep_flags <- c("Interruption", paste0("Interruption_", FLAG_CLASSES))
iv <- episodes[, c("turbine", "episode_id", ep_flags), with = FALSE]
iv[, `:=`(s0 = as.numeric(episodes$ep_start),
          s1 = pmax(as.numeric(episodes$ep_end), as.numeric(episodes$ep_start) + 1))]
gr <- core[, .(turbine, timestamp, s0 = as.numeric(timestamp), s1 = as.numeric(timestamp) + 600)]
setkey(iv, turbine, s0, s1); setkey(gr, turbine, s0, s1)
ov <- foverlaps(gr, iv, type = "any", nomatch = NULL)[i.s0 < s1 & s0 < i.s1]
ov[, cover := pmin(s1, i.s1) - pmax(s0, i.s0)]
f10 <- ov[, c(list(stop10 = 1L, cover10 = min(1, sum(cover) / 600), episode_id = episode_id[1]),
              lapply(.SD, function(z) as.integer(any(z == 1L)))),
          by = .(turbine, timestamp), .SDcols = ep_flags]
rm(ov, gr); invisible(gc(verbose = FALSE))
core <- merge(core, f10, by = c("turbine", "timestamp"), all.x = TRUE)
for (v in c("stop10", ep_flags)) set(core, which(is.na(core[[v]])), v, 0L)
core[is.na(cover10), cover10 := 0]

# ---- 4. reference power curve + in-spec flags at 10 minutes -----------------
core[, rated_kw := rated[as.character(turbine)]]
core[, wbin := floor(wind_speed_m_s / 0.5) * 0.5]
normal <- core[stop10 == 0L & !is.na(power_kw) & !is.na(wbin) &
                 (is.na(turbine_power_setpoint_kw) | turbine_power_setpoint_kw >= 0.99 * rated_kw)]
ref_curve <- normal[, .(ref_kw = median(power_kw), ref_sd = sd(power_kw), n = .N),
                    by = .(turbine, wbin)][n >= 30][order(turbine, wbin)]
rm(normal); invisible(gc(verbose = FALSE))
fwrite(ref_curve, file.path(out_dir, "kelmarsh_reference_curve.csv"))
core <- merge(core, ref_curve[, .(turbine, wbin, ref_kw, ref_sd)],
              by = c("turbine", "wbin"), all.x = TRUE)
core[, ref_ok := !is.na(ref_kw) & ref_kw > 0.05 * rated_kw & !is.na(power_kw)]
core[, `:=`(
  curve95 = fifelse(ref_ok, as.numeric(power_kw >= CURVE_FRAC * ref_kw), NA_real_),
  band2sd = fifelse(ref_ok, as.numeric(abs(power_kw - ref_kw) <= BAND_SD * ref_sd), NA_real_),
  avail97 = as.numeric(time_based_iec_b_2_2_users_view >= AVAIL_MIN),
  perf_ref = fifelse(ref_ok, power_kw / ref_kw, NA_real_))]

# ---- 5. aggregate to the slot ----------------------------------------------
core[, slot := floor_to(timestamp, panel_sec)]
panel <- core[, c(list(
  stop_any      = as.integer(any(stop10 == 1L)),
  stop_share    = mean(cover10),
  episode_id    = episode_id[!is.na(episode_id)][1]),
  lapply(.SD, function(z) as.integer(any(z == 1L))),
  list(
  Power         = mean(power_kw, na.rm = TRUE),
  Power_Min     = min(power_minimum_kw, na.rm = TRUE),
  Power_SD      = mean(power_standard_deviation_kw, na.rm = TRUE),
  Potential     = mean(potential_power_default_pc_kw, na.rm = TRUE),
  Reference     = mean(fifelse(is.na(power_kw), NA_real_, ref_kw), na.rm = TRUE),
  Perf_Ratio_Ref = mean(perf_ref, na.rm = TRUE),
  Curve95_Share = mean(curve95, na.rm = TRUE),
  Band2SD_Share = mean(band2sd, na.rm = TRUE),
  Avail97_Share = mean(avail97, na.rm = TRUE),
  Setpoint      = mean(turbine_power_setpoint_kw, na.rm = TRUE),
  Wind_Speed    = mean(wind_speed_m_s, na.rm = TRUE),
  Wind_Speed_SD = mean(wind_speed_standard_deviation_m_s, na.rm = TRUE),
  Wind_Dir      = mean(wind_direction_deg, na.rm = TRUE),
  Ambient_Temp  = mean(nacelle_ambient_temperature_degc, na.rm = TRUE),
  Rotor_Speed   = mean(rotor_speed_rpm, na.rm = TRUE),
  Pitch_A       = mean(blade_angle_pitch_position_a_deg, na.rm = TRUE),
  Gear_Oil_Temp = mean(gear_oil_temperature_degc, na.rm = TRUE),
  Gen_Bearing_Temp = mean(generator_bearing_front_temperature_degc, na.rm = TRUE),
  Energy_kWh    = sum(energy_export_kwh, na.rm = TRUE),
  Lost_Downtime_kWh = sum(lost_production_to_downtime_kwh, na.rm = TRUE),
  Availability  = mean(time_based_iec_b_2_2_users_view, na.rm = TRUE),
  n_obs         = sum(!is.na(power_kw)))),
  by = .(turbine, slot), .SDcols = ep_flags]
rm(core); invisible(gc(verbose = FALSE))
for (v in names(panel)) if (is.double(panel[[v]]))
  set(panel, which(!is.finite(panel[[v]])), v, NA_real_)

panel[, Perf_Ratio := fifelse(Potential > 0.05 * rated[as.character(turbine)],
                              Power / Potential, NA_real_)]
panel[, Wind_Bin := floor(Wind_Speed)]
for (q in c("Curve95", "Band2SD", "Avail97"))
  set(panel, j = paste0(q, "_Pass"), value = as.integer(panel[[paste0(q, "_Share")]] >= PASS_SHARE))

# ---- 6. concentration metrics at BUCKET_WIDTH (per turbine) -----------------
episodes[, bucket := floor_to(ep_start, bucket_sec)]
bucket_metrics <- episodes[Interruption == 1L, {
  tau <- as.numeric(difftime(ep_start, bucket, units = "mins")) / Tn_min
  mu <- mean(tau); sg <- sqrt(mean((tau - mu)^2))
  .(N_Interruptions = .N, Avg_Interruption_Dur = mean(duration_min),
    Total_Interruption_Dur = sum(duration_min),
    Concentration_Mu = mu, Concentration_Sigma = sg,
    Concentration_CV = if (mu > 0) sg / mu else NA_real_)
}, by = .(turbine, bucket)]

panel[, bucket := floor_to(slot, bucket_sec)]
panel <- merge(panel, bucket_metrics, by = c("turbine", "bucket"), all.x = TRUE)
panel[, `:=`(
  N_Interruptions        = fcoalesce(N_Interruptions, 0L),
  Total_Interruption_Dur = fcoalesce(Total_Interruption_Dur, 0),
  Concentration_C0       = fcoalesce(Concentration_Mu, 0),
  Concentration_Sigma0   = fcoalesce(Concentration_Sigma, 0),
  Concentration_CV0      = fcoalesce(Concentration_CV, 0),
  Date  = as.IDate(slot), Hour = hour(slot), Month = month(slot), Year = year(slot))]
panel[, Treated := as.integer(N_Interruptions > 0)]
panel[, `:=`(Date_FE = Date, Hour_FE = Hour, Turbine_FE = turbine,
             Hub_Height = static$hub_height_m[match(turbine, static$turbine)])]

# ---- 7. leads and lags (step = PANEL_WIDTH, within turbine) -----------------
setkey(panel, turbine, slot)
regular <- panel[, all(diff(as.numeric(slot)) == panel_sec), by = turbine][, all(V1)]
if (!regular) stop("slot grid is not regular within turbine; leads/lags would misalign")
ll_vars <- c("Power", "Perf_Ratio", "Perf_Ratio_Ref", "Power_SD", "Gear_Oil_Temp",
             "Curve95_Pass", "Band2SD_Pass", "Avail97_Pass")
for (k in seq_len(N_LEADLAG)) {
  panel[, paste0(ll_vars, "_lead", k) := lapply(.SD, shift, n = k, type = "lead"),
        by = turbine, .SDcols = ll_vars]
  panel[, paste0(ll_vars, "_lag", k)  := lapply(.SD, shift, n = k, type = "lag"),
        by = turbine, .SDcols = ll_vars]
}
# controls measured at the outcome slot (t+1) and stop state of the next slot
ctl <- c("Wind_Speed", "Wind_Bin", "Wind_Speed_SD", "Ambient_Temp", "stop_any")
panel[, paste0(ctl, "_lead1") := lapply(.SD, shift, n = 1, type = "lead"),
      by = turbine, .SDcols = ctl]
setcolorder(panel, c("turbine", "slot", "bucket", "Date", "Hour", "stop_any", "stop_share",
                     "episode_id", ep_flags))

# ---- save -------------------------------------------------------------------
stamp <- gsub(" ", "", tolower(PANEL_WIDTH))
write_parquet(panel, file.path(out_dir, paste0("kelmarsh_panel_", stamp, ".parquet")),
              compression = "zstd")
if (panel_sec == 600) {
  write_parquet(panel, file.path(out_dir, "kelmarsh_panel.parquet"), compression = "zstd")
  saveRDS(episodes, file.path(out_dir, "kelmarsh_episodes.rds"))
}

# ---- checks -----------------------------------------------------------------
cat("panel:", nrow(panel), "rows x", ncol(panel), "cols at", PANEL_WIDTH,
    "| buckets:", bucket_sec / 3600, "h\n")
cat("logged stops:", nrow(stops), "(open-ended set to 10 min:", n_open, "| end<start fixed:", n_neg,
    ") ->", nrow(episodes), "episodes | exogenous:", sum(episodes$Interruption), "\n")
cat("slots: stop_any", sum(panel$stop_any), "| Interruption", sum(panel$Interruption),
    "| Manual", sum(panel$Interruption_Manual), "| Treated", sum(panel$Treated), "\n")
smp <- panel[!(stop_any == 1 & Interruption == 0)]
cat("RIF-style sample:", nrow(smp), "rows,", sum(smp$Interruption), "interrupted\n")
cat("in-spec rates in sample (%): Curve95", round(100 * mean(smp$Curve95_Pass, na.rm = TRUE), 1),
    "| Band2SD", round(100 * mean(smp$Band2SD_Pass, na.rm = TRUE), 1),
    "| Avail97", round(100 * mean(smp$Avail97_Pass, na.rm = TRUE), 1),
    "| mean Perf_Ratio_Ref", round(mean(smp$Perf_Ratio_Ref, na.rm = TRUE), 3), "\n")
# 10 minutes, verified 2026-11-11: 7,045 stops -> 7,026 episodes, 1,459 exogenous;
#   slots stop_any 103,452 | Interruption 46,546 | Manual 22,082
