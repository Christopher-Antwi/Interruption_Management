# ============================================================
# build_kelmarsh_panel.R : clean Kelmarsh outputs -> kelmarsh_panel
#   scada_10min_core.parquet + turbine_events.rds + turbine_static.rds
#   + kelmarsh_stop_classes.csv (the wind "Table A1" rulebook)
#   -> kelmarsh_panel.parquet     turbine x slot panel
#   -> kelmarsh_episodes.rds      machine-side stop episodes + log match
#   -> kelmarsh_log_stops.rds     classified log stops + match result
#
# Same two-source logic as construct_interruptions.R (CDC-50):
#
#   1. MACHINE SIDE. A slot is stopped when the turbine produced nothing
#      at some point in the interval although the wind could drive it:
#        power_minimum_kw <= 0  AND  wind_speed_m_s >= CUT_IN
#      (CDC-50: key flows / shoe speed == 0). Consecutive stopped slots
#      of one turbine = one machine episode. NA power counts as NOT
#      stopped, as in CDC-50 (coalesce(.x == 0, FALSE)).
#   2. LOGBOOK SIDE. Every controller "Stop" in the event log gets a
#      class from kelmarsh_stop_classes.csv:
#        exogenous: Grid, Environment, Fault, Comm, Requested
#        separate : Manual   (timing chosen; labor / recovery angle)
#        excluded : Routine  (battery & brake tests, cable unwind ...
#                             the analogue of CDC-50 pit stops)
#   3. MATCH. Log stops are taken in time order; each is matched to the
#      earliest still-unmatched machine episode of the same turbine
#      whose start is within +-MATCH_WIN of the log start (CDC-50:
#      +-30 min, sequential, non-overwriting). The episode inherits
#      the log stop's class.
#        Interruption        = 1 : episode matched to an exogenous stop
#        Interruption_<Class>    : the class split (Grid ~ Feeding role)
#        Interruption_Manual     : matched to a manual stop (NOT in
#                                  Interruption)
#
#   CDC-50                         Kelmarsh
#   Speed                          Power
#   Weight / Drug_Content          Perf_Ratio (power / power curve)
#   Weight_SD                      Power_SD
#   Press temp / humidity          Wind_Speed, Wind_Speed_SD, Ambient_Temp
#   Date_FE / Hour_FE              Date_FE / Hour_FE / Turbine_FE
#
# Robustness layer: log_stop_any / log_Interruption flag slots straight
# from the log (no SCADA confirmation). They matter because SCADA is
# often missing during grid losses, which the machine side cannot see.
# ============================================================
suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(lubridate)
})

# ---- config ----------------------------------------------------------------
PANEL_WIDTH  <- "10 minutes"     # native SCADA grain (multiples of 10 min ok)
BUCKET_WIDTH <- "6 hours"        # concentration-metric T_n
CUT_IN       <- 4                # m/s; MM92 cut-in is ~3, 4 avoids edge noise
MATCH_WIN    <- 20 * 60          # seconds; log times are exact, slots 10 min
EXOG_CLASSES <- c("Grid", "Environment", "Fault", "Comm", "Requested")
FLAG_CLASSES <- c(EXOG_CLASSES, "Manual")
N_LEADLAG    <- 6

clean_dir  <- "C:/Users/cantwi/Dropbox/Interruption Management/IM Project/Data/Source Data/Wind Farm Data/Clean"
out_dir    <- "C:/Users/cantwi/Dropbox/Interruption Management/IM Project/Data/Source Data/Wind Farm Data/Panel"
class_file <- "C:/Interruption Management/Interruption_Management/IM Project/Data/Regression Panels/Kelmarsh Panel/kelmarsh_stop_classes.csv"
dir.create(out_dir, showWarnings = FALSE)

panel_sec  <- as.numeric(duration(tolower(PANEL_WIDTH)))
bucket_sec <- as.numeric(duration(tolower(BUCKET_WIDTH)))
stopifnot(!is.na(panel_sec), !is.na(bucket_sec),
          panel_sec %% 600 == 0, bucket_sec %% panel_sec == 0)
Tn_min <- bucket_sec / 60
floor_to <- function(t, sec) as.POSIXct(floor(as.numeric(t) / sec) * sec,
                                        origin = "1970-01-01", tz = "UTC")

# ---- 1. SCADA: machine-side stop flag at 10 min, then slot means ------------
core <- as.data.table(read_parquet(file.path(clean_dir, "scada_10min_core.parquet")))
static <- readRDS(file.path(clean_dir, "turbine_static.rds"))
rated <- setNames(static$rated_power_kw, static$turbine)

core[, m_stop := fcoalesce(power_minimum_kw <= 0 & wind_speed_m_s >= CUT_IN, FALSE)]
setkey(core, turbine, timestamp)
core[, m_ep := { r <- rleid(m_stop); fifelse(m_stop, r, NA_integer_) }, by = turbine]
core[!is.na(m_ep), m_ep := .GRP, by = .(turbine, m_ep)]

episodes <- core[!is.na(m_ep), .(
  ep_start     = min(timestamp),
  ep_end       = max(timestamp) + 600,             # end of last stopped slot
  n_slots      = .N,
  wind_at_stop = mean(wind_speed_m_s, na.rm = TRUE)
), by = .(turbine, episode_id = m_ep)]
episodes[, duration_min := as.numeric(difftime(ep_end, ep_start, units = "mins"))]

core[, slot := floor_to(timestamp, panel_sec)]
scada_slot <- core[, .(
  stop_any      = as.integer(any(m_stop)),
  stop_share    = mean(m_stop),
  episode_id    = if (all(is.na(m_ep))) NA_integer_ else min(m_ep, na.rm = TRUE),
  Power         = mean(power_kw, na.rm = TRUE),
  Power_Min     = min(power_minimum_kw, na.rm = TRUE),
  Power_SD      = mean(power_standard_deviation_kw, na.rm = TRUE),
  Potential     = mean(potential_power_default_pc_kw, na.rm = TRUE),
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
  n_obs         = sum(!is.na(power_kw))
), by = .(turbine, slot)]
rm(core); invisible(gc(verbose = FALSE))
for (v in names(scada_slot)) if (is.double(scada_slot[[v]]))
  set(scada_slot, which(!is.finite(scada_slot[[v]])), v, NA_real_)
scada_slot[, Perf_Ratio := fifelse(Potential > 0.05 * rated[as.character(turbine)],
                                   Power / Potential, NA_real_)]
scada_slot[, Wind_Bin := floor(Wind_Speed)]

# ---- 2. logbook: classify every Stop with the rulebook ---------------------
classes <- fread(class_file, encoding = "UTF-8")[, .(code, class)]
ev <- readRDS(file.path(clean_dir, "turbine_events.rds"))
log_stops <- ev[status == "Stop" & dup_cross_year == FALSE & dup_exact == FALSE & !is.na(start)]
rm(ev); invisible(gc(verbose = FALSE))
log_stops <- merge(log_stops, classes, by = "code", all.x = TRUE)
unknown <- log_stops[is.na(class), unique(code)]
if (length(unknown)) {
  message("Codes missing from the rulebook (classed Fault if IEC Forced outage, else Unclassified): ",
          paste(unknown, collapse = ", "))
  log_stops[is.na(class), class := fifelse(iec_category %in% "Forced outage", "Fault", "Unclassified")]
}
log_stops[, exogenous := class %in% EXOG_CLASSES]
log_stops[is.na(end), end := start + 600]
setorder(log_stops, start)
log_stops[, log_id := .I]

# ---- 3. sequential +-MATCH_WIN matching (non-overwriting) -------------------
episodes[, `:=`(matched_log_id = NA_integer_, class = NA_character_)]
log_stops[, matched_episode := NA_integer_]
to_match <- log_stops[class %in% FLAG_CLASSES]
for (tb in unique(episodes$turbine)) {
  e_idx <- which(episodes$turbine == tb)
  e_st  <- as.numeric(episodes$ep_start[e_idx])
  free  <- rep(TRUE, length(e_idx))
  L <- to_match[turbine == tb]
  for (i in seq_len(nrow(L))) {
    cand <- which(free & abs(e_st - as.numeric(L$start[i])) <= MATCH_WIN)
    if (length(cand)) {
      j <- cand[1]; free[j] <- FALSE
      set(episodes, e_idx[j], c("matched_log_id", "class"), list(L$log_id[i], L$class[i]))
      set(log_stops, L$log_id[i], "matched_episode", episodes$episode_id[e_idx[j]])
    }
  }
}
episodes <- merge(episodes,
                  log_stops[, .(matched_log_id = log_id, log_start = start, log_end = end,
                                log_code = code, log_message = message, log_iec = iec_category)],
                  by = "matched_log_id", all.x = TRUE)
episodes[, Interruption := as.integer(class %in% EXOG_CLASSES)]
for (cl in FLAG_CLASSES) set(episodes, j = paste0("Interruption_", cl),
                             value = as.integer(episodes$class %in% cl))
setcolorder(episodes, c("turbine", "episode_id", "ep_start", "ep_end", "duration_min",
                        "n_slots", "class", "Interruption"))
setorder(episodes, turbine, ep_start)

# ---- 4. slot flags: matched (two-source) and log-only (robustness) ----------
ep_flags <- c("Interruption", paste0("Interruption_", FLAG_CLASSES))
panel <- merge(scada_slot, episodes[, c("turbine", "episode_id", ep_flags), with = FALSE],
               by = c("turbine", "episode_id"), all.x = TRUE)
for (v in ep_flags) set(panel, which(is.na(panel[[v]])), v, 0L)

# log-only: slot overlaps any Stop (non-routine) / an exogenous Stop in the log
lg <- log_stops[class != "Routine", .(turbine, exogenous, s0 = as.numeric(start),
                                      s1 = pmax(as.numeric(end), as.numeric(start) + 1))]
gr <- panel[, .(turbine, slot, s0 = as.numeric(slot), s1 = as.numeric(slot) + panel_sec)]
setkey(lg, turbine, s0, s1); setkey(gr, turbine, s0, s1)
ov <- foverlaps(gr, lg, type = "any", nomatch = NULL)[i.s0 < s1 & s0 < i.s1]
log_flags <- ov[, .(log_stop_any = 1L, log_Interruption = as.integer(any(exogenous))),
                by = .(turbine, slot)]
panel <- merge(panel, log_flags, by = c("turbine", "slot"), all.x = TRUE)
panel[is.na(log_stop_any), `:=`(log_stop_any = 0L, log_Interruption = 0L)]

# ---- 5. concentration metrics at BUCKET_WIDTH (matched exogenous episodes) --
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

# ---- 6. grid-aware leads and lags (within turbine) -------------------------
ll_vars <- c("Power", "Perf_Ratio", "Power_SD", "Gear_Oil_Temp")
key_dt  <- panel[, c("turbine", "slot", ll_vars), with = FALSE]
for (k in seq_len(N_LEADLAG)) {
  lead_k <- copy(key_dt)[, slot := slot - panel_sec * k]
  setnames(lead_k, ll_vars, paste0(ll_vars, "_lead", k))
  lag_k  <- copy(key_dt)[, slot := slot + panel_sec * k]
  setnames(lag_k, ll_vars, paste0(ll_vars, "_lag", k))
  panel <- lead_k[panel, on = .(turbine, slot)]
  panel <- lag_k[panel, on = .(turbine, slot)]
}
setcolorder(panel, c("turbine", "slot", "bucket", "Date", "Hour", "stop_any", "stop_share",
                     "episode_id", ep_flags, "log_stop_any", "log_Interruption"))
setkey(panel, turbine, slot)

# ---- save -------------------------------------------------------------------
stamp <- gsub(" ", "", tolower(PANEL_WIDTH))
write_parquet(panel, file.path(out_dir, "kelmarsh_panel.parquet"), compression = "zstd")
write_parquet(panel, file.path(out_dir, paste0("kelmarsh_panel_", stamp, ".parquet")),
              compression = "zstd")
saveRDS(episodes,  file.path(out_dir, "kelmarsh_episodes.rds"))
saveRDS(log_stops, file.path(out_dir, "kelmarsh_log_stops.rds"))

# ---- checks -----------------------------------------------------------------
cat("panel:", nrow(panel), "rows x", ncol(panel), "cols at", PANEL_WIDTH, "| buckets:", BUCKET_WIDTH, "\n")
cat("machine episodes:", nrow(episodes), "| median", median(episodes$duration_min), "min",
    "| matched to log:", sum(!is.na(episodes$class)), "\n")
cat("matched episodes by class:\n"); print(episodes[!is.na(class), .N, by = class][order(-N)])
cat("log stops by class, share matched to a machine episode:\n")
print(log_stops[, .(stops = .N, matched = sum(!is.na(matched_episode)),
                    pct = round(100 * mean(!is.na(matched_episode)))), by = class][order(-stops)])
cat("slots: stop_any", sum(panel$stop_any), "| Interruption", sum(panel$Interruption),
    "| Manual", sum(panel$Interruption_Manual), "| log_Interruption", sum(panel$log_Interruption),
    "| Treated", sum(panel$Treated), "\n")
cat("treated buckets:", nrow(bucket_metrics), "| multi-event:", bucket_metrics[N_Interruptions > 1, .N], "\n")
cat("RIF-style sample:", panel[!(stop_any == 1 & Interruption == 0) & !is.na(Wind_Speed), .N],
    "rows,", panel[!(stop_any == 1 & Interruption == 0) & !is.na(Wind_Speed), sum(Interruption)], "interrupted\n")
