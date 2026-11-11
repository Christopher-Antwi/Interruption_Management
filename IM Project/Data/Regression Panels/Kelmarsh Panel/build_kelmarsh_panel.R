# ============================================================
# build_kelmarsh_panel.R : clean Kelmarsh outputs -> kelmarsh_panel
#   scada_10min_core.parquet + turbine_stops.rds + turbine_static.rds
#   -> kelmarsh_panel.parquet       (turbine x slot panel)
#   -> kelmarsh_episodes.rds        (stop episodes, one row each)
#
# Mirrors build_base_panel.R / construct_interruptions.R (CDC-50):
#
#   CDC-50 tablet line                 Kelmarsh wind farm
#   ---------------------------------  ---------------------------------
#   slot = 30 s of the one line        slot = turbine x 10 min (native
#                                      SCADA grain), 6 turbines
#   machine stop = key params at 0     stop = controller "Stop" event in
#                                      the turbine event log
#   episode = chained stop seconds     episode = overlapping / touching
#     (2 s gap tolerance)                Stop events chained per turbine
#                                        (EPISODE_GAP tolerance)
#   Interruption = episode matched     Interruption = episode whose
#     to an EXOGENOUS logbook stop       TRIGGERING (first) stop has an
#                                        exogenous IEC category (below)
#   Speed (line rate)                  Power (kW)
#   Weight / Drug_Content (quality)    Perf_Ratio = power / power-curve
#                                        potential; Power_SD (stability)
#   Press temp / humidity controls     Wind_Speed, Wind_Speed_SD
#                                        (turbulence), Ambient_Temp
#   Lot_Change                         (none -- no lot analogue)
#   Date_FE / Hour_FE                  Date_FE / Hour_FE + Turbine_FE
#
# ---- EXOGENOUS STOP CATEGORIES (the key modelling choice) ---------------
# IEC 61400-26 category of the stop that opens the episode:
#   exogenous (Interruption = 1):
#     "Out of Electrical Specification"    grid loss / over-voltage
#     "Out of Environmental Specification" icing, storm, wind direction
#     "Requested Shutdown"                 owner / grid-operator request
#   NOT exogenous (stop_any = 1, Interruption = 0):
#     "Forced outage"          turbine's own fault (endogenous)
#     "Technical Standby"      routine automatic (battery test, cable
#                              unwind, hydraulic flushing)
#     "Scheduled Maintenance"  planned, manual stop on site
# Edit EXOG_IEC to change this; Interruption_Grid flags the grid subset
# (analogue of Interruption_Feeding).
#
# ---- THE KNOBS --------------------------------------------------------------
#   PANEL_WIDTH  : row grain, a multiple of 10 min ("10 minutes", "1 hour")
#   BUCKET_WIDTH : concentration-metric grain T_n (multiple of PANEL_WIDTH)
#   EPISODE_GAP  : stops closer than this (same turbine) chain into one
#                  episode
# ============================================================
suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(lubridate)
})

# ---- config ----------------------------------------------------------------
PANEL_WIDTH  <- "10 minutes"
BUCKET_WIDTH <- "6 hours"
EPISODE_GAP  <- 10 * 60          # seconds
EXOG_IEC     <- c("Out of Electrical Specification",
                  "Out of Environmental Specification",
                  "Requested Shutdown")
GRID_IEC     <- "Out of Electrical Specification"
N_LEADLAG    <- 6                # leads/lags 1..6 (1 hour at 10-min slots)

clean_dir <- "C:/Users/cantwi/Dropbox/Interruption Management/IM Project/Data/Source Data/Wind Farm Data/Clean"
out_dir   <- "C:/Users/cantwi/Dropbox/Interruption Management/IM Project/Data/Source Data/Wind Farm Data/Panel"
dir.create(out_dir, showWarnings = FALSE)

panel_sec  <- as.numeric(duration(tolower(PANEL_WIDTH)))
bucket_sec <- as.numeric(duration(tolower(BUCKET_WIDTH)))
stopifnot(!is.na(panel_sec), !is.na(bucket_sec),
          panel_sec %% 600 == 0, bucket_sec %% panel_sec == 0)
Tn_min <- bucket_sec / 60

# ---- 1. SCADA -> slot -------------------------------------------------------
core <- as.data.table(read_parquet(file.path(clean_dir, "scada_10min_core.parquet")))
static <- readRDS(file.path(clean_dir, "turbine_static.rds"))
rated <- setNames(static$rated_power_kw, static$turbine)

core[, slot := as.POSIXct(floor(as.numeric(timestamp) / panel_sec) * panel_sec,
                          origin = "1970-01-01", tz = "UTC")]
scada_slot <- core[, .(
  Power         = mean(power_kw, na.rm = TRUE),
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
  set(scada_slot, which(is.nan(scada_slot[[v]])), v, NA_real_)

# Perf_Ratio: output relative to the power curve at the measured wind.
# Only defined when the curve expects meaningful output (> 5% of rated),
# so near-cut-in noise does not explode the ratio.
scada_slot[, Perf_Ratio := fifelse(Potential > 0.05 * rated[as.character(turbine)],
                                   Power / Potential, NA_real_)]
scada_slot[, Wind_Bin := floor(Wind_Speed)]          # 1 m/s bins for FE/controls

# ---- 2. stop events -> episodes ---------------------------------------------
stops <- readRDS(file.path(clean_dir, "turbine_stops.rds"))
stops <- stops[!dup_cross_year & !dup_exact & !is.na(start)]
n_open <- sum(is.na(stops$end))
stops[is.na(end), end := start + 600]                # open-ended: 1 slot
setorder(stops, turbine, start)
stops[, run_end := cummax(as.numeric(end)), by = turbine]
stops[, episode_id := cumsum(c(TRUE, as.numeric(start[-1]) >
                                     head(run_end, -1) + EPISODE_GAP)), by = turbine]
stops[, episode_id := .GRP, by = .(turbine, episode_id)]

episodes <- stops[, .(
  ep_start      = min(start),
  ep_end        = as.POSIXct(max(run_end), origin = "1970-01-01", tz = "UTC"),
  n_stops       = .N,
  trigger_code  = code[1],
  trigger_msg   = message[1],
  trigger_iec   = iec_category[1],
  any_exog_iec  = any(iec_category %in% EXOG_IEC)
), by = .(turbine, episode_id)]
episodes[, duration_min := as.numeric(difftime(ep_end, ep_start, units = "mins"))]
episodes[, `:=`(Interruption      = as.integer(trigger_iec %in% EXOG_IEC),
                Interruption_Grid = as.integer(trigger_iec %in% GRID_IEC))]

# ---- 3. episodes -> slot flags (overlap with [slot, slot + width)) ----------
grid <- scada_slot[, .(turbine, slot)]
grid[, `:=`(s0 = as.numeric(slot), s1 = as.numeric(slot) + panel_sec)]
ep_iv <- episodes[, .(turbine, episode_id, Interruption, Interruption_Grid,
                      s0 = as.numeric(ep_start), s1 = as.numeric(ep_end))]
ep_iv[s1 <= s0, s1 := s0 + 1]                        # zero-length stops
setkey(ep_iv, turbine, s0, s1); setkey(grid, turbine, s0, s1)
ov <- foverlaps(grid, ep_iv, type = "any", nomatch = NULL)
ov <- ov[i.s0 < s1 & s0 < i.s1]                      # strict (not touching)
ov[, cover := pmin(s1, i.s1) - pmax(s0, i.s0)]
flag_slot <- ov[, .(
  stop_any          = 1L,
  stop_share        = min(1, sum(cover) / panel_sec),
  Interruption      = as.integer(any(Interruption == 1L)),
  Interruption_Grid = as.integer(any(Interruption_Grid == 1L)),
  episode_id        = min(episode_id)
), by = .(turbine, slot)]

# ---- 4. concentration metrics at BUCKET_WIDTH (per turbine) -----------------
episodes[, bucket := as.POSIXct(floor(as.numeric(ep_start) / bucket_sec) * bucket_sec,
                                origin = "1970-01-01", tz = "UTC")]
bucket_metrics <- episodes[Interruption == 1L, {
  tau <- as.numeric(difftime(ep_start, bucket, units = "mins")) / Tn_min
  mu <- mean(tau); sg <- sqrt(mean((tau - mu)^2))
  .(N_Interruptions = .N, Avg_Interruption_Dur = mean(duration_min),
    Total_Interruption_Dur = sum(duration_min),
    Concentration_Mu = mu, Concentration_Sigma = sg,
    Concentration_CV = if (mu > 0) sg / mu else NA_real_)
}, by = .(turbine, bucket)]

# ---- 5. assemble ------------------------------------------------------------
panel <- merge(scada_slot, flag_slot, by = c("turbine", "slot"), all.x = TRUE)
for (v in c("stop_any", "Interruption", "Interruption_Grid"))
  set(panel, which(is.na(panel[[v]])), v, 0L)
panel[is.na(stop_share), stop_share := 0]
panel[, bucket := as.POSIXct(floor(as.numeric(slot) / bucket_sec) * bucket_sec,
                             origin = "1970-01-01", tz = "UTC")]
panel <- merge(panel, bucket_metrics, by = c("turbine", "bucket"), all.x = TRUE)
panel[, `:=`(
  N_Interruptions        = fcoalesce(N_Interruptions, 0L),
  Total_Interruption_Dur = fcoalesce(Total_Interruption_Dur, 0),
  Treated                = as.integer(fcoalesce(N_Interruptions, 0L) > 0),
  Concentration_C0       = fcoalesce(Concentration_Mu, 0),
  Concentration_Sigma0   = fcoalesce(Concentration_Sigma, 0),
  Concentration_CV0      = fcoalesce(Concentration_CV, 0),
  Date    = as.IDate(slot),
  Hour    = hour(slot),
  Month   = month(slot),
  Year    = year(slot))]
panel[, `:=`(Date_FE = Date, Hour_FE = Hour, Turbine_FE = turbine,
             Hub_Height = static$hub_height_m[match(turbine, static$turbine)])]
setkey(panel, turbine, slot)

# ---- 6. grid-aware leads and lags (step = PANEL_WIDTH, within turbine) -----
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
setcolorder(panel, c("turbine", "slot", "bucket", "Date", "Hour", "stop_any",
                     "stop_share", "Interruption", "Interruption_Grid", "episode_id"))
setkey(panel, turbine, slot)

# ---- save -------------------------------------------------------------------
stamp <- gsub(" ", "", tolower(PANEL_WIDTH))
write_parquet(panel, file.path(out_dir, "kelmarsh_panel.parquet"), compression = "zstd")
write_parquet(panel, file.path(out_dir, paste0("kelmarsh_panel_", stamp, ".parquet")),
              compression = "zstd")
saveRDS(episodes, file.path(out_dir, "kelmarsh_episodes.rds"))

# ---- checks -----------------------------------------------------------------
cat("panel:", nrow(panel), "rows x", ncol(panel), "cols at", PANEL_WIDTH,
    "| buckets:", BUCKET_WIDTH, "\n")
cat("stop events used:", nrow(stops), "(open-ended set to 10 min:", n_open, ")\n")
cat("episodes:", nrow(episodes), "| exogenous (Interruption):", sum(episodes$Interruption),
    "| grid:", sum(episodes$Interruption_Grid),
    "| median dur (min): all", round(median(episodes$duration_min), 1),
    "exog", round(median(episodes[Interruption == 1, duration_min]), 1), "\n")
print(episodes[, .N, by = trigger_iec][order(-N)])
cat("slots stop_any:", sum(panel$stop_any), "| Interruption:", sum(panel$Interruption),
    "| Treated:", sum(panel$Treated), "\n")
cat("multi-event treated buckets:",
    bucket_metrics[N_Interruptions > 1, .N], "of", nrow(bucket_metrics), "\n")
cat("NA: Power", sum(is.na(panel$Power)), "| Perf_Ratio", sum(is.na(panel$Perf_Ratio)),
    "| Power_lead1", sum(is.na(panel$Power_lead1)), "\n")
cat("RIF-style sample (drop non-exogenous stop slots):",
    panel[!(stop_any == 1 & Interruption == 0) & !is.na(Wind_Speed), .N], "rows,",
    panel[!(stop_any == 1 & Interruption == 0) & !is.na(Wind_Speed), sum(Interruption)],
    "interrupted\n")
cat("power by state (mean kW): running", round(panel[stop_any == 0, mean(Power, na.rm = TRUE)]),
    "| in exog stop", round(panel[Interruption == 1, mean(Power, na.rm = TRUE)]), "\n")
# DEFAULT (10 minutes / 6 hours / 10-min chaining) -- verified 2026-11-11:
#   nrow 2,839,104 x 92 cols ; stop events 7,045 -> 5,681 episodes
#   episodes exogenous 567 (grid 366, environment 101, requested 100)
#   median episode 9.3 min (exogenous 13.4) ; trigger IEC NA: 16
#   slots stop_any 103,452 | Interruption 41,254 | Treated 19,188
#   treated buckets 533, only 28 multi-event -> Sigma/CV vary little
#   RIF-style sample (drop non-exog stop slots, need Wind_Speed):
#     2,728,427 rows, 21,564 interrupted (about half of the interrupted
#     slots have no SCADA wind reading, mostly during grid losses)
#   mean Power: running 605 kW | inside exogenous stop 6 kW
