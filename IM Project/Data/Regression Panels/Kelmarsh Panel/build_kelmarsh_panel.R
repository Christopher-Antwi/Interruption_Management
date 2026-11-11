# ============================================================
# build_kelmarsh_panel.R : clean Kelmarsh outputs -> kelmarsh_panel
#   scada_10min_core.parquet + turbine_events.rds + turbine_static.rds
#   + kelmarsh_stop_classes.csv (the wind "Table A1" rulebook)
#   -> kelmarsh_panel.parquet     turbine x slot panel
#   -> kelmarsh_episodes.rds      stop episodes from the event log
#
# Interruptions come straight from the turbine event log, no SCADA
# matching. CDC-50 needed +-30 min matching because its logbook was
# hand-written and approximate. The Kelmarsh log is written by the
# turbine controller to the second, so a stop maps to its slots by
# exact time overlap. Matching would only lose stops when SCADA is
# blank (most grid losses).
#
#   1. CLASSIFY. Every controller "Stop" gets a class from
#      kelmarsh_stop_classes.csv (edit that file to re-class a code):
#        exogenous: Grid, Environment, Fault (incl. forced outages),
#                   Comm, Requested
#        separate : Manual   (timing chosen; labor / recovery angle)
#        excluded : Routine  (battery & brake tests, cable unwind, oil
#                             flushing: the analogue of CDC-50 pit stops)
#   2. EPISODES. Stops on one turbine that overlap or touch (gap <=
#      EPISODE_GAP) chain into one episode. Its class is the class of
#      its first exogenous stop; else Manual if it has one; else
#      Routine. A routine battery test just before a grid loss therefore
#      cannot hide the grid loss.
#   3. LOW-WIND STOPS. An episode starting while the wind is below CUT_IN
#      cost no output (the turbine would be idle anyway). It is flagged
#      low_wind = TRUE and, with DROP_LOW_WIND, kept out of Interruption.
#      Unknown wind (SCADA blank, typical in grid losses) is kept IN.
#   4. SLOTS. A slot is flagged when an episode overlaps [slot, slot+w):
#        stop_any          any Stop episode (all classes)
#        Interruption      exogenous episode (not low-wind)
#        Interruption_<C>  class split (Grid ~ Interruption_Feeding)
#        Interruption_Manual  manual episode (NOT part of Interruption)
#
#   CDC-50                         Kelmarsh
#   Speed                          Power
#   Weight / Drug_Content          Perf_Ratio (power / power curve)
#   Weight_SD                      Power_SD
#   Press temp / humidity          Wind_Speed, Wind_Speed_SD, Ambient_Temp
#   Date_FE / Hour_FE              Date_FE / Hour_FE / Turbine_FE
# ============================================================
suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(lubridate)
})

# ---- config ----------------------------------------------------------------
PANEL_WIDTH   <- "10 minutes"    # native SCADA grain (multiples of 10 min ok)
BUCKET_WIDTH  <- "6 hours"       # concentration-metric T_n
EPISODE_GAP   <- 0               # seconds; 0 = chain only overlapping/touching stops
CUT_IN        <- 4               # m/s; MM92 cut-in is ~3, 4 avoids edge noise
DROP_LOW_WIND <- TRUE            # keep low-wind stops out of Interruption
EXOG_CLASSES  <- c("Grid", "Environment", "Fault", "Comm", "Requested")
FLAG_CLASSES  <- c(EXOG_CLASSES, "Manual")
N_LEADLAG     <- 6

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

# ---- 1. SCADA -> slot -------------------------------------------------------
core <- as.data.table(read_parquet(file.path(clean_dir, "scada_10min_core.parquet")))
static <- readRDS(file.path(clean_dir, "turbine_static.rds"))
rated <- setNames(static$rated_power_kw, static$turbine)

core[, slot := floor_to(timestamp, panel_sec)]
scada_slot <- core[, .(
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

# ---- 2. classify logged stops with the rulebook -----------------------------
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
stops[is.na(end), end := start + 600]                # open-ended: one slot

# ---- 3. stops -> episodes ---------------------------------------------------
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

# wind when the episode starts (slot containing ep_start)
episodes[, slot := floor_to(ep_start, panel_sec)]
episodes <- merge(episodes, scada_slot[, .(turbine, slot, wind_at_start = Wind_Speed)],
                  by = c("turbine", "slot"), all.x = TRUE)
episodes[, slot := NULL]
episodes[, low_wind := fcoalesce(wind_at_start < CUT_IN, FALSE)]   # unknown wind kept in
episodes[, Interruption := as.integer(class %in% EXOG_CLASSES & !(DROP_LOW_WIND & low_wind))]
for (cl in FLAG_CLASSES) set(episodes, j = paste0("Interruption_", cl),
  value = as.integer(episodes$class == cl & !(DROP_LOW_WIND & episodes$low_wind)))
setcolorder(episodes, c("turbine", "episode_id", "ep_start", "ep_end", "duration_min",
                        "class", "Interruption", "low_wind", "wind_at_start"))
setorder(episodes, turbine, ep_start)

# ---- 4. episodes -> slot flags (exact overlap) ------------------------------
ep_flags <- c("Interruption", paste0("Interruption_", FLAG_CLASSES))
iv <- episodes[, c("turbine", "episode_id", ep_flags), with = FALSE]
iv[, `:=`(s0 = as.numeric(episodes$ep_start),
          s1 = pmax(as.numeric(episodes$ep_end), as.numeric(episodes$ep_start) + 1))]
gr <- scada_slot[, .(turbine, slot, s0 = as.numeric(slot), s1 = as.numeric(slot) + panel_sec)]
setkey(iv, turbine, s0, s1); setkey(gr, turbine, s0, s1)
ov <- foverlaps(gr, iv, type = "any", nomatch = NULL)[i.s0 < s1 & s0 < i.s1]
ov[, cover := pmin(s1, i.s1) - pmax(s0, i.s0)]
flag_slot <- ov[, c(list(stop_any = 1L,
                         stop_share = min(1, sum(cover) / panel_sec),
                         episode_id = episode_id[1]),
                    lapply(.SD, function(z) as.integer(any(z == 1L)))),
                by = .(turbine, slot), .SDcols = ep_flags]

panel <- merge(scada_slot, flag_slot, by = c("turbine", "slot"), all.x = TRUE)
for (v in c("stop_any", ep_flags)) set(panel, which(is.na(panel[[v]])), v, 0L)
panel[is.na(stop_share), stop_share := 0]

# ---- 5. concentration metrics at BUCKET_WIDTH (per turbine) -----------------
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
                     "episode_id", ep_flags))
setkey(panel, turbine, slot)

# ---- save -------------------------------------------------------------------
stamp <- gsub(" ", "", tolower(PANEL_WIDTH))
write_parquet(panel, file.path(out_dir, "kelmarsh_panel.parquet"), compression = "zstd")
write_parquet(panel, file.path(out_dir, paste0("kelmarsh_panel_", stamp, ".parquet")),
              compression = "zstd")
saveRDS(episodes, file.path(out_dir, "kelmarsh_episodes.rds"))
old <- file.path(out_dir, "kelmarsh_log_stops.rds")          # from the matched version
if (file.exists(old)) invisible(file.remove(old))

# ---- checks -----------------------------------------------------------------
cat("panel:", nrow(panel), "rows x", ncol(panel), "cols at", PANEL_WIDTH, "| buckets:", BUCKET_WIDTH, "\n")
cat("logged stops:", nrow(stops), "(open-ended set to 10 min:", n_open, ") ->", nrow(episodes), "episodes\n")
print(episodes[, .(episodes = .N, low_wind = sum(low_wind), in_treatment = sum(Interruption) +
                     sum(Interruption_Manual), median_min = round(median(duration_min), 1),
                   wind_unknown = sum(is.na(wind_at_start))), by = class][order(-episodes)])
cat("slots: stop_any", sum(panel$stop_any), "| Interruption", sum(panel$Interruption),
    "| Manual", sum(panel$Interruption_Manual), "| Treated", sum(panel$Treated), "\n")
cat("treated buckets:", nrow(bucket_metrics), "| multi-event:", bucket_metrics[N_Interruptions > 1, .N], "\n")
smp <- panel[!(stop_any == 1 & Interruption == 0)]
cat("RIF-style sample (drop non-exogenous stop slots):", nrow(smp), "rows,", sum(smp$Interruption),
    "interrupted, of which", smp[Interruption == 1, sum(is.na(Wind_Speed))], "have no wind reading\n")
# DEFAULT (10 min / 6 h / gap 0 / cut-in 4, low-wind dropped) -- 2026-11-11:
#   7,045 logged stops -> 7,026 episodes
#   exogenous episodes in Interruption 1,459 (Fault 614, Environment 394,
#     Grid 374, Requested 76, Comm 1); 459 exogenous low-wind episodes left out
#   Manual episodes 851 (316 low-wind left out); Routine 3,941
#   slots stop_any 103,452 | Interruption 46,546 | Manual 22,082
#   treated buckets 929, 254 multi-event
#   (SCADA-matched version had 816 exogenous episodes / 6,314 slots)
