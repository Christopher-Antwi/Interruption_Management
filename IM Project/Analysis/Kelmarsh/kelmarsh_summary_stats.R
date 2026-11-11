# =============================================================================
# kelmarsh_summary_stats.R
#
# Kelmarsh wind farm versions of the two CDC-50 descriptive tables:
#   (1) Summary statistics       (CDC: make_table1.R -> sumstats.tex)
#   (2) Duration + reliability   (CDC: summarize_durations_mtbf.R
#                                      -> duration_mtbf_table.tex)
#
# Sample (same rule as the CDC RIF tables): the full turbine x 10-min panel
# less slots stopped for a non-exogenous reason, i.e.
#   filter(!(stop_any == 1 & Interruption == 0))
# so routine, manual and low-wind stop slots are excluded.
#
# Reliability differences from CDC-50, by design:
#   - MTBF is computed WITHIN TURBINE, not within date. Turbines run around
#     the clock, so an uptime spell crossing midnight is real running time.
#     (CDC-50 split by date because the line did not run overnight.)
#   - MTBF / onset-to-onset are reported in HOURS (spells run to days);
#     episode durations and MTTR stay in minutes.
#
# Inputs : Panel/kelmarsh_panel.parquet, Panel/kelmarsh_episodes.rds
# Outputs: kelmarsh_sumstats.tex, kelmarsh_duration_mtbf.tex,
#          kelmarsh_sumstats.csv, kelmarsh_duration_summary.csv,
#          kelmarsh_duration_by_class.csv, kelmarsh_mtbf_summary.csv,
#          kelmarsh_mtbf_by_turbine.csv
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(tibble); library(arrow)
})

# ------------------------------------------------------------------ PATHS ----

ROOT <- local({
  up <- Sys.getenv("USERPROFILE")
  cand <- c(file.path(up, "Dropbox", "Interruption Management", "IM Project"),
            "C:/Dropbox/Interruption Management/IM Project")
  hit <- cand[dir.exists(cand)]
  if (!length(hit)) stop("Could not resolve ROOT. Set it manually.")
  hit[1]
})
PANEL_DIR <- file.path(ROOT, "Data", "Source Data", "Wind Farm Data", "Panel")
OUT       <- file.path(ROOT, "Analysis", "Kelmarsh")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

SLOT_MIN <- 10
EXOG_CLASSES <- c("Grid", "Environment", "Fault", "Comm", "Requested")

# ------------------------------------------------------------------- LOAD ----

panel <- read_parquet(file.path(PANEL_DIR, "kelmarsh_panel.parquet"),
  col_select = c("turbine", "slot", "stop_any", "Interruption",
                 paste0("Interruption_", c(EXOG_CLASSES, "Manual")),
                 "Power", "Rotor_Speed", "Perf_Ratio", "Power_SD", "Gear_Oil_Temp",
                 "Wind_Speed", "Wind_Speed_SD", "Ambient_Temp"))
ep <- readRDS(file.path(PANEL_DIR, "kelmarsh_episodes.rds")) %>%
  as_tibble() %>% arrange(turbine, ep_start)

p <- panel %>% filter(!(stop_any == 1L & Interruption == 0L))
cat("Estimation sample:", format(nrow(p), big.mark = ","), "slots |",
    format(sum(p$Interruption), big.mark = ","), "interrupted\n")

ep_exo <- ep %>% filter(Interruption == 1L)
message("Episodes: ", nrow(ep), " | exogenous: ", nrow(ep_exo))

fmt  <- function(x, d = 2) ifelse(is.na(x), "--", formatC(x, format = "f", digits = d, big.mark = ","))
fmtN <- function(x) formatC(x, format = "d", big.mark = ",")

# =============================================================================
# TABLE 1.  SUMMARY STATISTICS
# =============================================================================

VARS <- tribble(
  ~col,                       ~label,                                   ~dp, ~panel,
  "Interruption",             "Interruption (0/1)",                       3, "Treatment",
  "Interruption_Grid",        "\\quad Grid (0/1)",                        3, "Treatment",
  "Interruption_Environment", "\\quad Environment (0/1)",                 3, "Treatment",
  "Interruption_Fault",       "\\quad Fault (0/1)",                       3, "Treatment",
  "Interruption_Requested",   "\\quad Requested (0/1)",                   3, "Treatment",
  "Power",                    "Active power (kW)",                        1, "Productivity",
  "Rotor_Speed",              "Rotor speed (rpm)",                        2, "Productivity",
  "Perf_Ratio",               "Performance ratio (power / power curve)",  3, "Quality",
  "Power_SD",                 "Within-interval power SD (kW)",            1, "Quality",
  "Gear_Oil_Temp",            "Gear oil temperature ($^\\circ$C)",        1, "Quality",
  "Wind_Speed",               "Wind speed (m/s)",                         2, "Controls",
  "Wind_Speed_SD",            "Wind speed SD (m/s)",                      2, "Controls",
  "Ambient_Temp",             "Ambient temperature ($^\\circ$C)",         1, "Controls"
)

stat_row <- function(v, dp) {
  ok <- v[!is.na(v)]
  tibble(N = length(ok), Mean = mean(ok), SD = sd(ok),
         P10 = quantile(ok, .10, names = FALSE), Median = median(ok),
         P90 = quantile(ok, .90, names = FALSE), Min = min(ok), Max = max(ok))
}

sumstats <- bind_rows(lapply(seq_len(nrow(VARS)), function(i)
  bind_cols(VARS[i, ], stat_row(p[[VARS$col[i]]], VARS$dp[i]))))
dur_row <- bind_cols(
  tibble(col = "duration_min", label = "Interruption duration (min)", dp = 2, panel = "Treatment"),
  stat_row(ep_exo$duration_min, 2))
sumstats <- bind_rows(sumstats[1:5, ], dur_row, sumstats[-(1:5), ])

tex_row <- function(r) sprintf("\\quad %s & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
  r$label, fmtN(r$N), fmt(r$Mean, r$dp), fmt(r$SD, r$dp), fmt(r$P10, r$dp),
  fmt(r$Median, r$dp), fmt(r$P90, r$dp), fmt(r$Min, r$dp), fmt(r$Max, r$dp))

body <- character(0)
for (pn in unique(sumstats$panel)) {
  sub <- sumstats %>% filter(panel == pn)
  if (length(body)) body <- c(body, "\\addlinespace")
  body <- c(body, sprintf("\\multicolumn{9}{l}{\\emph{%s}} \\\\", pn))
  for (i in seq_len(nrow(sub))) body <- c(body, tex_row(sub[i, ]))
}

n_turb  <- n_distinct(p$turbine)
span    <- format(range(as.Date(p$slot)), "%Y-%m-%d")
tex1 <- c(
  "\\begin{table}[htbp]", "\\centering",
  "\\caption{Summary Statistics: Kelmarsh Wind Farm}",
  "\\label{tab:sumstats-kelmarsh}", "\\small",
  "\\begin{tabular}{lrrrrrrrr}", "\\toprule",
  "Variable & $N$ & Mean & S.D. & P10 & Median & P90 & Min & Max \\\\",
  "\\midrule", body, "\\bottomrule", "\\end{tabular}",
  "\\begin{minipage}{\\textwidth}\\footnotesize\\vspace{4pt}",
  paste0("\\emph{Notes.} Turbine $\\times$ 10-minute panel for the ", n_turb,
         " Senvion MM92 turbines at Kelmarsh, ", span[1], " to ", span[2], ". ",
         "The estimation sample of ", fmtN(nrow(p)), " slots is the full panel less ",
         "slots stopped for a non-exogenous reason (routine self-tests, manual stops, ",
         "and stops that began below the 4 m/s cut-in). \\emph{Interruption} equals one ",
         "when the slot overlaps a controller stop classed as Grid, Environment, Fault, ",
         "Comm or Requested in the stop-code rulebook (the wind analogue of Shao, ",
         "Choudhary and Ding (2025), Table A1); the Comm class has a single episode and ",
         "is not shown separately. Interruption duration is reported at the episode level ",
         "($N$ = episodes) and is descriptive only. The performance ratio is defined when ",
         "the power curve expects more than 5\\% of rated output. SCADA signals are ",
         "missing for part of the sample, mostly during grid losses, hence the varying $N$."),
  "\\end{minipage}", "\\end{table}")
writeLines(tex1, file.path(OUT, "kelmarsh_sumstats.tex"))
write_csv(sumstats %>% select(-dp), file.path(OUT, "kelmarsh_sumstats.csv"))

# =============================================================================
# TABLE 2.  DURATION AND RELIABILITY
# =============================================================================

dur_stats <- function(x, label) {
  x <- x[!is.na(x)]
  tibble(Set = label, N = length(x), Mean = mean(x), SD = sd(x), Min = min(x),
         P25 = quantile(x, .25, names = FALSE), Median = median(x),
         P75 = quantile(x, .75, names = FALSE), P90 = quantile(x, .90, names = FALSE),
         Max = max(x), Total = sum(x))
}

med_exo <- median(ep_exo$duration_min)
duration_summary <- bind_rows(
  dur_stats(ep_exo$duration_min, "Exogenous interruptions"),
  dur_stats(ep$duration_min,     "All stop episodes"),
  dur_stats(ep_exo$duration_min[ep_exo$duration_min >  med_exo], "Exogenous, long ($>$ median)"),
  dur_stats(ep_exo$duration_min[ep_exo$duration_min <= med_exo], "Exogenous, short ($\\le$ median)")
)

class_order <- c(EXOG_CLASSES, "Manual", "Routine")
duration_by_class <- bind_rows(lapply(class_order, function(cl) {
  x <- ep %>% filter(class == cl, !(class != "Routine" & low_wind))
  if (!nrow(x)) return(NULL)
  dur_stats(x$duration_min, cl)
})) %>% mutate(Exogenous = Set %in% EXOG_CLASSES)

# ---- reliability: spells within turbine --------------------------------------
running_min <- panel %>% filter(stop_any == 0L) %>% count(turbine, name = "n") %>%
  mutate(running_min = n * SLOT_MIN)

spells <- function(e) {
  e %>% arrange(turbine, ep_start) %>% group_by(turbine) %>%
    mutate(uptime_h      = as.numeric(difftime(ep_start, lag(ep_end),   units = "hours")),
           onset_gap_h   = as.numeric(difftime(ep_start, lag(ep_start), units = "hours"))) %>%
    ungroup()
}
sp_exo <- spells(ep_exo)
sp_all <- spells(ep)
run_total <- sum(running_min$running_min)

reliability <- function(e, sp, run_min, label) {
  up <- sp$uptime_h[!is.na(sp$uptime_h) & sp$uptime_h >= 0]   # overlaps -> not a spell
  og <- sp$onset_gap_h[!is.na(sp$onset_gap_h)]
  mtbf_h <- mean(up); mttr_min <- mean(e$duration_min)
  tibble(Set = label, Failures = nrow(e), `Uptime spells` = length(up),
         `MTBF (h)` = mtbf_h, `MTBF SD (h)` = sd(up), `MTBF median (h)` = median(up),
         `Onset-to-onset (h)` = mean(og),
         `MTTR (min)` = mttr_min, `MTTR median (min)` = median(e$duration_min),
         `Downtime (h)` = sum(e$duration_min) / 60, `Running time (h)` = run_min / 60,
         `Failures per 1,000 h` = nrow(e) / (run_min / 60) * 1000,
         Availability = (mtbf_h * 60) / (mtbf_h * 60 + mttr_min))
}
mtbf_summary <- bind_rows(
  reliability(ep_exo, sp_exo, run_total, "Exogenous interruptions"),
  reliability(ep,     sp_all, run_total, "All stop episodes"))

mtbf_by_turbine <- sp_exo %>% group_by(turbine) %>%
  summarise(Failures = n(), `MTBF (h)` = mean(uptime_h[uptime_h >= 0], na.rm = TRUE),
            `MTTR (min)` = mean(duration_min), Downtime_h = sum(duration_min) / 60,
            .groups = "drop") %>%
  left_join(running_min %>% select(turbine, running_min), by = "turbine") %>%
  mutate(`Failures per 1,000 h` = Failures / (running_min / 60) * 1000,
         Availability = (running_min / 60) / (running_min / 60 + Downtime_h))

write_csv(duration_summary,  file.path(OUT, "kelmarsh_duration_summary.csv"))
write_csv(duration_by_class, file.path(OUT, "kelmarsh_duration_by_class.csv"))
write_csv(mtbf_summary,      file.path(OUT, "kelmarsh_mtbf_summary.csv"))
write_csv(mtbf_by_turbine,   file.path(OUT, "kelmarsh_mtbf_by_turbine.csv"))

drow <- function(d) paste0(d$Set, " & ", fmtN(d$N), " & ", fmt(d$Mean), " & ", fmt(d$SD),
                           " & ", fmt(d$Median), " & ", fmt(d$P90), " & ", fmt(d$Max), " \\\\")
cls_lab <- duration_by_class %>%
  mutate(Set = paste0("\\quad ", Set, ifelse(Exogenous, "", " $^\\dagger$")))
tex2 <- c(
  "\\begin{table}[htbp]", "\\centering",
  "\\caption{Interruption Duration And Reliability Summary Statistics: Kelmarsh Wind Farm}",
  "\\label{tab:duration-mtbf-kelmarsh}", "\\small",
  "\\begin{tabular}{lrrrrrr}", "\\toprule",
  " & N & Mean & SD & Median & P90 & Max \\\\", "\\midrule",
  "\\multicolumn{7}{l}{\\emph{Panel A. Episode duration (minutes)}} \\\\",
  drow(duration_summary),
  "\\midrule",
  "\\multicolumn{7}{l}{\\emph{Panel B. Episode duration by stop class (minutes)}} \\\\",
  drow(cls_lab),
  "\\midrule",
  "\\multicolumn{7}{l}{\\emph{Panel C. Reliability}} \\\\",
  paste0(mtbf_summary$Set, " (MTBF, hours) & ", fmtN(mtbf_summary$Failures), " & ",
         fmt(mtbf_summary$`MTBF (h)`), " & ", fmt(mtbf_summary$`MTBF SD (h)`), " & ",
         fmt(mtbf_summary$`MTBF median (h)`), " & -- & -- \\\\"),
  paste0(mtbf_summary$Set, " (MTTR, minutes) & ", fmtN(mtbf_summary$Failures), " & ",
         fmt(mtbf_summary$`MTTR (min)`), " & -- & ",
         fmt(mtbf_summary$`MTTR median (min)`), " & -- & -- \\\\"),
  "\\bottomrule", "\\end{tabular}",
  "\\begin{minipage}{\\textwidth}\\footnotesize\\vspace{4pt}",
  "\\emph{Notes.} Episodes are overlapping or touching controller stops on one turbine,",
  "taken directly from the turbine event log. Exogenous interruptions are episodes classed",
  "Grid, Environment, Fault, Comm or Requested in the stop-code rulebook (the wind analogue",
  "of Shao, Choudhary and Ding (2025), Table A1), excluding episodes that began below the",
  "4 m/s cut-in. All stop episodes also include manual, routine and low-wind stops.",
  "In Panel B, $^\\dagger$ marks classes outside the treatment; low-wind episodes are",
  "excluded except for Routine. MTBF is the mean uptime spell from the end of one episode",
  "to the start of the next on the same turbine, in hours; it is computed within turbine",
  "rather than within date because turbines operate continuously. MTTR is the mean",
  "episode duration in minutes.",
  "\\end{minipage}", "\\end{table}")
writeLines(tex2, file.path(OUT, "kelmarsh_duration_mtbf.tex"))

# =============================================================================
# CONSOLE
# =============================================================================
options(width = 200)
cat("\n=========== Summary Statistics ===========\n")
print(as.data.frame(sumstats %>% select(label, N, Mean, SD, Median, Min, Max)), digits = 4)
cat("\n=========== Duration Metrics ===========\n"); print(as.data.frame(duration_summary), digits = 4)
cat("\n=========== Duration By Class ===========\n"); print(as.data.frame(duration_by_class), digits = 4)
cat("\n=========== Reliability Metrics ===========\n"); print(as.data.frame(mtbf_summary), digits = 4)
cat("\n=========== By Turbine ===========\n"); print(as.data.frame(mtbf_by_turbine), digits = 4)
message("Saved: ", OUT)
