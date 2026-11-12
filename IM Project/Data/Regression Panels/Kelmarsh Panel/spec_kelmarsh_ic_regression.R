# ============================================================
# spec_kelmarsh_ic_regression.R : Shao Eqs 1-4 on the Kelmarsh panel,
#   repeated at every period length (10 minutes ... 10 hours)
#
# Mirrors spec_ic_regression.R (CDC-50):
#   Treatments : Interruption          slot overlaps an exogenous stop
#                Concentration_C0      bucket concentration dose (0-1)
#                Interruption_Grid     robustness (the Feeding analogue)
#   Outcomes at t+1 (the next period):
#     Eq1  Power_lead1               rate          (CDC: Speed)
#     Eq2  Perf_Ratio_Ref_lead1      quality level (CDC: Weight)
#     Eq3  Power_SD_lead1            stability     (CDC: Drug_Content)
#     Eq4a logit Curve95_Pass_lead1  power >= 95% of reference curve
#     Eq4b logit Band2SD_Pass_lead1  power within 2 SD of reference curve
#     Eq4c logit Avail97_Pass_lead1  IEC availability >= 0.97
#   Controls : wind at the OUTCOME period, as 1 m/s bin fixed effects
#              (Wind_Bin_lead1), plus Ambient_Temp_lead1. Wind is the
#              turbine's input and differs sharply by stop class
#              (Environment stops begin at ~15 m/s vs 6 m/s normal).
#   FE   : Spec A = Turbine + Date + wind bin | Spec B = A + Hour
#   SEs  : every model is reported four ways --
#            HC1   heteroskedasticity-robust (what CDC-50 used)
#            Date  clustered by date (6 turbines share each day's weather)
#            NW    Newey-West, serial correlation within turbine
#            DK    Driscoll-Kraay, serial + cross-turbine correlation
#   Sample: interrupted slots kept; slots stopped for a non-exogenous
#           reason dropped; wind at t+1 must be observed.
#
# Output (OUT): kelmarsh_ic_results.csv / .rds, one LaTeX table per
#               treatment (rows = period length), console tables.
# ============================================================
suppressPackageStartupMessages({
  library(data.table); library(arrow); library(fixest)
})
setFixest_nthreads(0)

if (!exists("WIDTHS"))
  WIDTHS <- c("10 minutes", "30 minutes", "1 hour", "2 hours", "3 hours", "6 hours", "10 hours")
SPECS_TO_RUN <- c("A", "B")

ROOT <- file.path(Sys.getenv("USERPROFILE"), "Dropbox", "Interruption Management", "IM Project")
PANEL_DIR <- file.path(ROOT, "Data", "Source Data", "Wind Farm Data", "Panel")
OUT       <- file.path(ROOT, "Data", "Regression Panels", "Kelmarsh Panel", "Results")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

treatments <- c("Interruption", "Concentration_C0", "Interruption_Grid")
ols_eqs    <- c(Eq1 = "Power_lead1", Eq2 = "Perf_Ratio_Ref_lead1", Eq3 = "Power_SD_lead1")
logit_eqs  <- c(Eq4a = "Curve95_Pass_lead1", Eq4b = "Band2SD_Pass_lead1", Eq4c = "Avail97_Pass_lead1")
ctrl <- "Ambient_Temp_lead1"
fe   <- c(A = "Turbine_FE + Date_FE + Wind_Bin_lead1",
          B = "Turbine_FE + Date_FE + Wind_Bin_lead1 + Hour_FE")
SAMPLES <- c("all", "running")
need <- c("turbine", "slot", "stop_any", "stop_any_lead1", treatments, ols_eqs, logit_eqs,
          "Ambient_Temp_lead1", "Wind_Bin_lead1", "Turbine_FE", "Date_FE", "Hour_FE")

vcovs <- list(HC1 = "hetero", Date = ~Date_FE,
              NW = NW ~ turbine + slot_idx, DK = DK ~ slot_idx)

one_row <- function(m, tv, extra) {
  est <- unname(coef(m)[tv])
  if (is.na(est)) return(NULL)
  ses <- sapply(names(vcovs), function(v)
    tryCatch(unname(se(summary(m, vcov = vcovs[[v]]))[tv]), error = function(e) NA_real_))
  cbind(extra, data.table(estimate = est, se_HC1 = ses["HC1"], se_Date = ses["Date"],
                          se_NW = ses["NW"], se_DK = ses["DK"], n = nobs(m)))
}

results <- list()
for (w in WIDTHS) {
  f <- file.path(PANEL_DIR, paste0("kelmarsh_panel_", gsub(" ", "", w), ".parquet"))
  if (!file.exists(f)) { message("missing panel for ", w, " -- skipped"); next }
  panel_sec <- as.numeric(lubridate::duration(w))
  d <- as.data.table(read_parquet(f, col_select = all_of(need)))
  d <- d[!(stop_any == 1L & Interruption == 0L) & !is.na(Wind_Bin_lead1) & !is.na(Ambient_Temp_lead1)]
  d[, slot_idx := as.integer(as.numeric(slot) / panel_sec)]
  cat(sprintf("\n===== %s : %s slots | %s interrupted =====\n", w,
              format(nrow(d), big.mark = ","), format(sum(d$Interruption), big.mark = ",")))

  # Two samples:
  #   all     : faithful to CDC-50 -- the next period may still be inside the stop,
  #             so the estimate mixes downtime carry-over with recovery
  #   running : next period has no logged stop at all -- the estimate is the
  #             effect on a turbine that is back up (pure recovery)
  for (smp in SAMPLES) {
    dd <- if (smp == "running") d[stop_any_lead1 == 0L] else d
    cat(sprintf("  sample %-8s: %s slots | %s interrupted\n", smp,
                format(nrow(dd), big.mark = ","), format(sum(dd$Interruption), big.mark = ",")))
    for (tv in treatments) for (sp in SPECS_TO_RUN) {
      for (eq in names(ols_eqs)) {
        m <- tryCatch(feols(as.formula(paste0(ols_eqs[eq], " ~ ", tv, " + ", ctrl, " | ", fe[sp])),
                            data = dd, vcov = "hetero", panel.id = ~turbine + slot_idx, notes = FALSE),
                      error = function(e) NULL)
        if (!is.null(m)) results[[length(results) + 1]] <- one_row(m, tv, data.table(
          width = w, hours = panel_sec / 3600, sample = smp, treatment = tv, spec = sp, eq = eq,
          outcome = ols_eqs[eq], model = "OLS", dv_mean = mean(dd[[ols_eqs[eq]]], na.rm = TRUE)))
      }
      for (eq in names(logit_eqs)) {
        if (smp == "running" && eq == "Eq4c") next      # availability is ~1 by construction
        m <- tryCatch(feglm(as.formula(paste0(logit_eqs[eq], " ~ ", tv, " + ", ctrl, " | ", fe[sp])),
                            data = dd, family = binomial("logit"), vcov = "hetero",
                            panel.id = ~turbine + slot_idx, notes = FALSE),
                      error = function(e) NULL)
        if (!is.null(m)) results[[length(results) + 1]] <- one_row(m, tv, data.table(
          width = w, hours = panel_sec / 3600, sample = smp, treatment = tv, spec = sp, eq = eq,
          outcome = logit_eqs[eq], model = "Logit", dv_mean = mean(dd[[logit_eqs[eq]]], na.rm = TRUE)))
      }
    }
    cat("    done\n")
  }
  rm(d, dd); invisible(gc())
}

res <- rbindlist(results, fill = TRUE)
for (v in c("HC1", "Date", "NW", "DK"))
  set(res, j = paste0("p_", v), value = 2 * pnorm(-abs(res$estimate / res[[paste0("se_", v)]])))
fwrite(res, file.path(OUT, "kelmarsh_ic_results.csv"))
saveRDS(res, file.path(OUT, "kelmarsh_ic_results.rds"))

# ---- console: coefficient by period length (Spec B, Date-clustered) ---------
options(width = 220)
stars <- function(p) fifelse(is.na(p), "", fifelse(p < .01, "***", fifelse(p < .05, "**", fifelse(p < .1, "*", ""))))
res[, cell := sprintf("%s%s (%s)", formatC(estimate, format = "f", digits = 3), stars(p_Date),
                      formatC(se_Date, format = "f", digits = 3))]
for (smp in SAMPLES) for (tv in treatments) {
  cat("\n=====", tv, "| sample:", smp, "| Spec B | estimate (Date-clustered SE) =====\n")
  print(dcast(res[treatment == tv & spec == "B" & sample == smp], hours + width ~ eq,
              value.var = "cell")[order(hours)][, -"hours"])
}
cat("\n===== SE comparison: Interruption, Spec B, Eq1 (Power t+1), sample = all =====\n")
print(res[treatment == "Interruption" & spec == "B" & eq == "Eq1" & sample == "all",
          .(width, estimate = round(estimate, 2), se_HC1 = round(se_HC1, 2), se_Date = round(se_Date, 2),
            se_NW = round(se_NW, 2), se_DK = round(se_DK, 2), n)])

# ---- LaTeX: one table per treatment x sample, rows = period length (Spec B) --
hdr <- c(Eq1 = "Power$_{t+1}$", Eq2 = "Perf.\\ ratio$_{t+1}$", Eq3 = "Power SD$_{t+1}$",
         Eq4a = "Curve95$_{t+1}$", Eq4b = "Band2SD$_{t+1}$", Eq4c = "Avail97$_{t+1}$")
tex_label <- c(Interruption = "Interruption", Concentration_C0 = "Concentration ($C_0$)",
               Interruption_Grid = "Grid interruption")
smp_note <- c(all = "The sample keeps every next period, including those still inside the stop.",
              running = paste("The sample is restricted to next periods with no logged stop, so the",
                              "estimates describe a turbine that is running again; Avail97 is omitted",
                              "because availability is then one by construction."))
for (smp in SAMPLES) for (tv in treatments) {
  x <- res[treatment == tv & spec == "B" & sample == smp][order(hours)]
  body <- character(0)
  for (w in unique(x$width)) {
    xw <- x[width == w]
    g <- function(eq_name, what) {
      r <- xw[xw$eq == eq_name]
      if (!nrow(r)) return(if (what == "est") "--" else "")
      if (what == "est") paste0(formatC(r$estimate, format = "f", digits = 3, big.mark = ","), "$^{", stars(r$p_Date), "}$")
      else paste0("(", formatC(r$se_Date, format = "f", digits = 3, big.mark = ","), ")")
    }
    body <- c(body,
      paste0(w, " & ", paste(sapply(names(hdr), g, what = "est"), collapse = " & "),
             " & ", formatC(max(xw$n), format = "d", big.mark = ","), " \\\\"),
      paste0(" & ", paste(sapply(names(hdr), g, what = "se"), collapse = " & "), " & \\\\"))
  }
  tex <- c("\\begin{table}[htbp]", "\\centering",
    sprintf("\\caption{Effect Of %s On Next-Period Outcomes By Period Length: Kelmarsh (%s)}",
            tex_label[tv], if (smp == "all") "All Next Periods" else "Next Period Running"),
    sprintf("\\label{tab:kelmarsh-ic-%s-%s}", tolower(gsub("_", "-", tv)), smp), "\\small",
    "\\begin{tabular}{lrrrrrrr}", "\\toprule",
    " & \\multicolumn{3}{c}{OLS} & \\multicolumn{3}{c}{Logit (in spec)} & \\\\",
    "\\cmidrule(lr){2-4}\\cmidrule(lr){5-7}",
    paste0("Period length & ", paste(hdr, collapse = " & "), " & $N$ \\\\"), "\\midrule",
    body, "\\bottomrule", "\\end{tabular}",
    "\\begin{minipage}{\\textwidth}\\footnotesize\\vspace{4pt}",
    paste0("\\emph{Notes.} Each cell is the coefficient on ", tex_label[tv], " from a separate regression of the ",
           "next-period outcome, estimated on a turbine $\\times$ period panel of the stated length. ",
           smp_note[smp], " ",
           "All models include turbine, date, hour-of-day and 1 m/s wind-speed-bin fixed effects (wind measured ",
           "in the outcome period) and ambient temperature. Standard errors clustered by date in parentheses. ",
           "Curve95: power at least 95\\% of the turbine's reference power curve; Band2SD: power within two ",
           "standard deviations of the reference curve; Avail97: IEC time-based availability of at least 0.97. ",
           "$N$ is the largest estimation sample in the row. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."),
    "\\end{minipage}", "\\end{table}")
  writeLines(tex, file.path(OUT, paste0("kelmarsh_ic_", tolower(tv), "_", smp, ".tex")))
}
cat("\nSaved to", OUT, "\n")
