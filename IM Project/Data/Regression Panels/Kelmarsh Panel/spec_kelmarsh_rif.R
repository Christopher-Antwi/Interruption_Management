# ============================================================
# spec_kelmarsh_rif.R : FFL (2009) RIF regressions on the Kelmarsh
#   panel, repeated at every period length (10 minutes ... 10 hours)
#
# Mirrors spec_interruption_rif.R (CDC-50):
#   Treatments: Interruption (slot flag) and Concentration_C0 (0-1 dose)
#   Outcomes  : Power_lead1, Perf_Ratio_Ref_lead1, Power_SD_lead1
#   Statistics: Q10, Q50, Q90, Variance (each its own RIF-OLS model)
#   Density   : Gaussian kernel, TWO bandwidths side by side --
#                 SJ   Sheather-Jones (as CDC-50). At millions of rows it
#                      picks a very narrow bandwidth, so f(q) is spiky.
#                 NRD0 Silverman rule of thumb (smoother).
#   Controls  : wind bin at the outcome period (FE) + Ambient_Temp_lead1
#   FE  : Spec A = Turbine + Date + wind bin | Spec B = A + Hour
#   SEs : HC1 (as CDC-50) and clustered by date, both reported
#   Sample: interrupted slots kept; non-exogenous stop slots dropped
#
# Mass points: Power piles up at ~0 (idle) and at rated output, so a
# quantile can sit on a mass point; those cells are flagged
# identified = FALSE in the diagnostics and skipped.
# ============================================================
suppressPackageStartupMessages({
  library(data.table); library(arrow); library(fixest)
})
setFixest_nthreads(0)

if (!exists("WIDTHS"))
  WIDTHS <- c("10 minutes", "30 minutes", "1 hour", "2 hours", "3 hours", "6 hours", "10 hours")

ROOT <- file.path(Sys.getenv("USERPROFILE"), "Dropbox", "Interruption Management", "IM Project")
PANEL_DIR <- file.path(ROOT, "Data", "Source Data", "Wind Farm Data", "Panel")
OUT       <- file.path(ROOT, "Data", "Regression Panels", "Kelmarsh Panel", "Results")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

treatments <- c("Interruption", "Concentration_C0")
outs <- c(Power = "Power_lead1", PerfRatio = "Perf_Ratio_Ref_lead1", PowerSD = "Power_SD_lead1")
taus <- c(Q10 = 0.10, Q50 = 0.50, Q90 = 0.90)
bws  <- c("SJ", "NRD0")
ctrl <- "Ambient_Temp_lead1"
fe   <- c(A = "Turbine_FE + Date_FE + Wind_Bin_lead1",
          B = "Turbine_FE + Date_FE + Wind_Bin_lead1 + Hour_FE")
SAMPLES <- c("all", "running")
need <- c("turbine", "slot", "stop_any", "stop_any_lead1", "Interruption", "Concentration_C0", outs,
          "Ambient_Temp_lead1", "Wind_Bin_lead1", "Turbine_FE", "Date_FE", "Hour_FE")

# ---- RIF helpers ------------------------------------------------------------
rif_quantile_ffl <- function(y, tau, bw_type) {
  yc <- y[!is.na(y)]
  q  <- as.numeric(quantile(yc, probs = tau, type = 7))
  bw <- if (bw_type == "SJ") tryCatch(bw.SJ(yc, method = "ste"), error = function(e) bw.nrd0(yc))
        else bw.nrd0(yc)
  d  <- density(yc, bw = bw, n = 4096, from = min(yc), to = max(yc))
  fq <- approx(d$x, d$y, xout = q)$y
  if (is.na(fq) || fq < 1e-10) fq <- 1e-10
  rif <- q + (tau - as.numeric(y <= q)) / fq
  rif[is.na(y)] <- NA_real_
  list(rif = rif, q = q, fq = fq, bw = bw, mass = mean(yc == q))
}
rif_variance_ffl <- function(y) (y - mean(y, na.rm = TRUE))^2

results <- list(); diag <- list()
for (w in WIDTHS) {
  f <- file.path(PANEL_DIR, paste0("kelmarsh_panel_", gsub(" ", "", w), ".parquet"))
  if (!file.exists(f)) { message("missing panel for ", w, " -- skipped"); next }
  panel_sec <- as.numeric(lubridate::duration(w))
  d <- as.data.table(read_parquet(f, col_select = all_of(need)))
  d0 <- d[!(stop_any == 1L & Interruption == 0L) & !is.na(Wind_Bin_lead1) & !is.na(Ambient_Temp_lead1)]
  # all     : every next period (may still be inside the stop), as CDC-50
  # running : next period has no logged stop (turbine back up: pure recovery)
  for (smp in SAMPLES) {
  d <- if (smp == "running") d0[stop_any_lead1 == 0L] else copy(d0)
  cat(sprintf("\n===== %s | sample %s : %s slots | %s interrupted =====\n", w, smp,
              format(nrow(d), big.mark = ","), format(sum(d$Interruption), big.mark = ",")))

  for (o in names(outs)) {
    y <- d[[outs[o]]]
    lhs_list <- list()
    for (tq in names(taus)) for (b in bws) {
      r <- rif_quantile_ffl(y, taus[tq], b)
      nm <- paste("RIF", o, tq, b, sep = "_")
      set(d, j = nm, value = r$rif)
      ident <- uniqueN(r$rif[!is.na(r$rif)]) > 1 && r$mass < 0.01
      diag[[length(diag) + 1]] <- data.table(width = w, sample = smp, outcome = o, stat = tq, bw_type = b,
        q_hat = r$q, f_q = r$fq, bw = r$bw, mass_at_q = r$mass, identified = ident)
      if (ident) lhs_list[[nm]] <- list(stat = tq, bw = b)
    }
    nm <- paste("RIF", o, "Var", sep = "_")
    set(d, j = nm, value = rif_variance_ffl(y))
    lhs_list[[nm]] <- list(stat = "Var", bw = "-")

    for (lhs in names(lhs_list)) for (tv in treatments) for (sp in names(fe)) {
      m <- tryCatch(feols(as.formula(paste0(lhs, " ~ ", tv, " + ", ctrl, " | ", fe[sp])),
                          data = d, vcov = "hetero", notes = FALSE), error = function(e) NULL)
      if (is.null(m) || is.na(coef(m)[tv])) next
      results[[length(results) + 1]] <- data.table(
        width = w, hours = panel_sec / 3600, sample = smp, treatment = tv, spec = sp, outcome = o,
        stat = lhs_list[[lhs]]$stat, bw_type = lhs_list[[lhs]]$bw,
        estimate = unname(coef(m)[tv]), se_HC1 = unname(se(m)[tv]),
        se_Date = unname(se(summary(m, vcov = ~Date_FE))[tv]), n = nobs(m))
    }
    d[, (names(lhs_list)) := NULL]
    cat("  done:", o, "\n")
  }
  }
  rm(d, d0); invisible(gc())
}

res <- rbindlist(results); dg <- rbindlist(diag)
res[, `:=`(p_HC1 = 2 * pnorm(-abs(estimate / se_HC1)), p_Date = 2 * pnorm(-abs(estimate / se_Date)))]
fwrite(res, file.path(OUT, "kelmarsh_rif_results.csv"))
fwrite(dg,  file.path(OUT, "kelmarsh_rif_diagnostics.csv"))
saveRDS(list(results = res, diagnostics = dg), file.path(OUT, "kelmarsh_rif_results.rds"))

# ---- console ----------------------------------------------------------------
options(width = 220)
stars <- function(p) fifelse(p < .01, "***", fifelse(p < .05, "**", fifelse(p < .1, "*", "")))
cat("\n---- RIF diagnostics (10-minute panel; identified = FALSE means mass point) ----\n")
print(dg[width == WIDTHS[1] & sample == "all"][, .(outcome, stat, bw_type, q_hat = round(q_hat, 3), f_q = signif(f_q, 3),
                                 bw = signif(bw, 3), mass_at_q = round(mass_at_q, 4), identified)])
res[, cell := sprintf("%s%s (%s)", formatC(estimate, format = "f", digits = 3), stars(p_Date),
                      formatC(se_Date, format = "f", digits = 3))]
show_all <- res[spec == "B" & bw_type %in% c("NRD0", "-")]
for (smp in SAMPLES) for (tv in treatments) for (o in names(outs)) {
  cat("\n=====", tv, "| sample:", smp, "| Spec B |", o, "(t+1) | NRD0 bandwidth | estimate (Date-clustered SE) =====\n")
  print(dcast(show_all[sample == smp & treatment == tv & outcome == o], hours + width ~ stat, value.var = "cell")[order(hours)][, -"hours"])
}

# ---- LaTeX: per treatment, rows = period length, cols = outcome x statistic --
st_order <- c("Q10", "Q50", "Q90", "Var")
olab <- c(Power = "Power$_{t+1}$ (kW)", PerfRatio = "Performance ratio$_{t+1}$", PowerSD = "Power SD$_{t+1}$ (kW)")
tlab <- c(Interruption = "Interruption", Concentration_C0 = "Concentration ($C_0$)")
for (smp in SAMPLES) for (tv in treatments) {
  show <- show_all[sample == smp]
  body <- character(0)
  for (o in names(outs)) {
    body <- c(body, sprintf("\\multicolumn{5}{l}{\\emph{%s}} \\\\", olab[o]))
    x <- show[treatment == tv & outcome == o]
    for (w in WIDTHS) {
      xw <- x[width == w]; if (!nrow(xw)) next
      cellf <- function(s, what) {
        r <- xw[stat == s]; if (!nrow(r)) return(if (what == "est") "--" else "")
        if (what == "est") paste0(formatC(r$estimate, format = "f", digits = 3, big.mark = ","), "$^{", stars(r$p_Date), "}$")
        else paste0("(", formatC(r$se_Date, format = "f", digits = 3, big.mark = ","), ")")
      }
      body <- c(body, paste0("\\quad ", w, " & ", paste(sapply(st_order, cellf, what = "est"), collapse = " & "), " \\\\"),
                      paste0(" & ", paste(sapply(st_order, cellf, what = "se"), collapse = " & "), " \\\\"))
    }
    body <- c(body, "\\addlinespace")
  }
  tex <- c("\\begin{table}[htbp]", "\\centering",
    sprintf("\\caption{RIF Regressions Of Next-Period Outcomes On %s By Period Length: Kelmarsh (%s)}", tlab[tv],
            if (smp == "all") "All Next Periods" else "Next Period Running"),
    sprintf("\\label{tab:kelmarsh-rif-%s-%s}", tolower(gsub("_", "-", tv)), smp), "\\small",
    "\\begin{tabular}{lrrrr}", "\\toprule",
    "Period length & Q10 & Q50 & Q90 & Variance \\\\", "\\midrule",
    head(body, -1), "\\bottomrule", "\\end{tabular}",
    "\\begin{minipage}{\\textwidth}\\footnotesize\\vspace{4pt}",
    paste0("\\emph{Notes.} Each cell is the coefficient on ", tlab[tv], " from a separate RIF-OLS regression ",
           "(Firpo, Fortin and Lemieux 2009) of the stated unconditional statistic of the next-period outcome. ",
           if (smp == "all") "The sample keeps every next period, including those still inside the stop. " else
             "The sample is restricted to next periods with no logged stop (turbine running again). ",
           "Quantile densities use a Gaussian kernel with the Silverman rule-of-thumb bandwidth. All models include ",
           "turbine, date, hour-of-day and 1 m/s wind-speed-bin fixed effects (wind measured in the outcome period) ",
           "and ambient temperature. Standard errors clustered by date in parentheses. -- marks a quantile that ",
           "sits on a mass point and is not identified. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."),
    "\\end{minipage}", "\\end{table}")
  writeLines(tex, file.path(OUT, paste0("kelmarsh_rif_", tolower(tv), "_", smp, ".tex")))
}
cat("\nSaved to", OUT, "\n")
