################################################################################
#  mu_report.R
#  Presentation of the results: number formatting, the intermediate-result
#  tables shown on screen, and the printable report (Print report / save as
#  PDF). Every number comes from mu_topdown_core.R; nothing is calculated here.
################################################################################

# ---- display helpers ---------------------------------------------------------
fmt_num <- function(x, d = 4) {
  out <- formatC(signif(x, d), format = "fg", digits = d, flag = "#")
  out <- sub("\\.$", "", out)
  out[is.na(x)] <- ""
  out
}
fmt_fix <- function(x, d = 2) { out <- formatC(x, format = "f", digits = d); out[is.na(x)] <- ""; out }
fmt_pct <- function(x, d = 3) { out <- paste0(formatC(100 * x, format = "f", digits = d), " %"); out[is.na(x)] <- "\u2014"; out }
fmt_in  <- function(x) { out <- as.character(x); out[is.na(x)] <- ""; out }   # echo inputs as entered

qc_display <- function(r) {
  t <- r$prec$table; a <- r$prec$anova
  body <- data.frame(
    Date = t$Date, Mean = fmt_in(t$Mean), SD = fmt_in(t$SD),
    "CV = 100\u00b7SD/Mean" = fmt_fix(t$CV, 3), "# Points (n)" = as.character(t$Points),
    "df = n \u2212 1" = as.character(t$df), "SS = (n\u22121)\u00b7SD\u00b2" = fmt_num(t$SS, 5),
    "n\u00b7(Mean \u2212 x\u0304)\u00b2" = fmt_num(t$B, 5),
    check.names = FALSE, stringsAsFactors = FALSE)
  if (nrow(t)) {
    foot <- body[1, ]; foot[] <- ""
    foot$Date <- "All months"; foot$Mean <- paste("x\u0304 =", fmt_num(a$grand_mean))
    foot[["# Points (n)"]] <- paste("N =", a$n_points)
    foot[["df = n \u2212 1"]] <- paste("N \u2212 m =", a$df_within)
    foot[["SS = (n\u22121)\u00b7SD\u00b2"]] <- paste("SS_wth =", fmt_num(a$ss_within, 5))
    foot[["n\u00b7(Mean \u2212 x\u0304)\u00b2"]] <- if (is.na(a$ss_between)) "" else paste("SS_btw =", fmt_num(a$ss_between, 5))
    body <- rbind(body, foot)
  }
  body
}

# One-way ANOVA from the monthly summaries (CLSI C51-A 7.3)
anova_display <- function(a) {
  data.frame(
    Quantity = c("MS_wth = SS_wth / (N \u2212 m)", "MS_btw = SS_btw / (m \u2212 1)", "n\u2080 = (N \u2212 \u03a3n\u00b2/N) / (m \u2212 1)",
                 "s_wth = \u221aMS_wth", "s_btw = \u221amax(0, (MS_btw \u2212 MS_wth)/n\u2080)", "\u221a(s_wth\u00b2 + s_btw\u00b2)",
                 "Pooled monthly CV = 100\u00b7s_wth / x\u0304",
                 "Pooled monthly CV with month-to-month shifts = 100\u00b7\u221a(s_wth\u00b2 + s_btw\u00b2) / x\u0304"),
    Value = c(fmt_num(a$ms_within, 5), fmt_num(a$ms_between, 5), fmt_num(a$n0, 4),
              fmt_num(a$s_within, 4), fmt_num(a$s_between, 4), fmt_num(a$s_within_between, 4),
              fmt_pct(a$cv_pooled_pct / 100), fmt_pct(a$cv_pooled_shifts_pct / 100)),
    stringsAsFactors = FALSE)
}

ept_display <- function(b) {
  t <- b$table
  data.frame(
    Survey = t$Survey, Specimen = t$Specimen, "Your Result" = fmt_in(t$Your_Result),
    Mean = fmt_in(t$Mean), "S.D." = fmt_in(t$S.D.), "No. of Labs" = fmt_in(t$No_of_Labs),
    "B = Your Result \u2212 Mean" = fmt_num(t$bias), "%B = 100\u00b7B/Mean" = fmt_fix(100 * t$rel_bias, 3),
    "(%B)\u00b2" = fmt_num((100 * t$rel_bias)^2, 4),
    "S.D./\u221aNo. of Labs" = fmt_num(t$sd_sqrtn), "%SE = 100\u00b7(S.D./\u221aNo. of Labs)/Mean" = fmt_fix(100 * t$sd_sqrtn_rel, 4),
    "%SE\u00b2" = fmt_num((100 * t$sd_sqrtn_rel)^2, 4),
    "SDI = |%B| / CV" = fmt_fix(t$sdi_iqmh, 3),
    "S.D.I recomputed" = fmt_fix(t$sdi_report_calc, 2), "S.D.I entered" = fmt_fix(t$sdi_report_entered, 1),
    "S.D.I check" = t$sdi_check, check.names = FALSE, stringsAsFactors = FALSE)
}

# The same table split in two, to fit a portrait page in the printed report
ept_inputs_display <- function(b) {
  t <- b$table
  data.frame(
    Survey = t$Survey, Specimen = t$Specimen, "Your Result" = fmt_in(t$Your_Result),
    Mean = fmt_in(t$Mean), "S.D." = fmt_in(t$S.D.), "No. of Labs" = fmt_in(t$No_of_Labs),
    "S.D.I entered" = fmt_fix(t$sdi_report_entered, 1), "S.D.I recomputed" = fmt_fix(t$sdi_report_calc, 2),
    "S.D.I check" = t$sdi_check, check.names = FALSE, stringsAsFactors = FALSE)
}
ept_calc_display <- function(b) {
  t <- b$table
  data.frame(
    Specimen = t$Specimen, "B = Your Result \u2212 Mean" = fmt_num(t$bias), "%B = 100\u00b7B/Mean" = fmt_fix(100 * t$rel_bias, 3),
    "(%B)\u00b2" = fmt_num((100 * t$rel_bias)^2, 4), "S.D./\u221aNo. of Labs" = fmt_num(t$sd_sqrtn),
    "%SE = 100\u00b7(S.D./\u221aNo. of Labs)/Mean" = fmt_fix(100 * t$sd_sqrtn_rel, 4),
    "%SE\u00b2" = fmt_num((100 * t$sd_sqrtn_rel)^2, 4), "SDI = |%B| / CV" = fmt_fix(t$sdi_iqmh, 3),
    check.names = FALSE, stringsAsFactors = FALSE)
}

crm_display <- function(c) {
  data.frame(
    Quantity = c("n replicates", "Mean of replicates", "SD of replicates", "SEM = SD/\u221an",
                 "Certified value", "Bias B = mean \u2212 certified", "B %", "SEM % of certified",
                 "u_ref = U_cert / k (CRM value)", "u_ref % of certified", "u_c(b) = \u221a(SEM\u00b2 + u_ref\u00b2)",
                 "|B| > 2\u00b7u_c(b)? (CLSI \u00a78.3.2)", "SDI = |B %| / CV %"),
    Value = c(as.character(c$n), fmt_num(c$mean, 5), fmt_num(c$sd), fmt_num(c$sem), fmt_in(c$certified),
              fmt_num(c$bias), fmt_pct(c$bias_rel), fmt_pct(c$sem_rel), fmt_num(c$u_ref),
              fmt_pct(c$u_ref_rel), fmt_num(c$u_c_b),
              if (is.na(c$significant)) "\u2014" else if (c$significant) "yes \u2013 significant" else "no",
              fmt_fix(c$sdi, 3)),
    stringsAsFactors = FALSE)
}

# ---- uncertainty at reference / decision limits ("Apply to a result") ----------
# One row per entered limit, using U of the pooled estimate or of one level
# (the pooled estimate falls back to level 1 when only one level is entered).
limits_table <- function(state, r) {
  if (!is.null(r$error)) return(NULL)
  lim <- state$limits; u <- state$test$units %||% ""
  has_pooled <- !is.null(r$pooled) && is.null(r$pooled$error)
  rows <- lapply(seq_along(lim$value), function(j) {
    x <- as_num(lim$value[j]); if (is.na(x)) return(NULL)
    lv <- as.character(lim$level[j] %||% "pooled")
    if (length(lv) != 1 || is.na(lv)) lv <- "pooled"
    # with a single level there is no pooled estimate: use that level
    if (lv == "pooled" && !has_pooled && length(r$levels) == 1) lv <- "1"
    if (lv == "pooled") {
      Upct <- if (has_pooled) r$pooled$U_pct else NA_real_
      src <- if (has_pooled) paste("Pooled", paste(r$pooled$levels, collapse = "+")) else "Pooled (not available)"
    } else {
      i <- suppressWarnings(as.integer(lv))
      Upct <- if (!is.na(i) && i <= length(r$levels)) r$levels[[i]]$U_pct else NA_real_
      src <- paste("Level", lv)
    }
    Uabs <- abs(x) * Upct / 100
    desc <- as.character(lim$desc[j] %||% ""); if (is.na(desc)) desc <- ""
    data.frame(Description = desc, Value = paste(fmt_in(x), u),
               "U of" = src,
               "U (%)" = fmt_pct(Upct / 100, 2), "Result \u00b1 U" = format_x_pm_U(x, Uabs, u),
               Interval = if (is.na(Uabs)) "\u2014" else paste(fmt_num(x - Uabs), "to", fmt_num(x + Uabs), u),
               check.names = FALSE, stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(NULL)
  do.call(rbind, rows)
}

# ---- printable report ----------------------------------------------------------
# One self-contained HTML document, laid out for printing / "Save as PDF"
# (US Letter, portrait): page 1 = test information, settings, summary, limits
# and sign-off; then one page per level; then a short method summary.
REPORT_CSS <- "
@page { size: letter portrait; margin: 14mm 12mm 16mm 12mm;
        @bottom-right { content: 'Page ' counter(page) ' of ' counter(pages);
                        font-family: Arial, Helvetica, sans-serif; font-size: 7.5pt; color: #555; } }
html { -webkit-print-color-adjust: exact; print-color-adjust: exact; }
body { font-family: Arial, Helvetica, sans-serif; font-size: 9.5pt; color: #111; margin: 0; }
.sheet { max-width: 190mm; margin: 0 auto; padding: 8mm 0; }
h1 { font-size: 16pt; margin: 0 0 2pt; }
h2 { font-size: 12.5pt; margin: 12pt 0 5pt; padding-bottom: 2pt; border-bottom: 1.2pt solid #333; }
h3 { font-size: 10.5pt; margin: 10pt 0 4pt; }
p { margin: 3pt 0; }
h2, h3 { break-after: avoid; page-break-after: avoid; }
.meta { color: #444; font-size: 8.5pt; }
table { border-collapse: collapse; width: 100%; margin: 3pt 0 6pt; font-size: 8pt; }
thead { display: table-header-group; }
tr { page-break-inside: avoid; break-inside: avoid; }
th, td { border: 0.5pt solid #9aa0a6; padding: 1.4pt 4pt; text-align: left; vertical-align: top; }
th { background: #e9edf2; font-weight: 700; }
td.num { text-align: right; white-space: nowrap; }
table.kv td:first-child { width: 42%; font-weight: 600; background: #f6f7f9; }
table.kv { width: auto; min-width: 60%; }
tr.total td { font-weight: 700; background: #f6f7f9; }
.level { break-before: page; page-break-before: always; }
.methods { margin-top: 14pt; break-inside: avoid; page-break-inside: avoid; }
.pair { display: flex; gap: 5mm; align-items: flex-start; break-inside: avoid; page-break-inside: avoid; }
.pair > div { flex: 1 1 0; min-width: 0; }
.pair table.kv { width: 100%; min-width: 0; }
.pair h3 { margin-top: 3pt; }
.checks { margin: 3pt 0 6pt 14pt; padding: 0; }
.checks li { margin: 1pt 0; }
.alert { color: #b00020; font-weight: 700; }
.warning { color: #8a5300; }
.error { color: #b00020; }
.info { color: #1d4f7a; }
.ok { color: #1e7b34; }
.signoff { margin-top: 14pt; width: 100%; }
.signoff td { border: none; border-bottom: 0.6pt solid #333; height: 22pt; vertical-align: bottom; }
.signoff td.lbl { border: none; width: 16%; font-weight: 600; }
.small { font-size: 8pt; color: #333; }
@media screen { body { background: #e9ecef; } .sheet { background: #fff; padding: 12mm; margin: 6mm auto; box-shadow: 0 0 4px rgba(0,0,0,.25); } }
"

build_report <- function(state, r, version = APP_VERSION, title = NULL) {
  t <- state$test; s <- state$settings; units <- t$units %||% ""
  is_num <- function(v) grepl("^[\u2212+-]?[0-9][0-9.,]*(e[+-]?[0-9]+)?( ?%)?$", v)
  tbl <- function(df, total_last = FALSE, cls = NULL) {
    if (is.null(df) || !nrow(df)) return(NULL)
    tags$table(class = cls,
      tags$thead(tags$tr(lapply(names(df), tags$th))),
      tags$tbody(lapply(seq_len(nrow(df)), function(j) {
        tags$tr(class = if (total_last && j == nrow(df)) "total",
                lapply(df[j, ], function(v) { v <- as.character(v); tags$td(class = if (is_num(v)) "num", v) }))
      })))
  }
  kv <- function(keys, vals) {
    tags$table(class = "kv", tags$tbody(lapply(seq_along(keys), function(j)
      tags$tr(tags$td(keys[j]), tags$td(vals[j])))))
  }
  checks_list <- function(ch) {
    if (is.null(ch) || !nrow(ch)) return(tags$p(class = "ok", "No warnings."))
    tags$ul(class = "checks", lapply(seq_len(nrow(ch)), function(j) tags$li(class = ch$severity[j], ch$message[j])))
  }
  rule <- c(sdi = "when mean SDI > SDI limit (IQMH)", always = "always (IQMH Eq 2)", never = "never")
  u_cal <- calibrator_u_ref(t$cal_value, t$cal_u)
  blank <- function(x) if (is.null(x) || is.na(x) || !nzchar(as.character(x))) "\u2014" else as.character(x)

  # ---- page 1: test, settings, summary, limits, sign-off ----
  page1 <- tagList(
    tags$h1("Measurement uncertainty report (top-down)"),
    tags$p(class = "meta", sprintf("Generated %s \u00b7 MU calculator version %s \u00b7 IQMH guidance (2014); CLSI C51-A",
                                   format(Sys.time(), "%Y-%m-%d %H:%M"), version)),
    tags$h2("Test information"),
    kv(c("Analyte", "Method", "Instrument", "Units", "Calibrator value", "u_ccal, or u_cal",
         "Uncertainty of calibrator, u_ref = u \u00f7 calibrator value"),
       c(blank(t$analyte), blank(t$method), blank(t$instrument), blank(t$units),
         blank(as_num(t$cal_value)), blank(as_num(t$cal_u)),
         if (is.na(u_cal)) "not entered" else fmt_pct(u_cal))),
    tags$h2("Calculation settings"),
    kv(c("Number of levels", "IQC pooling method", "Include SR_g and u_ref in u_B", "SDI limit",
         "Coverage factor k", "Target maximum U"),
       c(as.character(state$n_levels), IMPRECISION_LABELS[[s$imprecision]], rule[[s$bias_rule]],
         if (s$bias_rule == "sdi") format(s$sdi_limit) else "\u2014", format(s$k),
         if (is.na(s$target_U_pct)) "\u2014" else paste(s$target_U_pct, "%"))))
  body_levels <- NULL
  if (!is.null(r$error)) {
    page1 <- tagList(page1, tags$p(class = "alert", paste("Calculation error:", r$error)))
  } else {
    key <- do.call(rbind, lapply(seq_along(r$levels), function(i) {
      L <- r$levels[[i]]
      data.frame(Level = sprintf("%d (report %s)", i, L$qc$level), "Control material" = L$qc$material %||% "",
                 "IQC mean" = fmt_num(L$qc_mean),
                 "EQA/PT or CRM mean" = if (L$bias$source == "none") "\u2014" else fmt_num(L$bias$mean_ref),
                 "u_s" = fmt_pct(L$u_prec, 2), "u_B" = fmt_pct(L$u_bias, 2), "u_c" = fmt_pct(L$uc, 2),
                 "U" = fmt_pct(L$U, 2), "Result \u00b1 U" = format_x_pm_U(L$qc_mean, L$U_abs, units),
                 check.names = FALSE, stringsAsFactors = FALSE)
    }))
    p <- r$pooled
    if (!is.null(p) && is.null(p$error))
      key <- rbind(key, data.frame(Level = paste("Pooled", paste(p$levels, collapse = "+")), "Control material" = "",
                                   "IQC mean" = "\u2014", "EQA/PT or CRM mean" = "\u2014", "u_s" = fmt_pct(p$u_prec, 2),
                                   "u_B" = fmt_pct(p$u_bias, 2), "u_c" = fmt_pct(p$uc, 2), "U" = fmt_pct(p$U, 2),
                                   "Result \u00b1 U" = sprintf("\u00b1 %s %% of result", format(round_up_sig(p$U_pct, 2))),
                                   check.names = FALSE, stringsAsFactors = FALSE))
    alerts <- do.call(rbind, lapply(seq_along(r$levels), function(i) {
      ch <- r$levels[[i]]$checks; ch <- ch[ch$severity %in% c("alert", "error"), , drop = FALSE]
      if (nrow(ch)) ch$message <- paste0("Level ", i, ": ", ch$message)
      ch
    }))
    lt <- limits_table(state, r)
    page1 <- tagList(page1,
      tags$h2("Summary of results"),
      tbl(key, total_last = !is.null(p) && is.null(p$error)),
      tags$p(class = "small", sprintf("U = k \u00b7 u_c with k = %s (\u2248 95 %%). Results are stated per CLSI C51-A \u00a79.2 ",
                                      format(s$k)), "(U rounded up to two significant digits)."),
      if (!is.null(alerts) && nrow(alerts)) checks_list(alerts),
      tags$h2("Uncertainty at reference intervals / clinical decision limits"),
      if (is.null(lt)) tags$p(class = "small", "None entered.") else tbl(lt),
      tags$table(class = "signoff", tags$tbody(
        tags$tr(tags$td(class = "lbl", "Prepared by"), tags$td(""), tags$td(class = "lbl", "Date"), tags$td("")),
        tags$tr(tags$td(class = "lbl", "Reviewed by"), tags$td(""), tags$td(class = "lbl", "Date"), tags$td("")))))

    # ---- one page per level ----
    body_levels <- lapply(seq_along(r$levels), function(i) {
      L <- r$levels[[i]]; b <- L$bias; a <- L$prec$anova
      head <- sprintf("Level %d \u2014 report level %s%s%s", i, L$qc$level,
                      if (nzchar(L$qc$material %||% "")) paste0(" \u00b7 ", L$qc$material) else "",
                      if (nzchar(L$qc$lot %||% "")) paste0(" (lot ", L$qc$lot, ")") else "")
      bias_part <- switch(b$source,
        ept = if (b$n > 0) tagList(
          tags$p(sprintf("Provider / survey: %s. Peer Group: %s.", blank(b$provider), blank(b$peer_group))),
          tbl(ept_inputs_display(b)), tbl(ept_calc_display(b))) else tags$p("No valid EQA/PT rows."),
        crm = tagList(tags$p(paste("CRM:", blank(b$material))), tbl(crm_display(b$crm))),
        tags$p("No bias component (u_B = 0)."))
      bias_kv <- switch(b$source,
        ept = if (b$n > 0) kv(c("Challenges J / surveys", "Mean assigned value (Mean)", "RMS_bias", "SR_g (S.D./\u221aNo. of Labs)",
               "u_ref (calibrator)", "Mean SDI (|%B| / CV)", "Decision", "u_B"),
             c(sprintf("%d / %d", b$n, b$n_surveys), paste(fmt_num(b$mean_ref), units), fmt_pct(b$rms), fmt_pct(b$srg),
               if (is.na(b$u_ref)) "not entered" else fmt_pct(b$u_ref), fmt_fix(b$mean_sdi, 3), b$reason,
               fmt_pct(L$u_bias))),
        crm = kv("u_B = \u221a(SEM\u00b2 + u_ref\u00b2) (relative)", fmt_pct(L$u_bias)),
        kv("u_B", fmt_pct(0)))
      tags$div(class = "level",
        tags$h2(head),
        tags$h3("1. Imprecision (u_s) from monthly IQC"),
        tbl(qc_display(L), total_last = TRUE),
        tbl(anova_display(a)),
        tags$p(sprintf("N = %d results over m = %d months; IQC mean x\u0304 = %s %s. ", as.integer(a$n_points),
                       as.integer(a$n_months), fmt_num(L$qc_mean), units),
               tags$strong(sprintf("u_s = %s [%s].", fmt_pct(L$u_prec), L$prec$label))),
        tags$h3(sprintf("2. Uncertainty associated with bias (u_B) \u2014 %s",
                        switch(b$source, ept = "EQA/PT", crm = "CRM", none = "none"))),
        bias_part,
        # bias summary (left) next to the combined and expanded uncertainty (right)
        tags$div(class = "pair",
          tags$div(bias_kv),
          tags$div(
            tags$h3("3. Combined and expanded uncertainty"),
            kv(c("u_c = \u221a(u_s\u00b2 + u_B\u00b2)", "k", "U = k \u00b7 u_c", "U at the IQC mean", "Result statement (CLSI \u00a79.2)"),
               c(fmt_pct(L$uc), format(L$k), fmt_pct(L$U, 2), paste(fmt_num(L$U_abs), units),
                 format_x_pm_U(L$qc_mean, L$U_abs, units))))),
        tags$h3("Checks"),
        checks_list(L$checks))
    })
  }

  methods <- tags$div(class = "methods",
    tags$h2("Method summary"),
    tags$ul(class = "small",
      tags$li("Imprecision u_s: monthly IQC (n_i results, mean x\u0304_i, SD s_i) analysed by one-way ANOVA with months as groups ",
              "(CLSI C51-A \u00a77.3). Pooled monthly CV = 100\u00b7s_wth / x\u0304 with s_wth = \u221a[\u03a3(n_i\u22121)s_i\u00b2 / (N\u2212m)] ",
              "(CLSI Eq 6, 27). With month-to-month shifts: 100\u00b7\u221a(s_wth\u00b2 + s_btw\u00b2) / x\u0304, s_btw\u00b2 = max(0, (MS_btw \u2212 MS_wth)/n\u2080) ",
              "(CLSI Eq 28\u201329). x\u0304 is the mean of all results."),
      tags$li("Bias from EQA/PT: %B_j = 100\u00b7(Your Result \u2212 Mean)/Mean; RMS_bias = \u221a(\u03a3 %B_j\u00b2 / J) (IQMH App B); ",
              "SR_g = \u221a(\u03a3 %SE_j\u00b2 / J) with %SE_j = 100\u00b7(S.D./\u221aNo. of Labs)/Mean (IQMH App C); ",
              "u_ref = u_ccal (or u_cal) / Calibrator Value; SDI_j = |%B_j| / CV. ",
              "u_B = \u221a(RMS_bias\u00b2 + SR_g\u00b2 + u_ref\u00b2), with SR_g and u_ref included when the mean SDI exceeds the SDI limit ",
              "(IQMH Eq 2; top-down alternative step 3d)."),
      tags$li("Bias from a CRM: u_B = \u221a[(SEM/x_CRM)\u00b2 + (u_ref/x_CRM)\u00b2], SEM = s/\u221an, u_ref = U_cert/k_cert ",
              "(IQMH Eq 2, App A; CLSI C51-A \u00a78.3.2)."),
      tags$li("u_c = \u221a(u_s\u00b2 + u_B\u00b2); U = k\u00b7u_c (IQMH Eq 3, 5; CLSI C51-A Eq 10, 12). Pooled across levels: CV weighted ",
              "by within-month degrees of freedom; u_B\u00b2 weighted by the number of EQA/PT challenges."),
      tags$li("Scope: analytical phase only (IQMH \u00a76). The EQA/PT pathway assumes consensus surveys.")),
    tags$p(class = "small", "Sources: Institute for Quality Management in Healthcare, Guidance on Measurement Uncertainty for ",
           "Medical Laboratories (2014); CLSI C51-A, Expression of Measurement Uncertainty in Laboratory Medicine (2012)."))

  # The <head> is written as text: htmltools moves tags$head() out of a tag
  # tree when rendering it, which would drop the stylesheet.
  title <- title %||% paste("MU report", if (nzchar(t$analyte %||% "")) paste("-", t$analyte) else "")
  # footer on every printed page: test and print date
  footer <- c("MU report", t$analyte, t$instrument, format(Sys.Date()))
  footer <- gsub("[\"\\\\\n]", " ", paste(footer[nzchar(footer)], collapse = " \u00b7 "))
  css <- paste0(REPORT_CSS, "@page { @bottom-left { content: \"", footer, "\"; font-family: Arial, Helvetica, sans-serif; ",
                "font-size: 7.5pt; color: #555; } }\n")
  paste0("<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n",
         "<title>", htmltools::htmlEscape(title), "</title>\n<style>", css, "</style>\n</head>\n<body>\n",
         as.character(tags$div(class = "sheet", page1, body_levels, methods)), "\n</body>\n</html>\n")
}
