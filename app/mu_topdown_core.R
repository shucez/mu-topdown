################################################################################
#  mu_topdown_core.R
#  Top-down measurement uncertainty (MU) for quantitative medical laboratory
#  examinations, from internal quality control (IQC) and external quality
#  assessment / proficiency testing (EQA/PT) or certified reference material
#  (CRM) data.
#
#  This file contains ALL of the arithmetic used by the web app (app.R,
#  mu_io.R and mu_report.R only collect inputs and display what these
#  functions return). Every function
#  is pure: it takes plain vectors / data frames / lists and returns the same,
#  with no dependency on Shiny. It can therefore be audited, sourced and run in
#  plain R:
#
#      source("mu_topdown_core.R")
#      res <- mu_level(qc = ..., bias = ..., settings = mu_default_settings())
#
#  ---------------------------------------------------------------------------
#  SOURCES (cited by section / equation number throughout)
#  ---------------------------------------------------------------------------
#  [IQMH]  Institute for Quality Management in Healthcare. Guidance on
#          Measurement Uncertainty for Medical Laboratories, v1.0 Dec 2014
#          (document v1.1, 2018-01-19). Section 4 "Steps" Equations 1-5,
#          Section 5 conversions and "A Top-Down Alternative Approach"
#          (steps 1-6), Appendices A (SEM), B (RMS), C (SD/sqrtN).
#  [CLSI]  CLSI C51-A. Expression of Measurement Uncertainty in Laboratory
#          Medicine; Approved Guideline. 2012. Sections 3.6 (summary
#          statistics, Eqs 1-12), 6.3 & Appendix A (Type B conversions),
#          7.2-7.3 (IQC data; one-way ANOVA variance components, Eqs 27-29),
#          8.3 (uncertainty of bias), 9.2 (significant digits).
#
#  ---------------------------------------------------------------------------
#  CONVENTIONS
#  ---------------------------------------------------------------------------
#  * Uncertainties are carried internally as RELATIVE values (fractions,
#    e.g. 0.0405 = 4.05 %). The app multiplies by 100 for display. Absolute
#    values are obtained at the end by multiplying by the IQC mean.
#  * Monthly CV is never entered: it is computed from the monthly SD and Mean,
#    so the calculation is not limited by the one-decimal CV printed on QC
#    reports. Arguments that take percent end in `_pct`.
#  * Column / argument names follow the wording on the raw reports:
#      Bio-Rad Unity monthly QC report, "Your Lab - Mon":  Mean, SD, # Points
#      CAP EQA/PT evaluation report: Specimen, Your Result, Mean, S.D.,
#                                 No. of Labs, S.D.I
#
#  ---------------------------------------------------------------------------
#  THE MODEL IN ONE PLACE  (notation of IQMH; ANOVA notation of CLSI)
#  ---------------------------------------------------------------------------
#     u_s   = standard uncertainty from imprecision (relative), from monthly IQC
#             by one-way ANOVA with the months as groups [CLSI 7.3]:
#             "Pooled monthly CV"                          s_wth / xbar
#             "Pooled monthly CV with month-to-month shifts"
#                                          sqrt(s_wth^2 + s_btw^2) / xbar
#             (variable u_prec in the code)
#     u_B   = standard uncertainty associated with bias, from EQA/PT or CRM
#             (variable u_bias in the code)
#     u_c   = sqrt(u_s^2 + u_B^2)                       [IQMH Eq 3; CLSI Eq 10]
#     U     = k * u_c        (k = 1.96 or 2, ~95 %)     [IQMH Eq 5; CLSI Eq 12]
#     U_abs = U * IQC mean   (reporting units)          [IQMH alt. step 6]
#
#  EQA/PT pathway (peer-group / all-method mean, or assigned value):
#     u_B = sqrt( RMS_bias^2 + SR_g^2 + u_ref^2 )       [IQMH Eq 2, right]
#        RMS_bias : root mean square of the relative biases   [IQMH App B]
#        SR_g     : uncertainty of the EQA/PT assigned value (the report's
#                   consensus Mean), (SD/sqrtN)group from the report [IQMH App C]
#        u_ref    : uncertainty of the calibrator's assigned value, per test,
#                   u_ccal (or u_cal) / Calibrator Value from the manufacturer's
#                   traceability sheet [IQMH notes to Eq 4]
#        SR_g and u_ref are included only if mean SDI > 2 by default
#        [IQMH alt. step 3d; IQMH note after the ACR example]
#  CRM pathway (higher-order reference):
#     u_B = sqrt( SEM^2 + u_ref^2 ), u_ref = CRM certificate U / k
#                                                [IQMH Eq 2 left, App A;
#                                                  CLSI 8.3.2]
################################################################################


# ==============================================================================
# 0. HELPERS
# ==============================================================================

#' Names of the two imprecision estimators (see qc_anova_months())
IMPRECISION_LABELS <- c(
  pooled        = "Pooled monthly CV",
  pooled_shifts = "Pooled monthly CV with month-to-month shifts"
)

#' Default calculation settings
#' @return named list
mu_default_settings <- function() {
  list(
    k            = 2,           # coverage factor [IQMH Eq 5: 1.96 or 2]
    bias_rule    = "sdi",       # "sdi" | "always" | "never"   (section 5)
    sdi_limit    = 2,           # SDI threshold [IQMH alt. step 3d]
    imprecision  = "pooled",    # "pooled" | "pooled_shifts"   (section 1)
    target_U_pct = NA_real_     # optional performance goal for U, percent
  )
}

#' Convert anything table-like to a numeric vector, treating "" as NA.
as_num <- function(x) {
  if (is.null(x)) return(numeric(0))
  x <- as.character(x)
  x <- trimws(gsub(",", "", x))      # tolerate thousands separators
  x <- sub("^\\+", "", x)            # CAP prints S.D.I as "+1.6"
  x[x == ""] <- NA
  suppressWarnings(as.numeric(x))
}

#' Round UP to a given number of significant digits.
#'
#' [CLSI 9.2]: "It usually suffices to quote uc(x) and U(x) to, at most, two
#' significant digits. In reporting final results, it is generally better to
#' round uncertainties up rather than to the nearest digit."
#' Example: 8.064 -> 8.1 ; 0.11867 -> 0.12 ; 21.88 -> 22.
#'
#' @param x numeric (non-negative uncertainty)
#' @param digits significant digits (default 2)
round_up_sig <- function(x, digits = 2) {
  vapply(x, function(v) {
    if (is.na(v) || !is.finite(v)) return(NA_real_)
    if (v == 0) return(0)
    p <- digits - 1 - floor(log10(abs(v)))
    # round(..., 8) removes floating-point noise so that an exact 8.1 is not
    # pushed up to 8.2 by ceiling().
    ceiling(round(v * 10^p, 8)) / 10^p
  }, numeric(1))
}

#' Number of decimals implied by a value rounded to `digits` significant digits.
decimals_for <- function(x, digits = 2) {
  if (is.na(x) || x <= 0) return(0)
  max(0, digits - 1 - floor(log10(x)))
}

#' Format "x +/- U units" with x stated consistently with U [CLSI 9.2]:
#' U is rounded up to 2 significant digits and x is rounded to the same
#' decimal place (e.g. x = 48.261, U = 1.2 -> "48.3 +/- 1.2").
format_x_pm_U <- function(x, U_abs, units = "") {
  if (is.na(x) || is.na(U_abs)) return(NA_character_)
  Ur <- round_up_sig(U_abs, 2)
  d  <- decimals_for(Ur, 2)
  trimws(sprintf(paste0("%.", d, "f \u00b1 %.", d, "f %s"), x, Ur, units))
}


# ==============================================================================
# 1. IMPRECISION FROM MONTHLY IQC (u_prec)
# ==============================================================================

#' Monthly IQC table with the intermediate columns used for pooling
#'
#' One row per monthly QC report (Unity "Your Lab - Mon"): Mean, SD and
#' # Points. The monthly CV is computed, not entered:
#'     CV_i % = 100 * SD_i / Mean_i                        [CLSI 3.6.4, Eq 4]
#'
#' Intermediate columns (month i, n_i = # Points):
#'     df_i = n_i - 1                   degrees of freedom of the month
#'     SS_i = (n_i - 1) * SD_i^2        sum of squared residuals of the month's
#'                                      results about the month's own mean,
#'                                      recovered exactly because the reported
#'                                      SD uses the divisor n_i - 1 [CLSI Eq 3]
#'     B_i  = n_i * (Mean_i - xbar)^2   month i's contribution to the
#'                                      between-month sum of squares, where
#'                                      xbar = sum(n_i * Mean_i) / sum(n_i)
#'                                      is the mean of all results
#'
#' Rows that are completely empty are dropped. Rows lacking Mean, SD or
#' # Points, or with # Points < 2 (no within-month SD), are dropped and their
#' positions returned in attr(, "dropped").
#'
#' @param date   character, information only (never used in calculations)
#' @param mean,sd numeric, monthly Mean and SD (reporting units)
#' @param points numeric, monthly # Points (number of QC results)
#' @return data.frame
qc_month_table <- function(date, mean, sd, points) {
  n_in <- max(length(date), length(mean), length(sd), length(points))
  rep_to <- function(v) if (length(v) == n_in) v else rep_len(if (length(v)) v else NA, n_in)
  date   <- as.character(rep_to(date)); date[is.na(date)] <- ""
  mean   <- as_num(rep_to(mean))
  sd     <- as_num(rep_to(sd))
  points <- as_num(rep_to(points))

  empty   <- trimws(date) == "" & is.na(mean) & is.na(sd) & is.na(points)
  invalid <- !empty & (is.na(mean) | is.na(sd) | is.na(points) | points < 2 |
                         mean == 0 | sd < 0)
  keep    <- !empty & !invalid

  xbar <- if (any(keep)) sum(points[keep] * mean[keep]) / sum(points[keep]) else NA_real_
  out <- data.frame(
    Date   = date,
    Mean   = mean,
    SD     = sd,
    CV     = 100 * sd / mean,              # [CLSI Eq 4]
    Points = points,
    df     = points - 1,
    SS     = (points - 1) * sd^2,          # within-month sum of squares
    B      = points * (mean - xbar)^2,     # between-month contribution
    stringsAsFactors = FALSE
  )[keep, , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "dropped") <- which(invalid)
  out
}

#' Pool monthly IQC by one-way ANOVA computed from the monthly summaries
#'
#' The months are the groups of a one-way analysis of variance [CLSI 7.3].
#' Every quantity is recovered exactly from each month's # Points (n_i),
#' Mean (xbar_i) and SD (s_i); no individual results are needed:
#'
#'   N    = sum(n_i),  m = number of months
#'   xbar = sum(n_i * xbar_i) / N                          mean of all results
#'   SS_w = sum((n_i - 1) * s_i^2)    df_w = N - m    MS_w = SS_w / df_w
#'   SS_b = sum(n_i * (xbar_i - xbar)^2)
#'                                    df_b = m - 1    MS_b = SS_b / df_b
#'   n0   = (N - sum(n_i^2) / N) / (m - 1)       [CLSI 7.3, footnote b;
#'                                                 n0 = n when all n_i = n]
#'
#' "Pooled monthly CV" - within-month imprecision:
#'   s_w = sqrt(MS_w)                            [CLSI 3.6.6 Eq 6; 7.3 Eq 27]
#'   the pooled sum of squared residuals divided by its degrees of freedom
#'   (each month's mean costs one degree of freedom). [IQMH] Step 1,
#'   Equation 1 and its note pool monthly SDs across "at least two levels of
#'   QC ... for the past 6 months"; this is that pooled SD with the
#'   degrees-of-freedom weights.
#'
#' "Pooled monthly CV with month-to-month shifts" - within + between months:
#'   s_b  = sqrt( max(0, (MS_b - MS_w) / n0) )   [CLSI 7.3 Eq 28]
#'   s_wb = sqrt( s_w^2 + s_b^2 )                [CLSI 7.3 Eq 29]
#'   s_b is the spread of the monthly means beyond what within-month scatter
#'   explains: shifts at reagent or calibrator lot changes, recalibrations,
#'   maintenance. [CLSI 7.2] regards such shifts, viewed over the long term,
#'   "as random variation attributable to ongoing routine changes of
#'   conditions rather than bias"; [IQMH Section 4] asks for six months of data
#'   so that "variations due to multiple users, reagents and calibrator lots
#'   are captured". (The plain SD of all N results understates this; CLSI 7.3
#'   Example 4.) If MS_b <= MS_w, s_b = 0 and the two estimates coincide.
#'
#' Both are expressed relative to the mean of all results:
#'   CV % = 100 * s / xbar
#'
#' With a single month there is no between-month information: s_b and the
#' second estimate are NA.
#'
#' @param tbl output of qc_month_table()
#' @return list of every ANOVA quantity, plus cv_pooled_pct and
#'         cv_pooled_shifts_pct
qc_anova_months <- function(tbl) {
  n <- tbl$Points; m <- nrow(tbl); N <- sum(n)
  na <- NA_real_
  out <- list(n_points = N, n_months = m, grand_mean = na,
              ss_within = na, df_within = na, ms_within = na,
              ss_between = na, df_between = na, ms_between = na, n0 = na,
              s_within = na, s_between = na, s_within_between = na,
              cv_pooled_pct = na, cv_pooled_shifts_pct = na)
  if (m == 0 || N - m < 1) return(out)
  xbar <- sum(n * tbl$Mean) / N
  ss_w <- sum((n - 1) * tbl$SD^2)
  ms_w <- ss_w / (N - m)
  out[c("grand_mean", "ss_within", "df_within", "ms_within")] <- list(xbar, ss_w, N - m, ms_w)
  out$s_within      <- sqrt(ms_w)                                    # [CLSI Eq 27]
  out$cv_pooled_pct <- 100 * out$s_within / xbar
  if (m >= 2) {
    ss_b <- sum(n * (tbl$Mean - xbar)^2)
    ms_b <- ss_b / (m - 1)
    n0   <- (N - sum(n^2) / N) / (m - 1)                             # [CLSI 7.3 fn b]
    s_b  <- sqrt(max(0, (ms_b - ms_w) / n0))                         # [CLSI Eq 28]
    out[c("ss_between", "df_between", "ms_between", "n0", "s_between")] <-
      list(ss_b, m - 1, ms_b, n0, s_b)
    out$s_within_between     <- sqrt(ms_w + s_b^2)                   # [CLSI Eq 29]
    out$cv_pooled_shifts_pct <- 100 * out$s_within_between / xbar
  }
  out
}

#' Pool imprecision across QC levels
#'
#' Done on the relative (CV) scale because the levels differ in
#' concentration. Each level is weighted by its within-month degrees of
#' freedom df_l = N_l - m_l, the same weighting as [CLSI Eq 6]:
#'     CV_pooled = sqrt( sum(df_l * CV_l^2) / sum(df_l) )
#' For "Pooled monthly CV" this recovers the pooled relative sum of squares,
#' since df_l * (CV_l/100)^2 = SS_w,l / xbar_l^2. For "with month-to-month
#' shifts" the same weights are used as a convention (the between-month part
#' of each level's variance rests on fewer degrees of freedom, m_l - 1).
#' [IQMH Eq 1 note]: pooling across levels suits methods whose "control
#' levels depict comparable performance across the entire" measuring
#' interval; otherwise MU "must be calculated at each ... control level".
#'
#' @param df_within per-level within-month degrees of freedom (N_l - m_l)
#' @param cv_pct per-level CV in percent (the estimator in use)
#' @return pooled CV in percent
pool_qc_levels <- function(df_within, cv_pct) {
  ok <- !is.na(df_within) & !is.na(cv_pct) & df_within > 0
  sqrt(sum(df_within[ok] * cv_pct[ok]^2) / sum(df_within[ok]))
}


# ==============================================================================
# 2. CONVERSION OF STATED UNCERTAINTIES TO STANDARD UNCERTAINTY
# ==============================================================================

#' Convert a stated uncertainty to a standard uncertainty (1 SD equivalent)
#'
#' [IQMH 5.0] and [CLSI 6.3, Appendix A Table A1]:
#'   "standard"   : already an SD -> use as is       ("MU as SD")
#'   "expanded"   : U / k                            ("MU as Expanded
#'                  Uncertainty"; certificates normally use k = 2)
#'   "ci"         : half-width / t                   ("MU as a CI"; t from
#'                  Student's t for the stated confidence and df)
#'   "range_rect" : a / sqrt(3)  (value +/- a, all values equally likely)
#'   "range_tri"  : a / sqrt(6)  (value +/- a, central values more likely)
#'   "range_norm" : a / 3        (value +/- a taken as ~99.7 % normal limits;
#'                  CLSI Table A1 "Gaussian": (UL-LL)/(2*sqrt(9)))
#'
#' @param value the stated uncertainty (U, CI half-width or half-range a)
#' @return standard uncertainty in the same units as `value`
standard_uncertainty <- function(value,
                                 form = c("expanded", "standard", "ci",
                                          "range_rect", "range_tri", "range_norm"),
                                 k = 2, t = NA) {
  form <- match.arg(form)
  switch(form,
         standard   = value,
         expanded   = value / k,
         ci         = value / t,
         range_rect = value / sqrt(3),
         range_tri  = value / sqrt(6),
         range_norm = value / 3)
}


# ==============================================================================
# 3. BIAS FROM EXTERNAL PROFICIENCY TESTING (EQA/PT / PT / EQA)
# ==============================================================================

#' Per-challenge EQA/PT table with every intermediate used for u_bias
#'
#' One row per EQA/PT challenge (CAP report row):
#'   bias             = Your Result - Mean          ("Bias = Result - Assigned
#'                                                    Value", IQMH App B)
#'   rel_bias         = bias / Mean                 (relative bias, IQMH App B)
#'   rel_bias_sq      = rel_bias^2                  (IQMH App B "% Bias^2")
#'   sd_sqrtn         = S.D. / sqrt(No. of Labs)    (standard error of the
#'                                                    consensus Mean, IQMH App C)
#'   sd_sqrtn_rel     = sd_sqrtn / Mean             (IQMH App C)
#'   sd_sqrtn_rel_sq  = sd_sqrtn_rel^2              (IQMH App C)
#'   sdi_iqmh         = |rel_bias| / (method CV / 100)
#'       [IQMH alt. step 3c: "Express actual bias as SDI (Z-value or SD
#'        ratio; SDI is mean bias divided by method SD)"]
#'
#' Two different SDIs exist and must not be confused:
#'   * sdi_iqmh   : bias relative to YOUR LAB's IQC imprecision. This is the
#'                  one used for the inclusion rule in u_bias_ept().
#'   * sdi_report : (Your Result - Mean) / S.D., i.e. bias relative to the
#'                  PEER GROUP's spread. This is what the CAP report prints as
#'                  "S.D.I". It is computed here only to check transcription:
#'                  if an S.D.I was typed in and differs from the recomputed
#'                  value by more than `sdi_tol` (report rounds to 0.1), the
#'                  row is flagged.
#'
#' Rows missing Your Result or Mean are dropped (attr "dropped").
#'
#' @param method_cv_pct the IQC CV (percent) of the matched QC level, i.e.
#'        100 * u_prec
#' @return data.frame
ept_challenge_table <- function(survey, specimen, your_result, mean, sd,
                                no_of_labs, method_cv_pct, report_sdi = NA,
                                sdi_tol = 0.15) {
  n_in <- max(length(survey), length(specimen), length(your_result), length(mean))
  rep_to <- function(v) if (length(v) == n_in) v else rep_len(if (length(v)) v else NA, n_in)
  survey   <- as.character(rep_to(survey));   survey[is.na(survey)] <- ""
  specimen <- as.character(rep_to(specimen)); specimen[is.na(specimen)] <- ""
  y    <- as_num(rep_to(your_result))
  ref  <- as_num(rep_to(mean))
  rsd  <- as_num(rep_to(sd))
  nlab <- as_num(rep_to(no_of_labs))
  rsdi <- as_num(rep_to(report_sdi))

  empty   <- trimws(survey) == "" & trimws(specimen) == "" & is.na(y) & is.na(ref) &
             is.na(rsd) & is.na(nlab) & is.na(rsdi)
  invalid <- !empty & (is.na(y) | is.na(ref) | ref == 0)
  keep    <- !empty & !invalid

  bias      <- y - ref
  rel_bias  <- bias / ref
  sd_sqrtn  <- ifelse(!is.na(rsd) & !is.na(nlab) & nlab > 0, rsd / sqrt(nlab), NA_real_)
  sdsn_rel  <- sd_sqrtn / ref
  sdi_iqmh  <- abs(rel_bias) / (method_cv_pct / 100)
  sdi_calc  <- ifelse(!is.na(rsd) & rsd > 0, bias / rsd, NA_real_)
  sdi_check <- ifelse(is.na(rsdi), "not entered",
                      ifelse(is.na(sdi_calc), "cannot check",
                             ifelse(abs(sdi_calc - rsdi) > sdi_tol, "MISMATCH", "ok")))

  out <- data.frame(
    Survey          = survey,
    Specimen        = specimen,
    Your_Result     = y,
    Mean            = ref,
    S.D.            = rsd,
    No_of_Labs      = nlab,
    bias            = bias,
    abs_bias        = abs(bias),
    rel_bias        = rel_bias,
    rel_bias_sq     = rel_bias^2,
    sd_sqrtn        = sd_sqrtn,
    sd_sqrtn_rel    = sdsn_rel,
    sd_sqrtn_rel_sq = sdsn_rel^2,
    sdi_iqmh        = sdi_iqmh,
    sdi_report_calc = sdi_calc,
    sdi_report_entered = rsdi,
    sdi_check       = sdi_check,
    stringsAsFactors = FALSE, check.names = FALSE
  )[keep, , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "dropped") <- which(invalid)
  out
}

#' Root mean square of the relative bias
#'
#' [IQMH Appendix B]: "RMS_Bias = (Sum of SQ relative bias to reference
#' value / N)^0.5 ... where N is # of challenges"; steps: take relative
#' differences from the reference, square, sum, divide by N, square root.
#' Worked example: sum(%bias^2) = 38.08 over 9 challenges -> 2.057 %.
#'
#' @param rel_bias relative biases (fractions)
rms_bias <- function(rel_bias) {
  rel_bias <- rel_bias[!is.na(rel_bias)]
  if (!length(rel_bias)) return(NA_real_)
  sqrt(mean(rel_bias^2))
}

#' Standard uncertainty of the consensus (reference) value: (SD/sqrt N)group
#'
#' [IQMH Appendix C]: "(SD/Sqrt N)group = ((Sum of (Reference Value SD/SqrtN
#' expressed as value relative to reference value)^2) / Count of # reference
#' value SDs)^0.5" - the standard error of the group mean, i.e. the
#' uncertainty associated with the assigned value. Worked example result:
#' 0.007135541.  [IQMH Section 4, SRg.]
#'
#' IQMH defines it for the all-method mean; applied to a peer-group Mean (as
#' printed by CAP) it is the standard error of that peer mean.
#'
#' @param sd_sqrtn_rel per-challenge (S.D./sqrt(No. of Labs))/Mean
sd_sqrtn_group <- function(sd_sqrtn_rel) {
  sd_sqrtn_rel <- sd_sqrtn_rel[!is.na(sd_sqrtn_rel)]
  if (!length(sd_sqrtn_rel)) return(NA_real_)
  sqrt(mean(sd_sqrtn_rel^2))
}

#' Average SDI over challenges (IQMH alt. step 3d tests this average)
mean_sdi <- function(sdi) {
  sdi <- sdi[!is.na(sdi) & is.finite(sdi)]
  if (!length(sdi)) return(NA_real_)
  mean(sdi)
}


# ==============================================================================
# 4. BIAS FROM REPLICATE MEASUREMENTS OF A CERTIFIED REFERENCE MATERIAL (CRM)
# ==============================================================================

#' Bias and its uncertainty from replicate measurements of a CRM
#'
#' [IQMH Appendix A] (worked example: NIST 914a creatinine, 50 results,
#' mean 79.66, SD 0.872, SEM 0.123, B -1.64, B% -2.02, SEM% 0.152):
#'   mean, SD of the n replicates (n >= 10 recommended, IQMH Section 4)
#'   SEM   = SD / sqrt(n)                         [CLSI 3.6.5, Eq 5]
#'   B     = mean - certified value               (bias)
#'   B%    = 100 * B / certified value
#'   SEM%  = 100 * SEM / certified value
#'   u_ref = U_cert / k_cert  (certificate gives expanded U, usually k = 2;
#'                             IQMH App A: 0.37 (k=2) -> 0.185)
#' Uncertainty of the bias estimate [CLSI 8.3.2, Example 6a]:
#'   u_c(b) = sqrt(SEM^2 + u_ref^2)
#' Significance [CLSI 8.3.2]: if |B| > 2 * u_c(b), "the bias cannot be
#' asserted to be zero and consideration should be given to recalibrating ...
#' or by correcting for the observed bias" (CLSI 8.2). The uncertainty model
#' assumes significant bias has been corrected; this function only flags it.
#' SDI = |B%| / method CV%  (same definition as the EQA/PT pathway).
#'
#' @param replicates numeric vector of results on the CRM
#' @param certified certified value (reporting units)
#' @param U_cert certificate expanded uncertainty (reporting units)
#' @param k_cert certificate coverage factor
#' @param method_cv_pct IQC CV (percent) of the matched level, for the SDI
#' @return list
crm_bias_stats <- function(replicates, certified, U_cert, k_cert = 2,
                           method_cv_pct = NA) {
  x <- as_num(replicates); x <- x[!is.na(x)]
  n <- length(x)
  certified <- as_num(certified); U_cert <- as_num(U_cert); k_cert <- as_num(k_cert)
  if (n < 2 || is.na(certified) || certified == 0) {
    return(list(n = n, mean = if (n) mean(x) else NA_real_, sd = NA_real_,
                sem = NA_real_, bias = NA_real_, bias_rel = NA_real_,
                sem_rel = NA_real_, u_ref = NA_real_, u_ref_rel = NA_real_,
                u_c_b = NA_real_, u_c_b_rel = NA_real_, significant = NA,
                sdi = NA_real_, certified = certified))
  }
  m   <- mean(x)
  s   <- stats::sd(x)                                   # [CLSI Eq 3]
  sem <- s / sqrt(n)                                    # [CLSI Eq 5]
  u_ref <- if (is.na(U_cert)) 0 else standard_uncertainty(U_cert, "expanded", k = k_cert)
  b   <- m - certified
  ucb <- sqrt(sem^2 + u_ref^2)                          # [CLSI 8.3.2]
  list(n = n, mean = m, sd = s, sem = sem,
       bias = b, bias_rel = b / certified,
       sem_rel = sem / certified,
       u_ref = u_ref, u_ref_rel = u_ref / certified,
       u_c_b = ucb, u_c_b_rel = ucb / certified,
       significant = abs(b) > 2 * ucb,
       sdi = abs(b / certified) / (method_cv_pct / 100),
       certified = certified)
}


# ==============================================================================
# 5. COMBINATION
# ==============================================================================

#' Relative standard uncertainty of the calibrator (u_ref), per test
#'
#' From the manufacturer's calibrator traceability sheet (e.g. Roche C.f.a.s.
#' "Traceability and Uncertainty"), which gives per test the Calibrator Value
#' and, in the same units, standard uncertainties (k = 1):
#'   u_cal  : standard calibrator uncertainty of the manufacturer
#'   u_ccal : combined calibrator uncertainty, which adds the contribution of
#'            a higher-order reference material or method; given for some tests
#' Use u_ccal when given, otherwise u_cal. Both are standard uncertainties
#' (k = 1), so no coverage factor is applied. [IQMH, notes to Eq 4: u_ref is
#' the standard uncertainty of the calibrator traceable to a reference method,
#' obtained from the manufacturer.]
#'     u_ref = u / Calibrator Value          (relative; units cancel)
#'
#' @param cal_value Calibrator Value (any units)
#' @param cal_u     u_ccal or u_cal, same units as cal_value
#' @return relative standard uncertainty (fraction), NA if not available
calibrator_u_ref <- function(cal_value, cal_u) {
  v <- as_num(cal_value); u <- as_num(cal_u)
  if (length(v) != 1 || length(u) != 1 || is.na(v) || is.na(u) || v <= 0 || u < 0) return(NA_real_)
  u / v
}

#' Uncertainty associated with bias, EQA/PT pathway
#'
#' [IQMH Eq 2, right-hand branch] (peer-group / all-method mean from PT):
#'     u_B = [ RMS_bias^2 + SR_g^2 + u_ref^2 ]^(1/2)
#' with three terms (all relative standard uncertainties):
#'   RMS_bias : root mean square of the relative biases, your typical bias
#'              against the assigned values                    [IQMH App B]
#'   SR_g     : uncertainty of the EQA/PT assigned value itself (the report's
#'              consensus Mean): the standard error of that mean,
#'              (SD/sqrtN)group                                 [IQMH App C]
#'   u_ref    : uncertainty of the calibrator's assigned value, traceable to a
#'              reference method, from the manufacturer   [IQMH, notes to Eq 4]
#'              (calibrator_u_ref())
#'
#' SR_g and u_ref are the reference terms. Whether they are included depends
#' on the size of the bias relative to the method's imprecision (mean SDI):
#'   [IQMH alt. step 3d]: SDI <= 2 -> RMS only; SDI > 2 -> RMS and the
#'   interlaboratory SD/sqrtN or Uref.
#'   [IQMH, note after the ACR example]: if the actual bias is no more than
#'   twice the method SD, the reference-value term may be excluded.
#' rule = "sdi"    : include SR_g and u_ref only when mean SDI > sdi_limit
#'        "always" : always include them (IQMH Eq 2 as written)
#'        "never"  : never include them
#' If the mean SDI cannot be computed under rule "sdi", they are included
#' (conservative). A missing u_ref (NA) counts as 0; mu_checks() then warns
#' whenever the reference terms are included.
#'
#' @param rms       RMS relative bias (fraction)
#' @param srg       SR_g, relative standard uncertainty of the assigned value
#' @param mean_sdi  average SDI (IQMH definition)
#' @param u_ref     relative standard uncertainty of the calibrator (NA if none)
#' @return list(value, included, srg, u_ref, reason)
u_bias_ept <- function(rms, srg, mean_sdi,
                       rule = c("sdi", "always", "never"), sdi_limit = 2,
                       u_ref = NA_real_) {
  rule <- match.arg(rule)
  if (is.na(srg)) srg <- 0
  u_ref_used <- if (is.na(u_ref)) 0 else u_ref
  include <- switch(rule,
    sdi    = if (is.na(mean_sdi)) TRUE else mean_sdi > sdi_limit,
    always = TRUE,
    never  = FALSE)
  terms <- "SR_g and u_ref"
  reason <- switch(rule,
    sdi = if (is.na(mean_sdi)) {
            sprintf("Mean SDI not available: %s included (conservative).", terms)
          } else if (include) {
            sprintf("Mean SDI = %.2f > %g: %s included [IQMH alt. step 3d(ii)].", mean_sdi, sdi_limit, terms)
          } else {
            sprintf("Mean SDI = %.2f \u2264 %g: %s omitted [IQMH alt. step 3d(i)].", mean_sdi, sdi_limit, terms)
          },
    always = sprintf("Rule 'always': %s included [IQMH Eq 2].", terms),
    never  = sprintf("Rule 'never': %s omitted.", terms))
  value <- if (include) combine_rss(rms, srg, u_ref_used) else rms
  list(value = value, included = include, srg = srg, u_ref = u_ref, reason = reason)
}

#' Uncertainty associated with bias, CRM pathway
#'
#' [IQMH Eq 2, left-hand branch] (higher-order reference measurement
#' procedure / CRM): uB = [SEM^2 + uref^2]^(1/2)   (relative values)
#' [CLSI 8.3.2]: u_c(b) = sqrt(u_c(xbar)^2 + u(x_CRM)^2)
u_bias_crm <- function(sem_rel, u_ref_rel) {
  combine_rss(sem_rel, if (is.na(u_ref_rel)) 0 else u_ref_rel)
}

#' Root-sum-of-squares combination of independent standard uncertainties
#'
#' [IQMH Eq 3]  uc = [ (us)^2 + (uB)^2 ]^(1/2)
#' [IQMH Eq 4]  uc = [ (us)^2 + (SEM^2 or RMS^2) + (SRg)^2 + (uref)^2 ]^(1/2)
#' [CLSI 3.6.9.2, Eq 10] u(x +/- y) = sqrt(u(x)^2 + u(y)^2) for independent
#' inputs. Relative uncertainties combine the same way for products/quotients
#' [CLSI Eq 11]; here all terms are relative to the same measured value.
combine_rss <- function(...) {
  v <- c(...)
  sqrt(sum(v^2))
}

#' Expanded uncertainty
#'
#' [IQMH Eq 5]  U = uc x 1.96 (~2) ("represents a 95% confidence level")
#' [CLSI 3.6.10, Eq 12]  U(x) = k x uc(x)
expanded_uncertainty <- function(uc, k = 2) k * uc


# ==============================================================================
# 6. ORCHESTRATION: ONE LEVEL, POOLED LEVELS, SUMMARY, CHECKS
# ==============================================================================

#' Complete MU calculation for one level
#'
#' Steps follow [IQMH] "A Top-Down Alternative Approach":
#'   2  determine method imprecision from IQC          -> u_prec
#'   3a determine bias from EQA/PT (or CRM)
#'   3c express bias as SDI
#'   3d test SDI and decide on the reference-uncertainty term -> u_bias
#'   4  combined uncertainty u_c = sqrt(u_s^2 + u_B^2)  (code: u_prec, u_bias)
#'   5  expanded uncertainty U = k * u_c
#'   6  express U relative (%) and absolute (U * IQC mean)
#'
#' @param qc list(level, material, lot, data = data.frame(Date, Mean, SD,
#'        Points))
#' @param bias list(source = "ept" | "crm" | "none",
#'        ept = list(provider, peer_group,
#'                   data = data.frame(Survey, Specimen, Your_Result, Mean,
#'                                     S.D., No_of_Labs, S.D.I)),
#'        crm = list(material, certified, U_cert, k_cert, replicates))
#' @param settings see mu_default_settings()
#' @param calibrator list(value, u): Calibrator Value and u_ccal (or u_cal) of
#'        the test, from the manufacturer's traceability sheet (calibrator_u_ref())
#' @return nested list holding every intermediate result
mu_level <- function(qc, bias, settings = mu_default_settings(), calibrator = NULL) {
  s <- utils::modifyList(mu_default_settings(), settings)
  if (!s$imprecision %in% names(IMPRECISION_LABELS)) s$imprecision <- "pooled"
  d <- qc$data

  # --- Step 2: imprecision ---------------------------------------------------
  qtab <- qc_month_table(d$Date, d$Mean, d$SD, d$Points)
  aov  <- qc_anova_months(qtab)
  cv_in_use <- if (s$imprecision == "pooled_shifts") aov$cv_pooled_shifts_pct else aov$cv_pooled_pct
  u_prec  <- cv_in_use / 100
  qc_mean <- aov$grand_mean
  prec <- list(table = qtab, anova = aov, estimator = s$imprecision,
               label = unname(IMPRECISION_LABELS[s$imprecision]),
               cv_pct = cv_in_use, u_prec = u_prec, qc_mean = qc_mean)

  # --- Step 3: bias ------------------------------------------------------------
  src <- if (is.null(bias$source)) "none" else bias$source
  b <- list(source = src, u_bias = 0, reason = "", n = 0)
  if (src == "ept") {
    e  <- bias$ept; ed <- e$data
    et <- ept_challenge_table(ed$Survey, ed$Specimen, ed$Your_Result, ed$Mean,
                              ed[["S.D."]], ed$No_of_Labs,
                              method_cv_pct = cv_in_use, report_sdi = ed[["S.D.I"]])
    rms   <- rms_bias(et$rel_bias)
    srg   <- sd_sqrtn_group(et$sd_sqrtn_rel)        # SR_g [IQMH App C]
    msdi  <- mean_sdi(et$sdi_iqmh)
    u_ref <- calibrator_u_ref(calibrator$value, calibrator$u)
    ub <- u_bias_ept(rms, srg, msdi, rule = s$bias_rule, sdi_limit = s$sdi_limit, u_ref = u_ref)
    b <- c(b[c("source")], list(
      table = et, n = nrow(et), n_surveys = length(unique(et$Survey[et$Survey != ""])),
      mean_ref = if (nrow(et)) mean(et$Mean) else NA_real_,
      mean_result = if (nrow(et)) mean(et$Your_Result) else NA_real_,
      range_ref = if (nrow(et)) range(et$Mean) else c(NA_real_, NA_real_),
      rms = rms, group = srg, srg = srg, mean_sdi = msdi,
      u_ref = u_ref, included = ub$included, u_bias = ub$value, reason = ub$reason,
      peer_group = e$peer_group, provider = e$provider))
  } else if (src == "crm") {
    cm <- bias$crm
    cs <- crm_bias_stats(cm$replicates, cm$certified, cm$U_cert,
                         k_cert = if (is.null(cm$k_cert)) 2 else cm$k_cert,
                         method_cv_pct = cv_in_use)
    ubv <- if (is.na(cs$sem_rel)) NA_real_ else u_bias_crm(cs$sem_rel, cs$u_ref_rel)
    b <- c(b[c("source")], list(
      crm = cs, n = cs$n, mean_ref = cs$certified, mean_result = cs$mean,
      u_bias = ubv, included = TRUE, material = cm$material,
      reason = "CRM pathway: u_bias = \u221a(SEM\u00b2 + u_ref\u00b2) [IQMH Eq 2 left; CLSI 8.3.2]."))
  } else {
    b$reason <- "Bias not considered: u_bias = 0."
    b$mean_ref <- NA_real_; b$mean_result <- NA_real_
  }

  # --- Steps 4-6: combine, expand, express ------------------------------------
  u_bias <- if (is.na(b$u_bias)) NA_real_ else b$u_bias
  uc     <- if (is.na(u_prec) || is.na(u_bias)) NA_real_ else combine_rss(u_prec, u_bias)
  U      <- expanded_uncertainty(uc, s$k)
  U_abs  <- U * qc_mean

  res <- list(qc = qc[c("level", "material", "lot")],
              prec = prec, bias = b,
              u_prec = u_prec, u_bias = u_bias, uc = uc, k = s$k,
              U = U, U_pct = 100 * U, U_abs = U_abs, qc_mean = qc_mean,
              settings = s)
  res$checks <- mu_checks(res)
  res
}

#' Pooled MU across all entered levels
#'
#' Always reported alongside the per-level results when >= 2 levels are
#' entered. [IQMH Eq 1 note] says a pooled estimate suits methods whose
#' control levels perform comparably across the measuring interval; whether
#' to use the pooled or the per-level values is the reviewer's decision (a
#' level can also be left out by not entering it).
#'
#' Imprecision: pool_qc_levels(), weights = within-month degrees of freedom.
#' Bias: the per-level u_bias^2 are averaged with weights w_i:
#'     u_bias_pooled = sqrt( sum(w_i * u_bias_i^2) / sum(w_i) )
#'   EQA/PT: w_i = number of challenges. When every level uses RMS only, this
#'        equals the RMS over all challenges together, which is how
#'        [IQMH App B] computes RMS (one RMS from all challenges, all levels).
#'   CRM: w_i = 1 (equal weight per level).
#' All levels must use the same bias source; otherwise an error message is
#' returned instead. No absolute U is given (there is no single concentration).
#'
#' @param levels list of mu_level() results
#' @return list, or NULL when fewer than 2 levels
mu_pooled <- function(levels, k = 2) {
  sel <- levels
  if (length(sel) < 2) return(NULL)
  srcs <- unique(vapply(sel, function(r) r$bias$source, ""))
  if (length(srcs) != 1) {
    return(list(error = "Levels use different bias sources (EQA/PT, CRM, none); pooled estimate not computed."))
  }
  dfw <- vapply(sel, function(r) as.numeric(r$prec$anova$df_within), 0)
  cvs <- vapply(sel, function(r) as.numeric(r$prec$cv_pct), 0)
  cv_pool <- pool_qc_levels(dfw, cvs)
  ub  <- vapply(sel, function(r) as.numeric(r$u_bias), 0)
  w   <- if (srcs == "ept") vapply(sel, function(r) as.numeric(r$bias$n), 0) else rep(1, length(sel))
  ok  <- !is.na(ub) & w > 0
  ub_pool <- if (srcs == "none") 0 else sqrt(sum(w[ok] * ub[ok]^2) / sum(w[ok]))
  u_prec <- cv_pool / 100
  uc <- combine_rss(u_prec, ub_pool)
  U  <- expanded_uncertainty(uc, k)
  list(levels = vapply(sel, function(r) as.character(r$qc$level), ""),
       n_points = sum(vapply(sel, function(r) r$prec$anova$n_points, 0)),
       df_within = dfw, cv_pct = cv_pool, u_prec = u_prec,
       bias_source = srcs, weights = w, u_bias = ub_pool,
       uc = uc, k = k, U = U, U_pct = 100 * U,
       qc_mean_range = range(vapply(sel, function(r) r$qc_mean, 0)))
}

#' Data-quality and interpretation checks for one level
#'
#' Minimum-data expectations:
#'   IQC : >= 6 months [IQMH Section 4: "A minimum of six months' data is
#'         recommended"]; >= 30 results for an interim SD [IQMH Section 4].
#'   EQA/PT : >= 3 surveys [IQMH Section 4: "calculate the RMS from peer group
#'         means in PT performance data from at least three different
#'         surveys, each with multiple levels"].
#'   CRM : >= 10 replicates [IQMH Section 4 / App A].
#' @return data.frame(severity, message); severity is "error", "alert" (result
#'         computed but understated; shown in red), "warning" or "info"
mu_checks <- function(res) {
  msgs <- character(0); sev <- character(0)
  add <- function(s, m) { sev <<- c(sev, s); msgs <<- c(msgs, m) }
  p <- res$prec; a <- p$anova; s <- res$settings

  if (a$n_months == 0) add("error", "No valid IQC rows (each row needs Mean, SD and # Points \u2265 2).")
  if (length(attr(p$table, "dropped")))
    add("warning", sprintf("IQC row(s) %s ignored: Mean, SD and # Points (\u2265 2) are required.",
                           paste(attr(p$table, "dropped"), collapse = ", ")))
  if (a$n_months > 0 && a$n_months < 6)
    add("warning", sprintf("IQC covers %d monthly report(s); IQMH recommends a minimum of six months.",
                           a$n_months))
  if (a$n_points > 0 && a$n_points < 30)
    add("warning", sprintf("Only %d IQC results; IQMH requires at least 30 for an interim SD.",
                           a$n_points))
  if (a$n_months == 1 && p$estimator == "pooled_shifts")
    add("error", "Month-to-month shifts need at least two months of IQC.")
  if (!is.na(a$cv_pooled_shifts_pct) && !is.na(a$cv_pooled_pct) && a$cv_pooled_pct > 0) {
    ratio <- a$cv_pooled_shifts_pct / a$cv_pooled_pct
    if (ratio > 1.2 && p$estimator == "pooled")
      add("info", sprintf(paste0("Pooled monthly CV with month-to-month shifts (%.2f %%) is %.0f %% larger than ",
                                 "the pooled monthly CV (%.2f %%): the monthly means shift (lot changes, ",
                                 "recalibration). CLSI C51-A \u00a77.2\u20137.3 treats such shifts as long-term random ",
                                 "variation."),
                          a$cv_pooled_shifts_pct, 100 * (ratio - 1), a$cv_pooled_pct))
  }

  b <- res$bias
  if (b$source == "none") {
    add("warning", "Bias not considered. IQMH VI.8: bias shall be considered when sufficient data are available.")
  } else if (b$source == "ept") {
    if (b$n == 0) add("error", "No valid EQA/PT rows (each row needs Your Result and Mean).")
    if (length(attr(b$table, "dropped")))
      add("warning", sprintf("EQA/PT row(s) %s ignored: Your Result and Mean are required.",
                             paste(attr(b$table, "dropped"), collapse = ", ")))
    if (b$n > 0 && b$n_surveys < 3)
      add("warning", sprintf("EQA/PT data from %d survey(s); IQMH asks for at least three surveys.",
                             b$n_surveys))
    if (any(b$table$sdi_check == "MISMATCH"))
      add("warning", sprintf(paste0("Entered S.D.I differs from (Your Result \u2212 Mean)/S.D. in EQA/PT row(s) %s: ",
                                    "check transcription."),
                             paste(which(b$table$sdi_check == "MISMATCH"), collapse = ", ")))
    if (b$n > 0 && is.na(b$group))
      add("warning", "S.D. or No. of Labs missing for all EQA/PT rows: SR_g cannot be computed (taken as 0).")
    # The calculation continues without u_ref, but u_B is then underestimated
    # whenever the reference terms are required (shown in red by the app).
    if (b$n > 0 && isTRUE(b$included) && is.na(b$u_ref))
      add("alert", sprintf(paste0("Uncertainty of calibrator (u_ref) not entered, but the reference terms are required here ",
                                  "(%s): u_B and U are underestimated. Enter the Calibrator Value and u_ccal (or u_cal) ",
                                  "in the sidebar."),
                           if (identical(s$bias_rule, "sdi") && !is.na(b$mean_sdi))
                             sprintf("mean SDI %.2f > %g", b$mean_sdi, s$sdi_limit) else paste0("rule '", s$bias_rule, "'")))
    if (!is.na(b$mean_sdi) && b$mean_sdi > s$sdi_limit)
      add("info", sprintf(paste0("Mean SDI %.2f > %g: bias is large relative to imprecision. ",
                                 "Investigate and consider correcting it (CLSI C51-A \u00a78.2)."),
                          b$mean_sdi, s$sdi_limit))
    if (!is.na(b$mean_ref) && !is.na(res$qc_mean) && res$qc_mean > 0) {
      r <- b$mean_ref / res$qc_mean
      if (r > 2 || r < 0.5)
        add("info", sprintf("Mean EQA/PT value (%.4g) is %.1f\u00d7 the IQC mean (%.4g): confirm the EQA/PT and IQC level matching.",
                            b$mean_ref, r, res$qc_mean))
    }
  } else if (b$source == "crm") {
    cs <- b$crm
    if (is.na(cs$certified)) add("error", "CRM certified value missing.")
    if (cs$n < 10) add("warning", sprintf("%d CRM replicate(s); IQMH recommends a minimum of N = 10.", cs$n))
    if (isTRUE(cs$significant))
      add("warning", sprintf(paste0("CRM bias %.4g (%.2f %%) exceeds 2\u00d7u_c(b) = %.4g: bias is significant. ",
                                    "Consider recalibration or correction (CLSI C51-A \u00a78.2\u20138.3)."),
                             cs$bias, 100 * cs$bias_rel, 2 * cs$u_c_b))
  }
  if (!is.na(s$target_U_pct) && !is.na(res$U_pct) && res$U_pct > s$target_U_pct)
    add("warning", sprintf("U = %.2f %% exceeds the target of %.2f %%.", res$U_pct, s$target_U_pct))
  data.frame(severity = sev, message = msgs, stringsAsFactors = FALSE)
}

#' Summary table: one row per quantity, one column per level (+ pooled)
#'
#' Row labels use the IQMH notation (u_s, u_B, SR_g, u_ref, u_c, U) with a
#' plain-language description.
#'
#' @param levels list of mu_level() results
#' @param pooled mu_pooled() result or NULL
#' @param units reporting units, for the absolute statement
#' @return data.frame of character
mu_summary <- function(levels, pooled = NULL, units = "") {
  f  <- function(x, d = 4) ifelse(is.na(x), "\u2014", formatC(signif(x, d), format = "fg", digits = d, flag = "#"))
  pc <- function(x, d = 3) ifelse(is.na(x), "\u2014", sprintf(paste0("%.", d, "f %%"), 100 * x))
  short <- c(pooled = "pooled monthly CV", pooled_shifts = "with month-to-month shifts")
  ept <- function(r) r$bias$source == "ept"
  crm <- function(r) r$bias$source == "crm"
  L <- list(
    level = "QC level (as on report)",
    mat   = "Control material / lot",
    mean  = "IQC mean x\u0304 (all results)",
    nm    = "IQC results N / months m",
    cvp   = "Pooled monthly CV",
    cvs   = "Pooled monthly CV with month-to-month shifts",
    us    = "u_s \u2014 imprecision (CV in use)",
    src   = "Bias source",
    ref   = "Mean EQA/PT assigned value / CRM certified value",
    res   = "Mean Your Result / mean of CRM replicates",
    n     = "EQA/PT challenges J / CRM replicates n",
    sdi   = "Mean SDI (|relative bias| / CV)",
    rms   = "RMS_bias (EQA/PT) / SEM (CRM), relative",
    srg   = "SR_g \u2014 uncertainty of EQA/PT assigned value",
    inc   = "SR_g and u_ref included?",
    uref  = "u_ref \u2014 uncertainty of calibrator (EQA/PT) / CRM value",
    ub    = "u_B \u2014 uncertainty of bias",
    uc    = "u_c = \u221a(u_s\u00b2 + u_B\u00b2)",
    k     = "k",
    U     = "U = k \u00b7 u_c (relative)",
    Uabs  = "U (absolute, at x\u0304)",
    stmt  = "Result statement (CLSI \u00a79.2)")
  rows <- list(
    level = function(r) as.character(r$qc$level),
    mat   = function(r) trimws(paste(r$qc$material, if (nzchar(r$qc$lot %||% "")) paste0("(lot ", r$qc$lot, ")") else "")),
    mean  = function(r) f(r$qc_mean),
    nm    = function(r) sprintf("%d / %d", as.integer(r$prec$anova$n_points), as.integer(r$prec$anova$n_months)),
    cvp   = function(r) pc(r$prec$anova$cv_pooled_pct / 100),
    cvs   = function(r) pc(r$prec$anova$cv_pooled_shifts_pct / 100),
    us    = function(r) paste0(pc(r$u_prec), " [", short[[r$prec$estimator]], "]"),
    src   = function(r) switch(r$bias$source, ept = "EQA/PT", crm = "CRM", none = "none"),
    ref   = function(r) f(r$bias$mean_ref),
    res   = function(r) f(r$bias$mean_result),
    n     = function(r) as.character(r$bias$n),
    sdi   = function(r) if (ept(r)) f(r$bias$mean_sdi, 3) else if (crm(r)) f(r$bias$crm$sdi, 3) else "\u2014",
    rms   = function(r) if (ept(r)) pc(r$bias$rms) else if (crm(r)) pc(r$bias$crm$sem_rel) else "\u2014",
    srg   = function(r) if (ept(r)) pc(r$bias$srg) else "\u2014",
    inc   = function(r) if (ept(r)) (if (isTRUE(r$bias$included)) "yes" else "no") else "\u2014",
    uref  = function(r) if (ept(r)) (if (is.na(r$bias$u_ref)) "not entered" else pc(r$bias$u_ref))
                        else if (crm(r)) pc(r$bias$crm$u_ref_rel) else "\u2014",
    ub    = function(r) pc(r$u_bias),
    uc    = function(r) pc(r$uc),
    k     = function(r) format(r$k),
    U     = function(r) pc(r$U, 2),
    Uabs  = function(r) paste(f(r$U_abs), units),
    stmt  = function(r) format_x_pm_U(r$qc_mean, r$U_abs, units)
  )
  out <- data.frame(Quantity = unlist(L[names(rows)]), stringsAsFactors = FALSE, check.names = FALSE)
  for (i in seq_along(levels)) {
    r <- levels[[i]]
    out[[paste("Level", i)]] <- unname(vapply(rows, function(fn) {
      v <- tryCatch(fn(r), error = function(e) "\u2014")
      if (length(v) != 1 || is.na(v)) "\u2014" else as.character(v)
    }, ""))
  }
  if (!is.null(pooled) && is.null(pooled$error)) {
    pv <- setNames(rep("\u2014", length(rows)), names(rows))
    pv["level"] <- paste(pooled$levels, collapse = " + ")
    pv["mean"]  <- paste(f(pooled$qc_mean_range[1]), "\u2013", f(pooled$qc_mean_range[2]))
    pv["nm"]    <- as.character(pooled$n_points)
    pv["us"]    <- paste0(pc(pooled$u_prec), " [pooled across levels]")
    pv["src"]   <- switch(pooled$bias_source, ept = "EQA/PT", crm = "CRM", none = "none")
    pv["ub"]    <- pc(pooled$u_bias)
    pv["uc"]    <- pc(pooled$uc)
    pv["k"]     <- format(pooled$k)
    pv["U"]     <- pc(pooled$U, 2)
    pv["stmt"]  <- sprintf("\u00b1 %s %% of result", format(round_up_sig(pooled$U_pct, 2)))
    out[["Pooled"]] <- unname(pv)
  }
  rownames(out) <- NULL
  out
}

# Null-coalescing helper (base R >= 4.4 has `%||%`; defined for older R).
if (!exists("%||%", mode = "function")) `%||%` <- function(a, b) if (is.null(a)) b else a
