################################################################################
#  Validation of app/mu_topdown_core.R against published worked examples
#  (IQMH guidance appendices, CLSI C51-A examples) and, for the bias and
#  combination steps, the intermediate values of the acetaminophen
#  spreadsheet the example data come from.
#
#  Run from the project root:   Rscript tests/test_mu_topdown_core.R
#  Exits with status 1 if any check fails.
################################################################################

# readxl / writexl (Excel save/load) may be installed in the project library
if (dir.exists(".Rlib")) .libPaths(c(normalizePath(".Rlib"), .libPaths()))
source("app/mu_topdown_core.R")
source("app/mu_io.R")

n_pass <- 0L; n_fail <- 0L
check <- function(label, got, want, tol = 1e-9) {
  ok <- length(got) == length(want) && all(!is.na(got)) && all(abs(got - want) <= tol)
  if (ok) {
    n_pass <<- n_pass + 1L
  } else {
    n_fail <<- n_fail + 1L
    cat(sprintf("FAIL  %s\n      got  %s\n      want %s (tol %g)\n", label,
                paste(format(got, digits = 10), collapse = ", "),
                paste(format(want, digits = 10), collapse = ", "), tol))
  }
}
check_true <- function(label, cond) check(label, as.numeric(isTRUE(cond)), 1, 0)

# Run (or month) summaries -> qc_month_table(), as the app does with monthly reports
summ <- function(groups) qc_month_table(seq_along(groups), sapply(groups, mean), sapply(groups, sd),
                                        lengths(groups))

# ------------------------------------------------------------------------------
# IMPRECISION: one-way ANOVA from group summaries (CLSI C51-A 7.3, Eqs 27-29)
# ------------------------------------------------------------------------------
# CLSI Example 4 (Table 2): P-Creatinine, 5 runs x 5 replicates
ex4 <- list(c(140, 140, 140, 141, 140), c(138, 139, 138, 137, 139), c(143, 144, 144, 145, 143),
            c(143, 143, 142, 143, 142), c(142, 143, 141, 142, 143))
a4 <- qc_anova_months(summ(ex4))
check("CLSI Ex 4: MS_between 24.4", a4$ms_between, 24.4, 1e-9)
check("CLSI Ex 4: MS_within 0.52",  a4$ms_within, 0.52, 1e-9)
check("CLSI Ex 4: n0 = 5",          a4$n0, 5, 1e-12)
check("CLSI Ex 4: s_wth 0.72",      a4$s_within, 0.72, 0.005)
check("CLSI Ex 4: s_btw 2.19",      a4$s_between, 2.19, 0.005)
check("CLSI Ex 4: uc 2.30",         a4$s_within_between, 2.30, 0.005)

# CLSI Example 6a (Table 3): CRM, 6 runs x duplicates
ex6 <- list(c(6.0, 6.1), c(6.1, 6.3), c(5.9, 6.0), c(6.0, 5.9), c(5.8, 6.1), c(6.0, 6.3))
a6 <- qc_anova_months(summ(ex6))
check("CLSI Ex 6a: MS_within 0.02083",  a6$ms_within, 0.02083, 5e-6)
check("CLSI Ex 6a: MS_between 0.02483", a6$ms_between, 0.02483, 5e-6)
check("CLSI Ex 6a: s_wth 0.144",        a6$s_within, 0.144, 0.0005)
check("CLSI Ex 6a: s_btw 0.045",        a6$s_between, 0.045, 0.0005)
check("CLSI Ex 6a: uc 0.151",           a6$s_within_between, 0.151, 0.0005)

# CLSI Appendix B (Table B1): routine cholesterol QC, 42 runs x 3; run 4 excluded
b1 <- list(c(7.9, 7.9, 8.1), c(7.5, 7.3, 7.6), c(7.2, 7.2, 7.2), c(6.6, 6.8, 6.5), c(7.3, 7.3, 7.3),
           c(7.6, 7.5, 7.6), c(7.5, 7.5, 7.5), c(8.0, 8.0, 8.0), c(7.3, 7.3, 7.3), c(7.8, 7.8, 7.9),
           c(7.8, 7.8, 8.0), c(7.4, 7.5, 7.4), c(7.5, 7.6, 7.4), c(7.7, 7.6, 7.7), c(7.9, 7.9, 8.0),
           c(8.0, 8.0, 8.2), c(7.8, 7.8, 7.7), c(7.9, 8.1, 7.9), c(7.5, 7.6, 7.7), c(7.6, 7.6, 7.8),
           c(7.8, 7.6, 7.7), c(7.9, 7.9, 7.8), c(7.6, 7.7, 7.6), c(7.4, 7.6, 7.4), c(7.3, 7.2, 7.4),
           c(7.7, 7.5, 7.7), c(7.8, 7.9, 7.7), c(8.0, 8.0, 8.1), c(7.9, 8.1, 7.8), c(7.7, 7.6, 7.7),
           c(7.9, 7.7, 7.8), c(7.5, 7.5, 7.5), c(7.4, 7.4, 7.6), c(7.5, 7.5, 7.4), c(7.7, 7.7, 7.6),
           c(7.6, 7.6, 7.8), c(7.8, 7.9, 7.7), c(7.8, 7.8, 7.8), c(8.0, 7.9, 8.0), c(7.8, 7.7, 7.8),
           c(7.9, 8.0, 7.9), c(7.8, 7.8, 7.9))[-4]
aB <- qc_anova_months(summ(b1))
check("CLSI App B: 123 results / 41 runs", c(aB$n_points, aB$n_months), c(123, 41), 0)
check("CLSI App B: SS_between 6.09 (df 40)", c(aB$ss_between, aB$df_between), c(6.09, 40), 0.005)
check("CLSI App B: SS_within 0.58 (df 82)",  c(aB$ss_within, aB$df_within), c(0.58, 82), 0.005)
check("CLSI App B: MS_within 0.00707",  aB$ms_within, 0.00707, 5e-6)
check("CLSI App B: MS_between 0.152",   aB$ms_between, 0.152, 0.0005)
check("CLSI App B: s_wth 0.084",        aB$s_within, 0.084, 0.0005)
check("CLSI App B: s_btw 0.220",        aB$s_between, 0.220, 0.0005)
check("CLSI App B: uc 0.235",           aB$s_within_between, 0.235, 0.0005)
check("CLSI App B: grand mean 7.69",    aB$grand_mean, 7.69, 0.005)
check("CLSI App B: %U = 6.1",           round(100 * 2 * aB$s_within_between / aB$grand_mean, 1), 6.1, 1e-9)

# Unbalanced groups: summaries reproduce lm()/anova() exactly; n0 matches CLSI footnote b
set.seed(20261027)
raw <- lapply(1:7, function(m) rnorm(sample(15:32, 1), mean = 100 + 2 * m, sd = 3))
au  <- qc_anova_months(summ(raw))
g   <- factor(rep(seq_along(raw), lengths(raw)))
tab <- anova(lm(unlist(raw) ~ g))
check("Unbalanced: MS_within == anova()",  au$ms_within,  tab[["Mean Sq"]][2], 1e-9)
check("Unbalanced: MS_between == anova()", au$ms_between, tab[["Mean Sq"]][1], 1e-9)
check("Unbalanced: grand mean == mean(all)", au$grand_mean, mean(unlist(raw)), 1e-10)
nn <- lengths(raw)
check("Unbalanced: n0 == nbar - s_n^2/N (CLSI fn b)", au$n0, mean(nn) - var(nn) / sum(nn), 1e-10)
check_true("Shifts estimate >= pooled monthly estimate", au$s_within_between >= au$s_within)
check("Pooled monthly SD == CLSI Eq 6", au$s_within,
      sqrt(sum((nn - 1) * sapply(raw, sd)^2) / (sum(nn) - length(nn))), 1e-12)
# Means that do not shift beyond within-month noise: s_b = 0, both estimates equal
flat <- qc_anova_months(qc_month_table(1:3, c(10, 10, 10), c(1, 1, 1), c(20, 20, 20)))
check("No month-to-month shift: s_b = 0", flat$s_between, 0, 0)
check_true("Single month: shifts estimate not available",
           is.na(qc_anova_months(qc_month_table("m", 10, 1, 20))$cv_pooled_shifts_pct))
check_true("Month with # Points < 2 is dropped",
           identical(attr(qc_month_table(c("a", "b"), c(10, 11), c(1, 1), c(20, 1)), "dropped"), 2L))

# Across-level pooling uses within-month degrees of freedom (CLSI Eq 6 on CV)
check("pool_qc_levels: df-weighted", pool_qc_levels(c(100, 50), c(2, 4)), sqrt((100 * 4 + 50 * 16) / 150), 1e-12)

# CV computed from SD reproduces the one-decimal CV printed on the Unity report
unity <- qc_month_table(c("TBIL L1", "TBIL L3", "CL L1", "CL L3", "TBIL L1 cum"),
                        mean = c(9.69, 128.7, 81.47, 124.5, 9.01), sd = c(0.576, 2.34, 0.670, 0.716, 1.16),
                        points = c(31, 31, 32, 32, 114))
check("Unity: 100*SD/Mean rounds to printed CV", unity$CV, c(5.9, 1.8, 0.8, 0.6, 12.9), 0.05)

# ------------------------------------------------------------------------------
# BIAS: IQMH Appendices B (RMS), C (SD/sqrtN), A (CRM)
# ------------------------------------------------------------------------------
assigned <- c(88, 235, 85, 277, 246, 84, 665, 91, 97)
result   <- c(86, 236, 83, 279, 254, 83, 676, 94, 98)
check("IQMH App B: RMS bias % = 2.057", 100 * rms_bias((result - assigned) / assigned), 2.057, 0.001)
labs   <- c(39, 39, 38, 73, 73, 72, 78, 77, 78)
ref_sd <- c(5.4, 6.7, 6.1, 8.3, 6.9, 7.3, 18.4, 5.4, 4)
check("IQMH App C: (SD/sqrtN)group", sd_sqrtn_group(ref_sd / sqrt(labs) / assigned), 0.007135541, 1e-9)
et <- ept_challenge_table(rep("S", 9), LETTERS[1:9], result, assigned, ref_sd, labs, method_cv_pct = 3)
check("IQMH App C: per-row SD/sqrtN (row 1)", et$sd_sqrtn[1], 0.86469203, 1e-8)
check("IQMH App C via ept_challenge_table", sd_sqrtn_group(et$sd_sqrtn_rel), 0.007135541, 1e-9)

crm_vals <- c(79.1, 80.1, 80.9, 80.9, 80.7, 80.5, 79.6, 79.2, 80.5, 79.6,
              79.9, 78.1, 80.7, 80.6, 80.7, 79.9, 78.2, 78.4, 80.2, 79.9,
              78.4, 78.8, 78.4, 80.0, 80.4, 78.7, 79.9, 78.7, 80.3, 78.8,
              80.3, 79.1, 78.8, 80.1, 79.5, 80.5, 80.1, 78.4, 78.2, 79.6,
              78.5, 80.0, 80.8, 79.9, 80.5, 78.8, 81.0, 79.5, 78.9, 80.5)
cs <- crm_bias_stats(crm_vals, certified = 81.3, U_cert = 0.37, k_cert = 2)
check("IQMH App A: n", cs$n, 50, 0)
check("IQMH App A: mean 79.66", cs$mean, 79.66, 0.005)
check("IQMH App A: SD 0.872", cs$sd, 0.872, 0.0005)
check("IQMH App A: SEM 0.123", cs$sem, 0.123, 0.0005)
check("IQMH App A: bias -1.64", cs$bias, -1.64, 0.005)
# IQMH derived B% from the rounded mean: (79.66 - 81.3)/81.3 = -2.017 -> "-2.02";
# the unrounded mean 79.662 gives -2.015, hence the slightly wider tolerance.
check("IQMH App A: bias % -2.02", 100 * cs$bias_rel, -2.02, 0.006)
check("IQMH App A: SEM % 0.152", 100 * cs$sem_rel, 0.152, 0.0005)
check("IQMH App A: u_Cref 0.185", cs$u_ref, 0.185, 1e-12)
check("IQMH App A: u_Cref % 0.228", 100 * cs$u_ref_rel, 0.228, 0.0005)
check_true("IQMH App A: bias significant (|-1.64| > 2*u_c(b))", cs$significant)

# CAP C-B 2026 Iron (EPT example.png): printed S.D.I = 0.0, +1.6, -1.3, +0.3, 0.0
iron <- ept_challenge_table(rep("C-B 2026", 5), c("CHM-06", "CHM-07", "CHM-08", "CHM-09", "CHM-10"),
                            your_result = c(6, 45, 15, 21, 30), mean = c(6.0, 43.9, 15.5, 20.9, 30.0),
                            sd = c(0.2, 0.7, 0.4, 0.4, 0.5), no_of_labs = c(647, 667, 663, 670, 666),
                            method_cv_pct = 2, report_sdi = c("0.0", "+1.6", "-1.3", "+0.3", "0.0"))
check("CAP iron: report-style SDI within \u00b10.05 of printed",
      as.numeric(all(abs(iron$sdi_report_calc - c(0, 1.6, -1.3, 0.3, 0)) <= 0.05 + 1e-9)), 1, 0)
check_true("CAP iron: no S.D.I mismatch flags", all(iron$sdi_check == "ok"))
check_true("CAP iron: wrong S.D.I is flagged",
           ept_challenge_table("x", "y", 45, 43.9, 0.7, 667, 2, report_sdi = "-1.6")$sdi_check == "MISMATCH")

# Bias-inclusion rule switches (IQMH alt. step 3d)
# u_B = sqrt(RMS^2 + [SR_g^2 if mean SDI > limit] + u_ref^2)  (IQMH Eq 2; alt. step 3d)
check("SDI rule: SDI 1.5 -> SR_g omitted", u_bias_ept(0.03, 0.01, 1.5, rule = "sdi")$value, 0.03, 1e-15)
check("SDI rule: SDI 2.5 -> SR_g included", u_bias_ept(0.03, 0.01, 2.5, rule = "sdi")$value, sqrt(0.03^2 + 0.01^2), 1e-15)
check("rule 'always'", u_bias_ept(0.03, 0.01, 1.5, rule = "always")$value, sqrt(0.03^2 + 0.01^2), 1e-15)
check("rule 'never'",  u_bias_ept(0.03, 0.01, 2.5, rule = "never")$value, 0.03, 1e-15)
check("Calibrator u_ref omitted with SR_g (SDI 1.5)", u_bias_ept(0.03, 0.01, 1.5, rule = "sdi", u_ref = 0.005)$value,
      0.03, 1e-15)
check("Calibrator u_ref + SR_g (SDI 2.5)", u_bias_ept(0.03, 0.01, 2.5, rule = "sdi", u_ref = 0.005)$value,
      sqrt(0.03^2 + 0.01^2 + 0.005^2), 1e-15)
check("Missing u_ref counts as 0", u_bias_ept(0.03, 0.01, 2.5, rule = "sdi", u_ref = NA)$value, sqrt(0.03^2 + 0.01^2), 1e-15)
# Roche C.f.a.s. traceability sheets: u_ref = u_ccal (or u_cal) / Calibrator Value, k = 1
check("Calibrator u_ref: GLUC3 u_cal 0.0443 / 10.7 mmol/L", calibrator_u_ref(10.7, 0.0443), 0.0443 / 10.7, 1e-15)
check("Calibrator u_ref: ALB2 u_ccal 3.14 / 34.5 g/L", calibrator_u_ref("34.5", "3.14"), 3.14 / 34.5, 1e-15)
check_true("Calibrator u_ref: blank -> NA", is.na(calibrator_u_ref(10.7, NA)) && is.na(calibrator_u_ref(NA, 0.04)))

# ------------------------------------------------------------------------------
# COMBINATION, CONVERSION, ROUNDING
# ------------------------------------------------------------------------------
check("IQMH RBC example: uc = 0.240169", combine_rss(0.24, 0.009), 0.240169, 1e-6)
check("IQMH RBC example: U = 0.47", round(expanded_uncertainty(combine_rss(0.24, 0.009), 1.96), 2), 0.47, 1e-12)
check("IQMH creatinine example: uc = 2.25",
      round(combine_rss(2.24, standard_uncertainty(0.5, "expanded", k = 2), 0.024, 0.030), 2), 2.25, 1e-12)
check("IQMH glucose example: uc = 0.2", round(combine_rss(0.14, 0.1, 0.1), 1), 0.2, 1e-12)
check("CLSI App A rectangular 26.0", round(standard_uncertainty(45, "range_rect"), 1), 26.0, 1e-12)
check("CLSI App A triangular 18.4",  round(standard_uncertainty(45, "range_tri"), 1), 18.4, 1e-12)
check("CLSI App A gaussian 15.0",    round(standard_uncertainty(45, "range_norm"), 1), 15.0, 1e-12)
check("CLSI 6.3: X \u00b1 1 % (k = 2) -> 0.5 %", standard_uncertainty(1, "expanded", k = 2), 0.5, 1e-12)
check("round_up_sig 8.064 -> 8.1",   round_up_sig(8.064),   8.1,  1e-12)
check("round_up_sig 0.11867 -> 0.12", round_up_sig(0.11867), 0.12, 1e-12)
check("round_up_sig 8.1 stays 8.1",  round_up_sig(8.1),     8.1,  1e-12)
check("round_up_sig 21.88 -> 22",    round_up_sig(21.88),   22,   1e-12)
check("round_up_sig 113.7 -> 120",   round_up_sig(113.7),   120,  1e-12)
check_true("format_x_pm_U 48.261 \u00b1 1.2 -> '48.3 \u00b1 1.2'", format_x_pm_U(48.261, 1.2) == "48.3 \u00b1 1.2")

# ------------------------------------------------------------------------------
# Acetaminophen workbook: bias and combination steps against its intermediate
# values, fed with the workbook's own pooled CV and QC mean
# ------------------------------------------------------------------------------
state <- state_from_json(paste(readLines("app/example_acetaminophen.json"), collapse = "\n"))
wb <- list(
  list(mean = 67.95, cv = 4.013305958820125, rms = 0.04533902112412293, sdi = 1.1036735112019138,
       uc = 0.06054989106847161, U = 0.11867778649420435, Uabs = 8.064155592281185),
  list(mean = 226.53333333333333, cv = 2.277859234030602, rms = 0.04370151700502935, sdi = 1.4848569087919272,
       uc = 0.04928170915813206, U = 0.09659214994993884, Uabs = 21.88134170199281),
  list(mean = 691.1166666666664, cv = 2.509085479117933, rms = 0.08012588593039746, sdi = 2.9347180635012693,
       uc = 0.08396254278118971, U = 0.16456658385113182, Uabs = 113.73470887591468))
for (i in 1:3) {
  e <- level_calc_inputs(state$levels[[i]])$bias$ept$data; w <- wb[[i]]; L <- paste0("Workbook L", i, ": ")
  t <- ept_challenge_table(e$Survey, e$Specimen, e$Your_Result, e$Mean, e[["S.D."]], e$No_of_Labs, w$cv)
  r <- rms_bias(t$rel_bias); uc <- combine_rss(w$cv / 100, r)
  check(paste0(L, "RMS bias"),    r,                         w$rms,  1e-12)
  check(paste0(L, "average SDI"), mean_sdi(t$sdi_iqmh),       w$sdi,  1e-9)
  check(paste0(L, "u_c"),         uc,                        w$uc,   1e-12)
  check(paste0(L, "U (k = 1.96)"), expanded_uncertainty(uc, 1.96), w$U, 1e-12)
  check(paste0(L, "U absolute"),  expanded_uncertainty(uc, 1.96) * w$mean, w$Uabs, 1e-9)
}
check("Workbook L1 row 1: Reference SD/SqrtN", ept_challenge_table("s", "x", 100, 94.52, 4.76, 307, 4.013305958820125)$sd_sqrtn,
      0.2716675492748264, 1e-12)
check("Workbook L1 row 1: ABS SDI", ept_challenge_table("s", "x", 100, 94.52, 4.76, 307, 4.013305958820125)$sdi_iqmh,
      1.444623168243435, 1e-9)

# ------------------------------------------------------------------------------
# End to end on the shipped example (app path: JSON -> compute_all)
# ------------------------------------------------------------------------------
res <- compute_all(state)
for (i in 1:3) {
  q  <- level_calc_inputs(state$levels[[i]])$qc$data
  n  <- as.numeric(q$Points); s <- as.numeric(q$SD); x <- as.numeric(q$Mean)
  xb <- sum(n * x) / sum(n)
  check(sprintf("Example L%d: u_prec = sqrt(sum((n-1)SD^2)/(N-m)) / weighted mean", i),
        res$levels[[i]]$u_prec, sqrt(sum((n - 1) * s^2) / (sum(n) - length(n))) / xb, 1e-12)
}
pl <- res$pooled
check("Pooled: all 3 levels included", length(pl$levels), 3, 0)
dfw <- sapply(res$levels, function(L) L$prec$anova$df_within)
cvl <- sapply(res$levels, function(L) L$prec$cv_pct)
check("Pooled: df weights = N - m", dfw, c(172, 165, 165), 0)
check("Pooled: CV = sqrt(sum(df*CV^2)/sum(df))", pl$cv_pct, sqrt(sum(dfw * cvl^2) / sum(dfw)), 1e-12)
all_rel <- unlist(lapply(res$levels, function(L) L$bias$table$rel_bias))
Jl <- sapply(res$levels, function(L) L$bias$n); ubl <- sapply(res$levels, function(L) L$u_bias)
check("Pooled: u_B = sqrt(sum(J*u_B^2)/sum(J))", pl$u_bias, sqrt(sum(Jl * ubl^2) / sum(Jl)), 1e-12)
check_true("Example L3 (SDI 2.93): SR_g from report included", isTRUE(res$levels[[3]]$bias$included) &&
           abs(res$levels[[3]]$u_bias - sqrt(res$levels[[3]]$bias$rms^2 + res$levels[[3]]$bias$group^2)) < 1e-12)
has_alert <- function(L) any(L$checks$severity == "alert")
check_true("Example L3 (SDI > 2, no u_ref): red alert", has_alert(res$levels[[3]]))
check_true("Example L1 (SDI <= 2, no u_ref): no alert", !has_alert(res$levels[[1]]))
stc <- state; stc$test$cal_value <- 10.7; stc$test$cal_u <- 0.0443
rc  <- compute_all(stc)$levels[[3]]
check_true("With calibrator entered: no alert", !has_alert(rc))
check("With calibrator: L3 u_B = sqrt(RMS^2 + SR_g^2 + u_ref^2)", rc$u_bias,
      sqrt(rc$bias$rms^2 + rc$bias$srg^2 + (0.0443 / 10.7)^2), 1e-12)
stn <- state; stn$settings$bias_rule <- "never"
check("Pooled, rule never: u_B = RMS over all 18 challenges (IQMH App B)", compute_all(stn)$pooled$u_bias, rms_bias(all_rel), 1e-12)
check("Default coverage factor k = 2", c(state$settings$k, pl$k), c(2, 2), 0)
check("Pooled: U = k*sqrt(u_prec^2 + u_bias^2)", pl$U, pl$k * sqrt((pl$cv_pct / 100)^2 + pl$u_bias^2), 1e-12)
check_true("Pooled: NULL with a single level", is.null(mu_pooled(res$levels[1])))

st2 <- state; st2$settings$imprecision <- "pooled_shifts"
r2 <- compute_all(st2)
check("Example L1 with month-to-month shifts", r2$levels[[1]]$u_prec, res$levels[[1]]$prec$anova$cv_pooled_shifts_pct / 100, 1e-12)

# JSON round trip and files saved by earlier versions
rt <- state_from_json(state_to_json(state))
check("JSON round trip reproduces U (L2)", compute_all(rt)$levels[[2]]$U_abs, res$levels[[2]]$U_abs, 1e-12)
old <- state_to_json(state)
old <- sub('"level": "1",', '"level": "1", "pooled": false,', old, fixed = TRUE)
old <- sub('"imprecision": "pooled"', '"imprecision": "total", "cv_source": "entered", "pool_weights": "n"', old, fixed = TRUE)
old <- sub('"SD": [', '"CV": ["4.1", "4.65", "2.6", "4.62", "3.99", "3.05"], "SD": [', old, fixed = TRUE)
os  <- state_from_json(old)
check_true("Old JSON: 'total' maps to month-to-month shifts", os$settings$imprecision == "pooled_shifts")
check_true("Old JSON: CV column ignored", identical(names(os$levels[[1]]$qc$data), QC_COLS))
check("Old JSON loads and computes", compute_all(os)$levels[[1]]$u_prec, r2$levels[[1]]$u_prec, 1e-12)
# v1.1/v1.2 files: per-level u_ref / SR_g fields and the reference-type label are ignored
ol <- state; e1 <- ol$levels[[1]]$bias$ept
e1$uref_source <- "manual"; e1$uref_value_pct <- 1; e1$srg_source <- "manual"; e1$ref_type <- "peer"
ol$levels[[1]]$bias$ept <- e1
check("Old per-level u_ref/SR_g fields ignored", compute_all(state_from_json(state_to_json(ol)))$levels[[1]]$u_bias,
      res$levels[[1]]$u_bias, 1e-15)

# Limits ("Apply to a result"): JSON round trip; files without limits get the defaults
stl <- state; stl$limits$desc[1:2] <- c("Upper reference limit", "Decision limit"); stl$limits$value[1:2] <- c(130, 990)
stl$limits$level[2] <- "1"
check_true("JSON round trip keeps limits", identical(state_from_json(state_to_json(stl))$limits, stl$limits))
nolim <- jsonlite::fromJSON(state_to_json(state), simplifyVector = FALSE); nolim$limits <- NULL
check_true("JSON without limits: defaults", identical(state_from_json(jsonlite::toJSON(nolim, auto_unbox = TRUE))$limits,
                                                     blank_state()$limits))

# Excel workbook save / load (the format offered to users)
if (requireNamespace("writexl", quietly = TRUE) && requireNamespace("readxl", quietly = TRUE)) {
  xf <- tempfile(fileext = ".xlsx")
  stx <- stl; stx$test$cal_value <- 10.7; stx$test$cal_u <- 0.0443; stx$settings$imprecision <- "pooled_shifts"
  stx$settings$target_U_pct <- 15; stx$n_levels <- 3L
  rx <- compute_all(stx)
  state_to_xlsx(stx, xf, results = rx$summary, version = "test")
  check_true("Excel: sheets written", all(c("About", "Test", "Levels", "IQC", "EQA_PT", "CRM", "Limits", "Results") %in%
                                         readxl::excel_sheets(xf)))
  bx <- state_from_xlsx(xf)
  check("Excel round trip reproduces U (all levels)", sapply(compute_all(bx)$levels, `[[`, "U"), sapply(rx$levels, `[[`, "U"), 1e-12)
  check("Excel round trip reproduces pooled U", compute_all(bx)$pooled$U, rx$pooled$U, 1e-12)
  check_true("Excel round trip: test, settings and limits", identical(bx$test, stx$test) && isTRUE(all.equal(bx$settings, stx$settings)) &&
             identical(bx$limits, stx$limits) && bx$n_levels == 3L)
  same_cells <- function(a, b) identical(unname(as.matrix(a)), unname(as.matrix(b)))
  check_true("Excel round trip: grids (values as entered)", same_cells(bx$levels[[2]]$bias$ept$data[1:10, ], stx$levels[[2]]$bias$ept$data[1:10, ]) &&
             same_cells(bx$levels[[1]]$qc$data[1:6, ], stx$levels[[1]]$qc$data[1:6, ]))
  # A workbook edited by hand: labels in other case, bias source "EPT", level as "Level 2"
  sh <- state_to_sheets(stx)
  sh$Test$Value[sh$Test$Field == "IQC pooling method"] <- "pooled monthly cv"
  sh$Test$Value[sh$Test$Field == "Coverage factor k"] <- "1.96"
  sh$Levels[["Bias source"]][1] <- "EPT"
  sh$Limits[["Use U of"]][1] <- "Level 2"
  writexl::write_xlsx(sh, xf)
  ex <- state_from_xlsx(xf)
  check_true("Edited workbook: labels read back", ex$settings$imprecision == "pooled" && ex$settings$k == 1.96 &&
             ex$levels[[1]]$bias$source == "ept" && ex$limits$level[1] == "2")
  writexl::write_xlsx(list(Sheet1 = data.frame(a = 1)), xf)
  check_true("Not a calculator workbook: clear error", inherits(try(state_from_xlsx(xf), silent = TRUE), "try-error"))
} else cat("SKIP Excel tests: readxl / writexl not installed\n")

cat(sprintf("\n%d passed, %d failed\n", n_pass, n_fail))
if (n_fail > 0) quit(status = 1)
