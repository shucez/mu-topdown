# Builds app/example_acetaminophen.json from the values in the source spreadsheet
# references/"Acetaminophen; Serum Uncertainty Calculator_Ver6.4.xls" (sheets "Determine
# Method SD" and "Estimate Bias Level 1-3"; the workbook is not in the repository, the
# values are copied below). Run from the project root under a UTF-8 locale:
#   LANG=en_US.UTF-8 Rscript tests/make_example_json.R
Sys.setlocale("LC_ALL", "en_US.UTF-8")
source("app/mu_topdown_core.R"); source("app/mu_io.R")
st <- blank_state()
st$test <- list(analyte = "Acetaminophen; Serum", method = "",
                instrument = "Roche cobas c502", units = "\u00b5mol/L")
st$n_levels <- 3L
dates <- c("2019-09", "2019-10", "2019-11", "2019-12", "2020-01", "2020-02")
qc <- list(
  list(mean = c(75.3, 72.4, 68.3, 64.0, 64.4, 63.3), sd = c(3.09, 3.36, 1.78, 2.96, 2.57, 1.93),
       n = c(34, 33, 24, 32, 34, 21)),
  list(mean = c(233.2, 228.4, 229.2, 224.8, 223.7, 219.9), sd = c(5.08, 5.84, 4.35, 6.15, 3.88, 5.14),
       n = c(33, 33, 24, 30, 31, 20)),
  list(mean = c(676.5, 677.1, 704.8, 700.7, 685.1, 702.5), sd = c(14.56, 19.18, 18.56, 15.0, 15.2, 22.41),
       n = c(33, 33, 24, 30, 31, 20)))
ept <- list(
  data.frame(Survey = c("CAP C-A 2020", "CAP C-B 2019", "CAP C-A 2019"),
             Specimen = c("CHM-01", "CHM-10", "CHM-02"),
             res = c(100, 100, 100), mean = c(94.52, 96.42, 96.36),
             sd = c(4.76, 5.14, 5.15), labs = c(307, 275, 258)),
  data.frame(Survey = c(rep("CAP C-A 2020", 3), rep("CAP C-C 2019", 4), "CAP C-B 2019", rep("CAP C-A 2019", 2)),
             Specimen = c("CHM-02", "CHM-04", "CHM-05", "CHM-11", "CHM-12", "CHM-13", "CHM-14", "CHM-08", "CHM-01", "CHM-05"),
             res = c(189, 142, 143, 264, 279, 189, 296, 173, 315, 303),
             mean = c(189.4, 141.35, 142.13, 252.54, 271.74, 177.9, 271.99, 164.34, 317.65, 316.15),
             sd = c(8.06, 6.7, 6.33, 10.4, 10.22, 7.53, 10.77, 7.45, 13.12, 11.91),
             labs = c(320, 320, 319, 300, 303, 303, 302, 283, 270, 269)),
  data.frame(Survey = c("CAP C-A 2020", rep("CAP C-B 2019", 3), "CAP C-A 2019"),
             Specimen = c("CHM-03", "CHM-06", "CHM-07", "CHM-09", "CHM-03"),
             res = c(761, 618, 487, 642, 833), mean = c(777.93, 579.12, 442.58, 577.19, 780.97),
             sd = c(41.55, 26.94, 19.1, 27.47, 39.42), labs = c(320, 285, 285, 286, 270)))
for (i in 1:3) {
  lv <- st$levels[[i]]
  lv$qc$level <- as.character(i); lv$qc$material <- paste("Immunoassay Plus", i)
  q <- qc[[i]]
  lv$qc$data <- grid_as_df(setNames(list(dates, q$mean, q$sd, q$n), QC_COLS), QC_COLS)
  e <- ept[[i]]
  lv$bias$source <- "ept"
  lv$bias$ept$provider <- "CAP Survey C"; lv$bias$ept$peer_group <- "Roche Cobas"
  lv$bias$ept$data <- grid_as_df(setNames(list(e$Survey, e$Specimen, e$res, e$mean, e$sd, e$labs, rep("", nrow(e))),
                                          EPT_COLS), EPT_COLS)
  st$levels[[i]] <- lv
}
writeLines(state_to_json(st), "app/example_acetaminophen.json", useBytes = TRUE)
r <- compute_all(state_from_json(paste(readLines("app/example_acetaminophen.json"), collapse = "\n")))
print(r$summary[, ], right = FALSE)
