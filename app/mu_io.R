################################################################################
#  mu_io.R
#  Input state for the app: blank templates; conversion of the on-screen
#  grids (headed exactly as on the raw reports) into the inputs expected by
#  mu_topdown_core.R; save / load as an Excel workbook (and JSON, used for the
#  bundled example). No calculations live here.
################################################################################

# Grid headers, worded as on the raw reports ----------------------------------
# Bio-Rad Unity monthly QC report, "Your Lab - Mon" block (+ report date).
# CV is not entered: it is computed from SD and Mean (mu_topdown_core.R).
QC_COLS  <- c("Date", "Mean", "SD", "# Points")
# CAP EQA/PT evaluation, "Evaluation and Comparative Method Statistics" (+ survey).
# Internally the EQA/PT bias source is coded "ept".
EPT_COLS <- c("Survey", "Specimen", "Your Result", "Mean", "S.D.", "No. of Labs", "S.D.I")
# Replicate results on a certified reference material
CRM_COLS <- c("Result")

MAX_LEVELS <- 5L
N_LIMITS   <- 3L   # rows in "Apply to a result"

blank_grid <- function(cols, n) {
  as.data.frame(setNames(rep(list(rep("", n)), length(cols)), cols),
                stringsAsFactors = FALSE, check.names = FALSE)
}
blank_qc_df  <- function(n = 8)  blank_grid(QC_COLS, n)
blank_ept_df <- function(n = 6)  blank_grid(EPT_COLS, n)
blank_crm_df <- function(n = 12) blank_grid(CRM_COLS, n)

blank_level <- function(i) {
  list(
    qc = list(level = as.character(i), material = "", lot = "", data = blank_qc_df()),
    bias = list(
      source = "ept",
      ept = list(provider = "", peer_group = "", data = blank_ept_df()),
      crm = list(material = "", certified = NA, U_cert = NA, k_cert = 2,
                 data = blank_crm_df())
    )
  )
}

blank_state <- function() {
  list(version = 1L,
       # cal_value, cal_u: Calibrator Value and u_ccal (or u_cal) of the test, from
       # the manufacturer's calibrator traceability sheet -> u_ref (calibrator)
       test = list(analyte = "", method = "", instrument = "", units = "",
                   cal_value = NA, cal_u = NA),
       settings = mu_default_settings(),
       n_levels = 2L,
       # "Apply to a result": reference interval or decision limits; level is
       # "pooled" or a level number ("1" ... "5")
       limits = list(desc = rep("", N_LIMITS), value = rep(NA_real_, N_LIMITS), level = rep("pooled", N_LIMITS)),
       levels = lapply(seq_len(MAX_LEVELS), blank_level))
}

# Grid <-> calculation inputs ---------------------------------------------------
grid_as_df <- function(x, cols) {
  if (is.null(x)) return(blank_grid(cols, 0))
  df <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  for (cl in cols) if (!cl %in% names(df)) df[[cl]] <- rep("", nrow(df))
  df <- df[, cols, drop = FALSE]
  df[] <- lapply(df, function(v) { v <- as.character(v); v[is.na(v)] <- ""; v })
  df
}

#' Convert one level of app state into the lists taken by mu_level()
level_calc_inputs <- function(lv) {
  q <- grid_as_df(lv$qc$data, QC_COLS)
  e <- grid_as_df(lv$bias$ept$data, EPT_COLS)
  r <- grid_as_df(lv$bias$crm$data, CRM_COLS)
  list(
    qc = list(level = lv$qc$level, material = lv$qc$material, lot = lv$qc$lot,
              data = data.frame(Date = q$Date, Mean = q$Mean, SD = q$SD,
                                Points = q[["# Points"]], stringsAsFactors = FALSE)),
    bias = list(
      source = lv$bias$source,
      ept = c(lv$bias$ept[setdiff(names(lv$bias$ept), "data")],
              list(data = data.frame(Survey = e$Survey, Specimen = e$Specimen,
                                     Your_Result = e[["Your Result"]], Mean = e$Mean,
                                     S.D. = e[["S.D."]], No_of_Labs = e[["No. of Labs"]],
                                     S.D.I = e[["S.D.I"]],
                                     stringsAsFactors = FALSE, check.names = FALSE))),
      crm = list(material = lv$bias$crm$material, certified = lv$bias$crm$certified,
                 U_cert = lv$bias$crm$U_cert, k_cert = lv$bias$crm$k_cert,
                 replicates = r$Result)
    )
  )
}

#' Run every level and the pooled estimate for a whole app state
compute_all <- function(state) {
  n  <- as.integer(state$n_levels)
  st <- utils::modifyList(mu_default_settings(), state$settings)
  lv <- lapply(seq_len(n), function(i) {
    inp <- level_calc_inputs(state$levels[[i]])
    mu_level(inp$qc, inp$bias, st,
             calibrator = list(value = state$test$cal_value, u = state$test$cal_u))
  })
  pooled <- mu_pooled(lv, k = st$k)
  list(levels = lv, pooled = pooled,
       summary = mu_summary(lv, pooled, units = state$test$units %||% ""))
}

# JSON save / load ------------------------------------------------------------
state_to_json <- function(state) {
  jsonlite::toJSON(state, auto_unbox = TRUE, dataframe = "columns", na = "null",
                   null = "null", pretty = TRUE, digits = NA)
}

#' Parse a saved state, filling anything missing from the blank template so
#' that files from older versions (or hand-written files) still load.
state_from_json <- function(txt) {
  raw  <- jsonlite::fromJSON(txt, simplifyVector = TRUE, simplifyDataFrame = FALSE)
  base <- blank_state()
  fill <- function(def, got) {
    if (is.null(got)) return(def)
    if (is.list(def) && !is.data.frame(def) && is.list(got) && !is.data.frame(got)) {
      for (nm in names(def)) def[[nm]] <- fill(def[[nm]], got[[nm]])
      return(def)
    }
    if (length(got) == 1 && is.na(got) && !is.data.frame(def)) return(if (is.logical(def)) def else got)
    got
  }
  out <- base
  out$version  <- base$version
  out$test     <- fill(base$test, raw$test)
  out$settings <- fill(base$settings, raw$settings)
  # Older files: "total" estimator -> month-to-month shifts; anything unknown -> pooled.
  imp <- as.character(out$settings$imprecision)
  if (identical(imp, "total")) imp <- "pooled_shifts"
  if (!imp %in% names(IMPRECISION_LABELS)) imp <- "pooled"
  out$settings$imprecision <- imp
  out$n_levels <- min(MAX_LEVELS, max(1L, as.integer(raw$n_levels %||% base$n_levels)))
  lim <- fill(base$limits, raw$limits)
  pad <- function(x, def) { x <- as.vector(x)[seq_len(N_LIMITS)]; x[is.na(x)] <- def; x }
  out$limits <- list(desc = pad(as.character(lim$desc), ""), value = as.numeric(as.vector(lim$value)[seq_len(N_LIMITS)]),
                     level = pad(as.character(lim$level), "pooled"))
  rl <- raw$levels %||% list()
  for (i in seq_len(MAX_LEVELS)) {
    if (i > length(rl)) next
    got <- rl[[i]]; def <- base$levels[[i]]
    lv <- fill(def, got)
    lv$qc$data        <- grid_as_df(got$qc$data %||% def$qc$data, QC_COLS)
    lv$bias$ept$data  <- grid_as_df(got$bias$ept$data %||% def$bias$ept$data, EPT_COLS)
    lv$bias$crm$data  <- grid_as_df(got$bias$crm$data %||% def$bias$crm$data, CRM_COLS)
    out$levels[[i]]   <- lv
  }
  out
}

# Excel workbook: human-readable save / load of the inputs ---------------------
# One sheet per kind of input. IQC, EQA_PT and CRM are long tables with a
# Level column (1-5). Settings are written with their on-screen labels and
# read back from either the label or the internal code.
SOURCE_LABELS <- c(ept = "EQA/PT", crm = "CRM", none = "None")
RULE_LABELS   <- c(sdi = "when mean SDI > SDI limit", always = "always", never = "never")

label_to_code <- function(x, labels, default) {
  x <- trimws(as.character(x))
  if (length(x) != 1 || is.na(x) || !nzchar(x)) return(default)
  if (x %in% names(labels)) return(x)
  hit <- names(labels)[tolower(labels) == tolower(x)]
  if (length(hit)) hit[1] else default
}

#' Build the workbook as a named list of data frames (one per sheet)
#' @param results optional mu_summary() table, written to sheet Results
state_to_sheets <- function(state, results = NULL, version = "") {
  t <- state$test; s <- state$settings; lv <- state$levels
  num1 <- function(x) { v <- as_num(x); if (length(v) != 1) NA_real_ else v }
  chr  <- function(x) { x <- if (is.null(x)) "" else as.character(x); x[is.na(x)] <- ""; x }
  about <- data.frame(`Top-down measurement uncertainty calculator` = c(
    "This workbook holds the inputs of the calculator. Edit the sheets Test, Levels, IQC, EQA_PT, CRM and Limits,",
    "then load the file back with 'Load inputs (Excel)'.",
    "In the sheets IQC, EQA_PT and CRM, the column Level (1 to 5) says which level each row belongs to.",
    "IQC: one row per monthly QC report (Mean, SD, # Points); CV is calculated by the app from SD and Mean.",
    "EQA_PT: one row per challenge, columns as on the CAP evaluation; S.D.I is optional (transcription check only).",
    "The sheet Results is written for reference only and is ignored when the file is loaded.",
    sprintf("Saved %s%s.", format(Sys.time(), "%Y-%m-%d %H:%M"), if (nzchar(version)) paste0(" with app version ", version) else "")),
    check.names = FALSE)
  test <- data.frame(
    Field = c("Analyte", "Method", "Instrument", "Units", "Calibrator value", "u_ccal, or u_cal",
              "Number of levels", "IQC pooling method", "Include SR_g and u_ref in u_B", "SDI limit",
              "Coverage factor k", "Target maximum U (%)"),
    Value = c(chr(t$analyte), chr(t$method), chr(t$instrument), chr(t$units),
              chr(num1(t$cal_value)), chr(num1(t$cal_u)), as.character(state$n_levels),
              IMPRECISION_LABELS[[s$imprecision]], RULE_LABELS[[s$bias_rule]], chr(s$sdi_limit),
              chr(s$k), chr(num1(s$target_U_pct))),
    Notes = c("", "", "", "", "Calibrator Value on the manufacturer's traceability sheet",
              "Combined calibrator uncertainty u_ccal if given, otherwise u_cal (k = 1), same units as the calibrator value",
              "1 to 5", paste(IMPRECISION_LABELS, collapse = " | "), paste(RULE_LABELS, collapse = " | "),
              "used with 'when mean SDI > SDI limit'", "1.96 or 2", "optional"),
    stringsAsFactors = FALSE)
  levels <- data.frame(
    Level = seq_along(lv),
    `Level (as on report)` = vapply(lv, function(l) chr(l$qc$level), ""),
    `Control material` = vapply(lv, function(l) chr(l$qc$material), ""),
    Lot = vapply(lv, function(l) chr(l$qc$lot), ""),
    `Bias source` = unname(SOURCE_LABELS[vapply(lv, function(l) chr(l$bias$source), "")]),
    `Provider / survey` = vapply(lv, function(l) chr(l$bias$ept$provider), ""),
    `Peer Group` = vapply(lv, function(l) chr(l$bias$ept$peer_group), ""),
    `CRM (name, level)` = vapply(lv, function(l) chr(l$bias$crm$material), ""),
    `CRM certified value` = vapply(lv, function(l) num1(l$bias$crm$certified), 0),
    `CRM expanded U` = vapply(lv, function(l) num1(l$bias$crm$U_cert), 0),
    `CRM k` = vapply(lv, function(l) num1(l$bias$crm$k_cert), 0),
    check.names = FALSE, stringsAsFactors = FALSE)
  # long tables: non-empty grid rows, numbers written as numbers
  long <- function(get, cols, numeric_cols) {
    parts <- lapply(seq_along(lv), function(i) {
      g <- grid_as_df(get(lv[[i]]), cols)
      keep <- apply(g, 1, function(r) any(nzchar(trimws(r))))
      g <- g[keep, , drop = FALSE]
      if (!nrow(g)) return(NULL)
      for (cl in numeric_cols) g[[cl]] <- as_num(g[[cl]])
      cbind(data.frame(Level = rep(i, nrow(g))), g)
    })
    out <- do.call(rbind, parts)
    if (is.null(out)) {
      out <- cbind(data.frame(Level = integer(0)), grid_as_df(NULL, cols))
      for (cl in numeric_cols) out[[cl]] <- numeric(0)
    }
    rownames(out) <- NULL
    out
  }
  iqc <- long(function(l) l$qc$data, QC_COLS, c("Mean", "SD", "# Points"))
  ept <- long(function(l) l$bias$ept$data, EPT_COLS, c("Your Result", "Mean", "S.D.", "No. of Labs", "S.D.I"))
  crm <- long(function(l) l$bias$crm$data, CRM_COLS, "Result")
  lim <- state$limits
  limits <- data.frame(
    Description = chr(lim$desc),
    Value = as_num(lim$value),
    `Use U of` = ifelse(chr(lim$level) == "pooled", "Pooled", paste("Level", chr(lim$level))),
    check.names = FALSE, stringsAsFactors = FALSE)
  out <- list(About = about, Test = test, Levels = levels, IQC = iqc, EQA_PT = ept, CRM = crm, Limits = limits)
  if (!is.null(results)) out$Results <- results
  out
}

state_to_xlsx <- function(state, path, results = NULL, version = "") {
  writexl::write_xlsx(state_to_sheets(state, results, version), path, format_headers = TRUE)
  invisible(path)
}

#' Read a workbook written by state_to_xlsx() (and possibly edited in Excel)
state_from_xlsx <- function(path) {
  sheets <- readxl::excel_sheets(path)
  rd <- function(name) {
    if (!name %in% sheets) return(NULL)
    df <- as.data.frame(readxl::read_excel(path, sheet = name, col_types = "text"),
                        check.names = FALSE, stringsAsFactors = FALSE)
    # Numeric cells come back as text at full binary precision (72.4 ->
    # "72.40000000000001"); round them to 15 significant digits as R prints.
    df[] <- lapply(df, function(v) {
      v[is.na(v)] <- ""; v <- trimws(v)
      num <- grepl("^[-+]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][-+]?[0-9]+)?$", v)
      v[num] <- as.character(signif(as.numeric(v[num]), 15))
      v
    })
    df
  }
  test <- rd("Test")
  if (is.null(test) || !all(c("Field", "Value") %in% names(test)))
    stop("this is not a workbook saved by the MU calculator (sheet 'Test' with columns Field and Value not found).")
  val <- function(field) { v <- test$Value[match(field, test$Field)]; if (!length(v) || is.na(v)) "" else v }
  numv <- function(field, default = NA_real_) { v <- as_num(val(field)); if (is.na(v)) default else v }
  st <- blank_state()
  st$test[c("analyte", "method", "instrument", "units")] <-
    list(val("Analyte"), val("Method"), val("Instrument"), val("Units"))
  st$test$cal_value <- numv("Calibrator value")
  st$test$cal_u     <- numv("u_ccal, or u_cal")
  st$n_levels <- as.integer(min(MAX_LEVELS, max(1, round(numv("Number of levels", 2)))))
  st$settings$imprecision  <- label_to_code(val("IQC pooling method"), IMPRECISION_LABELS, "pooled")
  st$settings$bias_rule    <- label_to_code(val("Include SR_g and u_ref in u_B"), RULE_LABELS, "sdi")
  st$settings$sdi_limit    <- numv("SDI limit", 2)
  st$settings$k            <- numv("Coverage factor k", 2)
  st$settings$target_U_pct <- numv("Target maximum U (%)")

  lev_of <- function(df) { if (is.null(df) || !"Level" %in% names(df)) integer(0) else as.integer(round(as_num(df$Level))) }
  lvl <- rd("Levels")
  if (!is.null(lvl) && nrow(lvl)) {
    li <- lev_of(lvl)
    get <- function(r, col) if (col %in% names(lvl)) lvl[[col]][r] else ""
    for (r in seq_len(nrow(lvl))) {
      i <- li[r]; if (is.na(i) || i < 1 || i > MAX_LEVELS) next
      L <- st$levels[[i]]
      if (nzchar(get(r, "Level (as on report)"))) L$qc$level <- get(r, "Level (as on report)")
      L$qc$material <- get(r, "Control material"); L$qc$lot <- get(r, "Lot")
      L$bias$source <- label_to_code(sub("^EPT$", "EQA/PT", get(r, "Bias source")), SOURCE_LABELS, "ept")
      L$bias$ept$provider <- get(r, "Provider / survey"); L$bias$ept$peer_group <- get(r, "Peer Group")
      L$bias$crm$material  <- get(r, "CRM (name, level)")
      L$bias$crm$certified <- as_num(get(r, "CRM certified value"))
      L$bias$crm$U_cert    <- as_num(get(r, "CRM expanded U"))
      k <- as_num(get(r, "CRM k")); L$bias$crm$k_cert <- if (is.na(k)) 2 else k
      st$levels[[i]] <- L
    }
  }
  fill_grid <- function(df, cols, set) {
    if (is.null(df)) return()
    li <- lev_of(df)
    for (i in seq_len(MAX_LEVELS)) {
      rows <- df[!is.na(li) & li == i, , drop = FALSE]
      if (nrow(rows)) set(i, grid_as_df(rows, cols))
    }
  }
  fill_grid(rd("IQC"), QC_COLS, function(i, g) st$levels[[i]]$qc$data <<- g)
  fill_grid(rd("EQA_PT"), EPT_COLS, function(i, g) st$levels[[i]]$bias$ept$data <<- g)
  fill_grid(rd("CRM"), CRM_COLS, function(i, g) st$levels[[i]]$bias$crm$data <<- g)

  lim <- rd("Limits")
  if (!is.null(lim) && nrow(lim)) {
    for (r in seq_len(min(N_LIMITS, nrow(lim)))) {
      st$limits$desc[r]  <- if ("Description" %in% names(lim)) lim$Description[r] else ""
      st$limits$value[r] <- if ("Value" %in% names(lim)) as_num(lim$Value[r]) else NA_real_
      u <- if ("Use U of" %in% names(lim)) lim[["Use U of"]][r] else ""
      lv_n <- suppressWarnings(as.integer(sub("^Level\\s*", "", u, ignore.case = TRUE)))
      st$limits$level[r] <- if (!is.na(lv_n) && lv_n >= 1 && lv_n <= MAX_LEVELS) as.character(lv_n) else "pooled"
    }
  }
  st
}
