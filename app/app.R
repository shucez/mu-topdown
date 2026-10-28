################################################################################
#  Top-down measurement uncertainty calculator (Shiny / Shinylive)
#
#  This file is the user interface and server only. Every number shown is
#  produced by mu_topdown_core.R (calculations), fed by mu_io.R (input state,
#  Excel save/load) and formatted by mu_report.R (tables, printable report).
#  Runs locally with shiny::runApp("app") or, exported with shinylive,
#  entirely in the browser.
################################################################################

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(rhandsontable)
})
# writexl / readxl (Excel save/load) and jsonlite (bundled example, .json files
# from earlier versions) are used via pkg:: in mu_io.R; jsonlite is not
# attached so it does not mask shiny::validate().

APP_VERSION <- "1.3.0"

source("mu_topdown_core.R", local = TRUE)
source("mu_io.R", local = TRUE)
source("mu_report.R", local = TRUE)

IMPRECISION_LABELS_CHOICES <- setNames(names(IMPRECISION_LABELS), IMPRECISION_LABELS)

# Warnings and notes from mu_checks(), coloured by severity
checks_ui <- function(ch) {
  if (is.null(ch) || !nrow(ch)) return(div(class = "text-success small", "\u2714 No warnings."))
  tags$ul(class = "mu-checks", lapply(seq_len(nrow(ch)), function(j) {
    cls <- switch(ch$severity[j], error = "text-danger", alert = "text-danger fw-semibold", warning = "text-warning-emphasis", "text-info-emphasis")
    icon <- switch(ch$severity[j], error = "\u2716", alert = "\u26a0", warning = "\u26a0", "\u2139")
    tags$li(class = cls, paste(icon, ch$message[j]))
  }))
}

# ---- per-level cards ---------------------------------------------------------
lvl_show <- function(i) sprintf("Number(input.n_levels) >= %d", i)

qc_card <- function(i) {
  conditionalPanel(
    lvl_show(i), class = "mu-col",
    card(
      card_header(class = "d-flex justify-content-between align-items-center",
                  strong(sprintf("Level %d \u2014 IQC", i)), textOutput(paste0("qc_head_", i), inline = TRUE)),
      layout_columns(col_widths = c(3, 5, 4), gap = "0.5rem",
        textInput(paste0("qc_level_", i), "Level (as on report)", value = i),
        textInput(paste0("qc_material_", i), "Control material", ""),
        textInput(paste0("qc_lot_", i), "Lot", "")),
      div(class = "small text-muted mb-1",
          "One row per monthly QC report (Unity \u2018Your Lab \u2013 Mon\u2019). ",
          "Date is information only. CV is not entered: it is calculated from SD and Mean. ",
          "Right-click to add/remove rows."),
      rHandsontableOutput(paste0("qc_grid_", i)),
      h6(class = "mt-3", "Intermediate results"),
      div(class = "mu-scroll mu-qc", tableOutput(paste0("qc_calc_", i))),
      div(class = "mu-scroll mu-anova", tableOutput(paste0("qc_anova_", i))),
      uiOutput(paste0("qc_sum_", i))
    )
  )
}

bias_card <- function(i) {
  src <- paste0("bias_source_", i)
  conditionalPanel(
    lvl_show(i), class = "mu-col mu-col-wide",
    card(
      card_header(class = "d-flex justify-content-between align-items-center",
                  strong(sprintf("Level %d \u2014 bias", i)), textOutput(paste0("bias_head_", i), inline = TRUE)),
      radioButtons(src, "Bias source", inline = TRUE,
                   c("EQA/PT" = "ept", "CRM replicates" = "crm", "None" = "none"), selected = "ept"),
      conditionalPanel(
        sprintf("input.%s == 'ept'", src),
        layout_columns(col_widths = c(4, 8), gap = "0.5rem",
          textInput(paste0("ept_provider_", i), "Provider / survey", ""),
          textInput(paste0("ept_peer_", i), "Peer Group (as on report)", "")),
        div(class = "small text-muted mb-1",
            "SR_g (uncertainty of the EQA/PT assigned value) is calculated from the S.D. and No. of Labs columns ",
            "(IQMH App C). The calibrator u_ref is set once per test in the sidebar."),
        div(class = "small text-muted mb-1",
            "One row per challenge, headed as on the CAP evaluation. Paste Specimen\u2013S.D.I as a block. ",
            "S.D.I is optional and only checks transcription: the report\u2019s S.D.I is (Your Result \u2212 Mean)/peer S.D., ",
            "whereas the rule for SR_g and u_ref uses the IQMH SDI = |relative bias| / your IQC CV."),
        rHandsontableOutput(paste0("ept_grid_", i)),
        h6(class = "mt-3", "Intermediate results"),
        div(class = "mu-scroll", tableOutput(paste0("ept_calc_", i))),
        uiOutput(paste0("ept_sum_", i))
      ),
      conditionalPanel(
        sprintf("input.%s == 'crm'", src),
        layout_columns(col_widths = c(6, 6), gap = "0.5rem",
          textInput(paste0("crm_material_", i), "CRM (name, level)", ""),
          numericInput(paste0("crm_cert_", i), "Certified value", value = NA)),
        layout_columns(col_widths = c(6, 6), gap = "0.5rem",
          numericInput(paste0("crm_U_", i), "Certificate expanded U (units) \u2192 u_ref", value = NA, min = 0),
          numericInput(paste0("crm_k_", i), "Certificate k", value = 2, min = 1, step = 0.01)),
        div(class = "small text-muted mb-1", "Replicate results on the CRM (\u2265 10 recommended, IQMH App A). Paste a column."),
        layout_columns(col_widths = c(4, 8), gap = "0.75rem",
          rHandsontableOutput(paste0("crm_grid_", i)),
          div(h6("Intermediate results"), tableOutput(paste0("crm_calc_", i))))
      ),
      conditionalPanel(
        sprintf("input.%s == 'none'", src),
        div(class = "alert alert-warning small",
            "No bias component: u_B = 0. IQMH requirement VI.8 expects bias to be considered when sufficient data are available."))
    )
  )
}

# ---- "Apply to a result": reference interval / decision limits --------------------
limit_rows <- function() {
  cw <- c(5, 3, 4)
  examples <- c("e.g. Upper reference limit", "e.g. Lower reference limit", "e.g. Clinical decision limit")
  div(class = "mu-limits",
    layout_columns(col_widths = cw, gap = "0.5rem", class = "mu-limits-head mb-1",
                   "Reference interval or decision limit", "Value", "Use U of"),
    lapply(seq_len(N_LIMITS), function(j) layout_columns(col_widths = cw, gap = "0.5rem",
      textInput(paste0("lim_desc_", j), NULL, "", placeholder = examples[(j - 1) %% length(examples) + 1]),
      numericInput(paste0("lim_value_", j), NULL, value = NA),
      selectInput(paste0("lim_level_", j), NULL, choices = c(Pooled = "pooled"), selected = "pooled"))))
}

# ---- getting started page --------------------------------------------------------
# "Gather your data" checklist, then how the calculator works and its sources.
# The ticks are a checklist for the user only; they are not part of the state.
gather_item <- function(id, title, ...) {
  div(class = "form-check mu-gather-item",
    tags$input(class = "form-check-input", type = "checkbox", id = id),
    tags$label(class = "form-check-label fw-semibold", `for` = id, title),
    tags$ul(class = "small mb-0 mt-1", ...))
}

gather_checklist <- function() {
  tagList(
    h4("Gather your data"),
    p("Before you start, collect the following for the test (analyte, method and instrument). Tick each item when you ",
      "have it; the ticks are for your own use and are not saved."),
    gather_item("gather_cal", "Traceability and Uncertainty sheet for the manufacturer\u2019s calibrator",
      tags$li("The manufacturer\u2019s statement of the calibrator\u2019s assigned value and its uncertainty, for the calibrator ",
              "lot in use (e.g. Roche C.f.a.s. \u2018Traceability and Uncertainty\u2019). This is the calibrator traceable to a ",
              "reference method whose uncertainty IQMH calls u_ref (notes to Eq 2)."),
      tags$li("Note the Calibrator Value and the combined calibrator uncertainty u_ccal, or the standard calibrator uncertainty ",
              "u_cal if no u_ccal is given. Both are standard uncertainties (k = 1). If a sheet gives only an expanded ",
              "uncertainty, divide it by its coverage factor k (CLSI C51-A \u00a76.3)."),
      tags$li(em("Enter in: sidebar, Uncertainty of calibrator (u_ref)."))),
    gather_item("gather_qc", "Monthly QC reports from at least 6 months",
      tags$li("Internal quality control (QC) results of at least 2 levels of control material analysed over a minimum of six ",
              "months (IQMH \u00a74). For a new method without six months of data, at least 30 results per level give an ",
              "interim SD (IQMH \u00a74)."),
      tags$li("The period should include routine changes of conditions: recalibrations, new reagent and calibrator lots, ",
              "instrument maintenance and different operators. Do not trim results or remove outliers without a documented ",
              "reason (CLSI C51-A \u00a77.2)."),
      tags$li("From each monthly report (e.g. Unity \u2018Your Lab \u2013 Mon\u2019), for each level: Mean, SD and # Points."),
      tags$li(em("Enter in: 1 \u00b7 IQC imprecision, one row per month."))),
    gather_item("gather_bias", "EQA/PT surveys, or CRM measurements",
      tags$li(strong("EQA/PT: "), "results for a minimum of 3 challenges with a minimum of 2 different levels per test, or a ",
              "minimum of 6 survey rounds with a single value per round (IQMH: at least three surveys, each with multiple ",
              "levels)."),
      tags$li("From each survey evaluation (e.g. CAP): Survey, Specimen, Your Result, Mean, S.D. and No. of Labs (S.D.I ",
              "optional). For each QC level, choose the challenges closest to its concentration."),
      tags$li(strong("Or CRM: "), "a certified reference material, preferably with a natural (patient-like) matrix ",
              "(CLSI C51-A \u00a78.3.2), measured at least 10 times (IQMH App A), and its certificate: certified value, ",
              "expanded uncertainty and k."),
      tags$li(em("Enter in: 2 \u00b7 Bias (EQA/PT or CRM), one card per level."))))
}

# Notation follows IQMH (u_s, u_B, u_c, U, k, RMS_bias, SR_g, u_ref, SDI, SEM)
# and, for the ANOVA, CLSI C51-A 7.3 (SS, MS, s with subscripts wth / btw, n_0).
getting_started_page <- function() {
  sym_row <- function(sym, meaning, where) tags$tr(tags$td(HTML(sym)), tags$td(meaning), tags$td(where))
  withMathJax(div(class = "mu-methods",
    gather_checklist(),
    h4(class = "mt-4", "How the calculator works"),
    p("A result x is reported with its expanded uncertainty U as x \u00b1 U: the interval expected to contain the true value ",
      "with about 95 % probability. For each QC level, the calculator estimates U from two sources of uncertainty:"),
    tags$ul(
      tags$li(strong("Imprecision"), " \u2014 how much results scatter when the same material is measured repeatedly, ",
              "estimated from internal quality control (IQC) over at least six months."),
      tags$li(strong("Bias"), " \u2014 how far results sit from the assigned or true value, estimated from external quality ",
              "assessment / proficiency testing (EQA/PT) or from replicate measurements of a certified reference material (CRM). The bias is assumed to be ",
              "negligible or corrected; what enters U is the uncertainty associated with it.")),
    p("Both are expressed as relative standard uncertainties (a standard deviation divided by the measured value, shown as %). ",
      "They are combined and multiplied by a coverage factor (IQMH Eq 3 and Eq 5; CLSI C51-A Eq 10 and Eq 12):"),
    p("$$u_c=\\sqrt{u_s^2+u_B^2}\\qquad U=k\\,u_c\\qquad U_{abs}=U\\times\\bar{x}$$"),
    p("where \\(u_s\\) is the standard uncertainty from imprecision (section 1), \\(u_B\\) the standard uncertainty associated ",
      "with bias (section 2), \\(u_c\\) the combined standard uncertainty, \\(k\\) the coverage factor (1.96 or 2, for about 95 %), ",
      "and \\(U_{abs}\\) the expanded uncertainty in reporting units at the IQC mean \\(\\bar{x}\\)."),

    h5("Notation"),
    tags$table(class = "table table-sm small mu-notation",
      tags$thead(tags$tr(tags$th("Symbol"), tags$th("Meaning"), tags$th("Section"))),
      tags$tbody(
        sym_row("\\(u_s\\)", "Standard uncertainty from imprecision: the IQC CV in use / 100", "1"),
        sym_row("\\(u_B\\)", "Standard uncertainty associated with bias", "2"),
        sym_row("\\(u_c,\\ U,\\ k\\)", "Combined standard uncertainty, expanded uncertainty, coverage factor", "above"),
        sym_row("\\(\\bar{x}\\)", "Mean of all IQC results of a level", "1"),
        sym_row("\\(s_{wth},\\ s_{btw}\\)", "Within-month and between-month standard deviations of the IQC results", "1"),
        sym_row("\\(RMS_{bias}\\)", "Root mean square of the relative biases found in EQA/PT: the typical size of your bias", "2a"),
        sym_row("\\(SR_g\\)", "Standard uncertainty of the EQA/PT assigned value (the Mean on the report)", "2a"),
        sym_row("\\(u_{ref}\\)", "Standard uncertainty of the calibrator\u2019s assigned value, per test (EQA/PT); of the CRM\u2019s certified value (CRM)", "2a, 2b"),
        sym_row("\\(SDI\\)", "Standard deviation index: a bias expressed as a multiple of your method\u2019s imprecision", "2a"),
        sym_row("\\(SEM\\)", "Standard error of the mean of replicate CRM results", "2b"))),
    p(class = "small text-muted", "Symbols follow the IQMH guidance; the analysis-of-variance symbols follow CLSI C51-A \u00a77.3."),

    h5("1. Imprecision (\\(u_s\\)) from monthly IQC"),
    p("Enter one row per monthly QC report. For month \\(i\\) (\\(i = 1, \\dots, m\\), where \\(m\\) is the number of months), the report gives:"),
    tags$ul(
      tags$li("\\(n_i\\) \u2014 the number of QC results (# Points);"),
      tags$li("\\(\\bar{x}_i\\) \u2014 their mean (Mean);"),
      tags$li("\\(s_i\\) \u2014 their standard deviation (SD), calculated with divisor \\(n_i-1\\).")),
    p("The CV is not entered; it is calculated from SD and Mean so that no precision is lost to the one-decimal CV printed on reports."),
    p("The months are treated as the groups of a one-way analysis of variance (ANOVA; CLSI C51-A \u00a77.3). This separates the ",
      "scatter of results within each month from the movement of the monthly means, and it is computed exactly from the monthly ",
      "summaries. First the totals:"),
    p("$$N=\\sum_{i=1}^{m} n_i\\qquad \\bar{x}=\\frac{1}{N}\\sum_{i=1}^{m} n_i\\,\\bar{x}_i$$"),
    p("where \\(N\\) is the total number of IQC results and \\(\\bar{x}\\) the mean of all of them."),
    p(strong("Within-month variation"), " \u2014 how far results lie from their own month\u2019s mean:"),
    p("$$SS_{wth}=\\sum_{i=1}^{m}(n_i-1)\\,s_i^2\\qquad MS_{wth}=\\frac{SS_{wth}}{N-m}$$"),
    p("where \\(SS_{wth}\\) is the within-month sum of squares (recovered exactly from each month\u2019s SD) and \\(MS_{wth}\\) the ",
      "within-month mean square: \\(SS_{wth}\\) divided by its \\(N-m\\) degrees of freedom (each month\u2019s mean uses up one)."),
    p(strong("Between-month variation"), " \u2014 how far the monthly means lie from \\(\\bar{x}\\):"),
    p("$$SS_{btw}=\\sum_{i=1}^{m} n_i\\,(\\bar{x}_i-\\bar{x})^2\\qquad MS_{btw}=\\frac{SS_{btw}}{m-1}\\qquad n_0=\\frac{N-\\sum_i n_i^2/N}{m-1}$$"),
    p("where \\(SS_{btw}\\) is the between-month sum of squares, \\(MS_{btw}\\) the between-month mean square, and \\(n_0\\) the ",
      "effective number of results per month (\\(n_0=n\\) when every month has \\(n\\) results; CLSI C51-A \u00a77.3, note b)."),
    p("The two IQC pooling methods (sidebar) then give:"),
    p(strong("Pooled monthly CV"), " \u2014 within-month imprecision only (CLSI C51-A Eq 6 and Eq 27; IQMH Eq 1):"),
    p("$$s_{wth}=\\sqrt{MS_{wth}}\\qquad CV=100\\,\\frac{s_{wth}}{\\bar{x}}$$"),
    p(strong("Pooled monthly CV with month-to-month shifts"), " \u2014 adds \\(s_{btw}\\), the spread of the monthly means beyond what ",
      "the within-month scatter explains, such as shifts at reagent or calibrator lot changes and recalibrations, which CLSI C51-A ",
      "\u00a77.2 treats as long-term random variation (CLSI C51-A Eq 28 and Eq 29):"),
    p("$$s_{btw}=\\sqrt{\\max(0,\\ (MS_{btw}-MS_{wth})/n_0)}\\qquad CV=100\\,\\frac{\\sqrt{s_{wth}^2+s_{btw}^2}}{\\bar{x}}$$"),
    p("In both cases \\(u_s = CV/100\\). Both CVs are always shown for comparison. The second is never smaller than the first and ",
      "needs at least two months. The SD of all results pooled into one group is not used, because it understates the ",
      "between-month component (CLSI C51-A \u00a77.3, Example 4)."),

    h5("2a. Uncertainty associated with bias (\\(u_B\\)) from EQA/PT"),
    p("Enter the EQA/PT challenges for each level, choosing those whose concentration is closest to that level\u2019s QC. ",
      "For challenge \\(j\\) (\\(j = 1, \\dots, J\\), where \\(J\\) is the number of challenges entered for the level), the CAP evaluation ",
      "gives Your Result\\(_j\\); the assigned value Mean\\(_j\\), a consensus (peer-group or all-method) mean; the group\u2019s ",
      "standard deviation S.D.\\(_j\\); and its No. of Labs\\(_j\\)."),
    p(strong("Bias. "), "The bias and the relative bias (in %) of challenge \\(j\\) are (IQMH App B):"),
    p("$$B_j=\\text{Your Result}_j-\\text{Mean}_j\\qquad \\%B_j=100\\,\\frac{B_j}{\\text{Mean}_j}$$"),
    p("Their root mean square summarises the typical size of your bias across the \\(J\\) challenges, regardless of sign (IQMH App B):"),
    p("$$RMS_{bias}=\\sqrt{\\tfrac{1}{J}\\textstyle\\sum_{j=1}^{J}(\\%B_j)^2}$$"),
    p(strong("Uncertainty of EQA/PT assigned value, \\(SR_g\\). "), "The assigned value is itself uncertain. For a consensus mean, ",
      "its standard error is S.D.\\(_j/\\sqrt{\\text{No. of Labs}_j}\\). Expressed relative to the Mean and combined over the ",
      "challenges in the same root-mean-square way, this gives \\(SR_g\\) (IQMH App C, \u2018(SD/\u221aN)group\u2019):"),
    p("$$\\%SE_j=100\\,\\frac{\\text{S.D.}_j/\\sqrt{\\text{No. of Labs}_j}}{\\text{Mean}_j}\\qquad SR_g=\\sqrt{\\tfrac{1}{J}\\textstyle\\sum_{j=1}^{J}\\%SE_j^{\\,2}}$$"),
    p("where \\(\\%SE_j\\) is the relative standard error of challenge \\(j\\)\u2019s assigned value. IQMH defines \\(SR_g\\) from the ",
      "all-method group; the CAP evaluation report gives the peer group\u2019s S.D. and number of labs, which yields the standard error ",
      "of the peer mean."),
    p(strong("Uncertainty of calibrator, \\(u_{ref}\\). "), "The uncertainty of the calibrator\u2019s assigned value, which links ",
      "your results to the reference system (IQMH, notes to Eq 4). It is entered once per test in the sidebar, from the ",
      "manufacturer\u2019s calibrator traceability sheet (e.g. Roche C.f.a.s. \u2018Traceability and Uncertainty\u2019):"),
    p("$$u_{ref}=\\frac{u_{ccal}\\ \\text{(or } u_{cal}\\text{)}}{\\text{Calibrator Value}}$$"),
    p("where \\(u_{cal}\\) is the manufacturer\u2019s standard calibrator uncertainty and \\(u_{ccal}\\) the combined calibrator ",
      "uncertainty, which adds the contribution of a higher-order reference material or method and is given for some tests only. ",
      "Use \\(u_{ccal}\\) when given, otherwise \\(u_{cal}\\). Both are standard uncertainties (k = 1) in the same units as the ",
      "Calibrator Value, so the ratio is a relative uncertainty and no coverage factor is applied."),
    p(strong("Standard deviation index, \\(SDI\\). "), "Each bias is expressed as a multiple of your method\u2019s own imprecision ",
      "(IQMH top-down alternative, step 3c):"),
    p("$$SDI_j=\\frac{|\\%B_j|}{CV}\\qquad \\overline{SDI}=\\frac{1}{J}\\sum_{j=1}^{J}SDI_j$$"),
    p("where \\(CV\\) is the IQC CV in use for this level (section 1) and \\(\\overline{SDI}\\) the mean SDI. This is not the S.D.I ",
      "printed on the CAP report, which is (Your Result \u2212 Mean)/S.D. and measures bias against the peer group\u2019s spread; that ",
      "value is recomputed only to check that the row was copied correctly."),
    p(strong("Combining. "), "The three terms are combined as in IQMH Eq 2 (all in %):"),
    p("$$u_B=\\sqrt{RMS_{bias}^2+SR_g^2+u_{ref}^2}$$"),
    p("The reference terms \\(SR_g\\) and \\(u_{ref}\\) are included only when the bias is large compared with the method\u2019s ",
      "imprecision, i.e. when \\(\\overline{SDI}\\) exceeds the SDI limit (default 2; IQMH top-down alternative, step 3d, and the IQMH ",
      "note after the ACR example); otherwise \\(u_B = RMS_{bias}\\). The sidebar can instead include them always (IQMH Eq 2 as ",
      "written) or never. If they are required but \\(u_{ref}\\) has not been entered, the calculation continues with ",
      "\\(u_{ref} = 0\\) and a red warning on the Bias and Results pages states that \\(u_B\\) and U are underestimated."),

    h5("2b. Uncertainty associated with bias (\\(u_B\\)) from a CRM"),
    p("Alternatively, measure a certified reference material \\(n\\) times (\\(n \\ge 10\\); IQMH App A). With \\(x_{CRM}\\) its ",
      "certified value, and \\(U_{cert}\\) and \\(k_{cert}\\) the expanded uncertainty and coverage factor on its certificate ",
      "(IQMH Eq 2, left-hand branch; CLSI C51-A \u00a78.3.2):"),
    p("$$SEM=\\frac{s}{\\sqrt{n}}\\qquad u_{ref}=\\frac{U_{cert}}{k_{cert}}\\qquad u_B=\\sqrt{(SEM/x_{CRM})^2+(u_{ref}/x_{CRM})^2}$$"),
    p("where \\(s\\) is the SD of the \\(n\\) replicate results and \\(SEM\\) the standard error of their mean; here \\(u_{ref}\\) ",
      "is the uncertainty of the CRM\u2019s certified value (IQMH App A: \\(u_{Cref}\\)). The bias \\(B = \\text{mean} - x_{CRM}\\) is ",
      "reported and flagged when \\(|B| > 2\\sqrt{SEM^2+u_{ref}^2}\\) (CLSI C51-A \u00a78.3.2). The model assumes a significant bias is ",
      "corrected (CLSI C51-A \u00a78.2); the bias itself is not added to U."),

    h5("3. Pooled estimate across levels"),
    p("When two or more levels are entered, a pooled estimate is shown next to the per-level results. Imprecision is pooled on ",
      "the CV scale, each level weighted by its within-month degrees of freedom (CLSI C51-A Eq 6; IQMH Eq 1 note):"),
    p("$$CV_{pooled}=\\sqrt{\\textstyle\\sum_\\ell (N_\\ell-m_\\ell)\\,CV_\\ell^2\\,/\\,\\sum_\\ell (N_\\ell-m_\\ell)}$$"),
    p("where \\(\\ell\\) numbers the levels, \\(CV_\\ell\\) is the CV in use for level \\(\\ell\\), and \\(N_\\ell\\) and \\(m_\\ell\\) ",
      "are its numbers of results and months. \\(u_B^2\\) is averaged across levels weighted by the number of EQA/PT challenges ",
      "\\(J_\\ell\\) (when neither \\(SR_g\\) nor \\(u_{ref}\\) is included, this equals the RMS over all challenges together, as in ",
      "IQMH App B), or with equal weights for CRM levels. Pooled values suit methods whose levels perform comparably across the ",
      "measuring interval; otherwise use each level\u2019s own result. To leave a level out, do not enter it."),

    h5("4. Reporting"),
    p("U is rounded up to two significant digits and the result is stated to the same decimal place (CLSI C51-A \u00a79.2). ",
      "For example, \\(\\bar{x}\\) = 68.29 \u00b5mol/L with U = 8.134 \u00b5mol/L is reported as 68.3 \u00b1 8.2 \u00b5mol/L: ",
      "U is rounded up (8.134 \u2192 8.2), not to the nearest value (8.1), so that it is never understated."),

    h5("Minimum data checked by the app"),
    tags$ul(
      tags$li("IQC: \u2265 6 months (IQMH \u00a74); \u2265 30 results for an interim SD (IQMH \u00a74); \u2265 2 results per month."),
      tags$li("EQA/PT: \u2265 3 surveys, each with multiple levels (IQMH \u00a74)."),
      tags$li("CRM: \u2265 10 replicates (IQMH \u00a74, App A).")),

    h5("Report field \u2192 app field"),
    layout_columns(col_widths = c(6, 6),
      tags$table(class = "table table-sm small",
        tags$thead(tags$tr(tags$th("Unity monthly QC report"), tags$th("App (IQC grid)"))),
        tags$tbody(
          tags$tr(tags$td("Report period"), tags$td("Date (information only)")),
          tags$tr(tags$td("Your Lab \u2013 Mon: Mean"), tags$td("Mean")),
          tags$tr(tags$td("Your Lab \u2013 Mon: SD"), tags$td("SD")),
          tags$tr(tags$td("Your Lab \u2013 Mon: CV"), tags$td("not entered (calculated from SD and Mean)")),
          tags$tr(tags$td("Your Lab \u2013 Mon: # Points"), tags$td("# Points")),
          tags$tr(tags$td("Level"), tags$td("Level (as on report)")),
          tags$tr(tags$td("Analyte / method / instrument / units"), tags$td("Sidebar")))),
      tags$table(class = "table table-sm small",
        tags$thead(tags$tr(tags$th("CAP evaluation report"), tags$th("App (EQA/PT grid)"))),
        tags$tbody(
          tags$tr(tags$td("Survey (page header, e.g. C-B 2026)"), tags$td("Survey")),
          tags$tr(tags$td("Specimen"), tags$td("Specimen")),
          tags$tr(tags$td("Your Result"), tags$td("Your Result")),
          tags$tr(tags$td("Mean"), tags$td("Mean (assigned value)")),
          tags$tr(tags$td("S.D."), tags$td("S.D.")),
          tags$tr(tags$td("No. of Labs"), tags$td("No. of Labs")),
          tags$tr(tags$td("S.D.I"), tags$td("S.D.I (optional, check only)")),
          tags$tr(tags$td("Test / Unit of Measure / Peer Group"), tags$td("Sidebar / Peer Group"))))),
    tags$table(class = "table table-sm small", style = "max-width: 640px",
      tags$thead(tags$tr(tags$th("Calibrator traceability sheet (e.g. Roche C.f.a.s.)"), tags$th("App (sidebar)"))),
      tags$tbody(
        tags$tr(tags$td("Calibrator Value"), tags$td("Calibrator value")),
        tags$tr(tags$td("Combined Calibrator Uncertainty u_ccal (if given), otherwise Standard Roche Calibrator Uncertainty u_cal"),
                tags$td("u_ccal, or u_cal")),
        tags$tr(tags$td("Expanded Roche Calibrator Uncertainty U_cal (k = 2)"), tags$td("not used (u_cal is the same at k = 1)")))),

    h5("Scope and limitations"),
    tags$ul(class = "small",
      tags$li("Top-down MU covers the analytical phase only; pre-analytical sources (patient preparation, sampling) are not included (IQMH \u00a76)."),
      tags$li("IQC material may not behave like patient samples (CLSI C51-A \u00a74.2, \u00a77.2)."),
      tags$li("Consensus (non-accuracy-based) EQA/PT shows bias against the peer or all-method group, not against a true value; ",
              "\\(SR_g\\) covers only the uncertainty of the consensus mean (CLSI C51-A \u00a77.5)."),
      tags$li("The EQA/PT pathway assumes consensus surveys. For an accuracy-based survey (commutable material with a reference-method ",
              "target), the observed bias is already measured against the reference system, so the calibrator \\(u_{ref}\\) should ",
              "not be added; the uncertainty of the target value itself is needed instead (CLSI C51-A \u00a78.3.2). This case is not ",
              "handled by the EQA/PT pathway."),
      tags$li("Results above the measuring interval that are diluted need the dilution uncertainty added (IQMH \u00a74 step 1).")),

    h5("Sources"),
    tags$ol(class = "small",
      tags$li("Institute for Quality Management in Healthcare. Guidance on Measurement Uncertainty for Medical Laboratories, v1.0 (2014), document v1.1 (2018)."),
      tags$li("CLSI. Expression of Measurement Uncertainty in Laboratory Medicine; Approved Guideline. CLSI document C51-A. 2012.")),
    p(downloadButton("dl_core", "Download the calculation code (mu_topdown_core.R)", class = "btn-sm btn-outline-secondary")),
    p(class = "small text-muted", paste("App version", APP_VERSION))
  ))
}

# ---- UI ----------------------------------------------------------------------
ui <- page_navbar(
  title = "Top-down MU calculator",
  window_title = "Top-down MU calculator",
  id = "nav",
  fillable = FALSE,
  header = tags$head(tags$style(HTML("
    .mu-flex { display:flex; flex-wrap:wrap; gap:1rem; align-items:flex-start; }
    /* shown conditionalPanels use display:contents, so size the card itself */
    .mu-col > .card { flex: 1 1 380px; min-width: 340px; max-width: 100%; margin-bottom: 0; }
    .mu-col-wide > .card { flex: 1 1 620px; }
    .mu-scroll { overflow-x:auto; }
    .mu-scroll table { font-size: 0.78rem; white-space: nowrap; margin-bottom: 0.25rem; }
    .mu-qc table tr:last-child td { font-weight: 600; }
    .mu-anova table tr:last-child td, .mu-anova table tr:nth-last-child(2) td { font-weight: 600; }
    .mu-anova table tr td { font-weight: 400; }
    .mu-summary table { font-size: 0.85rem; }
    .mu-summary table td:first-child { font-weight: 600; white-space: nowrap; }
    .mu-key table { font-size: 0.95rem; }
    .mu-key table td:nth-child(7), .mu-key table td:nth-child(9) { font-weight: 700; }
    .mu-checks { padding-left: 1.1rem; font-size: 0.85rem; margin-bottom: 0.25rem; }
    .mu-testline { font-size: 0.95rem; color: var(--bs-secondary-color); }
    .mu-sum { font-size: 0.85rem; background: var(--bs-tertiary-bg); border-radius: .4rem; padding: .5rem .75rem; }
    .handsontable { font-size: 0.82rem; }
    .mu-methods h5 { margin-top: 1.4rem; }
    .mu-methods { max-width: 980px; }
    .mu-notation td:first-child { white-space: nowrap; }
    .mu-gather-item { border: 1px solid var(--bs-border-color); border-radius: .4rem; padding: .6rem .9rem .6rem 2.4rem;
                      margin-bottom: .6rem; background: var(--bs-body-bg); }
    .mu-gather-item .form-check-input { margin-left: -1.6rem; transform: scale(1.15); }
    .mu-gather-item .form-check-input:checked ~ .form-check-label { text-decoration: line-through; color: var(--bs-secondary-color); }
    .mu-limits .shiny-input-container { margin-bottom: .35rem; width: 100%; }
    .mu-limits .mu-limits-head { font-size: .8rem; font-weight: 600; color: var(--bs-secondary-color); }
    .mu-limit-tbl table { font-size: 0.85rem; }
    @media print { .bslib-sidebar-layout > .sidebar, .navbar, .btn { display:none !important; } }
  ")),
  # Printing: the server builds the report (build_report) and sends it here; it
  # is printed from a hidden frame, so the printout is the formatted report and
  # not the app screen. Ctrl/Cmd+P inside the app is routed to the same path.
  tags$script(HTML("
    $(function() {
      Shiny.addCustomMessageHandler('mu_print', function(m) {
        var old = document.getElementById('mu-print-frame');
        if (old) old.parentNode.removeChild(old);
        var f = document.createElement('iframe');
        f.id = 'mu-print-frame';
        f.setAttribute('aria-hidden', 'true');
        f.style.cssText = 'position:fixed; right:0; bottom:0; width:0; height:0; border:0;';
        f.srcdoc = m.html;
        f.onload = function() {
          setTimeout(function() { f.contentWindow.focus(); f.contentWindow.print(); }, 250);
        };
        document.body.appendChild(f);
      });
      document.addEventListener('keydown', function(e) {
        if ((e.ctrlKey || e.metaKey) && !e.altKey && !e.shiftKey && (e.key === 'p' || e.key === 'P')) {
          e.preventDefault();
          Shiny.setInputValue('print_req', Date.now(), {priority: 'event'});
        }
      });
    });
  "))),
  sidebar = sidebar(
    width = 320,
    h6("Test"),
    textInput("analyte", "Analyte", "", placeholder = "e.g. Bilirubin, Total/TBIL"),
    textInput("method", "Method", "", placeholder = "e.g. Diphylline, Diazonium Salt"),
    textInput("instrument", "Instrument", "", placeholder = "e.g. VITROS 350"),
    textInput("units", "Units", "", placeholder = "e.g. \u00b5mol/L"),
    tags$label(class = "control-label", "Uncertainty of calibrator (u_ref)"),
    layout_columns(col_widths = c(6, 6), gap = "0.5rem",
      numericInput("cal_value", "Calibrator value", value = NA, min = 0),
      numericInput("cal_u", "u_ccal, or u_cal", value = NA, min = 0)),
    div(class = "small text-muted mb-2",
        "From the manufacturer\u2019s calibrator traceability sheet: the Calibrator Value, and the combined calibrator ",
        "uncertainty u_ccal if given, otherwise the standard calibrator uncertainty u_cal (k = 1, same units). ",
        "Both are needed, because u_ref is relative: u_ref = u \u00f7 calibrator value. ",
        textOutput("cal_uref", inline = TRUE)),
    radioButtons("n_levels", "Number of levels", choices = 1:5, selected = 2, inline = TRUE),
    hr(),
    h6("Calculation"),
    # In the order the calculation uses them: imprecision -> bias -> expansion -> goal
    selectInput("imprecision", "IQC pooling method", IMPRECISION_LABELS_CHOICES),
    selectInput("bias_rule", "Include SR_g and u_ref in u_B",
                c("when mean SDI > SDI limit (IQMH)" = "sdi",
                  "always (IQMH Eq 2)" = "always", "never" = "never")),
    conditionalPanel("input.bias_rule == 'sdi'",
      numericInput("sdi_limit", "SDI limit", value = 2, min = 0, step = 0.1)),
    radioButtons("k", "Coverage factor k (\u2248 95 %)", c("1.96" = "1.96", "2" = "2"), selected = "2", inline = TRUE),
    numericInput("target_U_pct", "Target maximum U (%), optional", value = NA, min = 0, step = 0.5),
    hr(),
    h6("Report and inputs"),
    actionButton("print_report", "Print report / save as PDF", icon = icon("print"),
                 class = "btn-sm btn-primary w-100 mb-2"),
    downloadButton("save_xlsx", "Save inputs (Excel)", class = "btn-sm btn-outline-secondary w-100 mb-2"),
    fileInput("load_inputs", NULL, accept = c(".xlsx", ".json"), buttonLabel = "Load inputs\u2026",
              placeholder = "saved .xlsx"),
    actionButton("load_example", "Load example (acetaminophen, 3 levels)", class = "btn-sm btn-outline-primary w-100 mb-2"),
    actionButton("reset", "Clear all", class = "btn-sm btn-outline-danger w-100")
  ),
  nav_panel("Getting started", getting_started_page()),
  nav_panel("1 \u00b7 IQC imprecision",
            div(class = "mu-testline mb-2", textOutput("testline_1")),
            div(class = "mu-flex", lapply(seq_len(MAX_LEVELS), qc_card))),
  nav_panel("2 \u00b7 Bias (EQA/PT or CRM)",
            div(class = "mu-testline mb-2", textOutput("testline_2")),
            div(class = "small text-muted mb-2",
                "Match each bias level to the IQC level with the closest concentration yourself; EQA/PT and IQC concentrations rarely coincide."),
            div(class = "mu-flex", lapply(seq_len(MAX_LEVELS), bias_card))),
  nav_panel("3 \u00b7 Results",
            div(class = "mu-testline mb-2", textOutput("testline_3")),
            card(card_header("Key results"),
                 div(class = "mu-summary mu-key mu-scroll", tableOutput("key_tbl")), uiOutput("key_alerts")),
            card(card_header("Measurement uncertainty by level \u2014 all intermediate results"),
                 div(class = "mu-summary mu-scroll", tableOutput("summary_tbl")),
                 uiOutput("pooled_note")),
            # side by side on wide screens, stacked on narrow ones
            layout_columns(col_widths = breakpoints(sm = c(12, 12), xl = c(5, 7)),
              card(card_header("Share of u_c\u00b2 by component"), plotOutput("contrib_plot", height = "260px")),
              card(card_header("Apply to a result"),
                   div(class = "small text-muted mb-2",
                       "Enter your reference interval limits or clinical decision limits to see the uncertainty at each. ",
                       "They are saved with the inputs and printed in the report."),
                   limit_rows(),
                   div(class = "mu-limit-tbl mu-scroll", tableOutput("lim_tbl")))),
            card(card_header("Checks and warnings"), uiOutput("checks_all"))),
  nav_spacer(),
  nav_item(tags$span(class = "small text-muted", "IQMH \u00b7 CLSI C51-A"))
)

# ---- server ------------------------------------------------------------------
server <- function(input, output, session) {
  rv <- reactiveValues(state = blank_state(), load_version = 0)

  # Modify the state with a function, then store it back.
  edit_state <- function(f) { s <- isolate(rv$state); rv$state <- f(s) }

  # TRUE only for a user edit (cell change, row added/removed) coming from a
  # grid rendered at the current load_version; see make_grid().
  grid_edit_ok <- function(v) {
    if (is.null(v)) return(FALSE)
    ver <- suppressWarnings(as.numeric(unlist(v$params$mu_version)))
    if (length(ver) != 1 || is.na(ver) || ver != isolate(rv$load_version)) return(FALSE)
    ev <- v$changes$event
    isTRUE(ev %in% c("afterCreateRow", "afterRemoveRow")) ||
      (identical(ev, "afterChange") && !is.null(v$changes$changes))
  }

  # Push a whole state into every input and grid.
  apply_state <- function(st) {
    rv$state <- st
    updateTextInput(session, "analyte", value = st$test$analyte)
    updateTextInput(session, "method", value = st$test$method)
    updateTextInput(session, "instrument", value = st$test$instrument)
    updateTextInput(session, "units", value = st$test$units)
    updateNumericInput(session, "cal_value", value = st$test$cal_value)
    updateNumericInput(session, "cal_u", value = st$test$cal_u)
    updateRadioButtons(session, "n_levels", selected = st$n_levels)
    s <- st$settings
    updateRadioButtons(session, "k", selected = as.character(s$k))
    updateSelectInput(session, "bias_rule", selected = s$bias_rule)
    updateNumericInput(session, "target_U_pct", value = s$target_U_pct)
    updateSelectInput(session, "imprecision", selected = s$imprecision)
    updateNumericInput(session, "sdi_limit", value = s$sdi_limit)
    for (i in seq_len(MAX_LEVELS)) {
      lv <- st$levels[[i]]
      updateTextInput(session, paste0("qc_level_", i), value = lv$qc$level)
      updateTextInput(session, paste0("qc_material_", i), value = lv$qc$material)
      updateTextInput(session, paste0("qc_lot_", i), value = lv$qc$lot)
      updateRadioButtons(session, paste0("bias_source_", i), selected = lv$bias$source)
      e <- lv$bias$ept
      updateTextInput(session, paste0("ept_provider_", i), value = e$provider)
      updateTextInput(session, paste0("ept_peer_", i), value = e$peer_group)
      cm <- lv$bias$crm
      updateTextInput(session, paste0("crm_material_", i), value = cm$material)
      updateNumericInput(session, paste0("crm_cert_", i), value = cm$certified)
      updateNumericInput(session, paste0("crm_U_", i), value = cm$U_cert)
      updateNumericInput(session, paste0("crm_k_", i), value = cm$k_cert)
    }
    for (j in seq_len(N_LIMITS)) {
      updateTextInput(session, paste0("lim_desc_", j), value = st$limits$desc[j])
      updateNumericInput(session, paste0("lim_value_", j), value = st$limits$value[j])
      updateSelectInput(session, paste0("lim_level_", j), selected = st$limits$level[j])
    }
    rv$load_version <- isolate(rv$load_version) + 1
  }

  # ---- sidebar inputs -> state ----
  for (nm in c("analyte", "method", "instrument", "units")) local({
    nm <- nm
    observeEvent(input[[nm]], edit_state(function(s) { s$test[[nm]] <- input[[nm]]; s }), ignoreInit = TRUE)
  })
  for (nm in c("cal_value", "cal_u")) local({
    nm <- nm
    observeEvent(input[[nm]], edit_state(function(s) { s$test[[nm]] <- as.numeric(input[[nm]]); s }),
                 ignoreInit = TRUE, ignoreNULL = FALSE)
  })
  output$cal_uref <- renderText({
    u <- calibrator_u_ref(rv$state$test$cal_value, rv$state$test$cal_u)
    if (is.na(u)) "u_ref: not entered." else sprintf("u_ref = u / calibrator value = %s.", fmt_pct(u))
  })
  observeEvent(input$n_levels, edit_state(function(s) { s$n_levels <- as.integer(input$n_levels); s }), ignoreInit = TRUE)
  observeEvent(input$k, edit_state(function(s) { s$settings$k <- as.numeric(input$k); s }), ignoreInit = TRUE)
  for (nm in c("bias_rule", "imprecision")) local({
    nm <- nm
    observeEvent(input[[nm]], edit_state(function(s) { s$settings[[nm]] <- input[[nm]]; s }), ignoreInit = TRUE)
  })
  observeEvent(input$target_U_pct, edit_state(function(s) { s$settings$target_U_pct <- as.numeric(input$target_U_pct); s }),
               ignoreInit = TRUE, ignoreNULL = FALSE)
  observeEvent(input$sdi_limit, {
    v <- as.numeric(input$sdi_limit)
    if (!is.na(v)) edit_state(function(s) { s$settings$sdi_limit <- v; s })
  }, ignoreInit = TRUE)

  # ---- per-level inputs -> state ----
  lapply(seq_len(MAX_LEVELS), function(i) {
    bind <- function(id, setter, ignoreNULL = TRUE) {
      observeEvent(input[[id]], edit_state(function(s) setter(s, input[[id]])),
                   ignoreInit = TRUE, ignoreNULL = ignoreNULL)
    }
    bind(paste0("qc_level_", i),    function(s, v) { s$levels[[i]]$qc$level <- v; s })
    bind(paste0("qc_material_", i), function(s, v) { s$levels[[i]]$qc$material <- v; s })
    bind(paste0("qc_lot_", i),      function(s, v) { s$levels[[i]]$qc$lot <- v; s })
    bind(paste0("bias_source_", i), function(s, v) { s$levels[[i]]$bias$source <- v; s })
    bind(paste0("ept_provider_", i), function(s, v) { s$levels[[i]]$bias$ept$provider <- v; s })
    bind(paste0("ept_peer_", i),     function(s, v) { s$levels[[i]]$bias$ept$peer_group <- v; s })
    bind(paste0("crm_material_", i), function(s, v) { s$levels[[i]]$bias$crm$material <- v; s })
    bind(paste0("crm_cert_", i),     function(s, v) { s$levels[[i]]$bias$crm$certified <- v; s }, FALSE)
    bind(paste0("crm_U_", i),        function(s, v) { s$levels[[i]]$bias$crm$U_cert <- v; s }, FALSE)
    bind(paste0("crm_k_", i),        function(s, v) { s$levels[[i]]$bias$crm$k_cert <- v; s }, FALSE)

    # Grids: rendered from the stored state only when a state is loaded, so
    # typing never triggers a re-render; edits flow back into the state.
    # Each render is tagged with the current load_version (echoed back by
    # rhandsontable in `params`), and only genuine edits are accepted: the
    # echo a grid sends right after rendering, or anything from a grid
    # rendered before the last load, must not overwrite the loaded state.
    make_grid <- function(df, height = NULL) {
      rhandsontable(df, rowHeaders = TRUE, stretchH = "all", minSpareRows = 1, height = height,
                    mu_version = isolate(rv$load_version)) |>
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    }
    output[[paste0("qc_grid_", i)]] <- renderRHandsontable({
      rv$load_version
      make_grid(isolate(rv$state$levels[[i]]$qc$data))
    })
    output[[paste0("ept_grid_", i)]] <- renderRHandsontable({
      rv$load_version
      make_grid(isolate(rv$state$levels[[i]]$bias$ept$data))
    })
    output[[paste0("crm_grid_", i)]] <- renderRHandsontable({
      rv$load_version
      make_grid(isolate(rv$state$levels[[i]]$bias$crm$data), height = 320)
    })
    observeEvent(input[[paste0("qc_grid_", i)]], {
      v <- input[[paste0("qc_grid_", i)]]; req(grid_edit_ok(v))
      df <- grid_as_df(hot_to_r(v), QC_COLS)
      edit_state(function(s) { s$levels[[i]]$qc$data <- df; s })
    })
    observeEvent(input[[paste0("ept_grid_", i)]], {
      v <- input[[paste0("ept_grid_", i)]]; req(grid_edit_ok(v))
      df <- grid_as_df(hot_to_r(v), EPT_COLS)
      edit_state(function(s) { s$levels[[i]]$bias$ept$data <- df; s })
    })
    observeEvent(input[[paste0("crm_grid_", i)]], {
      v <- input[[paste0("crm_grid_", i)]]; req(grid_edit_ok(v))
      df <- grid_as_df(hot_to_r(v), CRM_COLS)
      edit_state(function(s) { s$levels[[i]]$bias$crm$data <- df; s })
    })
  })

  # ---- calculation ----
  res <- reactive({
    tryCatch(compute_all(rv$state), error = function(e) list(error = conditionMessage(e)))
  })
  level_res <- function(i) {
    r <- res()
    if (!is.null(r$error) || i > length(r$levels)) return(NULL)
    r$levels[[i]]
  }

  testline <- reactive({
    t <- rv$state$test
    parts <- c(t$analyte, t$method, t$instrument, if (nzchar(t$units)) paste0("[", t$units, "]"))
    parts <- parts[nzchar(parts)]
    if (length(parts)) paste(parts, collapse = " \u00b7 ") else "Enter the analyte, method, instrument and units in the sidebar."
  })
  output$testline_1 <- renderText(testline())
  output$testline_2 <- renderText(testline())
  output$testline_3 <- renderText(testline())

  # ---- per-level outputs ----
  lapply(seq_len(MAX_LEVELS), function(i) {
    output[[paste0("qc_head_", i)]] <- renderText({
      r <- level_res(i); if (is.null(r) || is.na(r$u_prec)) return("")
      sprintf("u_s = %s", fmt_pct(r$u_prec))
    })
    output[[paste0("bias_head_", i)]] <- renderText({
      r <- level_res(i); if (is.null(r)) return("")
      sprintf("u_B = %s", fmt_pct(r$u_bias))
    })
    output[[paste0("qc_calc_", i)]] <- renderTable({
      r <- level_res(i); req(r, nrow(r$prec$table) > 0); qc_display(r)
    }, striped = TRUE, spacing = "xs", align = "l", na = "")
    output[[paste0("qc_anova_", i)]] <- renderTable({
      r <- level_res(i); req(r, r$prec$anova$n_months > 0); anova_display(r$prec$anova)
    }, striped = TRUE, spacing = "xs", align = "l", na = "")
    output[[paste0("qc_sum_", i)]] <- renderUI({
      r <- level_res(i); req(r); p <- r$prec; a <- p$anova
      if (a$n_months == 0) return(div(class = "small text-muted", "Enter at least one month with Mean, SD and # Points (\u2265 2)."))
      div(class = "mu-sum",
        div(sprintf("N = %d results over m = %d month(s); IQC mean (all results) = %s", as.integer(a$n_points),
                    as.integer(a$n_months), fmt_num(p$qc_mean))),
        div(sprintf("Pooled monthly CV = %s; with month-to-month shifts = %s",
                    fmt_pct(a$cv_pooled_pct / 100), fmt_pct(a$cv_pooled_shifts_pct / 100))),
        div(strong(sprintf("u_s in use = %s [%s]", fmt_pct(r$u_prec), p$label))),
        checks_ui(r$checks[grepl("IQC|CV|month", r$checks$message), , drop = FALSE]))
    })
    output[[paste0("ept_calc_", i)]] <- renderTable({
      r <- level_res(i); req(r, r$bias$source == "ept", nrow(r$bias$table) > 0); ept_display(r$bias)
    }, striped = TRUE, spacing = "xs", align = "l", na = "")
    output[[paste0("ept_sum_", i)]] <- renderUI({
      r <- level_res(i); req(r, r$bias$source == "ept"); b <- r$bias
      if (b$n == 0) return(div(class = "small text-muted", "Enter at least one challenge with Your Result and Mean."))
      div(class = "mu-sum",
        div(sprintf("J = %d challenge(s) from %d survey(s); mean assigned value (Mean) = %s (range %s\u2013%s); mean Your Result = %s",
                    b$n, b$n_surveys, fmt_num(b$mean_ref), fmt_num(b$range_ref[1]), fmt_num(b$range_ref[2]),
                    fmt_num(b$mean_result))),
        div(sprintf("RMS_bias = %s (IQMH App B); mean SDI = %s, using the IQC CV %s",
                    fmt_pct(b$rms), fmt_fix(b$mean_sdi, 3), fmt_pct(r$u_prec))),
        div(sprintf("SR_g = %s [from report: S.D./\u221aNo. of Labs]; u_ref (calibrator, sidebar) = %s", fmt_pct(b$srg),
                    if (is.na(b$u_ref)) "not entered" else fmt_pct(b$u_ref))),
        div(em(b$reason)),
        div(strong(sprintf("u_B = \u221a(%s) = %s",
                           paste(c("RMS_bias\u00b2", if (isTRUE(b$included)) "SR_g\u00b2", if (!is.na(b$u_ref)) "u_ref\u00b2"),
                                 collapse = " + "), fmt_pct(r$u_bias)))),
        checks_ui(r$checks[grepl("EQA/PT|S\\.D\\.I|SDI|survey|matching|u_ref", r$checks$message), , drop = FALSE]))
    })
    output[[paste0("crm_calc_", i)]] <- renderTable({
      r <- level_res(i); req(r, r$bias$source == "crm"); crm_display(r$bias$crm)
    }, striped = TRUE, spacing = "xs", align = "l")
  })

  # ---- results page ----
  output$summary_tbl <- renderTable({
    r <- res()
    validate(need(is.null(r$error), paste("Calculation error:", r$error)))
    r$summary
  }, striped = TRUE, hover = TRUE, spacing = "s", align = "l")
  output$key_tbl <- renderTable({
    r <- res(); req(is.null(r$error), length(r$levels) > 0)
    u <- rv$state$test$units
    rows <- lapply(seq_along(r$levels), function(i) {
      L <- r$levels[[i]]
      data.frame(
        Level = sprintf("%d (report: %s)", i, L$qc$level),
        "IQC mean" = fmt_num(L$qc_mean),
        "EQA/PT or CRM mean used" = if (L$bias$source == "none") "\u2014" else
          paste0(fmt_num(L$bias$mean_ref), " (n = ", L$bias$n, ", ", SOURCE_LABELS[[L$bias$source]], ")"),
        "u_s" = fmt_pct(L$u_prec, 2), "u_B" = fmt_pct(L$u_bias, 2), "u_c" = fmt_pct(L$uc, 2),
        "U (%)" = fmt_pct(L$U, 2), "U (abs.)" = paste(fmt_num(L$U_abs, 3), u),
        "Statement" = format_x_pm_U(L$qc_mean, L$U_abs, u),
        check.names = FALSE, stringsAsFactors = FALSE)
    })
    out <- do.call(rbind, rows)
    p <- r$pooled
    if (!is.null(p) && is.null(p$error))
      out <- rbind(out, data.frame(Level = paste("Pooled", paste(p$levels, collapse = "+")), "IQC mean" = "\u2014",
                                   "EQA/PT or CRM mean used" = "\u2014", "u_s" = fmt_pct(p$u_prec, 2),
                                   "u_B" = fmt_pct(p$u_bias, 2), "u_c" = fmt_pct(p$uc, 2), "U (%)" = fmt_pct(p$U, 2),
                                   "U (abs.)" = "\u2014", "Statement" = sprintf("\u00b1 %s %% of result", format(round_up_sig(p$U_pct, 2))),
                                   check.names = FALSE, stringsAsFactors = FALSE))
    out
  }, striped = TRUE, spacing = "s", align = "l")

  # Results that are computed but understated (e.g. u_ref missing when required), in red
  output$key_alerts <- renderUI({
    r <- res(); req(is.null(r$error))
    al <- do.call(rbind, lapply(seq_along(r$levels), function(i) {
      ch <- r$levels[[i]]$checks; ch <- ch[ch$severity == "alert", , drop = FALSE]
      if (nrow(ch)) ch$message <- paste0("Level ", i, ": ", ch$message)
      ch
    }))
    if (is.null(al) || !nrow(al)) return(NULL)
    checks_ui(al)
  })
  output$pooled_note <- renderUI({
    r <- res(); p <- r$pooled
    if (is.null(p)) return(div(class = "small text-muted", "Enter \u2265 2 levels to see the pooled estimate (IQMH Eq 1)."))
    if (!is.null(p$error)) return(div(class = "small text-warning-emphasis", p$error))
    div(class = "small text-muted",
        "Pooled column: IQMH Eq 1 across all entered levels. It suits methods whose levels perform comparably across the measuring interval; ",
        "otherwise use the per-level results.")
  })
  output$contrib_plot <- renderPlot({
    # skip while the card is too narrow for the margins (e.g. during layout)
    req(isTRUE(session$clientData$output_contrib_plot_width > 200))
    r <- res(); req(is.null(r$error), length(r$levels) > 0)
    up <- vapply(r$levels, function(x) x$u_prec^2, 0); ub <- vapply(r$levels, function(x) x$u_bias^2, 0)
    tot <- up + ub; req(any(!is.na(tot) & tot > 0))
    m <- rbind("Imprecision" = 100 * up / tot, "Bias" = 100 * ub / tot)
    colnames(m) <- paste("Level", seq_along(r$levels))
    cols <- c("#3b6ea8", "#e0a030")
    op <- par(mar = c(4, 6, 2.2, 1)); on.exit(par(op))
    barplot(m[, ncol(m):1, drop = FALSE], horiz = TRUE, las = 1, col = cols, border = NA,
            xlab = "% of u_c\u00b2", xlim = c(0, 100))
    legend("top", inset = c(0, -0.13), xpd = TRUE, horiz = TRUE, bty = "n", cex = 0.85,
           fill = cols, border = NA, legend = c("Imprecision (u_s\u00b2)", "Bias (u_B\u00b2)"))
  }, res = 96)
  # ---- "Apply to a result": reference interval / decision limits ----
  lapply(seq_len(N_LIMITS), function(j) {
    observeEvent(input[[paste0("lim_desc_", j)]],
                 edit_state(function(s) { s$limits$desc[j] <- input[[paste0("lim_desc_", j)]]; s }), ignoreInit = TRUE)
    observeEvent(input[[paste0("lim_value_", j)]],
                 edit_state(function(s) { s$limits$value[j] <- as.numeric(input[[paste0("lim_value_", j)]]); s }),
                 ignoreInit = TRUE, ignoreNULL = FALSE)
    observeEvent(input[[paste0("lim_level_", j)]],
                 edit_state(function(s) { s$limits$level[j] <- input[[paste0("lim_level_", j)]]; s }), ignoreInit = TRUE)
  })
  # "Use U of" offers Pooled and the levels in use; default Pooled
  observeEvent(input$n_levels, {
    n <- as.integer(input$n_levels)
    ch <- c(Pooled = "pooled", setNames(as.character(seq_len(n)), paste("Level", seq_len(n))))
    for (j in seq_len(N_LIMITS)) {
      sel <- isolate(rv$state$limits$level[j])
      if (length(sel) != 1 || is.na(sel) || !sel %in% ch) sel <- "pooled"
      updateSelectInput(session, paste0("lim_level_", j), choices = ch, selected = sel)
    }
  })
  output$lim_tbl <- renderTable({
    lt <- limits_table(rv$state, res()); req(lt); lt
  }, striped = TRUE, spacing = "s", align = "l")
  output$checks_all <- renderUI({
    r <- res(); if (!is.null(r$error)) return(div(class = "text-danger", r$error))
    tagList(lapply(seq_along(r$levels), function(i) {
      div(h6(sprintf("Level %d (report level %s)", i, r$levels[[i]]$qc$level)), checks_ui(r$levels[[i]]$checks))
    }))
  })

  # ---- load / save / report ----
  observeEvent(input$load_example, {
    txt <- paste(readLines("example_acetaminophen.json", encoding = "UTF-8", warn = FALSE), collapse = "\n")
    apply_state(state_from_json(txt))
    showNotification(paste("Loaded the acetaminophen example (3 levels, 6 months of IQC, CAP EQA/PT). The calibrator",
                           "uncertainty is not entered, so level 3 (mean SDI > 2) shows a red warning."),
                     type = "message", duration = 10)
  })
  # Load inputs: the Excel workbook written by "Save inputs (Excel)"; .json
  # files saved by earlier versions of the app are still accepted.
  observeEvent(input$load_inputs, {
    f <- input$load_inputs; req(f)
    ext <- tolower(tools::file_ext(f$name))
    st <- tryCatch({
      if (ext == "json") state_from_json(paste(readLines(f$datapath, encoding = "UTF-8", warn = FALSE), collapse = "\n"))
      else if (ext %in% c("xlsx", "xls")) state_from_xlsx(f$datapath)
      else stop("expected an Excel workbook (.xlsx) saved by this app.")
    }, error = function(e) { showNotification(paste("Could not read file:", conditionMessage(e)), type = "error", duration = 10); NULL })
    if (!is.null(st)) { apply_state(st); showNotification(paste("Inputs loaded from", f$name), type = "message") }
  })
  observeEvent(input$reset, apply_state(blank_state()))

  file_stub <- function() {
    a <- gsub("[^A-Za-z0-9]+", "_", rv$state$test$analyte); if (!nzchar(a)) a <- "MU"
    paste0(a, "_", format(Sys.Date(), "%Y%m%d"))
  }
  output$save_xlsx <- downloadHandler(
    filename = function() paste0(file_stub(), "_MU_inputs.xlsx"),
    content = function(file) {
      r <- res()
      state_to_xlsx(rv$state, file, results = if (is.null(r$error)) r$summary, version = APP_VERSION)
    })
  # Print report / save as PDF (button, or Ctrl/Cmd+P inside the app)
  print_report <- function() {
    html <- tryCatch(build_report(rv$state, res(), title = paste0(file_stub(), "_MU_report")),
                     error = function(e) { showNotification(paste("Could not build the report:", conditionMessage(e)), type = "error"); NULL })
    if (!is.null(html)) session$sendCustomMessage("mu_print", list(html = html))
  }
  observeEvent(input$print_report, print_report())
  observeEvent(input$print_req, print_report())
  output$dl_core <- downloadHandler(
    filename = function() "mu_topdown_core.R",
    content = function(file) file.copy("mu_topdown_core.R", file, overwrite = TRUE))
}

shinyApp(ui, server)
