# Top-down measurement uncertainty (MU) calculator

**Live:** https://shucez.github.io/mu-topdown/ (GitHub Pages) and https://home.cc.umanitoba.ca/~zhangs57/mu/ (UofM).

A browser-only R Shiny app (compiled with [shinylive](https://posit-dev.github.io/r-shinylive/)) that estimates measurement uncertainty from IQC and EQA/PT data, following the IQMH *Guidance on Measurement Uncertainty for Medical Laboratories* (2014) and CLSI C51-A. All calculations run in the visitor's browser (webR). No data are sent to any server.

## Files

| Path | What it is |
|---|---|
| `app/mu_topdown_core.R` | **All calculations**: pure, modular, annotated functions, each citing its source (IQMH equation/appendix, CLSI section/equation). This is the file to audit. |
| `app/mu_io.R` | Input state: blank templates, mapping of the report-style grid headers to the calculation inputs, and the Excel workbook used to save and load inputs (JSON for the bundled example). No maths. |
| `app/mu_report.R` | Presentation: number formatting, the intermediate-result tables and the printable report. No maths. |
| `app/app.R` | User interface and server. |
| `app/example_acetaminophen.json` | Example data (3 levels, 6 months of IQC, CAP EQA/PT), loaded by the "Load example" button. |
| `tests/test_mu_topdown_core.R` | Validation of the core. IQC: CLSI C51-A ANOVA examples (Ex 4, Ex 6a, App B) and identity with R's `anova()`. Bias: IQMH Appendices A–C, the CAP screenshot, and the bias/combination intermediates of the spreadsheet the example comes from. Also IQMH and CLSI combination, conversion and rounding examples. |
| `tests/make_example_json.R` | Rebuilds the example JSON from the workbook values. |
| `build_site.R` | Installs missing packages into `.Rlib/`, runs the tests, exports `app/` to `site/` with shinylive, and writes `.htaccess`. |
| `.github/workflows/deploy-pages.yml` | On every push to `main`: installs R, runs `build_site.R` (tests, then export) and publishes `site/` to GitHub Pages. |
| `site/` | Generated static website (not in the repository). Upload its contents to `public_html`. |
| `references/` | Source documents kept locally and **not** in the repository: the CLSI and IQMH guidance, the spreadsheet, the report screenshots and the calibrator traceability sheets. The CLSI document is licensed and must not be redistributed. |

## Run, test, build

Requires R ≥ 4.3 with `shiny`, `bslib`, `rhandsontable`, `jsonlite`, `readxl` and `writexl`. `build_site.R` installs any that are missing into the project library `.Rlib/`, which the tests also use.

```bash
Rscript tests/test_mu_topdown_core.R
```

```bash
Rscript -e 'shiny::runApp("app")'
```

```bash
Rscript build_site.R
```

`build_site.R` installs `shinylive` into `.Rlib/` on first use. It needs internet access to CRAN, GitHub (shinylive web assets) and repo.r-wasm.org (WebAssembly R packages), and it stops if any validation test fails.

## Deploy to GitHub Pages

Automatic: every push to `main` runs `.github/workflows/deploy-pages.yml`, which rebuilds the site and publishes it at https://shucez.github.io/mu-topdown/ after a few minutes (repository Settings → Pages → Source: GitHub Actions). If a validation test fails, nothing is published. A run can also be started by hand from the Actions tab ("Run workflow"). GitHub Pages caches files for about 10 minutes, so visitors may see the previous version briefly after an update.

## Deploy to `public_html`

Copy the **contents** of `site/` into a folder under your web directory, e.g. `~/public_html/mu/`:

```bash
rsync -av --delete site/ YOUR_ID@YOUR_UNIX_HOST:~/public_html/mu/
```

```bash
ssh YOUR_ID@YOUR_UNIX_HOST 'chmod -R a+rX ~/public_html/mu'
```

Then open the matching address, e.g. `https://home.cc.umanitoba.ca/~YOUR_ID/mu/`. Requirements:
- **HTTPS.** Shinylive uses a service worker, which browsers only allow over HTTPS (or on `localhost`).
- **`.htaccess` is honoured.** It sets the WebAssembly MIME type (`application/wasm`). It also tells browsers to revalidate `app.json`, `index.html` and the service worker, so returning visitors get updates. If the server ignores `.htaccess`, the app may fail to load. After an update, visitors may then also see the old version until they hard-refresh.
- **Copy hidden files too.** `rsync` or `scp -r` copies `.htaccess`; drag-and-drop tools may skip it.

The first visit downloads about 30–60 MB (webR and packages), so it takes 10–30 s; the browser cache makes later visits fast. `index.html` must be opened over http(s); double-clicking the file locally will not work.

## Using the app

The app opens on **Getting started**, which begins with a checklist of the data to gather (calibrator traceability and uncertainty sheet; monthly QC reports covering at least six months; EQA/PT survey evaluations or CRM measurements) and then explains the method, notation and sources.

1. **Sidebar, Test:**
   - analyte, method, instrument and units;
   - **Uncertainty of calibrator (u_ref):** the Calibrator Value and u_ccal (or u_cal if no u_ccal is given), from the manufacturer's traceability sheet (e.g. Roche C.f.a.s.). Both are needed because u_ref = u ÷ calibrator value;
   - number of levels (1–5).
2. **Sidebar, Calculation**, in the order the calculation uses them:
   - IQC pooling method;
   - when to include SR_g and u_ref;
   - SDI limit;
   - k (default 2; 1.96 available);
   - optional target U.
3. **IQC imprecision**: one card per level. Each row is one monthly Unity report ("Your Lab – Mon"): **Date | Mean | SD | # Points**.
   - Date is information only.
   - CV is not entered: it is calculated from SD and Mean.
4. **Bias (EQA/PT or CRM)**: one card per level, which you match to the IQC level.
   - **EQA/PT** rows follow the CAP evaluation: **Survey | Specimen | Your Result | Mean | S.D. | No. of Labs | S.D.I**. S.D.I is optional and only checks transcription.
   - **CRM** instead takes the certified value, its expanded U and k, and the replicate results.
5. **Results**:
   - Key results: IQC mean, EQA/PT mean used, U % and U at the IQC mean.
   - Every intermediate value, and a Pooled column across all entered levels.
   - **Apply to a result:** up to three reference interval or clinical decision limits, each with U from the pooled estimate (default) or one level.
   - Warnings. A missing calibrator uncertainty that the calculation needs is shown in red.
6. **Report and inputs** (sidebar):
   - **Print report / save as PDF** (or Ctrl/Cmd+P inside the app) prints a formatted report:
     - page 1: test information, settings, the summary, the limits and sign-off lines;
     - then one page per level with all intermediate tables;
     - then a method summary.

     Choose "Save as PDF" in the print dialog to archive it.
   - **Save inputs (Excel)** downloads a workbook, and **Load inputs…** reads it back. `.json` files from earlier versions also load. Nothing is stored online.

## Excel workbook

The workbook can be read and edited in Excel and then loaded back. It has these sheets:

| Sheet | Contents |
|---|---|
| About | How the workbook is laid out. |
| Test | Field / Value / Notes: test, calibrator, number of levels and calculation settings. Settings are written as their on-screen labels. |
| Levels | One row per level: level as on the report, control material, lot, bias source (EQA/PT, CRM or None), provider, Peer Group, CRM details. |
| IQC | Level, Date, Mean, SD, # Points: one row per month. |
| EQA_PT | Level, Survey, Specimen, Your Result, Mean, S.D., No. of Labs, S.D.I: one row per challenge. |
| CRM | Level, Result: one row per replicate. |
| Limits | Description, Value, Use U of (Pooled or Level n). |
| Results | The intermediate results at the time of saving. For reference only; it is ignored on loading. |

## Method notes

- **IQC pooling.** The months are treated as the groups of a one-way ANOVA (CLSI C51-A §7.3), computed exactly from each month's # Points, Mean and SD:
  - *Pooled monthly CV*: s_w = √(Σ(nᵢ−1)sᵢ² / (N−m)) (CLSI Eq 6 / Eq 27), divided by the mean of all results.
  - *Pooled monthly CV with month-to-month shifts*: adds the between-month component, s_b² = max(0, (MS_b − MS_w)/n₀) (CLSI Eqs 28–29). It captures shifts at lot changes and recalibrations. In the example, level 1 is 8.5 % vs 4.0 %.
- **Pooling across levels.** CVs are weighted by each level's within-month degrees of freedom (N − m).
- **Uncertainty of bias (IQMH Eq 2):** u_B = √(RMS_bias² + SR_g² + u_ref²).
  - **SR_g, uncertainty of the EQA/PT assigned value:** from the report's S.D./√No. of Labs (IQMH App C).
  - **u_ref, uncertainty of the calibrator's assigned value:** u_ccal (or u_cal) ÷ Calibrator Value, entered once per test from the manufacturer's traceability sheet (standard uncertainties, k = 1).
  - SR_g and u_ref are included only when the mean SDI exceeds the SDI limit (default 2; IQMH top-down alternative, step 3d). If u_ref is then missing, the calculation continues with u_ref = 0 and shows a red warning.
- **Reporting.** U is rounded up to two significant digits and the result is stated to the same decimal place (CLSI C51-A §9.2).
- **Two SDIs.** The SDI printed on the CAP report is (Your Result − Mean)/peer S.D. The SDI that decides whether SR_g and u_ref are included is IQMH's |relative bias| / your IQC CV. The app computes both and uses only the second for the rule.
- **Notation** follows IQMH (u_s, u_B, u_c, U, SR_g, u_ref) and, for the ANOVA, CLSI C51-A §7.3 (SS/MS/s with subscripts wth/btw). The Getting started page in the app lists the data to gather and defines every symbol.
