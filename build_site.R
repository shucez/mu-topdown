################################################################################
#  Build the static site: exports app/ with shinylive into site/, ready to copy
#  into public_html. Run from the project root:
#
#      Rscript build_site.R
#
#  Needs internet the first time (CRAN for shinylive, GitHub for the shinylive
#  web assets, repo.r-wasm.org for WebAssembly builds of the R packages).
################################################################################

Sys.setlocale("LC_ALL", "en_US.UTF-8")

# shinylive and any missing app packages go into a project-local library so the
# system library is untouched. shinylive bundles the WebAssembly builds of the
# packages the app uses, which must also be installed locally to be detected.
lib <- file.path(getwd(), ".Rlib")
dir.create(lib, showWarnings = FALSE)
.libPaths(c(lib, .libPaths()))
pkgs <- c("shinylive", "shiny", "bslib", "rhandsontable", "jsonlite", "readxl", "writexl")
need <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(need)) install.packages(need, lib = lib, repos = "https://cloud.r-project.org")

# Refuse to publish if the calculation tests fail.
status <- system2(file.path(R.home("bin"), "Rscript"), "tests/test_mu_topdown_core.R")
if (status != 0) stop("Validation tests failed; site not built.")

unlink("site", recursive = TRUE)
shinylive::export(appdir = "app", destdir = "site",
                  template_params = list(title = "Top-down MU calculator"))

# Some Apache servers do not know the WebAssembly MIME type. The app code
# (app.json), page and service worker must be revalidated on every visit, or
# browsers keep serving an old version of the app after an update.
writeLines(c(
  "# Serve WebAssembly and app files with correct types (shinylive / webR)",
  "AddType application/wasm .wasm",
  "AddType application/json .json",
  "AddType text/javascript .js .mjs",
  "",
  "# Always revalidate the app itself so updates reach returning visitors",
  "<IfModule mod_headers.c>",
  "  <FilesMatch \"(app\\.json|index\\.html|shinylive-sw\\.js)$\">",
  "    Header set Cache-Control \"no-cache\"",
  "  </FilesMatch>",
  "</IfModule>"
), "site/.htaccess")

size_mb <- sum(file.info(list.files("site", recursive = TRUE, full.names = TRUE, all.files = TRUE))$size) / 1e6
message(sprintf("Site written to %s (%.0f MB). Copy the *contents* of site/ to your public_html folder.",
                normalizePath("site"), size_mb))
