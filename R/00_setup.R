# =============================================================================
# 00_setup.R
# Packages, paths, parameters, and helpers. Sourced by run_all.R.
# Run everything from the repository root.
# =============================================================================

PKGS <- c("data.table", "ggplot2", "sandwich", "lmtest")
missing_pkgs <- PKGS[!vapply(PKGS, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Missing packages. Run once:\n  install.packages(c(",
       paste0('"', missing_pkgs, '"', collapse = ", "), "))", call. = FALSE)
}
for (p in PKGS) suppressPackageStartupMessages(library(p, character.only = TRUE))

if (!file.exists("README.md") || !dir.exists("R")) {
  stop("Please run the scripts from the repository root (the folder with README.md).",
       call. = FALSE)
}

DIR_RAW     <- file.path("data", "raw")
DIR_DERIVED <- file.path("data", "derived")
DIR_TABLES  <- file.path("output", "tables")
DIR_FIGS    <- file.path("output", "figures")
for (d in c(DIR_DERIVED, DIR_TABLES, DIR_FIGS)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

RAW_ILOSTAT  <- file.path(DIR_RAW, "LKA_Q-filtered-2025-10-11.csv")
ILOSTAT_RDS  <- file.path(DIR_DERIVED, "ilostat_lka.rds")

# Policy timing. The EU temporarily withdrew GSP+ preferences for Sri Lanka
# with effect from 15 August 2010 and restored them in May 2017.
SUSPENSION_YEAR    <- 2010
REINSTATEMENT_YEAR <- 2017

# ---- Helpers ----------------------------------------------------------------

# Stata's "reg ..., vce(cluster g)" variance: CR1 with the small-sample factor
# G/(G-1) * (N-1)/(N-K). sandwich::vcovCL(type = "HC1", cadjust = TRUE) is the
# same estimator, so standard errors match Stata exactly.
vcov_stata_cluster <- function(fit, cluster) {
  sandwich::vcovCL(fit, cluster = cluster, type = "HC1", cadjust = TRUE)
}

# One coefficient with its standard error and p-value (t distribution with
# G - 1 degrees of freedom for clustered SEs, as Stata reports).
coef_row <- function(fit, V, term, label, extra = list(), df = NULL) {
  est <- unname(coef(fit)[term]); se <- sqrt(V[term, term])
  if (is.null(df)) df <- fit$df.residual
  data.table(spec = label, term = term, estimate = est, se = se,
             p = 2 * pt(-abs(est / se), df), n = nobs(fit), as.data.table(extra))
}

save_csv <- function(x, name) fwrite(x, file.path(DIR_TABLES, paste0(name, ".csv")))

save_fig <- function(plot, name, width = 8, height = 4.5) {
  ggsave(file.path(DIR_FIGS, paste0(name, ".png")), plot,
         width = width, height = height, dpi = 300)
}
