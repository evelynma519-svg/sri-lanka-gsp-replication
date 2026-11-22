# =============================================================================
# 02_replicate.R
# Exact replication of the original Stata analysis (Main_Do_file.do, Blocks
# 4-9) in R. Every step mirrors the original code, including its choices:
#   * Q4 of each year is used as the annual observation
#   * sectors are assigned by substring matching on classif1 labels
#   * outcomes are built by substring matching on indicator labels and then
#     collapsed to sector-year with (sum) employment, (mean) hours/informality
#   * 2010-2019, 2012 dropped; sector and year fixed effects; standard errors
#     clustered by sector (4 clusters)
# Nothing is corrected here. See 03_diagnose.R and 04_corrected.R.
# =============================================================================

ilo <- readRDS(ILOSTAT_RDS)

# ---- Block 4: Q4 only, sector assignment ------------------------------------
d <- ilo[year >= 2000 & year <= 2019 & quarter == "Q4"]
has <- function(s) grepl(s, d$classif1_label, fixed = TRUE)
d[, sector := ""]
d[has("C. Manufacturing") |
    classif1_label == "Economic activity (Aggregate): Manufacturing", sector := "Manufacturing"]
for (s in c("G. Wholesale", "H. Transportation", "I. Accommodation", "J. Information",
            "K. Financial", "L. Real estate", "M. Professional", "N. Administrative",
            "P. Education", "Q. Human health", "R. Arts", "S. Other service")) {
  d[has(s), sector := "Services"]
}
d[has("F. Construction") |
    classif1_label == "Economic activity (Aggregate): Construction", sector := "Construction"]
d[has("A. Agriculture") |
    classif1_label == "Economic activity (Aggregate): Agriculture", sector := "Agriculture"]
d <- d[sector != ""]

# ---- Block 5: outcomes by substring matching, then collapse -----------------
ind <- d$indicator_label
tot <- d$sex_label == "Total"
hit <- function(...) Reduce(`&`, lapply(c(...), function(s) grepl(s, ind, fixed = TRUE)))
d[, employment          := fifelse(hit("Employment by sex", "economic activity") & tot, obs_value, NA_real_)]
d[, hours               := fifelse(hit("Mean weekly hours", "economic activity") & tot, obs_value, NA_real_)]
d[, informal_employment := fifelse(hit("Informal employment by", "economic activity") & tot, obs_value, NA_real_)]
d[, informal_rate       := fifelse(hit("Informal employment rate", "economic activity") & tot, obs_value, NA_real_)]

# Stata's collapse: (sum) ignores missing and returns 0 if all are missing;
# (mean) returns missing if all are missing; (count) counts non-missing.
stata_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
panel <- d[, .(employment          = sum(employment, na.rm = TRUE),
               informal_employment = sum(informal_employment, na.rm = TRUE),
               hours               = stata_mean(hours),
               informal_rate       = stata_mean(informal_rate),
               n_obs               = sum(!is.na(employment))),
           by = .(sector, year)]
setorder(panel, sector, year)
fwrite(panel, file.path(DIR_DERIVED, "replication_sector_year_panel.csv"))

# ---- Blocks 6-9: sample, treatment variables, regressions -------------------
a <- panel[year >= 2010 & year <= 2019 & year != 2012]
a[, `:=`(manufacturing  = as.integer(sector == "Manufacturing"),
         log_employment = fifelse(employment > 0, log(employment), NA_real_))]
a[, period := fcase(year == 2010, 1L,
                    year >= 2013 & year <= 2016, 2L,
                    year >= 2017 & year <= 2019, 3L)]
a[, `:=`(mfg_suspended  = manufacturing * as.integer(period == 2L),
         mfg_reinstated = manufacturing * as.integer(period == 3L))]

run_did <- function(y) {
  df  <- a[!is.na(get(y))]
  fit <- lm(as.formula(paste(y, "~ mfg_suspended + mfg_reinstated + factor(sector) + factor(year)")),
            data = df)
  V <- vcov_stata_cluster(fit, df$sector)
  G <- uniqueN(df$sector)
  rbind(coef_row(fit, V, "mfg_suspended",  y, df = G - 1),
        coef_row(fit, V, "mfg_reinstated", y, df = G - 1))
}
rep <- rbindlist(lapply(c("log_employment", "hours", "informal_rate"), run_did))

# ---- Compare with the original Stata output (analysis_log.log) --------------
original <- data.table(
  spec = rep(c("log_employment", "hours", "informal_rate"), each = 2),
  term = rep(c("mfg_suspended", "mfg_reinstated"), 3),
  orig_estimate = c(-0.051, 0.006, 0.892, 0.471, 11.961, 9.021),
  orig_se       = c( 0.393, 0.442, 1.773, 1.546,  8.063, 8.539),
  orig_n        = 32L)
check <- merge(original, rep, by = c("spec", "term"), sort = FALSE)
check[, matches := round(estimate, 3) == orig_estimate & round(se, 3) == orig_se & n == orig_n]
save_csv(check, "replication_check")
print(check[, .(spec, term, orig_estimate, estimate = round(estimate, 3),
                orig_se, se = round(se, 3), orig_n, n, matches)])
if (!all(check$matches)) message("Some estimates differ from the original Stata output.")
