# =============================================================================
# 04_corrected.R
# Corrected panel and re-estimation.
#
# Data construction (fixes D1-D4 from 03_diagnose.R):
#   * one indicator per outcome, sex = Total, no second breakdown
#   * one consistent classification ("Economic activity (Aggregate)") in
#     every year, so sector definitions do not change over time
#   * only the 2010-2019 ILO microdata series (same methodology throughout);
#     the 2012 published-table values and pre-2010 series are not mixed in
#   * annual value = mean of the available quarters (2011 uses Q1-Q3), so
#     2011 is no longer lost
#
# Estimation:
#   C1. The original design (2010 baseline; suspended 2011-2016; reinstated
#       2017-2019; sector and year FE) on the corrected panel.
#   C2. Inference with few clusters: randomization inference that assigns
#       "treatment" to each sector in turn, alongside Stata-style CR1 SEs.
#   C3. Event study: manufacturing x year, relative to 2010, plotted against
#       the same estimate for every other sector (placebo sectors).
#   C4. Quarterly version with a cleaner baseline: 2010 Q1-Q2, before the
#       suspension took effect in August 2010.
#   C5. Pre-2010 descriptive trends from the older published series.
# =============================================================================

ilo <- readRDS(ILOSTAT_RDS)
AGG <- "Economic activity (Aggregate): "

SECTOR_LABELS <- c(
  "Agriculture"   = "Agriculture",
  "Manufacturing" = "Manufacturing",
  "Construction"  = "Construction",
  "Mining and quarrying; Electricity, gas and water supply" = "Mining & utilities",
  "Trade, Transportation, Accommodation and Food, and Business and Administrative Services" =
    "Trade, transport & business services",
  "Public Administration, Community, Social and other Services and Activities" =
    "Public admin & other services")

OUTCOMES <- c(
  employment    = "Employment by sex and economic activity (thousands)",
  hours         = "Mean weekly hours actually worked per employed person by sex and economic activity",
  informal_rate = "Informal employment rate by sex and economic activity (%)")

# ---- Build the quarterly and annual panels ----------------------------------
q <- ilo[indicator_label %in% OUTCOMES & sex_label == "Total" & is.na(classif2_label) &
           startsWith(classif1_label, AGG) &
           grepl("Micro data processing", note_source_label, fixed = TRUE) &
           year >= 2010 & year <= 2019]
q[, sector := SECTOR_LABELS[sub(AGG, "", classif1_label, fixed = TRUE)]]
q <- q[!is.na(sector)]
q[, outcome := names(OUTCOMES)[match(indicator_label, OUTCOMES)]]
q <- dcast(q, sector + year + quarter ~ outcome, value.var = "obs_value")
q[, `:=`(qtime = year + (as.integer(substr(quarter, 2, 2)) - 1) / 4,
         manufacturing = as.integer(sector == "Manufacturing"),
         log_employment = log(employment))]

annual <- q[, .(employment = mean(employment), hours = mean(hours),
                informal_rate = mean(informal_rate), n_quarters = .N),
            by = .(sector, year)]
annual[, `:=`(manufacturing  = as.integer(sector == "Manufacturing"),
              log_employment = log(employment))]
setorder(annual, sector, year)
fwrite(annual, file.path(DIR_DERIVED, "corrected_sector_year_panel.csv"))
save_csv(dcast(annual, year ~ sector, value.var = "n_quarters"), "corrected_panel_quarters_per_year")

# ---- C1 + C2: original design on the corrected panel ------------------------
add_periods <- function(df, treated_sector) {
  df <- copy(df)
  df[, treat := as.integer(sector == treated_sector)]
  df[, `:=`(treat_suspended  = treat * as.integer(year > SUSPENSION_YEAR & year < REINSTATEMENT_YEAR),
            treat_reinstated = treat * as.integer(year >= REINSTATEMENT_YEAR))]
  df
}
did_fit <- function(df, y, treated_sector = "Manufacturing") {
  df <- add_periods(df[!is.na(get(y))], treated_sector)
  lm(as.formula(paste(y, "~ treat_suspended + treat_reinstated + factor(sector) + factor(year)")),
     data = df)
}

# Randomization inference: re-estimate with each sector labelled "treated";
# p-value = share of sectors whose estimate is at least as large in absolute
# value as manufacturing's. With 6 sectors the smallest attainable p is 1/6.
ri_pvalue <- function(df, y, term) {
  sectors <- unique(df$sector)
  est <- vapply(sectors, function(s) coef(did_fit(df, y, s))[[term]], numeric(1))
  mean(abs(est) >= abs(est[["Manufacturing"]]) - 1e-12)
}

control_sets <- list(
  "All other sectors" = unique(annual$sector),
  "Excluding mining & utilities" = setdiff(unique(annual$sector), "Mining & utilities"))

c1 <- list()
for (cs in names(control_sets)) {
  df_cs <- annual[sector %in% control_sets[[cs]]]
  for (y in c("log_employment", "hours", "informal_rate")) {
    fit <- did_fit(df_cs, y)
    V   <- vcov_stata_cluster(fit, add_periods(df_cs[!is.na(get(y))], "Manufacturing")$sector)
    for (term in c("treat_suspended", "treat_reinstated")) {
      c1[[length(c1) + 1]] <- coef_row(fit, V, term, y,
        list(controls = cs, n_sectors = length(control_sets[[cs]]),
             p_randomization = ri_pvalue(df_cs, y, term)),
        df = length(control_sets[[cs]]) - 1)
    }
  }
}
c1 <- rbindlist(c1)
setnames(c1, c("se", "p"), c("se_cluster_CR1", "p_cluster_CR1"))
save_csv(c1, "corrected_did")
print(c1, digits = 3)

# ---- C3: event study with placebo sectors -----------------------------------
event_coefs <- function(df, treated_sector) {
  df <- copy(df)
  df[, treat := as.integer(sector == treated_sector)]
  yrs <- setdiff(sort(unique(df$year)), SUSPENSION_YEAR)
  for (yy in yrs) df[, paste0("ev_", yy) := treat * as.integer(year == yy)]
  fit <- lm(as.formula(paste("log_employment ~", paste0("ev_", yrs, collapse = " + "),
                             "+ factor(sector) + factor(year)")), data = df)
  data.table(treated = treated_sector, year = c(SUSPENSION_YEAR, yrs),
             estimate = c(0, unname(coef(fit)[paste0("ev_", yrs)])))
}
es <- rbindlist(lapply(unique(annual$sector), function(s) event_coefs(annual, s)))
save_csv(es, "corrected_event_study")

p_es <- ggplot(es, aes(year, estimate, group = treated)) +
  annotate("rect", xmin = SUSPENSION_YEAR + 0.6, xmax = REINSTATEMENT_YEAR - 0.4,
           ymin = -Inf, ymax = Inf, alpha = 0.08) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_line(data = es[treated != "Manufacturing"], color = "grey75") +
  geom_line(data = es[treated == "Manufacturing"], color = "#08519c", linewidth = 1.1) +
  geom_point(data = es[treated == "Manufacturing"], color = "#08519c", size = 2) +
  scale_x_continuous(breaks = 2010:2019) +
  labs(title = "Manufacturing employment relative to other sectors, vs. 2010",
       subtitle = "Blue: manufacturing. Grey: the same estimate with each other sector as 'treated' (placebos).\nShaded: GSP+ suspended. 2012 not available in the consistent series.",
       x = NULL, y = "Log employment, relative to 2010") +
  theme_minimal(base_size = 11)
save_fig(p_es, "fig_event_study_placebo_sectors", width = 8.5, height = 5)

# ---- C4: quarterly design with a pre-suspension baseline --------------------
# Baseline = 2010 Q1-Q2 (suspension effective 15 Aug 2010). Sector-specific
# quarter-of-year effects absorb seasonality.
qd <- q[!is.na(log_employment)]
qd[, `:=`(qoy = quarter,
          treat_suspended  = manufacturing * as.integer(qtime >= 2010.5 & qtime < REINSTATEMENT_YEAR),
          treat_reinstated = manufacturing * as.integer(qtime >= REINSTATEMENT_YEAR))]
fit_q <- lm(log_employment ~ treat_suspended + treat_reinstated +
              factor(sector):factor(qoy) + factor(sector) + factor(qtime), data = qd)
V_q <- vcov_stata_cluster(fit_q, qd$sector)
G_q <- uniqueN(qd$sector)
c4 <- rbind(coef_row(fit_q, V_q, "treat_suspended",  "log_employment (quarterly)", df = G_q - 1),
            coef_row(fit_q, V_q, "treat_reinstated", "log_employment (quarterly)", df = G_q - 1))
save_csv(c4, "corrected_did_quarterly")
print(c4, digits = 3)

# ---- C5: pre-2010 trends from the older published series --------------------
old <- ilo[indicator_label == OUTCOMES[["employment"]] & sex_label == "Total" &
             is.na(classif2_label) & startsWith(classif1_label, AGG) & year <= 2009]
old[, sector := SECTOR_LABELS[sub(AGG, "", classif1_label, fixed = TRUE)]]
old <- old[!is.na(sector), .(employment = mean(obs_value)), by = .(sector, year)]
old[, index := 100 * employment / employment[year == min(year)], by = sector]
save_csv(old, "pre2010_published_series")
p_old <- ggplot(old, aes(year, index, color = sector == "Manufacturing", group = sector)) +
  geom_line(linewidth = 0.9) + geom_point(size = 1.5) +
  scale_color_manual(values = c("grey60", "#08519c"), labels = c("Other sectors", "Manufacturing"),
                     name = NULL) +
  labs(title = "Pre-2010 employment trends (older published series, 2004 = 100)",
       subtitle = "Narrower geographic coverage than the 2010+ series; not pooled with it.",
       x = NULL, y = "Index") +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
save_fig(p_old, "fig_pre2010_trends")
