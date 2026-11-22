# =============================================================================
# 03_diagnose.R
# Documents why the original outcome variables do not measure what they are
# meant to measure.
#
#   D1. Employment is summed over many cross-tabulated indicators (by
#       education, marital status, public/private sector, ...), each of which
#       repeats the sector total plus its sub-categories.
#   D2. "Services" is built from ISIC Rev.4 section labels, which only exist
#       from 2013; in 2010 a single ISIC Rev.3.1 section matches instead, so
#       the definition of the control group changes over time.
#   D3. Using Q4 as the annual value: 2011 Q4 is missing (so 2011 is lost) and
#       2012 comes from a different, sparser source.
#   D4. Series breaks: the consistent microdata-based series starts in 2010 Q1
#       ("Break in series: Methodology revised"); earlier years come from
#       published tables with narrower geographic coverage.
# =============================================================================

ilo <- readRDS(ILOSTAT_RDS)
panel <- fread(file.path(DIR_DERIVED, "replication_sector_year_panel.csv"))

EMP_IND <- "Employment by sex and economic activity (thousands)"
AGG     <- "Economic activity (Aggregate): "

# Clean benchmark: one indicator, one classification, no second breakdown.
clean_q4 <- ilo[indicator_label == EMP_IND & sex_label == "Total" & is.na(classif2_label) &
                  startsWith(classif1_label, AGG) & quarter == "Q4",
                .(sector = sub(AGG, "", classif1_label, fixed = TRUE), year, clean = obs_value)]
clean_q4 <- clean_q4[sector %in% c("Agriculture", "Manufacturing", "Construction")]

# ---- D1: summed vs. single published value ----------------------------------
d1 <- merge(panel[year >= 2010, .(sector, year, summed = employment, rows_summed = n_obs)],
            clean_q4, by = c("sector", "year"), all.x = TRUE)
d1[, ratio := round(summed / clean, 2)]
save_csv(d1, "diag_employment_double_counting")

# Which indicators feed the sum (manufacturing, 2010 Q4)?
mfg_rows <- ilo[quarter == "Q4" & year == 2010 & sex_label == "Total" &
                  classif1_label == paste0(AGG, "Manufacturing") &
                  grepl("Employment by sex", indicator_label, fixed = TRUE),
                .(indicator_label, classif2_label, obs_value)]
save_csv(mfg_rows, "diag_rows_summed_manufacturing_2010Q4")

p_d1 <- ggplot(d1[!is.na(clean)], aes(year)) +
  geom_col(aes(y = summed, fill = "Original variable (sum of rows)"), alpha = 0.8) +
  geom_point(aes(y = clean, color = "Published sector total"), size = 2) +
  facet_wrap(~ sector, scales = "free_y") +
  scale_x_continuous(breaks = seq(2010, 2019, 3)) +
  scale_fill_manual(values = "grey70", name = NULL) +
  scale_color_manual(values = "#d95f0e", name = NULL) +
  labs(title = "Original employment variable vs. published sector totals (Q4)",
       x = NULL, y = "Thousands of workers") +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
save_fig(p_d1, "fig_diag_double_counting", width = 9)

# ---- D2: what "Services" contains in each year -----------------------------
svc_patterns <- c("G. Wholesale", "H. Transportation", "I. Accommodation", "J. Information",
                  "K. Financial", "L. Real estate", "M. Professional", "N. Administrative",
                  "P. Education", "Q. Human health", "R. Arts", "S. Other service")
svc <- ilo[quarter == "Q4" & year >= 2010 &
             Reduce(`|`, lapply(svc_patterns, function(s) grepl(s, classif1_label, fixed = TRUE)))]
d2 <- svc[, .(n_rows = .N), by = .(year, classification = sub(":.*", "", classif1_label))]
d2 <- dcast(d2, classification ~ year, value.var = "n_rows", fill = 0)
save_csv(d2, "diag_services_definition_by_year")

# ---- D3/D4: quarterly coverage and source of the consistent series ----------
cov <- ilo[indicator_label == EMP_IND & sex_label == "Total" & is.na(classif2_label) &
             classif1_label == paste0(AGG, "Manufacturing"),
           .(year, quarter,
             source = fifelse(grepl("Micro data processing", note_source_label),
                              "ILO microdata (2010 methodology)", "Published tables (older series)"))]
d3 <- dcast(cov, year + source ~ quarter, fun.aggregate = length)
save_csv(d3, "diag_quarterly_coverage")

print(d1)
print(d2)
print(d3)
