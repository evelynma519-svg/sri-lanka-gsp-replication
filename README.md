# Losing EU Trade Preferences and Employment in Sri Lanka: Replication and Correction

An independent replication, in R, of a difference-in-differences analysis of how the EU's suspension of GSP+ trade preferences for Sri Lanka (2010–2017) affected the labour market. The replication found a problem in how the original outcome variables were built. This repository documents that problem and re-estimates the model with corrected data.

**Original paper:** Aisha Alsayegh, Evelyn Ma, and Miguel Villanucci Lopez (2025), course research project for EC 136, Tufts University. The original analysis was written in Stata and co-authored. This repository, including all R code, was written independently by Evelyn Ma.

---

## Summary

| Step | Result |
|---|---|
| **Exact replication** (`R/02_replicate.R`) | All 6 published coefficients and standard errors are reproduced exactly in R (`output/tables/replication_check.csv`). |
| **Diagnosis** (`R/03_diagnose.R`) | The original "employment" variable adds up 19–26 rows per sector-year and overstates each sector's employment about **12-fold**. The overstatement is smaller for manufacturing in the 2010 baseline (9-fold), which alone creates a spurious relative change of about 0.29 log points. The "Services" control group also changes definition between 2010 and 2013. |
| **Corrected estimates** (`R/04_corrected.R`) | With a consistent panel, manufacturing employment shows **no detectable change** relative to other sectors during the suspension (−0.013 log points, SE 0.034), and the event study lies well inside the range of placebo sectors. |

## Research question

Sri Lanka's GSP+ preferences were suspended with effect from August 2010 and restored in 2017. Manufacturing, especially garments, was the sector most exposed to EU tariffs. The design compares manufacturing with other sectors before and after the suspension, using sector and year fixed effects.

## What went wrong in the original data construction

ILOSTAT publishes each sector's employment many times: in the headline table and again inside cross-tabulations by education, marital status, public/private sector, and other breakdowns. Each cross-tabulation repeats the sector total and adds its sub-categories. The original Stata code selected indicators by substring (`"Employment by sex"` and `"economic activity"`) and then ran `collapse (sum)` within each sector and year. So it added together every table that contained the sector:

| Manufacturing, 2010 Q4 | Thousands |
|---|---|
| Published sector total | 1,339.7 |
| Original variable (sum of 19 rows) | 12,055.2 |

Full tables: `diag_employment_double_counting.csv`, `diag_rows_summed_manufacturing_2010Q4.csv`; see also `fig_diag_double_counting.png`.

Three further issues are documented:

- **Control group changes definition.** "Services" was assembled from ISIC Rev.4 section labels, which first appear in 2013. In 2010 only one ISIC Rev.3.1 section matches, so the group contains 6 rows in 2010 and about 60 rows from 2013 onward (`diag_services_definition_by_year.csv`).
- **Q4 as the annual value.** 2011 Q4 is missing, so all of 2011 drops out. The 2012 values come from a different, sparser source.
- **Series break.** The consistent ILO microdata series starts in 2010 Q1 ("Break in series: Methodology revised"). Earlier years come from published tables with narrower geographic coverage (`diag_quarterly_coverage.csv`).

## Corrected analysis

The corrected panel uses one indicator per outcome, sex = Total, no second breakdown, and the same classification (ILO "Aggregate" economic activity) in every year. It covers six sectors: agriculture, manufacturing, construction, mining & utilities, trade/transport/business services, and public administration & other services. It uses only the 2010–2019 microdata series. The annual value is the mean of the available quarters, so 2011 (Q1–Q3) is now included.

**Difference-in-differences, original design on corrected data** (`corrected_did.csv`; 6 sectors × 9 years):

| Outcome | Manufacturing × suspended (2011–16) | Manufacturing × reinstated (2017–19) |
|---|---|---|
| Log employment | −0.013 (0.034); RI p = 1.00 | −0.006 (0.099); RI p = 1.00 |
| Mean weekly hours | −0.40 (0.64); RI p = 1.00 | −0.82 (0.80); RI p = 0.83 |
| Informal employment rate (pp) | 3.81 (1.51); RI p = 0.33 | 4.80 (3.80); RI p = 0.50 |

Standard errors in parentheses are clustered by sector (Stata CR1, for comparability with the original). With only six clusters and one treated sector, these standard errors are unreliable. The table therefore also reports **randomization inference (RI)**: the model is re-estimated with each sector in turn labelled as "treated", and the p-value is the share of sectors whose estimate is at least as large as manufacturing's. With six sectors, the smallest attainable p-value is 1/6.

The employment result also holds with a quarterly specification whose baseline is 2010 Q1–Q2, before the suspension took effect, with sector-specific seasonality controls: −0.012 (0.042) (`corrected_did_quarterly.csv`). The informal-employment increase looks significant with clustered SEs (p ≈ 0.05) but not under randomization inference, so it should not be read as evidence of an effect.

**Event study.** `fig_event_study_placebo_sectors.png` plots manufacturing's year-by-year change relative to 2010 (blue) against the same estimate for every other sector (grey). Manufacturing stays close to zero and inside the placebo range throughout.

**Interpretation.** With corrected data, the sector-level comparison finds no evidence that the suspension reduced manufacturing employment. That is consistent with the original paper's null finding, but the original estimate rested on a mismeasured outcome, and this design can only rule out large effects. Sector aggregates also hide the most exposed industries: garments are a subset of manufacturing.

## Repository structure

```
sri-lanka-gsp-replication/
├── README.md
├── run_all.R              # runs everything in order
├── R/
│   ├── 00_setup.R         # packages, paths, parameters, helpers
│   ├── 01_import.R        # raw ILOSTAT file -> data/derived/ilostat_lka.rds
│   ├── 02_replicate.R     # exact replication of the original Stata results
│   ├── 03_diagnose.R      # documents the data-construction problems
│   └── 04_corrected.R     # corrected panel, DiD, randomization inference, event study
├── data/
│   ├── raw/               # put the ILOSTAT file here (not tracked)
│   └── derived/           # created by the scripts (not tracked)
└── output/
    ├── tables/
    └── figures/
```

## Data

The labour market data come from **ILOSTAT** bulk download for Sri Lanka, quarterly, Labour Force Survey. The original project used the file `LKA_Q-filtered-2025-10-11.csv` (about 500 MB), downloaded on 11 October 2025. It is too large for GitHub and is not tracked. To reproduce:

1. Download the Sri Lanka quarterly file from the ILOSTAT bulk download facility (https://ilostat.ilo.org/data/bulk/).
2. Save it as `data/raw/LKA_Q-filtered-2025-10-11.csv`. If you use a newer download, change the file name in `R/00_setup.R`. ILOSTAT revises data, so a newer file may not reproduce the original numbers exactly.

## How to run

Requires R (≥ 4.1). Install the packages once:

```r
install.packages(c("data.table", "ggplot2", "sandwich", "lmtest"))
```

Then, from the repository root:

```bash
Rscript run_all.R
```

Outputs go to `output/tables/` and `output/figures/`.

## Limitations and next steps

- **One treated sector.** Any sector-level design for a single country has one treated unit, which limits inference fundamentally. A natural extension is a **cross-country synthetic control**: Sri Lankan manufacturing employment compared with a weighted combination of comparable garment exporters that kept EU preferences, using the same ILOSTAT indicators.
- **Baseline timing.** The suspension was decided in early 2010 and took effect in August 2010, so an annual 2010 baseline is partly post-announcement. The quarterly specification addresses this only partially (two pre-suspension quarters).
- **No consistent pre-2010 series.** The older published series (`fig_pre2010_trends.png`) has narrower coverage and is shown descriptively only.
- **Trade data.** The original project also analysed EU imports from Sri Lanka (UN Comtrade, WITS tariffs). That part is not yet ported to this repository.

## Contact

Evelyn Ma · [LinkedIn](https://www.linkedin.com/in/-evelynma-)
