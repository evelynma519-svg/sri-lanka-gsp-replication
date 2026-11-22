# =============================================================================
# 01_import.R
# Read the raw ILOSTAT bulk download (~500 MB) once and keep only the rows
# used anywhere in the analysis: indicators broken down by economic activity.
# Output: data/derived/ilostat_lka.rds
# =============================================================================

if (!file.exists(RAW_ILOSTAT)) {
  stop("Raw ILOSTAT file not found at ", RAW_ILOSTAT, ". See README > Data.", call. = FALSE)
}

message("Reading ", RAW_ILOSTAT, " (this takes a minute)...")
ilo <- fread(RAW_ILOSTAT,
             select = c("indicator.label", "sex.label", "classif1.label",
                        "classif2.label", "time", "obs_value",
                        "note_indicator.label", "note_source.label"),
             encoding = "UTF-8", showProgress = FALSE)
setnames(ilo, gsub("\\.", "_", names(ilo)))
n_raw <- nrow(ilo)

ilo <- ilo[grepl("economic activity", indicator_label, fixed = TRUE)]
ilo[, `:=`(year    = as.integer(substr(time, 1, 4)),
           quarter = substr(time, 5, 6))]
# Empty strings and NA mean the same thing (no second breakdown).
ilo[classif2_label == "", classif2_label := NA_character_]

saveRDS(ilo, ILOSTAT_RDS)
message(sprintf("Kept %s of %s rows (indicators by economic activity). Saved %s",
                format(nrow(ilo), big.mark = ","), format(n_raw, big.mark = ","),
                ILOSTAT_RDS))
