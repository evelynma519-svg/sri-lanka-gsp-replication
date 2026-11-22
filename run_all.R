# =============================================================================
# run_all.R  --  reproduce everything from the raw ILOSTAT download.
# Usage (from the repository root):  Rscript run_all.R
# =============================================================================
source(file.path("R", "00_setup.R"))
source(file.path("R", "01_import.R"))     # -> data/derived/ilostat_lka.rds
source(file.path("R", "02_replicate.R"))  # -> exact replication of the original Stata results
source(file.path("R", "03_diagnose.R"))   # -> documents the data-construction problems
source(file.path("R", "04_corrected.R"))  # -> corrected panel, re-estimation, event study, inference
message("Done. Tables in output/tables/, figures in output/figures/.")
