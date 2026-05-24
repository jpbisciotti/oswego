wd_root <- "/Users/johnpaulbisciotti/Desktop/oswego/"

oswego <- dget(paste0(wd_root, "01-oswego-data.R"))

source(paste0(wd_root, "02-clean-the-data.R"), local = TRUE)

oswego

dplyr::glimpse(oswego)

# View(oswego)

oswego |> skimr::skim()

oswego |> dplyr::group_by(ill) |> skimr::skim()

source(paste0(wd_root, "03-roles.R"), local = TRUE)

source(paste0(wd_root, "04-handle-missingness.R"), local = TRUE)

source(paste0(wd_root, "05-eda-counts.R"), local = TRUE)

# eda_analysis
eda_not_all_cell_levels
eda_n_dependent_not_ok
eda_n_predictor_not_ok
eda_n_cell_not_ok

source(paste0(wd_root, "06-eda-metrics.R"), local = TRUE)

eda_metrics


