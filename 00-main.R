wd_root <- "Desktop/oswego/"

oswego <- dget(paste0(wd_root, "01-oswego-data.R"))

source(paste0(wd_root, "02-clean-the-data.R"), local = TRUE)

oswego

dplyr::glimpse(oswego)

# View(oswego)

oswego |> skimr::skim()

oswego |> dplyr::group_by(ill) |> skimr::skim()

source(paste0(wd_root, "03-roles.R"), local = TRUE)

levels_dependent <- c("Y", "N")
levels_predictor <- c("Y", "N")

source(paste0(wd_root, "04-eda-counts.R"), local = TRUE)

eda_not_all_cell_levels
eda_n_dependent_not_ok
eda_n_predictor_not_ok
eda_n_cell_not_ok

