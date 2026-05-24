# Choice of handlers ===========================================================

# 1/ Complete-case analysis — drop 2/75 = 2.7 %
# 
# 2/ Mode imputation by stratum (ill × food) — instant; for row 29 the stratum
# strongly votes "Y" (most cases ate chocolate ice cream); for row 53 the
# stratum is more even on mashed potato (split among non-ill).
#
# 3/ mice with method = "logreg" or "logreg.boot" restricted to the food block, m =
# 5–20 — the pedagogically-aligned route. Will produce essentially identical
# point estimates to the above, which is itself the pedagogical point.
#
# 4/ Code NA as a third level ("U") and retain the row — defensible if you want to
# avoid imputing at all.

# Try 3 ========================================================================

# Multiple imputation of the two missing food-exposure cells in the cleaned
# Oswego dataset using `mice`

# `mice` infers an imputation model from each column's data type, so the binary
# food and outcome columns must be factors (NOT character). Two-level factors
# with NAs default to method = "logreg.boot", which is what we want.

oswego_mi <- oswego |>
  # # IDs cannot serve as predictors
  dplyr::select(-tidyselect::all_of(c(role_id, role_independent_numeric))) |>
  dplyr::mutate(dplyr::across(tidyselect::everything(), factor))

# logreg vs logreg.boot: the default logreg draws regression coefficients from
# their asymptotic posterior. For small n where the asymptotics are more
# uncertain, logreg.boot bootstraps the data first, then fits the logistic
# regression — slightly more honest about model uncertainty.

vars_with_missingness <- oswego_mi |>
  dplyr::select(tidyselect::where(~ any(is.na(.x)))) |>
  names()

meth <- mice::make.method(oswego_mi)

meth_logreg <- meth
meth_logreg_boot <- meth

meth_logreg[vars_with_missingness] <- "logreg"
meth_logreg_boot[vars_with_missingness] <- "logreg.boot"

# Predictors: use all other columns. `make.predictorMatrix` produces the
# standard "everything-predicts-everything" matrix with diagonal = 0.

pred <- mice::make.predictorMatrix(oswego_mi)

#  Run the imputations
imp_logreg <- mice::mice(
  oswego_mi,
  method = meth_logreg,
  predictorMatrix = pred,
  m = 100,
  maxit = 10,
  seed = 123,
  printFlag = FALSE
)

imp_logreg_boot <- mice::mice(
  oswego_mi,
  method = meth_logreg_boot,
  predictorMatrix = pred,
  m = 100,
  maxit = 10,
  seed = 123,
  printFlag = FALSE
)

imp_logreg$imp$mashed_potato |> as.matrix() |> as.vector() |> janitor::tabyl()
imp_logreg_boot$imp$mashed_potato |> as.matrix() |> as.vector() |> janitor::tabyl()

imp_logreg$imp$chocolate_ice_cream |> as.matrix() |> as.vector() |> janitor::tabyl()
imp_logreg_boot$imp$chocolate_ice_cream |> as.matrix() |> as.vector() |> janitor::tabyl()

# Pool a downstream analysis across imputations. Demonstrate Rubin's rules on a
# logistic regression of illness on a couple of food exposures. `with()` fits
# the same model on each completed dataset; `pool()` combines the estimates and
# adjusts SEs.

fit_logreg <- with(imp_logreg, glm(ill ~ vanilla_ice_cream + mashed_potato + chocolate_ice_cream, family = binomial))
fit_logreg_boot <- with(imp_logreg_boot, glm(ill ~ vanilla_ice_cream + mashed_potato + chocolate_ice_cream, family = binomial))

pooled_logreg <- mice::pool(fit_logreg)
pooled_logreg_boot <- mice::pool(fit_logreg_boot)

pooled_logreg |> broom::tidy() |> dplyr::select(term, estimate, riv, fmi, lambda)
pooled_logreg_boot |> broom::tidy() |> dplyr::select(term, estimate, riv, fmi, lambda)

# FMI ≈ 3 % across coefficients, meaning the imputation adds ~3 % to total
# variance versus a complete-data analysis. m = 20 is plenty; m = 5 would be
# defensible. This is also the empirical evidence for this dataset MI is
# methodologically valid but operationally near-equivalent to complete-case
# analysis.

oswego_logreg <- mice::complete(imp_logreg, action = "long") |> tibble::as_tibble()
oswego_logreg_boot <- mice::complete(imp_logreg_boot, action = "long") |> tibble::as_tibble()

oswego_imp <- dplyr::bind_rows(oswego_logreg, oswego_logreg_boot) |>
  dplyr::select(.id, tidyselect::all_of(vars_with_missingness)) |>
  dplyr::group_by(dplyr::across(tidyselect::all_of(c(".id", vars_with_missingness)))) |>
  dplyr::summarize(n = dplyr::n()) |>
  dplyr::ungroup() |>
  # Keep the mode.
  # If mode is a tie, handle later with dplyr::distinct()
  dplyr::group_by(.id) |>
  dplyr::filter(n == max(n)) |>
  dplyr::ungroup() |>
  dplyr::select(-n) |>
  # In case of a tie for n
  dplyr::distinct(.id, .keep_all = TRUE) |>
  # Prepare for dplyr::bind_cols()
  dplyr::arrange(.id) |>
  dplyr::select(tidyselect::all_of(vars_with_missingness))

# Add imputed NA to working dataset
oswego <- dplyr::bind_cols(
  oswego |> dplyr::select(-tidyselect::all_of(vars_with_missingness)),
  oswego_imp
) |>
  # Preserve original column order
  dplyr::select(tidyselect::all_of(colnames(oswego)))

