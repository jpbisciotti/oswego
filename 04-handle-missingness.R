# Choice of handlers ===========================================================

# 1/ Complete-case analysis. Drop 2/75 = 2.7 %. Defensible and simplest;
# attack-rate tables lose essentially nothing.
# 
# 2/ Mode imputation by stratum (ill × food), instant; for row 29 the stratum
# strongly votes "Y" (most cases ate chocolate ice cream); for row 53 the
# stratum is more even on mashed potato (split among non-ill).
#
# 3/ mice with method = "logreg" or "logreg.boot" restricted to the food block, m =
# 5–20, the pedagogically-aligned route. Will produce essentially identical
# point estimates to the above, which is itself the pedagogical point.
#
# 4/ Code NA as a third level ("U") and retain the row, defensible if you want to
# avoid imputing at all.

# Try 3 ========================================================================

# Multiple imputation of the two missing food-exposure cells in the cleaned
# Oswego dataset using `mice` with `method = "logreg.boot"`.

library(dplyr)
library(tidyr)
library(cli)
library(mice)

# `mice` infers an imputation model from each column's data type, so the binary
# food and outcome columns must be factors (NOT character). Two-level factors
# with NAs default to method = "logreg.boot", which is what we want.

vars_mi <- c(
  role_dependent,
  role_independent_nominal_predictor
)

oswego_mi <- oswego |>
  # IDs cannot serve as predictors
  select(-all_of(c(role_id))) |>
  mutate(sex = factor(sex, levels = c("F", "M"))) |>
  mutate(across(all_of(vars_mi), ~ factor(.x, levels = c("N", "Y"))))

# logreg vs logreg.boot: the default logreg draws regression coefficients from
# their asymptotic posterior. For small n where the asymptotics are more
# uncertain, logreg.boot bootstraps the data first, then fits the logistic
# regression, slightly more honest about model uncertainty.

vars_with_missingness <- c("mashed_potato", "chocolate_ice_cream")

meth <- make.method(oswego_mi)
meth[vars_with_missingness] <- "logreg.boot"

# Predictors: use all other columns. `make.predictorMatrix` produces the
# standard "everything-predicts-everything" matrix with diagonal = 0.

pred <- make.predictorMatrix(oswego_mi)

#  Run the imputations

imp <- mice(
  oswego_mi,
  method     = meth,
  predictorMatrix = pred,
  m          = 20,
  maxit      = 10,
  seed       = 2026,
  printFlag  = FALSE
)

cli::cli_h2("Imputed values for mashed_potato (row 53, an 11-yr-old non-ill M)")

imp$imp$mashed_potato |> print()
tibble::enframe(unname(t(imp$imp$mashed_potato))) |> 
  dplyr::count(value)

# 17 Y, 3 N

cli::cli_h2("Imputed values for chocolate_ice_cream (row 29, a 58-yr-old ill M)")
imp$imp$chocolate_ice_cream |> print()
tibble::enframe(unname(t(imp$imp$chocolate_ice_cream))) |> 
  dplyr::count(value)

# 14 Y, 3 5, 1 NA

# Pool a downstream analysis across imputations. Demonstrate Rubin's rules on a
# logistic regression of illness on a couple of food exposures. `with()` fits
# the same model on each completed dataset; `pool()` combines the estimates and
# adjusts SEs.

fit <- with(imp, glm(
  ill ~ vanilla_ice_cream + chocolate_ice_cream + mashed_potato,
  family = binomial
))

pooled <- pool(fit)

cli::cli_h2("Fraction of missing information (FMI) per coefficient")

pooled$pooled |>
  select(term, estimate, riv, fmi, lambda) |>
  print()

# FMI ≈ 3 % across coefficients, meaning the imputation adds ~3 % to total
# variance versus a complete-data analysis. m = 20 is plenty; m = 5 would be
# defensible. This is also the empirical evidence for this dataset MI is 
# methodologically valid but operationally near-equivalent to complete-case 
# analysis.

