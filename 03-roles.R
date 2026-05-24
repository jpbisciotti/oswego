role_id <- c("person_id")

role_dependent <- c("ill")

role_independent_numeric <- c("age")

role_independent_nominal_control <- c("sex")

not_independent_nominal_predictors <- c(
  role_id, 
  role_dependent, 
  role_independent_numeric, 
  role_independent_nominal_control
)

if (FALSE) {
  # Identify independent nominal predictors. Copy console output and paste into
  # source code. We prefer to define column roles explicitly, rather than just
  # using the result of setdiff.
  oswego |>
    colnames() |>
    setdiff(not_independent_nominal_predictors) |>
    dput()
}

role_independent_nominal_predictor <- c(
  "baked_ham", 
  "spinach", 
  "mashed_potato", 
  "cabbage_salad", 
  "jello", 
  "rolls", 
  "brown_bread", 
  "milk", 
  "coffee", 
  "water", 
  "cakes", 
  "vanilla_ice_cream", 
  "chocolate_ice_cream", 
  "fruit_salad"
)

role_independent_nominal <- c(
  role_independent_nominal_predictor,
  role_independent_nominal_control
)

rm(not_independent_nominal_predictors)
