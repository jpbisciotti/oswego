role_id <- c("person_id")

role_dependent <- c("ill")

role_independent_numeric <- c(
  "age", 
  "exposure_value", 
  "onset_value", 
  "incubation_period"
)

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
  "fruit_salad", 
  "exposure_missing"
)

role_independent_nominal_control <- c(
  "sex"
)

role_independent_nominal <- c(
  role_independent_nominal_predictor,
  role_independent_nominal_control
)
