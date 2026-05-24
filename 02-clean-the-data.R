# Data structure
oswego <- oswego |>
  tibble::as_tibble()

# Column names
oswego <- oswego |>
  janitor::clean_names()

# ID Role: original values appear numeric but are meaningfully nominal. Use 
# a hash function to create a value that appears nominal to represent nominal.
# We use person_id rather than just id to distinguish the two. 
oswego <- oswego |>
  dplyr::mutate(person_id = paste0(substr(openssl::md5(as.character(id)), 1, 8))) |>
  dplyr::select(-id)

# Meal time and onset could be used as end points to calculate incubation 
# period. However, onset is available only if there was illness, onset is 
# NA when therw as not illness. This NA creates a missingness that affects 
# logistic regression, therefore, we drop these variables from the workflow. 
oswego <- oswego |>
  dplyr::select(-meal_time, -onset_date, -onset_time) 
