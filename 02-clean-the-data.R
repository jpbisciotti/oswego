# Data Structure
oswego <- oswego |>
  janitor::clean_names() |>
  tibble::as_tibble()

# Exposure
oswego <- oswego |>
  dplyr::mutate(mt_month = 4L) |>
  dplyr::mutate(mt_day = 18L) |> 
  dplyr::mutate(mt_pm = as.integer(stringr::str_detect(meal_time, "PM"))) |>
  dplyr::mutate(mt_hour = as.integer(stringr::str_extract(meal_time, "[0-9]+(?=:)"))) |>
  dplyr::mutate(mt_hour24 = (mt_hour %% 12L) + mt_pm * 12L) |>
  dplyr::mutate(mt_minute = as.integer(stringr::str_extract(meal_time, "(?<=:)[0-9]+"))) |>
  dplyr::mutate(mt_datetime = lubridate::make_datetime(year = 1940, mt_month, mt_day, mt_hour24, mt_minute)) |>
  dplyr::mutate(exposure_value = as.integer((mt_datetime - min(mt_datetime, na.rm = TRUE))) / (60L*60L)) 

# Onset 
oswego <- oswego |>
  dplyr::mutate(onset_month = as.integer(stringr::str_extract(onset_date, "[0-9]+(?=\\/)"))) |>
  dplyr::mutate(onset_day = as.integer(stringr::str_extract(onset_date, "(?<=\\/)[0-9]+"))) |> 
  dplyr::mutate(onset_pm = as.integer(stringr::str_detect(onset_time, "PM"))) |>
  dplyr::mutate(onset_hour = as.integer(stringr::str_extract(onset_time, "[0-9]+(?=:)"))) |>
  dplyr::mutate(onset_hour24 = (onset_hour %% 12L) + onset_pm * 12L) |>
  dplyr::mutate(onset_minute = as.integer(stringr::str_extract(onset_time, "(?<=:)[0-9]+"))) |> 
  dplyr::mutate(onset_datetime = lubridate::make_datetime(year = 1940, onset_month, onset_day, onset_hour24, onset_minute)) |>
  dplyr::mutate(onset_value = as.integer((onset_datetime - min(onset_datetime, na.rm = TRUE))) / (60L*60L))

# Incubation
oswego <- oswego |>
  dplyr::mutate(incubation_datetime = onset_datetime - mt_datetime) |>
  dplyr::mutate(incubation_period = as.integer(incubation_datetime)) 

if (FALSE) {
  # Missingness
  # exposure_value: missing if without a meal_time, complete if with a meal_time 
  # onset_value: missing if well, complete if ill 
  # incubation_period: missing if exposure or onset is missing, complete if both exposure and onset are complete
  oswego |>
    dplyr::mutate(exposure_missing = ifelse(is.na(exposure_value), "Y", "N")) |>
    dplyr::mutate(onset_missing = ifelse(is.na(onset_value), "Y", "N")) |>
    dplyr::mutate(incubation_missing = ifelse(is.na(incubation_period), "Y", "N")) 
}

# ID Role: use a hash so the original values are not mistaken for meaningfully numeric
oswego <- oswego |>
  dplyr::mutate(person_id = paste0(substr(openssl::md5(as.character(id)), 1, 8)))

# Drop
oswego <- oswego |> 
  dplyr::select(-mt_month, -mt_day, -mt_pm, -mt_hour, -mt_hour24, -mt_minute, -meal_time) |> 
  dplyr::select(-onset_month, -onset_day, -onset_pm, -onset_hour, -onset_hour24, -onset_minute, -onset_date, -onset_time) |>
  dplyr::select(-mt_datetime, -onset_datetime, -incubation_datetime) |>
  dplyr::select(-id) |> 
  dplyr::select(-exposure_value, -onset_value, -incubation_period)
