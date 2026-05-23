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

# Missingness
oswego <- oswego |>
  dplyr::mutate(exposure_missing = ifelse(is.na(exposure_value), "Y", "N")) 

if (FALSE) {
  # We don not create onset_missing or onset_missing because they are
  # analytically redundant. The onset_missing variable would map directly to
  # ill. The incubation_missing variable would depend on incubation_period,
  # which depends on ill.
  oswego <- oswego |>
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
  dplyr::select(-id)
  
