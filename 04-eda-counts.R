levels_dependent <- c("Y", "N")

levels_predictor <- c("Y", "N")

eda_analysis <- purrr::map(
  role_independent_nominal_predictor,
  ~ oswego |> 
    dplyr::count(
      !!rlang::sym(role_dependent), 
      !!rlang::sym(.x)
    ) |>
    dplyr::mutate(predictor = .x) |>
    dplyr::rename(predictor_level = !!rlang::sym(.x)) |>
    dplyr::mutate(dependent = role_dependent) |>
    dplyr::rename(dependent_level := !!rlang::sym(role_dependent)) |> 
    dplyr::select(predictor, predictor_level, dependent, dependent_level, n) |>
    #
    dplyr::rename(n_cell = n) |>
    dplyr::mutate(n_predictor = sum(n_cell), .by = predictor_level) |>
    dplyr::mutate(n_dependent = sum(n_cell), .by = dependent_level) |>
    #
    dplyr::mutate(n_cell_ok = all(n_cell[!is.na(predictor_level)] >= 5L)) |>
    dplyr::mutate(n_predictor_ok = all(n_predictor[!is.na(predictor_level)] >= 5L)) |>
    dplyr::mutate(n_dependent_ok = all(n_dependent[!is.na(predictor_level)] >= 10L)) |>
    #
    dplyr::mutate(all_dependent_levels = all(levels_dependent %in% dependent_level)) |>
    dplyr::mutate(all_predictor_levels = all(na.omit(levels_predictor %in% predictor_level))) |>
    dplyr::mutate(all_cell_levels = all_dependent_levels & all_predictor_levels) 
)

eda_not_all_cell_levels  <- eda_analysis |>
  purrr::keep(~ !all(dplyr::pull(.x, all_cell_levels))) |>
  purrr::map(
    ~ .x |> 
      dplyr::select(dependent, dependent_level, predictor, predictor_level, n_cell) |>
      janitor::adorn_totals() |>
      tibble::as_tibble()
  )

eda_n_dependent_not_ok <- eda_analysis |>
  purrr::keep(~ !all(dplyr::pull(.x, n_dependent_ok))) |>
  purrr::map(
    ~ .x |> 
      dplyr::select(dependent, dependent_level, n_dependent) |>
      dplyr::distinct() |>
      janitor::adorn_totals() |>
      tibble::as_tibble()
  )

eda_n_predictor_not_ok <- eda_analysis |>
  purrr::keep(~ !all(dplyr::pull(.x, n_predictor_ok))) |>
  purrr::map(
    ~ .x |> 
      dplyr::select(predictor, predictor_level, n_predictor) |>
      dplyr::distinct() |>
      janitor::adorn_totals() |>
      tibble::as_tibble()
  )

eda_n_cell_not_ok <- eda_analysis |>
  purrr::keep(~ !all(dplyr::pull(.x, n_cell_ok))) |> 
  purrr::map(
    ~ .x |> 
      dplyr::select(dependent, dependent_level, predictor, predictor_level, n_cell) |>
      janitor::adorn_totals() |>
      tibble::as_tibble()
  )
