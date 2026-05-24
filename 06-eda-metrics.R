metrics_desc <- c("tpr", "tnr", "ppv", "npv", "youden_j", "bal_acc", "lr_pos", "dor", "markedness", "mcc", "f1") 

metrics_asc <- c("fnr", "fpr", "fdr", "for_rate", "lr_neg")

eda_metrics <- oswego |>
  dplyr::select(ill, dplyr::all_of(role_independent_nominal_predictor)) |>
  tidyr::pivot_longer(-ill, names_to = "name", values_to = "exposed") |>
  dplyr::filter(!is.na(exposed)) |>
  dplyr::summarise(
    ill_Y = sum(ill == "Y"),
    ill_N = sum(ill == "N"),
    pred_Y = sum(exposed == "Y"),
    pred_N = sum(exposed == "N"),
    tp = sum(ill == "Y" & exposed == "Y"),
    fp = sum(ill == "N" & exposed == "Y"),
    fn = sum(ill == "Y" & exposed == "N"),
    tn = sum(ill == "N" & exposed == "N"),
    .by = name
  ) |>
  dplyr::mutate(
    tpr        = tp / pmax(tp + fn, 1L),
    tnr        = tn / pmax(tn + fp, 1L),
    fnr        = 1 - tpr,
    fpr        = 1 - tnr,
    ppv        = tp / pmax(tp + fp, 1L),
    npv        = tn / pmax(tn + fn, 1L),
    fdr        = 1 - ppv,
    for_rate   = 1 - npv,
    youden_j   = tpr + tnr - 1,
    bal_acc    = (tpr + tnr) / 2,
    lr_pos     = dplyr::if_else(fpr > 0, tpr / fpr, Inf),
    lr_neg     = dplyr::if_else(tnr > 0, fnr / tnr, Inf),
    dor        = dplyr::if_else(fp * fn > 0, (tp * tn) / (fp * fn), Inf),
    markedness = ppv + npv - 1,
    mcc_den    = sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn)),
    mcc        = dplyr::if_else(mcc_den > 0, (tp * tn - fp * fn) / mcc_den, 0),
    f1         = dplyr::if_else(2 * tp + fp + fn > 0, 2 * tp / (2 * tp + fp + fn), 0),
    mcc_den    = NULL
  ) |>
  dplyr::arrange(dplyr::desc(tpr)) |>
  dplyr::mutate(dplyr::across(tidyselect::all_of(c(metrics_desc)), ~ dplyr::dense_rank(dplyr::desc(.x)), .names = "dr_{.col}"))|>
  dplyr::mutate(dplyr::across(tidyselect::all_of(c(metrics_asc)), ~ dplyr::dense_rank(.x), .names = "dr_{.col}")) |>
  dplyr::rename(predictor = name)

eda_metrics <- eda_metrics |>
  dplyr::bind_cols(
    dr_avg = eda_metrics |>
      dplyr::select(tidyselect::all_of(paste0("dr_", c(metrics_desc, metrics_asc)))) |>
      rowMeans() |>
      dplyr::dense_rank()
  )
