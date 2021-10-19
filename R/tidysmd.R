#' Tidy Standardized Mean Differences
#'
#' `tidy_smd()` calculates the standardized mean difference (SMD) for variables
#' in a dataset between groups. Optionally, you may also calculate weighted
#' SMDs. `tidy_smd()` wraps `smd::smd()`, returning a tidy dataframe with the
#' columns `variable`, `weights`, and `smd`. You may also supply multiple
#' weights to calculate multiple weighted SMDs, useful when comparing different
#' types of weights.
#'
#' @param .df A data frame
#' @param .vars Variables for which to calculate SMD
#' @param .group Grouping variable
#' @param .wts Variables to use for weighting the SMD calculation
#' @param include_unweighted Logical. If using `.wts`, also calculate the
#'   unweighted SMD?
#' @inheritParams smd::smd
#'
#' @return a tibble
#' @export
#'
#' @examples
#'
#' tidy_smd(nhefs_weights, c(age, education, race), .group = qsmk)
#'
#' tidy_smd(
#'   nhefs_weights,
#'   c(age, race, education),
#'   .group = qsmk,
#'   .wts = c(w_ate, w_att, w_atm)
#' )
tidy_smd <- function(.df, .vars, .group, .wts = NULL, include_unweighted = TRUE, na.rm = FALSE, gref = 1L) {
  # check_weights(.wt)
  .vars <- enquo(.vars)
  .group <- enquo(.group)
  .wts <- enquo(.wts)

  if (!isTRUE(include_unweighted) && quo_is_null(.wts)) {
    abort("Must specify `.wts` if `include_unweighted = FALSE`")
  }

  if (include_unweighted) {
    unwts <- tidy_unweighted_smd(.df, !!.vars, !!.group, na.rm = na.rm, gref = gref)
  } else {
    unwts <- NULL
  }

  if (!quo_is_null(.wts)) {
    wts <- map_tidy_smd(.df, !!.vars, !!.group, !!.wts, na.rm = na.rm, gref = gref)
  } else {
    wts <- NULL
  }

  dplyr::bind_rows(unwts, wts)
}

tidy_unweighted_smd <- function(.df, .vars, .group, na.rm = FALSE, gref = 1L) {
  .vars <- enquo(.vars)
  .group <- enquo(.group)

  .df <- dplyr::summarise(
    .df,
    dplyr::across(
      !!.vars,
      ~ smd::smd(.x, !!.group, na.rm = na.rm, gref = gref)$estimate
    )
  )

  pivot_smd(.df, "unweighted")
}

map_tidy_smd <- function(.df, .vars, .group, .wts, na.rm = FALSE, gref = 1L) {
  .vars <- enquo(.vars)
  .group <- enquo(.group)
  wt_cols <- enquo(.wts)
  wt_vars <- names(tidyselect::eval_select(wt_cols, .df))

  purrr::map_dfr(wt_vars, ~ tidy_weighted_smd(.df, !!.vars, !!.group, .wts = .x))
}


tidy_weighted_smd <- function(.df, .vars, .group, .wts, na.rm = FALSE, gref = 1L) {
  .vars <- enquo(.vars)
  .group <- enquo(.group)
  force(.wts)
  .wts_sym <- ensym(.wts)

  .df <- dplyr::summarise(
    .df,
    dplyr::across(
      !!.vars,
      ~ smd::smd(.x, !!.group, w = !!.wts_sym, na.rm = na.rm, gref = gref)$estimate
    )
  )

  pivot_smd(.df, .wts)
}

pivot_smd <- function(.df, weights_col) {
  .df <- tidyr::pivot_longer(
    .df,
    dplyr::everything(),
    values_to = "smd",
    names_to = "variable"
  )

  .df <- dplyr::mutate(.df, weights = weights_col)
  dplyr::select(.df, variable, weights, smd)
}
