#' Tidy Standardized Mean Differences
#'
#' `tidy_smd()` calculates the standardized mean difference (SMD) for variables
#' in a dataset between groups. Optionally, you may also calculate weighted
#' SMDs. `tidy_smd()` wraps `smd::smd()`, returning a tidy dataframe with the
#' columns `variable`, `method`, and `smd`, as well as fourth column the
#' contains the level of `.group` the SMD represents. You may also supply
#' multiple weights to calculate multiple weighted SMDs, useful when comparing
#' different types of weights. Additionally, the `.wts` argument supports
#' matched datasets where the variable supplied to `.wts` is an binary variable
#' indicating whether the row was included in the match. If you're using
#' MatchIt, the helper function [`bind_matches()`] will bind these indicators to
#' the original dataset,  making it easier to compare across matching
#' specifications.
#'
#' @param .df A data frame
#' @param .vars Variables for which to calculate SMD. Can be unquoted (`x`) or
#'   quoted (`"x"`).
#' @param .group Grouping variable. Can be unquoted (`x`) or quoted (`"x"`).
#' @param .wts Variables to use for weighting the SMD calculation. These can be,
#'   for instance, propensity score weights or a binary indicator signaling
#'   whether or not a participant was included in a matching algorithm. Can be
#'   unquoted (`x`) or quoted (`"x"`).
#' @param include_observed Logical. If using `.wts`, also calculate the
#'   unweighted SMD?
#' @param include_unweighted Deprecated. Please use `include_observed`.
#' @param make_dummy_vars Logical. Transform categorical variables to dummy
#'   variables using `model.matrix()`? By default, [smd::smd] uses a summary
#'   value based on the Mahalanobis distance distance to approximate the SMD of
#'   categorical variables. An alternative approach is to transform categorical
#'   variables to a set of dummy variables.
#' @inheritParams smd::smd
#'
#' @return a tibble
#' @export
#'
#' @examples
#'
#' tidy_smd(nhefs_weights, c(age, education, race), .group = qsmk)
#' tidy_smd(nhefs_weights, c(age, education), .group = qsmk, std.error = TRUE)
#'
#' tidy_smd(
#'   nhefs_weights,
#'   c(age, race, education),
#'   .group = qsmk,
#'   .wts = c(w_ate, w_att, w_atm)
#' )
tidy_smd <- function(.df, .vars, .group, .wts = NULL, include_observed = TRUE, include_unweighted = NULL,
                     na.rm = FALSE, gref = 1L, std.error = FALSE,
                     make_dummy_vars = FALSE) {
  if (!is.null(include_unweighted)) {
    warn("`include_unweighted` is deprecated. Please use `include_observed` instead.")
    include_observed <- include_unweighted
  }

  # check_weights(.wt)
  .df <- dplyr::as_tibble(.df)
  .vars <- enquo(.vars)
  .group <- ensym(.group)
  .wts <- enquo(.wts)

  .df <- dplyr::select(.df, !!.vars, !!.group, !!.wts)

  if (isTRUE(make_dummy_vars)) {
    .df <- model_matrix(.df)
  }

  if (!isTRUE(include_observed) && quo_is_null(.wts)) {
    abort("Must specify `.wts` if `include_observed = FALSE`")
  }

  if (include_observed) {
    unwts <- tidy_observed_smd(.df, .group = !!.group, .wts = !!.wts, na.rm = na.rm, gref = gref, std.error = std.error)
  } else {
    unwts <- NULL
  }

  if (!quo_is_null(.wts)) {
    wts <- map_tidy_smd(.df, !!.group, !!.wts, na.rm = na.rm, gref = gref, std.error = std.error)
  } else {
    wts <- NULL
  }

  dplyr::bind_rows(unwts, wts)
}

tidy_observed_smd <- function(.df, .group, .wts, na.rm = FALSE, gref = 1L, std.error = FALSE) {
  .group <- ensym(.group)
  .wts <- enquo(.wts)

  # `summarize()` with multiple rows was  deprecated in dplyr 1.1.0
  if (packageVersion("dplyr") >= "1.1.0") {
    .df <- dplyr::reframe(
      .df,
      dplyr::across(
        c(-!!.group, -!!.wts),
        ~ smd::smd(.x, !!.group, na.rm = na.rm, gref = gref, std.error = std.error)
      )
    )
  } else {
    .df <- dplyr::summarize(
      .df,
      dplyr::across(
        c(-!!.group, -!!.wts),
        ~ smd::smd(.x, !!.group, na.rm = na.rm, gref = gref, std.error = std.error)
      )
    )
  }

  .df <- pivot_smd(.df, "observed")

  dplyr::rename(.df, {{ .group }} := term)
}

map_tidy_smd <- function(.df, .group, .wts, na.rm = FALSE, gref = 1L, std.error = FALSE) {
  .group <- ensym(.group)
  wt_cols <- enquo(.wts)
  wt_vars <- names(tidyselect::eval_select(wt_cols, .df))

  purrr::map_dfr(wt_vars, ~ tidy_weighted_smd(.df, !!.group, .wts = .x, .all_wts= !!wt_cols, na.rm = na.rm, gref = gref, std.error = std.error))
}


tidy_weighted_smd <- function(.df, .group, .wts, .all_wts, na.rm = FALSE, gref = 1L, std.error = FALSE) {
  .group <- ensym(.group)
  .all_wts <- enquo(.all_wts)
  force(.wts)
  .wts_sym <- ensym(.wts)

  # `summarize()` with multiple rows was  deprecated in dplyr 1.1.0
  if (packageVersion("dplyr") >= "1.1.0") {
    .df <- dplyr::reframe(
      .df,
      dplyr::across(
        c(-!!.group, -!!.all_wts),
        ~ smd::smd(.x, !!.group, w = as.double(!!.wts_sym), na.rm = na.rm, gref = gref, std.error = std.error)
      )
    )
  } else {
    .df <- dplyr::summarise(
      .df,
      dplyr::across(
        c(-!!.group, -!!.all_wts),
        ~ smd::smd(.x, !!.group, w = as.double(!!.wts_sym), na.rm = na.rm, gref = gref, std.error = std.error)
      )
    )
  }

  .df <- pivot_smd(.df, .wts)

  dplyr::rename(.df, {{ .group }} := term)
}

#' @export
as.double.psw <- function(x, ...) {
  vctrs::vec_data(x)
}

pivot_smd <- function(.df, weights_col) {
  .df <- tidyr::pivot_longer(
    .df,
    dplyr::everything(),
    values_to = "smd_results",
    names_to = "variable"
  )

  .df <- tidyr::unpack(.df, smd_results)

  .df <- dplyr::mutate(.df, method = weights_col)

  dplyr::select(.df, variable, method, term, smd = estimate, dplyr::any_of("std.error"))
}

model_matrix <- function(.df) {
  mod_matrix <- dplyr::as_tibble(model.matrix(
    ~ .,
    .df
  ))

  dplyr::select(mod_matrix, -`(Intercept)`)
}
