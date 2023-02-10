#' Bind Match Indicator Columns to a Data Frame
#'
#' Given a data frame `.df`, the function `bind_matches` creates binary
#' indicator variables for each match returned by the `MatchIt` library and
#' binds the resulting columns to `.df`. In other words, the result is the
#' original data frame plus a column for however many matches you want to bind.
#'
#' @param .df A data frame.
#' @param ... `matchit` objects returned by the `MatchIt` package. They can be
#'   named or unnamed.
#'
#' @return `.df` with addition columns for every element of `...`
#' @export
bind_matches <- function(.df, ...) {
  rlang::check_installed("MatchIt")
  .matches <- rlang::dots_list(..., .named = TRUE)
  indicators <- purrr::map_dfc(.matches, get_match_indicator)
  dplyr::bind_cols(.df, indicators)
}

get_match_indicator <- function(.matchit) {
  assert_is_matchit(.matchit)
  .matchit$weights
}

assert_is_matchit <- function(.matchit) {
  if (!inherits(.matchit, "matchit")) {
    abort("Objects in `.matches` must be of class `matchit`.")
  }

  invisible(TRUE)
}
