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
  .matches <- rlang::dots_list(..., .named = TRUE)
  indicators <- purrr::map_dfc(.matches, get_match_indicator)
  dplyr::bind_cols(.df, indicators)
}


get_match_indicator <- function(.match) {
  UseMethod("get_match_indicator")
}

#' @exportS3Method
get_match_indicator.matchit <- function(.match) {
  .match$weights
}

get_match_indicator.default <- function(.match) {
  abort(paste("Objects of class", class(.match), "not supported"))
}
