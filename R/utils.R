globalVariables(c(
  "variable",
  "weights",
  "smd",
  "estimate",
  "smd_results",
  "term",
  "method",
  "(Intercept)"
))

warn <- function(...) {
  rlang::warn(..., use_cli_format = TRUE, class = "tidysmd_warning")
}

abort <- function(...) {
  rlang::abort(..., use_cli_format = TRUE, class = "tidysmd_error")
}
