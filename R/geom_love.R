#' Create a Love plot
#'
#' `geom_love()` is a helper function to create Love plots in ggplot2. Love
#' plots are a diagnostic approach to assessing balance before and after
#' weighting. Many researchers use 0.1 on the absolute SMD scale to evaluate if
#' a variable is well-balanced between groups, although this is just a rule of
#' thumb. `geom_love()` is a simple wrapper around [`ggplot2::geom_point()`],
#' [`ggplot2::geom_line()`], and [`ggplot2::geom_vline()`]. It also adds default
#' aesthetics via [`ggplot2::aes()`]. For more complex Love plots, we recommend
#' using ggplot2 directly.
#'
#' @param line_size The line size, passed to [`ggplot2::geom_line()`].
#' @param point_size The point size, passed to [`ggplot2::geom_point()`].
#' @param vline_xintercept The X intercept, passed to [`ggplot2::geom_vline()`].
#' @param vline_color The vertical line color, passed to
#'   [`ggplot2::geom_vline()`].
#' @param vline_size The vertical line size, passed to
#'   [`ggplot2::geom_vline()`].
#'
#' @return a list of `geoms`.
#' @export
#'
#' @examples
#' library(ggplot2)
#' plot_df <- tidy_smd(
#'   nhefs_weights,
#'   race:active,
#'   .group = qsmk,
#'   .wts = starts_with("w_")
#' )
#'
#' # no need for `aes()` by default.
#' ggplot(plot_df) + geom_love()
#'
geom_love <- function(line_size = .8, point_size = 1.85, vline_xintercept = 0.1, vline_color = "grey70", vline_size = 0.6) {
  check_installed("ggplot2")

  list(
    ggplot2::aes(
      x = abs(.data$smd),
      y = .data$variable,
      group = .data$weights,
      color = .data$weights,
      fill = .data$weights
    ),
    ggplot2::geom_vline(
      xintercept = vline_xintercept,
      color = vline_color,
      size = vline_size
    ),
    ggplot2::geom_line(orientation = "y", size = line_size),
    ggplot2::geom_point(size = point_size)
  )
}
