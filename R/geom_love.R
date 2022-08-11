#' Create a Love plot
#'
#' `geom_love()` and `love_plot()` are helper functions to create Love plots in
#' ggplot2. Love plots are a diagnostic approach to assessing balance before and
#' after weighting. Many researchers use 0.1 on the absolute SMD scale to
#' evaluate if a variable is well-balanced between groups, although this is just
#' a rule of thumb. `geom_love()` is a simple wrapper around
#' [`ggplot2::geom_point()`], [`ggplot2::geom_line()`], and
#' [`ggplot2::geom_vline()`]. It also adds default aesthetics via
#' [`ggplot2::aes()`]. `love_plot()` is a quick plotting function that further
#' wraps `geom_love()`. For more complex Love plots, we recommend using ggplot2
#' directly.
#'
#' @param line_size The line size, passed to [`ggplot2::geom_line()`].
#' @param point_size The point size, passed to [`ggplot2::geom_point()`].
#' @param vline_xintercept The X intercept, passed to [`ggplot2::geom_vline()`].
#' @param vline_color The vertical line color, passed to
#'   [`ggplot2::geom_vline()`].
#' @param vline_size The vertical line size, passed to
#'   [`ggplot2::geom_vline()`].
#'
#' @return a list of `geoms` or a `ggplot`
#' @export
#'
#' @examples
#' plot_df <- tidy_smd(
#'   nhefs_weights,
#'   race:active,
#'   .group = qsmk,
#'   .wts = starts_with("w_")
#' )
#'
#' love_plot(plot_df)
#'
#' # or use ggplot2 directly
#' library(ggplot2)
#' ggplot(
#'   plot_df,
#'   aes(
#'     x = abs(smd),
#'     y = variable,
#'     group = weights,
#'     color = weights,
#'     fill = weights
#'   )
#' ) +
#'   geom_love()
#'
geom_love <- function(line_size = .8, point_size = 1.85, vline_xintercept = 0.1, vline_color = "grey70", vline_size = 0.6) {
  check_installed("ggplot2")

  list(
    ggplot2::geom_vline(
      xintercept = vline_xintercept,
      color = vline_color,
      size = vline_size
    ),
    ggplot2::geom_line(orientation = "y", size = line_size),
    ggplot2::geom_point(size = point_size)
  )
}

#' @export
#' @rdname geom_love
love_plot <- function(.df, line_size = .8, point_size = 1.85, vline_xintercept = 0.1, vline_color = "grey70", vline_size = 0.6) {
  check_installed("ggplot2")

  ggplot2::ggplot(
    .df,
    ggplot2::aes(
      x = abs(.data$smd),
      y = .data$variable,
      group = .data$weights,
      color = .data$weights,
      fill = .data$weights
    )
  ) +
    geom_love(
      line_size = line_size,
      point_size = point_size,
      vline_xintercept = vline_xintercept,
      vline_color = vline_color,
      vline_size = vline_size
    )
}