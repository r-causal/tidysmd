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
#' @param linewidth The line size, passed to [`ggplot2::geom_line()`].
#' @param line_size Deprecated. Please use `linewidth`.
#' @param point_size The point size, passed to [`ggplot2::geom_point()`].
#' @param vline_xintercept The X intercept, passed to [`ggplot2::geom_vline()`].
#' @param vline_color The vertical line color, passed to
#'   [`ggplot2::geom_vline()`].
#' @param vlinewidth The vertical line size, passed to
#'   [`ggplot2::geom_vline()`].
#' @param vline_size Deprecated. Please use `vlinewidth`.
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
#'     group = method,
#'     color = method,
#'     fill = method
#'   )
#' ) +
#'   geom_love()
#'
geom_love <- function(
  linewidth = .8,
  line_size = NULL,
  point_size = 1.85,
  vline_xintercept = 0.1,
  vline_color = "grey70",
  vlinewidth = 0.6,
  vline_size = NULL
) {
  check_installed("ggplot2")
  if (!is.null(line_size)) {
    warn("`line_size` is deprecated. Please use `linewidth`")
    linewidth <- line_size
  }

  if (!is.null(vline_size)) {
    warn("`vline_size` is deprecated. Please use `vlinewidth`")
    vlinewidth <- vline_size
  }

  # `size` was deprecated for `geom_line()` in ggplot2 3.4.0
  if (packageVersion("ggplot2") >= "3.4.0") {
    vline_geom <- ggplot2::geom_vline(
      xintercept = vline_xintercept,
      color = vline_color,
      linewidth = vlinewidth
    )
    line_geom <- ggplot2::geom_line(orientation = "y", linewidth = linewidth)
  } else {
    vline_geom <- ggplot2::geom_vline(
      xintercept = vline_xintercept,
      color = vline_color,
      size = vlinewidth
    )
    line_geom <- ggplot2::geom_line(orientation = "y", size = linewidth)
  }

  list(
    vline_geom,
    line_geom,
    ggplot2::geom_point(size = point_size)
  )
}

#' @export
#' @rdname geom_love
#' @param .df a data frame produced by `tidy_smd()`
love_plot <- function(
  .df,
  linewidth = .8,
  line_size = NULL,
  point_size = 1.85,
  vline_xintercept = 0.1,
  vline_color = "grey70",
  vlinewidth = 0.6,
  vline_size = NULL
) {
  check_installed("ggplot2")
  if (!is.null(line_size)) {
    warn("`line_size` is deprecated. Please use `linewidth`")
    linewidth <- line_size
  }

  if (!is.null(vline_size)) {
    warn("`vline_size` is deprecated. Please use `vlinewidth`")
    vlinewidth <- vline_size
  }

  ggplot2::ggplot(
    .df,
    ggplot2::aes(
      x = abs(.data$smd),
      y = .data$variable,
      group = .data$method,
      color = .data$method,
      fill = .data$method
    )
  ) +
    geom_love(
      linewidth = linewidth,
      point_size = point_size,
      vline_xintercept = vline_xintercept,
      vline_color = vline_color,
      vlinewidth = vlinewidth
    )
}
