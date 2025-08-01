% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_love.R
\name{geom_love}
\alias{geom_love}
\alias{love_plot}
\title{Create a Love plot}
\usage{
geom_love(
  data = NULL,
  linewidth = 0.8,
  line_size = NULL,
  point_size = 1.85,
  vline_xintercept = 0.1,
  vline_color = "grey70",
  vlinewidth = 0.6,
  vline_size = NULL
)

love_plot(
  .df,
  linewidth = 0.8,
  line_size = NULL,
  point_size = 1.85,
  vline_xintercept = 0.1,
  vline_color = "grey70",
  vlinewidth = 0.6,
  vline_size = NULL
)
}
\arguments{
\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{linewidth}{The line size, passed to \code{\link[ggplot2:geom_path]{ggplot2::geom_line()}}.}

\item{line_size}{Deprecated. Please use \code{linewidth}.}

\item{point_size}{The point size, passed to \code{\link[ggplot2:geom_point]{ggplot2::geom_point()}}.}

\item{vline_xintercept}{The X intercept, passed to \code{\link[ggplot2:geom_abline]{ggplot2::geom_vline()}}.}

\item{vline_color}{The vertical line color, passed to
\code{\link[ggplot2:geom_abline]{ggplot2::geom_vline()}}.}

\item{vlinewidth}{The vertical line size, passed to
\code{\link[ggplot2:geom_abline]{ggplot2::geom_vline()}}.}

\item{vline_size}{Deprecated. Please use \code{vlinewidth}.}

\item{.df}{a data frame produced by \code{tidy_smd()}}
}
\value{
a list of \code{geoms} or a \code{ggplot}
}
\description{
\code{geom_love()} and \code{love_plot()} are helper functions to create Love plots in
ggplot2. Love plots are a diagnostic approach to assessing balance before and
after weighting. Many researchers use 0.1 on the absolute SMD scale to
evaluate if a variable is well-balanced between groups, although this is just
a rule of thumb. \code{geom_love()} is a simple wrapper around
\code{\link[ggplot2:geom_point]{ggplot2::geom_point()}}, \code{\link[ggplot2:geom_path]{ggplot2::geom_line()}}, and
\code{\link[ggplot2:geom_abline]{ggplot2::geom_vline()}}. It also adds default aesthetics via
\code{\link[ggplot2:aes]{ggplot2::aes()}}. \code{love_plot()} is a quick plotting function that further
wraps \code{geom_love()}. For more complex Love plots, we recommend using ggplot2
directly.
}
\examples{
plot_df <- tidy_smd(
  nhefs_weights,
  race:active,
  .group = qsmk,
  .wts = starts_with("w_")
)

love_plot(plot_df)

# or use ggplot2 directly
library(ggplot2)
ggplot(
  plot_df,
  aes(
    x = abs(smd),
    y = variable,
    group = method,
    color = method,
    fill = method
  )
) +
  geom_love()

}
