expect_doppelganger <- function(...) {
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(...)
}

test_that("multiplication works", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)
  .smds <- tidy_smd(
    nhefs_weights,
    c(age, race, education),
    .group = qsmk,
    .wts = c(w_ate, w_att, w_ato)
  )

  p1 <- ggplot(
    .smds,
    aes(
      x = abs(.data$smd),
      y = .data$variable,
      group = .data$method,
      color = .data$method,
      fill = .data$method
    )
  ) +
    geom_love()

  p2 <- love_plot(.smds)

  expect_doppelganger(
    title = "geom_love() works",
    fig = p1
  )

  expect_doppelganger(
    title = "love_plot() works",
    fig = p2
  )
})
