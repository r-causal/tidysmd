test_that("bind_matches() adds the correct columns", {
  skip_if_not_installed("MatchIt")
  set.seed(100)
  df <- data.frame(
    x = sample(c(0, 1), 100, replace = TRUE),
    z = rnorm(100)
  )

  # Create two matchit objects
  library(MatchIt)
  suppressWarnings(m1 <- matchit(x ~ z, data = df))
  suppressWarnings(m2 <- matchit(x ~ z, data = df, caliper = 0.5))

  # Bind the matchit objects to the data frame
  df_bind <- bind_matches(df, m1, caliper = m2)

  # Assert that the number of columns in the output is correct
  expect_equal(ncol(df_bind), ncol(df) + length(list(m1, m2)))
  expect_named(df_bind, c("x", "z", "m1", "caliper"))
})
