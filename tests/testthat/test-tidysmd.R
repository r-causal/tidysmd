expect_tidy_smd_tbl <- function(.smds, .rows, .cols = 4, .group = "qsmk") {
  expect_s3_class(.smds, c("tbl_df", "tbl", "data.frame"))
  expect_length(.smds, .cols)
  expect_equal(nrow(.smds), .rows)
  expect_named(.smds, c("variable", "method", .group, "smd"))
}

pull_smd <- function(.smds, .v, .w = "observed") {
  .smds <- dplyr::filter(.smds, variable == .v, method == .w)
  .smds[["smd"]]
}

pull_term <- function(.smds, .v, .term = "cyl", .w = "observed") {
  .smds <- dplyr::filter(.smds, variable == .v, method == .w)
  .smds[[.term]]
}

pull_std.error <- function(.smds, .v, .w = "observed") {
  .smds <- dplyr::filter(.smds, variable == .v, method == .w)
  .smds[["std.error"]]
}

test_that("tidy_smd() works without weights", {
  expect_error(
    tidy_smd(nhefs_weights, age, .group = qsmk, include_observed = FALSE),
    "Must specify `.wts` if `include_observed = FALSE`"
  )

  .smds <- tidy_smd(nhefs_weights, c(age, education, race), .group = qsmk)
  expect_tidy_smd_tbl(.smds, .rows = 3)

  expect_equal(
    pull_smd(.smds, "age"),
    smd::smd(nhefs_weights$age, nhefs_weights$qsmk)$estimate
  )

  expect_equal(
    pull_smd(.smds, "education"),
    smd::smd(nhefs_weights$education, nhefs_weights$qsmk)$estimate
  )

  expect_equal(
    pull_smd(.smds, "race"),
    smd::smd(nhefs_weights$race, nhefs_weights$qsmk)$estimate
  )
})

test_that("tidy_smd() works with weights", {
  .smds <- tidy_smd(
    nhefs_weights,
    c(age, race, education),
    .group = qsmk,
    .wts = w_ate
  )
  expect_tidy_smd_tbl(.smds, .rows = 6)

  expect_equal(
    pull_smd(.smds, "age"),
    smd::smd(nhefs_weights$age, nhefs_weights$qsmk)$estimate
  )

  expect_equal(
    pull_smd(.smds, "education"),
    smd::smd(nhefs_weights$education, nhefs_weights$qsmk)$estimate
  )

  expect_equal(
    pull_smd(.smds, "race"),
    smd::smd(nhefs_weights$race, nhefs_weights$qsmk)$estimate
  )

  wts <- as.double(nhefs_weights$w_ate)

  expect_equal(
    pull_smd(.smds, "age", "w_ate"),
    smd::smd(nhefs_weights$age, nhefs_weights$qsmk, wts)$estimate
  )

  expect_equal(
    pull_smd(.smds, "race", "w_ate"),
    smd::smd(nhefs_weights$race, nhefs_weights$qsmk, wts)$estimate
  )

  expect_equal(
    pull_smd(.smds, "education", "w_ate"),
    smd::smd(nhefs_weights$education, nhefs_weights$qsmk, wts)$estimate
  )
})

test_that("tidy_smd() works with weights and no observed", {
  .smds <- tidy_smd(
    nhefs_weights,
    c(age, race, education),
    .group = qsmk,
    .wts = w_ate,
    include_observed = FALSE
  )

  expect_tidy_smd_tbl(.smds, .rows = 3)

  wts <- as.double(nhefs_weights$w_ate)

  expect_equal(
    pull_smd(.smds, "age", "w_ate"),
    smd::smd(nhefs_weights$age, nhefs_weights$qsmk, wts)$estimate
  )

  expect_equal(
    pull_smd(.smds, "race", "w_ate"),
    smd::smd(nhefs_weights$race, nhefs_weights$qsmk, wts)$estimate
  )

  expect_equal(
    pull_smd(.smds, "education", "w_ate"),
    smd::smd(nhefs_weights$education, nhefs_weights$qsmk, wts)$estimate
  )
})

test_that("tidy_smd() works with many weights", {
  .smds <- tidy_smd(
    nhefs_weights,
    c(age, race, education),
    .group = qsmk,
    .wts = c(w_ate, w_att, w_atm)
  )

  expect_tidy_smd_tbl(.smds, .rows = 12)

  expect_equal(
    pull_smd(.smds, "age"),
    smd::smd(nhefs_weights$age, nhefs_weights$qsmk)$estimate
  )

  expect_equal(
    pull_smd(.smds, "education"),
    smd::smd(nhefs_weights$education, nhefs_weights$qsmk)$estimate
  )

  expect_equal(
    pull_smd(.smds, "race"),
    smd::smd(nhefs_weights$race, nhefs_weights$qsmk)$estimate
  )

  expect_equal(
    pull_smd(.smds, "age", "w_ate"),
    smd::smd(
      nhefs_weights$age,
      nhefs_weights$qsmk,
      nhefs_weights$w_ate |> as.double()
    )$estimate
  )

  expect_equal(
    pull_smd(.smds, "race", "w_ate"),
    smd::smd(
      nhefs_weights$race,
      nhefs_weights$qsmk,
      nhefs_weights$w_ate |> as.double()
    )$estimate
  )

  expect_equal(
    pull_smd(.smds, "education", "w_ate"),
    smd::smd(
      nhefs_weights$education,
      nhefs_weights$qsmk,
      nhefs_weights$w_ate |> as.double()
    )$estimate
  )

  expect_equal(
    pull_smd(.smds, "age", "w_att"),
    smd::smd(
      nhefs_weights$age,
      nhefs_weights$qsmk,
      nhefs_weights$w_att |> as.double()
    )$estimate
  )

  expect_equal(
    pull_smd(.smds, "race", "w_att"),
    smd::smd(
      nhefs_weights$race,
      nhefs_weights$qsmk,
      nhefs_weights$w_att |> as.double()
    )$estimate
  )

  expect_equal(
    pull_smd(.smds, "education", "w_att"),
    smd::smd(
      nhefs_weights$education,
      nhefs_weights$qsmk,
      nhefs_weights$w_att |> as.double()
    )$estimate
  )

  expect_equal(
    pull_smd(.smds, "age", "w_atm"),
    smd::smd(
      nhefs_weights$age,
      nhefs_weights$qsmk,
      nhefs_weights$w_atm |> as.double()
    )$estimate
  )

  expect_equal(
    pull_smd(.smds, "race", "w_atm"),
    smd::smd(
      nhefs_weights$race,
      nhefs_weights$qsmk,
      nhefs_weights$w_atm |> as.double()
    )$estimate
  )

  expect_equal(
    pull_smd(.smds, "education", "w_atm"),
    smd::smd(
      nhefs_weights$education,
      nhefs_weights$qsmk,
      nhefs_weights$w_atm |> as.double()
    )$estimate
  )
})

test_that("tidy_smd() works with tidyselect", {
  .smds <- tidy_smd(
    nhefs_weights,
    c(age, race, education),
    .group = qsmk,
    .wts = starts_with("w_")
  )

  expect_tidy_smd_tbl(.smds, .rows = 18)

  expect_equal(
    pull_smd(.smds, "age"),
    smd::smd(nhefs_weights$age, nhefs_weights$qsmk)$estimate
  )

  expect_equal(
    pull_smd(.smds, "education"),
    smd::smd(nhefs_weights$education, nhefs_weights$qsmk)$estimate
  )

  expect_equal(
    pull_smd(.smds, "race"),
    smd::smd(nhefs_weights$race, nhefs_weights$qsmk)$estimate
  )

  expect_equal(
    pull_smd(.smds, "age", "w_ate"),
    smd::smd(
      nhefs_weights$age,
      nhefs_weights$qsmk,
      nhefs_weights$w_ate |> as.double()
    )$estimate
  )

  expect_equal(
    pull_smd(.smds, "race", "w_ate"),
    smd::smd(
      nhefs_weights$race,
      nhefs_weights$qsmk,
      nhefs_weights$w_ate |> as.double()
    )$estimate
  )

  expect_equal(
    pull_smd(.smds, "education", "w_ate"),
    smd::smd(
      nhefs_weights$education,
      nhefs_weights$qsmk,
      nhefs_weights$w_ate |> as.double()
    )$estimate
  )

  expect_equal(
    pull_smd(.smds, "age", "w_att"),
    smd::smd(
      nhefs_weights$age,
      nhefs_weights$qsmk,
      nhefs_weights$w_att |> as.double()
    )$estimate
  )

  expect_equal(
    pull_smd(.smds, "race", "w_att"),
    smd::smd(
      nhefs_weights$race,
      nhefs_weights$qsmk,
      nhefs_weights$w_att |> as.double()
    )$estimate
  )

  expect_equal(
    pull_smd(.smds, "education", "w_att"),
    smd::smd(
      nhefs_weights$education,
      nhefs_weights$qsmk,
      nhefs_weights$w_att |> as.double()
    )$estimate
  )

  expect_equal(
    pull_smd(.smds, "age", "w_atm"),
    smd::smd(
      nhefs_weights$age,
      nhefs_weights$qsmk,
      nhefs_weights$w_atm |> as.double()
    )$estimate
  )

  expect_equal(
    pull_smd(.smds, "race", "w_atm"),
    smd::smd(
      nhefs_weights$race,
      nhefs_weights$qsmk,
      nhefs_weights$w_atm |> as.double()
    )$estimate
  )

  expect_equal(
    pull_smd(.smds, "education", "w_atm"),
    smd::smd(
      nhefs_weights$education,
      nhefs_weights$qsmk,
      nhefs_weights$w_atm |> as.double()
    )$estimate
  )

  expect_equal(
    pull_smd(.smds, "age", "w_atc"),
    smd::smd(
      nhefs_weights$age,
      nhefs_weights$qsmk,
      nhefs_weights$w_atc |> as.double()
    )$estimate
  )

  expect_equal(
    pull_smd(.smds, "race", "w_atc"),
    smd::smd(
      nhefs_weights$race,
      nhefs_weights$qsmk,
      nhefs_weights$w_atc |> as.double()
    )$estimate
  )

  expect_equal(
    pull_smd(.smds, "education", "w_atc"),
    smd::smd(
      nhefs_weights$education,
      nhefs_weights$qsmk,
      nhefs_weights$w_atc |> as.double()
    )$estimate
  )

  expect_equal(
    pull_smd(.smds, "age", "w_ato"),
    smd::smd(
      nhefs_weights$age,
      nhefs_weights$qsmk,
      nhefs_weights$w_ato |> as.double()
    )$estimate
  )

  expect_equal(
    pull_smd(.smds, "race", "w_ato"),
    smd::smd(
      nhefs_weights$race,
      nhefs_weights$qsmk,
      nhefs_weights$w_ato |> as.double()
    )$estimate
  )

  expect_equal(
    pull_smd(.smds, "education", "w_ato"),
    smd::smd(
      nhefs_weights$education,
      nhefs_weights$qsmk,
      nhefs_weights$w_ato |> as.double()
    )$estimate
  )
})

test_that("standard errors return correctly", {
  .smds <- tidy_smd(
    nhefs_weights,
    c(age, race, education),
    .group = qsmk,
    .wts = c(w_ate, w_att, w_atm),
    std.error = TRUE
  )

  expect_length(.smds, 5)

  expect_equal(
    pull_std.error(.smds, "age"),
    smd::smd(nhefs_weights$age, nhefs_weights$qsmk, std.error = TRUE)$std.error
  )

  expect_equal(
    pull_std.error(.smds, "education"),
    smd::smd(
      nhefs_weights$education,
      nhefs_weights$qsmk,
      std.error = TRUE
    )$std.error
  )

  expect_equal(
    pull_std.error(.smds, "race"),
    smd::smd(nhefs_weights$race, nhefs_weights$qsmk, std.error = TRUE)$std.error
  )

  expect_equal(
    pull_std.error(.smds, "age", "w_ate"),
    smd::smd(
      nhefs_weights$age,
      nhefs_weights$qsmk,
      nhefs_weights$w_ate |> as.double(),
      std.error = TRUE
    )$std.error
  )

  expect_equal(
    pull_std.error(.smds, "race", "w_ate"),
    smd::smd(
      nhefs_weights$race,
      nhefs_weights$qsmk,
      nhefs_weights$w_ate |> as.double(),
      std.error = TRUE
    )$std.error
  )

  expect_equal(
    pull_std.error(.smds, "education", "w_ate"),
    smd::smd(
      nhefs_weights$education,
      nhefs_weights$qsmk,
      nhefs_weights$w_ate |> as.double(),
      std.error = TRUE
    )$std.error
  )

  expect_equal(
    pull_std.error(.smds, "age", "w_att"),
    smd::smd(
      nhefs_weights$age,
      nhefs_weights$qsmk,
      nhefs_weights$w_att |> as.double(),
      std.error = TRUE
    )$std.error
  )

  expect_equal(
    pull_std.error(.smds, "race", "w_att"),
    smd::smd(
      nhefs_weights$race,
      nhefs_weights$qsmk,
      nhefs_weights$w_att |> as.double(),
      std.error = TRUE
    )$std.error
  )

  expect_equal(
    pull_std.error(.smds, "education", "w_att"),
    smd::smd(
      nhefs_weights$education,
      nhefs_weights$qsmk,
      nhefs_weights$w_att |> as.double(),
      std.error = TRUE
    )$std.error
  )

  expect_equal(
    pull_std.error(.smds, "age", "w_atm"),
    smd::smd(
      nhefs_weights$age,
      nhefs_weights$qsmk,
      nhefs_weights$w_atm |> as.double(),
      std.error = TRUE
    )$std.error
  )

  expect_equal(
    pull_std.error(.smds, "race", "w_atm"),
    smd::smd(
      nhefs_weights$race,
      nhefs_weights$qsmk,
      nhefs_weights$w_atm |> as.double(),
      std.error = TRUE
    )$std.error
  )

  expect_equal(
    pull_std.error(.smds, "education", "w_atm"),
    smd::smd(
      nhefs_weights$education,
      nhefs_weights$qsmk,
      nhefs_weights$w_atm |> as.double(),
      std.error = TRUE
    )$std.error
  )
})

test_that("groups with more than two levels return correctly", {
  .smds <- tidy_smd(
    mtcars,
    c(mpg, disp, hp),
    .group = cyl
  )

  expect_tidy_smd_tbl(.smds, .rows = 6, .group = "cyl")

  expect_equal(
    pull_term(.smds, "mpg"),
    smd::smd(mtcars$mpg, mtcars$cyl)$term
  )

  expect_equal(
    pull_term(.smds, "disp"),
    smd::smd(mtcars$disp, mtcars$cyl)$term
  )

  expect_equal(
    pull_term(.smds, "hp"),
    smd::smd(mtcars$hp, mtcars$cyl)$term
  )
})


test_that("tidy_smd() works with `make_dummy_vars = TRUE`", {
  .smds <- tidy_smd(
    nhefs_weights,
    c(age, race, education),
    .group = qsmk,
    .wts = w_ate,
    make_dummy_vars = TRUE
  )

  expect_tidy_smd_tbl(.smds, .rows = 12)
})

test_that("tidy_smd() works with quoted variables", {
  .smds <- tidy_smd(
    nhefs_weights,
    c("age", "race", "education"),
    .group = "qsmk",
    .wts = "w_ate"
  )

  expect_tidy_smd_tbl(.smds, .rows = 6)
})
