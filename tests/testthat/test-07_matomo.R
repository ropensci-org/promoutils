test_that("matomo_dir()", {
  expect_silent(matomo_dir())
  expect_match(matomo_dir(), "promoutils/views")
})

test_that("matomo_fetch()", {
  expect_message(
    v <- matomo_fetch(as.Date(c("2025-01-01", "2025-01-05"))),
    "Fetching segments"
  ) |>
    expect_message("Fetching pages")
  expect_s3_class(v, "data.frame")
})

test_that("matomo_update()", {
  expect_message(matomo_update(), "Fetching segments") |>
    expect_message("Fetching pages") |>
    expect_message("Writing")
})


test_that("matomo_read()", {
  expect_silent(v <- matomo_read())
  expect_s3_class(v, "data.frame")
  expect_true("type" %in% names(v))
  expect_true(nrow(v) > 0)
})

test_that("matomo_blogposts()", {
  expect_silent(v <- matomo_read() |> matomo_blogposts())
  expect_s3_class(v, "data.frame")
  expect_false("type" %in% names(v))
  expect_true(nrow(v) > 0)
})
