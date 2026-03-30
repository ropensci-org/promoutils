test_that("help_fetch()", {
  expect_silent(h <- help_fetch("2025-01-01"))
  expect_s3_class(h, "data.frame")
})

test_that("help_fetch() |> help_handles()", {
  expect_silent(h <- help_fetch("2025-01-01") |> help_handles())
  expect_s3_class(h, "data.frame")
  expect_all_true(c("maintainer_linkedin", "labeller_mastodon") %in% names(h))
})

test_that("help_fetch() |> help_handles() |> by_platform() then help_post()", {
  expect_silent(
    h <- help_fetch("2025-05-23") |>
      help_handles() |>
      by_platform()
  )
  expect_s3_class(h, "data.frame")
  expect_true(c("platform") %in% names(h))
  expect_all_true(h$platform %in% c("linkedin", "mastodon"))

  expect_output(help_post(h, print = TRUE))
  expect_output(
    help_post(h[1, ], print = TRUE),
    "Check out this 'help wanted'"
  )
})
