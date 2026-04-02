test_that("help_fetch()", {
  skip_on_ci()
  expect_silent(h <- help_fetch("2025-01-01"))
  expect_s3_class(h, "data.frame")
})

test_that("help_handles()", {
  local_mocked_cocoon()

  expect_silent(h <- help_handles(test_help_data()))
  expect_s3_class(h, "data.frame")
  expect_all_true(c("maintainer_linkedin", "labeller_mastodon") %in% names(h))
})

test_that("by_platform() & help_post()", {
  local_mocked_cocoon()

  expect_silent(
    h <- test_help_data() |>
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
