test_that("help_fetch()", {
  skip_on_ci()
  skip_on_runiverse()

  expect_message(h <- help_fetch())
})

test_that("help_handles()", {
  local_mocked_cocoon()

  expect_silent(h <- help_handles(test_help_data()))
  expect_s3_class(h, "data.frame")
  expect_named(h, c("name", "github", "mastodon", "bluesky", "linkedin"))
})

test_that("help_preview()", {
  expect_message(h <- help_preview(test_help_data()), "Preview") |>
    expect_message("1\\. weathercan") |>
    suppressMessages()
})

test_that("help_post()", {
  local_mocked_cocoon()
  expect_output(help_post(test_help_data(), dry_run = TRUE))
})
