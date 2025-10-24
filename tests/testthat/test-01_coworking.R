test_that("cw_details()", {
  skip_on_ci()
  expect_silent(d <- cw_details(which = "next"))
  expect_s3_class(d, "data.frame")
  expect_named(d, c("title", "body", "date", "tz", "theme", "cohost"))
})


test_that("cw_issue()", {
  skip_on_ci()

  expect_error(cw_issue("2023-11", dry_run = TRUE), "Invalid date")

  with_mocked_bindings(
    cw_details = function(...) {
      data.frame(
        title = "[Coworking]",
        body = "Theme...",
        date = "2025-01-01",
        tz = "Americas Pacific",
        theme = "Test theme",
        cohost = "Best ever"
      )
    },
    code = {
      expect_message(
        cw_issue("2025-01-01", dry_run = TRUE),
        "Post"
      ) %>%
        suppressMessages()
    }
  )
})

test_that("cw_event()", {
  skip_on_ci()

  with_mocked_bindings(
    cw_details = function(...) {
      data.frame(
        title = "[Coworking]",
        body = "Theme...",
        date = "2025-01-01",
        tz = "Americas Pacific",
        theme = "Test theme",
        cohost = "Best ever"
      )
    },
    code = {
      expect_silent(e <- cw_event("2025-01", dry_run = TRUE))
      expect_true(is.character(e))
    }
  )

  with_mocked_bindings(
    cw_details = function(...) {
      data.frame(
        title = "[Coworking]",
        body = "Theme...",
        date = "2025-01-01",
        tz = "Americas",
        theme = "Test theme",
        cohost = "Best ever"
      )
    },
    code = {
      expect_error(
        cw_event("2025-01", dry_run = TRUE),
        "`tz` \\(NA\\) not in `OlsonNames\\(\\)`"
      )
    }
  )
})

test_that("cw_socials()", {
  skip_on_ci()
  l <- cw_socials("2025-07-01", "Test ", "@test", "", dry_run = TRUE) |>
    expect_message("Timezone: America/Vancouver") |>
    suppressMessages()

  expect_type(l, "list")
})
