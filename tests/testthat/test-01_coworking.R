test_that("cw_details()", {
  expect_silent(d <- cw_details(which = "next"))
  expect_s3_class(d, "data.frame")
  expect_silent(d <- cw_details(which = "2023-11"))
  expect_s3_class(d, "data.frame")
  expect_named(d, c("title", "body", "date", "tz", "theme", "cohost"))
})


test_that("cw_issue()", {
  expect_error(cw_issue("2023-11", dry_run = TRUE))
  expect_message(cw_issue("2023-11-07", dry_run = TRUE), "Posting issue")
})

test_that("cw_event()", {
  expect_silent(e <- cw_event("2023-11", dry_run = TRUE))
  expect_true(is.character(e))
})

test_that("cw_socials()", {
  cw_socials("2023-10-03", "@Drmowinckels@fosstodon.org ", "@Mo (Athanasia Mowinckel)", "", dry_run = TRUE) |>
    expect_message("Timezone: Europe/Paris") |>
    expect_type("list")
})
