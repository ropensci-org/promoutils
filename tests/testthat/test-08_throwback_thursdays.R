test_that("tt_posts()", {
  withr::local_envvar(list(CLIPR_ALLOW = TRUE))

  t <- tt_post(
    "2017-08-22",
    "So you (don't) think you can review a package",
    url = "https://ropensci.org/blog/2017/08/22/first-package-review/",
    blurb = "test",
    print = TRUE
  ) |>
    expect_message("Copied Throwback Thursday") |>
    capture_output()

  expect_match(t, "promoutils::socials_post_issue")
  expect_match(t, "mastodon|linkedin")
  expect_true(all(stringr::str_detect(t, "mastodon|linkedin")))
  expect_equal(stringr::str_count(t, "\\:calendar\\:"), 2)
})

test_that("tt_post() multiple posts", {
  withr::local_envvar(list(CLIPR_ALLOW = TRUE))

  # Error on lengths mismatch
  expect_error(
    tt_post(
      date = c("2020-01-01", "2020-02-01"),
      title = "single-title",
      url = c("u1", "u2"),
    ),
    "all need to be the same length"
  )

  # But only require one blurb

  expect_message(
    tt_post(date = "2020-01-01", title = "t", url = "u"),
    "Copied Throwback Thursday"
  )

  # But if more than one blurb, must match the others
  expect_error(
    tt_post(
      date = "2020-01-01",
      title = "t",
      url = "u",
      blurb = c("b1", "b2", "b3")
    ),
    "all need to be the same length"
  )
})

test_that("tt_post() allows multiple links", {
  withr::local_envvar(list(CLIPR_ALLOW = TRUE))

  t <- tt_post(
    c("2017-08-22", "2017-08-22"),
    c(
      "So you (don't) think you can review a package",
      "Onboarding visdat, a tool for preliminary visualisation of whole dataframes"
    ),
    url = c(
      "https://ropensci.org/blog/2017/08/22/first-package-review/",
      "https://ropensci.org/blog/2017/08/22/visdat/"
    ),
    blurb = "test",
    print = TRUE
  ) |>
    expect_message("Copied Throwback Thursday") |>
    capture_output()

  expect_match(t, "promoutils::socials_post_issue")
  expect_equal(stringr::str_count(t, "mastodon|linkedin"), 2)
  expect_equal(stringr::str_count(t, "\\:calendar\\:"), 4)
})
