test_that("gh_issue_fetch(), gh_issue_fmt()", {
  skip_if_not_all()

  expect_silent(i1 <- gh_issue_fetch(owner = "ropensci", repo = "weathercan"))
  expect_type(i1, "list")

  expect_silent(i2 <- gh_issue_fmt(i1))
  expect_s3_class(i2, "data.frame")
  expect_equal(nrow(i2), length(i1))

  expect_silent(i3 <- gh_issue_labels(i2))
  expect_s3_class(i3, "data.frame")
})

test_that("gh_label_events()", {
  skip_if_not_all()

  expect_silent(gh_label_events(
    "ropensci",
    "weathercan",
    issue = 149,
    labels = "help wanted"
  )) |>
    expect_equal(dplyr::tibble(
      gh_user_labelled = "steffilazerte",
      "label_created" = as.POSIXct("2025-02-03 20:06:12", tz = "UTC")
    ))
})
