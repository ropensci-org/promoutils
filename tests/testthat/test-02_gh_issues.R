test_that("gh_issue_fetch(), gh_issue_fmt()", {
  skip_on_ci()

  expect_silent(i1 <- gh_issue_fetch())
  expect_type(i1, "list")

  expect_silent(i2 <- gh_issue_fmt(i1))
  expect_s3_class(i2, "data.frame")
  expect_equal(nrow(i2), length(i1))

  expect_silent(i3 <- gh_issue_labels(i2))
  expect_s3_class(i3, "data.frame")
})
