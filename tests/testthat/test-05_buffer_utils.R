test_that("buffer_query()", {
  template <- "query { account { organizations { {{fields}} } } }"
  fields <- c("id", "name")
  expect_silent(buffer_query(template, fields = fields)) |>
    expect_equal("query { account { organizations { id name } } }")
})

with_mock_dir("buffer", {
  test_that("buffer_request() & buffer_df()", {})

  template <- "query { channels(input: { organizationId: \"{{org}}\" }) { {{fields}} } }"

  q <- buffer_query(
    template,
    fields = c("id", "service"),
    org = buff_org
  )

  expect_silent(r <- buffer_request(q)) |>
    expect_s3_class("httr2_response")

  expect_silent(d <- buffer_df(r)) |>
    expect_s3_class("data.frame")
  expect_equal(nrow(d), 3)

  expect_silent(d <- buffer_df(list(list(r), list(r)))) |>
    expect_s3_class("data.frame")
  expect_equal(nrow(d), 6)
})
