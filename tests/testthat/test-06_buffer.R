test_that("dry_run argument", {
  expect_output(q <- buffer_org(dry_run = TRUE), "POST / HTTP/1.1")
  expect_true(attr(q, "dry_run"))

  expect_output(q <- buffer_channels(dry_run = TRUE), "POST / HTTP/1.1")
  expect_true(attr(q, "dry_run"))

  expect_output(q <- buffer_posts_list(dry_run = TRUE), "POST / HTTP/1.1")
  expect_true(attr(q, "dry_run"))

  expect_output(
    q <- buffer_posts_write("Testing dry-run", when = "now", dry_run = TRUE),
    "POST / HTTP/1.1"
  )
  expect_length(q, 3)
  expect_true(attr(q[[1]], "dry_run"))

  expect_output(
    q <- buffer_posts_write(
      "Testing dry-run",
      when = "2099-01-01 10:00:00",
      dry_run = TRUE
    ),
    "POST / HTTP/1.1"
  )
  expect_length(q, 3)
  expect_true(attr(q[[1]], "dry_run"))
})

with_mock_dir("../mock/buffer-org", {
  test_that("buffer_org()", {
    expect_silent(o <- buffer_org()) |>
      expect_type("character")
    expect_equal(o, buff_org)
  })
})

with_mock_dir("../mock/buffer-channel", {
  test_that("buffer_channels()", {
    expect_silent(c <- buffer_channels()) |>
      expect_s3_class("data.frame")
    expect_equal(sort(c$service), c("bluesky", "linkedin", "mastodon"))
  })
})

with_mock_dir("../mock/buffer-list", {
  test_that("buffer_posts_list()", {
    expect_silent(p <- buffer_posts_list()) |>
      expect_s3_class("data.frame")
  })
})
