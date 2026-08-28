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

with_mock_dir("buffer", {
  test_that("buffer_org()", {
    expect_silent(o <- buffer_org()) |>
      expect_type("character")
    expect_equal(o, buff_org)
  })

  test_that("buffer_channels()", {
    expect_silent(c <- buffer_channels()) |>
      expect_s3_class("data.frame")
    expect_equal(c$service, c("linkedin", "mastodon", "bluesky"))
  })

  test_that("buffer_posts_list()", {
    expect_silent(p <- buffer_posts_list()) |>
      expect_s3_class("data.frame")
  })

  test_that("now - buffer_posts_write() / buffer_posts_remove()", {
    expect_silent(
      p <- buffer_posts_write("testing Api again...", when = "now")
    ) |>
      expect_s3_class("data.frame")
    expect_named(p, c("id", "channelService", "text", "dueAt", "status"))
    expect_true(all(p$text == "testing Api again..."))

    l <- buffer_posts_list(status = "draft")
    expect_true(all(p$id %in% l$id))

    expect_silent(p0 <- buffer_posts_remove(p$id))
    expect_named(p0, "removed_post_ids")
    expect_equal(p0$removed_post_ids, p$id)

    l <- buffer_posts_list(status = "draft")
    expect_false(any(p$id %in% l$id))
  })

  test_that("scheduled - buffer_posts_write() / buffer_posts_remove()", {
    expect_silent(
      p <- buffer_posts_write(
        "testing scheduled Api again...",
        when = Sys.time() + lubridate::years(1)
      )
    ) |>
      expect_s3_class("data.frame")
    expect_named(p, c("id", "channelService", "text", "dueAt", "status"))
    expect_true(all(p$text == "testing scheduled Api again..."))

    l <- buffer_posts_list(status = "draft")
    expect_true(all(p$id %in% l$id))

    expect_silent(p0 <- buffer_posts_remove(p$id))
    expect_named(p0, "removed_post_ids")
    expect_equal(p0$removed_post_ids, p$id)

    l <- buffer_posts_list(status = "draft")
    expect_false(any(p$id %in% l$id))
  })
})
