test_that("socials_post_issue", {
  expect_message(
    socials_post_issue(
      time = "2023-01-01 10:00:00",
      tz = "America/Vancouver",
      title = "Test Post",
      body = "TEsting social_post_issue\n\nSo cool!",
      where = "mastodon",
      dry_run = TRUE
    ),
    "Post to ",
  ) |>
    suppressMessages()

  #No hashtags at the end
  expect_message(
    socials_post_issue(
      time = "2023-01-01 10:00:00",
      tz = "America/Vancouver",
      title = "Test Post",
      body = "Testing social_post_issue\n\nSo cool!",
      where = "mastodon",
      add_hash = FALSE,
      dry_run = TRUE
    ),
    "Post to"
  ) |>
    expect_message("So cool!\\n$") |>
    suppressMessages()

  # From file
  f <- withr::local_tempfile()
  write(
    "--- Mastodon ---\nTesting the body\n\n--- LinkedIn ---\nTesting the body again",
    f
  )
  socials_post_issue(
    time = "2023-01-01 10:00:00",
    tz = "America/Vancouver",
    title = "Test Post",
    body = f,
    dry_run = TRUE
  ) |>
    expect_message("mastodon") |>
    expect_message("Testing the body\\n") |>
    expect_message("linkedin") |>
    expect_message("Testing the body again") |>
    suppressMessages()

  # Too long
  f <- withr::local_tempfile()
  write(rep(LETTERS, 100), f)

  socials_post_issue(
    time = "2023-01-01 10:00:00",
    tz = "America/Vancouver",
    title = "Test Post",
    body = f,
    dry_run = TRUE
  ) |>
    expect_error("Very close or over the character limit")
})
