# Test Slack Functions (Dry Runs Only)

test_that("slack_posts_write()", {
  # Basic ------------------------------------
  expect_message(
    slack_posts_write(
      "Test message for immediate posting",
      when = "now",
      channel = "#testing-api",
      dry_run = TRUE
    )
  ) |>
    suppressMessages()

  # Scheduled ---------------------------------
  future_time <- Sys.time() + 3600
  expect_message(
    slack_posts_write(
      "Test message for scheduled posting",
      when = future_time,
      tz = "America/Winnipeg",
      channel = "#testing-api",
      dry_run = TRUE
    )
  ) |>
    suppressMessages()

  # Multiline -------------------------------
  multiline_msg <- paste(
    "Join us for Social Coworking!",
    "",
    ":grey_exclamation: Theme: TESTING",
    ":hourglass_flowing_sand: When: TODAY!",
    ":cookie: Hosted by: Test User",
    "",
    "You can use this time for...",
    "- General coworking",
    "- Testing features",
    sep = "\n"
  )
  expect_message(
    slack_posts_write(
      multiline_msg,
      when = "now",
      channel = "#testing-api",
      dry_run = TRUE
    )
  ) |>
    suppressMessages()

  # Links ------------------------------------
  msg_with_link <- "Check out [this cool link](https://mycoolsite.com) for more info!"
  expect_message(
    slack_posts_write(
      msg_with_link,
      when = "now",
      channel = "#testing-api",
      dry_run = TRUE
    )
  ) |>
    suppressMessages()

  # Default Channel----------------------------
  expect_message(
    slack_posts_write(
      "Test with default channel",
      when = "now",
      dry_run = TRUE
    )
  ) |>
    suppressMessages()

  # Long messages ------------------------------------
  long_msg <- paste(rep("This is a test message. ", 50), collapse = "")

  expect_message(
    slack_posts_write(
      long_msg,
      when = "now",
      channel = "#testing-api",
      dry_run = TRUE
    )
  ) |>
    suppressMessages()

  # Emojis ---------------------------------------------------------
  emoji_msg <- ":rocket: Launch time! :tada: :sparkles:"

  expect_message(
    slack_posts_write(
      emoji_msg,
      when = "now",
      channel = "#testing-api",
      dry_run = TRUE
    )
  ) |>
    suppressMessages()
})
