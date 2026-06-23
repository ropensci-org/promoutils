test_that("slack_posts_write() immediate", {
  # Actually post but only to the #testing-api channel
  skip_if_not_all()

  expect_message(
    slack_posts_write(
      "Test message for immediate posting",
      when = "now",
      channel = "#testing-api"
    ),
    "Slack message posted successfully to #testing-api"
  ) |>
    suppressMessages()

  # Test present and removed successfully
  m <- slack_messages(channel = "#testing-api")
  expect_equal(m$text[1], "Test message for immediate posting")
  expect_message(
    slack_message_rm(channel = "#testing-api", ts = m$ts[1]),
    "successfully removed"
  )

  m <- slack_messages(channel = "#testing-api")
  expect_true(m$text[1] != "Test message for immediate posting")
})

test_that("slack_posts_write() future", {
  skip_if_not_all()

  # Actually post but only to the #testing-api channel
  future_time <- Sys.time() + 3600
  expect_message(
    slack_posts_write(
      "Test message for scheduled posting",
      when = future_time,
      tz = "America/Winnipeg",
      channel = "#testing-api"
    ),
    "Slack message scheduled successfully to #testing-api"
  ) |>
    expect_message("Scheduled message successfully added to #admin-scheduled")

  # Test present and removed successfully
  expect_silent(
    m <- slack_scheduled_list() |> dplyr::arrange(dplyr::desc(date_created_dt))
  )
  expect_equal(m$text[1], "Test message for scheduled posting")
  expect_equal(round(m$post_at[1]), round(as.numeric(future_time)))
  expect_message(
    slack_scheduled_rm(m[1, ], "#testing-api"),
    "successfully removed"
  )

  m <- slack_scheduled_list()
  expect_true(m$text[1] != "Test message for immediate posting")
})

test_that("slack_posts_write", {
  skip_if_not_all()

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
