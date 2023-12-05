test_that("socials_post_issue", {

  expect_message(
    socials_post_issue(time = "2023-01-01 10:00:00", tz = "America/Vancouver",
                       title = "Test Post", body = "TEsting social_post_issue\n\nSo cool!",
                       where = "mastodon", dry_run = TRUE),
    "Posting",
  ) |>
    expect_message("#RStats")

  expect_message(
    socials_post_issue(time = "2023-01-01 10:00:00", tz = "America/Vancouver",
                       title = "Test Post", body = "Testing social_post_issue\n\nSo cool!",
                       where = "mastodon", add_hash = FALSE, dry_run = TRUE),
    "Posting"
  ) |>
    expect_message("So cool!\\n$") #No hashtags at the end

})
