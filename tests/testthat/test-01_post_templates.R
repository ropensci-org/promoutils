test_that("post templates", {

  # Works for past posts
  expect_message(post_coworking("2023-07-04", "@maelle@mastodon.social", "@maelle"))

  post_coworking("2023-10-03", "@Drmowinckels@fosstodon.org ", "@Mo (Athanasia Mowinckel)") |>
    expect_message("Timezone: Europe/Paris") |>
    expect_type("list")
})
