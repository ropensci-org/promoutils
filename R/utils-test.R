test_help_data <- function(missing = NULL) {
  h <- data.frame(
    package = "weathercan",
    maintainer_name = "Steffi LaZerte",
    maintainer_github = "steffilazerte",
    maintainer_mastodon = "steffilazerte@fosstodon.org",
    labeller_name = "Yanina Bellini Saibene",
    labeller_github = "yabellini",
    labels = "help wanted",
    labels_first = TRUE,
    label_created = as.POSIXct("2025-01-01 00:00:00"),
    title = "test",
    url = "https://test"
  )

  h[, missing] <- NA_character_

  h
}

local_mocked_cocoon <- function(.env = rlang::caller_env()) {
  testthat::local_mocked_bindings(
    cocoon_fetch = \(x) {
      data.frame(
        type = rep(c("github", "name", "mastodon", "linkedin"), 2),
        value = c(
          "steffilazerte",
          "Steffi LaZerte",
          "steffilazerte@fosstodon.ca",
          "steffi-lazerte",
          "yabellini",
          "Yanina Bellini Saibene",
          "@yabellini@rstats.me",
          "yabellini"
        ),
        github = c(rep("steffilazerte", 4), rep("yabellini", 4))
      )
    },
    .package = "monarch" # Not advised generally, but works for us
  )
}
