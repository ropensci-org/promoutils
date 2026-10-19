test_help_data <- function(missing = NULL) {
  h <- data.frame(
    package = "weathercan",
    owner = "ropensci",
    issue_url = "https://test/#1",
    title = "test",
    opened = as.POSIXct("2025-01-01 00:00:00"),
    updated = as.POSIXct("2025-01-02 00:00:00"),
    labels = "help wanted",
    maintainer_name = "Steffi LaZerte",
    maintainer_github = "steffilazerte",
    labels_name = "Yanina Bellini Saibene",
    labels_github = "yabellini",
    labels_first = TRUE,
    url = "https://test"
  )

  h[, missing] <- NA_character_

  h
}


# Dummy sign ups imitating the output of `chats_signups()`
test_signups_data <- function(ids = NULL, langs = NULL) {
  if (is.null(ids) || is.null(langs)) {
    s <- dplyr::tribble(
      ~name , ~real_name      , ~id           , ~language    ,
      "ann" , "Ann Smith"     , "<@U0000001>" , "English"    ,
      "ann" , "Ann Smith"     , "<@U0000001>" , "French"     ,
      "bob" , "Bob Jones"     , "<@U0000002>" , "English"    ,
      "cat" , "Cat Martin"    , "<@U0000003>" , "French"     ,
      "dan" , "Dan Garcia"    , "<@U0000004>" , "Spanish"    ,
      "eve" , "Eve Rodriguez" , "<@U0000005>" , "English"    ,
      "eve" , "Eve Rodriguez" , "<@U0000005>" , "Spanish"    ,
      "fay" , "Fay Silva"     , "<@U0000006>" , "Portuguese" ,
      "gus" , "Gus Costa"     , "<@U0000007>" , "Portuguese" ,
      "han" , "Hank Reid"     , "<@U0000008>" , "Portuguese"
    )
  } else {
    s <- dplyr::tibble(
      name = tolower(ids),
      real_name = paste("Person", ids),
      id = ids,
      language = langs
    )
  }
  s
}


# Use a temporary cache
test_cache <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  testthat::local_mocked_bindings(
    cache_dir = \(type) file.path(dir, type),
    .env = env
  )
  dir
}

local_mocked_cocoon <- function(.env = rlang::caller_env()) {
  testthat::local_mocked_bindings(
    cocoon_open = \(x) {
      data.frame(
        type = rep(c("github", "name", "mastodon", "linkedin", "bluesky"), 2),
        value = c(
          "steffilazerte",
          "Steffi LaZerte",
          "steffilazerte@fosstodon.ca",
          "steffi-lazerte",
          "@steffilazerte.bsky.social",
          "yabellini",
          "Yanina Bellini Saibene",
          "@yabellini@rstats.me",
          "yabellini",
          "@yabellini.bsky.social"
        ),
        github = c(rep("steffilazerte", 5), rep("yabellini", 5))
      )
    },
    .env = .env,
    .package = "monarch" # Not advised generally, but works for us
  )
}

skip_if_not_all <- function() {
  # Do not run on R-Universe ever (no credentials for API)
  # On CI run only if set to TEST_ALL
  # Otherwise always run
  not_ci <- Sys.getenv("CI") == ""
  test_all <- Sys.getenv("TEST_ALL") == "yes"

  testthat::skip_if_not(
    !on_runiverse() && (not_ci || test_all),
    "Not time for a full API test"
  )
}

on_runiverse <- function() {
  Sys.getenv("MY_UNIVERSE") != ""
}

skip_on_runiverse <- function() {
  testthat::skip_if(on_runiverse(), "On R-Universe")
}

# Forces an Error on any HTTP request, so tests never reach an API through httr2
local_no_api <- function(env = parent.frame()) {
  httr2::local_mocked_responses(
    \(req) cli::cli_abort("Unexpected API call to {req$url}"),
    env = env
  )
}
