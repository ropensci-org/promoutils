# Test Chat Functions (No Slack API calls)

# Pretend these pairs were paired in the past
write_history <- function(dir, history) {
  dir.create(file.path(dir, "pairings"))
  readr::write_csv(history, file.path(dir, "pairings", "pairings_old.csv"))
}

all_ids <- function(p) {
  x <- c(p$id_1, p$id_2, p$id_3)
  x[!is.na(x)]
}


# chats_languages() / chats_pinned() -----------------------------------------

test_that("chats_languages()", {
  l <- chats_languages()
  expect_named(l)
  expect_true(all(c("English", "French", "Spanish", "Portuguese") %in% l))
})

test_that("chats_pinned()", {
  expect_match(chats_pinned(), "^https://ropensci.slack.com/archives/")
})

# chats_announce() -----------------------------------------------------------

test_that("chats_announce()", {
  local_no_api()
  expect_message(chats_announce(dry_run = TRUE), "Where: coffee-chats")
})

# chats_signups() ------------------------------------------------------------

test_that("chats_signups()", {
  author <- "U0000009"
  reactions <- list(
    ok = TRUE,
    message = list(
      user = author,
      reactions = list(
        list(name = "bee", users = list("U0000001", "U0000002", author)),
        list(name = "swan", users = list("U0000001", author)),
        list(name = "tada", users = list("U0000003"))
      )
    )
  )

  # Only the reactions.get request reaches httr2, return dummy response
  httr2::local_mocked_responses(\(req) httr2::response_json(body = reactions))

  local_mocked_bindings(
    slack_auth = \(req) req,
    slack_channel = \(...) "C0000000",
    slack_messages = \(...) {
      dplyr::tibble(
        ts = "100.000",
        time = as.POSIXct(100),
        text = "We're preparing for our next round of coffee chats"
      )
    },
    slack_users = \() {
      dplyr::tibble(
        id = paste0("<@U000000", c(1, 2, 9), ">"),
        name = c("ann", "bob", "zoe"),
        real_name = c("Ann Smith", "Bob Jones", "Zoe Author")
      )
    }
  )

  # Only language emoji, and author only in English
  s <- chats_signups()
  expect_named(s, c("name", "real_name", "id", "language"))
  expect_equal(
    s$id,
    c("<@U0000001>", "<@U0000001>", "<@U0000002>", "<@U0000009>")
  )
  expect_equal(s$language, c("English", "French", "English", "English"))

  # Author excluded
  s <- chats_signups(author_lang = NULL)
  expect_false("<@U0000009>" %in% s$id)
})

test_that("chats_signups() with no announcement", {
  local_no_api()
  local_mocked_bindings(
    slack_channel = \(...) "C0000000",
    slack_messages = \(...) {
      dplyr::tibble(ts = "100.000", time = as.POSIXct(100), text = "Hi")
    }
  )
  expect_error(chats_signups(), "No chat announcement found")
})


# chats_pairings() -----------------------------------------------------------

test_that("chats_pairings()", {
  local_no_api()
  test_cache()
  s <- test_signups_data()

  expect_message(p <- chats_pairings(s), "No cached pairings data") |>
    expect_message("No partner for")

  expect_s3_class(p, "data.frame")
  expect_true(all(is.na(p$days_since_paired)))
  expect_false(anyDuplicated(all_ids(p)) > 0)

  # Names match ids
  names <- rlang::set_names(s$real_name, s$id)
  expect_equal(p$person_1, unname(names[p$id_1]))
  expect_equal(p$person_3, unname(names[p$id_3]))

  # The three Portuguese speakers are a trio
  trio <- dplyr::filter(p, !is.na(.data$id_3))
  expect_equal(trio$language, "Portuguese")
})

test_that("chats_pairings() pairs people with the fewest options first", {
  local_no_api()
  test_cache()

  # B can only chat with A (French), so A & B must be paired
  s <- test_signups_data(
    c("A", "A", "B", "C", "D"),
    c("English", "French", "French", "English", "English")
  )
  p <- suppressMessages(chats_pairings(s))
  expect_equal(paste(p$id_1, p$id_2), c("A B", "C D"))
})

test_that("chats_pairings() avoids repeat pairings", {
  local_no_api()
  dir <- test_cache()
  write_history(
    dir,
    dplyr::tibble(
      date = Sys.Date() - 30,
      id_1 = c("A", "C"),
      id_2 = c("B", "D")
    )
  )

  # No A-B or C-D pairing
  p <- chats_pairings(test_signups_data(c("A", "B", "C", "D"), "English"))
  expect_true(all(is.na(p$days_since_paired)))
  expect_false(any(paste(p$id_1, p$id_2) %in% c("A B", "C D")))
})

test_that("chats_pairings() uses the oldest repeat when there's no other option", {
  local_no_api()
  dir <- test_cache()
  write_history(
    dir,
    dplyr::tibble(
      date = as.Date(c("2026-01-01", "2025-01-01", "2026-06-01")),
      id_1 = c("A", "A", "B"),
      id_2 = c("B", "C", "C")
    )
  )

  p <- chats_pairings(test_signups_data(c("A", "B", "C"), "English"))
  expect_equal(c(p$id_1, p$id_2, p$id_3), c("A", "C", "B"))
  expect_equal(
    p$days_since_paired,
    as.numeric(Sys.Date() - as.Date("2025-01-01"))
  )
})

test_that("chats_pairings(save = TRUE) saves pairings", {
  local_no_api()
  dir <- test_cache()

  expect_message(
    chats_pairings(test_signups_data(c("A", "B"), "English")),
    "No cached"
  )
  expect_false(dir.exists(file.path(dir, "pairings")))

  expect_message(
    chats_pairings(test_signups_data(c("A", "B"), "English"), save = TRUE),
    "Writing data"
  )
  expect_length(list.files(file.path(dir, "pairings")), 1)
})

# chats_trios() --------------------------------------------------------------

test_that("chats_trios() prefers pairs with people you haven't met", {
  s <- test_signups_data(c("A", "B", "C", "D", "E"), "English")
  pairs <- dplyr::tibble(
    id_1 = c("A", "C"),
    id_2 = c("B", "D"),
    language = "English"
  )

  # E has met A, but no one in C & D, so added to second group as trio
  met <- dplyr::tibble(
    id_1 = "A",
    id_2 = "E",
    last_date = as.Date("2025-01-01")
  )
  expect_equal(chats_trios(pairs, met, s)$id_3, c(NA, "E"))

  # E has met A and C, but met C longer ago
  met <- dplyr::tibble(
    id_1 = c("A", "C"),
    id_2 = "E",
    last_date = as.Date(c("2026-01-01", "2025-01-01"))
  )
  expect_equal(chats_trios(pairs, met, s)$id_3, c(NA, "E"))
})

# chats_save() / chats_read() ------------------------------------------------

test_that("chats_read() with no cache", {
  test_cache()
  expect_message(h <- chats_read(), "No cached pairings data")
  expect_equal(nrow(h), 0)
  expect_named(h, c("date", "id_1", "id_2"))
})

test_that("chats_save() stores trios as three pairs", {
  dir <- test_cache()
  pairs <- dplyr::tibble(
    id_1 = c("A", "D"),
    id_2 = c("C", "E"),
    id_3 = c("B", NA),
    language = "English"
  )
  expect_message(chats_save(pairs, save = TRUE), "Writing data")

  # Read file directly (chats_read() ignores today's pairings)
  h <- list.files(file.path(dir, "pairings"), full.names = TRUE) |>
    readr::read_csv(show_col_types = FALSE)
  expect_setequal(paste(h$id_1, h$id_2), c("A C", "A B", "B C", "D E"))
})

# chats_announce_pairs() -----------------------------------------------------

test_that("chats_announce_pairs()", {
  local_no_api()
  withr::local_options(cli.width = 1000)
  pairs <- dplyr::tibble(
    id_1 = c("<@U0000001>", "<@U0000004>"),
    id_2 = c("<@U0000002>", "<@U0000005>"),
    id_3 = c(NA, "<@U0000006>"),
    language = c("English, French", "Spanish")
  )

  chats_announce_pairs(pairs, dry_run = TRUE) |>
    expect_message("Here are the pairings for this round")
  m <- paste(m, collapse = "")
  expect_match(m, "<@U0000001> & <@U0000002> (English, French)", fixed = TRUE)
  expect_match(
    m,
    "<@U0000004> & <@U0000005> & <@U0000006> (Spanish)",
    fixed = TRUE
  )

  expect_error(
    chats_announce_pairs(pairs[0, ], dry_run = TRUE),
    "No pairings to announce"
  )
})
