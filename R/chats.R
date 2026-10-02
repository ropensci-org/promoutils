#' Announce/remind people about chats
#'
#' @param when Character or Date/time. When to post announcement.
#' @param channel Character. Channel name to make announcement in.
#'
#' @inheritParams common_docs
#'
#' @returns Success message
#'
#' @export
#' @seealso Slacks Special Mentions - <https://docs.slack.dev/messaging/formatting-message-text/#special-mentions>
#'
#' @examplesIf interactive()
#' chats_announce(when = "now", test_run = TRUE)

chats_announce <- function(
  when = NULL,
  channel = "coffee-chats",
  dry_run = TRUE,
  test_run = FALSE
) {
  if (test_run) {
    cli::cli_inform("Test-Run: Using #testing-api channel instead of {channel}")
    channel <- "testing-api"
  }

  # Announce at the next available 1st Monday in the future (including today)
  when <- when %||%
    next_date(Sys.Date(), which = "Mon", n = 1) |>
    paste("10:00:00")

  # Due a week later
  due <- next_date(Sys.Date(), which = "Mon", n = 2) |>
    format("%A, %B %e %Y") |>
    stringr::str_squish()

  langs <- chats_languages()
  langs <- glue::glue(":{names(langs)}: -> {langs}") |>
    glue::glue_collapse(sep = "\n")

  body <- glue::glue(
    "<!channel> Hello everyone! We're preparing for our next round of coffee chats :coffee: :tea: :mate-drink:
  
If you would like to participate, please respond with an appropriate emoji so we can set you up with a partner (you can use more than one emoji if you are open to chatting in multiple languages).

{langs}

Please sign up by *{due}* to be considered in this round.
See this channel's pinned message for more details."
  )

  ts <- slack_posts_write(
    when = when,
    body = body,
    tz = "America/Toronto",
    channel = channel,
    dry_run = dry_run
  )

  # Add preliminary reactions
  if (!dry_run) {
    slack_react(ts, c("bee", "hibiscus", "swan", "ant"), channel = channel)
  }

  invisible(ts)
}

#' Get chat sign ups
#'
#' Returns the users who reacted to the most recent chat announcement (see
#' `chats_announce()`) with a language emoji.
#'
#' @param channel Character. Channel Name. Defaults to "coffee-chats"
#' @param author_lang Character vector. If the author of the announcement post
#'   wishes to participate, these are the languages to include for them. Any of
#'   `chats_languages()`.
#'
#' @returns Data frame of users and languages, with one row per user per
#'   language.
#'
#' @export
#' @references https://docs.slack.dev/reference/methods/reactions.get
#'
#' @examplesIf interactive()
#' chats_signups(channel = "testing-api")

chats_signups <- function(channel = "coffee-chats", author_lang = "English") {
  langs <- chats_languages()
  channel_id <- slack_channel(channel)

  # Find the most recent announcement
  ts <- slack_messages(channel_id = channel_id) |>
    dplyr::filter(stringr::str_detect(
      .data$text,
      "next round of coffee chats"
    )) |>
    dplyr::arrange(dplyr::desc(.data$time)) |>
    dplyr::pull(.data$ts)

  if (length(ts) == 0) {
    cli::cli_abort(
      "No chat announcement found in the last 100 messages",
      call = NULL
    )
  }

  msg <- httr2::request("https://slack.com/api/reactions.get") |>
    httr2::req_url_query(
      channel = channel_id,
      timestamp = ts[1],
      full = TRUE
    ) |>
    slack_auth() |>
    httr2::req_perform() |>
    slack_check() |>
    purrr::pluck("message")

  # Get all Slack space users
  # TODO: Should this be only get the relevant users? Or Cached?
  users <- slack_users() |>
    dplyr::select("id", "name", "real_name")

  r <- msg$reactions |>
    purrr::map(\(x) dplyr::tibble(emoji = x$name, id = unlist(x$users))) |>
    purrr::list_rbind(
      ptype = dplyr::tibble(emoji = character(), id = character())
    ) |>
    dplyr::mutate(
      language = unname(langs[.data$emoji]),
      id = paste0("<@", .data$id, ">")
    ) |>
    dplyr::filter(.data$emoji %in% names(.env$langs))

  # Assign author language
  author <- paste0("<@", msg$user, ">")
  if (!is.null(author_lang)) {
    r <- dplyr::filter(
      r,
      !(!.data$language %in% author_lang & .data$id == author)
    )
  } else {
    r <- dplyr::filter(r, .data$id != author)
  }

  r |>
    dplyr::left_join(users, by = "id") |>
    dplyr::select("name", "real_name", "id", "language") |>
    dplyr::arrange(.data$real_name, .data$language)
}

#' Pair chat sign ups
#'
#' Pairs people who signed up for chats (see `chats_signups()`) by a language
#' they have in common, using each person only once. Pairings are prioritized so
#' that:
#'
#' 1. People who have never been paired together before are paired first.
#' 2. People with the fewest possible partners are paired first (so that as many
#'    people as possible get a partner).
#' 3. Repeat pairings are only used when there are no other options, and then
#'    the pairs who were paired the longest ago are used first.
#'
#' Anyone left over (e.g., an odd number of people in a language) is added to a
#' pair that shares a language with them to make a trio, preferring pairs where
#' they haven't met either person before.
#'
#' Ties are broken randomly, so use `set.seed()` for reproducible pairings.
#' Previous pairings are read from the "pairings" cache (see `cache_dir()`).
#'
#' @param signups Data frame. Output of `chats_signups()`.
#' @param save Logical. Save these pairings to the "pairings" cache so they are
#'   avoided in the future? Only save the final pairings.
#'
#' @returns Data frame of pairs (or trios) with the names of each person, the
#'   language(s) in common, the number of days since any of them were last
#'   paired (`NA` if never), and their user ids. `person_3` and `id_3` are
#'   `NA` for pairs.
#'
#' @export
#' @examples
#' # Dummy sign ups imitating the output of `chats_signups()`
#' s <- dplyr::tribble(
#'   ~name, ~real_name,      ~id,           ~language,
#'   "ann", "Ann Smith",     "<@U0000001>", "English",
#'   "ann", "Ann Smith",     "<@U0000001>", "French",
#'   "bob", "Bob Jones",     "<@U0000002>", "English",
#'   "cat", "Cat Martin",    "<@U0000003>", "French",
#'   "dan", "Dan Garcia",    "<@U0000004>", "Spanish",
#'   "eve", "Eve Rodriguez", "<@U0000005>", "English",
#'   "eve", "Eve Rodriguez", "<@U0000005>", "Spanish",
#'   "fay", "Fay Silva",     "<@U0000006>", "Portuguese",
#'   "gus", "Gus Costa",     "<@U0000007>", "Portuguese",
#'   "han", "Hank Reid",     "<@U0000008>", "Portuguese")
#'
#' chats_pairings(s)
#'
#' @examplesIf interactive()
#' s <- chats_signups()
#' p <- chats_pairings(s)
#'
#' # Once happy with the pairings, save them
#' p <- chats_pairings(s, save = TRUE)

chats_pairings <- function(signups, save = FALSE) {
  last_paired <- chats_read() |>
    dplyr::arrange(dplyr::desc(.data$date)) |>
    dplyr::distinct(.data$id_1, .data$id_2, .keep_all = TRUE) |>
    dplyr::select("id_1", "id_2", "last_date" = "date")

  # All possible pairs sharing a language
  s <- dplyr::select(signups, "id", "language")
  candidates <- dplyr::inner_join(
    s,
    s,
    by = "language",
    suffix = c("_1", "_2"),
    relationship = "many-to-many"
  ) |>
    # Get only one of each pair
    dplyr::filter(.data$id_1 < .data$id_2) |>
    dplyr::summarize(
      language = paste(sort(unique(.data$language)), collapse = ", "),
      .by = c("id_1", "id_2")
    ) |>
    dplyr::left_join(last_paired, by = c("id_1", "id_2"))

  # Select pairs from all options
  pairs <- chats_match(candidates) |>
    dplyr::mutate(days_since_paired = as.numeric(Sys.Date() - .data$last_date))

  # Deal with odd-ones-out
  pairs <- chats_trios(pairs, candidates, s)
  names <- dplyr::distinct(signups, .data$id, .data$real_name)
  names <- rlang::set_names(names$real_name, names$id)

  pairs <- pairs |>
    dplyr::mutate(
      person_1 = unname(names[.data$id_1]),
      person_2 = unname(names[.data$id_2]),
      person_3 = unname(names[.data$id_3])
    ) |>
    dplyr::select(
      "person_1",
      "person_2",
      "person_3",
      "language",
      "days_since_paired",
      "id_1",
      "id_2",
      "id_3"
    )

  unpaired <- setdiff(
    names(names),
    c(pairs$id_1, pairs$id_2, pairs$id_3)
  )
  if (length(unpaired) > 0) {
    cli::cli_inform("No partner for: {names[unpaired]}")
  }

  chats_save(pairs, save)

  pairs
}


chats_match <- function(candidates) {
  all <- candidates

  # Shuffle so ties are broken randomly
  candidates <- dplyr::slice_sample(candidates, prop = 1)
  pairs <- candidates[0, ]

  while (nrow(candidates) > 0) {
    # Number of possible partners per person
    n <- table(c(candidates$id_1, candidates$id_2))

    # Prefer new pairs, then people with the fewest options
    p <- candidates |>
      dplyr::mutate(
        repeated = !is.na(.data$last_date),
        n_min = pmin(n[.data$id_1], n[.data$id_2]),
        n_max = pmax(n[.data$id_1], n[.data$id_2])
      ) |>
      dplyr::arrange(
        .data$repeated,
        .data$n_min,
        .data$n_max,
        .data$last_date
      ) |>
      dplyr::slice(1)

    pairs <- dplyr::bind_rows(
      pairs,
      dplyr::select(p, -"repeated", -"n_min", -"n_max")
    )

    # Remove everyone who is now paired
    used <- c(p$id_1, p$id_2)
    candidates <- dplyr::filter(
      candidates,
      !.data$id_1 %in% used,
      !.data$id_2 %in% used
    )
  }

  pairs
}

# Add anyone left over to a pair which shares a language with them
chats_trios <- function(pairs, candidates, s) {
  # Who is unpaired?
  unpaired <- setdiff(s$id, c(pairs$id_1, pairs$id_2))
  pairs$id_3 <- NA_character_

  for (u in unpaired) {
    u_lang <- s$language[s$id == u]

    # Get pair options which could be joined
    #  - share at least one language (pairs may share several, "English, French")
    #  - do not have a trio yet (an earlier unpaired person may have joined)
    options <- pairs |>
      dplyr::mutate(
        language = purrr::map_chr(
          stringr::str_split(.data$language, ", "),
          \(l) paste(intersect(l, u_lang), collapse = ", ")
        )
      ) |>
      dplyr::filter(.data$language != "", is.na(.data$id_3)) |>
      dplyr::mutate(id_3 = .env$u)

    # No trios to join, too bad :(
    if (nrow(options) == 0) {
      next
    }

    # More than one option, join the pair with the fewest people you've met
    # before, then the pair you met the longest ago
    if (nrow(options) > 1) {
      last_met <- candidates |>
        dplyr::filter(.data$id_1 == .env$u | .data$id_2 == .env$u) |>
        dplyr::mutate(
          partner = dplyr::if_else(
            .data$id_1 == .env$u,
            .data$id_2,
            .data$id_1
          )
        )
      last_met <- rlang::set_names(last_met$last_date, last_met$partner)

      options <- options |>
        dplyr::mutate(
          last_met_1 = .env$last_met[.data$id_1],
          last_met_2 = .env$last_met[.data$id_2],
          n_met = (!is.na(.data$last_met_1)) + (!is.na(.data$last_met_2)),
          last_met = pmax(.data$last_met_1, .data$last_met_2, na.rm = TRUE)
        ) |>
        dplyr::arrange(.data$n_met, .data$last_met) |>
        dplyr::slice(1) |>
        dplyr::select(-dplyr::contains("met"))
    }

    # Join the pair
    pairs <- dplyr::rows_update(
      pairs,
      options,
      by = c("id_1", "id_2")
    )
  }

  pairs
}

chats_save <- function(pairs, save) {
  if (save) {
    # Store trios as three pairs
    trios <- dplyr::filter(pairs, !is.na(.data$id_3))
    dplyr::bind_rows(
      dplyr::select(pairs, "id_1", "id_2", "language"),
      dplyr::select(trios, "id_1", "id_2" = "id_3", "language"),
      dplyr::select(
        trios,
        "id_1" = "id_2",
        "id_2" = "id_3",
        "language"
      )
    ) |>
      dplyr::mutate(
        date = Sys.Date(),
        first = pmin(.data$id_1, .data$id_2),
        id_2 = pmax(.data$id_1, .data$id_2),
        id_1 = .data$first
      ) |>
      dplyr::select("date", "id_1", "id_2", "language") |>
      cache_write("pairings", paste0("pairings_", Sys.Date(), ".csv"))
  }
}

chats_read <- function() {
  h <- cache_read("pairings")

  if (nrow(h) == 0) {
    h <- dplyr::tibble(
      date = as.Date(character()),
      id_1 = character(),
      id_2 = character()
    )
  }
  dplyr::filter(h, .data$date != Sys.Date())
}

#' Announce chat pairings
#'
#' Posts a message to Slack announcing the chat pairs and the language(s) they
#' have in common. People are mentioned so they are notified.
#'
#' @param pairings Data frame. Output of `chats_pairings()`.
#' @param channel Character. Channel to post announcement in.
#' @param dry_run Logical. Show the message without posting it?
#'
#' @returns Timestamp of the posted message.
#'
#' @export
#' @examplesIf interactive()
#' s <- chats_signups()
#' p <- chats_pairings(s)
#' chats_announce_pairs(p, dry_run = TRUE)
#' chats_announce_pairs(p, channel = "testing-api")

chats_announce_pairs <- function(
  pairings,
  channel = "coffee-chats",
  dry_run = FALSE
) {
  if (nrow(pairings) == 0) {
    cli::cli_abort("No pairings to announce", call = NULL)
  }

  perm <- chats_pinned()

  trio <- dplyr::if_else(
    is.na(pairings$id_3),
    "",
    paste0(" & ", pairings$id_3)
  )

  pairs <- glue::glue(
    "• {pairings$id_1} & {pairings$id_2}{trio} ",
    "({pairings$language})"
  ) |>
    glue::glue_collapse(sep = "\n")

  body <- glue::glue(
    "Hello everyone! Here are the pairings for this round of coffee chats :coffee:

  {pairs}

  Please reach out to your partner to find a time to chat in the language(s) you have in common.
  See the [pinned message]({perm}) for more details."
  )

  slack_posts_write(
    when = "now",
    body = body,
    channel = channel,
    dry_run = dry_run
  )
}

chats_languages <- function() {
  c(
    "bee" = "English",
    "hibiscus" = "Spanish",
    "swan" = "French",
    "ant" = "Portuguese"
  )
}


chats_pinned <- function() {
  # Pinned message permalink
  "https://ropensci.slack.com/archives/C0C3TL8T4S3/p1790280924811209"
}
