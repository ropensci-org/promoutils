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
  slack_react(ts, c("bee", "hibiscus", "swan", "ant"), channel = channel)
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
