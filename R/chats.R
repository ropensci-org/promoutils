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
