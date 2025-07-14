#' Write Slack message
#'
#' Write a Slack message for posting now or later.
#'
#' See https://docs.slack.dev/messaging/formatting-message-text#special-mentions
#' https://docs.slack.dev/messaging/formatting-message-text#mentioning-users
#'
#' @param body Character. Text of message to post.
#' @param when Character or Date/time. When to post message.
#' @param where Character. Channel to post message to.
#' @param dry_run Logical. Test run?
#'
#' @returns Success message
#' @export
#'
#' @references
#'   - https://docs.slack.dev/messaging/sending-and-scheduling-messages
#'   - https://docs.slack.dev/messaging/sending-and-scheduling-messages#scheduling
#'
#' @examples
#' slack_posts_write("testing on Tuesday")
#' slack_posts_write("testing more and more", when = "2025-05-22 14:00:00", tz = "Europe/Paris")
#' slack_posts_write(
#'   paste(
#'   "Join us for Social Coworking and office hours next week!",
#'   "",
#'   ":grey_exclamation: Theme: TESTING",
#'   ":hourglass_flowing_sand: When: TODAY!",
#'   ":cookie: Hosted by: USER! and community host HOST",
#'   "",
#'   "You can use this time for...",
#'   "- General coworking", sep = "\n"), when = "now")
slack_posts_write <- function(body, when = "now", tz = "America/Winnipeg",
                              channel = "#testing-api", dry_run = FALSE) {

  if(is.character(when) && when == "now") {
    end <- "chat.postMessage"
    post_at <- NULL
    type <- "posted"
  } else {
    end <- "chat.scheduleMessage"
    when <- lubridate::as_datetime(when, tz = tz) |> lubridate::round_date()
    post_at <- as.integer(when)
    type <- "scheduled"
  }

  r <- httr2::request("https://slack.com/api/") |>
    httr2::req_url_path_append(end) |>
    httr2::req_body_json(list(channel = channel, text = body, post_at = post_at)) |>
    slack_auth() |>
    httr2::req_perform() |>
    slack_check(msg = paste("Slack message", type, "successfully to", channel))

  if(type == "scheduled") {

    # Post to #admin-scheduled as a quick check record
    body2 <- paste0(
      "-------------------------\n",
      ":clock1: SCHEDULED FOR: ", lubridate::with_tz(when, Sys.timezone()), " ", Sys.timezone(), "\n",
      ":bookmark: CHANNEL: ", channel, "\n",
      ":id: MESSAGE ID: ", r$scheduled_message_id, "\n",
      "\n> ", stringr::str_replace_all(body, "\\\n", "\n>"),
      "\n-------------------------")
    r2 <- httr2::request("https://slack.com/api/") |>
      httr2::req_url_path_append("chat.postMessage") |>
      httr2::req_body_json(list(channel = "#admin-scheduled", text = body2)) |>
      slack_auth() |>
      httr2::req_perform() |>
      slack_check(msg = "Scheduled message successfully added to #admin-scheduled")
  }
}

#' Title
#'
#' @returns
#' @export
#'
#' @examples
#' slack_scheduled_list()

slack_scheduled_list <- function() {

  r_list <- httr2::request("https://slack.com/api/chat.scheduledMessages.list") |>
    slack_auth() |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    purrr::pluck("scheduled_messages") |>
    purrr::map(dplyr::as_tibble) |>
    purrr::list_rbind()

  cols <- c("channel", "scheduled_local", "text", "date_created_dt",
  "channel_id", "id", "post_at", "date_created")

  if(nrow(r_list) == 0) {
    r_list <- dplyr::tibble(x = cols, y = NA) |>
      tidyr::pivot_wider(names_from = x, values_from = y) |>
      dplyr::as_tibble(.rows = 0)
    return(r_list)
  }

  r_list |>
    dplyr::mutate(
      scheduled = lubridate::as_datetime(.data$post_at),
      scheduled_local = lubridate::with_tz(.data$scheduled, Sys.timezone()),
      date_created_dt = lubridate::as_datetime(.data$date_created)) |>
    dplyr::mutate(channel = purrr::map_chr(
      .data$channel_id, \(x) slack_channel(channel_id = x)$channel)) |>
    dplyr::select(dplyr::all_of(cols))
}

#' Delete a scheduled message
#'
#' @param msg Data frame. Output of `slack_list_scheduled()` containing messages
#'   to remove. Should contain columns "channel" and "id"
#' @param channel Character. If no msg, the Channel of the message to be
#'   deleted.
#' @param id Character. If no msg, the ID of the message to be deleted.
#'
#' @returns
#' @export
#'
#' @examples
#' # Schedule message
#' slack_posts_write("Testing delete msg", when = (Sys.Date() + lubridate::days(2)))
#'
#' # Confirm scheduled
#' slack_scheduled_list()
#'
#' # Remove message
#' slack_scheduled_list() |>
#'   dplyr::filter(text == "Testing delete msg") |>
#'   slack_scheduled_rm()
#'
#' # Confirm that removed
#' slack_scheduled_list()
#'
#' \dontrun{
#' slack_scheduled_rm(channel = "#testing-api", id = "Q08U4S3J6QG")
#' }

slack_scheduled_rm <- function(msg = NULL, channel = NULL, id = NULL) {

  if(!is.null(msg)) {
    if(nrow(msg) == 0) {
      rlang::inform("No messages to remove")
      return(invisible())
    }
    channel <- msg$channel
    id <- msg$id
  }

  purrr::walk2(channel, id, \(c, i) {
    r <- httr2::request("https://slack.com/api/chat.deleteScheduledMessage") |>
      httr2::req_body_json(list(channel = c, scheduled_message_id = i)) |>
      slack_auth() |>
      httr2::req_perform() |>
      slack_check(
        msg = paste("Message", i, "successfully removed from scheduled queue for", c))

    admin_ts <- slack_messages(channel_id = slack_admin())|>
      dplyr::filter(stringr::str_detect(.data$text, i)) |>
      dplyr::pull(ts)

    if(length(admin_ts) > 0) slack_message_rm(slack_admin(), admin_ts)
  })
}

#' Clean up old scheduled messages
#'
#' Removes previously scheduled messages from `#admin-scheduled` if after the
#' posting date.
#'
#' @returns
#' @export
#'
#' @examples
#'
#' slack_posts_write("testing cleanup",
#'                   when = Sys.time() + lubridate::seconds(600),
#'                   tz = Sys.timezone())
#' slack_scheduled_list()
#'
#' slack_cleanup()
slack_cleanup <- function() {

  sched <- slack_scheduled_list()
  admin <- slack_messages(channel_id = slack_admin()) |>
    dplyr::mutate(id = stringr::str_extract(
      .data$text, "(?<=MESSAGE ID: )[\\S]+(?=\\n)")) |>
    tidyr::drop_na("id") # Ignore non-scheduled messages

  if(nrow(sched) == 0 & nrow(admin) == 0) {
    rlang::inform("Nothing to clean up")
    return(invisible())
  }

  # If none scheduled, remove all from admin-scheduled
  if(nrow(sched) == 0 & nrow(admin) > 0) {
    purrr::map(admin$ts, \(x) slack_message_rm(channel_id = slack_admin(), ts = x))
  }

  # Otherwise remove any that are not currently scheduled
  admin_rm <- dplyr::anti_join(admin, sched, by = c("id"))
  if(nrow(admin_rm) > 0) {
    purrr::map(admin_rm$ts, \(x) slack_message_rm(channel_id = slack_admin(), ts = x))
  }

  rlang::inform("Channel #admin-scheduled cleaned up")
}



#' Title
#'
#' @param channel
#'
#' @returns
#' @export
#'
#' @examples
#' slack_channels()
#' slack_channels("general")
#' chn <- slack_channels(types = "private_channel")
slack_channels <- function(types = c("public_channel", "private_channel")) {
  types <- paste(types, collapse = ",")
  r <- httr2::request("https://slack.com/api/conversations.list") |>
    httr2::req_url_query(types = types, exclude_archived = TRUE) |>
    slack_auth() |>
    slack_paginate() |>
    slack_check(element = "channels", paginate = TRUE)

  slack_df(r, "channels", c("name", "id")) |>
    dplyr::rename("channel" = "name", "channel_id" = "id")
}

slack_channel <- function(channel = NULL, channel_id = NULL, channels = NULL) {

  if(is.null(channels)) channels <- slack_channels()

  if(!is.null(channel)) {
    chn <- dplyr::filter(
      channels,
      stringr::str_detect(tolower(.data$channel), tolower(.env$channel)))
  } else if(!is.null(channel_id)) {
    chn <- dplyr::filter(
      channels,
      .data$channel_id == .env$channel_id)
  }
  chn
}




#' Fetch details on a specific users
#'
#' @param name Character. String to match real name to
#' @param users Data frame. Data frame of users from `slack_users()`.
#'
#' @returns Data frame
#' @export
#'
#' @examples
#' slack_user("Steffi")

slack_user <- function(name, users = NULL) {

  if(is.null(users)) users <- slack_users()

  dplyr::filter(
    users,
    stringr::str_detect(tolower(.data$real_name), tolower(.env$name)))
}

#' Fetch a list of Slack users
#'
#' @returns
#' @export
#'
#' @examples
#' u <- slack_users()

slack_users <- function() {
  httr2::request("https://slack.com/api/users.list") |>
    slack_auth() |>
    slack_paginate() |>
    slack_check(element = "members", paginate = TRUE) |>
    slack_df("members", c("id", "name", "real_name", "deleted")) |>
    dplyr::filter(!deleted)
}

#' Get the last 100 messages from a channel
#'
#' @returns
#' @export
#'
#' @examples
#' slack_messages(channel_id = "C026GCWKA") # General
slack_messages <- function(channel = NULL, channel_id = NULL) {

  if(is.null(channel_id) & !is.null(channel)) channel_id <- slack_channel(channel)

  httr2::request("https://slack.com/api/conversations.history") |>
    slack_auth() |>
    httr2::req_url_query(channel = channel_id) |>
    httr2::req_perform() |>
    slack_check(msg = NULL) |>
    slack_df("messages", c("ts", "user", "text")) |>
    dplyr::mutate(time = lubridate::as_datetime(as.numeric(ts)),
                  time = lubridate::with_tz(time, tz = Sys.timezone())) |>
    dplyr::relocate(time, .after = "ts")
}

slack_message_rm <- function(channel_id, ts) {
  httr2::request("https://slack.com/api/chat.delete") |>
    httr2::req_body_json(list(channel = channel_id, ts = ts)) |>
    slack_auth() |>
    httr2::req_perform() |>
    slack_check(
      msg = paste("Message", ts, "successfully removed from ", channel_id))
}


