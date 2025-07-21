#' Create Coworking To-Dos
#'
#' Create a GitHub issue in a repository listing the Coworking ToDos. If no date
#' or timezone, picks the next appropriate date (first Tuesday of the month
#' following an existing coworking issue) and next appropriate timezone (cycling
#' through America, Europe, and Australia) automatically.
#'
#' @param date Character. Date of next event (if NULL picks next first Tuesday).
#' @param tz Character. Timezone of next event (if NULL picks next in order).
#' @param theme Character. Name of theme, if unknown, uses XXXX placeholder
#' @param cohost Character. Name of cohost, if unknown, uses XXXX placeholder
#' @param repo Character. GitHub repository including owner (e.g.,
#'   `rosadmin/comms`)
#'
#' @return Nothing
#'
#' @export
#'
#' @examples
#' cw_issue(dry_run = TRUE)

cw_issue <- function(date = NULL, tz = NULL, theme = "XXXX", cohost = "XXXX",
                     repo = "rosadmin/comms", dry_run = FALSE) {

  # Get last posted Coworking Session
  t <- cw_details(which = "last")

  # Find the date/time of the next
  if(is.null(date)) {
    date <- next_date(t$date)
  } else {
    date <- suppressWarnings(lubridate::as_date(date))
    if(is.na(date)) {
      cli::cli_abort(
        c("Invalid date",
          "*" = "Date must include day of the month.",
          "*" = "Use NULL for automatic assignment"))
    }
  }

  if(is.null(tz)) {
    tz <- which(t$tz == cw_tz()$tz_nice) + 1
    if(tz > 3) tz <- tz - 3
    tz <- cw_tz()$tz_nice[tz]
  }

  # Prepare details
  title <- glue::glue("[Coworking] - {date} - {theme}")
  link <- glue::glue("https://ropensci.org/events/coworking-{format(date, '%Y-%m')}")

  # Fill in issue template with details
  body <- glue::glue(template("cw_checklist"))

  # Create issue
  repo <- gh_split_repo(repo)

  gh_issue_post(title = title, body = body,
                labels = "coworking",
                owner = repo[1], repo = repo[2],
                dry_run = dry_run)
}

#' Create a draft event for coworking
#'
#' Creates a draft coworking event for the roweb3 website. All details pulled
#' from the coworking todo list issue in `rosadmin/comms`. See `cw_issue()`.
#'
#' @param date Character/Date. Date of the coworking event to create
#' @param dry_run Logical. Whether to really create the event or just return the
#'   text.
#'
#' @export
cw_event <- function(date, dry_run = FALSE) {

  # Get issue
  e <- cw_details(which = date)

  # Get details
  details <- cw_times(e) |>
    # Split multiple hosts
    dplyr::mutate(
      cohost = list(stringr::str_split_1(.data$cohost, pattern = "\\band\\b")),
      cohost = purrr::map(.data$cohost, \(x) glue::glue_collapse(x, sep = "\n  -")))

  if(!dry_run) {
    dir <- "./content/events/"
    if(!dir.exists(dir)) {
      cli::cli_abort(c("Directory {dir} doesn't exist",
                       "Are you running this in `roweb3`?"))
    }
    f <- file.path(dir, glue::glue("{lubridate::as_date(details$date)}-{details$slug}.md"))
  } else f <- "DRY RUN"

  yaml <- glue::glue_data(details, template("cw_event_yml"), .sep = "\n")
  body <- glue::glue_data(details, template("cw_event_body"), .sep = "\n")

  e <- paste0(yaml, "\n\n", body)
  if(dry_run) e else writeLines(e, f)

  f
}

cw_times <- function(details) {
  dates <- cw_tz()

  details <- details |>
    dplyr::mutate(
      tz = stringr::str_subset(dates$tz, .data[["tz"]])) |>
    dplyr::left_join(dates, by = "tz")

  if(!details$tz %in% OlsonNames()) {
    cli::cli_abort("`tz` ({tz}) not in `OlsonNames()`")
  }

  details |>
    dplyr::mutate(
      tz_nice = stats::setNames(tz_nice, tz),
      time = stats::setNames(time, tz),
      date = lubridate::ymd_h(paste(date, time), tz = tz),
      date_utc = lubridate::with_tz(date, "UTC"),
      date_utc_end = lubridate::format_ISO8601(date_utc + lubridate::hours(2)),
      date_nice = glue::glue("{format(date, '%A %B %d, %H:00')} ",
                          "{tz_nice[[tz]]} ({format(date_utc, '%H')}:00 UTC)"),
      date_utc = lubridate::format_ISO8601(date_utc),
      slug = glue::glue("coworking-{format(date, '%Y-%m')}"))
}


#' Create a draft post for coworking
#'
#' Creates draft posts for Mastodon and LinkedIn (by opening issues on
#' rosadmin/scheduled_socials) and Slack (by printing the post text and
#' schedule).
#'
#' @param date Character/Date. Date of the coworking event (local)
#' @param who_masto Character. The full mastodon handle for the cohost (i.e. `XXXX@XXXX.com`)
#' @param who_slack Character. The full API Slack id for the cohost (i.e. `<@UXXXXXXX>`)
#' @param who_linkedin Character. The full LinkedIn handle for the cohost (i.e. `@XXXX`)
#' @param who_main_masto Character. The full mastodon handle for the rOpenSci staff organizer.
#' @param who_main_slack Character. The Slack id for the rOpenSci staff
#'   organizer (i.e., `<@UXXXXXXX>`. Defaults to Steffi's id.
#' @param who_main_linkedin Character. The full LinkedIn handle for the rOpenSci staff organizer.
#' @param posters_tz Character. Timezone of poster. Required for getting the
#'   time at which to post Slack messages as these are posted in the local
#'   timezone
#' @param branch Character. Branch name if not on main.
#' @export
#'
#' @examples
#'
#' cw_socials("2023-07-04",
#'            who_masto = "@cohost@mastodon.org",
#'            who_linkedin = "Cohost the Best",
#'            who_slack = "<UXXXXX>",
#'            dry_run = TRUE)
#'
#' \dontrun{
#' cw_socials("2023-07-04", who_masto = "@cohost@mastodon.org", who_slack = "<UXXXXX>")
#' }

cw_socials <- function(date, who_masto, who_slack, who_linkedin,
                       who_main_masto = "@steffilazerte@fosstodon.org",
                       who_main_slack = "<@UNRAUCMTK>",
                       who_main_linkedin = "Steffi LaZerte",
                       posters_tz = "America/Winnipeg",
                       test_run = FALSE, dry_run = FALSE, branch = NULL) {

  if(test_run) dry_run <- TRUE

  i <- gh_cache("/repos/{owner}/{repo}/contents/content/events",
                ref = branch,
                owner = "ropensci", repo = "roweb3")

  event <- dplyr::tibble(name = purrr::map_chr(i, "name"),
                         download_url = purrr::map_chr(i, "download_url")) |>
    dplyr::mutate(date = stringr::str_extract(.data$name, "\\d{4}-\\d{2}-\\d{2}"),
                  date = lubridate::ymd(.data$date)) |>
    dplyr::filter(stringr::str_detect(.data$name, "coworking"),
                  stringr::str_detect(.data$date, .env$date))

  if(nrow(event) > 1) {
    cli::cli_abort(
      c("Detected more than one coworking event with this date:",
      rlang::set_names(event$name, "*")))
  }

  event <- event |>
    dplyr::mutate(content = purrr::map(
      .data$download_url,
      ~{
        x <- readLines(.x, n = 60)
        x[x != ""]
      }
    ),
    yaml_end = purrr::map_dbl(.data$content, \(x) stringr::str_which(x, "---")[2]),
    yaml = purrr::map2(.data$content, .data$yaml_end, \(x, y) x[1:y] |> paste0(collapse = "\n")))

  tz <- stringr::str_extract(
    event$content[[1]],
    glue::glue_collapse(cw_tz()$tz_nice, sep = "|")) |> # Match tz
    stats::na.omit() |>
    cw_tz() # Convert tz to Olson Names

  if(!tz %in% OlsonNames()) cli::cli_abort("Couldn't detect timezone")

  cli::cli_h2("Timezone: {tz}")

  slug <- stringr::str_subset(event$content[[1]], "slug") |>
    stringr::str_extract("coworking-\\d*-\\d*(-\\d*)?")

  deets <- yaml::read_yaml(text = event$yaml[[1]]) |>
    purrr::keep_at(c("title", "dateStart", "date", "title", "author")) |>
    dplyr::as_tibble() |>
    dplyr::summarize(author = paste0(author, collapse = ", "),
                     .by = c("title", "dateStart", "date")) |>
    dplyr::rename("date_UTC" = "dateStart", "theme" = "title") |>
    dplyr::mutate(
      who_masto = .env$who_masto,
      who_slack = .env$who_slack,
      who_linkedin = .env$who_linkedin,
      who_main_masto = .env$who_main_masto,
      who_main_linkedin = .env$who_main_linkedin,
      who_main_slack = .env$who_main_slack,
      author = stringr::str_extract(.data$author, "^[^ ]+"),
      action1 = purrr::map(event$content, ~.x[stringr::str_which(.x, "### Cowork") + 1:2]),
      action1 = purrr::map_chr(.data$action1, ~glue::glue_collapse(.x, sep = "\n")),
      tz = .env$tz,
      tz_txt = cw_tz(.data$tz), # Convert to nice tz
      date_UTC = lubridate::as_datetime(.data$date_UTC),
      date_local = lubridate::with_tz(.data$date_UTC, tz = .env$tz),
      month = lubridate::month(.data$date_local, label = TRUE, abbr = TRUE),
      year = lubridate::year(.data$date_local),
      theme = stringr::str_remove(.data$theme, "Social Coworking and Office Hours - "),
      nth = nth_day(lubridate::day(.data$date)),
      time = paste0(stringr::str_trim(format(.data$date_local, "%A %B")),
                    " ", .data$nth, " ",
                    format(.data$date_local, "%H:00"),
                    " ", .data$tz_txt,
                    " (", format(.data$date_UTC, "%H:00"), " UTC)"),
      event_url = glue::glue("https://ropensci.org/events/{slug}"))

  # Open event for comparison
  utils::browseURL(deets$event_url)

  Sys.sleep(1) # Give time for event to open first

  # Create draft issues for the mastodon posts
  cw_social_week(deets, where = "mastodon", dry_run = dry_run)
  cw_social_hour(deets, where = "mastodon", dry_run = dry_run)
  cw_social_week(deets, where = "linkedin", dry_run = dry_run)
  cw_social_hour(deets, where = "linkedin", dry_run = dry_run)

  # Post slack week before message
  cw_slack_week(deets, posters_tz, test_run, dry_run)
}



cw_social_week <- function(x, where, dry_run) {
  p <- x |>
    dplyr::mutate(
      time_post = .data$date_local - lubridate::weeks(1),
      title = glue::glue("Coworking {month} {year} - week before"),
      who = dplyr::if_else(where == "mastodon", .data$who_masto, .data$who_linkedin),
      who_main = dplyr::if_else(where == "mastodon", .data$who_main_masto, .data$who_main_linkedin),
      body = glue::glue(template("cw_social_week"), .sep = "\n")
    )

  socials_post_issue(time = p$time_post, tz = p$tz, where = where,
                     title = p$title, body = p$body, dry_run = dry_run,
                     over_char_limit = cli::cli_warn)
}

cw_social_hour <- function(x, where, dry_run) {
  p <- x |>
    dplyr::mutate(
      time_post = .data$date_local - lubridate::hours(1),
      title = glue::glue("Coworking {month} {year} - 1-hr before"),
      who = dplyr::if_else(where == "mastodon", .data$who_masto, .data$who_linkedin),
      body = glue::glue(template("cw_social_hour"), .sep = "\n")
    )

  socials_post_issue(time = p$time_post, tz = p$tz, where = where,
                     title = p$title, body = p$body, dry_run = dry_run,
                     over_char_limit = cli::cli_warn)
}

cw_slack_week <- function(x, posters_tz, test_run = FALES, dry_run = FALSE) {

  time_post <- x |>
    dplyr::mutate(
      time_post = .data$date_local - lubridate::weeks(2),
      time_post = lubridate::with_tz(.data$time_post, .env$posters_tz)) |>
    dplyr::pull(time_post)

  body <- glue::glue_data(x, template("cw_slack"), .sep = "\n")

  # Use linkedin handle for Sister Slacks (i.e. Full names)
  body_sister <- glue::glue_data(x, template("cw_slack_sister"), .sep = "\n")

  if(test_run) {
    slack_posts_write(body, when = time_post, tz = posters_tz)
  } else {
    slack_posts_write(body, when = time_post, tz = posters_tz,
                      channel = "#general", dry_run = dry_run)
    slack_posts_write(body, when = time_post, tz = posters_tz,
                      channel = "#co-working", dry_run = dry_run)
  }

  copy(body_sister, "Sister-Slack messages")
  cli::cli_alert_info("Post on {time_post}")

}

#' Schedule 1-hour before messages on rOpenSci Slack
#'
#' Will only work if running between the time that the 1-week message was posted
#' and the start of the coworking.
#'
#' @returns
#' @export
#'
#' @examples
#'
#' \dontrun{
#'   cw_slack_hour(test_run = TRUE)
#'   cw_slack_hour(dry_run = TRUE)
#' }
cw_slack_hour <- function(user = "UNRAUCMTK", test_run = FALSE, dry_run = FALSE,
                          call = rlang::caller_env()) {

  dt <- cw_details() |>
    cw_times() |>
    dplyr::select(date, tz)

  if(is.null(dt$date) | dt$date < Sys.Date()) {
    cli::cli_abort("Either event isn't posted or is passed", call = call)
  }

  msg_link_gen <- cw_slack_msg_link("C026GCWKA", user = user)
  msg_link_co <- cw_slack_msg_link("C0152F1SKAP",  user = user)

  body <- glue::glue(
    "See you in an hour :wink:",
    "",
    "{msg_link_gen}",
    "{msg_link_co}",
    sep = "\n")

  if(test_run) {
    slack_posts_write(body[1], when = dt$date - lubridate::hours(1),
                      tz = dt$tz, channel = "#testing-api")
    slack_posts_write(body[2], when = dt$date - lubridate::hours(1),
                      tz = dt$tz, channel = "#testing-api")
  } else {
    slack_cleanup() # Remove previously scheduled posts from #admin-scheduled
    slack_posts_write(body[1], when = dt$date - lubridate::hours(1),
                      tz = dt$tz, channel = "#general", dry_run = dry_run)
    slack_posts_write(body[2], when = dt$date - lubridate::hours(1),
                      tz = dt$tz, channel = "#co-working", dry_run = dry_run)
  }
}

cw_slack_msg_link <- function(channel_id, user, call = rlang::caller_env()) {


  prev_msgs <- slack_messages(channel_id = channel_id) |>
    dplyr::mutate(
      hour = stringr::str_detect(.data$text, "See you in an hour :wink:"),
      week = stringr::str_detect(.data$text, "Join us for Social Coworking")) |>
    dplyr::filter(.data$user == .env$user,
                  .data$hour | .data$week,
                  .data$time > Sys.Date() - months(1))

  if(prev_msgs$hour[1]) {
    cli_cli_abort("Haven't posted the one-week before announcement yet", call = call)
  }

  # Create message link
  ts <- stringr::str_remove(prev_msgs$ts[prev_msgs$week][1], "\\.")
  glue::glue("https://ropensci.slack.com/archives/{channel_id}/{ts}")
}

#' Fetch details about coworking sessions
#'
#' @param which Character/Date. "next" to fetch details on the next coworking
#'   session, "last" to fetch details on the last scheduled (in future)
#'   coworking session, or a Date fetch details for a specific coworking
#'   session.
#'
#' @return Data frame with coworking event details
#' @export
#' @examples
#' cw_details()
#' # cw_details("2023-11") # Only works for events in the future

cw_details <- function(which = "next") {

  i <- gh_issue_fetch(repo = "comms", labels = "coworking", state = "open")

  d <- data.frame(title = purrr::map_chr(i, "title"),
                  body = purrr::map_chr(i, "body")) |>
    dplyr::mutate(date = stringr::str_extract(.data$title, "\\d{4}-\\d{2}-\\d{2}"))

  if(is.character(which) && which %in% c("last", "next")) {
    if(which == "last") {
      d <- dplyr::arrange(d, dplyr::desc(.data$title))
    } else if(which == "next") {
      d <- dplyr::filter(d, lubridate::ymd(.data$date) >= Sys.Date()) |>
        dplyr::arrange(.data$date)
    }
     d <- dplyr::slice(d, 1)
  } else { # Assume date
    d <- dplyr::filter(d, stringr::str_detect(.data$title, .env$which))
  }

  if(nrow(d) != 1) {
    if(nrow(d) == 0) cli::cli_abort("No details found")
    cli::cli_abort(c("Mulitple event details selected:",
                     rlang::set_names(d$title, "*")))
  }

  dplyr::mutate(
    d,
    tz = stringr::str_extract(.data$body, glue::glue_collapse(cw_tz()$tz_nice, sep = "|")),
    theme = stringr::str_subset(stringr::str_split(.data$body, "\n", simplify = TRUE),
                                "Theme"),
    theme = stringr::str_remove(.data$theme, "\\*\\*Theme\\*\\*: "),
    cohost = stringr::str_subset(stringr::str_split(.data$body, "\n", simplify = TRUE),
                                 "\\*\\*Co-host"),
    cohost = stringr::str_remove(.data$cohost, "\\*\\*Co-host\\*\\*: "))
}


#' Create draft message for checking in with Cohosts
#'
#' The week before, prepare the slides and coworking document, then use this
#' draft text to invite the cohost(s) to review via Slack or Email.
#'
#' @param which Character/Date. "next" to fetch details on the next coworking
#'   session, "last" to fetch details on the last scheduled (in future)
#'   coworking session, or a Date fetch details for a specific coworking
#'   session.
#' @param names Character. Names of cohost if overriding those in the event listing.
#' @param notes_link Character. Link to the Google doc with coworking notes.
#' @param slides_link Character. Link to the Google slides.
#'
#' @export
cw_checkin <- function(which = "next", names = NULL, notes_link, slides_link) {
  if(is.null(which)) cw <- cw_details() else cw <- cw_details(which)
  if(is.null(names)) names <- cw$cohost
  date_nice <- cw_times(cw)$date_nice

  body <- glue::glue(template("cw_checkin"))
  copy(body, "Checkin message")
}

#' Work with Coworking timezones
#'
#' Transform or return coworking timezones.
#'
#' @param tz Character. Timezone to switch the display type of. If NULL, return
#'   all timezones.
#'
#' @returns Character of the opposite timezone display type, or a vector of all
#'   timezones.
#'
#' @examples
#' cw_tz()
#' cw_tz("Americas Pacific")
#' cw_tz("America/Vancouver")
#' cw_tz("Europe Central")

cw_tz <- function(tz = NULL) {
  if(is.null(tz)) {
    return({
      data.frame(
        tz = c("America/Vancouver", "Australia/Perth", "Europe/Paris"),
        time = c(9, 9, 14),
        tz_nice = c("Americas Pacific", "Australian Western", "European Central"))
    })
  }

  dplyr::case_match(
    tz,
    "Americas Pacific" ~ "America/Vancouver",
    "America/Vancouver" ~ "Americas Pacific",
    "Europe Central" ~ "Europe/Paris",
    "Europe/Paris" ~ "Europe Central",
    "Australia West" ~ "Australia/Perth",
    "Australia/Perth" ~ "Australia West"
  )
}

