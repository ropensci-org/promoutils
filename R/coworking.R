#' Create Coworking To-Do's on Comms Repo
#'
#' Create an issue listing the coworking todos. If no date or timezone, picks
#' the next appropriate date (first Tuesday in the month following an existing
#' coworking issue) and next appropriate timezone (cycling through America,
#' Europe, and Australia) automatically.
#'
#' @param date Character. Date of next event (if NULL picks next first Tuesday).
#' @param tz Character. Timezone of next event (if NULL picks next in order).
#' @param theme Character. Name of theme, if unknown, uses XXXX placeholder
#' @param cohost Character. Name of cohost, if unknown, uses XXXX placeholder
#' @param dry_run Logical. Whether to do a dry run (i.e. don't post)
#'
#' @export
cw_issue <- function(date = NULL, tz = NULL, theme = "XXXX", cohost = "XXXX",
                     dry_run = FALSE) {

  t <- cw_details(which = "last")

  if(is.null(date)) {
    date <- next_date(t$date)
  } else {
    date <- suppressWarnings(lubridate::as_date(date))
    if(is.na(date)) {
      stop("Invalid data, must be a full date down to the day, ",
           "or do not supply date and let the function find the next j",
           "one for you", call. = FALSE)
    }
  }

  if(is.null(tz)) {
    tz <- which(t$tz == c("Australia", "Europe", "America")) + 1
    if(tz > 3) tz <- tz - 3
    tz <- c("Australia", "Europe", "America")[tz]
  }
  title <- glue::glue("[Coworking] - {date} - {theme}")
  link <- glue::glue("https://ropensci.org/events/coworking-{format(date, '%Y-%m')}")

  # Get issue template from repository and fill in
  body <- gh::gh("/repos/rosadmin/comms/contents/.github/ISSUE_TEMPLATE/coworking-prep.md",
              .accept = "application/vnd.github.raw+json") |>
    unlist() |>
    stringr::str_remove("^---(\\S|\\s)+---\\n?\\n?") |> # Remove YAML
    stringr::str_remove_all("\\[[^\\[\\]]+\\]") |>      # Remove comments
    glue::glue()                                        # Add details

  # Create issue
  gh_issue_post(title = title, body = body,
                labels = "coworking",
                owner = "rosadmin", repo = "comms",
                dry_run = dry_run)

}

#' Create a draft event for coworking
#'
#' Creates a draft coworking event for the roweb3 website. All details pulled
#' from the coworking todo list issue in `rosadmin/comms`.
#'
#' @param date Character/Date. Date of the coworking event (local)
#' @param dry_run Logical. Whether to really create the event or just return the
#'   text.
#'
#' @export
cw_event <- function(date, dry_run = FALSE) {

  # Get issue
  e <- cw_details(which = date)

  # Get details
  details <- cw_times(e)

  if(!dry_run) {
    dir <- "./content/events/"
    if(!dir.exists(dir)) {
      stop("Directory ", dir, " doesn't exist. ",
           "Are you running this in `roweb3`?", call. = FALSE)
    }
    f <- file.path(dir, glue::glue("{lubridate::as_date(details$date)}-{details$slug}.md"))
  } else f <- "DRY RUN"

  yaml <- glue::glue_data(
    details,
    .sep = "\n",
    "---",
    "title: Social Coworking and Office Hours - {theme}",
    "dateStart: {date_utc} # UTC!!",
    "dateEnd: {date_utc_end} # UTC!!",
    "date: {date_utc_end} # UTC!! same as dateEnd",
    "description: Monthly coworking for productivity, asking questions, socializing",
    "coworking: true",
    "location: 'online' # free text",
    "slug: \"{slug}\"",
    "country: \"\U0001F310\" # emoji",
    "ropensci: yes",
    "outputs:",
    "  - HTML",
    "  - Calendar",
    "attendees:",
    "  - {stringr::str_split_1(details$cohost, pattern = 'and') |> paste0(collapse = '\n  -')}",
    "  - Steffi LaZerte",
    "author:",
    "  - {stringr::str_split_1(details$cohost, pattern = 'and') |> paste0(collapse = '\n  -')}",
    "deets: |",
    "    Meeting ID: 913 2825 6625",
    "    Passcode: 512767",
    "zoomurl: https://zoom.us/j/91328256625?pwd=WGVDdWpGdnhWWTFvZkZVTkNzWElNQT09",
    "---")

  time_check <- glue::glue_data(
    details,
    .sep = "\n",
    "<!--",
    "```{{r}}",
    "d <- lubridate::ymd_hms('{date}', tz = '{tz}')",
    "lubridate::with_tz(d, 'UTC')",
    "lubridate::with_tz(d, 'America/Winnipeg')",
    "```",
    "-->")

  body <- glue::glue_data(
    details,
    .sep = "\n",
    "**Join us for 2 hours {date_nice} for ",
    "[Social Coworking + Office Hours](/blog/2023/06/21/coworking/)**",
    "",
    "This month we're focusing on the theme **{theme}** ",
    "with community host **{cohost}**, DESCRIPTION OF HOST.",
    "",
    "Come for the full 2 hrs or only as long as you need!",
    "",
    "### Cowork",
    "",
    "- THEME WORK",
    "- THEME WORK/LEARN",
    "- Cowork independently on work related to R",
    "    - Plan out that package you’ve always wanted to create",
    "    - Work on packages that tend to be neglected",
    "    - What ever you need to get done!",
    "",
    "### Socialize/Network",
    "",
    "- Meet community host, **{cohost}**, and discuss THEME.",
    "- Meet other R users and rOpenSci staff in Zoom",
    "- Get answers to your questions",
    "    - Ask your hosts",
    "    - Ask your fellow coworkers",
    "    - Discuss your work, best practices, or get advice and resources",
    "- Answer other coworkers' questions!",
    "",
    "We host ",
    "[Social Coworking + Office Hours](/blog/2023/06/21/coworking/) ",
    "on the first Tuesday of each month, alternating among timezones to ",
    "accommodate different parts of the world."
  )

  e <- paste0(yaml, "\n\n", time_check, "\n\n", body)
  if(dry_run) e else writeLines(e, f)

  f
}

cw_times <- function(details) {
  dates <- data.frame(
    tz = c("America/Vancouver", "Australia/Perth", "Europe/Paris"),
    time = c(9, 9, 14),
    tz_nice = c("Americas Pacific", "Australian Western", "European Central"))

  details <- details |>
    dplyr::mutate(
      tz = stringr::str_subset(dates$tz, .data[["tz"]])) |>
    dplyr::left_join(dates, by = "tz")

  if(!details$tz %in% OlsonNames()) {
    stop("`tz` (", tz, ") not in `OlsonNames()`", call. = FALSE)
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
#' @param who_masto Character. The full mastodon handle for the cohost (i.e. XXXX@XXXX.com)
#' @param who_slack Character. The full Slack handle for the cohost (i.e. @XXXX)
#' @param who_linkedin Character. The full LinkedIn handle for the cohost (i.e. @XXXX)
#' @param who_main_masto Character. The full mastodon handle for the rOpenSci staff organizer.
#' @param who_main_slack Character. The Slack id for the rOpenSci staff
#'   organizer (i.e., `<@UXXXXXXX>`. Defaults to Steffi's id.
#' @param who_main_linkedin Character. The full LinkedIn handle for the rOpenSci staff organizer.
#' @param posters_tz Character. Timezone of poster. Required for getting the
#'   time at which to post Slack messages as these are posted in the local
#'   timezone
#' @param dry_run Logical. Whether to do a dry run (i.e. don't post)
#' @param branch Character. Branch name if not on main.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' cw_socials("2023-07-04", who_masto = "@cohost@mastodon.org", who_slack = "<UXXXXX>")
#' }

cw_socials <- function(date, who_masto, who_slack, who_linkedin,
                       who_main_masto = "@steffilazerte@fosstodon.org",
                       who_main_slack = "<@UNRAUCMTK>",
                       who_main_linkedin = "Steffi LaZerte",
                       posters_tz = "America/Winnipeg", dry_run = FALSE,
                       branch = NULL) {

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
    stop("Detected more than one coworking event with this date:\n",
         paste0(event$name, collapse = "\n"), call. = FALSE)
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

  tz <- stringr::str_extract(event$content[[1]], "(America/Vancouver)|(Europe/Paris)|(Australia/Perth)") |>
    stats::na.omit()

  if(!tz %in% OlsonNames()) stop("Couldn't detect timezone", call. = FALSE)

  message("Timezone: ", tz)

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
      tz_txt = dplyr::case_when(.data$tz == "America/Vancouver" ~ "Americas Pacific",
                                .data$tz == "Europe/Paris" ~ "European Central",
                                .data$tz == "Australia/Perth" ~ "Australian Western"),
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
  cw_slack_week(deets, posters_tz, dry_run)
}



cw_social_week <- function(x, where, dry_run) {
  p <- x |>
    dplyr::mutate(
      time_post = .data$date_local - lubridate::weeks(1),
      title = glue::glue("Coworking {month} {year} - week before"),
      who = dplyr::if_else(where == "mastodon", .data$who_masto, .data$who_linkedin),
      who_main = dplyr::if_else(where == "mastodon", .data$who_main_masto, .data$who_main_linkedin),
      body = glue::glue(
        "Coworking and Office Hours next week!",
        "",
        "Theme: {theme}",
        "",
        "{time}",
        "",
        "Join {who} and {who_main}",
        "",
        "- General coworking",
        "{action1}",
        "- Chat with {author} and other attendees and discuss our theme!",
        "",
        "{event_url}",
        .sep = "\n"
      ))

  socials_post_issue(time = p$time_post, tz = p$tz, where = where,
                     title = p$title, body = p$body, dry_run = dry_run,
                     over_char_limit = warning)
}

cw_social_hour <- function(x, where, dry_run) {
  p <- x |>
    dplyr::mutate(
      time_post = .data$date_local - lubridate::hours(1),
      title = glue::glue("Coworking {month} {year} - 1-hr before"),
      who = dplyr::if_else(where == "mastodon", .data$who_masto, .data$who_linkedin),
      body = glue::glue(
        "rOpenSci Coworking and Office Hours coming up in an hour!",
        "",
        "Today's Theme: {theme} with cohost {who}",
        "",
        "{time}",
        "",
        "{event_url}",
        .sep = "\n"
      )
    )
  socials_post_issue(time = p$time_post, tz = p$tz, where = where,
                     title = p$title, body = p$body, dry_run = dry_run,
                     over_char_limit = warning)
}

cw_slack_week <- function(x, posters_tz, dry_run = FALSE) {

  time_post <- x |>
    dplyr::mutate(
      time_post = .data$date_local - lubridate::weeks(1),
      time_post = lubridate::with_tz(.data$time_post, .env$posters_tz)) |>
    dplyr::pull(time_post)

  body <- x |>
    glue::glue_data(
      #"[SLACK WEEK BEFORE: #general & #co-working]",
      #"[POST AT: {time_post}]",
      #"\n",
      "Join us for Social Coworking and office hours next week!",
      "",
      ":grey_exclamation: Theme: {theme}",
      ":hourglass_flowing_sand: When: {time}",
      ":cookie: Hosted by: {who_main_slack} and community host {who_slack}",
      "",
      "You can use this time for...",
      "- General coworking",
      "{action1}",
      "- Chat with others for advice/resources",
      "",
      "{event_url}",
      .sep = "\n") |>
    fmt_slack()

  if(dry_run) {
    slack_posts_write(body, when = time_post, tz = posters_tz)
  } else {
    slack_posts_write(body, when = time_post, tz = posters_tz, channel = "#general")
    slack_posts_write(body, when = time_post, tz = posters_tz, channel = "#co-working")
  }
}

fmt_slack <- function(body) {
 stringr::str_replace_all(
   body, "\\[(.+)\\]\\((.+)\\)", "<\\2|\\1>")
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
#' \dontrun{
#'   cw_slack_hour(dry_run = TRUE)
#' }
cw_slack_hour <- function(user = "UNRAUCMTK", dry_run = FALSE) {

  dt <- cw_details() |>
    cw_times() |>
    dplyr::select(date, tz)

  if(is.null(dt$date) | dt$date < Sys.Date()) {
    rlang::abort("Either event isn't posted or is passed", call = NULL)
  }

  msg_link_gen <- cw_slack_msg_link("C026GCWKA", user = user)
  msg_link_co <- cw_slack_msg_link("C0152F1SKAP",  user = user)

  body <- paste(
    "See you in an hour :wink:",
    "",
    c(msg_link_gen, msg_link_co),
    sep = "\n")

  if(dry_run) {
    slack_posts_write(body[1], when = dt$date - lubridate::hours(1),
                      tz = dt$tz, channel = "#testing-api")
    slack_posts_write(body[2], when = dt$date - lubridate::hours(1),
                      tz = dt$tz, channel = "#testing-api")
  } else {
    slack_cleanup() # Remove previously scheduled posts from #admin-scheduled
    slack_posts_write(body[1], when = dt$date - lubridate::hours(1),
                      tz = dt$tz, channel = "#general")
    slack_posts_write(body[2], when = dt$date - lubridate::hours(1),
                      tz = dt$tz, channel = "#co-working")
  }
}

cw_slack_msg_link <- function(channel_id, user) {


  prev_msgs <- slack_messages(channel_id = channel_id) |>
    dplyr::mutate(
      hour = stringr::str_detect(.data$text, "See you in an hour :wink:"),
      week = stringr::str_detect(.data$text, "Join us for Social Coworking")) |>
    dplyr::filter(.data$user == .env$user,
                  .data$hour | .data$week,
                  .data$time > Sys.Date() - months(1))

  if(prev_msgs$hour[1]) {
    rlang::abort("Haven't posted the one-week before announcement yet", call = NULL)
  }

  # Create message link
  paste0(
    "https://ropensci.slack.com/archives/", channel_id, "/",
    stringr::str_remove(prev_msgs$ts[prev_msgs$week][1], "\\."))
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
#' cw_details("2023-11")

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
    if(nrow(d) == 0) stop("No details found", call. = FALSE)
    stop("Mulitple event details selected:\n",
         paste0(d$title, collapse = "\n"), call. = FALSE)
  }

  dplyr::mutate(
    d,
    tz = stringr::str_extract(.data$body, "America|Europe|Australia"),
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
  times <- cw_times(cw)

  glue::glue(
    "Hi {names}!

I'm excited for coworking with you next week :tada:  ({times$date_nice})

I've shared two Google docs with you

1. The [coworking document]({notes_link}) - Feel free to add items to the \"Shared\" section, or anywhere else (especially any links you think might be relevant to get started with).
2. And the [Slides]({slides_link}) - Please fill out the \"{names}'s Spot\" introduction slide (and change the name if you have a different preferred name). If you'd like some inspiration, checkout the other \"Spot\" slides, but any format is good!

Remember that you can always checkout our [Coworking Blog post](https://ropensci.org/blog/2023/06/21/coworking/) for a quick run down of how things work, and of course, ask me any questions you have!

I'll be advertising through rOpenSci channels and feel free to boost our posts or add your own to spread the word into your own networks. Thanks again for your time!") |>
    cat()
}
