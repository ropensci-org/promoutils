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
#'
#' @export
cw_issue <- function(date = NULL, tz = NULL, theme = "XXXX", cohost = "XXXX") {

  t <- get_details(i, type = "next")

  if(is.null(date)) date <- next_tues(t$date)
  if(is.null(tz)) {
    tz <- which(t$tz == c("Australia", "Europe", "America")) + 1
    if(tz > 3) tz <- tz - 3
    tz <- c("America", "Europe", "Australia")[tz]
  }
  title <- glue::glue("[Coworking] - {date} - {theme}")

  body <- glue::glue(
    "**Theme**: {theme}",
    "**Co-host**: {cohost}",
    "**Timezone**: {tz}",
    "**Link**: https://ropensci.org/events/coworking-{format(date, '%Y-%m')}",
    "",
    cw_issue_body, .sep = "\n")

  gh_issue_post(title = title, body = body,
                labels = "coworking",
                owner = "rosadmin", repo = "comms")

}

#' Create a draft event for coworking
#'
#' Creates a draft coworking event for the roweb3 website
#'
#' @param date Character/Date. Date of the coworking event (local)
#' @param tz Character. Timezone of the event (match from `OlsonNames()`)
#' @param theme Character. Theme.
#' @param cohost Character. Name of cohost
#'
#' @export
cw_event <- function(date) {

  # Get issue
  e <- cw_details(type = "date", date = date)

  # Get details
  date <- e$date
  tz <- e$tz
  theme <- e$theme
  cohost <- e$cohost

  dir <- "./content/events/"
  if(!dir.exists(dir)) {
    stop("Directory ", dir, " doesn't exist. ",
         "Are you running this in `roweb3`?", call. = FALSE)
  }

  details <- data.frame(
    tz = c("America/Vancouver", "Australia/Perth", "Europe/Paris"),
    time = c(9, 9, 14),
    tz_nice = c("Americas Pacific", "Australian Western", "European Central"))

  tz <- stringr::str_subset(details$tz, tz)

  if(!tz %in% OlsonNames()) {
    stop("`tz` (", tz, ") not in `OlsonNames()`", call. = FALSE)
  }


  tz_nice <- setNames(details$tz_nice, details$tz)
  times <- setNames(details$time, details$tz)


  date <- lubridate::ymd_h(paste(date, times[[tz]]), tz = tz)
  date_utc <- lubridate::with_tz(date, "UTC")
  date_utc_end <- lubridate::format_ISO8601(date_utc + lubridate::hours(2))

  date_nice <- glue::glue("{format(date, '%A %B %d, %H:00')} ",
                          "{tz_nice[[tz]]} ({format(date_utc, '%H')}:00 UTC)")

  date_utc <- lubridate::format_ISO8601(date_utc)

  slug <- glue::glue("coworking-{format(date, '%Y-%m')}")

  f <- file.path(dir, glue::glue("{lubridate::as_date(date)}-{slug}.md"))

  yaml <- glue::glue(
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
    "  - {cohost}",
    "  - Steffi LaZerte",
    "author:",
    "  - {cohost}",
    "deets: |",
    "    Meeting ID: 913 2825 6625",
    "    Passcode: 512767",
    "zoomurl: https://zoom.us/j/91328256625?pwd=WGVDdWpGdnhWWTFvZkZVTkNzWElNQT09",
    "---")

  time_check <- glue::glue(
    .sep = "\n",
    "<!--",
    "```{{r}}",
    "d <- lubridate::ymd_hms('{date}', tz = '{tz}')",
    "lubridate::with_tz(d, 'UTC')",
    "lubridate::with_tz(d, 'America/Winnipeg')",
    "```",
    "-->")

  body <- glue::glue(
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


  writeLines(paste0(yaml, "\n\n", time_check, "\n\n", body), f)

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
#' @param when Character. Is this a "week" before reminder or an "hour" before reminder?
#' @param posters_tz Character. Timezone of poster. Required for getting the
#'   time at which to post Slack messages as these are posted in the local
#'   timezone
#' @export
#'
#' @examples
#'
#' \dontrun{
#' cw_socials("2023-07-04", who_masto = "@cohost@mastodon.org", who_slack = "@cohost")
#' }

cw_socials <- function(date, who_masto, who_slack, who_linkedin,
                           posters_tz = "America/Winnipeg") {

  i <- gh::gh("/repos/{owner}/{repo}/contents/content/events",
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
    yaml = purrr::map(.data$content, ~.x[1:20] |> paste0(collapse = "\n")))

  tz <- stringr::str_extract(event$content[[1]], "(America/Vancouver)|(Europe/Paris)|(Australia/Perth)") |>
    na.omit()

  if(!tz %in% OlsonNames()) stop("Couldn't detect timezone", call. = FALSE)

  message("Timezone: ", tz)

  slug <- stringr::str_subset(event$content[[1]], "slug") |>
    stringr::str_extract("coworking-\\d*-\\d*(-\\d*)?")

  deets <- yaml::read_yaml(text = event$yaml[[1]]) |>
    purrr::keep_at(c("title", "dateStart", "date", "title", "author")) |>
    dplyr::as_tibble() |>
    dplyr::rename("date_UTC" = "dateStart", "theme" = "title") |>
    dplyr::mutate(
      who_masto = .env$who_masto,
      who_slack = .env$who_slack,
      who_linkedin = .env$who_linkedin,
      author = stringr::str_extract(.data$author, "^[^ ]+"),
      action1 = purrr::map(event$content, ~.x[stringr::str_which(.x, "### Cowork") + 1:2]),
      action1 = purrr::map_chr(.data$action1, ~glue::glue_collapse(.x, sep = "\n")),
      tz = .env$tz,
      tz_txt = dplyr::case_when(.data$tz == "America/Vancouver" ~ "Americas Pacific",
                                .data$tz == "Europe/Paris" ~ "European Central",
                                .data$tz == "Australia/Perth" ~ "Australian Western"),
      date_UTC = lubridate::as_datetime(.data$date_UTC),
      date_local = lubridate::with_tz(.data$date_UTC, tz = .data$tz),
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
  browseURL(deets$event_url)

  Sys.sleep(1) # Give time for event to open first

  # Create draft issues for the mastodon posts
  social_week(deets, where = "mastodon")
  social_hour(deets, where = "mastodon")
  social_week(deets, where = "linkedin")
  social_hour(deets, where = "linkedin")

  # Create draft text for slack messages
  glue::glue(slack_week(deets, posters_tz),
             "\n\n---------\n\n",
             slack_hour(deets, posters_tz))
}



social_week <- function(x, where) {
  p <- x |>
    dplyr::mutate(
      time_post = .data$date_local - lubridate::weeks(1),
      title = glue::glue("Coworking {month} {year} - week before"),
      who = if_else(where == "mastodon", who_masto, who_linkedin),
      body = glue::glue(
        "Coworking and Office Hours next week!",
        "",
        "Theme: {theme}",
        "",
        "{time}",
        "",
        "Join {who} and @steffilazerte@fosstodon.org",
        "",
        "- General coworking",
        "{action1}",
        "- Chat with {author} and other attendees and discuss strategies for XXXX",
        "",
        "{event_url}",
        .sep = "\n",
      ))

  post_issue(time = p$time_post, tz = p$tz, where = where, title = p$title, body = p$body)
}

masto_hour <- function(x) {
  p <- x |>
    dplyr::mutate(
      time_post = .data$date_local - lubridate::hours(1),
      title = glue::glue("Coworking {month} {year} - 1-hr before"),
      body = glue::glue(
        "rOpenSci Coworking and Office Hours coming up in an hour!",
        "",
        "Today's Theme: {theme} with cohost {who_masto}",
        "",
        "{time}",
        "",
        "{event_url}",
        .sep = "\n"
      )
    )
  post_issue(time = p$time_post, tz = p$tz, title = p$title, body = p$body)
}

slack_week <- function(x, posters_tz) {
  x |>
    dplyr::mutate(time_post = .data$date_local - lubridate::weeks(1),
                  time_post = lubridate::with_tz(.data$time_post, .env$posters_tz)) |>
    glue::glue_data(
      "[SLACK WEEK BEFORE: #general & #co-working]",
      "[POST AT: {time_post}]",
      "\n",
      "Join us for Social Coworking and office hours next week!",
      "",
      ":grey_exclamation: Theme: {theme}",
      ":hourglass_flowing_sand: When: {time}",
      ":cookie: Hosted by: @Steffi LaZerte and community host {who_slack}",
      "",
      "You can use this time for...",
      "- General coworking",
      "{action1}",
      "- Chat with others for advice/resources",
      "",
      "{event_url}",
      .sep = "\n")
}

slack_hour <- function(x, posters_tz) {
  x |>
    dplyr::mutate(time_post = .data$date_local - lubridate::hours(1),
                  time_post = lubridate::with_tz(.data$time_post, .env$posters_tz)) |>
    glue::glue_data(
      "[SLACK HOUR BEFORE: #general]",
      "[POST AT: {time_post}]",
      "\n",
      "See you in an hour :wink:",
      "",
      "URL TO ORIGINAL SLACK POST!",
      .sep = "\n")
}


# Given a date return the next month's first Tuesday
next_tues <- function(month) {
  month <- lubridate::as_date(month) + lubridate::period("1 month")

  month |>
    lubridate::floor_date(unit = "months") |>
    lubridate::ceiling_date(unit = "weeks", week_start = "Tues")
}


cw_details <- function(type, ...) {

  args <- list(...) # date

  i <- gh_issue_get(repo = "comms", labels = "coworking", state = "all")

  d <- data.frame(t = purrr::map_chr(i, "title"),
                  b = purrr::map_chr(i, "body"))

  if(type == "next") {
    d <- d |>
      dplyr::arrange(dplyr::desc(t)) |>
      dplyr::slice(1)
  } else if (type == "date") {
    d <- dplyr::filter(d, stringr::str_detect(t, args[["date"]]))
  }

  if(nrow(d) != 1) {
    if(nrow(d) == 0) stop("No details found", call. = FALSE)
    stop("Mulitple event details selected:\n", paste0(d$t, collapse = "\n"), call. = FALSE)
  }

  dplyr::mutate(
    d,
    date = stringr::str_extract(t, "\\d{4}-\\d{2}-\\d{2}"),
    tz = stringr::str_extract(b, "America|Europe|Australia"),
    #b = stringr::str_split(b, "\r\n", simplify = TRUE),
    theme = stringr::str_subset(stringr::str_split(b, "\n", simplify = TRUE), "Theme"),
    theme = stringr::str_remove(theme, "\\*\\*Theme\\*\\*: "),
    cohost = stringr::str_subset(stringr::str_split(b, "\n", simplify = TRUE), "\\*\\*Co-host"),
    cohost = stringr::str_remove(cohost, "\\*\\*Co-host\\*\\*: "))
}
