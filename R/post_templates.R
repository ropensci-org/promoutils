#' Create a template post for coworking
#'
#' @param date Character/Date. Date of the coworking event (local)
#' @param who_masto Character. The full mastodon handle for the cohost (i.e. XXXX@XXXX.com)
#' @param who_slack Character. The full Slack handle for the cohost (i.e. @XXXX)
#' @param when Character. Is this a "week" before reminder or an "hour" before reminder?
#' @param posters_tz Character. Timezone of poster. Required for getting the
#'   time at which to post Slack messages as these are posted in the local
#'   timezone
#' @export
#'
#' @example
#'
#' post_coworking("2023-07-04", who_masto = "@cohost@mastodon.org", who_slack = "@cohost")

post_coworking <- function(date, who_masto, who_slack, posters_tz = "America/Winnipeg") {

  i <- gh::gh("/repos/{owner}/{repo}/contents/content/events",
         owner = "ropensci", repo = "roweb3")

  event <- dplyr::tibble(name = purrr::map_chr(i, "name"),
                download_url = purrr::map_chr(i, "download_url")) |>
    dplyr::mutate(date = stringr::str_extract(.data$name, "\\d{4}-\\d{2}-\\d{2}"),
                  date = lubridate::ymd(.data$date)) |>
    dplyr::filter(stringr::str_detect(.data$name, "coworking"),
                  .data$date == .env$date) |>
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

  deets <- yaml::read_yaml(text = event$yaml[[1]]) |>
    purrr::keep_at(c("title", "dateStart", "date", "title", "author")) |>
    dplyr::as_tibble() |>
    dplyr::rename("date_UTC" = "dateStart", "theme" = "title") |>
    dplyr::mutate(
      who_masto = .env$who_masto,
      who_slack = .env$who_slack,
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
      event_url = glue::glue("https://ropensci.org/events/coworking-{format(date_UTC, '%Y-%m')}"))


  glue::glue(week_before(deets),
             "\n\n---------\n\n",
             hour_before(deets),
             "\n\n---------\n\n",
             slack_week(deets, posters_tz),
             "\n\n---------\n\n",
             slack_hour(deets, posters_tz))
}

week_before <- function(x) {

  x |>
    dplyr::mutate(time_post = .data$date_local - lubridate::weeks(1)) |>
    glue::glue_data(
      "[MASTO WEEK BEFORE]",
      "",
      "[POST] - Coworking {month} {year} - week before",
      "",
      "~~~",
      "time: {time_post}",
      "tz: {tz}       # Must be valid tz from `OlsonNames()`",
      "~~~",
      "",
      "Coworking next week!",
      "",
      "Theme: {theme}", #Integrating and merging datasets from different sources
      "",
      "{time}", #Tues June 6th 9:00 Australia Western (01:00 UTC)
      "",
      "Join {who_masto} and @steffilazerte@fosstodon.org",
      "",
      "- General coworking",
      "{action1}",
      "- Chat with {author} and other attendees and discuss strategies for XXXX",
      "",
      "{event_url}",
      "",
      "#RStats",
      "@rstats@a.gup.pe",
      .sep = "\n",
    )
}

hour_before <- function(x) {

  x |>
    dplyr::mutate(time_post = .data$date_local - lubridate::hours(1)) |>
    glue::glue_data(
      "[MASTO HOUR BEFORE]",
      "",
      "[POST] - Coworking {month} {year} - 1-hr before",
      "",
      "~~~",
      "time: {time_post}",
      "tz: {tz}       # Must be valid tz from `OlsonNames()`",
      "~~~",
      "",
      "rOpenSci coworking and office hours coming up in an hour!",
      "",
      "Today's Theme: {theme} with cohost {who_masto}", #Integrating and merging datasets from different sources
      "",
      "{time}", #Tues June 6th 9:00 Australia Western (01:00 UTC)
      "",
      "{event_url}",
      "",
      "#RStats",
      "@rstats@a.gup.pe",
      .sep = "\n",
    )
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
