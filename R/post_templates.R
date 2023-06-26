#' Create a template post for coworking
#'
#' @param date Character/Date. Date of the coworking event (local)
#' @param who Character. The full mastodon handle for the cohost (i.e. XXXX@XXXX.com)
#' @param when Character. Is this a "week" before reminder or an "hour" before reminder?
#' @export
post_coworking <- function(date, when = "week", who) {

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
        x <- readr::read_lines(.x, n_max = 60)
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
      author = stringr::str_extract(.data$author, "^[^ ]+"),
      action1 = purrr::map(event$content, ~.x[stringr::str_which(.x, "### Cowork") + 1:2]),
      action1 = purrr::map_chr(.data$action1, ~glue::glue_collapse(.x, sep = "\n")),
      tz = .env$tz,
      tz_txt = dplyr::case_when(.data$tz == "America/Vancouver" ~ "Americas Pacific",
                                .data$tz == "Europe/Paris" ~ "European Central",
                                .data$tz == "Australia/Perth" ~ "Australian Western"),
      date_UTC = lubridate::as_datetime(.data$date_UTC),
      date_local = lubridate::with_tz(.data$date_UTC, tz = .data$tz),
      theme = stringr::str_remove(.data$theme, "Social Coworking and Office Hours - "),
      nth = nth_day(lubridate::day(.data$date)),
      time = paste0(stringr::str_trim(format(.data$date_local, "%A %B")),
                    " ", .data$nth, " ",
                    format(.data$date_local, "%H:00"),
                    " ", .data$tz_txt,
                    " (", format(.data$date_UTC, "%H:00"), " UTC)"),
      when_text = dplyr::if_else(
        .env$when == "week",
        "Coworking next week!",
        "rOpenSci coworking and office hours coming up in an hour!"),
      time_post = date_local - dplyr::if_else(
        .env$when == "week",
        lubridate::weeks(1),
        lubridate::hours(1)))

  if(when == "week") week_before(deets) else hour_before(deets)
}

week_before <- function(x) {
  glue::glue_data(
    x,
    "~~~",
    "time: {time_post}",
    "tz: {tz}       # Must be valid tz from `OlsonNames()`",
    "~~~",
    "\n",
    "{when_text}",
    "\n",
    "Theme: {theme}", #Integrating and merging datasets from different sources
    "\n",
    "{time}", #Tues June 6th 9:00 Australia Western (01:00 UTC)
    "\n",
    "Join {who} and @steffilazerte@fosstodon.org",
    "\n",
    "- General coworking",
    "{action1}",
    "- Chat with {author} and other attendees and discuss strategies for XXXX",
    "\n",
    "https://ropensci.org/events/coworking-{format(date_UTC, '%Y-%m')}",
    "\n",
    "#RStats",
    "@rstats@a.gup.pe",
    .sep = "\n",
  )
}

hour_before <- function(x) {
  glue::glue_data(
    x,
    "~~~",
    "time: {time_post}",
    "tz: {tz}       # Must be valid tz from `OlsonNames()`",
    "~~~",
    "\n",
    "{when_text}",
    "\n",
    "Today's Theme: {theme} with cohost {who}", #Integrating and merging datasets from different sources
    "\n",
    "{time}", #Tues June 6th 9:00 Australia Western (01:00 UTC)
    "\n",
    "https://ropensci.org/events/coworking-{format(date_UTC, '%Y-%m')}",
    "\n",
    "#RStats",
    "@rstats@a.gup.pe",
    .sep = "\n",
  )
}
