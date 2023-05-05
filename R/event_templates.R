#' Create a template event for coworking
#'
#' @param date Character/Date. Date of the coworking event (local)
#' @param tz Character. Timezone of the event (match from `OlsonNames()`)
#' @param theme Character. Theme.
#' @param cohost Character. Name of cohost
#'
#' @return
#' @export
#'
#' @examples
event_coworking <- function(date, tz, theme = "THEME", cohost = "COHOST") {

  dir <- "./content/events/"
  if(!dir.exists(dir)) stop("Directory ", dir, " doesn't exist. ",
                            "Are you running this in `roweb3`?", call. = FALSE)

  if(!tz %in% OlsonNames()) {
    stop("`tz` (", tz, ") not in `OlsonNames()`", call. = FALSE)
  }

  details <- data.frame(
    tz = c("America/Vancouver", "Australia/Perth", "Europe/Paris"),
    time = c(9, 9, 14),
    tz_nice = c("Americas Pacific", "Australian Western", "European Central"))

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
    "[Social Coworking + Office Hours](/blog/2021/08/17/coworking-sessions/)**",
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
    "[Social Coworking + Office Hours](/blog/2021/08/17/coworking-sessions/) ",
    "on the first Tuesday of each month, alternating among timezones to ",
    "accommodate different parts of the world."
  )


writeLines(paste0(yaml, "\n\n", time_check, "\n\n", body), f)

}
