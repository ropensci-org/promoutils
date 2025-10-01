#' Create Throwback Thursday `socials_post_issue()` command
#'
#' @param date YMD Character. Date of the original post.
#' @param title Character. Title of the original post.
#' @param url Character. URL to original blog post
#' @param blurb Character. Interesting bit to describe post (optional).
#'
#' @inheritParams common_docs
#'
#' @returns Character string of R functions with relevant details (normally
#'   pasted into a POSTS_TT.R script file and tweaked as necessary before being
#'   executed).
#' @export
#'
#' @examples
#' # Standard
#' tt_post("2019-05-14", "POWER to the People", "https://ropensci.org/blog/2019/05/14/nasapower/", print = TRUE)
#'
#' # Double throwback
#' tt_post(c("2017-08-22", "2017-08-22"),
#'         c("So you (don’t) think you can review a package",
#'           "Onboarding visdat, a tool for preliminary visualisation of whole dataframes"),
#'         url = c("https://ropensci.org/blog/2017/08/22/first-package-review/",
#'                 "https://ropensci.org/blog/2017/08/22/visdat/"),
#'         print = TRUE)
#'
#' # Slug only
#' tt_post("2019-05-14", "POWER to the People", "nasapower", print = TRUE)
tt_post <- function(
  date,
  title,
  url,
                    blurb = "<-- Interesting description/blurb mentioning the author -->",
  dry_run = FALSE,
  print = FALSE
) {
  m <- lubridate::month(date)

  if (length(date) > 1 & length(blurb) == 1) {
    blurb <- rep(blurb, length(date))
  }

  if (length(unique(lengths(list(date, title, url, blurb)))) > 1) {
    cli::cli_abort(
      "{.arg date}, {.arg title} and {.arg url} all need to be the same length. If {.arg blurb} has more than one item, it must also match the lengths of the others."
    )
  }

  if(!stringr::str_detect(url, "https://ropensci.org/blog")) {
    url <- glue::glue("https://ropensci.org/blog/{lubridate::year(date)}/",
                      "{stringr::str_pad(lubridate::month(date), 2, pad = 0)}/",
                      "{stringr::str_pad(lubridate::day(date), 2, pad = 0)}/{url}")
  }

  # Get the first Thursday of the relevant month in future
  time_post <- date |>
    lubridate::ymd() |>
    lubridate::floor_date(unit = "month") |>
    update(
      year = lubridate::year(Sys.Date()) +
        as.numeric(m < lubridate::month(Sys.Date()))
    ) |>
    lubridate::ceiling_date(unit = "week", week_start = "Thursday") |>
    update(hour = 8, tz = "America/Vancouver") |>
    unique()

  date_pretty <- format(as.Date(date), "%B %e, %Y") |>
    stringr::str_squish()
  body <- glue::glue(template("tt_social"), .sep = "\n")
  if (length(body) > 1) {
    b1 <- body[1]
    body <- body[-1] |>
      stringr::str_extract("\\:hourglass(\\s?.)*")
    body <- glue::glue_collapse(glue::glue(b1, body, .sep = "\n\n"), sep = "\n")
  }

  where <- c("mastodon", "linkedin")

  cmd <- glue::glue(
    "
promoutils::socials_post_issue(
  time = \"{time_post}\", tz = \"America/Vancouver\",
  where = \"{where}\", dry_run = {dry_run},
  over_char_limit = cli::cli_warn,
  title = \"TT\",
  body = \"{body}\n\")\n",
    .trim = FALSE
  )

  copy(cmd, "Throwback Thursday post command", print = print)
}
