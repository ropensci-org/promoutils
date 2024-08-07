#' Create a cached version of the GH api calls
#'
#' @details `memoise::memoise(gh::gh)`
#'
#' @export
gh_cache <- memoise::memoise(gh::gh, omit_args = c(".max_rate"))

#' Return a data frame of rOpenSci packages
#'
#' @param url Character. Registry url
#' @param which Character. Status of packages to return ("all" or "active")
#' @param return Character. Return a subset ("sub") or all ("all") package fields.
#'
#' @return data frame
#' @export
#'
#' @examples
#' pkgs()
#' pkgs(which = "all", return = "all")
pkgs <- function(url = "https://ropensci.github.io/roregistry/registry.json",
                 which = "active", return = "sub") {

  pkgs <- jsonlite::fromJSON(url)$package

   if(which == "active") {
     pkgs <- dplyr::filter(pkgs, .data$type == "active")
   } else {
     pkgs <- dplyr::filter(pkgs, .data$type != "archived")
   }

  p <- pkgs |>
    dplyr::mutate(
      repo = stringr::str_extract(.data$github, "[[:alnum:].-]+$"),
      owner = stringr::str_remove_all(
        .data$github, glue::glue("(https://github.com/)|(/{repo})")))
  if(return == "sub") p <- dplyr::select(p, dplyr::any_of(c("name", "maintainer", "owner", "repo")))

  p
}


#' Get package author names
#'
#' @param x Character. Package name
#' @param pkgs Data frame. Packages returned by `pkgs()`.
#'
#' @return Character name of maintainer
#' @export
pkg_authors <- function(x, pkgs) {
  a <- dplyr::filter(pkgs, .data$name %in% x) |>
    dplyr::pull(.data$maintainer)

  if(length(a) == 0) a <- NA_character_
  a
}

#' Extract mentions from forum text
#'
#' @param x Forum text
#'
#' @return Character of metions
#' @export
forum_mention <- function(x) {
  if(stringr::str_detect(rvest::html_text(x), "@")) {
    r <- stringr::str_extract_all(
      # Should get Twitter or Mastodon handles
      rvest::html_text(x), "@[0-9a-zA-Z]+(@[0-9a-zA-Z.]+)?") |>
      unlist() |>
      stringr::str_subset("rOpenSci", negate = TRUE)

    r <- glue::glue_collapse(r, sep = ", ", last = " & ")

  } else r <- ""
  r
}

#' Extract resources from forum text
#'
#' @param x Forum text
#'
#' @return Character vector of resources
#'
#' @export
#'
#' @examples
#' # forum_post(3920) |> # Needs auth
#' #   forum_resource()  # > weatherOz

forum_resource <- function(x) {
  x |>
    # https://stackoverflow.com/questions/60137188/xpath-picking-div-after-h4-with-specific-text
    rvest::html_elements(css = 'h4:contains(resource) + *') |>
    rvest::html_text2() |>
    stringr::str_split("\\\n|,( )*|;( )*") |>
    unlist() |>
    stringr::str_trim() |>
    stringr::str_remove_all("(^\\.)|(\\.$)|(\\[.+\\])|(\\{)|(\\})") |>
    stringr::str_trim() |>
    unlist()
}

#' Fetch post text from by topic id
#'
#' @param x Topic id
#'
#' @return HTML of the post
#'
#' @export
#' @examples
#' # forum_post(3920) # Needs auth
forum_post <- function(topic_id) {
  httr2::request(glue::glue("https://discuss.ropensci.org/t/{topic_id}.json")) |>
    httr2::req_headers("API-Key" = Sys.getenv("DISCOURSE_API_KEY"),
                       "Api-Username" = Sys.getenv("DISCOURSE_USERNAME")) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON() |>
    purrr::pluck("post_stream", "posts") |>
    dplyr::slice(1) |>
    purrr::pluck("cooked") |>
    xml2::read_html()
}


nth_day <- function(x) {

  th <- dplyr::case_when(x %in% c(1, 21, 31) ~ "st",
                         x %in% c(2, 22) ~ "nd",
                         x %in% c(3, 23) ~ "rd",
                         TRUE ~ "th")

  paste0(x, th)
}

#' Find the next date
#'
#' Given a date and a day of the week,
#' Given a date return the next month's first Tuesday
#'
#' @param month Character/Date. The current month. Date returned is the next month.
#' @param which Character/Numeric. Which week day to return. Either number or
#'   abbreviated English weekday.
#' @param n Numeric. The nth week to return (i.e. the 1st Tuesday if `n = 1`
#'   and `which = "Tues"`).
#'
#' @return A date
#' @export
#'
#' @examples
#'
#' # Get the next first Tuesday
#' next_date("2023-11-01")
#' next_date("2023-11-30")
#'
#' # Get the next 3rd Tuesday
#' next_date("2023-11-01", n = 3)
#'
#' # Oops
#' \dontrun{
#' next_date("2023-11-01", n = 5)
#' }
#'
next_date <- function(month, which = "Tues", n = 1) {
  month <- lubridate::as_date(month) + lubridate::period("1 month")

  d <- month |>
    lubridate::floor_date(unit = "months") |>
    lubridate::ceiling_date(unit = "weeks", week_start = which,
                            change_on_boundary = FALSE)

  d <- d + lubridate::weeks(n - 1)

  if(lubridate::month(d) != lubridate::month(month)) {
    stop("There are not ", n, " ", format(d, "%A"), "s in ", format(month, "%B %Y"),
         call. = FALSE)
  }
  d
}


#' Replace emoji codes with unicode
#'
#' Replaces emoji codes like :tada: with unicode like 🎉.
#'
#' @param x Character. Text string within which to replace codes
#'
#' @return  Text string with emoji unicodes
#' @export
#'
#' @examples
#' x <- replace_emoji("hi :tada: testing \n\n\n Whow ! 🔗 \n\n\n :smile:")
#' x
replace_emoji <- function(x) {
  emo <- stringr::str_extract_all(x, "\\:.+\\:") |>
    unlist() |>
    unique()

  if(length(emo) > 1) {
    emo <- stats::setNames(
      purrr::map(emo, ~pandoc::pandoc_convert(
        text = .x, from = "markdown+emoji", to = "plain")) |>
        unlist(),
      nm = emo)

    x <- stringr::str_replace_all(x, emo)
  }
  x
}


#' Extract YAML keys from block
#'
#' @param yaml Character. String from which to extract YAML keys
#' @param trim Character. Text to remove from the YAML block before processing.
#'   Usually the text that defines the block.
#'
#' @return data frame of yaml keys
#' @export
#'
#' @examples
#'
#' yaml_extract("~~~start: 2023-11-12\nauthor: Steffi\n~~~")
#'
yaml_extract <- function(yaml, trim = "~~~") {
  y <- stringr::str_remove_all(yaml, trim) %>%
    yaml::yaml.load() %>%
    purrr::map_if(is.null,  ~"") %>%
    data.frame()

  # Catch common typos
  names(y) <- tolower(names(y))
  names(y) <- stringr::str_replace_all(names(y),
                                       "(reocuring)|(reoccuring)|(reocurring)",
                                       "reoccurring")
  y
}


# LinkedIn Chars to escape
escape_linkedin_chars <- function(x) {
  chars <- c("\\|", "\\{", "\\}", "\\@", "\\[", "\\]", "\\(", "\\)", "\\<", "\\>",
             "\\#", "\\\\", "\\*", "\\_", "\\~")
  p <- stats::setNames(paste0("\\", chars), chars)
  stringr::str_replace_all(x, p)
}


#' Convert a mastodon user link to handle
#'
#' @param x Character. Link to user's profile
#'
#' @return Character user handle @user@instance
#' @export
#'
#' @examples
#' masto2user("https://fosstodon.org/@steffilazerte")
#' masto2user("steffi")
#' masto2user("@steffilazerte@fosstodon.org")
#' masto2user(NA)

masto2user <- function(x) {
  if(is.na(x) || stringr::str_count(x, "@") > 1) {
    n <- x
  } else if(stringr::str_detect(x, "http|@")) {
    n <- stringr::str_remove(x, "http(s?)://") |>
      stringr::str_split("/", simplify = TRUE) |>
      as.vector()
    n <- paste0(n[2], "@", n[1])
  } else n <- x
  n
}
