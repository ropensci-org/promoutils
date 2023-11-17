
#' @export
pkgs <- function(url = "https://ropensci.github.io/roregistry/registry.json",
                 which = "active", return = "sub") {
  if(return == "sub") cols <- c("name", "maintainer", "owner")
  if(return == "all") cols <- substitute(dplyr::everything())


  pkgs <- jsonlite::fromJSON(url)[["package"]]

   if(which == "active") {
     pkgs <- dplyr::filter(pkgs, stringr::str_detect(.data$status, "active"))
   } else {
     pkgs <- dplyr::filter(pkgs, stringr::str_detect(.data$status, "abandoned",
                                                     negate = TRUE))
   }

  pkgs |>
    dplyr::mutate(owner = stringr::str_remove_all(
      .data$github, glue::glue("(https://github.com/)|(/{name})"))) |>
    dplyr::select(!!cols) |>
    dplyr::mutate(owner = dplyr::if_else(owner == "frictionlessdata-r",
                                         "frictionlessdata",
                                         owner),
                  name = dplyr::if_else(name == "frictionless",
                                         "frictionless-r",
                                         name))

}

#' @export
pkg_authors <- function(x, pkgs) {
  a <- dplyr::filter(pkgs, .data$name %in% x) |>
    dplyr::pull(.data$maintainer)

  if(length(a) == 0) a <- NA_character_
  a
}

#' Fetch full name of GitHub user
#'
#' @param gh_user Character. GitHub username/handle
#'
#' @return Character, full name of the user or NA
#'
#' @export
#'
#' @examples
#' gh_name("steffilazerte")

gh_name <- function(gh_user) {
  i <- gh::gh("/users/:username", username = gh_user)
  if(!is.null(i$name)) return(i$name) else return(NA_character_)
}


#' Find GH username from repository and full name
#'
#' Look up users of a repository and match to a name. Try with and without
#' initials.
#'
#' @param name Character. Full or partial name of the person for whom you want
#'   to fetch the GitHub username.
#' @param owner Character. Owner of the repository.
#' @param pkg Character. Repository name (package name).
#'
#' @return Data frame with names attempted and usernames found
#' @export
#'
#' @examples
#'
#' gh_user(name = "Steffi E. LaZerte", owner = "ropensci", pkg = "weathercan")
#' gh_user(name = "Steffi", owner = "ropensci", pkg = "weathercan")

gh_user <- function(name, owner = "ropensci", pkg) {

  repo_users <- gh::gh(endpoint = "/repos/:owner/:pkg/contributors",
                       owner = owner, pkg = pkg)
  repo_users <- purrr::map_chr(repo_users, "login")

  # Try also without initials
  n <- stringr::str_remove_all(name, "\\.")
  if(stringr::str_detect(n, "\\b[A-Z]{1} ")) {
    n <- c(n, stringr::str_remove_all(n, "\\b[A-Z]{1} "))
  }

  dplyr::tibble(name = n) |>
    dplyr::mutate(gh_user = purrr::map(
      name, \(x) gh::gh(endpoint = "/search/users",
                        q = glue::glue("{x} in:name"))$items)) |>
    tidyr::unnest(gh_user, keep_empty = TRUE) |>
    dplyr::mutate(gh_user = purrr::map(.data$gh_user, "login")) |>
    tidyr::unnest(gh_user, keep_empty = TRUE) |>
    dplyr::filter(is.na(.data$gh_user) | .data$gh_user %in% !!repo_users) |>
    dplyr::arrange(is.na(gh_user)) |>
    dplyr::slice(1)
}

#' Fetch GH and Mastodon usernames
#'
#' Based on a name and a repository, fetch the usernames
#'
#' @param name Character. Full name
#' @param owner Character. Repository owner.
#' @param pkg Character. Repository name (package name).
#'
#' @return Data frame of user names: gh_user and masto_user
#' @export
#'
#' @examples
#' all_users(name = "Steffi LaZerte", pkg = "weathercan")
all_users <- function(name, owner = "ropensci", pkg) {
  gh_user(name, owner, pkg) |>
    dplyr::mutate(masto_user = masto_user(gh_user = u$gh_user, name = u$name)) |>
    dplyr::select(-"name")
}


#' Fetch Mastodon username
#'
#' Using the GH username or the Full name, check rOpenSci author pages and then
#' GitHub for references to the person Mastodon account.
#'
#' @param gh_user Character. GH user name.
#' @param name Character. Full/Partial name
#'
#' @return Character url to Mastodon profile
#' @export
#'
#' @examples
#' masto_user("steffilazerte")

masto_user <- function(gh_user = NULL, name = NULL) {

  # Name placeholder
  m <- NA_character_

  # If no name let's get it from GH
  if(is.null(name) && !is.null(gh_user) && !is.na(gh_user) ) name <- gh_name(gh_user)

  # Now try via name from rOpenSci
  if(!is.na(name) && !is.null(name)) {
    name <- stringr::str_remove_all(name, "\\.")
    t <- ro_masto(name)
  }

  # Otherwise chech GH profile
  if(!is.na(t) && !is.null(gh_user) && !is.na(gh_user)) t <- gh_masto(gh_user)

  t
}

#' Find Mastodon username from GitHub profile
#'
#' @param gh_user Character. GH username
#'
#' @return Character URL to mastodon profile if it exists, NA otherwise.
#' @export
#'
#' @examples
#' gh_masto("steffilazerte")

gh_masto <- function(gh_user) {
  info <- gh::gh("/users/{username}/social_accounts", username = gh_user)
  m <- info |>
    purrr::map(dplyr::as_tibble) |>
    purrr::list_rbind() |>
    dplyr::filter(stringr::str_detect(tolower(provider), "mastodon")) |>
    dplyr::pull("url")

  if(is.null(m) || length(m) == 0) m <- NA_character_
  m
}

#' Find Mastodon username from rOpenSci author pages
#'
#' @param gh_user Character. Full name as on RO author pages
#'
#' @return Character URL to mastodon profile if it exists, NA otherwise.
#' @export
#'
#' @examples
#' ro_masto("Steffi LaZerte")

ro_masto <- function(name) {
  name <- stringr::str_replace_all(name, " ", "-")
  name <- tolower(name)

  t <- try(gh::gh("/repos/ropensci/roweb3/contents/content/author/{name}/_index.md",
                  name = name)[["download_url"]], silent = TRUE)
  if(class(t) %in% "try-error") return(NA_character_)

  t <- t |>
    httr2::request() |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    yaml::read_yaml(text = _)
  t <- t$mastodon
  if(is.null(t)) t <- NA_character_
  t
}



#' Extract mentions from forum text
#'
#' @param x Forum text
#'
#' @return Character of metions
#' @export
forum_mention <- function(x) {
  if(stringr::str_detect(rvest::html_text(x), "@")) {
    r <- rvest::html_nodes(x, css = ".mention") |>
      rvest::html_text() |>
      stringr::str_subset("rOpenSci", negate = TRUE)

    if(length(r) == 0) {
      r <- stringr::str_extract_all(rvest::html_text(x), "@[0-9a-zA-Z]+") |>
        unlist() |>
        stringr::str_subset("rOpenSci", negate = TRUE)
    }

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
forum_resource <- function(x) {
  x |>
    # https://stackoverflow.com/questions/60137188/xpath-picking-div-after-h4-with-specific-text
    rvest::html_elements(css = 'h4:contains(resource) + *') |>
    rvest::html_text2() |>
    stringr::str_split("\\\n|,( )*|;( )*") |>
    unlist() |>
    stringr::str_trim() |>
    stringr::str_remove_all("(^\\.)|(\\.$)|(\\[.+\\])") |>
    stringr::str_trim() |>
    unlist()
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
#' @param which Numeric. The nth week to return (i.e. the 1st Tuesday if `n = 1`
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

