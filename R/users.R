
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
  i <- gh_cache("/users/:username", username = gh_user)
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

  repo_users <- gh_cache(endpoint = "/repos/:owner/:pkg/contributors",
                         owner = owner, pkg = pkg)
  repo_users <- purrr::map_chr(repo_users, "login")

  # Try also without initials
  n <- stringr::str_remove_all(name, "\\.")
  if(stringr::str_detect(n, "\\b[A-Z]{1} ")) {
    n <- c(n, stringr::str_remove_all(n, "\\b[A-Z]{1} "))
  }

  u <- dplyr::tibble(name = n) |>
    dplyr::mutate(gh_user = purrr::map(
      name, \(x) gh_cache(endpoint = "/search/users",
                          q = glue::glue("{x} in:name"))$items)) |>
    tidyr::unnest(gh_user, keep_empty = TRUE) |>
    dplyr::mutate(gh_user = purrr::map(.data$gh_user, "login")) |>
    tidyr::unnest(gh_user, keep_empty = TRUE) |>
    dplyr::filter(is.na(.data$gh_user) | .data$gh_user %in% !!repo_users) |>
    dplyr::arrange(is.na(gh_user)) |>
    dplyr::slice(1)
  if(nrow(u) == 0) u <- data.frame(name = name, gh_user = NA_character_)
  u
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
    dplyr::mutate(masto_user = masto_user(gh_user = .data$gh_user, name = .data$name)) |>
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
    m <- ro_masto(name)
  }

  # Otherwise check GH profile
  if(is.na(m) && !is.null(gh_user) && !is.na(gh_user)) m <- gh_masto(gh_user)

  m
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
  info <- gh_cache("/users/{username}/social_accounts", username = gh_user)

  if(length(info) > 0) {
    m <- info |>
      purrr::map(dplyr::as_tibble) |>
      purrr::list_rbind() |>
      dplyr::filter(stringr::str_detect(tolower(.data$provider), "mastodon")) |>
      dplyr::pull("url")
  }

  if(!exists("m") || is.null(m) || length(m) == 0) m <- NA_character_
  m
}

#' Find Mastodon username from rOpenSci author pages
#'
#' @param name Character. Full name as on RO author pages
#'
#' @return Character URL to mastodon profile if it exists, NA otherwise.
#' @export
#'
#' @examples
#' ro_masto("Steffi LaZerte")

ro_masto <- function(name) {
  name <- stringr::str_replace_all(name, " ", "-")
  name <- tolower(name)

  t <- try(gh_cache("/repos/ropensci/roweb3/contents/content/author/{name}/_index.md",
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
