twitter_lists <- function() {
  dplyr::tribble(
    ~gh_user,   ~name,                     ~twitter_user,
    "elinw",    "Elin Waring",             "elinwaring",
    "florianm", "Florian W. Mayer",        "fistful_of_bass",
    "agila5",   "Andrea Gilardi",          "a_gilardi5",
    "andysouth", "Andy South",             "southmapr",
    "wlandau",   "William Michael Landau", "wmlandau",
    "HajkD",     "Hajk-Georg Drost",       "@HajkDrost")
}

#' @export
pkgs <- function(url = "https://ropensci.github.io/roregistry/registry.json",
                 which = "active") {
  pkgs <- url %>%
    jsonlite::fromJSON(.) %>%
    .[["packages"]]

   if(which == "active") {
     pkgs <- dplyr::filter(pkgs, stringr::str_detect(.data$status, "active"))
   } else {
     pkgs <- dplyr::filter(pkgs, stringr::str_detect(.data$status, "abandoned",
                                                     negate = TRUE))
   }

  pkgs %>%
    dplyr::mutate(owner = stringr::str_remove_all(
      .data$github, glue::glue("(https://github.com/)|(/{name})"))) %>%
    dplyr::select("name", "maintainer", "owner")
}

#' @export
get_authors <- function(x, pkgs) {
  a <- dplyr::filter(pkgs, .data$name %in% x) %>%
    dplyr::pull(.data$maintainer)

  if(length(a) == 0) a <- NA_character_
  a
}

#' @export
get_user <- function(name, owner, pkg) {
  if(name %in% twitter_lists()$name) {
    u <- dplyr::filter(twitter_lists(), .data$name == !!name) %>%
      dplyr::select("gh_user", "twitter_user")
  } else {
    repo_users <- gh::gh(endpoint = "/repos/:owner/:pkg/contributors",
                         owner = owner, pkg = pkg)
    repo_users <- purrr::map_chr(repo_users, "login")

    u <- dplyr::tibble(name = stringr::str_remove_all(name, "\\.")) %>%
      dplyr::mutate(gh_user = list(
        gh::gh(endpoint = "/search/users",
               q = glue::glue("{.data$name} in:name"))$items),
        n = purrr::map_dbl(.data$gh_user, length),
        gh_user = purrr::map2_chr(.data$n, .data$gh_user, ~{if(.x == 0) NA_character_ else .y[[1]]$login})) %>%
      dplyr::filter(is.na(.data$gh_user) | .data$gh_user %in% !!repo_users) %>%
      dplyr::mutate(twitter_user = purrr::map2_chr(.data$gh_user, .data$name,
                                                   get_twitter)) %>%
      dplyr::select("gh_user", "twitter_user")
  }
  u
}

#' @export
get_gh_name <- function(gh_user) {
  i <- gh::gh("/users/:username", username = gh_user)
  if(!is.null(i$name)) return(i$name) else return(NA_character_)
}

#' @export
get_twitter <- function(gh_user, name = NULL) {

  t <- NA_character_

  if(!is.na(gh_user) & gh_user %in% twitter_lists()$gh_user) {
    t <- dplyr::filter(twitter_lists(), .data$gh_user == !!gh_user)$twitter_user
  }

  if(is.na(t) && is.null(name) && !is.na(gh_user)) name <- get_gh_name(gh_user)

  if(is.na(t) && !is.na(name) && !is.null(name)) {
    name <- stringr::str_remove_all(name, "\\.")
    t <- get_twitter_from_ro(name)
  }

  if(is.na(t) && !is.na(gh_user)) t <- get_twitter_from_gh(gh_user)

  t
}

get_twitter_from_gh <- function(gh_user) {
  info <- gh::gh("/users/:username", username = gh_user)
  twitter_user <- info$twitter_username
  if(is.null(twitter_user)) twitter_user <- NA_character_
  twitter_user
}

get_twitter_from_ro <- function(name) {
  name <- stringr::str_replace_all(name, " ", "-")
  name <- tolower(name)

  t <- try(gh::gh("/repos/ropensci/roweb3/contents/content/author/{name}/_index.md",
                  name = name)[["download_url"]], silent = TRUE)
  if(class(t) %in% "try-error") return(NA_character_)

  t <- t %>%
    httr::GET() %>%
    httr::content(as = "text") %>%
    yaml::read_yaml(text = .) %>%
    .$twitter
  if(is.null(t)) t <- NA_character_
  t
}

#' @export
forum_mention <- function(x) {
  if(stringr::str_detect(rvest::html_text(x), "@")) {
    r <- rvest::html_nodes(x, css = ".mention") %>%
      rvest::html_text() %>%
      stringr::str_subset("rOpenSci", negate = TRUE)

    if(length(r) == 0) {
      r <- stringr::str_extract_all(rvest::html_text(x), "@[0-9a-zA-Z]+") %>%
        unlist() %>%
        stringr::str_subset("rOpenSci", negate = TRUE)
    }

    r <- glue::glue_collapse(r, sep = ", ", last = " & ")

  } else r <- ""
  r
}

#' @export
forum_resource <- function(x) {
  x %>%
    # https://stackoverflow.com/questions/60137188/xpath-picking-div-after-h4-with-specific-text
    rvest::html_elements(css = 'h4:contains(resource) + *') %>%
    rvest::html_text2() %>%
    stringr::str_split("\\\n") %>%
    unlist()
}

#' @export
get_issues <- function(owner, repo, labels_help, labels_first, since = NULL) {

  message("owner: ", owner, "; repo: ", repo)

  if(!is.null(since)) {
    i <- gh::gh(endpoint = "/repos/{owner}/{repo}/issues",
                owner = owner, repo = repo,
                state = "open",
                since = format(since, "%Y-%m-%dT%H:%M:%SZ"),
                .limit = Inf)
  } else {
    i <- gh::gh(endpoint = "/repos/{owner}/{repo}/issues",
                owner = owner, repo = repo,
                state = "open",
                .limit = Inf)
  }

  dplyr::tibble(url = purrr::map_chr(i, "url"),
                number = purrr::map_chr(i, "number"),
                labels = purrr::map(i, "labels"),
                title = purrr::map_chr(i, "title"),
                created = purrr::map_chr(i, "created_at"),
                update = purrr::map_chr(i, "updated_at"),
                gh_user_issue = purrr::map_chr(i, ~.[["user"]]$login)) %>%
    dplyr::mutate(labels = purrr::map_depth(.data$labels, 2, "name"),
                  labels_help = purrr::map_lgl(
                    .data$labels,
                    ~any(str_detect(tolower(.), !!labels_help))),
                  labels_first = purrr::map_lgl(
                    .data$labels,
                    ~any(str_detect(tolower(.), !!labels_first))),
                  n_labels = purrr::map_int(.data$labels, length)) %>%
    dplyr::filter(.data$labels_help) %>%
    dplyr::mutate(events = purrr::map(.data$number,
                                      ~get_label_events(owner,
                                                        repo, .,
                                                        labels = !!labels_help))) %>%
    tidyr::unnest(.data$events)
}

get_label_events <- function(owner, repo, issue, labels) {
  events <- gh::gh(endpoint = "/repos/{owner}/{repo}/issues/{issue}/events",
                   owner = owner, repo = repo, issue = issue)

  e <- dplyr::tibble(event = purrr::map_dbl(events, "id"),
                     type = purrr::map_chr(events, "event"),
                     label = purrr::map(events, "label"),
                     gh_user = purrr::map_chr(events, ~.[["actor"]]$login),
                     label_created = purrr::map_chr(events, "created_at")) %>%
    dplyr::filter(.data$type == "labeled") %>%
    tidyr::unnest(.data$label)
  if(nrow(e) > 1) {
    e %>%
      tidyr::unnest(.data$label) %>%
      dplyr::filter(stringr::str_detect(.data$label, !!labels)) %>%
      dplyr::mutate(label_created = lubridate::ymd_hms(.data$label_created)) %>%
      dplyr::arrange(dplyr::desc(.data$label_created)) %>%
      dplyr::slice(1) %>%
      dplyr::select("gh_user_labelled" = "gh_user", "label_created")
  } else data.frame()
}


# Get package references out of url
# get_package_ref(url = "https://link.springer.com/article/10.1007/s40823-021-00067-y/tables/1")
get_package_ref <- function(url, pkgs = NULL) {

  text <- httr::GET(url) %>%
    httr::content() %>%
    rvest::html_element("body") %>%
    rvest::html_text2() %>%
    stringr::str_split(pattern = "\n") %>%
    unlist()

  if(is.null(pkgs)) pkgs <- pkgs()
  pkgs %>%
    dplyr::mutate(
      name_reg = glue::glue("\\b{name}\\b"),
      text_ref = purrr::map(.data$name_reg,
                            ~any(stringr::str_detect(!!text, .))),
      text_context = purrr::map(.data$name_reg,
                                ~stringr::str_extract_all(!!text, .))) %>%
    dplyr::filter(.data$text_ref == TRUE) %>%
    dplyr::mutate(author = purrr::map2(.data$maintainer, .data$name,
                                       ~get_user(name = .x,
                                                 owner = "ropensci",
                                                 pkg = .y))) %>%
    tidyr::unnest(.data$author) %>%
    dplyr::mutate(draft = glue::glue("{name} by @{twitter_user}"))
}
