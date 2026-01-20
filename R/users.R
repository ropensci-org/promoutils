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
  if (!is.null(i$name)) return(i$name) else return(NA_character_)
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
#' @param .max_rate Numeric. Passed through to `gh:gh()`.
#'
#' @return Data frame with names attempted and usernames found
#' @export
#'
#' @examples
#'
#' gh_user(name = "Steffi E. LaZerte", owner = "ropensci", pkg = "weathercan")
#' gh_user(name = "Steffi", owner = "ropensci", pkg = "weathercan")

gh_user <- function(name, owner = "ropensci", pkg, .max_rate = NULL) {
  repo_users <- gh_cache(
    endpoint = "/repos/:owner/:pkg/contributors",
    owner = owner,
    pkg = pkg,
    .max_rate = .max_rate
  )
  repo_users <- purrr::map_chr(repo_users, "login")

  # Try also without initials
  n <- stringr::str_remove_all(name, "\\.")
  if (stringr::str_detect(n, "\\b[A-Z]{1} ")) {
    n <- c(n, stringr::str_remove_all(n, "\\b[A-Z]{1} "))
  }

  u <- dplyr::tibble(name = n) |>
    dplyr::mutate(
      gh_user = purrr::map(
        name,
        \(x) {
          gh_cache(
            endpoint = "/search/users",
            q = glue::glue("{x} in:name"),
            .max_rate = .max_rate
          )$items
        }
      )
    ) |>
    tidyr::unnest(gh_user, keep_empty = TRUE) |>
    dplyr::mutate(gh_user = purrr::map(.data$gh_user, "login")) |>
    tidyr::unnest(gh_user, keep_empty = TRUE) |>
    dplyr::filter(is.na(.data$gh_user) | .data$gh_user %in% !!repo_users) |>
    dplyr::arrange(is.na(gh_user)) |>
    dplyr::slice(1)
  if (nrow(u) == 0) {
    u <- data.frame(name = name, gh_user = NA_character_)
  }
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
    dplyr::mutate(
      masto_user = masto_user(gh_user = .data$gh_user, name = .data$name)
    ) |>
    dplyr::select(-"name")
}


#' Fetch full name of Discourse user (id)
#'
#' Using the Discourse username id, return the full name of that discourse user.
#'
#' @param user_id Integer. Discourse user id.
#'
#' @return Character
#' @export
#'
#' @examples
#' # discourse_user(1) # Requires authentication

discourse_user <- function(user) {
  httr2::request("https://discuss.ropensci.org") |>
    httr2::req_headers(
      "API-Key" = Sys.getenv("DISCOURSE_API_KEY"),
      "Api-Username" = Sys.getenv("DISCOURSE_USERNAME")
    ) |>
    httr2::req_url_path("admin", "users", paste0(user, ".json")) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON() |>
    purrr::pluck("name")
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
  if (is.null(name) && !is.null(gh_user) && !is.na(gh_user)) {
    name <- gh_name(gh_user)
  }

  # Now try via name from rOpenSci
  if (!is.na(name) && !is.null(name)) {
    name <- stringr::str_remove_all(name, "\\.")
    m <- ro_masto(name)
  }

  # Otherwise check GH profile
  if (is.na(m) && !is.null(gh_user) && !is.na(gh_user)) {
    m <- gh_masto(gh_user)
  }

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

  if (length(info) > 0) {
    m <- info |>
      purrr::map(dplyr::as_tibble) |>
      purrr::list_rbind() |>
      dplyr::filter(stringr::str_detect(tolower(.data$provider), "mastodon")) |>
      dplyr::pull("url")
  }

  if (!exists("m") || is.null(m) || length(m) == 0) {
    m <- NA_character_
  }
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

  t <- tryCatch(
    gh_cache(
      "/repos/ropensci/roweb3/contents/content/author/{name}/_index.md",
      name = name
    )[["html_url"]],
    error = function(e) NA_character_
  )

  if (!is.na(t)) {
    # In case of accents, need to use the HTML encoding with the download origin
    t <- t |>
      stringr::str_remove_all("blob/") |>
      stringr::str_replace(
        "https://github.com/",
        "https://raw.githubusercontent.com/"
      ) |>
      httr2::request() |>
      httr2::req_perform() |>
      httr2::resp_body_string() |>
      yaml::read_yaml(text = _)

    t <- t$mastodon
    if (is.null(t)) {
      t <- NA_character_
    }
  }
  t
}


#' Add in author and maintainer social media handles
#'
#' @param df Data frame with `maintainer_name` and `author_github`
#'
#' @returns Data frame with added names and social media handles.
#'
#' @export
#' @examples
#' u <- uc_fetch() |>
#'   uc_fmt("2025-01-01") |>
#'   add_names()

add_names <- function(df) {
  if (nrow(df) == 0) {
    return(data.frame())
  }

  # Fetch maintainer socials we have
  df <- df |>
    dplyr::mutate(
      maintainer_github = monarch::fetch(
        .data$maintainer_name,
        type = "github"
      ),
      maintainer_mastodon = monarch::fetch(
        .data$maintainer_github,
        type = "mastodon"
      )
    )

  # Get missing maintainer GH usename
  nms_miss <- dplyr::select(
    df,
    "maintainer_name",
    "maintainer_github",
    "resource"
  ) |>
    dplyr::distinct()

  purrr::pwalk(
    nms_miss,
    \(maintainer_name, maintainer_github, resource, ...) {
      if (is.na(maintainer_github) & !is.na(maintainer_name)) {
        monarch::socials_fetch(name = maintainer_name, pkg = resource) |>
          monarch::cocoon_update()
      }
    }
  )

  # Get missing maintainer socials - Only if we already have a GH username
  nms_miss <- dplyr::select(
    df,
    "maintainer_mastodon",
    "maintainer_github"
  ) |>
    dplyr::distinct()

  purrr::pwalk(nms_miss, \(maintainer_mastodon, maintainer_github, ...) {
    if (is.na(maintainer_mastodon) & !is.na(maintainer_github)) {
      monarch::socials_fetch(github = maintainer_github) |>
        monarch::cocoon_update()
    }
  })

  # Update maintainer socials
  # Fetch author socials we have
  df <- df |>
    dplyr::mutate(
      maintainer_github = monarch::fetch(
        .data$maintainer_name,
        type = "github"
      ),
      maintainer_mastodon = monarch::fetch(.data$maintainer_github),
      author_name = monarch::fetch(.data$author_github, type = "name"),
      author_mastodon = monarch::fetch(.data$author_github)
    )

  # Get missing author socials
  purrr::pwalk(df, \(author_mastodon, author_name, author_github, ...) {
    if (is.na(author_mastodon) | is.na(author_name)) {
      monarch::socials_fetch(github = author_github) |>
        monarch::cocoon_update()
    }
  })

  # Add in any we recently retrieved
  df <- df |>
    dplyr::mutate(
      author_name = monarch::fetch(.data$author_github, type = "name"),
      author_mastodon = monarch::fetch(.data$author_github)
    )

  # Put in placeholders for missing
  df <- df |>
    dplyr::mutate(
      author_linkedin = dplyr::if_else(
        is.na(author_name),
        author_github,
        author_name
      ),
      author_mastodon = dplyr::if_else(
        is.na(author_mastodon),
        author_name,
        author_mastodon
      ),
      maintainer_mastodon = dplyr::if_else(
        is.na(maintainer_mastodon),
        maintainer_name,
        maintainer_mastodon
      ),
      maintainer_linkedin = maintainer_name
    )

  df
}
