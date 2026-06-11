#' Create help wanted JSON file
#'
#' Function created for rosadmin/help-wanted. Fetches package repos with
#' issues labelled help-wanted (and variants). Saves output to issues.json for
#' use by helpwanted workflows including https://ropensci.org/help-wanted.
#'
#' @param min_date Date. Fetch issues since this data.
#' @param throttle Numeric. Maximum requests per second. Default throttled to 10
#' requests per minute which is the max rate for unauthenticated GitHub Pats
#' (i.e. on GH actions).
#' @param path Character. Filename/path to save json of issues to.
#'
#' @inheritParams common_docs
#'
#' @returns Creates/updates issues json list at `path`
#'
#' @export
#'
#' @examplesIf interactive()
#' hw_issues()

hw_issues <- function(
  min_date = Sys.Date() - lubridate::years(2),
  throttle = 10 / 60,
  path = "issues.json",
  verbose = TRUE
) {
  # Ignore packages which use help-wanted labels in a different way
  pkgs_ignore <- c("plotly", "opentripplanner")

  # GH label search not case sensitive
  labels_help <- c("help", "help wanted", "help-wanted", "help_wanted")
  labels_first <- c(
    "good first issue",
    "beginner",
    "good-first-issue",
    "good_first_issue"
  )
  labels_regex <- paste0("(", c(labels_help, labels_first), ")", collapse = "|")

  labels_gh <- glue::glue("\"{labels_help}\"") |>
    glue::glue_collapse(sep = ",")

  pkgs <- pkgs_ru() |>
    dplyr::filter(!package %in% pkgs_ignore)

  repos <- pkgs |>
    dplyr::filter(!stringr::str_detect(.data$owner, "ropensci")) |>
    dplyr::mutate(
      repos = stringr::str_remove(.data$url, "https://github.com/"),
      repos = dplyr::if_else(
        package == "assertr",
        paste0(.data$owner, "/", .data$package),
        repos
      )
    ) |>
    dplyr::pull(.data$repos)

  orgs <- pkgs$owner |>
    unique() |>
    stringr::str_subset("ropensci")

  cli::cli_inform("Fetch issues")

  # rOpenSci repos (Get all)
  i_ro <- gh::gh(
    "/search/issues",
    q = glue::glue(
      'org:{paste0(orgs, collapse = ",")} label:{labels_gh} state:open updated:>={min_date}'
    ),
    .progress = TRUE,
    .max_rate = throttle,
    .limit = Inf
  )$items

  # Non-ropensci repos (get specific)
  i_extra <- purrr::map(repos, \(r) {
    gh::gh(
      "/search/issues",
      q = glue::glue(
        'repo:{r} label:{labels_gh} state:open updated:>={min_date}'
      ),
      .progress = TRUE,
      .max_rate = throttle,
      .limit = Inf
    )$items
  }) |>
    purrr::compact() |>
    purrr::list_flatten()

  # Combine and extract details
  i <- append(i_ro, i_extra) |>
    purrr::map(\(i) {
      l <- purrr::map(i$labels, "name") |>
        stringr::str_subset(stringr::regex(labels_regex, ignore_case = TRUE)) |>
        stringr::str_replace_all("-|_", " ") |>
        tolower()

      dplyr::tibble(
        repo_url = i$repository_url,
        package = stringr::str_extract(.data$repo_url, "[^/]+$"),
        owner = stringr::str_remove_all(
          .data$repo_url,
          paste0("https://api.github.com/repos/|", "/", .data$package)
        ),
        issue_url = url_from_api(i$url),
        title = i$title,
        opened = i$created_at,
        updated = i$updated_at,
        labels = list(l),
        labels_github = i$user$login,
        labels_first = any(stringr::str_detect(l, "good first issue"))
      )
    }) |>
    purrr::list_rbind()

  # Filter to those in pkgs and get maintainer names/github
  i <- i |>
    dplyr::filter(lubridate::as_date(.data$updated) >= .env$min_date) |>
    dplyr::inner_join(
      dplyr::select(
        pkgs,
        "package",
        "url",
        "maintainer_github",
        "maintainer_name"
      ),
      by = "package"
    ) |>
    dplyr::arrange(.data$updated)

  # Add/fetch missing author information
  i <- i |>
    monarch::add_handles(
      primary = "github",
      prefix = "labels_",
      which_cols = "name"
    ) |>
    monarch::add_handles(
      primary = "name",
      pkg_col = "package",
      owner_col = "owner",
      prefix = "maintainer_",
      which_cols = "github"
    )

  # Need:
  # package, pkg_author, title (issue), labels (list), username (issue author), url (issue),
  # opened (issue, date), updated (issue, date), label_created (date), labels_first (T/F for good first)
  # author (name of issue author)

  jsonlite::write_json(
    i,
    path,
    pretty = TRUE,
    auto_unbox = TRUE
  )
}
