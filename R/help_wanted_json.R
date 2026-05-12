#' Create help wanted JSON file
#'
#' Function created for rosadmin/help-wanted. Fetches ropensci repos with
#' issues labelled help-wanted. Saves output to issues.json for use by
#' helpwanted workflows including https://ropensci.org/help-wanted.
#'
#' @returns Creates/updates `issues.json`
#'
#' @export

help_wanted_json <- function() {
  min_date <- Sys.Date() - lubridate::years(2)

  pkgs_ignore <- c("plotly", "opentripplanner") # Uses help-wanted in a different way

  labels_help <- "(help)|(help wanted)|(help-wanted)|(help_wanted)"
  labels_first <- "(good first issue)|(beginner)|(good-first-issue)"

  issues <- dplyr::tibble(owner = c("ropensci", "ropenscilabs")) |>
    dplyr::mutate(
      repos = purrr::map(.data$owner, \(x) {
        gh::gh(glue::glue("/users/{x}/repos"), .limit = Inf)
      })
    ) |>
    dplyr::mutate(
      info = purrr::map(.data$repos, \(x) {
        dplyr::tibble(
          package = purrr::map_chr(x, "name"),
          open_issues = purrr::map_dbl(x, "open_issues")
        ) |>
          dplyr::filter(.data$open_issues > 0)
      })
    ) |>
    dplyr::select(-"repos") |>
    tidyr::unnest("info") |>
    dplyr::filter(!.data$package %in% .env$pkgs_ignore) |>
    dplyr::mutate(
      issues = purrr::map2(.data$owner, .data$package, \(x, y) {
        gh_issue_fetch(owner = x, repo = y, since = min_date)
      }),
      issues = purrr::map(.data$issues, gh_issue_fmt),
      issues = purrr::map(.data$issues, \(x) {
        gh_issue_labels(x, labels_help, labels_first) |>
          dplyr::select(-"owner", -"repo")
      })
    )

  pkgs <- pkgs_ru()

  issues_clean <- issues |>
    tidyr::unnest("issues") |>
    dplyr::filter(lubridate::as_date(.data$label_created) >= .env$min_date) |>
    dplyr::mutate(
      url = stringr::str_remove_all(
        .data$url,
        glue::glue("(api\\.)|(repos\\/)")
      ),
      title = stringr::str_remove_all(.data$title, "`"),
      issue_created = lubridate::ymd_hms(.data$created),
      maintainer_name = purrr::map_chr(.data$package, \(x) pkg_authors(x, pkgs))
    ) |>
    dplyr::select(
      "owner",
      "package",
      "maintainer_name",
      "title",
      "labels",
      "labels_github" = "gh_user_issue",
      "url",
      "opened" = "issue_created",
      "updated",
      "label_created",
      "labels_first"
    ) |>
    dplyr::arrange(.data$label_created)

  # Get authors from any existing lists
  old_list <- "issues.json"
  if (file.exists(old_list)) {
    old_list <- jsonlite::fromJSON(old_list) |>
      dplyr::as_tibble() |>
      dplyr::rename("labels_github" = "username", "labels_name" = "author")

    author_index <- dplyr::select(old_list, "labels_name", "labels_github") |>
      dplyr::distinct()

    issues_clean <- dplyr::left_join(
      issues_clean,
      author_index,
      by = "labels_github"
    )
  } else {
    issues_clean <- dplyr::mutate(issues_clean, labels_name = NA_character_)
  }

  # Get any more missing authors
  issues_clean <- monarch::add_handles(
    issues_clean,
    primary = "github",
    pkg_col = "package",
    owner_col = "owner",
    prefix = "labels_"
  ) |>
    dplyr::select(-"labels_mastodon", -"labels_linkedin") |>
    dplyr::rename("username" = "labels_github", "author" = "labels_name")

  jsonlite::write_json(
    issues_clean,
    "issues.json",
    pretty = TRUE,
    auto_unbox = TRUE
  )
}
