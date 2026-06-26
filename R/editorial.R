#' Create Word lists for spelling dictionaries
#'
#' Creates a word list of packages, and/or maintainer, and/or contributor names.
#' If wordlist already exists will update existing list.
#'
#' @param which Character vector. Which words to create lists for? Any of
#'   "packages", "maintainers", "contributors".
#' @param path Character. File path to save wordlist to.
#'
#' @returns Path of new wordlist
#'
#' @export
#' @examplesIf interactive()
#' wordlist_create()

wordlist_create <- function(
  which = c("packages", "maintainers"),
  path = ".wordlists/names.txt"
) {
  p <- pkgs_ru()

  words <- c()
  if ("packages" %in% which) {
    pkgs <- p$package
    words <- c(words, pkgs)
  }

  if ("maintainers" %in% which) {
    nms_maint <- p |>
      dplyr::select(
        "github" = "maintainer_github",
        "name" = "maintainer_name"
      ) |>
      dplyr::distinct() |>
      monarch::add_handles(
        primary = "github",
        which_cols = "name"
      ) |>
      dplyr::pull(.data$name)
    words <- c(words, nms_maint)
  }

  if ("contributors" %in% which) {
    nms_ctrb <- p |>
      tidyr::unnest("contributors") |>
      dplyr::select("github" = "contributor_github") |>
      dplyr::distinct() |>
      dplyr::filter(.data$github != "copilot") |>
      monarch::add_handles(
        primary = "github",
        which_cols = "name"
      ) |>
      dplyr::pull(.data$name)
    words <- c(words, stringr::str_split(nms_ctrb, " +"))
  }

  wordlist_update(words, path)
}


#' Add specific words to the wordlist
#'
#' @param words Character vector. Words to add to word list, note that these are
#' all split into individual words by spaces.
#' @param path Character. Where the word list is to be saved/updated.
#'
#' @returns Path of word list
#'
#' @export

wordlist_update <- function(
  words,
  path = ".wordlist/names.txt"
) {
  words <- stringr::str_split(words, " +") |>
    unlist() |>
    stringr::str_trim() |>
    unique()

  if (!file.exists(dirname(path))) {
    dir.create(dirname(path))
  }

  # Merge with existing wordlists
  if (file.exists(path)) {
    cli::cli_inform(
      "There is an existing word list at this path, merging lists..."
    )
    words <- c(readr::read_lines(path), words)
  }

  words <- words |>
    unique() |>
    sort()

  cli::cli_inform("Remember to add this dictionary to your spell check config")

  readr::write_lines(words, path)

  path
}
