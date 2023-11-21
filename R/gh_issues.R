gh_issue_post <- function(title, body, labels, owner, repo, avoid_dups = TRUE,
                          dry_run = FALSE) {

  if(missing(title)) stop("Require a title for this issue", call. = FALSE)
  if(missing(body)) stop("Require a body for this issue", call. = FALSE)

  if(avoid_dups) {
    current <- gh_issue_fetch(state = "all") |>
      gh_issue_fmt(which = c("title", "body", "labels")) |>
      tidyr::unnest("labels")

    if(title %in% current$title) {

      if(any(labels[!labels %in% c("draft", "needs-review")] %in% current$labels[current$title == title])) {
        message("Skipping duplicate - ", title,
                " (labels: ", paste0(labels, collapse = ", "), ")",
                dplyr::if_else(dry_run, " [DRY-RUN]", ""))
        return()
      }
    }
  }

  message("Posting issue - ", title, " (labels: ", paste0(labels, collapse = ", "), ")",
          dplyr::if_else(dry_run, " [DRY-RUN]", ""))

  if(!dry_run) {
    r <- gh::gh("POST /repos/{owner}/{repo}/issues",
                title = title, body = body, labels = as.list(labels),
                owner = owner, repo = repo)

    utils::browseURL(r$html_url)
  }
}

#' Fetch issues from a GH repository
#'
#' @param state Character. Which issues to fetch: "open", "closed", "all"
#' @param labels Character vector. Fetch only issues with these labels. (Optional)
#' @param since Character/Date/datetime. Fetch only issues since this date/time. (Optional)
#' @param owner Character. Owner of the repository
#' @param repo Character. Name of the repository (name of the package)
#' @param verbose Logical. Show progress messages.
#'
#' @return List of issues
#' @export
#'
#' @examples
#' i <- gh_issue_fetch()

gh_issue_fetch <- function(state = "open", labels = NULL, since = NULL,
                           owner = "rosadmin", repo = "scheduled_socials",
                           verbose = FALSE) {
  if(verbose) message("owner: ", owner, "; repo: ", repo)

  if(!is.null(since)) since <- format(lubridate::as_datetime(since), "%Y-%m-%dT%H:%M:%SZ")

  gh::gh("/repos/{owner}/{repo}/issues",
         state = state, labels = labels,
         since = since,
         sort = "created", direction = "desc",
         owner = owner, repo = repo, .limit = Inf)
}


#' Format issues list from GH
#'
#' @param i List of issues from `gh_issue_fetch()`
#' @param which Which fields to includ
#'
#' @return Issues formated as a data frame
#' @export
#'
#' @examples
#' i <- gh_issue_fetch()
#' i <- gh_issue_fmt(i, which = "title")
gh_issue_fmt <- function(i, which = c("title", "number", "body", "labels", "url",
                                  "created", "updated", "gh_user_issue")) {

  df <- dplyr::tibble(url = purrr::map_chr(i, "url"),
                      number = purrr::map_dbl(i, "number"),
                      labels = purrr::map(i, "labels"),
                      title = purrr::map_chr(i, "title"),
                      created = purrr::map_chr(i, "created_at"),
                      updated = purrr::map_chr(i, "updated_at"),
                      body = purrr::map_chr(i, "body", .default = ""),
                      gh_user_issue = purrr::map_chr(i, \(x) x[["user"]]$login)) |>
    dplyr::select(dplyr::any_of(which))


  if("labels" %in% which) {
    df <- dplyr::mutate(df, labels = purrr::map_depth(.data$labels, 2, "name"),
                        n_labels = purrr::map_int(.data$labels, length))
  }

  df
}



#' Extract information on help-wanted labels for issues
#'
#' @param i Data frame of issues from `gh_issue_fmt()`
#' @param labels_help Character. Regular expression to match help-wanted
#'   labels.
#' @param labels_first Character. Regular expression to meatch good-first-issue
#'   labels.
#'
#' @return data frame with added label details
#' @export
#'
#' @examples
#' i <- gh_issue_fetch(owner = "ropensci", repo = "weathercan")
#' i <- gh_issue_fmt(i)
#' i <- gh_issue_labels(i)

gh_issue_labels <- function(
    i,
    labels_help = "(help)|(help wanted)|(help-wanted)|(help_wanted)",
    labels_first = "(good first issue)|(beginner)|(good-first-issue)") {

  i |>
    dplyr::mutate(
      info = stringr::str_remove_all(.data$url, "(https://api.github.com/repos/)|(/issues/\\d+)"),
      owner = stringr::str_extract(.data$info, "^[^/]*"),
      repo = stringr::str_extract(.data$info, "[^/]*$"),
      labels_help = purrr::map_lgl(
        .data$labels,
        \(x) any(stringr::str_detect(tolower(x), .env$labels_help))),
      labels_first = purrr::map_lgl(
        .data$labels,
        \(x) any(stringr::str_detect(tolower(x), .env$labels_first)))) |>
    dplyr::filter(.data$labels_help) |>
    dplyr::mutate(
      events = purrr::pmap(
        list(.data$owner, .data$repo, .data$number),
        \(x, y, z) gh_label_events(x, y, z, labels = .env$labels_help))) |>
    tidyr::unnest("events", keep_empty = TRUE)

}



gh_label_events <- function(owner, repo, issue, labels) {
  events <- gh::gh(endpoint = "/repos/{owner}/{repo}/issues/{issue}/events",
                   owner = owner, repo = repo, issue = issue)

  e <- dplyr::tibble(event = purrr::map_dbl(events, "id"),
                     type = purrr::map_chr(events, "event"),
                     label = purrr::map(events, "label"),
                     gh_user = purrr::map_chr(events, ~.[["actor"]]$login),
                     label_created = purrr::map_chr(events, "created_at")) |>
    dplyr::filter(.data$type == "labeled") |>
    tidyr::unnest("label")

  # Get only the events reflecting a specific 'label' pattern
  if(nrow(e) > 0) {
    e |>
      tidyr::unnest(.data$label) |>
      dplyr::filter(stringr::str_detect(.data$label, .env$labels)) |>
      dplyr::mutate(label_created = lubridate::ymd_hms(.data$label_created)) |>
      dplyr::arrange(dplyr::desc(.data$label_created)) |>
      dplyr::slice(1) |>
      dplyr::select("gh_user_labelled" = "gh_user", "label_created")
  } else data.frame()
}



