gh_issue_post <- function(title, body, labels, owner, repo, avoid_dups = TRUE) {

  if(missing(title)) stop("Require a title for this issue", call. = FALSE)
  if(missing(body)) stop("Require a body for this issue", call. = FALSE)

  if(avoid_dups) {
    current <- gh_issue_get(state = "all") |>
      issue_df()

    if(title %in% current$title) {
      if(any(labels[!labels %in% c("draft", "needs-review")] %in% current$labels[current$title == title])) {
        message("Skipping duplicate - ", title, " (labels: ", paste0(labels, collapse = ", "), ")")
        return()
      }
    }
  }

  message("Posting issue - ", title, " (labels: ", paste0(labels, collapse = ", "), ")")

  r <- gh::gh("POST /repos/{owner}/{repo}/issues",
              title = title, body = body, labels = as.list(labels),
              owner = owner, repo = repo)

  browseURL(r$html_url)
}

gh_issue_get <- function(state = "open", labels = NULL,
                         owner = "rosadmin", repo = "scheduled_socials") {
  gh::gh("/repos/{owner}/{repo}/issues",
         state = state, labels = labels,
         sort = "created", direction = "desc",
         owner = owner, repo = repo)
}

issue_df <- function(i) {
  purrr::map(i, \(x) purrr::keep_at(x, c("title", "body", "labels")) |> dplyr::as_tibble()) |>
    purrr::list_rbind() |>
    dplyr::mutate(labels = purrr::map_chr(labels, "name"))
}
