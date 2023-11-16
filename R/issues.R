gh_issue_post <- function(title, body, labels, owner, repo, avoid_dups = TRUE) {

  if(missing(title)) stop("Require a title for this issue", call. = FALSE)
  if(missing(body)) stop("Require a body for this issue", call. = FALSE)

  if(avoid_dups) {
    current <- purrr::map_chr(gh_issue_get(), "title")
    if(title %in% current) {
      message("Skipping duplicate - ", title)
      return()
    }
  }

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
