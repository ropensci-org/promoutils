#' Create a draft issue to post to Mastodon and LinkedIn
#'
#' Formats the body and title of an issue and posts it on
#' "rosadmin/scheduled_socials". The issue will bed opened in a browser for
#' editing and confirmation. Note that issues will not be posted until the
#' labels "draft" and "needs-review" have been removed.
#'
#' @param time Date/time. Date and time at which the post should be made
#' @param tz Character. Timezone (from `OlsonNames()`) in which to post
#' @param title Character. Title of the post (`[Post]` and the date will be
#'   prepended and appended
#' @param body Character. Text to be posted (omit the YAML for posting info;
#'   #RStats and @rstats@a.gup.pe will be appended for Mastodon, #RStats for
#'   LinkedIn),
#'   *or* link to text file with both Mastodon and LinkedIn body text, headed by
#'   by ---- Mastodon ----- and --- LinkedIn -----.
#' @param where Character vector. Either `mastodon` and/or `linkedin` to
#'   specify which platforms this should be posted on.
#' @param add_hash Logical. Whether to automatically add the RStats hashtags.
#' @param dry_run Logical. Whether to perform a dry run (do not post, but
#'   display draft if `verbose = TRUE`).
#' @param avoid_dups Logical. Don't post an issue if any open issue has the
#'  same title.
#' @param
#' @param verbose Logical. If dry run, displace draft?
#'
#' @export
socials_post_issue <- function(time, tz = "America/Winnipeg",
                               title, body, where = "mastodon",
                               avoid_dups = TRUE, add_hash = TRUE,
                               dry_run = FALSE, open_browser = TRUE,
                               verbose = FALSE) {

  if(!all(where %in% c("mastodon", "linkedin"))) {
    stop("'where' must be one of 'mastodon' or 'linkedin'", call. = FALSE)
  }

  if(!tz %in% OlsonNames()) stop("Couldn't detect timezone", call. = FALSE)

  date <- lubridate::as_date(time)
  title <- glue::glue("[Post] - {title} - {date}")

  if(file.exists(body)) {
    body <- readr::read_lines(body)
    n <- stringr::str_which(body, "--- (Mastodon)|(LinkedIn) ---")
    if(length(n) == 2) {
      where <- c("mastodon", "linkedin")
      body <- list(body[(n[1]+1):(n[2]-1)],
                   body[(n[2]+1):(length(body))])
    } else if(length(n) == 1) {
      body <- list(body[(n[1]+1):(length(body))])
    } else {
      n <- 0
      body <- list(body[(n[1]+1):(length(body))])
    }
    body <- purrr::map(body, \(x) glue::glue_collapse(x, sep = "\n"))
  }

  purrr::map2(body, where, \(x, y) {
    socials_post_single(time, tz, title, x, y,
                        avoid_dups, add_hash, dry_run, open_browser, verbose)
  })
}


socials_post_single <- function(time, tz, title, body, where, avoid_dups,
                                add_hash, dry_run, open_browser, verbose) {

  labels <- c(where, "draft", "needs-review")

  if(add_hash) {
    hash <- c("mastodon" = "\n#RStats\n@rstats@a.gup.pe", "linkedin" = "\n#RStats")
    hash <- hash[where]
    body <- glue::glue("{body}\n{hash}")
  }


  if((n <- nchar(body, type = "width")) >= 490) {
    stop("Very close or over the character limit of 500\n",
         "(this message has ", n, " including hashtags)",
         call. = FALSE)
  }

  body <- glue::glue(
      "~~~",
      "time: {time}",
      "tz: {tz}",
      "~~~\n",
      "{body}",  .sep = "\n") |>
    stringr::str_replace("\n\n", "\n")

  if(dry_run & verbose) {
    message("labels: ", paste0(labels, collapse = ", ") , "\n\n", title, "\n\n", body)
  }

  gh_issue_post(title, body,
                labels = labels,
                owner = "rosadmin", repo = "scheduled_socials",
                avoid_dups = avoid_dups, dry_run = dry_run,
                open_browser = open_browser)
}
