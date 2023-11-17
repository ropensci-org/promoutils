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
#'   #RStats and @rstats@a.gup.pe will be appended
#' @param where Character vector. Either `mastodon` and/or `linkedin` to
#'   specifty which platforms this should be posted on.
#' @param dry_run Logical. Whether to perform a dry run (do not post, but
#'   display draft if `verbose = TRUE`).
#' @param avoid_dups Logical. Don't post an issue if any open issue has the
#'  same title.
#' @param verbose Logical. If dry run, displace draft?
#'
#' @export
socials_post_issue <- function(time, tz, title, body, where = "mastodon",
                               avoid_dups = TRUE, dry_run = FALSE,
                               verbose = FALSE) {

  if(!all(where %in% c("mastodon", "linkedin"))) {
    stop("'where' must be one of 'mastodon' or 'linkedin'", call. = FALSE)
  }

  if(!tz %in% OlsonNames()) stop("Couldn't detect timezone", call. = FALSE)

  date <- lubridate::as_date(time)
  labels <- c(where, "draft", "needs-review")
  title <- glue::glue("[Post] - {title} - {date}")
  body <- glue::glue(
    "~~~",
    "time: {time}",
    "tz: {tz}",
    "~~~",
    "",
    "{body}",
    "",
    "#RStats",
    "@rstats@a.gup.pe", .sep = "\n")

  if(dry_run & verbose) {
    message("labels: ", paste0(labels, collapse = ", ") , "\n\n", title, "\n\n", body)
  }

  gh_issue_post(title, body,
                labels = labels,
                owner = "rosadmin", repo = "scheduled_socials",
                avoid_dups = avoid_dups, dry_run = dry_run)
}
