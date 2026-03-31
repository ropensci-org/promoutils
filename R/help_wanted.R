#' Fetch help wanted issues
#'
#' @param min_date Character/Date. Earliest date a help-wanted label was added to an issue to be included.
#' @param json Character. Location of the issues.json file to use.
#'
#' @returns Data frame of help wanted issues
#'
#' @export
#' @examples
#' h <- help_fetch("2025-01-01")

help_fetch <- function(
  min_date,
  json = "/repos/rosadmin/help-wanted/contents/issues.json"
) {
  min_date <- lubridate::ymd(min_date)

  help <- tryCatch(
    {
      httr2::request("https://api.github.com") |>
        httr2::req_url_path_append(json) |>
        httr2::req_auth_bearer_token(gh::gh_token()) |>
        httr2::req_perform() |>
        httr2::resp_body_json() |>
        purrr::pluck("download_url") |>
        jsonlite::fromJSON()
    },
    error = \(e) data.frame()
  )

  pkgs <- pkgs_ru() |>
    dplyr::select(
      "package",
      "maintainer_name",
      "maintainer_github",
      "maintainer_mastodon"
    )

  help |>
    dplyr::filter(
      .data$label_created >= .env$min_date,
      .data$label_created < Sys.Date()
    ) |>
    dplyr::rename(
      "labeller_name" = "author",
      "labeller_github" = "username",
      "maintainer_name" = "pkg_author"
    ) |>
    dplyr::left_join(pkgs, by = c("package", "maintainer_name"))
}

#' Get social media handles for helpwanted issues
#'
#' @param help Data frame of help wanted issues.
#' @inheritParams common_docs
#'
#' @returns Data frame of help wanted issues with social median handles added
#'
#' @export
#' @examplesIf interactive()
#' h <- help_fetch("2025-01-01") |>
#'   help_handles()

help_handles <- function(help, force_masto = FALSE) {
  if (nrow(help) == 0) {
    cli::cli_inform("No Help Wanted")
    return(data.frame())
  }
  pkgs <- pkgs_ru()

  help <- help |>
    dplyr::left_join(
      dplyr::select(pkgs, "package", "owner"),
      by = "package"
    )

  # Get missing Github by Name and other handles for maintainers
  if (anyNA(help$maintainer_github)) {
    help <- monarch::add_handles(
      help,
      primary = "name",
      prefix = "maintainer_",
      pkg_col = "package",
      owner_col = "owner",
      force_masto = force_masto
    )
  } else {
    help <- monarch::add_handles(
      help,
      primary = "github",
      prefix = "maintainer_",
      force_masto = force_masto
    )
  }
  # Get handles by Github for Issue authors
  help <- monarch::add_handles(
    help,
    prefix = "labeller_",
    force_masto = force_masto
  )

  help
}

#' Create help-wanted `socials_post_issue()` command
#'
#' @param help Data frame. Formatted help-wanted issues including social media
#'   handles and arranged by platform upon which to post, output of
#'   `by_platform()`.
#' @param date_time Character/Date. When to post. Defaults to next Thursday if `NULL`.
#'
#' @inheritParams common_docs
#'
#' @returns Copies commands to clipboard, optionally prints if (`print = TRUE`).
#'
#' @export
#' @examplesIf interactive()
#' h <- help_fetch("2025-05-23") |>
#'   help_handles() |>
#'   by_platform()
#'
#' help_post(h, print = TRUE)  # For non-interactive examples

help_post <- function(help, date_time = NULL, dry_run = FALSE, print = FALSE) {
  #TODO: Use dictionary?

  if (nrow(help) == 0) {
    message("No new help-wanted issues to post")
  }

  h <- help |>
    dplyr::mutate(
      labels_first = dplyr::if_else(
        .data$labels_first,
        glue::glue(
          "\U1F4A1 A great way to learn with this 'good first issue'! {\U1F4A1}\n\n\n"
        ),
        glue::glue("")
      )
    ) |>
    dplyr::arrange(dplyr::desc(.data$label_created)) |>
    dplyr::mutate(
      type = dplyr::if_else(
        stringr::str_detect(.data$title, "New Maintainer Wanted"),
        "maintainer",
        "regular"
      ),
      not_maint = is.na(.data$maintainer_github) |
        any(.data$maintainer_github != .data$labeller_github)
    )

  if (any(h$type == "maintainer")) {
    drafts_maintainer <- h |>
      dplyr::filter(.data$type == "maintainer") |>
      dplyr::select("package", "url", "platform") |>
      dplyr::distinct() |>
      dplyr::mutate(
        how_many = dplyr::if_else(
          dplyr::n_distinct(.data$package) == 1,
          "An rOpenSci package is",
          "rOpenSci packages are"
        ),
        opening = glue::glue(
          "[help wanted] {how_many} looking for maintainer help \U1F64F\n\n",
          "The following packages are looking for a new maintainer (or maintainer team).\n",
          "Check out their issues to see about getting involved\n\n"
        ),
        body = glue::glue("{package} - {url}")
      ) |>
      dplyr::summarize(
        draft = paste0(
          .data$opening[1],
          "\n",
          glue::glue_collapse(.data$body, sep = "\n")
        ),
        .by = "platform"
      ) |>
      dplyr::arrange(.data$platform)
  } else {
    drafts_maintainer <- data.frame()
  }

  if (any(h$type == "regular")) {
    n_pkgs <- dplyr::n_distinct(h$package[h$type == "regular"])
    how_many <- dplyr::if_else(
      n_pkgs == 1,
      "An rOpenSci package is",
      "rOpenSci packages are"
    )
    opening <- glue::glue(
      "[help wanted] {how_many} looking for some help! \U1F64F\n\n"
    )

    drafts_regular <- h |>
      dplyr::filter(.data$type == "regular") |>
      dplyr::mutate(
        pkg = glue::glue("\U1F4E6 {package}"),
        body = glue::glue(
          "'{title}' at {url} (by {labeller}) {labels_first}"
        ),
        space = dplyr::if_else(.env$n_pkgs == 1, "\n", ""),
        pkg = dplyr::if_else(
          .data$not_maint,
          glue::glue("{pkg} (maintained by {maintainer})"),
          .data$pkg
        )
      ) |>
      dplyr::summarize(
        n = dplyr::n(),
        body = glue::glue_collapse(.data$body, sep = "\n"),
        subopening = dplyr::if_else(
          .data$n == 1,
          "Check out this 'help wanted' issue:\n",
          "Check out 'help wanted' issues:\n"
        ),
        body = glue::glue(
          "{pkg[1]}\n{space[1]}{subopening[1]}{body}{space[1]}"
        ),
        .by = c("platform", "package")
      )

    drafts_regular <- drafts_regular |>
      dplyr::mutate(
        nchar = nchar(.data$body),
        cum_char = cumsum(.data$nchar),
        n_post = (.data$cum_char + nchar(.env$opening)) / 500,
        n_post = ceiling(.data$n_post),
        n_post = dplyr::if_else(.data$platform == "linkedin", 1, .data$n_post),
        .by = "platform"
      ) |>
      dplyr::summarize(
        body = glue::glue_collapse(.data$body, sep = "\n\n"),
        draft = glue::glue("{opening}\n{body}"),
        .by = c("platform", "n_post")
      ) |>
      dplyr::arrange(.data$platform, .data$n_post)
  } else {
    drafts_regular <- data.frame()
  }

  # Next Thursday
  date_time <- date_time %||% post_time("Thursday", 8)

  cmd <- glue::glue(
    "# {Sys.Date()} ------------------------------------------\n",
    .trim = FALSE
  )

  if (nrow(drafts_maintainer) > 0) {
    cmd <- c(
      cmd,
      glue::glue(
        "## Maintainers - {drafts_maintainer$platform}\npromoutils::socials_post_issue(
    time = \"{date_time}\", tz = \"America/Vancouver\",
    where = \"{drafts_maintainer$platform}\", dry_run = {dry_run},
    over_char_limit = cli::cli_warn,
    title = \"help-wanted - maintainer\",
    body = \"{drafts_maintainer$draft}\n\")\n\n",
        .trim = FALSE
      )
    )
  }

  if (nrow(drafts_regular) > 0) {
    cmd <- c(
      cmd,
      glue::glue(
        "## Regular - {drafts_regular$platform}\npromoutils::socials_post_issue(
    time = \"{date_time}\", tz = \"America/Vancouver\",
    where = \"{drafts_regular$platform}\", dry_run = {dry_run},
    over_char_limit = cli::cli_warn,
    title = \"help-wanted\",
    body = \"{drafts_regular$draft}\n\")\n",
        .trim = FALSE
      )
    )
  }

  cmd <- glue::glue_collapse(cmd, "\n")

  copy(cmd, "Help-wanted post commands", print = print)
}
