#' Fetch help wanted issues
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
    dplyr::filter(label_created >= .env$min_date, label_created < Sys.Date()) |>
    dplyr::rename(
      "labeller_github" = "username",
      "maintainer_name" = "pkg_author"
    ) |>
    dplyr::left_join(pkgs, by = c("package", "maintainer_name"))
}

#' Title
#'
#' @param help
#'
#' @returns
#'
#' @export
#' @examples
#' h <- help_fetch("2025-01-01") |>
#'   help_handles()

help_handles <- function(help, force_masto = FALSE) {
  help |>
    # Get missing Github by Name and other handles for maintainers
    monarch::add_handles(
      primary = "name",
      prefix = "maintainer_",
      pkg_col = "package",
      force_masto = force_masto
    ) |>
    # Get handles by Github for Issue authors
    monarch::add_handles(
      prefix = "labeller_",
      force_masto = force_masto
    )
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
#' @examples
#' h <- help_fetch("2025-05-23") |>
#'   help_handles() |>
#'   by_platform()
#'
#' help_post(h)

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
          "{emo::ji('idea')}",
          " A great way to learn with this 'good first issue'! ",
          "{emo::ji('idea')}\n\n\n"
        ),
        glue::glue("")
      )
    ) |>
    dplyr::arrange(dplyr::desc(label_created)) |>
    dplyr::mutate(
      type = dplyr::if_else(
        stringr::str_detect(title, "New Maintainer Wanted"),
        "maintainer",
        "regular"
      ),
      not_maint = is.na(maintainer_github) |
        any(maintainer_github != labeller_github)
    )

  if (any(h$type == "maintainer")) {
    drafts_maintainer <- h |>
      dplyr::filter(type == "maintainer") |>
      dplyr::select("package", "url", "platform") |>
      dplyr::distinct() |>
      dplyr::mutate(
        how_many = dplyr::if_else(
          dplyr::n_distinct(.data$package) == 1,
          "An rOpenSci package is",
          "rOpenSci packages are"
        ),
        opening = glue::glue(
          "[help wanted] {how_many} looking for maintainer help {emo::ji('folded hands')}\n\n",
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
      )
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
      "[help wanted] {how_many} looking for some help! {emo::ji('folded hands')}\n\n"
    )
    browser()
    drafts_regular <- h |>
      dplyr::filter(type == "regular") |>
      dplyr::mutate(
        pkg = glue::glue("{emo::ji('package')} {package}"),
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
    # TODO: FIXME!!!!!!!!!!!!!!----------------------------
    drafts_regular |>
      dplyr::mutate(
        nchar = nchar(body),
        cum_char = cumsum(nchar),
        cum_char_adj = dplyr::if_else(
          cum_char + nchar(opening) > 500,
          cum_char - 500,
          cum_char
        ),
        cum_char_adj2 = dplyr::if_else(
          cum_char_adj + nchar(opening) > 500,
          cum_char_adj - 500,
          cum_char_adj
        ),
        cum_char_adj3 = dplyr::if_else(
          cum_char_adj2 + nchar(opening) > 500,
          cum_char_adj2 - 500,
          cum_char_adj2
        ),
        cum_char_adj3 = cum_char_adj3 + nchar(opening),
        post_n = ceiling(cum_char_adj3 / 500)
      ) |>
      dplyr::select(-"body")
    dplyr::summarize(
      body = glue::glue_collapse(.data$body, sep = "\n\n"),
      draft = glue::glue("{opening}\n{body}"),
      .by = "platform"
    )
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
    cmd <- glue::glue(
      "{cmd}## Maintainers\npromoutils::socials_post_issue(
    time = \"{date_time}\", tz = \"America/Vancouver\",
    where = \"{drafts_maintainer$platform}\", dry_run = {dry_run},
    over_char_limit = cli::cli_warn,
    title = \"help-wanted - maintainer\",
    body = \"{drafts_maintainer$draft}\n\")\n\n",
      .trim = FALSE
    )
  }

  if (nrow(drafts_regular) > 0) {
    cmd <- glue::glue(
      "{cmd}## Regular\npromoutils::socials_post_issue(
    time = \"{date_time}\", tz = \"America/Vancouver\",
    where = \"{drafts_regular$platform}\", dry_run = {dry_run},
    over_char_limit = cli::cli_warn,
    title = \"help-wanted\",
    body = \"{drafts_regular$draft}\n\")\n",
      .trim = FALSE
    )
  }

  copy(cmd, "Help-wanted post commands", print = print)
}
