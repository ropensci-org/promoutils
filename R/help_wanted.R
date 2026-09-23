#' Fetch help wanted issues
#'
#' @param min_date Character/Date. Earliest date a help-wanted label was added to an issue to be included.
#' @param json Character. Location of the issues.json file to use.
#'
#' @returns Data frame of help wanted issues
#'
#' @export
#' @examplesIf interactive()
#' h <- help_fetch()

help_fetch <- function(
  min_date = NULL,
  json = "/repos/rosadmin/help-wanted/contents/issues.json"
) {
  h_df <- help_read()
  if (!nrow(h_df)) {
    if (is.null(min_date)) {
      min_date <- lubridate::floor_date(Sys.Date(), "years")
    }
  } else {
    min_date <- max(h_df$updated)
  }

  min_date <- lubridate::as_date(min_date)

  h <- tryCatch(
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
  ) |>
    readr::type_convert(col_types = readr::cols()) |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      updated = dplyr::if_else(
        is.na(.data$updated),
        .data$opened,
        .data$updated
      )
    )

  pkgs <- pkgs_ru() |>
    dplyr::select(
      "package",
      "maintainer_name",
      "maintainer_github"
    )

  h <- h |>
    dplyr::filter(
      .data$updated >= .env$min_date,
      .data$updated < Sys.Date()
    ) |>
    dplyr::left_join(
      pkgs,
      by = c("package", "maintainer_name", "maintainer_github")
    ) |>
    dplyr::rename_with(tolower, .cols = dplyr::contains("github"))

  if (nrow(dplyr::anti_join(h, h_df, by = "issue_url"))) {
    h_df <- dplyr::rows_upsert(
      h_df,
      dplyr::select(h, "updated", "issue_url"),
      by = "issue_url"
    ) |>
      dplyr::mutate(posted = tidyr::replace_na(.data$posted, FALSE))
    cache_write(h_df, "help-wanted")
  } else {
    cli::cli_inform("No new help-wanted issues")
    return(invisible())
  }

  h
}


help_read <- function() {
  h <- cache_read("help-wanted")
  if (nrow(h)) {
    if (!"posted" %in% names(h)) h$posted <- FALSE
  } else {
    h <- dplyr::tibble(
      updated = lubridate::as_datetime(Sys.time()),
      issue_url = "",
      posted = FALSE,
      .rows = 0
    )
  }
  h
}

#' Get a data frame of social media handles for help-wanted issues
#'
#' @param help Data frame of help wanted issues.
#'
#' @returns Data frame of social media handles associated with `help`
#'
#' @export
#' @examplesIf interactive()
#' help_fetch() |>
#'   help_handles()

help_handles <- function(help) {
  if (nrow(help) == 0) {
    cli::cli_inform("No Help Wanted")
    return(data.frame())
  }
  pkgs <- pkgs_ru()

  h <- help |>
    dplyr::left_join(
      dplyr::select(pkgs, "package", "owner"),
      by = c("package", "owner")
    )

  # Get missing Github by Name and other handles for maintainers
  if (anyNA(help$maintainer_github)) {
    help <- monarch::add_handles(
      help,
      primary = "name",
      prefix = "maintainer_",
      pkg_col = "package",
      owner_col = "owner"
    )
  } else {
    help <- monarch::add_handles(
      help,
      primary = "github",
      prefix = "maintainer_"
    )
  }

  # Get handles by Github for Issue authors
  help <- monarch::add_handles(help, prefix = "labels_")

  # Return handles list
  h <- list()
  for (i in c("maintainer", "labels")) {
    h[[i]] <- help |>
      dplyr::select(dplyr::matches("name|github|mastodon|bluesky|linkedin")) |>
      dplyr::select(dplyr::starts_with(i)) |>
      dplyr::rename_with(\(x) stringr::str_remove(x, paste0(i, "_")))
  }
  h <- purrr::list_rbind(h) |>
    dplyr::distinct()
  h
}

#' Prepare help-wanted posts
#'
#' Prepare posts and preview
#'
#' @param help Data frame. Formatted help-wanted issues including social media
#'   handles.
#'
#' @returns Copies commands to clipboard, optionally prints if (`print = TRUE`).
#'
#' @export
#' @examplesIf interactive()
#' h <- help_fetch()
#' help_preview(h)

help_preview <- function(help) {
  #TODO: Use dictionary?
  h_posted <- help_read() |>
    dplyr::filter(.data$posted)

  h <- help |>
    dplyr::anti_join(h_posted, by = "issue_url") |>
    dplyr::select("package", "issue_url", "title")

  if (nrow(h) == 0) {
    message("No new help-wanted issues to post")
    return(invisible())
  }

  cli::cli_h2("Preview")
  for (i in seq_len(nrow(h))) {
    cli::cli_inform("{i}. {h$package[i]} - {.url {h$issue_url[i]}}")
  }

  invisible()
}

#' Create help-wanted post
#'
#' @param help Data frame. Formatted help-wanted issues including social media
#'   handles.
#' @param date_time Character/Date. When to post. Defaults to next Thursday if
#'   `NULL`.
#'
#' @inheritParams common_docs
#'
#' @returns Copies commands to clipboard, optionally prints if (`print = TRUE`).
#'
#' @export
#' @examplesIf interactive()
#' h <- help_fetch()
#'
#' help_preview(h)
#' # Review and note issues to skip
#'
#' help_post(h, skip = c(1:6), dry_run = TRUE)  # For non-interactive examples

help_post <- function(help, skip = NULL, date_time = NULL, dry_run = FALSE) {
  if (!dry_run) {
    h_skip <- dplyr::select(help, "updated", "issue_url") |>
      dplyr::slice(.env$skip) |>
      dplyr::mutate(posted = TRUE)
    dplyr::rows_upsert(help_read(), h_skip, by = "issue_url") |>
      cache_write("help-wanted")
  }

  h_posted <- help_read() |>
    dplyr::filter(.data$posted)

  h <- help |>
    dplyr::anti_join(h_posted, by = "issue_url") |>
    dplyr::mutate(
      labels_first = dplyr::if_else(
        .data$labels_first,
        "\U1F4A1 A great way to learn with this 'good first issue'! \U1F4A1\n\n\n",
        ""
      )
    ) |>
    dplyr::arrange(dplyr::desc(.data$updated)) |>
    dplyr::mutate(
      type = dplyr::if_else(
        stringr::str_detect(tolower(.data$title), "maintainer"),
        "maintainer",
        "regular"
      ),
      not_maint = is.na(.data$maintainer_github) |
        any(.data$maintainer_github != .data$labels_github)
    )

  if (!nrow(h)) {
    cli::cli_inform("No issues left to post")
    return(invisible)
  }

  if (any(h$type == "maintainer")) {
    drafts_maintainer <- h |>
      dplyr::filter(.data$type == "maintainer") |>
      dplyr::select("package", "issue_url") |>
      dplyr::distinct() |>
      dplyr::mutate(
        packages = dplyr::if_else(
          dplyr::n_distinct(.data$package) == 1,
          "An rOpenSci package is",
          "rOpenSci packages are"
        ),
        issues = dplyr::if_else(
          dplyr::n_distinct(.data$package) == 1,
          "issue",
          "issues"
        ),
        opening = glue::glue(
          "[help wanted] {packages} looking for a new (co-)maintainer (or maintainer team) \U1F64F\n\n",
          "Check out the {issues} to see about getting involved:\n\n"
        ),
        body = glue::glue(" \U1F4E6 {package} - {issue_url}")
      ) |>
      dplyr::summarize(
        draft = paste0(
          .data$opening[1],
          "\n",
          glue::glue_collapse(.data$body, sep = "\n")
        )
      ) |>
      dplyr::pull(.data$draft)
  } else {
    drafts_maintainer <- NULL
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
        n = dplyr::n(),
        pkg = glue::glue("\U1F4E6 {package}"),
        body = glue::glue(
          "'{title}' at {issue_url} (by {{`{labels_github}`}}) {labels_first}"
        ),
        space = dplyr::if_else(.env$n_pkgs == 1, "\n", ""),
        pkg = glue::glue("{pkg} (maintained by {{`{maintainer_github}`}})")
      ) |>
      dplyr::summarize(
        body = glue::glue_collapse(.data$body, sep = "\n"),
        subopening = dplyr::if_else(
          .data$n[1] == 1,
          "Check out this 'help wanted' issue:\n",
          "Check out 'help wanted' issues:\n"
        ),
        body = glue::glue(
          "{pkg[1]}\n{space[1]}{subopening[1]}{body}{space[1]}"
        ),
        .by = "package"
      )

    drafts_regular <- drafts_regular |>
      dplyr::summarize(
        body = glue::glue_collapse(.data$body, sep = "\n\n"),
        draft = glue::glue("{opening}\n{body}")
      ) |>
      dplyr::pull(.data$draft)
  } else {
    drafts_regular <- NULL
  }

  handles <- help_handles(help)
  handles <- purrr::pmap(handles, list) |>
    rlang::set_names(handles$github)

  # Next Thursday
  date_time <- date_time %||% post_time("Thursday", 8)

  if (!is.null(drafts_maintainer)) {
    promoutils::buffer_posts_write(
      when = date_time,
      tz = "America/Vancouver",
      dry_run = dry_run,
      body = drafts_maintainer
    )
  }

  if (!is.null(drafts_regular)) {
    promoutils::buffer_posts_write(
      when = date_time,
      tz = "America/Vancouver",
      dry_run = dry_run,
      body = drafts_regular,
      handles = handles
    )
  }

  if (!dry_run) {
    dplyr::rows_upsert(
      help_read(),
      dplyr::select(h, "updated", "issue_url") |> dplyr::mutate(posted = TRUE),
      by = "issue_url"
    ) |>
      cache_write("help-wanted")
  }
}
