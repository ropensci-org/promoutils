#' Fetch and format use cases from a GitHub Discussion board
#'
#' @param owner Character. Repository owner.
#' @param name Character. Repository for discussions
#'
#' @returns Data frame of usecases
#'
#' @export
#' @examples
#' u <- uc_fetch()
#' u

uc_fetch <- function(owner = "ropensci", name = "discussions") {
  u <- gh::gh_gql(
    glue::glue(
      "query {
  repository(owner: \"{{owner}}\", name: \"{{name}}\") {
    discussions(
      first: 100
      orderBy: {field: CREATED_AT, direction: ASC}
    ) {
        nodes {
          id
          title
          url
          createdAt
          body
          bodyText
          author {
            login
          }
          category {
            name
          }
        }
      }
    }
  }",
      .open = "{{",
      .close = "}}"
    )
  )

  u |>
    purrr::pluck("data", "repository", "discussions", "nodes") |>
    purrr::map(\(u) {
      dplyr::as_tibble(u) |> tidyr::unnest(c("author", "category"))
    }) |>
    purrr::list_rbind() |>
    dplyr::mutate(date = lubridate::ymd_hms(.data$createdAt)) |>
    dplyr::rename("author_github" = "author")
}

#' Extract out resource information
#'
#' @param uc Data frame. Data frame of usecases from `uc_fetch()`.
#' @param min_date Character/Date. Minimum date of usecases to retain.
#' @param pkgs Data frame. Optional data frame of package details (defaults to `pkgs_ru()`).
#'
#' @returns Data frame of formatted use cases.
#'
#' @export
#' @examples
#' u <- uc_fetch() |>
#'   uc_fmt("2025-01-01")
#' u

uc_fmt <- function(uc, min_date, pkgs = NULL) {
  min_date <- lubridate::ymd_hms(min_date, truncated = 3)
  pkgs <- pkgs %||% pkgs_ru()

  u <- dplyr::filter(uc, .data$date >= .env$min_date)

  if (nrow(u) == 0) {
    return(data.frame())
  }

  u |>
    dplyr::mutate(
      resource_lst = stringr::str_extract(
        .data$bodyText,
        "((?<=package or resource used\\n).+)|((?<=Paquete o recurso rOpenSci utilizado\\n).+)"
      ),
      resource_lst = tolower(.data$resource_lst),
      resource_lst = stringr::str_extract_all(
        .data$resource_lst,
        paste0(tolower(pkgs$package), collapse = "|")
      ),
      #resource_lst = stringr::str_extract_all(.data$resource, "[^.,;]+"),
      resource_n = lengths(.data$resource_lst),
      resource = purrr::map(.data$resource_lst, stringr::str_squish)
    ) |>
    tidyr::unnest("resource") |>
    dplyr::left_join(
      dplyr::select(
        pkgs,
        "resource" = "package",
        "maintainer_name",
        "maintainer_github",
        "maintainer_mastodon",
        "docs"
      ),
      by = "resource"
    )
}

#' Add social media handles
#'
#' @param uc Data frame. Formatted use cases output of `uc_fmt()`.
#' @inheritParams common_docs
#'
#' @returns Data frame with social media handles appended/filled in
#'
#' @export
#' @examplesIf interactive()
#' u <- uc_fetch() |>
#'   uc_fmt("2025-01-01") |>
#'   uc_handles()

uc_handles <- function(uc, force_masto = FALSE) {
  if (nrow(uc) == 0) {
    cli::cli_inform("No Use Cases")
    return(data.frame())
  }
  pkgs <- pkgs_ru()

  uc <- uc |>
    dplyr::left_join(
      dplyr::select(pkgs, "resource" = "package", "owner"),
      by = "resource"
    )
  # Get missing Github by Name and other handles for maintainers
  if (anyNA(uc$maintainer_github)) {
    uc <- monarch::add_handles(
      uc,
      primary = "name",
      prefix = "maintainer_",
      pkg_col = "resource",
      owner_col = "owner",
      force_masto = force_masto
    )
  } else {
    uc <- monarch::add_handles(
      uc,
      primary = "github",
      prefix = "maintainer_",
      force_masto = force_masto
    )
  }

  # Get handles by Github for authors
  uc <- monarch::add_handles(
    uc,
    prefix = "author_",
    force_masto = force_masto
  )

  uc
}


#' Create use case `socials_post_issue()` command
#'
#' @param uc Data frame. Formatted use cases including social media handles
#'   and arranged by platform upon which to post, output of `uc_platform()`.
#' @param date_time Character/Date. When to post. Defaults to next Wednesday if `NULL`.
#'
#' @inheritParams common_docs
#'
#' @returns Copies commands to clipboard, optionally prints if (`print = TRUE`).
#'
#' @export
#' @examplesIf interactive()
#' u <- uc_fetch() |>
#'   uc_fmt("2025-01-01") |>
#'   uc_handles() |>
#'   by_platform()
#'
#' uc_post(u, dry_run = TRUE,print = TRUE)

uc_post <- function(
  uc,
  date_time = NULL,
  dry_run = FALSE,
  print = FALSE
) {
  if (nrow(uc) == 0) {
    cli::cli_inform("No new usecases posted")
    return(invisible())
  }

  r <- uc |>
    dplyr::left_join(dict_usecases(), by = "category") |>
    dplyr::mutate(
      resource = dplyr::if_else(
        !is.na(.data$maintainer),
        glue::glue("{resource} {l_maintained} {maintainer}"),
        glue::glue("{resource}")
      ),
      .by = c("id", "platform")
    ) |>
    dplyr::summarize(
      resource = glue::glue(
        "{unique(l_using)} ",
        paste0(.data$resource, collapse = " & "),
        "!"
      ),
      .by = c("id", "platform")
    )

  drafts <- uc |>
    dplyr::select(
      "id",
      "date",
      "title",
      "author",
      "platform",
      "url",
      "category"
    ) |>
    dplyr::distinct() |>
    dplyr::left_join(dict_usecases(), by = "category") |>
    dplyr::left_join(r, by = c("id", "platform")) |>
    dplyr::mutate(
      body = glue::glue(
        "'{title}' {l_by} {author}\n",
        "{url}\n",
        "{resource}"
      )
    ) |>
    dplyr::summarize(
      opening = dplyr::if_else(
        dplyr::n() > 1,
        paste0(.data$l_intro_mult[1], "\n\n"),
        paste0(.data$l_intro_single[1], "\n\n")
      ),
      draft = paste0(
        .data$opening,
        glue::glue_collapse(.data$body, sep = "\n\n"),
        "\n\n",
        .data$l_share[1],
        collapse = "\n"
      ),
      .by = c("platform", "category")
    )

  # Next Wednesday
  date_time <- date_time %||% post_time("Wednesday", 8)

  cmd <- glue::glue(
    "promoutils::socials_post_issue(
    time = \"{date_time}\", tz = \"America/Vancouver\",
    where = \"{drafts$platform}\", dry_run = {dry_run},
    over_char_limit = cli::cli_warn,
    title = \"usecases\",
    body = \"{drafts$draft}\n\")\n",
    .trim = FALSE
  )

  copy(cmd, "Usecases post command", print = print)
}
