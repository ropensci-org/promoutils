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
      tibble::as_tibble(u) |> tidyr::unnest(c("author", "category"))
    }) |>
    purrr::list_rbind() |>
    dplyr::mutate(date = lubridate::ymd_hms(.data$createdAt)) |>
    dplyr::rename("author_github" = "author")
}

#' Extract out resource information
#'
#' @param uc Data frame. Data frame of usecases from `uc_fetch()`.
#' @param min_date Character/Date. Minimum date of usecases to retain.
#' @param pkgs Data frame. Optional data frame of package details (defaults to `pkgs()`).
#'
#' @returns Data frame of formatted use cases.
#'
#' @export
#' @examples
#' u <- uc_fetch() |>
#'   uc_fmt("2025-01-01")

uc_fmt <- function(uc, min_date, pkgs = NULL) {
  min_date <- lubridate::ymd_hms(min_date, truncated = 3)
  pkgs <- pkgs %||% pkgs()

  u <- uc |>
    dplyr::filter(date >= min_date)

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
        paste0(tolower(pkgs$name), collapse = "|")
      ),
      #resource_lst = stringr::str_extract_all(.data$resource, "[^.,;]+"),
      resource_n = purrr::map_int(.data$resource_lst, length),
      resource = purrr::map(.data$resource_lst, stringr::str_squish)
    ) |>
    tidyr::unnest(resource) |>
    dplyr::left_join(
      dplyr::select(
        pkgs,
        "resource" = "name",
        "maintainer_linkedin" = "maintainer",
        "docs"
      ),
      by = "resource"
    )
}

#' Add in author and maintainer social media handles
#'
#' @param uc Data frame of formatted use cases from `uc_fmt()`.
#'
#' @returns Data frame of formatted use cases including social media handles.
#'
#' @export
#' @examples
#' u <- uc_fetch() |>
#'   uc_fmt("2025-01-01") |>
#'   uc_names()

uc_names <- function(uc) {
  if (nrow(uc) == 0) {
    return(data.frame())
  }

  # Fetch maintainer socials we have
  u <- uc |>
    dplyr::mutate(
      maintainer_github = monarch::fetch(
        .data$maintainer_linkedin,
        type = "github"
      ),
      maintainer_mastodon = monarch::fetch(
        .data$maintainer_github,
        type = "mastodon"
      )
    )

  # Get missing maintainer GH
  purrr::pwalk(u, \(maintainer_linkedin, maintainer_github, resource, ...) {
    if (is.na(maintainer_github) & !is.na(maintainer_linkedin)) {
      monarch::socials_fetch(name = maintainer_linkedin, pkg = resource) |>
        monarch::cocoon_update()
    }
  })

  # Get missing maintainer socials
  purrr::pwalk(u, \(maintainer_mastodon, maintainer_github, ...) {
    if (is.na(maintainer_mastodon) & !is.na(maintainer_github)) {
      monarch::socials_fetch(github = maintainer_github) |>
        monarch::cocoon_update()
    }
  })

  # Update maintainer socials
  # Fetch author socials we have
  u <- u |>
    dplyr::mutate(
      maintainer_github = monarch::fetch(
        .data$maintainer_linkedin,
        type = "github"
      ),
      maintainer_mastodon = monarch::fetch(.data$maintainer_github),
      author_linkedin = monarch::fetch(.data$author_github, type = "name"),
      author_mastodon = monarch::fetch(.data$author_github)
    )

  # Get missing author socials
  purrr::pwalk(u, \(author_mastodon, author_linkedin, author_github, ...) {
    if (is.na(author_mastodon) | is.na(author_linkedin)) {
      monarch::socials_fetch(github = author_github) |>
        monarch::cocoon_update()
    }
  })

  # Add in any we recently retrieved
  u <- u |>
    dplyr::mutate(
      author_linkedin = monarch::fetch(.data$author_github, type = "name"),
      author_mastodon = monarch::fetch(.data$author_github)
    )

  # Put in placeholders for missing
  u <- u |>
    dplyr::mutate(
      author_linkedin = dplyr::if_else(
        is.na(author_linkedin),
        author_github,
        author_linkedin
      ),
      author_mastodon = dplyr::if_else(
        is.na(author_mastodon),
        author_linkedin,
        author_mastodon
      ),
      maintainer_mastodon = dplyr::if_else(
        is.na(maintainer_mastodon),
        maintainer_linkedin,
        maintainer_mastodon
      )
    )

  u
}

#' Arrange by platform
#'
#' @param uc Data frame. Formatted use cases including social media handles,
#'   output of `uc_names()`.
#'
#' @returns Data frame of use cases arranged by platform on which to advertise.
#'
#' @export
#' @examples
#' u <- uc_fetch() |>
#'   uc_fmt("2025-01-01") |>
#'   uc_names() |>
#'   uc_platform()

uc_platform <- function(uc) {
  if (nrow(uc) == 0) {
    return(data.frame())
  }

  uc |>
    tidyr::pivot_longer(
      cols = dplyr::matches("linkedin|mastodon"),
      names_to = c("role", "platform"),
      names_sep = "_",
      values_to = "handle"
    ) |>
    tidyr::pivot_wider(
      names_from = "role",
      values_from = "handle"
    )
}

#' Create use case `socials_post_issue()` command
#'
#' @param uc Data frame. Formatted use cases including social media handles
#'   and arranged by platform upon which to post, output of `uc_platform()`.
#' @param date Character/Date. When to post. Defaults to next Wednesday.
#'
#' @inheritParams common_docs
#'
#' @returns Copies commands to clipboard, optionally prints if (`print = TRUE`).
#'
#' @export
#' @examples
#' u <- uc_fetch() |>
#'   uc_fmt("2025-01-01") |>
#'   uc_names() |>
#'   uc_platform()
#'
#' uc_post(u, dry_run = TRUE)

uc_post <- function(
  uc,
  date = NULL,
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
        !is.na(maintainer),
        glue::glue("{resource} {l_maintained} {maintainer}"),
        glue::glue("{resource}")
      ),
      .by = c("id", "platform")
    ) |>
    dplyr::summarize(
      resource = glue::glue(
        "{unique(l_using)} ",
        paste0(resource, collapse = " & "),
        "!"
      ),
      .by = c("id", "platform")
    )

  drafts <- uc |>
    dplyr::select(
      id,
      date,
      title,
      "author",
      platform,
      url,
      category
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
        paste0(l_intro_mult[1], "\n\n"),
        paste0(l_intro_single[1], "\n\n")
      ),
      draft = paste0(
        opening,
        glue::glue_collapse(body, sep = "\n\n"),
        "\n\n",
        l_share[1],
        collapse = "\n"
      ),
      .by = c("platform", "category")
    )

  # Next Wednesday
  if (!is.null(date)) {
    time_post <- lubridate::as.date(date)
  } else {
    time_post <- lubridate::ceiling_date(Sys.Date(), "weeks", week_start = 3)
  }

  cmd <- glue::glue(
    "promoutils::socials_post_issue(
    time = \"{time_post}\", tz = \"America/Vancouver\",
    where = \"{drafts$platform}\", dry_run = {dry_run},
    over_char_limit = cli::cli_warn,
    title = \"usecases\",
    body = \"{drafts$draft}\n\")\n",
    .trim = FALSE
  )

  copy(cmd, "Usecases post command", print = print)
}
