#' Download all Matomo historic views
#'
#' Downloads all views from a specific year range (2010-2024 by defautl).
#' Views can be read with [matomo_read()].
#'
#' Requires a Renviron "MATOMO_TOKEN" key representing your token for Matomo
#' for the host you'd like to fetch data for.
#'
#' @param year_range Numeric vector. Years to download views for.
#' @param host Character. URL for the Matomo host to collect views for. Defaults
#' to that of rOpenSci.
#'
#' @returns Nothing. Writes views to disk.
#'
#' @export

matomo_all <- function(
  year_range = 2010:2024,
  host = "https://ropensci.matomo.cloud"
) {
  for (y in year_range) {
    date1 <- paste0(y, "-01-01")
    date2 <- paste0(y, "-12-31")
    views <- matomo_fetch(c(date1, date2), host = host)
    matomo_write(views)
  }

  invisible(TRUE)
}

#' Update Matomo views
#'
#' Update a specific year of Matomo blog views. Defaults to current year.
#' Views can be read with [matomo_read()].
#'
#' Requires a .Renviron "MATOMO_TOKEN" key representing your token for Matomo
#' for the host you'd like to fetch data for.
#'
#' @param year Numeric. Year to update
#' @param host Character. URL for the Matomo host to collect views for. Defaults
#' to that of rOpenSci.
#'
#' @returns Nothing. Updates views to disk.
#'
#' @export

matomo_update <- function(
  year = lubridate::year(Sys.Date()),
  host = "https://ropensci.matomo.cloud"
) {
  date1 <- lubridate::as_date(paste0(year, "-01-01"))
  date2 <- min(Sys.Date(), lubridate::as_date(paste0(year, "-12-31")))
  views <- matomo_fetch(c(date1, date2), host = host)
  matomo_write(views)
  invisible(TRUE)
}

matomo_fetch <- function(date_range, host = "https://ropensci.matomo.cloud") {
  rlang::check_installed("matomoR")

  if (lubridate::year(date_range[1]) != lubridate::year(date_range[2])) {
    cli::cli_abort("Can only fetch one year at a time")
  }

  Sys.setenv(MATOMO_HOST = host)
  idSite <- 1

  query <- list(
    module = "API",
    method = "SegmentEditor.getAll",
    idSite = idSite
  )

  cli::cli_inform("Fetching segments")

  segments <- matomoR::matomo_package_query(query)$content
  segment <- segments$definition[segments$name == "Main website"]

  date <- paste(date_range[1], date_range[2], sep = ",")

  query <- list(
    module = "API",
    method = "Actions.getPageUrls",
    period = "day",
    idSite = idSite,
    date = date,
    segment = segment,
    depth = 10,
    expanded = 1,
    flat = 1
  )

  cli::cli_inform("Fetching pages and page views")
  pageviews_raw <- matomoR::matomo_package_query(query)$content

  views <- pageviews_raw[lengths(pageviews_raw) != 0] |>
    purrr::imap_dfr(
      ~ dplyr::bind_cols(.x[, c("label", "nb_visits")], date = .y)
    )

  views
}

#' Cache folder to store matomo views
#'
#' @returns Character file path
#'
#' @export
#' @examples
#' matomo_dir()
matomo_dir <- function() {
  tools::R_user_dir("promoutils") |>
    file.path("views")
}

matomo_write <- function(views) {
  if (!dir.exists(matomo_dir())) {
    cli::cli_inform(
      c("Creating 'views' folder to save Matomo views info: ", matomo_dir())
    )
    dir.create(matomo_dir(), recursive = TRUE)
  }
  if (nrow(views) > 0) {
    cli::cli_inform(c("Writing new Matomo views to: ", matomo_dir()))
    y <- lubridate::year(views$date)[1]
    readr::write_csv(
      views,
      file.path(matomo_dir(), paste0("views_", y, ".csv"))
    )
  } else {
    cli::cli_inform("No new views to add")
  }

  invisible()
}

#' Read Matomo views saved to disk
#'
#' Reads a saved set of views.
#'
#' @returns Data frame of view data.
#'
#' @export
#'
#' @examplesIf dir.exists(matomo_dir())
#' matomo_read()

matomo_read <- function() {
  views <- list.files(matomo_dir(), pattern = "views", full.names = TRUE) |>
    readr::read_csv(show_col_types = FALSE, progress = FALSE) |>
    dplyr::filter(stringr::str_detect(
      .data$label,
      "^(/\\?)|(/aepstk)",
      negate = TRUE
    )) |>
    dplyr::mutate(
      date = lubridate::as_date(.data$date),
      year = lubridate::year(.data$date),
      month = lubridate::month(.data$date),
      type = dplyr::if_else(
        .data$label == "/",
        "home",
        stringr::str_extract(.data$label, "(?<=^/)[^/]+")
      ),
      pkg = dplyr::if_else(
        .data$type %in% pkgs_ru()$package,
        .data$type,
        NA_character_
      ),
      type = dplyr::case_when(
        !is.na(.data$pkg) ~ "package_docs",
        .data$type == "guide-for-authors.html" ~ "blog_guide",
        .data$type == "reviewerguide.html" ~ "reviewer_guide",
        .data$type == "authors-guide.html" ~ "authors_guide",
        TRUE ~ .data$type
      ),
    ) |>
    dplyr::as_tibble() |>
    dplyr::arrange(dplyr::desc(.data$nb_visits))

  views
}

matomo_check_cats <- function() {
  views <- matomo_read() |>
    dplyr::filter(
      views,
      !.data$type %in%
        c(
          "home",
          "blog",
          "commcalls",
          "careers",
          "events",
          "r-universe",
          "help-wanted",
          "packages",
          "news",
          "author",
          "about",
          "software-review",
          "stat-software-review",
          "donate",
          "talks-papers",
          "code-of-conduct",
          "usecases",
          "community",
          "technotes",
          "package_docs",
          "archive",
          "citations",
          "contact",
          "tags",
          "blog_guide",
          "reviewer_guide",
          "how-to-cite-ropensci",
          "related",
          "resources",
          "authors_guide"
        )
    ) |>
    dplyr::summarize(.by = c("label", "type", "pkg"))

  views
}

#' Formats and filter Matomo views to blogposts
#'
#' @param views Data frame from [matomo_read()].
#'
#' @returns Data frame of blog post views
#'
#' @export
#' @examplesIf dir.exists(matomo_dir())
#' matomo_read() |>
#'   matomo_blogposts()

matomo_blogposts <- function(views) {
  blogviews <- views |>
    dplyr::filter(.data$type %in% c("blog", "technotes")) |>
    dplyr::filter(
      !stringr::str_detect(.data$label, "/null$"),
      !stringr::str_detect(.data$label, "/?[^/]+$")
    ) |>
    dplyr::filter(.data$label != "/blog/") |>
    dplyr::mutate(
      post_date = lubridate::ymd(stringr::str_extract(
        .data$label,
        "[0-9]{4}/[0-9]{2}/[0-9]{2}"
      )),
      post_month = lubridate::month(.data$post_date)
    ) |>
    dplyr::summarise(
      nb_visits = sum(.data$nb_visits),
      .by = c("label", "post_month", "post_date")
    )

  blogviews
}
