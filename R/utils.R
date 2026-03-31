#' Create a cached version of the GH api calls
#'
#' @inheritParams gh::gh
#' @inheritDotParams gh::gh
#' @details `memoise::memoise(gh::gh)`
#'
#' @export
gh_cache <- memoise::memoise(gh::gh, omit_args = c(".max_rate"))

#' List of packages and details from R-Universe API
#'
#' @param universe Character. Universe to collect details from.
#'
#' @returns Data frame of package details
#'
#' @export
#' @examples
#' pkgs_ru()

pkgs_ru <- function(universe = "ropensci") {
  pkgs <- httr2::request("https://ropensci.r-universe.dev/api/packages") |>
    httr2::req_user_agent("promoutils") |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  pkgs |>
    purrr::map(\(x) {
      if (!is.null(x[["_contributors"]])) {
        cc <- purrr::map(x[["_contributors"]], data.frame) |>
          purrr::list_rbind() |>
          dplyr::select(-dplyr::any_of("uuid")) |>
          dplyr::rename_with(\(n) {
            stringr::str_replace(n, "^user$", "github")
          }) |>
          dplyr::rename_with(\(n) paste0("contributor_", n))
      } else {
        cc <- NULL
      }

      m <- x[["_maintainer"]]
      m <- rlang::set_names(m, paste0("maintainer_", names(m)))

      cols <- c("Package", "Title", "_owner", "_devurl", "_pkgdown")
      xx <- append(x[names(x) %in% cols], m)

      for (n in c(cols, "maintainer_login")) {
        if (!n %in% names(xx)) {
          xx[[n]] <- NA
        }
      }

      xx |>
        dplyr::as_tibble() |>
        dplyr::mutate(contributors = list(cc)) |>
        dplyr::rename_with(tolower) |>
        dplyr::rename(
          "maintainer_github" = "maintainer_login",
          "owner" = "_owner",
          "url" = "_devurl",
          "docs" = "_pkgdown"
        )
    }) |>
    purrr::list_rbind()
}


#' Get package author names
#'
#' @param package Character. Package name
#' @param pkgs Data frame. Packages returned by [pkgs_ru()].
#'
#' @return Character name of maintainer
#' @export
#' @examples
#' pkg_authors("weathercan", pkgs_ru())

pkg_authors <- function(package, pkgs) {
  # TODO: Direct call to R Universe API?
  a <- dplyr::filter(pkgs, .data$package %in% .env$package) |>
    dplyr::pull(.data$maintainer_name)

  if (length(a) == 0) {
    a <- NA_character_
  }
  a
}

nth_day <- function(x) {
  th <- dplyr::case_when(
    x %in% c(1, 21, 31) ~ "st",
    x %in% c(2, 22) ~ "nd",
    x %in% c(3, 23) ~ "rd",
    TRUE ~ "th"
  )

  paste0(x, th)
}

#' Find the next date
#'
#' Given a date and a day of the week,
#' Given a date return the next month's first Tuesday
#'
#' @param month Character/Date. The current month. Date returned is the next month.
#' @param which Character/Numeric. Which week day to return. Either number or
#'   abbreviated English weekday.
#' @param n Numeric. The nth week to return (i.e. the 1st Tuesday if `n = 1`
#'   and `which = "Tues"`).
#' @param call Environment. Calling environment for appropriate messages if used
#'   within another function.
#'
#' @return A date
#' @export
#'
#' @examples
#'
#' # Get the next first Tuesday
#' next_date("2023-11-01")
#' next_date("2023-11-30")
#'
#' # Get the next 3rd Tuesday
#' next_date("2023-11-01", n = 3)
#'
#' # Oops
#' \dontrun{
#' next_date("2023-11-01", n = 5)
#' }
#'
next_date <- function(
  month,
  which = "Tues",
  n = 1,
  call = rlang::caller_env()
) {
  month <- lubridate::as_date(month) + lubridate::period("1 month")

  d <- month |>
    lubridate::floor_date(unit = "months") |>
    lubridate::ceiling_date(
      unit = "weeks",
      week_start = which,
      change_on_boundary = FALSE
    )

  d <- d + lubridate::weeks(n - 1)

  if (lubridate::month(d) != lubridate::month(month)) {
    cli::cli_abort(
      "There are not {n} {format(d, '%A')}s in {format(month, '%B %Y')}",
      call = call
    )
  }
  d
}


#' Replace emoji codes with unicode
#'
#' Replaces emoji codes like :tada: with unicode like 🎉.
#'
#' @param x Character. Text string within which to replace codes
#'
#' @return  Text string with emoji unicodes
#' @export
#'
#' @examples
#' replace_emoji("hi :tada: testing \n\n\n Whow ! 🔗 \n\n\n :smile:")
#' replace_emoji(":link:")
replace_emoji <- function(x) {
  emo <- stringr::str_extract_all(x, "\\:.+\\:") |>
    unlist() |>
    unique()

  if (length(emo) > 0) {
    emo <- stats::setNames(
      purrr::map(
        emo,
        ~ pandoc::pandoc_convert(
          text = .x,
          from = "markdown+emoji",
          to = "plain"
        )
      ) |>
        unlist(),
      nm = emo
    )

    x <- stringr::str_replace_all(x, emo)
  }
  x
}


#' Extract YAML keys from block
#'
#' @param yaml Character. String from which to extract YAML keys
#' @param trim Character. Text to remove from the YAML block before processing.
#'   Usually the text that defines the block.
#'
#' @return data frame of yaml keys
#' @export
#'
#' @examples
#'
#' yaml_extract("~~~start: 2023-11-12\nauthor: Steffi\n~~~")
#'
yaml_extract <- function(yaml, trim = "~~~") {
  y <- stringr::str_remove_all(yaml, trim) |>
    yaml::yaml.load() |>
    purrr::map_if(is.null, \(x) "") |>
    data.frame()

  # Catch common typos
  names(y) <- tolower(names(y))
  names(y) <- stringr::str_replace_all(
    names(y),
    "(reocuring)|(reoccuring)|(reocurring)",
    "reoccurring"
  )
  y
}


# LinkedIn Chars to escape
escape_linkedin_chars <- function(x) {
  chars <- c(
    "\\|",
    "\\{",
    "\\}",
    "\\@",
    "\\[",
    "\\]",
    "\\(",
    "\\)",
    "\\<",
    "\\>",
    "\\#",
    "\\\\",
    "\\*",
    "\\_",
    "\\~"
  )
  p <- stats::setNames(glue::glue("\\{chars}"), chars)
  stringr::str_replace_all(x, p)
}

template <- function(name) {
  name <- stringr::str_remove(name, "\\.txt$")
  system.file(
    "extdata",
    "templates",
    glue::glue("{name}.txt"),
    package = "promoutils"
  ) |>
    readLines() |>
    glue::glue_collapse(sep = "\n")
}

copy <- function(body, what, print = FALSE) {
  if (print) {
    cli::cat_print(body)
    return(invisible(body))
  } else {
    clipr::write_clip(body)
    cli::cli_alert_success("Copied {what} to clipboard")
    return(invisible(body))
  }
}

#' Create url from content date and slug
#'
#' @param path Character. Slug, URL, or path to file in repository
#' @param date Character. Date for event, used to create link (otherwise extracted from slug)
#' @param lang Character. Language of blogpost (i.e., `en`, `es`, `fr`, etc.)
#' @param where Character. 'blog' or 'event' depending on the content type.
#' @param base_url Character. Base url of blog posts.
#'
#' @returns Full url
#'
#' @export
#' @examples
#' url_from_path("my-post", date = "2025-01-01")
#' url_from_path("2025-01-01-my-post/index.es.md")
#' url_from_path("content/blog/2025-09-29-news-september-2025/index.md")
#' url_from_path("content/blog/2025-09-29-news-september-2025/index.Rmd")
#' url_from_path("content/blog/2025-09-29-news-september-2025/")

url_from_path <- function(
  path,
  date = NULL,
  lang = NULL,
  where = "blog",
  base_url = "https://ropensci.org"
) {
  # If this is a url already, skip

  if (!stringr::str_detect(path, glue::glue("{base_url}/{where}"))) {
    # If this is a repository path
    if (stringr::str_detect(path, "content\\/|index")) {
      slug <- stringr::str_remove_all(
        path,
        glue::glue(
          "(content\\/{where}\\/)|(\\/index\\.([a-z]*\\.)?(R?)md)|\\/$"
        )
      )
      date <- stringr::str_extract(slug, "\\d{4}-\\d{2}-\\d{2}")
      slug <- stringr::str_remove(slug, glue::glue("{date}\\-"))
      lang <- stringr::str_extract(path, "(?<=index\\.)([a-z]*)?(?=\\.)")
    } else {
      slug <- path
    }

    if (where == "event") {
      url <- glue::glue("{base_url}/event/{slug}")
    }

    if (!(is.na(lang) || is.null(lang))) {
      base_url <- glue::glue("{base_url}/{lang}")
    }

    if (where == "blog") {
      if (is.null(date)) {
        cli::cli_abort("For blog slugs, must also provide a date")
      }
      url <- glue::glue(
        "{base_url}/{where}/{lubridate::year(date)}/",
        "{stringr::str_pad(lubridate::month(date), 2, pad = 0)}/",
        "{stringr::str_pad(lubridate::day(date), 2, pad = 0)}/",
        "{slug}/"
      )
    }
  } else {
    url <- path
  }
  url
}

#' List PRs
#'
#' List open PRs by url, title and number, optionally matching to a title or branch name (ref)
#'
#' @param match Character. String to match in the title or branch name
#' @param owner Character. GitHub owner of the repository (defaults to 'ropensci')
#' @param repo Character. GitHub repository name (defaults to 'roweb3')
#'
#' @returns Data frame with PR url, number, title, and branch name ('ref')
#' @export
#' @examples
#' prs_list("coworking") # List all coworking related (open) PRs

prs_list <- function(match = NULL, owner = "ropensci", repo = "roweb3") {
  pr <- gh::gh(
    "GET /repos/{owner}/{repo}/pulls",
    owner = owner,
    repo = repo,
    .limit = Inf
  ) |>
    purrr::map(\(x) {
      dplyr::tibble(
        html_url = x$html_url,
        number = x$number,
        title = x$title,
        open = is.null(x$merged_at) & is.null(x$closed_at),
        ref = x$head$ref
      )
    }) |>
    purrr::list_rbind() |>
    dplyr::filter(.data$open) |>
    dplyr::select(-"open")

  if (!is.null(match)) {
    pr <- dplyr::filter(
      pr,
      stringr::str_detect(.data$ref, .env$match) |
        stringr::str_detect(.data$title, .env$match)
    )
  }
  pr
}


#' Format markdown urls to Slack format
#'
#' Such that `[My awesome page](https://my-awesome.html)` becomes
#' `<https://my-awesome.html|My awesome page>`.
#'
#' @param body Character. Text to check
#'
#' @returns Character.
#'
#' @examples
#'
#' fmt_slack_urls("[My awesome page](https://my-awesome.html)")
#' fmt_slack_urls("[My awesome page](https://my-awesome.html) and this [email](mailto:mail@mail.com)")
#'
#' @noRd
fmt_slack_urls <- function(body) {
  stringr::str_replace_all(
    body,
    "\\[(.+?)\\]\\((.+?)\\)",
    "<\\2|\\1>"
  )
}

#' Arrange by platform
#'
#' Arranges data in long by platform (linkedin and mastodon).
#'
#' @param df Data frame. Formatted data including social media handles.
#'
#' @returns Data frame arranged by platform on which to advertise.
#'
#' @export
#' @examplesIf interactive()
#' u <- uc_fetch() |>
#'   uc_fmt("2025-01-01") |>
#'   uc_handles() |>
#'   by_platform()

by_platform <- function(df) {
  if (!is.null(df) && nrow(df) == 0) {
    return(data.frame())
  }

  df |>
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

#' Get the next post date/time
#'
#' Finds the next date/time to post by day of the week and hour.
#'
#' @param day Character. Day of the week (e.g., "Monday")
#' @param hour Numeric. Hour at which to post (e.g., 8)
#'
#' @returns Date time as character (without timezone)
#'
#' @export
#' @examples
#' post_time("Wednesday", 8)

post_time <- function(day, hour) {
  day_of_week <- c(
    "Monday" = 1,
    "Tuesday" = 2,
    "Wednesday" = 3,
    "Thursday" = 4,
    "Friday" = 5,
    "Saturday" = 6,
    "Sunday" = 7
  )[day]

  date_time <- lubridate::ceiling_date(
    Sys.Date(),
    "weeks",
    week_start = day_of_week
  )
  date_time <- date_time + lubridate::hours(hour)
  date_time <- as.character(date_time)

  date_time
}
