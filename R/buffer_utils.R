#' Great GraphQL Query for Buffer
#'
#' Note that the GraphQL requires double quotes (single quotes won't work).
#'
#' @param template Character. Template for the query, uses {} to insert
#'   arguments from the `...`.
#' @param fields Character vector. Fields to return
#' @param sort List. Fields to sort by (see [buffer_posts_list()] for an
#'   example).
#' @param filter List. Fields to filter by (see [buffer_posts_list()] for an
#'   example).
#' @param ... Character or lists. Query Parameters
#'
#' @returns Character string of query
#'
#' @noRd
#' @examples
#' buffer_query(
#'   "query GetOrganizations { account { organizations { {{fields}} } } }",
#'   fields = c("id", "name")
#' )

buffer_query <- function(
  template,
  fields = NULL,
  sort = NULL,
  filter = NULL,
  ...
) {
  list2env(list(...), rlang::current_env())

  if (!is.null(fields)) {
    fields <- paste0(fields, collapse = " ")
  }
  if (!is.null(sort)) {
    sort <- purrr::imap(sort, \(x, i) {
      glue::glue("{{field: {i}, direction: {x}}}")
    })
    if (length(sort) > 1) {
      sort <- paste0("[", paste0(sort, collapse = ", "), "]")
    }
    sort <- paste0("sort: ", sort)
  } else {
    sort <- ""
  }

  if (!is.null(filter)) {
    filter <- purrr::imap(filter, \(x, i) {
      if (i == "dueAt") {
        x <- glue::glue("{i}: {{ {x} }}")
      } else {
        x <- glue::glue("{i}: [{glue::glue_collapse(x, sep = ', ')}]")
      }
      x
    })
    if (length(filter) > 1) {
      filter <- paste0(filter, collapse = ", ")
    }
    filter <- paste0("filter: {", filter, "}")
  } else {
    filter <- ""
  }
  buff_glue(template)
}

buff_glue <- function(..., env = rlang::caller_env()) {
  glue::glue(
    ...,
    .sep = "\n",
    .open = "{{",
    .close = "}}",
    .envir = env
  )
}


#' Create httr2 request for Buffer API
#'
#' @param query Character. Query to send.
#' @param dry_run Logical. Whether or not should be a dry run.
#' @param paginate Whether to use pagination.
#'
#' @returns List of responses
#'
#' @noRd

buffer_request <- function(query, dry_run, paginate = FALSE) {
  r <- httr2::request("https://api.buffer.com") |>
    httr2::req_auth_bearer_token(token = key("buffer")) |>
    httr2::req_body_json(list(query = query)) |>
    httr2::req_error(body = buffer_error)

  if (dry_run) {
    resp <- httr2::req_dry_run(r)
    attr(resp, "dry_run") <- TRUE
    return(resp)
  } else {
    resp <- httr2::req_perform(r)
  }

  if (paginate) {
    c <- httr2::resp_body_json(resp)$data$posts$pageInfo$endCursor
    resps <- list(resp)

    while (!is.null(c)) {
      q <- stringr::str_replace(
        query,
        "(first\\: \\d+,)",
        paste0("\\1 after: \"", c, "\", ")
      )
      resp <- httr2::req_body_json(r, list(query = q)) |>
        httr2::req_perform()
      resps <- append(resps, list(resp))
      c <- httr2::resp_body_json(resp)$data$posts$pageInfo$endCursor
    }
    resp <- resps
  }

  resp
}

#' Catch and parse errors from Buffer API
#'
#' Used by [httr2::req_error()] in `buffer_request()`.
#'
#' @param resp httr2 response
#'
#' @returns Nothing
#'
#' @noRd

buffer_error <- function(resp) {
  e <- httr2::resp_body_json(resp)$error
  r <- httr2::resp_header(resp, "Retry-After") |> as.numeric()

  purrr::map(e, \(x) {
    if (
      "extensions" %in% names(x) && x$extensions$code == "RATE_LIMIT_EXCEEDED"
    ) {
      x <- c(
        paste0(x$message, " (", x$extensions$window, " window exceeded)"),
        paste(
          "Try again in",
          round(r / 60),
          "min /",
          round(r / 60 / 60, 1),
          "hrs"
        ),
        "See https://publish.buffer.com/settings/api for limits"
      )
    } else {
      x <- x$message
    }
    x
  }) |>
    unlist()
}

#' Create data frame of responses from Buffer API
#'
#' @param resp list of httr2 responses or httr2 response
#'
#' @returns data frame
#'
#' @noRd

buffer_df <- function(resp) {
  if (isTRUE(attr(resp, "dry_run"))) {
    return(resp)
  }

  if (!inherits(resp, "httr2_response")) {
    r <- purrr::map(resp, buffer_df) |>
      purrr::list_rbind()
    return(r)
  }

  r <- httr2::resp_body_json(resp)

  msg <- fetch(r, "message")

  if (!is.null(msg)) {
    if (length(msg) == 1 && msg == "Document not found") {
      msg <- "No post found"
    }
    cli::cli_inform(c("!" = "Message from the Buffer Server:"))
    cli::cli_verbatim(msg)
    return(invisible())
  }

  r |>
    purrr::map(.buffer_df) |>
    purrr::list_rbind()
}

#' Create data frame of single response from Buffer API
#'
#' @param x httr2 response
#'
#' @returns data frame
#'
#' @noRd
.buffer_df <- function(x) {
  if (!rlang::is_named(x) || is.list(x[[1]])) {
    x <- purrr::map(x, .buffer_df)
  } else {
    if (!"hasNextPage" %in% names(x)) {
      # Skip pagination data
      x <- purrr::map(x, \(xx) xx %||% NA)
      return(dplyr::as_tibble(x))
    } else {
      # TODO: START HERE AND FIX!!!!   b <- buffer_posts_list()
      return(NULL)
    }
  }

  purrr::list_rbind(x)
}

#' Fetch a nested named element from a list
#'
#' Return a named element from a list no matter how deep.
#'
#' @param list List. From which to fetch an element.
#' @param id Character. Name of the element to return.
#'
#' @returns Element in the list.
#'
#' @noRd
#' @examples
#' l <- list(
#'   data = list(
#'     account = list(
#'       organizations = list(
#'         first = list(id = 123),
#'         second = list(id = 456),
#'         third = list(other = 567)
#'       )
#'     )
#'   )
#' )
#' fetch(l, "id")
#' fetch(l, "other")
#' fetch(l, "test")

fetch <- function(list, id, drop_names = TRUE) {
  if (id %in% names(list)) {
    return(list[[id]])
  } else if (!is.list(list)) {
    return(NULL)
  } else {
    purrr::map(list, \(l) fetch(l, id)) |> unlist(use.names = !drop_names)
  }
}


#' Check that time is correct and format for Buffer
#'
#' @param time Character/Date time
#' @param tz Timezone if `time` is character, ignored otherwise.
#'
#' @returns formatted time
#'
#' @noRd

check_buff_time <- function(time, tz) {
  if (is.character(time) && time == "now") {
    return(time)
  } else if (is.character(time)) {
    tz <- tz %||% Sys.timezone()
    time <- lubridate::ymd_hms(time, truncated = 3, tz = tz)
  } else if (lubridate::is.POSIXct(time) || lubridate::is.Date(time)) {
    if (!is.null(tz)) {
      cli::cli_inform(
        "Ignoring `tz` as `time` is already a date/time object."
      )
    }
  }

  time <- lubridate::with_tz(time, "UTC") |>
    format("%Y-%m-%dT%H:%M:%SZ")

  time
}

#' Check length of Buffer post by channel
#'
#' @param body Character. Body of the post to check length of
#' @param channel Character. Channel to check against.
#'
#' @returns `body`, possibly split into threads
#'
#' @noRd
#' @examples
#' check_buff_body("Hello", "mastodon")
#' check_buff_body(paste(rep("Hello", 300), collapse = " "), "bluesky")

check_buff_body <- function(body, channel, thread = TRUE) {
  body <- replace_emoji(body)

  n <- nchar(body, type = "width") # Approximate
  if (n > buff_nchars[channel]) {
    if (!thread) {
      cli::cli_abort(
        "Message too long ({n}) for this channel ({channel} max: {buff_nchars[channel]})",
        call = NULL
      )
    } else {
      body <- split_body(body, n_max = buff_nchars[channel])
    }
  }
  # Escape new lines
  body <- stringr::str_replace_all(body, "\n", "\\\\n")

  # Remove whitespace
  body <- stringr::str_trim(body)

  body
}


#' Check for duplicates before posting
#'
#' @param body Character. Body text to compare
#' @param when Character. Post date/time to compare
#' @param channel Character. Channel to compare
#'
#' @returns `TRUE` if duplicate matched `FALSE` if not.
#'
#' @noRd

check_buff_dups <- function(body, when, channel) {
  body <- paste0(body, collapse = "|")
  s <- buffer_posts_list() |>
    dplyr::mutate(
      dueAt = stringr::str_remove(.data$dueAt, "\\.000"),
      body = stringr::str_extract(.data$text, "^.+\\n") |> stringr::str_trim()
    ) |>
    dplyr::filter(
      stringr::str_detect(.env$body, .data$body),
      .data$dueAt == .env$when,
      .data$channelService == .env$channel
    )

  if (nrow(s) > 0) {
    cli::cli_inform(
      "Skipping... There is/are similar post(s) scheduled or draft for the same time on the same platform"
    )
    return(TRUE)
  }
  FALSE
}

#' Clean up draft/scheduled testing messages
#'
#' Removes all testing draft and/or scheduled messages matching "^testing(
#' scheduled)? Api again...$".
#'
#' @returns Data frame with removed post ids
#'
#' @export
#' @examplesIf interactive()
#' buffer_cleanup()

buffer_cleanup <- function() {
  l <- buffer_posts_list() |>
    dplyr::filter(stringr::str_detect(
      text,
      "^testing( scheduled)? Api again...$"
    ))
  buffer_posts_remove(l$id)
}
