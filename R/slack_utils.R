
slack_auth <- function(resp) {
  httr2::req_auth_bearer_token(resp, token = keyring::key_get("slack-api"))
}


slack_check <- function(resp, msg = "Successful", element = NULL, paginate = FALSE) {

  if(paginate) {
    problems <- httr2::resps_failures(resp)
    if(length(problems) > 0) {
      rlang::abort("Errors in pagination", call = NULL)
    } else if(!is.null(msg)) rlang::inform(msg)

    r <- httr2::resps_data(resp, \(x) httr2::resp_body_json(x)[[element]])
    r <- list(t = r) |> stats::setNames(element)
    return(r)
  }

  r <- httr2::resp_body_json(resp)
  if(!r$ok) {
    r$error |>
      stringr::str_replace_all("_", " ") |>
      tools::toTitleCase() |>
      rlang::abort(call = NULL)
  }

  if(!is.null(msg)) rlang::inform(msg)

  r
}





slack_admin <- function() {
  "C08T0PS0QDR"
}

# https://docs.slack.dev/apis/web-api/pagination
slack_paginate <- function(req) {
  req |>
    httr2::req_throttle(capacity = 15) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_error(body = slack_error_info) |>
    httr2::req_perform_iterative(next_req = slack_next_req)
}

slack_error_info <- function(resp) {
  if("retry-after" %in% names(resp$headers)) {
    return(paste("Slack rate limits: Retry after", resp$headers$`retry-after`, "seconds"))
  } else "Slack API error"
}

slack_next_req <- function(resp, req) {
  cursor <- httr2::resp_body_json(resp)$response_metadata$next_cursor

  if(is.null(cursor) || cursor == "") return(NULL)

  httr2::req_url_query(req, cursor = cursor)
}



slack_df <- function(resp, element, cols) {
  purrr::pluck(resp, element) |>
    purrr::map(\(x) {
      x[cols] |>
        stats::setNames(cols) |>
        purrr::map(\(y) if(is.null(y)) NA else y)
    }) |>
    purrr::map(dplyr::as_tibble) |>
    purrr::list_rbind()
}
