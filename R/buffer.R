#' Create a social media post on Buffer
#'
#' @param body Character vector. Text to use as the body of the post. Can be a
#'   vector the same length as `channels` to use different text for each channel.
#' @param when Character/Date time. Either "now" to post immediately or create a
#'   draft post for immediate posting, or a Date/time to specify when the post
#'   should be scheduled.
#' @param tz Character. The [OlsonNames()] timezone to use for `when` if it is a
#'   character string (if `when` is a date/time `tz` is ignored). Default
#'   (`NULL`) uses system timezone.
#' @param channels Character vector. Where to post? Options are "mastodon",
#'   "bluesky", and/or "linkedin".
#' @param draft Logical. Whether this post should be a draft.
#'
#' @inheritParams common_docs
#'
#' @returns
#'
#' @export
#' @examplesIf interactive()
#' p <- buffer_posts_write("testing Api again...", when = "now") # Create draft
#' buffer_posts_remove(p$id)
#' p <- buffer_posts_write("testing Api again...", when = "2027-01-01 10:00") # Create schedueld draft
#' buffer_posts_remove(p$id)

buffer_posts_write <- function(
  body,
  when,
  tz = NULL,
  channels = c("mastodon", "linkedin", "bluesky"),
  draft = TRUE,
  dry_run = FALSE,
  open_browser = interactive()
) {
  # Define 'when'
  when <- check_buff_when(when, tz)

  # Recurse if multiple channels
  if (!length(body) %in% c(1, length(channels))) {
    cli::cli_abort(
      "Body must be either 1 (repeated) or the same length as channels (one per channel)",
      call = NULL
    )
  } else if (length(body) == 1) {
    body <- rep(body, length(channels))
  }

  resp <- purrr::map2(body, channels, \(b, c) {
    .buffer_posts_write(body = b, when, channel = c, draft, dry_run)
  })

  if (dry_run) {
    return(resp)
  }

  resp <- purrr::list_rbind(resp)

  if (draft && open_browser) {
    browseURL("https://publish.buffer.com/schedule?tab=drafts")
  }

  resp
}

.buffer_posts_write <- function(
  body,
  when,
  channel,
  draft = TRUE,
  dry_run = FALSE
) {
  check_buff_body(body, channel)

  channel_id <- get(paste0("buff_", channel))
  mode <- if (when == "now") "shareNow" else "customScheduled"

  template <- paste(
    "mutation CreatePost { ",
    "createPost(input: { ",
    "text: \"{{body}}\",",
    "channelId: \"{{channel_id}}\",",
    "schedulingType: automatic,",
    "mode: {{mode}},",
    if (when != "now") "dueAt: \"{{when}}\"" else "",
    "saveToDraft: {{tolower(draft)}}",
    "}) {
      ... on PostActionSuccess {
        post {
          id
          channelService
          text
          dueAt
          status
        }
      }
      ... on MutationError {
        message
      }
    }
  }",
    sep = "\n"
  )

  buffer_query(
    template,
    channel_id = channel_id,
    when = when,
    body = body,
    mode = mode,
    draft = draft
  ) |>
    buffer_request(dry_run = dry_run) |>
    buffer_df()
}

#' Remove posts on Buffer
#'
#' @param id Character vector. Id(s) of the post(s) to be removed.
#'
#' @returns
#'
#' @export
#' @examplesIf interactive()
#' id <- buffer_posts_write("testing Api again...", channels = "mastodon")
#' buffer_posts_remove("6a9099df479d79c4495fc168")

buffer_posts_remove <- function(id, dry_run = FALSE) {
  template <- "mutation removePosts { deletePost(input: { id: \"{{id}}\" }) {
      ... on DeletePostSuccess { id }
      ... on MutationError { message }
    }
  }"

  resp <- purrr::map(id, \(i) {
    buffer_query(template, id = i) |>
      buffer_request(dry_run = dry_run) |>
      buffer_df()
  }) |>
    purrr::list_rbind() |>
    dplyr::rename("removed_post_ids" = 1)

  if (is.null(resp)) return(invisible(resp)) else return(resp)
}

#' Return a list of Buffer posts
#'
#' Fetches a list of posts on all channels on Buffer.
#'
#' @param status Character vector. Status of posts to return. Defaults to
#'   "scheduled" and "draft", set to "sent" to return all posted messages.
#'   Options are "draft", "error", "needs_approal","scheduled", "sending",
#'   "sent".
#' @param filds Character vector. Information to return. Become the columns in
#' the data frame.
#'
#' @references
#'   Post status: https://developers.buffer.com/reference.html#type/PostStatus
#'   Posts: https://developers.buffer.com/reference.html#field-posts
#'   Fields: https://developers.buffer.com/reference.html#type/Post
#'
#' @returns
#'
#' @export
#' @examplesIf interactive()
#' buffer_posts_list()
#' buffer_posts_list(status = "error")
#' buffer_posts_list(status = "sent")

buffer_posts_list <- function(
  status = c("scheduled", "draft"),
  fields = c(
    "id",
    "channelService",
    "text",
    "createdAt",
    "dueAt",
    "status"
  ),
  dry_run = FALSE
) {
  template <- "query GetPosts { 
  posts(
    first: 20,
    input: {organizationId: \"{{org}}\" {{sort}} {{filter}} }
  ) {
    edges { node { {{fields}} } }
    pageInfo { hasNextPage endCursor }
} }"

  buffer_query(
    template,
    fields = fields,
    sort = list("dueAt" = "asc", "createdAt" = "desc"),
    filter = list("status" = status),
    org = ro_org
  ) |>
    buffer_request(dry_run = dry_run, paginate = TRUE) |>
    buffer_df()
}

#' Return Buffer channel ids
#'
#' @param org Character. Organizational ID to return channels for (see
#' [buffer_org()]). Uses rOpenSci's by default `buff_org`.
#'
#' @returns Data frame with `id` and `service` (linkedin, mastodon, bluesky,
#'  etc.)
#'
#' @export
#' @examplesIf interactive()
#' buffer_channels()

buffer_channels <- function(org = buff_org, dry_run = FALSE) {
  template <- "query GetChannels { channels(input: { organizationId: \"{{org}}\" }) { {{fields}} } }"

  q <- buffer_query(
    template,
    fields = c("id", "service"),
    org = org
  )

  buffer_request(q, dry_run = dry_run) |>
    buffer_df()
}

#' Return Buffer Organization account id
#'
#' @param fields Character. Information to return. Defaults to 'id'.
#'
#' @returns The organization account id on Buffer.
#'
#' @export
#' @examplesIf interactive()
#' buffer_org()

buffer_org <- function(fields = "id", dry_run = FALSE) {
  template <- "query GetOrganizations { account { organizations { {{fields}} } } }"

  r <- buffer_query(template, fields = fields) |>
    buffer_request(dry_run = dry_run)

  if (isTRUE(attr(r, "dry_run"))) {
    return(r)
  }

  r |>
    httr2::resp_body_json() |>
    fetch("id")
}
