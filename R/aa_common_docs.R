# common_docs ------------------
#' Common arguments and documentation for various functions
#'
#' @param test_run Logical. Whether to do a test run (i.e. post to a test area)
#' @param dry_run Logical. Whether to do a dry run (i.e. don't post)
#' @param quiet Logical. Whether to suppress progress messages.
#' @param verbose Logical. Show extra informative messages.
#' @param print Logical. Whether to simply print the text to console instead of
#'   copying to the clipboard.
#' @param force_masto Logical. Passed to [monarch::add_handles()]. Whether or
#'   not to force a re-check of mastodon handles (good if you think they've
#'   changed or they are 'none' and you want to check again).
#'
#' @details
#' Use `@inheritParams common_docs` to include the above in any function
#' documentation with a matching argument (will only include matching args)
#'
#' @keywords internal
#' @name common_docs
NULL
