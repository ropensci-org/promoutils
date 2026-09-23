#' Cache folder to store data
#'
#' Where the cache will be or is for specific types of data cached by
#' promoutils.
#'
#' @param type Character. Data type to cache.
#'
#' @returns Character file path
#'
#' @export
#' @examples
#' cache_dir(type = "matomo")
#' cache_dir(type = "help-wanted")
cache_dir <- function(type) {
  tools::R_user_dir("promoutils") |>
    file.path(type)
}

cache_write <- function(df, type, path = NULL) {
  path <- path %||% paste0(type, ".csv")
  path <- file.path(cache_dir(type), path)

  if (!dir.exists(cache_dir(type))) {
    cli::cli_inform(
      c("Creating '{type}' folder to save data: ", cache_dir(type))
    )
    dir.create(cache_dir(type), recursive = TRUE)
  }
  if (nrow(df) > 0) {
    cli::cli_inform(c("Writing data to: ", path))
    readr::write_csv(df, path)
  } else {
    cli::cli_inform("No new data to add")
  }

  invisible()
}

#' Read data saved to disk
#'
#' Reads a saved set of data.
#'
#' @returns Data frame of cached data.
#'
#' @noRd

cache_read <- function(type, paths = NULL) {
  if (!dir.exists(cache_dir(type))) {
    return(data.frame())
  }

  paths <- paths %||% list.files(cache_dir(type), full.names = TRUE)

  if (!length(paths)) {
    return(data.frame())
  }

  df <- readr::read_csv(paths, show_col_types = FALSE, progress = FALSE)

  df
}
