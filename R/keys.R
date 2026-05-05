#' Key status of credentials
#'
#' Check to see if you have set up all the keys required by promoutils.
#'
#' @returns Message informing of status
#'
#' @export
#' @examples
#' keys_check()

keys_check <- function() {
  keys <- c("slack", "matomo", "linkedin", "github")
  status <- purrr::map_lgl(keys, \(k) key(k, check = TRUE))
  msg <- rlang::set_names(
    paste0(keys, ": ", status),
    ifelse(status, "v", "x")
  )
  if (all(status)) {
    msg <- c(msg, ">" = "Good to go!")
  } else {
    msg <- c(msg, "x" = "Some keys to setup")
  }
  cli::cli_inform(c("Key status ", msg))
}

#' Return a key
#'
#' For use in functions which require a key, or need to know if it exists.
#'
#' @param type Character. Type of key to return: "slack", "matomo", "linkedin",
#'   or "github"
#' @param check Logical. If `FALSE`, just return the key, if `TRUE` return
#'   whether the key is present.
#'
#' @returns A key (`check = FALSE`) or logical (`check = TRUE`)
#'
#' @noRd
#' @examples
#' key("slack")

key <- function(type, check = FALSE) {
  key <- Sys.getenv(paste0("PU_", toupper(type), "_KEY"))

  # Use built in GitHub token if not stored as key
  if (type == "github" && key == "") {
    key <- gh::gh_token()
  }

  if (key == "") {
    if (interactive()) {
      key <- tryCatch(keyring::key_get(type), error = \(e) "")
    } else if (!check) {
      cli::cli_abort(
        "Cannot access {tools::toTitleCase(type)} key",
        call = NULL
      )
    }
  }
  if (check) {
    key <- !is.null(key) && key != ""
  }

  key
}
