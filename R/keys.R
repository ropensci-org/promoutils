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
  msg <- keys_check_internal()
  cli::cli_inform(c("Key status ", msg))
}

keys_check_internal <- function(msg = TRUE) {
  keys <- c("slack", "matomo", "linkedin", "linkedin_org", "github")
  status <- purrr::map_lgl(keys, \(k) key(k, check = TRUE))
  if (!msg) {
    return(rlang::set_names(status, keys))
  }
  msg <- rlang::set_names(
    paste0(keys, ": ", status),
    ifelse(status, "v", "x")
  )
  if (all(status)) {
    msg <- c(msg, ">" = "Good to go!")
  } else {
    msg <- c(msg, "x" = "Some keys to setup")
  }
  msg
}

#' Set keys required for promoutils API access
#'
#' This function guides uses through the finding and setting tokens so that
#' promoutils can find them.
#'
#' @param type Character Vector. Keys/tokens to set ("slack", "matomo",
#' "linkedin", "github").
#'
#' @returns Nothing, side effect of setting token credentials in the keyring or
#' via `gitcreds::gitcres_set()` for GitHub tokens.
#'
#' @export

keys_set <- function(type = NULL) {
  keys <- keys_check_internal(msg = FALSE)

  #keys <- keys[!keys]
  keys <- !keys

  if (!all(keys)) {
    if (!is.null(type)) {
      keys <- keys[keys %in% type]
    }

    for (k in names(keys)) {
      if (k == "slack") {
        key_guide(
          k,
          "See the Slack vignette for details. Briefly...",
          c(
            "Go to <https://docs.slack.dev/app-management/quickstart-app-settings/>",
            "Create a new app",
            "Create the scopes",
            "Install and authorize the app",
            "Copy the token",
            "Paste the token into the 'keyring' popup (or Esc to do this later)"
          )
        )
      }
      if (k == "matomo") {
        key_guide(
          k,
          c(
            "Fetch the Matomo token from the Administration Panel: ",
            "<https://matomo.org/faq/general/faq_114/>"
          ),
          "Paste the token into the 'keyring' popup (or Esc to do this later)"
        )
      }
      if (k == "linkedin") {
        key_guide(
          k,
          "See the LinkedIn vignette for details. Briefly...",
          c(
            "Fetch *and reset* the LinkedIn token/secret with `li_auth()`",
            "You must be admin in the LinkedIn rOpenSci Organization",
            "Use the `refresh_token` list item returned by `li_auth()`",
            "Note that this function not only fetches but *resets* the token",
            "Paste the token into the 'keyring' popup (or Esc to do this later)"
          )
        )
      }
      if (k == "github") {
        key_guide(
          k,
          "To add your GitHub token...",
          c(
            "Go to <https://github.com/settings/tokens>",
            "'Generate a new token' Classic, for general use",
            "Give this token to `gitcreds::gitcreds_set()`",
            "Calling `gitcreds::gitcreds_set()`..."
          )
        )
      }
    }
  } else {
    cli::cli_alert_success("All keys already set")
  }
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


key_guide <- function(type, msg, bullets) {
  cli::cli_h2(paste0("Set ", tools::toTitleCase(type), " Token: "))
  cli::cli_inform(msg)
  cli::cli_li(bullets)
  if (interactive()) {
    ready <- utils::askYesNo(paste0(
      "Are you ready to set your ",
      tools::toTitleCase(type),
      " token?"
    ))
  }
  if (ready) {
    if (type == "github") {
      gitcreds::gitcreds_set()
    } else {
      keyring::key_set(type, prompt = paste(tools::toTitleCase(type), "Token"))
    }
  }
}
