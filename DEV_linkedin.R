# This script requests a 1-year authorization token
#
# This token will expire after one year and will need to be refreshed by running this script.
#
# - Put this token in the GitHub secrets as HTTR2_REFRESH_TOKEN
# - For local testing add HTTR2_REFRESH_TOKEN to your .Renviron file
ro_org_id <- 77132573
ro_urn <- paste0("urn:li:organization:", ro_org_id)
steffi_urn <- li_urn_me()

# Define the client
#ro_auth()
#ro_auth()$refresh_token
#usethis::edit_r_environ()

# Get rOpenSci posts
p <- li_posts_read(ro_urn)

# Get error details about last response
httr2::last_response() |>
  httr2::resp_body_json()

# Write as test organization
id <- li_posts_write(
  author = "urn:li:organization:5515715",
  body = "testing 1...2...3..",
  dry_run = FALSE)

# Write as Steffi
id <- li_posts_write(
  author = ro_urn,
  body = "Testing out the LinkedIn API via R and httr2!",
  dry_run = TRUE)


id <- li_posts_write(
  author = li_urn_me(),
  body =
    paste(
      "Testing out the LinkedIn API via R and httr2!",
      "And again with links...",
      "",
      "- https://docs.ropensci.org/weathercan",
      "- weathercan docs (https://docs.ropensci.org/weathercan)",
      "- (what about other things in brackets?)",
      "",
      "🎉",
      sep = "\n"
    ), dry_run = FALSE
)


p <- paste(
  "Testing out the LinkedIn API via R and httr2!",
  "And again with links...",
  "",
  "- https://docs.ropensci.org/weathercan",
  "- weathercan docs (https://docs.ropensci.org/weathercan)",
  #"- (what about other things in brackets?)",
  "",
  "🎉",
  sep = "\n"
)

escape_url_brackets(p) |> cat()



li_posts_write(author = steffi_urn,
               body = glue::glue(
                 "testing urls... in line https://steffilazerte.ca",
                 "",
                 "with (https://github.com/steffilazerte)",
                 "with hashtags #testing", .sep = "\n"))
