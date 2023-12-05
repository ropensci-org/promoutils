test_that("escape_linkedin_chars()", {

  x <- paste(
    "Testing out the LinkedIn API via R and httr2!",
    "And again with links...",
    "",
    "- https://docs.ropensci.org/weathercan",
    "- weathercan docs (https://docs.ropensci.org/weathercan)",
    "- (what about other things in brackets?)",
    "",
    "🎉",
    sep = "\n"
  )


  escape_linkedin_chars(x)
})
