test_that("pkgs_ru() & pkg_authors()", {
  expect_silent(p <- pkgs_ru())
  expect_s3_class(p, "data.frame")
  expect_all_true(
    c("package", "title", "owner", "maintainer_name") %in% names(p)
  )
  expect_silent(a <- pkg_authors("weathercan", p))
  expect_equal(a, "Steffi LaZerte")
})


test_that("replace_emoji()", {
  expect_silent(replace_emoji(
    "hi :tada: testing \n\n\n Whow ! 🔗 \n\n\n :smile:"
  )) |>
    expect_match("🎉") |>
    expect_match("😄")
  expect_silent(replace_emoji(":link:")) |>
    expect_match("🔗")
})

test_that("yaml_extract()", {
  expect_silent(yaml_extract("~~~start: 2023-11-12\nauthor: Steffi\n~~~")) |>
    expect_equal(data.frame(start = "2023-11-12", author = "Steffi"))

  # Fix reoccurring typos
  expect_silent(yaml_extract("~~~reoccuring: false\n~~~")) |>
    expect_equal(data.frame(reoccurring = FALSE))
})

test_that("masto2user()", {
  expect <- "@steffilazerte@fosstodon.org"
  expect_equal(masto2user("https://fosstodon.org/@steffilazerte"), expect)
  expect_equal(masto2user("steffi"), "steffi")
  expect_equal(masto2user("@steffilazerte@fosstodon.org"), expect)
  expect_equal(masto2user(NA), NA)
})

test_that("url_from_path()", {
  expect_equal(
    url_from_path("my-post", date = "2025-01-01"),
    "https://ropensci.org/blog/2025/01/01/my-post/"
  )
  expect_equal(
    url_from_path("2025-01-01-my-post/index.es.md"),
    "https://ropensci.org/es/blog/2025/01/01/my-post/"
  )
  expect_equal(
    url_from_path(
      "content/blog/2025-09-29-news-september-2025/index.md"
    ),
    "https://ropensci.org/blog/2025/09/29/news-september-2025/"
  )
  expect_equal(
    url_from_path(
      "content/blog/2025-09-29-news-september-2025/index.Rmd"
    ),
    "https://ropensci.org/blog/2025/09/29/news-september-2025/"
  )
  expect_equal(
    url_from_path("content/blog/2025-09-29-news-september-2025/"),
    "https://ropensci.org/blog/2025/09/29/news-september-2025/"
  )
})

test_that("prs_list()", {
  expect_silent(prs_list()) |>
    expect_s3_class("tbl") |>
    expect_named(c("html_url", "number", "title", "ref"))
})

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

  expect_silent(xx <- escape_linkedin_chars(x))
  expect_true(stringr::str_detect(xx, "\\\\\\("))
})


test_that("fmt_slack_urls()", {
  input <- "[My awesome page](https://my-awesome.html)"
  expected <- "<https://my-awesome.html|My awesome page>"
  result <- fmt_slack_urls(input)
  expect_equal(result, expected)

  # Multiple links
  input <- "Check [link one](https://one.com) and [link two](https://two.com)!"
  expected <- "Check <https://one.com|link one> and <https://two.com|link two>!"
  result <- fmt_slack_urls(input)
  expect_equal(result, expected)

  # No links
  input <- "This is just plain text"
  result <- fmt_slack_urls(input)
  expect_equal(result, input)

  # Complex links
  input <- "[A link with spaces and-dashes](https://example.com/path/to/page)"
  expected <- "<https://example.com/path/to/page|A link with spaces and-dashes>"
  result <- fmt_slack_urls(input)
  expect_equal(result, expected)
})
