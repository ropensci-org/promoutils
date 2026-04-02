test_that("li_client()", {
  expect_s3_class(li_client(), "httr2_oauth_client")
  expect_equal(li_client()$name, "rOpenSci_linkedIn")
})

test_that("li_urn_me()", {
  expect_silent(li_urn_me()) |>
    expect_equal("urn:li:person:Bxn4HyByQ5")
})


test_that("li_req_posts()", {
  expect_silent(p <- li_req_posts())
  expect_s3_class(p, "httr2_request")
  expect_equal(p$url, "https://api.linkedin.com/rest/posts")
})

test_that("li_posts_read()", {
  expect_silent(p <- li_posts_read(ro_urn))
  expect_type(p, "list")
  expect_named(p, c("paging", "elements"))
})

test_that("li_posts_write()", {
  expect_output(
    details <- li_posts_write(
      author = ro_urn, # Post on behalf of rOpenSci
      body = "Testing out the LinkedIn API via R and httr2!",
      dry_run = TRUE
    ),
    "POST \\/rest\\/posts HTTP\\/1.1"
  )
  expect_type(details, "list")
})
