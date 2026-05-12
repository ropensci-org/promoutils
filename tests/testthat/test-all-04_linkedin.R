test_that("li_urn_me()", {
  skip_if_not_all()

  expect_silent(li_urn_me()) |>
    expect_equal("urn:li:person:Bxn4HyByQ5")
})

test_that("li_posts_read()", {
  skip_if_not_all()

  expect_silent(p <- li_posts_read(ro_urn))
  expect_type(p, "list")
  expect_named(p, c("paging", "elements"))
})
