test_that("pkgs() & pkg_authors()", {
  skip_if_offline()

  expect_silent(p <- pkgs())
  expect_s3_class(p, "data.frame")
  expect_named(p, c("name", "maintainer", "owner", "repo"))

  expect_silent(p <- pkgs(which = "all", return = "all"))


  expect_silent(a <- pkg_authors("weathercan", p))
  expect_equal(a, "Steffi LaZerte")
})
