
test_that("gh_name()", {
  skip_on_ci()
  expect_silent(n <- gh_name("steffilazerte"))
  expect_equal(n, "Steffi LaZerte")
})

test_that("gh_masto()", {
  skip_on_ci()
  expect_silent(m <- gh_masto("steffilazerte"))
  expect_equal(m, "https://fosstodon.org/@steffilazerte")
})

test_that("ro_masto()", {
  skip_on_ci()
  expect_silent(m <- ro_masto("Steffi LaZerte"))
  expect_equal(m, "https://fosstodon.org/@steffilazerte")
})

test_that("gh_user()", {
  skip_on_ci()
  expect_silent(u <- gh_user(name = "Steffi", owner = "ropensci", pkg = "weathercan"))
  expect_s3_class(u, "data.frame")
  expect_equal(u, dplyr::tibble(name = "Steffi", gh_user = "steffilazerte"))

  expect_silent(u <- gh_user(name = "Steffi E. LaZerte", owner = "ropensci", pkg = "weathercan"))
  expect_s3_class(u, "data.frame")
  expect_equal(u, dplyr::tibble(name = "Steffi LaZerte", gh_user = "steffilazerte"))
})

test_that("masto_user()", {
  skip_on_ci()
  expect_silent(m <- masto_user(name = "Steffi LaZerte"))
})
