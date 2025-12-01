test_that("gh_name()", {
  expect_silent(n <- gh_name("steffilazerte"))
  expect_equal(n, "Steffi LaZerte")
  expect_silent(n <- gh_name("XXXXXX"))
  expect_equal(n, NA_character_)
})

test_that("gh_user()", {
  expect_silent(
    u <- gh_user(name = "Steffi", owner = "ropensci", pkg = "weathercan")
  )
  expect_s3_class(u, "data.frame")
  expect_equal(u, dplyr::tibble(name = "Steffi", gh_user = "steffilazerte"))

  expect_silent(
    u <- gh_user(
      name = "Steffi E. LaZerte",
      owner = "ropensci",
      pkg = "weathercan"
    )
  )
  expect_s3_class(u, "data.frame")
  expect_equal(
    u,
    dplyr::tibble(name = "Steffi LaZerte", gh_user = "steffilazerte")
  )

  expect_silent(
    u <- gh_user(name = "XXXXXX", owner = "ropensci", pkg = "weathercan")
  )
  expect_equal(u, data.frame(name = "XXXXXX", gh_user = NA_character_))
})

test_that("masto_user()", {
  expect_silent(m <- masto_user(name = "Steffi LaZerte"))
  expect_silent(m <- masto_user("steffilazerte"))
})

test_that("gh_masto()", {
  expect_silent(m <- gh_masto("steffilazerte"))
  expect_equal(m, "https://fosstodon.org/@steffilazerte")
  expect_silent(m <- gh_masto("XXXX"))
  expect_equal(m, NA_character_)
})

test_that("ro_masto()", {
  expect_silent(m <- ro_masto("Steffi LaZerte"))
  expect_equal(m, "https://fosstodon.org/@steffilazerte")
  expect_silent(m <- ro_masto("XXXXXX"))
  expect_equal(m, NA_character_)
})

test_that("all_users()", {
  expect_silent(u <- all_users(name = "Steffi LaZerte", pkg = "weathercan"))
  expect_equal(
    u,
    dplyr::tibble(
      gh_user = "steffilazerte",
      masto_user = "https://fosstodon.org/@steffilazerte"
    )
  )
})
