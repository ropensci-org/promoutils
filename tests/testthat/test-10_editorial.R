test_that("wordlist_update()", {
  p1 <- withr::local_file("names.txt")
  expect_false(file.exists(p1))
  expect_message(
    p2 <- wordlist_update(c("testing", "words", "to", "add"), p1),
    "Remember to add this dictionary to your spell check config"
  )

  expect_equal(p1, p2)
  expect_true(file.exists(p1))
  expect_equal(readLines(p1), sort(c("testing", "words", "to", "add")))
})

test_that("wordlist_create()", {
  p1 <- withr::local_file("names.txt")
  expect_false(file.exists(p1))
  expect_message(
    p2 <- wordlist_create(path = p1),
    "Remember to add this dictionary"
  )
  expect_true(file.exists(p1))
})

test_that("wordlist_create() updates existing", {
  p <- withr::local_file("names.txt")

  expect_message(
    wordlist_update(c("testing", "words", "to", "add"), p),
    "Remember to add this dictionary"
  )
  expect_message(wordlist_create(path = p), "There is an existing word list") |>
    expect_message("Remember to add this dictionary")

  expect_true(all(c("testing", "words", "to", "add") %in% readLines(p)))
  expect_false(all(readLines(p) %in% sort(c("testing", "words", "to", "add"))))
})
