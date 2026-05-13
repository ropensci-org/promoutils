ed_fix_text <- function(path) {
  path <- "../roweb3/content/blog/2026-03-27-mentors-2026/index.md"
  l <- readLines(path)

  stringr::str_replace_all("testing\nchampions", ro_champions())
}

ro_champions <- function() {
  c("\\bchampion" = "Champion", "(C|c)hampions program" = "Champions Program")
}
