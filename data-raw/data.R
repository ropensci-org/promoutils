
ro_urn <- "urn:li:organization:77132573"

usethis::use_data(ro_urn, overwrite = TRUE)



cw_template <- gh::gh("/repos/rosadmin/comms/contents/.github/ISSUE_TEMPLATE/coworking-prep.md",
                      .accept = "application/vnd.github.raw+json") |>
  unlist() |>
  stringr::str_remove("^---(\\S|\\s)+---\\n?\\n?") |> # Remove YAML
  stringr::str_remove_all("\\[[^\\[\\]]+\\]")         # Remove comments

system.file("extdata", "templates", package = "promoutils") |>
  file.path("cw_checklist.txt") |>
  writeLines(cw_template, con = _)
