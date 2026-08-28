# LinkedIn constants
li_org <- "urn:li:organization:77132573"
usethis::use_data(li_org, overwrite = TRUE)

# Buffer constants
buff_org <- buffer_org()
channels <- buffer_channels()
buff_mastodon <- channels$id[channels$service == "mastodon"]
buff_linkedin <- channels$id[channels$service == "linkedin"]
buff_bluesky <- channels$id[channels$service == "bluesky"]
usethis::use_data(
  buff_org,
  buff_mastodon,
  buff_linkedin,
  buff_bluesky,
  overwrite = TRUE
)

# Social Media Posts Character limits -------------------------------
# https://developers.buffer.com/guides/character-limits.html
# Get mastodon from server
template <- "query { channel(input: { id: \"69f8afed5c4c051afa0c1a3c\" }) { service metadata { ... on MastodonMetadata { maxCharacters } } } }"
q <- buffer_query(template)

masto_nchars <- buffer_request(q) |>
  buffer_df() |>
  tidyr::unnest("metadata") |>
  dplyr::pull(metadata)

buff_nchars <- c("mastodon" = masto_nchars, "bluesky" = 300, "linkedin" = 3000)
usethis::use_data(
  buff_nchars,
  overwrite = TRUE
)

# Coworking template ------------------------------------------------------
cw_template <- gh::gh(
  "/repos/rosadmin/comms/contents/.github/ISSUE_TEMPLATE/coworking-prep.md",
  .accept = "application/vnd.github.raw+json"
) |>
  unlist() |>
  stringr::str_remove("^---(\\S|\\s)+---\\n?\\n?") |> # Remove YAML
  stringr::str_remove_all("\\[\\[[^\\[\\]]+\\]\\]") # Remove comments

system.file("extdata", "templates", package = "promoutils") |>
  file.path("cw_checklist.txt") |>
  writeLines(cw_template, con = _)
