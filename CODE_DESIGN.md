# Code Design

Notes on how promoutils is organized and the conventions to follow when working
on it (for human developers and AI agents alike).

## Overview

promoutils is an R package of utilities for rOpenSci community/comms staff:
gathering content (help-wanted issues, use cases, coworking events, Throwback
Thursdays, Matomo blog stats, coffee chats) and posting/scheduling it via Slack,
Buffer (Mastodon/LinkedIn/Bluesky), LinkedIn and GitHub issues. It is mostly
used interactively by staff.

## Commands

```r
devtools::load_all()
devtools::document() # regenerate NAMESPACE + man/ (roxygen, markdown on)
devtools::test() # all tests
devtools::test(filter = "03_slack") # single file, by filter
devtools::check()
devtools::build_readme() # README.md is generated from README.Rmd (also computes coverage)
source("vignettes/articles/_PRECOMPILE.R") # re-knit *.Rmd.orig -> *.Rmd articles
pkgdown::build_site()
```

The full release checklist is in `RELEASE.R`. Formatting uses Air (`air.toml`,
80 columns); linting uses jarl (`.panache.toml`).

## Credentials

All API access goes through `key(type)` in `R/keys.R`, which looks up
`PU_<TYPE>_KEY` env vars first (e.g. `PU_SLACK_KEY`, `PU_BUFFER_KEY`,
`PU_GITHUB_KEY`, `PU_MATOMO_KEY`, `PU_LINKEDIN_KEY`, `PU_LINKEDIN_ORG_KEY`),
then falls back to `keyring` only when interactive (GitHub also falls back to
`gh::gh_token()`). Tests are non-interactive, so live API tests need keys in
`.Renviron`, not the keyring.

## Tests

- `test-NN_*.R`: offline tests. They use `dry_run = TRUE`,
  `httptest2::with_mock_dir("../mock/<name>", ...)` (fixtures in `tests/mock/`),
  and helpers from `R/utils-test.R` (e.g. `local_mocked_cocoon()` mocks
  `monarch::cocoon_open()`, `test_help_data()`).
- `test-all-NN_*.R`: live API tests guarded by `skip_if_not_all()`. They run
  locally always, on CI only when `TEST_ALL=yes` (scheduled/manual workflow
  runs), and never on R-Universe. Slack live tests only post to `#testing-api`
  and clean up after themselves.
- Test helpers live in `R/utils-test.R` (inside the package, not
  `tests/testthat/helper-*.R`).

## Architecture

- **Per-service modules**: most external API function groups have a public file
  plus a `*_utils.R` with request/response plumbing
  - `slack.R`/`slack_utils.R` - httr2, bearer auth, `slack_paginate()` +
    `slack_check()` for `ok`-field errors
  - `buffer.R`/`buffer_utils.R` - GraphQL: `buffer_query()` fills templates via
    `buff_glue()` with `{{ }}` delimiters, `buffer_request()`
  - `linkedin.R` - LinkedIn API
  - `gh_issues.R` - gh package to access GitHub issues
  - `matomo.R` - Matomo page views
- **Content workflows** generally follow a fetch → format → add handles →
  per-platform → post pipeline, e.g.
  `help_fetch() |> help_handles() |> by_platform()` then `help_post()`;
  `uc_fetch() |> uc_fmt() |> uc_handles() |> by_platform()`. Social handles come
  from the `monarch` package (`monarch::add_handles()`). Posting to social media
  is centralized in `buffer_posts_write()`; older issue-based posting
  (`socials_post_issue()`) is deprecated.
- **Coworking** (`coworking.R`, prefix `cw_`) builds events, GitHub issues,
  social posts and scheduled Slack messages from text templates in
  `inst/extdata/templates/*.txt`, loaded with `template(name)` and filled with
  glue.
- **Coffee chats** (`chats.R`, prefix `chats_`) read Slack sign-ups, generate
  pairings and announce them.
- **Caching**: `cache_dir()`/`cache_write()`/`cache_read()` store CSVs under
  `tools::R_user_dir("promoutils")/<type>` (help-wanted, matomo, chats...).
  `gh_cache()` and (in `.onLoad`) `slack_users()`/`slack_channels()` are
  memoised.
- **Package data** (`data/`, built by `data-raw/data.R`): Buffer org/channel IDs
  (`buff_org`, `buff_mastodon`, ...), `buff_nchars` character limits, `li_org`.
- `dry_run = TRUE` is the convention for "don't touch the API"; shared roxygen
  params live in `R/aa_common_docs.R` (`@inheritParams common_docs`).
- User-facing messages/errors use `cli`.

## Slack

- Check the accepted content types for each API method (listed on its page at
  https://docs.slack.dev) before choosing how to send arguments.
  - Write methods (e.g., `chat.postMessage`, `chat.delete`) accept JSON, so use
    `httr2::req_body_json()`.
  - Most read methods (e.g., `chat.getPermalink`, `conversations.history`) only
    accept query/form parameters, so use `httr2::req_url_query()`. JSON sent to
    these methods is silently ignored and Slack returns `invalid_arguments`.
  - Query parameters work for all methods, so use them when unsure.

## Generated files

Don't hand-edit generated files; edit the source and regenerate:

- `NAMESPACE` and `man/` → roxygen comments, then `devtools::document()`.
- `README.md` → `README.Rmd`, then `devtools::build_readme()`.
- Articles in `vignettes/articles/` are precompiled: edit the `*.Rmd.orig`
  source, then run `_PRECOMPILE.R` to regenerate the `*.Rmd`. The articles
  directory is Rbuildignored (pkgdown-only).
