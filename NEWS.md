# 0.6.1

- Export `gh_issue_fetch()`, `gh_issue_fmt()` and `gh_issue_post()`
- Add `gh_issue_close()`
- keyring is suggested
- Add url_from_api()
- Add wordlist functions
- Fix `slack_cleanup()` with multiple users (ignores messages cannot remove)
- Add `slack_message_rm_bulk()`

## Bug fixes

- Fix `replace_emoji()` with multiple times
- Fix `li_posts_write()` where doesn't pass on request

# 0.6

- Consolidate and standarize key management - keys_check() etc.
- Add `help_wanted_json()` for rosadmin/help-wanted
- Add authentication details to articles

# 0.5

- Update LinkedIn API version
- Use templates
- Use R Universe for packages
- Move Usecases and help wanted etc. workflows to promoutils
- Use cli
- Add consistency
- Add Throwback Thursday functions
- Remove discourse functions

# 0.4.4

- Preliminary workflows for automatic Slack message scheduling

# 0.4.3

- New version of LinkedIn API
- Add more coworking helper functions

# 0.4.2

- Create Discourse user name function
- Move forum functions from workflow to promoutils
- Remove {} from package names in forum resources

# 0.4.1

- Can supply path to text file with both Mastodon and LinkedIn bodies for social
  media posts
- Fix bug in coworking posts where long yamls are not correctly processed
- Fix bug in coworking posts where multiple authors aren't handled correctly
- Opening browser when posting issues is now optional

# 0.4.0

- Tweak post templates
- Automatically create post issues on GitHub
- Add functions for posting to linkedIn
- Revamp functions
- Omit twitter
- Rename functions for clarity
- Add functions from scheduled_socials
- Helper function to create mastodon handles from urls

# 0.3.2

- Tweak post templates

# 0.3.1

- Fix get_issues use of map()

# 0.3.0

- Add NEWS.md
- Add post templates for coworking reminders
