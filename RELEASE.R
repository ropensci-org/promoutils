# Steps to release a new version of the promoutils package

# Bump version
file.edit("DESCRIPTION")

# Add changes to NEWS
file.edit("NEWS.md")

# Rebuild README
devtools::build_readme()

# Precompile Vignettes
source("vignettes/articles/_PRECOMPILE.R")

# Standard checks ------------------------------------------
# Use a cache to store values, reset cache to update values
memoise::forget(gh_cache_file) # Removes all cached to file!

devtools::test() # Use Ctrl-Shift-T to test non-interactively
devtools::run_examples()
devtools::check() # Ctrl-Shift-E to check via Terminal

# Additional quality checks -------------------------------

# Checks for good practices, but not code coverage
goodpractice::gp(checks = all_checks()[all_checks() != "cov"])


spelling::spell_check_package() # Check spelling in docs
urlchecker::url_check() # Check URLs in documentation

# Check docs
pkgdown::build_site()

# Platform checks
rhub::rhub_check() # Check on multiple platforms
devtools::check_win_devel() # Check on Windows R-devel

# Push changes
# Once CI clears, merge PR
# Create GitHub release
