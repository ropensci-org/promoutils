# Code Coverage ------------------------------------------
p <- covr::package_coverage(
  function_exclusions = "^li_auth$|^discourse_user$|^cw_checkin|^docs_link|^forum",
  line_exclusions = "R/zzz.R"
)
covr::report(p, file = "coverage_report.html")
