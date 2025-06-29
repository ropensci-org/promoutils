% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slack.R
\name{slack_posts_write}
\alias{slack_posts_write}
\title{Title}
\usage{
slack_posts_write(
  body,
  when = "now",
  tz = "America/Winnipeg",
  channel = "#testing-api",
  dry_run = FALSE
)
}
\arguments{
\item{dry_run}{}
}
\description{
See https://docs.slack.dev/messaging/formatting-message-text#special-mentions
https://docs.slack.dev/messaging/formatting-message-text#mentioning-users
}
\examples{
slack_posts_write("testing on Tuesday")
slack_posts_write("testing more and more", when = "2025-05-22 14:00:00", tz = "Europe/Paris")
slack_posts_write(
  paste(
  "Join us for Social Coworking and office hours next week!",
  "",
  ":grey_exclamation: Theme: TESTING",
  ":hourglass_flowing_sand: When: TODAY!",
  ":cookie: Hosted by: USER! and community host HOST",
  "",
  "You can use this time for...",
  "- General coworking", sep = "\n"), when = "now")
}
\references{
\itemize{
\item https://docs.slack.dev/messaging/sending-and-scheduling-messages
\item https://docs.slack.dev/messaging/sending-and-scheduling-messages#scheduling
}
}
