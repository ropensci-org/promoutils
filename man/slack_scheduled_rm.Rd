% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slack.R
\name{slack_scheduled_rm}
\alias{slack_scheduled_rm}
\title{Delete a scheduled message}
\usage{
slack_scheduled_rm(msg = NULL, channel = NULL, id = NULL)
}
\arguments{
\item{msg}{Data frame. Output of \code{slack_list_scheduled()} containing messages
to remove. Should contain columns "channel" and "id"}

\item{channel}{Character. If no msg, the Channel of the message to be
deleted.}

\item{id}{Character. If no msg, the ID of the message to be deleted.}
}
\description{
Delete a scheduled message
}
\examples{
# Schedule message
slack_posts_write("Testing delete msg", when = (Sys.Date() + lubridate::days(2)))

# Confirm scheduled
slack_scheduled_list()

# Remove message
slack_scheduled_list() |>
  dplyr::filter(text == "Testing delete msg") |>
  slack_scheduled_rm()

# Confirm that removed
slack_scheduled_list()

\dontrun{
slack_scheduled_rm(channel = "#testing-api", id = "Q08U4S3J6QG")
}
}
