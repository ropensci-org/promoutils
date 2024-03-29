% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{masto2user}
\alias{masto2user}
\title{Convert a mastodon user link to handle}
\usage{
masto2user(x)
}
\arguments{
\item{x}{Character. Link to user's profile}
}
\value{
Character user handle @user@instance
}
\description{
Convert a mastodon user link to handle
}
\examples{
masto2user("https://fosstodon.org/@steffilazerte")
masto2user("steffi")
masto2user("@steffilazerte@fosstodon.org")
masto2user(NA)
}
