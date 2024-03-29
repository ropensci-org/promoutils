% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/users.R
\name{masto_user}
\alias{masto_user}
\title{Fetch Mastodon username}
\usage{
masto_user(gh_user = NULL, name = NULL)
}
\arguments{
\item{gh_user}{Character. GH user name.}

\item{name}{Character. Full/Partial name}
}
\value{
Character url to Mastodon profile
}
\description{
Using the GH username or the Full name, check rOpenSci author pages and then
GitHub for references to the person Mastodon account.
}
\examples{
masto_user("steffilazerte")
}
