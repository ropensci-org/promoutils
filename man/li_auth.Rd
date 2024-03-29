% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linkedin.R
\name{li_auth}
\alias{li_auth}
\title{Authorize rOpenSci client with LinkedIn}
\usage{
li_auth()
}
\value{
httr2 authorization
}
\description{
This authorizes the rOpenSci client with \strong{your} credentials (and you must
be part of the rOpenSci organization as an admin).
Make sure to take note of the 'refresh_token' as that is what you'll
add to your .Renviron file for local work, or the GitHub secrets for
the comms/scheduled_socials workflow.
}
\details{
This function authorizes with a redirect url of "http://localhost:1444/",
this \emph{must} be the same as that listed in the LinkedIn Developer App,
https://www.linkedin.com/developers/apps.

\strong{If you retrieve a new token, you will have to put it in the .Renviron
and the re-start your R session to continue}

This function authorizes with the scopes:
\itemize{
\item w_member_social (default)
\item w_organization_social (special request)
\item r_organization_social (special request)
\item r_organization_admin (special request)
}
}
\examples{

\dontrun{
# Only run if you need to update the scopes or get a new token (otherwise
# you'll have to replace all your tokens)
t <- li_auth()
t$refresh_token
}
}
\references{
\itemize{
\item Refresh tokens API: https://learn.microsoft.com/en-us/linkedin/shared/authentication/programmatic-refresh-tokens
}
}
