% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{replace_emoji}
\alias{replace_emoji}
\title{Replace emoji codes with unicode}
\usage{
replace_emoji(x)
}
\arguments{
\item{x}{Character. Text string within which to replace codes}
}
\value{
Text string with emoji unicodes
}
\description{
Replaces emoji codes like :tada: with unicode like 🎉.
}
\examples{
x <- replace_emoji("hi :tada: testing \n\n\n Whow ! 🔗 \n\n\n :smile:")
x
}
