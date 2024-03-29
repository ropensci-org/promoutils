% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{next_date}
\alias{next_date}
\title{Find the next date}
\usage{
next_date(month, which = "Tues", n = 1)
}
\arguments{
\item{month}{Character/Date. The current month. Date returned is the next month.}

\item{which}{Character/Numeric. Which week day to return. Either number or
abbreviated English weekday.}

\item{n}{Numeric. The nth week to return (i.e. the 1st Tuesday if \code{n = 1}
and \code{which = "Tues"}).}
}
\value{
A date
}
\description{
Given a date and a day of the week,
Given a date return the next month's first Tuesday
}
\examples{

# Get the next first Tuesday
next_date("2023-11-01")
next_date("2023-11-30")

# Get the next 3rd Tuesday
next_date("2023-11-01", n = 3)

# Oops
\dontrun{
next_date("2023-11-01", n = 5)
}

}
