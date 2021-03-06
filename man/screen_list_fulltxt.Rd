% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/screen.R
\name{screen_list_fulltxt}
\alias{screen_list_fulltxt}
\title{Create a list to screen full texts}
\usage{
screen_list_fulltxt(x, reviewers, cols = "TITLE")
}
\arguments{
\item{x}{A set of publications records for full-text screening}

\item{reviewers}{Names of people that will screen full texts}

\item{cols}{Columns from x to include in the full-text screeners}
}
\value{
A list of of \code{data.frame}s, formatted for each reviewer to
screen abstracts
}
\description{
Creates a list of \code{data.frame}s to screen full texts
}
\examples{
\dontrun{
screening_list <- screen_list_fulltxt(form_mm_recs, c("you", "me"))
}
}
