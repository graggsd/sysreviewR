% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_RISmed.R
\name{format_RISmed}
\alias{format_RISmed}
\title{Builds a \code{data.frame} containing publication info from an \code{RISmed}
query}
\usage{
format_RISmed(x)
}
\arguments{
\item{x}{An object of class \code{RISmed::Medline} generated by
\code{RISmed::EUtilsGet}}
}
\value{
A \code{data.frame} containing information for each publication from
the \code{RISmed} query
}
\description{
This function takes the output of \code{RISmed::EUtilsGet} and builds a
\code{data.frame} containing information related to each publication from the
query including title, abstract, year of publication, author, journal
information. It may only be applied to objects of class
\code{Medline}.
}
\examples{
\dontrun{
library(RISmed)
res <- EUtilsSummary("myeloma[ti]",retmax=2,reldate=365)
fetch <- EUtilsGet(res)
format_RISmed(fetch)
}
}
\seealso{
\code{\link[RISmed]{EUtilsSummary}} \code{\link[RISmed]{EUtilsGet}}
}
