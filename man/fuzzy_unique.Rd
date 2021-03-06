% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fuzzy.R
\name{fuzzy_unique}
\alias{fuzzy_unique}
\title{Remove approximate duplicated}
\usage{
fuzzy_unique(x, string_dist = 5, protect_length = 5)
}
\arguments{
\item{x}{A vector}

\item{string_dist}{A minimum string distance}

\item{protect_length}{A minimum length a which strings will be considered}
}
\value{
A vector with only one copy of each duplicated element
}
\description{
Removes approximate duplicates using a user-defined cutoff string distance
}
\examples{
\dontrun{
fuzzy_unique(c("Apples", "Oranges", "Bananas", "Apple"),
             string_dist = 3, protect_length = 3)
}
}
