% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/latlon_from_address.R
\name{address_from_table_goodnames}
\alias{address_from_table_goodnames}
\title{utility to get USPS addresses from a table that has correct colnames}
\usage{
address_from_table_goodnames(
  x,
  colnames_allowed = c("address", "street", "city", "state", "zip")
)
}
\arguments{
\item{x}{a table with columns that overlap with colnames_allowed}

\item{colnames_allowed}{optional}
}
\value{
vector of USPS addresses
}
\description{
utility to get USPS addresses from a table that has correct colnames
}
\seealso{
\code{\link[=address_from_table]{address_from_table()}}
}
\keyword{internal}
