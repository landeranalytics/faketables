% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{.list_col_to_chr}
\alias{.list_col_to_chr}
\title{Convert list-type columns to character}
\usage{
.list_col_to_chr(x)
}
\arguments{
\item{x}{A data frame}
}
\value{
\code{x}, except list type columns are now \code{character}
}
\description{
This is handy for using \code{\link[shiny:renderTable]{shiny::renderTable()}} with list type
columns
}
\examples{
\dontrun{
df <- tibble::tibble('ls' = list('a', 'b'), 'num' = 1:2)
.list_col_to_chr(df)
}

}
\keyword{internal}