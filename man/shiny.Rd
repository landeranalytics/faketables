% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shiny.R
\name{shiny}
\alias{shiny}
\alias{faketablesUI}
\alias{faketablesServer}
\title{Create the \code{shiny} components required to use \code{faketables}}
\usage{
faketablesUI(id = "faketables")

faketablesServer(id = "faketables", faketable, insert = NULL)
}
\arguments{
\item{id}{An ID string used to identify the module UI}

\item{faketable}{A \code{\link[=faketable]{faketable()}} object}
}
\value{
\itemize{
\item \code{faketablesUI}: A \code{\link[shiny:shinyApp]{shiny::shinyApp()}} ui
\item \code{faketablesServer}: A \code{\link[shiny:reactive]{shiny::reactive()}} object that represents the
passed \code{\link[=faketable]{faketable()}} object
}
}
\description{
Create the \code{shiny} components required to use \code{faketables}
}
\keyword{internal}
