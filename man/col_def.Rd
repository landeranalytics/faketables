% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/definitions.R
\name{col_def}
\alias{col_def}
\title{Create a \code{faketable} column definition}
\usage{
col_def(name, input_call, cast, width, display_name = name, ...)
}
\arguments{
\item{name}{The column name}

\item{input_call}{A \code{\link[=input_call]{input_call()}} object}

\item{cast}{A bare function call to convert the column to its intended class.
It is important that this match exactly, or the \code{updated} table will
contain all rows.}

\item{width}{The grid width of the column (must be between 1 and 12)}

\item{display_name}{The name to use for the column header}

\item{...}{Elements to include within the column}
}
\value{
A \code{\link[=col_def]{col_def()}} object
}
\description{
Create a \code{faketable} column definition
}
\examples{
# to create a col_def for mtcars$mpg
# it is a shiny::textInput that is disabled using shinyjs::disabled
col_def(
  name = 'mpg',
  input = input_call(
    fun = shiny::textInput,
    args = list(label = NULL, placeholder = 'mpg')
  ),
  cast = as.numeric,
  width = 3,
  display_name = 'MPG'
)
}
