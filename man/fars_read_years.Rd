% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_function.R
\name{fars_read_years}
\alias{fars_read_years}
\title{Parse column month and column year from the data set specified by year

This is a function returning the month and year column from the specified
data set. The data set is specified by year, the year the data set
is for. This function can also take a list or a vector of years.}
\usage{
fars_read_years(years)
}
\arguments{
\item{years}{An integer year, a list of integer years and a vector of integer
years}
}
\value{
the column year and column month parsed from the data frame correspo-
nding to the \code{years} passed into the function
}
\examples{
\dontrun{
fars_read_years(list(2015, 2013))
fars_read_years(c(2015, 2013, 2014))
fars_read_years(2013)
}

}

