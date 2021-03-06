% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_function.R
\name{make_filename}
\alias{make_filename}
\title{Return String the name of the .csv.bz2 file

This is a function return the .csv.bz2 file name with the
given year.}
\usage{
make_filename(year)
}
\arguments{
\item{year}{Integer indicating the year, this is contained in the name of .csv.bz2 file}
}
\value{
return the .csv.bz2 file name with the given year
}
\examples{
\dontrun{
make_filename(year = 2015)
make_filename(2015)}

}

