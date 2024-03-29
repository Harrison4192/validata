% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diagnose.R
\name{diagnose_category}
\alias{diagnose_category}
\title{diagnose category}
\usage{
diagnose_category(.data, ..., max_distinct = 5)
}
\arguments{
\item{.data}{dataframe}

\item{...}{tidyselect}

\item{max_distinct}{integer}
}
\value{
dataframe
}
\description{
counts the distinct entries of categorical variables. The \code{max_distinct} argument limits the scope to
categorical variables with a maximum number of unique entries, to prevent overflow.
}
\examples{

iris \%>\%
diagnose_category()
}
