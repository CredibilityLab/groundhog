% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/toc.R
\name{toc}
\alias{toc}
\title{Show CRAN publication dates for all versions of a given package}
\usage{
toc(pkg, dependencies = FALSE)
}
\arguments{
\item{pkg}{(required) package name}

\item{dependencies}{logical (defaults to \code{FALSE}). Should the output contain
package dependencies (\code{Imports}, \code{Depends} and \code{Suggests}) for \code{pkg}.}
}
\value{
a \code{data.frame} where each row corresponds to one version of \code{pkg}, a date column contains the publication date,
and when \code{dependencies}=TRUE, columns show package dependencies over time as well.
}
\description{
Show CRAN publication dates for all versions of a given package
}
\examples{
\dontrun{
toc("R")
toc("magrittr")
toc("rio",dependencies = TRUE)
}

}
