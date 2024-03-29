% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uci.R, R/uci_package.R
\docType{package}
\name{uci}
\alias{uci}
\alias{_PACKAGE}
\alias{uci-package}
\title{Urban Centrality Index}
\usage{
uci(
  sf_object,
  var_name,
  dist_type = "euclidean",
  bootstrap_border = FALSE,
  showProgress = TRUE,
  parallel = FALSE
)
}
\arguments{
\item{sf_object}{A \verb{POLYGON sf data.frame} of the study area.}

\item{var_name}{A \code{string}. The name of the column in \code{sf_object} with the
number of activities/opportunities/resources/services to be considered
when calculating urban centrality levels. \code{NA} values are considered
to be equal to \code{0}.}

\item{dist_type}{A \code{string} indicating whether calculations should be based
on \code{"euclidean"} distances (Default) or \code{"spatial_link"} distances.
Spatial link distances consider Euclidean distances along the links of
spatial neighbor links. In the case of areas with a concave shape
(like a bay), it is strongly recommended to use \code{"spatial_link"}
distances (even though they are computationally more costly) because
simple Euclidean distances can bias UCI estimates in those cases.}

\item{bootstrap_border}{A \code{logical}. The calculation of UCI requires one to
find the maximum value of the Venables spatial separation index of the
study area. If \code{bootstrap_border = FALSE} (Default), the function uses
a heuristic approach that assumes that the max spatial separation
would occur when all activities were equally distributed along the
border of the study  area. This is a fast approach, but it does not
reach the maximum spatial separation. Alternatively, if \code{bootstrap_border = TRUE},
the function uses a bootstrap approach that simulates 20000 random
distributions of activities along the border and uses the max spatial
separation found. This approach is more computationally expensive and
although it might not return the maximum theoretical value of spatial
separation, it is probably very close to it.}

\item{showProgress}{A \code{logical}. Indicates whether to show a progress bar for
the bootstrap simulation. Defaults to \code{TRUE}.}

\item{parallel}{Decides whether the function should run in parallel. Defaults
is \code{FALSE.} When \code{TRUE}, it will use all cores available minus one using
\code{future::plan()} with strategy \code{"multisession"} internally. Note that
it is possible to create your own plan before calling uci(). In this
case, do not use this argument.}
}
\description{
Calculates the Urban Centrality Index (UCI) as in Pereira et al., (2013) \doi{10.1111/gean.12002}.
The UCI measures the extent to which the spatial organization of a city or
region varies from extreme monocentric to extreme polycentric in a continuous
scale from 0 to 1. Values close to 0 indicate more polycentric patterns and
values close to 1 indicate a more monocentric urban form.

Calculate Urban Centrality Index
}
\section{Usage}{

Please check the vignettes and data documentation on the
\href{https://ipeagit.github.io/uci/index.html}{website}.
}

\examples{
# load data
data_dir <- system.file("extdata", package = "uci")
grid <- readRDS(file.path(data_dir, "grid_bho.rds"))

# calculate UCI
df <- uci(
        sf_object = grid,
        var_name = 'jobs',
        dist_type = "euclidean",
        bootstrap_border = FALSE
        )
head(df)

# calculate UCI with bootstrap
df2 <- uci(
        sf_object = grid,
        var_name = 'jobs',
        dist_type = "euclidean",
        bootstrap_border = TRUE,
        showProgress = TRUE
        )
head(df2)
}
\seealso{
Useful links:
\itemize{
  \item \url{https://github.com/ipeaGIT/uci}
  \item \url{https://ipeagit.github.io/uci/}
  \item Report bugs at \url{https://github.com/ipeaGIT/uci/issues}
}

}
\author{
\strong{Maintainer}: Rafael H. M. Pereira \email{rafa.pereira.br@gmail.com} (\href{https://orcid.org/0000-0003-2125-7465}{ORCID})

}
\concept{urban centrality index}
