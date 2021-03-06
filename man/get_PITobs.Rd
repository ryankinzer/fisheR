% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_PITobs.R
\name{get_PITobs}
\alias{get_PITobs}
\title{DART PIT tag Observation Data}
\source{
\url{http://www.cbr.washington.edu/dart}
}
\usage{
get_PITobs(
  query_type = c("obs_site", "release_site"),
  obs_site = c("GRA", "B2A", "GRJ"),
  release_site = NULL,
  species = c("Chinook", "Coho", "Steelhead", "Sockeye"),
  run = c("All", "Spring", "Summer", "Fall", "Winter", "Unknown"),
  rear_type = c("All", "W+H", "Wild", "Hatchery", "Unknown"),
  start_date = NULL,
  end_date = NULL
)
}
\arguments{
\item{query_type}{Query PIT-tag observation data for either an observation site (default) or
a release site.}

\item{obs_site}{For use with observation site query. Currently only available for Lower Granite Dam (GRA, default) and Bonneville (B2A).}

\item{release_site}{For use with release site query. Use PTAGIS release codes.}

\item{species}{Species to query. Possible choices are: Chinook, Coho, Steelhead, Sockeye}

\item{run}{Run to query. Possible choices are: All, Spring, Summer, Fall, Winter, Unknown}

\item{rear_type}{Rear type to query. Possible choices are: All, W+H, Wild, Hatchery, Unknown}

\item{start_date}{Observation start date with format mm/dd/yyyy}

\item{end_date}{Observation end date with format mm/dd/yyyy}
}
\description{
Query DART's PIT tag observation detail records.
}
\examples{

queryPITtagAdult(spawn_yr = 2015)
}
\author{
Ryan N. Kinzer
}
