% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-data.R
\name{get_ihme}
\alias{get_ihme}
\title{Retrieve IHME data}
\usage{
get_ihme(
  version_date,
  source = "subnational",
  aggregation = "polio",
  vaccine = NULL
)
}
\arguments{
\item{version_date}{A string in the format of "yyyy-mm", corresponding to the version of the IHME file you wish to load.}

\item{source}{A string used to determine which IHME file to load. Options are: "national", "subnational", "raster", and "citations".}

\item{aggregation}{A string used to determine which aggregation to use when loading subnational data. Options are: "polio" or "gadm"}

\item{vaccine}{A string used to detemine which vaccine to use when loading raster or citation data. Use standard vaccine format (e.g. "dtp1")}
}
\value{
a data frame or raster object.
}
\description{
Retrieve IHME data
}
\examples{

#defaults load subnational file aggregated to polio shapes
get_ihme("2022-12")

get_ihme("2022-11", "national")
get_ihme("2022-12", "raster", vaccine = "dtp1")
get_ihme("2022-12", "citations", vaccine = "dtp1")

}
