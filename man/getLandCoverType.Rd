% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getLandCoverType.R
\name{getLandCoverType}
\alias{getLandCoverType}
\title{Retrieves the land cover type of a target geometry}
\usage{
getLandCoverType(
  pts = NULL,
  dataset_path = "~/OneDrive/Datasets/",
  separate_forests = TRUE
)
}
\arguments{
\item{pts}{target points}

\item{dataset_path}{path to the 'Datasets' directory}

\item{separate_forests}{boolean flag to separate forests from shrublands/grasslands}
}
\description{
Retrieves the land cover type of a target geometry
}
