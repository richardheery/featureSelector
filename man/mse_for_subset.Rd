% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subset_selection.R
\name{mse_for_subset}
\alias{mse_for_subset}
\title{Calculate MSE using new data for the best model of an indicated size identified by subset selection}
\usage{
mse_for_subset(data, response, regsubsets, n)
}
\arguments{
\item{data}{Matrix or data.frame containing the response and features as columns.}

\item{response}{Name of the column containing the response in data.}

\item{regsubsets}{A regsubsets object.}

\item{n}{The model size to evaluate.}
}
\value{
The calculated MSE.
}
\description{
Calculate MSE using new data for the best model of an indicated size identified by subset selection
}
