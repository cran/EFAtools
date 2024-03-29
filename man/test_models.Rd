% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test_models_doc.R
\docType{data}
\name{test_models}
\alias{test_models}
\title{Four test models used in Grieder and Steiner (2020)}
\format{
A list of 4 lists "baseline", "case_1a", "case_6b", and"case_11b", each with the following elements.
\describe{
  \item{cormat}{(matrix) - The correlation matrix of the simulated data.}
  \item{n_factors}{(numeric) - The true number of factors.}
  \item{N}{(numeric) - The sample size of the generated data.}
 }
}
\source{
Grieder, S., & Steiner, M.D. (2020). Algorithmic Jingle Jungle:
A Comparison of Implementations of Principal Axis Factoring and Promax Rotation
 in R and SPSS. Manuscript in Preparation.
}
\usage{
test_models
}
\description{
Correlation matrices created from simulated data from four of the
\code{\link{population_models}} cases, each with strong factor intercorrelations.
These are used in Grieder & Steiner (2020) to compare the psych and SPSS
implementations in this package with the actual implementations of the programs.
For details on the cases, see \code{\link{population_models}}.
}
\keyword{datasets}
