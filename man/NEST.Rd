% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NEST.R
\name{NEST}
\alias{NEST}
\title{Next eigenvalue sufficiency test (NEST)}
\source{
Achim, A. (2017). Testing the number of required dimensions in exploratory factor analysis. The Quantitative Methods for Psychology, 13(1), 64–74. https://doi.org/10.20982/tqmp.13.1.p064

Brandenburg, N., & Papenberg, M. (2024). Reassessment of innovative methods to determine the number of factors: A simulation-based comparison of exploratory graph analysis and Next Eigenvalue Sufficiency Test. Psychological Methods, 29(1), 21–47. https://doi.org/10.1037/met0000527

Caron, P.-O. (2025). A Comparison of the Next Eigenvalue Sufficiency Test to Other Stopping Rules for the Number of Factors in Factor Analysis.
Educational and Psychological Measurement, Online-first publication. https://doi.org/10.1177/00131644241308528
}
\usage{
NEST(
  x,
  N = NA,
  alpha = 0.05,
  use = c("pairwise.complete.obs", "all.obs", "complete.obs", "everything",
    "na.or.complete"),
  cor_method = c("pearson", "spearman", "kendall"),
  n_datasets = 1000,
  ...
)
}
\arguments{
\item{x}{data.frame or matrix. data.frame or matrix of raw data or matrix with
correlations.}

\item{N}{numeric. The number of observations. Only needed if x is a correlation
matrix.}

\item{alpha}{numeric. The alpha level to use (i.e., 1-alpha percentile of eigenvalues is used for reference values).}

\item{use}{character. Passed to \code{\link[stats:cor]{stats::cor}} if raw
data is given as input. Default is  \code{"pairwise.complete.obs"}.}

\item{cor_method}{character. Passed to \code{\link[stats:cor]{stats::cor}}.
Default is  \code{"pearson"}.}

\item{n_datasets}{numeric. The number of datasets to simulate. Default is 1000.}

\item{...}{Additional arguments passed to \code{\link{EFA}}. For example,
the extraction method can be changed here (default is "PAF"). PAF is more
robust, but it will take longer compared to the other estimation methods
available ("ML" and "ULS").}
}
\value{
A list of class NEST containing the following objects
\item{eigenvalues}{A vector containing the empirical eigenvalues of the entered data.}
\item{n_factors}{The number of factors to retain according to the NEST procedure.}
\item{references}{A vector containing the reference eigenvalues.}
\item{prob}{For the first n_factors + 1 empirical eigenvalues, the proportion <= the set of n_datasets  reference eigenvalues.}
\item{settings}{A list of control settings used in the print function.}
}
\description{
NEST uses many synthetic datasets to generate reference eigenvalues against
which to compare the empirical eigenvalues. This is similar to parallel
analysis, but other than parallel analysis, NEST does not just rely on
synthetic eigenvalues based on an identity matrix as null model.
It was introduced by Achim (2017), see also Brandenburg and Papenberg (2024) and
Caron (2025) for further simulation studies including NEST.
}
\details{
NEST compares the first empirical eigenvalue against the first eigenvalues
of \code{n_dataset} synthetic datasets based on a null model  (i.e.,
 with uncorrelated variables; same as in parallel analysis, see \code{\link{PARALLEL}}).
 The following eigenvalues are compared against synthetic datasets based on an EFA-model with one fewer factors
 than the position of the respective empirical eigenvalue. E.g, the second
 empirical eigenvalue is compared against synthetic data based on a one-factor
 model. The \code{alpha}-level defines against which percentile of the synthetic
 eigenvalue distribution to compare the empirical eigenvalues against, i.e., an
 alpha of .05 (the default) uses the 95th percentile as reference value.

 For details on the method, including simulation studies, see Achim (2017),
 Brandenburg and Papenberg (2024), and Caron (2025).

 The \code{NEST} function can also be called together with other factor
  retention criteria in the \code{\link{N_FACTORS}} function.
}
\examples{

# with correlation matrix
NEST(test_models$baseline$cormat, N = 500)

# with raw data
NEST(GRiPS_raw)
}
