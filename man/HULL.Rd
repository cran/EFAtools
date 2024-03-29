% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HULL.R
\name{HULL}
\alias{HULL}
\title{Hull method for determining the number of factors to retain}
\source{
Lorenzo-Seva, U., Timmerman, M. E., & Kiers, H. A. (2011).
The Hull method for selecting the number of common factors. Multivariate
Behavioral Research, 46(2), 340-364.
}
\usage{
HULL(
  x,
  N = NA,
  n_fac_theor = NA,
  method = c("PAF", "ULS", "ML"),
  gof = c("CAF", "CFI", "RMSEA"),
  eigen_type = c("SMC", "PCA", "EFA"),
  use = c("pairwise.complete.obs", "all.obs", "complete.obs", "everything",
    "na.or.complete"),
  cor_method = c("pearson", "spearman", "kendall"),
  n_datasets = 1000,
  percent = 95,
  decision_rule = c("means", "percentile", "crawford"),
  n_factors = 1,
  ...
)
}
\arguments{
\item{x}{matrix or data.frame. Dataframe or matrix of raw data or matrix with
correlations.}

\item{N}{numeric. Number of cases in the data. This is passed to \link{PARALLEL}.
Only has to be specified if x is a correlation matrix, otherwise it is determined
based on the dimensions of x.}

\item{n_fac_theor}{numeric. Theoretical number of factors to retain. The maximum
of this number and the number of factors suggested by \link{PARALLEL} plus
one will be used in the Hull method.}

\item{method}{character. The estimation method to use. One of  \code{"PAF"},
\code{"ULS"}, or  \code{"ML"}, for principal axis factoring, unweighted
least squares, and maximum likelihood, respectively.}

\item{gof}{character. The goodness of fit index to use. Either \code{"CAF"},
\code{"CFI"}, or \code{"RMSEA"}, or any combination of them.
If \code{method = "PAF"} is used, only
the CAF can be used as goodness of fit index. For details on the CAF, see
Lorenzo-Seva, Timmerman, and Kiers (2011).}

\item{eigen_type}{character. On what the eigenvalues should be found in the
parallel analysis. Can be one of \code{"SMC"}, \code{"PCA"}, or \code{"EFA"}.
 If using  \code{"SMC"} (default), the diagonal of the correlation matrices is
  replaced by the squared multiple correlations (SMCs) of the indicators. If
   using  \code{"PCA"}, the diagonal values of the correlation
matrices are left to be 1. If using  \code{"EFA"}, eigenvalues are found on the
correlation  matrices with the final communalities of an EFA solution as
diagonal. This is passed to  \code{\link{PARALLEL}}.}

\item{use}{character. Passed to \code{\link[stats:cor]{stats::cor}} if raw data
is given as input. Default is \code{"pairwise.complete.obs"}.}

\item{cor_method}{character. Passed to \code{\link[stats:cor]{stats::cor}}.
Default is  \code{"pearson"}.}

\item{n_datasets}{numeric. The number of datasets to simulate. Default is 1000.
This is passed to \code{\link{PARALLEL}}.}

\item{percent}{numeric. A vector of percentiles to take the simulated eigenvalues from.
Default is 95. This is passed to \code{\link{PARALLEL}}.}

\item{decision_rule}{character. Which rule to use to determine the number of
factors to retain. Default is \code{"means"}, which will use the average
simulated eigenvalues. \code{"percentile"}, uses the percentiles specified
in percent. \code{"crawford"} uses the 95th percentile for the first factor
and the mean afterwards (based on Crawford et al, 2010). This is passed to \code{\link{PARALLEL}}.}

\item{n_factors}{numeric. Number of factors to extract if  \code{"EFA"} is
included in \code{eigen_type}. Default is 1. This is passed to
\code{\link{PARALLEL}}.}

\item{...}{Further arguments passed to \code{\link{EFA}}, also in
\code{\link{PARALLEL}}.}
}
\value{
A list of class HULL containing the following objects
\item{n_fac_CAF}{The number of factors to retain according to the Hull method
with the CAF.}
\item{n_fac_CFI}{The number of factors to retain according to the Hull method
with the CFI.}
\item{n_fac_RMSEA}{The number of factors to retain according to the Hull method
with the RMSEA.}
\item{solutions_CAF}{A matrix containing the CAFs, degrees of freedom, and for the factors lying on the hull, the st values of the hull solution (see Lorenzo-Seva, Timmerman, and Kiers 2011 for details).}
\item{solutions_CFI}{A matrix containing the CFIs, degrees of freedom, and for the factors lying on the hull, the st values of the hull solution (see Lorenzo-Seva, Timmerman, and Kiers 2011 for details).}
\item{solutions_RMSEA}{A matrix containing the RMSEAs, degrees of freedom, and for the factors lying on the hull, the st values of the hull solution (see Lorenzo-Seva, Timmerman, and Kiers 2011 for details).}
\item{n_fac_max}{The upper bound \emph{J} of the number of factors to extract (see details).}
\item{settings}{A list of the settings used.}
}
\description{
Implementation of the Hull method suggested by Lorenzo-Seva, Timmerman,
and Kiers (2011), with an extension to principal axis factoring. See details for
parallelization.
}
\details{
The Hull method aims to find a model with an optimal balance between
 model fit and number of parameters. That is, it aims to retrieve only major
 factors (Lorenzo-Seva, Timmerman, & Kiers, 2011). To this end, it performs
 the following steps (Lorenzo-Seva, Timmerman, & Kiers, 2011, p.351):
 \enumerate{
   \item It performs parallel analysis and adds one to the identified number of factors (this number is denoted \emph{J}). \emph{J} is taken as an upper bound of the number of factors to retain in the hull method. Alternatively, a theoretical number of factors can be entered. In this case \emph{J} will be set to whichever of these two numbers (from parallel analysis or based on theory) is higher.
   \item For all 0 to \emph{J} factors, the goodness-of-fit (one of \emph{CAF}, \emph{RMSEA}, or \emph{CFI}) and the degrees of freedom (\emph{df}) are computed.
   \item The solutions are ordered according to their \emph{df}.
   \item Solutions that are not on the boundary of the convex hull are eliminated (see Lorenzo-Seva, Timmerman, & Kiers, 2011, for details).
   \item All the triplets of adjacent solutions are considered consecutively. The middle solution is excluded if its point is below or on the line connecting its neighbors in a plot of the goodness-of-fit versus the degrees of freedom.
   \item Step 5 is repeated until no solution can be excluded.
   \item The \emph{st} values of the “hull” solutions are determined.
   \item The solution with the highest \emph{st} value is selected.
 }

The \link{PARALLEL} function and the principal axis factoring of the
  different number of factors can be parallelized using the future framework,
  by calling the \link[future:plan]{future::plan} function. The examples
   provide example code on how to enable parallel processing.

  Note that if \code{gof = "RMSEA"} is used, 1 - RMSEA is actually used to
  compare the different solutions. Thus, the threshold of .05 is then .95. This
  is necessary due to how the heuristic to locate the elbow of the hull works.

  The ML estimation method uses the \link[stats:factanal]{stats::factanal}
   starting values. See also the \link{EFA} documentation.

   The \code{HULL} function can also be called together with other factor
   retention criteria in the \code{\link{N_FACTORS}} function.
}
\examples{
\donttest{
# using PAF (this will throw a warning if gof is not specified manually
# and CAF will be used automatically)
HULL(test_models$baseline$cormat, N = 500, gof = "CAF")

# using ML with all available fit indices (CAF, CFI, and RMSEA)
HULL(test_models$baseline$cormat, N = 500, method = "ML")

# using ULS with only RMSEA
HULL(test_models$baseline$cormat, N = 500, method = "ULS", gof = "RMSEA")
}

\dontrun{
# using parallel processing (Note: plans can be adapted, see the future
# package for details)
future::plan(future::multisession)
HULL(test_models$baseline$cormat, N = 500, gof = "CAF")
}
}
\seealso{
Other factor retention criteria: \code{\link{CD}}, \code{\link{EKC}},
\code{\link{KGC}}, \code{\link{PARALLEL}}, \code{\link{SMT}}

\code{\link{N_FACTORS}} as a wrapper function for this and all the
above-mentioned factor retention criteria.
}
