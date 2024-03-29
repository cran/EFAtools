% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/COMPARE.R
\name{COMPARE}
\alias{COMPARE}
\title{Compare two vectors or matrices (communalities or loadings)}
\usage{
COMPARE(
  x,
  y,
  reorder = c("congruence", "names", "none"),
  corres = TRUE,
  thresh = 0.3,
  digits = 4,
  m_red = 0.001,
  range_red = 0.001,
  round_red = 3,
  print_diff = TRUE,
  na.rm = FALSE,
  x_labels = c("x", "y"),
  plot = TRUE,
  plot_red = 0.01
)
}
\arguments{
\item{x}{matrix, or vector. Loadings or communalities of a factor
analysis output.}

\item{y}{matrix, or vector. Loadings or communalities of another
factor analysis output to compare to x.}

\item{reorder}{character. Whether and how elements / columns should be
reordered. If "congruence" (default), reordering is done according to Tuckers
correspondence coefficient, if "names", objects according to their names,
if "none", no reordering is done.}

\item{corres}{logical. Whether factor correspondences should be compared if a
matrix is entered.}

\item{thresh}{numeric. The threshold to classify a pattern coefficient as substantial. Default is .3.}

\item{digits}{numeric. Number of decimals to print in the output. Default is 4.}

\item{m_red}{numeric. Number above which the mean and median should be printed
in red (i.e., if .001 is used, the mean will be in red if it is larger than
.001, otherwise it will be displayed in green.) Default is .001.}

\item{range_red}{numeric. Number above which the min and max should be printed
in red (i.e., if .001 is used, min and max will be in red if the max is larger
 than .001, otherwise it will be displayed in green. Default is .001). Note that
 the color of min also depends on max, that is min will be displayed in the
 same color as max.}

\item{round_red}{numeric. Number above which the max decimals to round to where
all corresponding elements of x and y are still equal are displayed in red
(i.e., if 3 is used, the number will be in red if it is smaller than
 3, otherwise it will be displayed in green). Default is 3.}

\item{print_diff}{logical. Whether the difference vector or matrix should be
printed or not. Default is TRUE.}

\item{na.rm}{logical. Whether NAs should be removed in the mean, median, min,
and max functions. Default is FALSE.}

\item{x_labels}{character. A vector of length two containing identifying
labels for the two objects x and y that will be compared. These will be used
as labels on the x-axis of the plot. Default is "x" and "y".}

\item{plot}{logical. If TRUE (default), a plot illustrating the differences
will be shown.}

\item{plot_red}{numeric. Threshold above which to plot the absolute differences
in red. Default is .001.}
}
\value{
A list of class COMPARE containing summary statistics on the differences
 of x and y.

\item{diff}{The vector or matrix containing the differences between x and y.}
\item{mean_abs_diff}{The mean absolute difference between x and y.}
\item{median_abs_diff}{The median absolute difference between x and y.}
\item{min_abs_diff}{The minimum absolute difference between x and y.}
\item{max_abs_diff}{The maximum absolute difference between x and y.}
\item{max_dec}{The maximum number of decimals to which a comparison makes sense.
 For example, if x contains only values up to the third decimals, and y is a
 normal double, max_dec will be three.}
\item{are_equal}{The maximal number of decimals to which all elements of x and y
 are equal.}
\item{diff_corres}{The number of differing variable-to-factor correspondences
 between x and y, when only the highest loading is considered.}
\item{diff_corres_cross}{The number of differing variable-to-factor correspondences
 between x and y when all loadings \code{>= thresh} are considered.}
\item{g}{The root mean squared distance (RMSE) between x and y.}
\item{settings}{List of the settings used.}
}
\description{
The function takes two objects of the same dimensions containing numeric
information (loadings or communalities) and returns a list of class COMPARE
containing summary information of the differences of the objects.
}
\examples{
# A type SPSS EFA to mimick the SPSS implementation
EFA_SPSS_6 <- EFA(test_models$case_11b$cormat, n_factors = 6, type = "SPSS")

# A type psych EFA to mimick the psych::fa() implementation
EFA_psych_6 <- EFA(test_models$case_11b$cormat, n_factors = 6, type = "psych")

# compare the two
COMPARE(EFA_SPSS_6$unrot_loadings, EFA_psych_6$unrot_loadings,
        x_labels = c("SPSS", "psych"))
}
