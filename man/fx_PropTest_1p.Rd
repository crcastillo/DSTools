% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PropTest.R
\name{fx_PropTest_1p}
\alias{fx_PropTest_1p}
\title{Determine Alpha/Beta values for 1-sample Proportion Test}
\usage{
fx_PropTest_1p(siglevel, betalevel, obs, alternative, nullp, altp)
}
\arguments{
\item{siglevel}{The desired Alpha level, i.e. Type I Error - probability of a false positive}

\item{betalevel}{The desired Beta level, i.e. Type II Error - probability of a false negative}

\item{obs}{Vector of potential sample sizes}

\item{alternative}{The method of determining a difference in provided proportions: "greater", "less", "two.sided"}

\item{nullp}{The null hypothesis proportion}

\item{altp}{The alternative hypothesis proportion}
}
\value{
A named list that includes: a data.frame with alpha, beta, and power values for the given vector of observations;
a plot that shows the aforementioned data.frame as well as the observation counts that meet the given alpha and beta
thresholds; the observation counts that meet the given alpha and beta thresholds.
}
\description{
This function takes a provided set of constraints for a one sample proportion test including a vector of test observation counts
and determines the number of observations (from the provided vector) required to surpass the provided Alpha and Beta thresholds.
Essentially, this is to combine both significance testing and power analysis.
}
