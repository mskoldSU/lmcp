% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cp_test.R
\name{cp_test}
\alias{cp_test}
\title{Parametric Bootstrap P-value for the no-changepoint hypothesis}
\usage{
cp_test(object, n_sim = 1000)
}
\arguments{
\item{object}{An object of class lmcp}

\item{n_sim}{Number of simulations}
}
\value{
A Monte-Carlo P-value for the no-change-point hypothesis
}
\description{
Parametric Bootstrap P-value for the no-changepoint hypothesis
}
\examples{
fit <- lm_cp(log(PFUnDA) ~ year, data = pfunda_uria)
p_value <- cp_test(fit)
p_value
}
