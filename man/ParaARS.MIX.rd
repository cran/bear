\name{ParaARS.MIX}

\alias{ParaARS.MIX}

\title{Adjusted R squared (ARS) method and lme for parallel study}

\description{
Adjusted R squared (ARS) method:
This method selects data points to estimate lambda(z) based on the  maximun
adjustedR squred values.  It starts with the last three data points from the
concentration-time course, performing log-linear regression to calculate
the slope of that tail portion of the concentration-time curve.  And then
the last 4 data points, the last 5 data points, on and on until it excludes
the data points of Cmax. Thus, this method may exclude the data point of
(Tmax, Cmax). WNL v6. has the similar algoirthms like this.

With a two-treatment, two-sequence, one-period, parallel design, bear
deploys the linear mixed effect model (lme).  The statistical model
includes a factor regarding only one source of variation - treatment.
There are no sources of variation associated with sequence or period
because there are no sequences or periods in a parallel design.  Moreover,
lme in nlme package is used to obtain estimates for the adjusted
differences between treatment means and the standard error.
Log-transformed BA measures will also be analyzed.
}

\keyword{misc}