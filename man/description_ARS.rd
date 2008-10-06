\name{description_ARS}

\alias{description_ARS}

\title{Description for Adjusted R squared (ARS) method}

\description{
This method selects data points to estimate lambda(z) based on the  maximun
adjustedR squred values.  It starts with the last three data points from the
concentration-time course, performing log-linear regression to calculate
the slope of that tail portion of the concentration-time curve.  And then
the last 4data points, the last 5 data points, on and on until it includes
the data points of Cmax. Thus, this method may include the data point of
(Tmax, Cmax). WNL v5.x.x has the similar algoirthms like this.  WNL v6. has
similar algorithms except that it will rule out the data point of (Tmax, Cmax).
}

\keyword{misc}