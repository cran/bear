\name{RepAICdemo}

\alias{RepAICdemo}

\title{Akaike information criterion (AIC) method for demo function for replicated study}

\description{
This method selects data points to estimate lambda(z) based on the minimun
AIC values.  It starts with the last three data points from the concentration
-time profile, performing log-linear regression to calculate the slope of
that tail portion of the concentration-time curve.  And then the last 4
data points, the last 5 data points, on and on until it reaches the data point
right after Cmax. Thus, this method will not include the data point of
(Tmax, Cmax).
}

\keyword{misc}