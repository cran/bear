\name{TTTAIC}

\alias{TTTAIC}

\title{TTT and AIC}

\description{
The TTT method is based on the Bateman function.  The Bateman function
has two outstanding points.  First, the maximum of the curve (Cmax at
Tmax) and secondly, the inflection point of the curve (Cinpt at Tinpt).
The inflection point of the curve describes the point where the algebraic
sign of the curvature changes. Tinpt is equal to two times Tmax.  Thus,
The twofold of the observed Tmax value can be used as a threshold of time
points to be included in the calculation of lambda(z).  Then, within that
range, we apply minimun AIC values to get best fit. It starts with the last
three data points from the concentration-time profile, performing log-linear
regression to calculate the slope of that tail portion of the concentration-
time curve.  And then the last 4 data points, the last 5 data points, on and
on until it reaches the data point right after Cmax. Thus, this method
will not include the data point of (Tmax, Cmax).

}

\references{
Scheerans C, Derendorf H and C Kloft. Proposal for a Standardised
Identification of the Mono-Exponential Terminal Phase for Orally
Administered Drugs. Biopharm Drug Dispos 29, 145-157 (2008).
}
\keyword{misc}