\encoding{UTF-8}

\name{RepTTT.MIX}

\alias{RepTTT.MIX}

\title{TTT and lme for replicated study}

\description{
The TTT method is based on the Bateman function.  The Bateman function
has two outstanding points.  First, the maximum of the curve (Cmax at
Tmax) and secondly, the inflection point of the curve (Cinpt at Tinpt).
The inflection point of the curve describes the point where the algebraic
sign of the curvature changes. Tinpt is equal to two times Tmax.  Thus,
The twofold of the observed Tmax value can be used as a threshold of time
points to be included in the calculation of lambda(z).

With a two-treatment, more than two period, two-sequence randomized
crossover design, linear mixed-effects model may be used.  The lme in nlme
package is used to obtain estimates for the adjusted differences between
treatment means and the standard error associated with these differences.
Log-transformed BA measures will also be analyzed.
}

\references{
Scheerans C, Derendorf H and C Kloft. Proposal for a Standardised
Identification of the Mono-Exponential Terminal Phase for Orally
Administered Drugs. Biopharm Drug Dispos 29, 145-157 (2008).
}
\keyword{misc}