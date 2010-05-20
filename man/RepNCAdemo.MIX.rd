\encoding{UTF-8}

\name{RepNCAdemo.MIX}

\alias{RepNCAdemo.MIX}

\title{Select the exact 3 data points(NCA) and lme for demo function}

\description{
Noncompartmental analysis (NCA) approach is used to compute AUCs and
the terminal elimination rate constants (lambda z) for drug plasma
concentration.  The linear trapezoidal method is applied to calculate
AUC(time 0 to the last measurable Cp).  The extrapolated AUC (from time
of the last measurable Cp to time infinity) is equal to the last measurable
Cp divided by lambda z.

Statistical analysis (lme, 90CI...):
With a two-treatment, more than two period, two-sequence randomized
crossover design, linear mixed-effects model may be used.  The lme in nlme
package is used to obtain estimates for the adjusted differences between
treatment means and the standard error associated with these differences.
Log-transformed BA measures will also be analyzed.
}

\keyword{misc}