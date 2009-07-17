\encoding{UTF-8}

\name{MultipleParaNCA.MIX}

\alias{MultipleParaNCA.MIX}

\title{NCA and lme for parallel study for multiple dose}

\description{
Noncompartmental analysis (NCA) approach is used to compute AUCs and
the terminal elimination rate constants (lambda z) for drug plasma
concentration.  The linear trapezoidal method is applied to calculate
AUC(time 0 to the last measurable Cp).  The extrapolated AUC (from time
of the last measurable Cp to time infinity) is equal to the last measurable
Cp divided by lambda z.

Statistical analysis (lme, 90CI...):
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