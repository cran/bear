\encoding{UTF-8}

\name{NCA.BANOVA}

\alias{NCA.BANOVA}

\title{NCA and ANOVA}

\description{
Noncompartmental analysis (NCA) approach is used to compute AUCs and
the terminal elimination rate constants (lambda z) for drug plasma
concentration.  The linear trapezoidal method is applied to calculate
AUC(time 0 to the last measurable Cp).  The extrapolated AUC (from time
of the last measurable Cp to time infinity) is equal to the last measurable
Cp divided by lambda z.

Statistical analysis (ANOVA(lm), 90CI...):
With a two-treatment, two-period, two-sequence randomized crossover
design, ANOVA statistical model includes factors of the following sources:
sequence, subjects nested in sequences, period and treatment. ANOVA will
be applied to obtain estimates for the adjusted differences between
treatment means and the standard error associated with these differences.
Log-transformed BA measures will also be analyzed.
}

\keyword{misc}