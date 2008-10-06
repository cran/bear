\name{demoARS.BANOVA}

\alias{demoARS.BANOVA}

\title{Adjusted R squared (ARS) method and ANOVA for demo file}

\description{
Adjusted R squared (ARS) method:
This method selects data points to estimate lambda(z) based on the  maximun
adjustedR squred values.  It starts with the last three data points from the
concentration-time course, performing log-linear regression to calculate
the slope of that tail portion of the concentration-time curve.  And then
the last 4data points, the last 5 data points, on and on until it includes
the data points of Cmax. Thus, this method may include the data point of
(Tmax, Cmax). WNL v5.x.x has the similar algoirthms like this.  WNL v6. has
similar algorithms except that it will rule out the data point of (Tmax, Cmax).

Statistical analysis (ANOVA(lm), 90CI...):
With a two-treatment, two-period, two-sequence randomized crossover
design, ANOVA statistical model includes factors of the following sources:
sequence, subjects nested in sequences, period and treatment. ANOVA will
be applied to obtain estimates for the adjusted differences between
treatment means and the standard error associated with these differences.
Log-transformed BA measures will also be analyzed.
}

\keyword{misc}