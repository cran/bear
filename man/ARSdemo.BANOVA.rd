\name{ARSdemo.BANOVA}

\alias{ARSdemo.BANOVA}

\title{Adjusted R squared (ARS) method and ANOVA for demo function}

\description{
Adjusted R squared (ARS) method:
This method selects data points to estimate lambda(z) based on the  maximun   
adjustedR squred values.  It starts with the last three data points from the  
concentration-time course, performing log-linear regression to calculate      
the slope of that tail portion of the concentration-time curve.  And then     
the last 4 data points, the last 5 data points, on and on until it excludes   
the data points of Cmax. Thus, this method may exclude the data point of      
(Tmax, Cmax). WNL v6. has the similar algoirthms like this.

Statistical analysis (ANOVA(lm), 90CI...):
With a two-treatment, two-period, two-sequence randomized crossover
design, ANOVA statistical model includes factors of the following sources:
sequence, subjects nested in sequences, period and treatment. ANOVA will
be applied to obtain estimates for the adjusted differences between
treatment means and the standard error associated with these differences.
Log-transformed BA measures will also be analyzed.
}

\keyword{misc}