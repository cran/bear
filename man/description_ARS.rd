\name{description_ARS}

\alias{description_ARS}

\title{Description for Adjusted R squared (ARS) method}

\description{
This method selects data points to estimate lambda(z) based on the  maximun   
adjustedR squred values.  It starts with the last three data points from the  
concentration-time course, performing log-linear regression to calculate      
the slope of that tail portion of the concentration-time curve.  And then     
the last 4 data points, the last 5 data points, on and on until it excludes   
the data points of Cmax. Thus, this method may exclude the data point of      
(Tmax, Cmax). WNL v6. has the similar algoirthms like this.
}

\keyword{misc}