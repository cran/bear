description_ARS<-function(){
cat("*******************************************************************************\n")
cat("                      Adjusted R squared (ARS) method                          \n")
cat("-------------------------------------------------------------------------------\n")
cat(" This method selects data points to estimate lambda(z) based on the  maximun   \n")
cat(" adjustedR squred values.  It starts with the last three data points from the  \n")
cat(" concentration-time course, performing log-linear regression to calculate      \n")
cat(" the slope of that tail portion of the concentration-time curve.  And then     \n")
cat(" the last 4 data points, the last 5 data points, on and on until it excludes   \n")
cat(" the data points of Cmax. Thus, this method may exclude the data point of      \n")
cat(" (Tmax, Cmax). WNL v6. has the similar algoirthms like this.                   \n")
cat("*******************************************************************************\n")
cat("\n")
}