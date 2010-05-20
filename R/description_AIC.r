description_AIC<-function(){
cat("*******************************************************************************\n")
cat("                      Akaike information criterion (AIC)                       \n")
cat("-------------------------------------------------------------------------------\n")
cat(" This method selects data points to estimate lambda_z based on the minimum    \n")
cat(" AIC values.  It starts with the last three data points from drug Conc.       \n")
cat(" -time profile, performing log-linear regression to calculate the slope of      \n")
cat(" that tail portion of the concentration-time curve.  And then the last 4       \n")
cat(" data points, the last 5 data points, on and on until it reaches the data point \n")
cat(" right after Cmax. Thus, this method exclude the data point of (Tmax, Cmax).    \n")
cat("*******************************************************************************\n")
cat("\n")
}