description_AIC<-function(){
cat("\n\n")
cat("******************************************************************************* \n")
cat("                      Akaike information criterion (AIC)                        \n")
cat("------------------------------------------------------------------------------- \n")
cat(" This method selects data points to estimate lambda_z based on the minimum      \n")
cat(" AIC values.  It starts with the last three data points from drug conc.-        \n")
cat(" time profile, performing log-linear regression to calculate the slope of       \n")
cat(" that tail portion of the concentration-time curve.  And then the last 4        \n")
cat(" data points, the last 5 data points, on and on until it reaches the data point \n")
cat(" right after Cmax. Thus, this method excludes the data point of (Tmax, Cmax). \n\n")
cat(" Please note: AIC is only for when one (or more) curve(s) have data points      \n")
cat(" > 2 after Tmax; otherwise, it will cause error. bear may be crashed            \n")
cat(" in this case. If so, please try other method next time.                      \n\n")
cat("******************************************************************************* \n")
cat("\n")
alarm(); alarm()
readline(" Press Enter to continue...")
}