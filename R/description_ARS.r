description_ARS<-function(){
cat("\n\n")
cat("*******************************************************************************\n")
cat("                      Adjusted R squared (ARS) method                          \n")
cat("-------------------------------------------------------------------------------\n")
cat(" This method selects data points to estimate lambda_z based on the maximum     \n")
cat(" adjusted R squared values.  It starts with the last three data points from    \n")
cat(" the concentration-time course, performing log-linear regression to calculate  \n")
cat(" the slope of that tail portion of the concentration-time curve.  And then     \n")
cat(" the last 4 data points, the last 5 data points, on and on until it excludes   \n")
cat(" the data points of Cmax. Thus, this method will exclude the data point of     \n")
cat(" (Tmax, Cmax).                                                               \n\n")
cat(" Please note: ARS is only for when one (or more) curve(s) have # of data       \n")
cat(" points > 2 after Tmax; otherwise, it will cause error. bear will be crashed   \n")
cat(" in this case. If so, please try another method next time.                   \n\n")
cat("*******************************************************************************\n")
cat("\n")
alarm(); alarm()
readline(" Press Enter to proceed...")
}