description_TTTARS<-function(){
cat("\n\n")
cat("*******************************************************************************\n")
cat("                Two Times Tmax (TTT) and Adjusted R squared (ARS)             \n")
cat("-------------------------------------------------------------------------------\n")
cat(" The TTT method is based on the Bateman function.  The Bateman function       \n")
cat(" has two outstanding points.  First, the maximum of the curve (Cmax at        \n")
cat(" Tmax) and secondly, the inflection point of the curve (Cinpt at Tinpt).      \n")
cat(" The inflection point of the curve describes the point where the algebraic    \n")
cat(" sign of the curvature changes. Tinpt is equal to two times Tmax.  Thus,      \n")
cat(" The twofold of the observed Tmax value can be used as a threshold of time    \n")
cat(" points to be included in the calculation of lambda(z).  Then, within that    \n")
cat(" range, we apply maximun ARS values to get best fit. It starts with the last  \n")
cat(" three data points from the concentration-time profile, performing log-linear \n")
cat(" regression to calculate the slope of that tail portion of the concentration- \n")
cat(" time curve.  And then the last 4 data points, the last 5 data points, on and \n")
cat(" on until it reaches the data point right after Cmax. Thus, this method       \n")
cat(" will not include the data point of (Tmax, Cmax).                           \n\n")
cat(" Please note: TTT is only for when one (or more) curve(s) have # of data      \n")
cat(" points > 2 after 2*Tmax; otherwise, it will cause error. bear will be        \n")
cat(" crashed in this case. If so, please try another method next time.          \n\n")
cat("*******************************************************************************\n")
cat("\n")
alarm(); alarm()
readline(" Press Enter to continue...")
}