description_NCA<-function(){
cat("****************************************************************************\n")
cat("                      Noncompartmental analysis (NCA)                      \n")
cat("----------------------------------------------------------------------------\n")
cat(" Noncompartmental analysis (NCA) approach is used to compute AUCs and       \n")
cat(" the terminal elimination rate constants (lambda_z) for drug plasma         \n")
cat(" concentration.  The linear trapezoidal method is applied to calculate      \n")
cat(" AUC(time 0 to the last measurable Cp).  The extrapolated AUC (from time    \n")
cat(" of the last measurable Cp to time infinity) is equal to the last measurable\n")
cat(" Cp divided by lambda_z.                                                    \n")
cat("****************************************************************************\n")
cat("\n")
}