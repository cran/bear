description_NCA<-function(){
cat("\n\n")
cat("**************************************************************************** \n")
cat("                      Noncompartmental analysis (NCA)                        \n")
cat("---------------------------------------------------------------------------- \n")
cat(" Noncompartmental analysis (NCA) approach is used to compute AUCs and        \n")
cat(" the terminal elimination rate constants (lambda_z) for drug plasma          \n")
cat(" concentrations. All linear trapezoidal method or lin-up/log-down trapezoidal\n")
cat(" method will be applied to calculate AUC(time 0 to the last measurable Cp).  \n")
cat(" The extrapolated AUC (from time of the last measurable Cp to time infinity) \n")
cat(" is equal to the last measurable Cp divided by lambda_z.                     \n")
cat("............................................................................ \n")
cat("\n")
alarm(); alarm()
readline(" Press Enter to proceed...")
}