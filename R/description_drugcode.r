description_drugcode<-function(replicated=FALSE, multiple=FALSE){
cat("\n")
cat("****************************************************************************\n")
cat("*<<Conc. - Time Plots >>                                                    \n")
cat(" Plasma drug concentration (Ref. and Test) vs. Time                         \n")
cat(" Log transformed drug plasma concentration (Ref. and Test) vs. Time        \n")
cat("----------------------------------------------------------------------------\n")
cat("****************************************************************************\n")
cat("\n")

cat("****************************************************************************\n")
cat("<<Summary Result >>                                                         \n")
if(multiple){
cat(" AUC(tau)ss, Cmax_ss for the Reference and Test product                   \n")
cat("----------------------------------------------------------------------------\n")
cat(" Test_AUC(tau)ss: the area under the predicted plasma concentration time   \n")
cat("                   curve during a dosing interval (tau) at steady-state for \n")
cat("                   for Test  product. (time = 0 to time of the last         \n")
cat("                   measurable Cp)                                          \n")
cat(" Ref_AUC(tau)ss: the area under the predicted plasma concentration time    \n")
cat("                  curve during a dosing interval (tau) at steady-state for \n")
cat("                  Reference product. (time = 0 to time of the last          \n")
cat("                  measurable Cp)                                           \n")
}
else{
cat(" AUC(0-t), AUC(0-inf), Cmax for the Reference and Test product              \n")
cat("----------------------------------------------------------------------------\n")
cat(" Test_AUC0t: area under the plasma concentration time curve for Test         \n")
cat("            product. (time = 0 to time of the last measurable Cp)          \n")
cat(" Test_AUC0inf: area under the plasma concentration time curve for Test       \n")
cat("              product. (time = 0 to infinity)                               \n")
cat(" Ref_AUC0t: area under the plasma concentration time curve for Reference     \n")
cat("           product. (time = 0 to time of the last measurable Cp)           \n")
cat(" Ref_AUC0inf: area under the predicted plasma concentration time curve       \n")
cat("             for Reference product. (time = 0 to infinity)                  \n")
}
cat("****************************************************************************\n")
cat("\n")

cat("****************************************************************************\n")
cat(" Data Codes:                                                                \n")
cat("----------------------------------------------------------------------------\n")
cat(" Drug:                                                                      \n")
cat("     1: Ref.                                                                \n")
cat("     2: Test                                                                \n")
 if (replicated || multiple){
 }
   else{
cat(" Sequence:                                                                  \n")
cat("     1: Ref. --> Test                                                       \n")
cat("     2: Test --> Ref.                                                       \n")
cat(" Period:                                                                    \n")
cat("     1: 1st-treatment period                                                \n")
cat("     2: 2nd-treatment period                                                \n") 
 }
cat("****************************************************************************\n")
cat("\n\n")
}