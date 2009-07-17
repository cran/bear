description_size<-function(parallel=FALSE){
cat("****************************************************************************\n")
cat("                            Required data                                   \n")
cat("----------------------------------------------------------------------------\n")
cat(" 1. Theta (%) is the target ratio in average BA between the two formulations\n")
cat("    expressed in percentage of the average reference BA.                    \n")
cat(" 2. Power (%) is the least statistical power to detect (1-Power)          \n")
cat("    differences between the Test and the Reference formulation.             \n")
if(parallel){
cat(" 3. CV (%) can be the inter-subject coefficient of variation for parallel  \n")
cat("    study.                                                                  \n")
}
else{ 
cat(" 3. CV (%) can be the intra-subject coefficient of variation for crossover  \n")
cat("    (or replicate crossover) study.                                         \n")
}
cat(" 4. Lower acceptance limit represents the lower BE criteria to be accepted; \n")
cat("    the upper acceptance limit is equal to 1/(lower acceptance limit).     \n")
cat("    Usually the lower limit can be 80% or rarely 70% in some cases.         \n")
cat("****************************************************************************\n")
cat("\n")
}