description_NCAinput<-function(replicated=FALSE, parallel=FALSE){
cat("\n\n")
cat("****************************************************************************\n")
cat("  Input Data Foramt                                                         \n")
cat("----------------------------------------------------------------------------\n")
cat("   -> subject no.(subj)                                                     \n")
if(parallel){
cat("   -> treatment (trt or drug)                                               \n")
cat("          1: Ref.                                                           \n")
cat("          2: Test                                                           \n")
cat("   -> sampling time                                                         \n")
cat("   -> drug plasma/serum/blood concentration (conc)                          \n")
}
else{
cat("   -> sequence (seq)                                                        \n") 
  if (replicated){
cat("       if 4 periods                                                         \n")
cat("          1:Ref.-> Test -> Ref.-> Test                                      \n")
cat("          2:Test-> Ref. -> Test-> Ref. or                                 \n\n")
cat("       if 3 periods                                                         \n")
cat("          1:Ref. -> Test -> Rest                                            \n")
cat("          2:Test -> Ref. -> Tef.                                            \n")
}
  else{
cat("          1:Ref. -> Test                                                    \n")
cat("          2:Test -> Ref.                                                    \n")
}
cat("   -> period (prd)                                                          \n")
  if (replicated){
cat("          1: 1st-treatment period                                           \n")
cat("          2: 2nd-treatment period                                           \n")
cat("          3: 3rd-treatment period                                           \n")
cat("          4: 4th-treatment period                                           \n") 
}
  else{
cat("          1: 1st-treatment period                                           \n")
cat("          2: 2nd-treatment period                                           \n")
 }
  if (replicated){
cat("   -> treatment (trt or drug)                                               \n")
cat("          1: Ref.                                                           \n")
cat("          2: Test                                                           \n")
cat("   -> sampling time                                                         \n")
cat("   -> drug plasma/serum/blood concentration (Conc.)                         \n")
}
  else{
cat("   -> sampling time                                                         \n")
cat("   -> drug plasma/serum/blood concentration (Conc.)                         \n") 
 }
}
cat("****************************************************************************\n")
cat("\n")
}