description_NCAinput<-function(replicated=FALSE){
cat("****************************************************************************\n")
cat("Input/Edit Data                                                             \n")
cat("----------------------------------------------------------------------------\n")
cat("   -> subject no.(subj)                                                     \n")
cat("   -> sequence (seq)                                                        \n")
  if (replicated){
cat("           ex. 4 periods                                                    \n")
cat("            1:Ref.-> Test -> Ref.-> Test                                     \n")
cat("            2:Test-> Ref. -> Test-> Ref.                                     \n")
cat("        or ex. 3 periods                                                    \n")
cat("            1:Ref. -> Test -> Test                                            \n")
cat("            2:Test -> Ref. -> Ref.                                            \n")
}
  else{
cat("          1:Ref. -> Test                                                     \n")
cat("          2:Test -> Ref.                                                     \n")
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
cat("   -> treatment (drug)                                                      \n")
cat("          1: Ref.                                                           \n")
cat("          2: Test                                                           \n")
cat("   -> sampling time                                                         \n")
cat("   -> drug plasma/serum/blood concentration (conc)                          \n")
}
  else{
cat("   -> sampling time                                                         \n")
cat("   -> drug plasma/serum/blood concentration (conc)                          \n") 
 }
cat("****************************************************************************\n")
cat("\n")
}