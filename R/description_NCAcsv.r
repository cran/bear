description_NCAcsv<-function(replicated=FALSE){
cat("****************************************************************************\n")
cat(" Data file should consist of                                               \n")
cat("---------------------------------------------------------------------------\n")
cat("    row#1: column title, such as subj, seq, prd, time, conc & etc.         \n")
cat(" column#1: subject no.(subj)                                               \n")
cat(" column#2: sequence (seq)                                                  \n")
cat("             -> Sequence = 1 if Ref. -> Test                                \n")
cat("             -> Sequence = 2 if Test -> Ref.                                \n")
cat(" column#3: period (prd)                                                    \n")
cat("             -> Period = 1: the 1st-treatment period                       \n")
cat("             -> Period = 2: the 2nd-treatment period                       \n")
 if (replicated){
cat(" column#4: treatment (drug)                                                \n")
cat(" column#5: sampling time                                                   \n")
cat(" column#6: drug plasma/serum/blood concentration (conc)                    \n")
}
else{
cat(" column#4: sampling time                                                   \n")
cat(" column#5: drug plasma/serum/blood concentration (conc)                    \n")
}
cat("****************************************************************************\n")
}