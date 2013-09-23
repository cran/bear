description_NCAcsv<-function(replicated=FALSE, parallel=FALSE){
cat("****************************************************************************\n")
cat(" Input data file should consist of the following:                           \n")
cat("---------------------------------------------------------------------------\n")
if(parallel){
cat(" row#1: column title, such as subj, drug (trt), time & Conc.                \n")
}
else{
 if(replicated){
cat(" row#1: column title, such as subj, seq, prd, drug (trt), time & Conc.     \n")
 }
 else{
cat("    row#1: column title, such as subj, seq, prd, time & Conc.             \n")
 }
} 
cat(" column#1: subject no.(subj)                                               \n")
if(parallel){
cat(" column#2: treatment (trt or drug)                                         \n")
cat(" column#3: sampling time                                                   \n")
cat(" column#4: drug plasma/serum/blood concentration (conc)                    \n")
}
else{
 if (replicated){
cat(" column#2: sequence (seq)                                                  \n")
cat("             -> i.e. Seq = 1 if Ref. -> Test ->  Ref. -> Test              \n")
cat("             ->      Seq = 2 if Test -> Ref. ->  Test -> Ref.              \n")
cat(" column#3: period (prd)                                                    \n")
cat("             -> i.e. prd = 1: the 1st-treatment period                     \n")
cat("             ->      prd = 2: the 2nd-treatment period                     \n")
cat("             ->      prd = 3: the 3rd-treatment period                     \n")
cat("             ->      prd = 4: the 4th-treatment period                     \n")
cat(" column#4: treatment (trt or drug)                                         \n")
cat(" column#5: sampling time                                                   \n")
cat(" column#6: drug plasma/serum/blood concentration (conc)                    \n")
  }
 else{
cat(" column#2: sequence (seq)                                                  \n")
cat("             -> Sequence = 1 if Ref. -> Test                               \n")
cat("             -> Sequence = 2 if Test -> Ref.                               \n")
cat(" column#3: period (prd)                                                    \n")
cat("             -> Period = 1: the 1st-treatment period                       \n")
cat("             -> Period = 2: the 2nd-treatment period                       \n")
cat(" column#4: sampling time                                                   \n")
cat(" column#5: drug plasma/serum/blood concentration (conc)                    \n")
 }
} 
cat("****************************************************************************\n\n")
readline("  Press Enter to proceed...")
}