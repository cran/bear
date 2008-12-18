#input subject, time, test and ref concentration
BANOVAdata<-function(TotalData)
{
cat("\n")
file.menu <- c("Input/edit data from keyboard",
               "Import data file with .csv format",
               "Load data File with .RData format",
               "Back to Statistical analysis (ANOVA(lm), 90%CI...)",
               "Quit")
cat("\n")

pick <- menu(file.menu, title = " << Data Analysis for Statistical analysis (ANOVA(lm), 90%CI...)>> ")
if (pick == 1){
cat("\n")
cat("*****************************************************************************\n")
cat("About the data file...                                                       \n")
cat("-----------------------------------------------------------------------------\n")
cat("   -> subject no.(subj)                                                      \n")
cat("   -> drug                                                                   \n")
cat("       1:Ref.                                                                \n")
cat("       2:Test                                                                \n")
cat("   -> sequence (seq)                                                         \n")
cat("       Sequence = 1:Ref.-->Test                                              \n")
cat("       Sequence = 2:Test-->Ref.                                              \n")
cat("   -> period (prd)                                                           \n")
cat("       Period 1: the 1st-treatment period                                    \n")
cat("       Period 2: the 2nd-treatment period                                    \n")
cat("   -> Cmax                                                                   \n")
cat("   -> AUC0t: the area under the predicted plasma concentration time curve for\n")
cat("         test data. (time = 0 to time of the last measureable Cp)            \n")
cat("   -> AUC0INF: area under the predicted plasma concentration time curve for  \n")
cat("         test data. (time = 0 to infinity)                                   \n")
cat("   -> ln(Cmax): Log-transformed Cmax                                         \n")
cat("   -> ln(AUC0t): Log-transformed AUC0t                                       \n")
cat("   -> ln(AUC0INF): Log-transformed AUC0INF                                   \n")
cat("*****************************************************************************\n")
cat("\n")
     TotalData<-data.frame (subj=c(0), drug=c(0),seq=c(0), prd=c(0),Cmax=c(0), AUC0t=c(0), AUC0INF=c(0),
                   lnCmax=c(0),lnAUC0t=c(0),lnAUC0INF=c(0))
     TotalData<-edit(TotalData)
     TotalData<- na.omit(TotalData)
     show(TotalData)
     cat("\n")
     cat("Enter the file name:\n")
        Totalname <-readline()
        Totalname<-paste(Totalname,".RData",sep="")
           if(file.exists(Totalname)){
           cat("\n")
           cat("******************************************\n")
           cat(" The file have been existed.              \n")
           cat(" Would you like to overwrite it? (y/n)    \n")
           cat("******************************************\n")
           ans<-readline()
             if (ans == "y" | ans == "Y")
                {
                save(TotalData, file=Totalname)
                cat("\n")
                }
                else{
                cat("\nEnter file name:\n")
                Totalname <-readline()
                Totalname<-paste(Totalname,".RData",sep="")
                repeat{
                    if(file.exists(Totalname))
                      {
                      cat("\n")
                      cat("***********************************\n")
                      cat(" The file have been existed.       \n")
                      cat(" Please try another file name.     \n")
                      cat("***********************************\n")
                      Totalname<-readline()
                      Totalname<-paste(Totalname,".RData",sep="")
                      }
                       else{
                       break
                           }
                    }
             }
              save(TotalData,file=Totalname)
           }
        else{
           save(TotalData,file=Totalname)
          }

        BANOVAanalyze(TotalData)
        BANOVAmenu() 
      }

else {
  if (pick == 2){
cat("\n\n")
cat("***********************************************************************************\n")
cat(" row#1:subj, drug, seq, prd, Cmax, AUC0t, AUC0INF, ln(Cmax), ln(AUC0t), ln(AUC0INF)\n")
cat("-----------------------------------------------------------------------------------\n")
cat(" column#1: subject no.(subj)                                                       \n")
cat(" column#2: drug                                                                    \n")
cat("            ->1: Ref.                                                              \n")
cat("            ->2: Test                                                              \n")
cat(" column#3: sequence (seq)                                                          \n")
cat("            ->Sequence 1: Ref.-->Test                                              \n")
cat("            ->Sequence 2: Test-->Ref.                                              \n")
cat(" column#4: period (prd)                                                            \n")
cat("            ->Period 1: 1st-treatment period                                       \n")
cat("            ->Period 2: 2nd-treatment period                                       \n")
cat(" column#5: Cmax                                                                    \n")
cat(" column#6: AUC0t                                                                   \n")
cat(" column#7: AUC0INF                                                                 \n")
cat(" column#8: ln(Cmax)                                                                \n")
cat(" column#9: ln(AUC0t)                                                               \n")
cat(" column#10: ln(AUC0INF)                                                            \n")
cat("***********************************************************************************\n")
        BANOVAcsv()
        BANOVAmenu() 
      }

else {
  if (pick == 3){
     description_load() 
     Totalname <-readline()
     Totalname<-paste(Totalname,".RData",sep="")
     load(Totalname)
     TotalData<-edit(TotalData)
     TotalData<- na.omit(TotalData)
     colnames(TotalData)<-list("subj","drug","seq", "prd","Cmax", "AUC0t", "AUC0INF","lnCmax","lnAUC0t","lnAUC0INF")
     cat("\n\n")
     show(TotalData)
     save(TotalData,file=Totalname)
     cat("\n\n")
        BANOVAanalyze(TotalData)
        BANOVAmenu() 
      }

  else {
  if (pick == 4){
     cat("\n\n")
     BANOVAmenu()
                }
  else {
  if (pick == 5){
      cat("\n")
      cat("\nThank you for using bear!  Bye now. \n\n")
                }
     }
    }
   }
  }
}