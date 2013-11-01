#input subject, time, test and ref concentration
BANOVAdata<-function(TotalData,
                     replicated=FALSE,
                     parallel=FALSE,
                     multiple=FALSE)
{
cat("\n")
file.menu <- c("Input/edit data from keyboard",
               "Import data file with .csv format",
               "Load data File with .RData format",
               "Back to Statistical analysis",
               "Quit")
cat("\n")
if(parallel){
 pick <- menu(file.menu, title = " << Data Analysis for Statistical analysis (lme, 90%CI...)>> ", graphics=TRUE)
}
 else{
 if(replicated ){
 pick <- menu(file.menu, title = " << Data Analysis for Statistical analysis (lme, 90%CI...)>> ", graphics=TRUE)
 } 
 else{ 
 pick <- menu(file.menu, title = " << Data Analysis for Statistical analysis (ANOVA(lm), 90%CI...)>> ", graphics=TRUE)
 }
} 
if (pick == 1){
 if(parallel){
cat("\n")
cat("*****************************************************************************\n")
cat("About the data file for parallel design...                                   \n")
cat("-----------------------------------------------------------------------------\n")
cat("   -> subject no.(subj)                                                      \n")
cat("   -> drug                                                                   \n")
cat("       1:Ref.                                                                \n")
cat("       2:Test                                                                \n")
 if(multiple){
     cat("   -> Cmax_ss: Cmax at steady-state                                          \n")
     cat("   -> AUC(tau)ss: the area under the predicted plasma concentration time   \n")
     cat("         curve during a dosing interval (tau) at steady-state                \n")
     cat("   -> lnCmax_ss: Log-transformed Cmax_ss                                   \n")
     cat("   -> lnAUC(tau)ss: Log-transformed AUC(tau)ss                         \n")
     cat("*****************************************************************************\n")
     cat("\n")
      TotalData<-data.frame (subj=c(0), drug=c(0),Cmax_ss=c(0), AUCtau_ss=c(0), 
                   lnCmax_ss=c(0),lnAUCtau_ss=c(0))
    }
   else{
     cat("   -> Cmax                                                                   \n")
     cat("   -> AUC0t: the area under the predicted plasma concentration time curve.   \n")
     cat("            (time = 0 to time of the last measurable Cp)                    \n")
     cat("   -> AUC0INF: area under the predicted plasma concentration time curve.     \n")
     cat("            (time = 0 to infinity)                                           \n")
     cat("   -> ln(Cmax): Log-transformed Cmax                                         \n")
     cat("   -> ln(AUC0t): Log-transformed AUC0t                                       \n")
     cat("   -> ln(AUC0INF): Log-transformed AUC0INF                                   \n")
     cat("*****************************************************************************\n")
cat("\n")
TotalData<-data.frame (subj=c(0), drug=c(0),Cmax=c(0), AUC0t=c(0), AUC0INF=c(0),
                   lnCmax=c(0),lnAUC0t=c(0),lnAUC0INF=c(0))
   }
 }
 else{ 
    cat("\n")
     cat("*****************************************************************************\n")
     cat("About the data file for non-parallel design...                               \n")
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
   if(multiple){
     cat("   -> Cmax_ss: Cmax at steady-state                                          \n")
     cat("   -> AUC(tau)ss: the area under the predicted plasma concentration time   \n")
     cat("         curve during a dosing interval (tau) at steady-state                \n")
     cat("   -> lnCmax_ss: Log-transformed Cmax_ss                                   \n")
     cat("   -> lnAUC(tau)ss: Log-transformed AUC(tau)ss                         \n")
     cat("*****************************************************************************\n")
     cat("\n")
      TotalData<-data.frame (subj=c(0), drug=c(0),seq=c(0), prd=c(0),Cmax_ss=c(0), AUCtau_ss=c(0), 
                   lnCmax_ss=c(0),lnAUCtau_ss=c(0))
    }
   else{
     cat("   -> Cmax                                                                   \n")
     cat("   -> AUC0t: the area under the predicted plasma concentration time curve for\n")
     cat("         test data. (time = 0 to time of the last mensurable Cp)            \n")
     cat("   -> AUC0INF: area under the predicted plasma concentration time curve for  \n")
     cat("         test data. (time = 0 to infinity)                                   \n")
     cat("   -> ln(Cmax): Log-transformed Cmax                                         \n")
     cat("   -> ln(AUC0t): Log-transformed AUC0t                                       \n")
     cat("   -> ln(AUC0INF): Log-transformed AUC0INF                                   \n")
     cat("*****************************************************************************\n")
     cat("\n")
      TotalData<-data.frame (subj=c(0), drug=c(0),seq=c(0), prd=c(0),Cmax=c(0), AUC0t=c(0), AUC0INF=c(0),
                   lnCmax=c(0),lnAUC0t=c(0),lnAUC0INF=c(0))
    }
} 
     TotalData<-edit(TotalData)
     TotalData<- na.omit(TotalData)
     show(TotalData)
     cat("\n")
     cat("Enter the file name to be saved (no extension!):\n")
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
                saveRDS(TotalData, Totalname)
                cat("\n")
                }
                else{
                cat("\nEnter file name to be saved (no extension!):\n")
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
              saveRDS(TotalData,Totalname)
           }
        else{
           saveRDS(TotalData,Totalname)
          }
       if(parallel){
         if(multiple){
          MultipleParaMIXanalyze(TotalData)
          MultipleParaMIXmenu() 
         }
         else{
          ParaMIXanalyze(TotalData)
          ParaMIXmenu() 
         }
       }
       else{ 
        if(replicated){
         RepMIXanalyze(TotalData)
         RepMIXmenu()
         }
        else{
           if(multiple){
           MultipleBANOVAanalyze(TotalData)
           MultipleBANOVAmenu() 
           }
           else{
           BANOVAanalyze(TotalData)
           BANOVAmenu() 
           }
        }
      }
    } 
else {
  if (pick == 2){
cat("\n\n")
if(parallel){
 if(multiple){
   cat("***********************************************************************************\n")
   cat(" row#1:subj, drug, Cmax_ss, AUC(tau)ss, lnCmax_ss, lnAUC(tau)ss                    \n")
   cat("-----------------------------------------------------------------------------------\n")
   cat(" column#1: subject no.(subj)                                                       \n")
   cat(" column#2: drug                                                                    \n")
   cat("            ->1: Ref.                                                              \n")
   cat("            ->2: Test                                                              \n")
   cat(" column#3: Cmax_ss                                                                 \n")
   cat(" column#4: AUC(tau)ss                                                              \n")
   cat(" column#5: lnCmax_ss                                                               \n")
   cat(" column#6: lnAUC(tau)ss                                                            \n")
   cat("***********************************************************************************\n")
MultipleParaMIXcsv()
 }
 else{
cat("***********************************************************************************\n")
cat(" row#1:subj, drug, Cmax, AUC0t, AUC0INF, ln(Cmax), ln(AUC0t), ln(AUC0INF)          \n")
cat("-----------------------------------------------------------------------------------\n")
cat(" column#1: subject no.(subj)                                                       \n")
cat(" column#2: drug                                                                    \n")
cat("            ->1: Ref.                                                              \n")
cat("            ->2: Test                                                              \n")
cat(" column#3: Cmax                                                                    \n")
cat(" column#4: AUC0t                                                                   \n")
cat(" column#5: AUC0INF                                                                 \n")
cat(" column#6: ln(Cmax)                                                                \n")
cat(" column#7: ln(AUC0t)                                                               \n")
cat(" column#8: ln(AUC0INF)                                                            \n")
cat("***********************************************************************************\n") 
ParaMIXcsv()
   }
 }
 else{  
   if(multiple){
   cat("***********************************************************************************\n")
   cat(" row#1:subj, drug, seq, prd, Cmax_ss, AUC(tau)ss, lnCmax_ss, lnAUC(tau)ss          \n")
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
   cat(" column#5: Cmax_ss                                                                 \n")
   cat(" column#6: AUC(tau)ss                                                              \n")
   cat(" column#7: lnCmax_ss                                                               \n")
   cat(" column#8: lnAUC(tau)ss                                                            \n")
   cat("***********************************************************************************\n")
   }
   else{
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
     }
        if(replicated){
         RepMIXcsv()
         }
        else{
           if(multiple){
           MultipleBANOVAcsv()
           }
           else{
           BANOVAcsv()
           }
        }
      }
 } 
else {
  if (pick == 3){
     description_load() 
     ## Totalname <-readline()
     ## Totalname<-paste(Totalname,".RData",sep="")
     TotalData<-readRDS(file.choose())
     TotalData<-edit(TotalData)
     TotalData<- na.omit(TotalData)
     if(parallel){
       if(multiple){
       colnames(TotalData)<-list("subj","drug","Cmax_ss","AUCtau_ss","lnCmax_ss","lnAUCtau_ss")
       show(TotalData)
       }
       else{
       colnames(TotalData)<-list("subj","drug","Cmax", "AUC0t", "AUC0INF","lnCmax","lnAUC0t","lnAUC0INF") 
        }
      }
     else{ 
       if(multiple){
        colnames(TotalData)<-list("subj","drug","seq", "prd","Cmax_ss","AUCtau_ss","lnCmax_ss","lnAUCtau_ss")
       }
       else{
       colnames(TotalData)<-list("subj","drug","seq", "prd","Cmax", "AUC0t", "AUC0INF","lnCmax","lnAUC0t","lnAUC0INF")
       }
     } 
     cat("\n\n")
     show(TotalData)
     ### saveRDS(TotalData,Totalname)    ### it will be loaded from outside, no need to save it back.  --YJ
     cat("\n\n")
     if(parallel){
      if(multiple){
          MultipleParaMIXanalyze(TotalData)
          MultipleParaMIXmenu() 
         }
         else{
          ParaMIXanalyze(TotalData)
          ParaMIXmenu() 
         }
      }
      else{  
         if(replicated){
         RepMIXanalyze(TotalData)
         RepMIXmenu() 
          }
         else{
         if(multiple){
           MultipleBANOVAanalyze(TotalData)
           MultipleBANOVAmenu() 
           }
           else{
           BANOVAanalyze(TotalData)
           BANOVAmenu() 
           }
        }
      }
     } 

  else {
  if (pick == 4){
     cat("\n\n")
     if(multiple){
     Multiplestat1menu()
     }
     else{
      Multiplestatmenu()
      }   
     }
  else {
  if (pick == 5){
      cat("\n")
      cat("\n  Thank you for using bear!  Bye now. \n\n")
      graphics.off()
        }
     }
    }
   }
  }
}