#input subject, time, test and ref concentration
GLMdata<-function(TotalData)
{
cat("\n")
file.menu <- c("Input/Edit Data from keyboard",
               "Import Data Files (.CSV)",
               "Load Data Files (.RData)",
               "Back to ANOVA (lm) menu",
               "Quit")
cat("\n")

pick <- menu(file.menu, title = " << Data Analysis for ANOVA (lm) menu >> ")
if (pick == 1){
cat("\n")
cat("****************************************************************************\n")
cat("*Input/Edit Data                                                           *\n")
cat("*--------------------------------------------------------------------------*\n")
cat("*   ->subject no.(subj)                                                    *\n")
cat("*   ->drug                                                                 *\n")
cat("*       1:Reference                                                        *\n")
cat("*       2:Test                                                             *\n")
cat("*   ->sequence (seq)                                                       *\n")
cat("*       Sequence 1:Reference-->Test sequence                               *\n")
cat("*       Sequence 2:Test-->Reference sequence                               *\n")
cat("*   ->period (prd)                                                         *\n")
cat("*       Period 1: first treatment period                                   *\n")
cat("*       Period 2: second treatment period                                  *\n")
cat("*   ->Cmax                                                                 *\n")
cat("*   ->AUC0t: area under the predicted plasma concentration time curve for  *\n")
cat("*         test data. (time = 0 to t)                                       *\n")
cat("*   ->AUC0INF: area under the predicted plasma concentration time curve for*\n")
cat("*         test data. (time = 0 to infinity)                                *\n")
cat("*   ->LnCmax: Log-transformed Cmax                                         *\n")
cat("*   ->LnAUC0t: Log-transformed AUC0t                                       *\n")
cat("*   ->LnAUC0INF: Log-transformed AUC0INF                                   *\n")
cat("****************************************************************************\n")
cat("\n")
     TotalData<-data.frame (subj=c(0), drug=c(0),seq=c(0), prd=c(0),Cmax=c(0), AUC0t=c(0), AUC0INF=c(0),
                   LnCmax=c(0),LnAUC0t=c(0),LnAUC0INF=c(0))
     TotalData<-edit(TotalData)
     TotalData<- na.omit(TotalData)
     show(TotalData)
     cat("\nSave data (y/n) ?\n")
     ans<-readline()
     cat("\n")
     if (ans == "n" | ans == "N")
        {
        return (GLMmenu())
        }
     else {
        cat("Enter name you want to call this data\n")
        Totalname <-readline()
        Totalname<-paste(Totalname,".RData",sep="")
           if(file.exists(Totalname)){
           cat("\n")
           cat("******************************************\n")
           cat("* The file name have been existed.       *\n")
           cat("* Would you want to overwrite it ? (y/n) *\n")
           cat("******************************************\n")
           ans<-readline()
             if (ans == "y" | ans == "Y")
                {
                save(TotalData, file=Totalname)
                cat("\n")
                }
                else{
                cat("\nEnter name you want to call this data\n")
                Totalname <-readline()
                Totalname<-paste(Totalname,".RData",sep="")
                repeat{
                    if(file.exists(Totalname))
                      {
                      cat("\n")
                      cat("***********************************\n")
                      cat("* The file name have been existed *\n")
                      cat("* Enter name again, OK.           *\n")
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

        return(GLManalyze(TotalData))
      }
    }

else {
  if (pick == 2){
cat("\n\n")
cat("****************************************************************************\n")
cat("* row#1:subj,drug,seq,prd,Cmax,AUC0t,AUC0INF,LnCmax,LnAUC0t,LnAUC0INF      *\n")
cat("*--------------------------------------------------------------------------*\n")
cat("* column#1: subject no.(subj)                                              *\n")
cat("* column#2: drug                                                           *\n")
cat("*          ->1: Reference                                                  *\n")
cat("*          ->2: Test                                                       *\n")
cat("* column#3: sequence (seq)                                                 *\n")
cat("*          ->Sequence 1: Reference-->Test                                  *\n")
cat("*          ->Sequence 2: Test-->Reference                                  *\n")
cat("* column#4: period (prd)                                                   *\n")
cat("*          ->Period 1: 1st treatment period                                *\n")
cat("*          ->Period 2: 2nd treatment period                                *\n")
cat("* column#5: Cmax                                                           *\n")
cat("* column#6: AUC0t                                                          *\n")
cat("* column#7: AUC0INF                                                        *\n")
cat("* column#8: LnCmax (log-transformed Cmax )                                 *\n")
cat("* column#9: LnAUC0t (log-transformed AUC0t )                               *\n")
cat("* column#10: LnAUC0INF (log-transformed AUC0INF )                          *\n")
cat("****************************************************************************\n")
cat("\n")
filepath<-getwd()
cat("R will import your data from the directory of \n")
cat("",filepath,"\n")
        return(GLMcsv())
      }

else {
  if (pick == 3){
    cat("\n\n")
    filepath<-getwd()
    cat("R will load your data from the directory of \n")
    cat("",filepath,"\n")
    cat("\n")
    cat("\nEnter Data file name(without file extention of .RData)\n")
     Totalname <-readline()
     Totalname<-paste(Totalname,".RData",sep="")
     load(Totalname)
     TotalData<-edit(TotalData)
     TotalData<- na.omit(TotalData)
     colnames(TotalData)<-list("subj","drug","seq", "prd","Cmax", "AUC0t", "AUC0INF","LnCmax","LnAUC0t","LnAUC0INF")
     cat("\n\n")
     show(TotalData)
     save(TotalData,file=Totalname)
     cat("\n\n")

        return(GLManalyze(TotalData))
      }

  else {
  if (pick == 4){
     cat("\n\n")
     GLMmenu()
                }
  else {
  if (pick == 5){
      cat("\nBye~~ \n\n")
                }
     }
    }
   }
  }
}