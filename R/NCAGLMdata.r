#NCA-->GLM
NCAGLMdata<-function()
{
cat("\n")
file.menu <- c("Input/Edit Data from keyboard",
               "Import Data Files (.CSV)",
               "Load Data Files (.RData)",
               "Back to NCAGLMmenu",
               "Quit")
cat("\n")

pick <- menu(file.menu, title = " << Data Analysis for Single Dose menu >> ")
if (pick == 1){
cat("\n")
cat("****************************************************************************\n")
cat("*Input/Edit Basic Data                                                     *\n")
cat("*--------------------------------------------------------------------------*\n")
cat("*   ->subject no.(subj)                                                   *\n")
cat("*   ->sequence (seq)                                                       *\n")
cat("*       Sequence 1:Reference-->Test sequence                               *\n")
cat("*       Sequence 2:Test-->Reference sequence                               *\n")
cat("*   ->period (prd)                                                         *\n")
cat("*       Period 1: first treatmetn period                                   *\n")
cat("*       Period 2: second treatmetn period                                  *\n")
cat("*   ->time                                                                 *\n")
cat("*   ->concentration (conc)                                                 *\n")
cat("****************************************************************************\n")
cat("\n")
     TotalSingledata<-data.frame (subj=c(0), seq=c(0),prd=c(0),time=c(0), conc=c(0) )
     TotalSingledata<-edit(TotalSingledata)
     TotalSingledata<- na.omit(TotalSingledata)
     show(TotalSingledata)
     cat("\nSave data (y/n) ?\n")
     ans<-readline()
     cat("\n")
     if (ans == "n" | ans == "N")
        {
        return (NCAGLMmenu())
        }
     else {
        cat("Enter name you want to call this data\n")
        TotalSinglename <-readline()
        TotalSinglename<-paste(TotalSinglename,".RData",sep="")
           if(file.exists(TotalSinglename)){
           cat("\n")
           cat("******************************************\n")
           cat("* The file name have been existed.       *\n")
           cat("* Would you want to overwrite it ? (y/n) *\n")
           cat("******************************************\n")
           ans<-readline()
             if (ans == "y" | ans == "Y")
                {
                save(TotalSingledata, file=TotalSinglename)
                cat("\n")
                }
                else{
                cat("\nEnter name you want to call this data\n")
                TotalSinglename <-readline()
                TotalSinglename<-paste(TotalSinglename,".RData",sep="")
                repeat{
                    if(file.exists(TotalSinglename))
                      {
                      cat("\n")
                      cat("***********************************\n")
                      cat("* The file name have been existed *\n")
                      cat("* Enter name again, OK.           *\n")
                      cat("***********************************\n")
                      TotalSinglename<-readline()
                      TotalSinglename<-paste(TotalSinglename,".RData",sep="")
                      }
                       else{
                       break
                           }
                    }
             }
              save(TotalSingledata,file=TotalSinglename)
           }
        else{
           save(TotalSingledata,file=TotalSinglename)
          }

        return(NCAGLManalyze(TotalSingledata))
      }
    }

else {
  if (pick == 2){
  cat("\n\n")
cat("\nEnter subject-period-time-concentration data file name\n")
cat("****************************************************************************\n")
cat("* Data should consist of                                                   *\n")
cat("*--------------------------------------------------------------------------*\n")
cat("*   ->subject no.(subj)                                                   *\n")
cat("*   ->sequence (seq)                                                       *\n")
cat("*       Sequence 1:Reference-->Test sequence                               *\n")
cat("*       Sequence 2:Test-->Reference sequence                               *\n")
cat("*   ->period (prd)                                                         *\n")
cat("*       Period 1: first treatmetn period                                   *\n")
cat("*       Period 2: second treatmetn period                                  *\n")
cat("*   ->time                                                                 *\n")
cat("*   ->concentration (conc)                                                 *\n")
cat("****************************************************************************\n")
  TotalSingledata.file <-readline()
  TotalSingledata.file<-paste(TotalSingledata.file,".csv",sep="")
     cnames<-c("subj", "seq","prd", "time", "conc")
     TotalSingledata<-read.csv(TotalSingledata.file,header=TRUE,sep=",",row.names=NULL,col.names=cnames)
     TotalSingledata<-edit(TotalSingledata)
     TotalSingledata<-na.omit(TotalSingledata)
     cat("\n\n")
     show(TotalSingledata)

        return(NCAGLManalyze(TotalSingledata))
      }

else {
  if (pick == 3){
    cat("\n\n")
  cat("\nEnter subject-period-time-concentration data file name\n")
     TotalSinglename <-readline()
     TotalSinglename<-paste(TotalSinglename,".RData",sep="")
     load(TotalSinglename)
     TotalSingledata<-edit(TotalSingledata)
     TotalSingledata<- na.omit(TotalSingledata)
     colnames(TotalSingledata)<-list("subj", "seq", "prd", "time", "conc")
     cat("\n\n")
     show(TotalSingledata)
     save(TotalSingledata,file=TotalSinglename)
     cat("\n\n")

        return(NCAGLManalyze(TotalSingledata))
      }

  else {
  if (pick == 4){
     cat("\n\n")
     NCAGLMmenu()
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