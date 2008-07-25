#NCA-->GLM
NCAGLMdata<-function()
{
cat("\n")
file.menu <- c("Input/Edit Data from keyboard",
               "Import Data Files (.CSV)",
               "Load Data Files (.RData)",
               "Back to NCA--> ANOVA (lm) menu",
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
cat("*       Period 1: first treatment period                                   *\n")
cat("*       Period 2: second treatment period                                  *\n")
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
cat("****************************************************************************\n")
cat("* Data should consist of                                                   *\n")
cat("*--------------------------------------------------------------------------*\n")
cat("* row#1: subj, seq, prd, time, conc                                        *\n")
cat("* column#1: subject no.(subj)                                              *\n")
cat("* column#2: sequence (seq)                                                 *\n")
cat("*          ->Sequence = 1 if Reference-->Test                              *\n")
cat("*          ->Sequence = 2 if Test-->Reference                              *\n")                                                       
cat("* column#3: period (prd)                                                   *\n")   
cat("*          ->Period 1: 1st treatment period                                *\n")
cat("*          ->Period 2: 2nd treatment period                                *\n")                                                       
cat("* column#4: time (unit)                                                    *\n")
cat("* column#5: drug plasma/serum concentration (conc)                         *\n")
cat("****************************************************************************\n")
cat("\n")
filepath<-getwd()
cat("R will import your data from the directory of \n")
cat("",filepath,"\n")
        return(NCAGLMcsv())
      }

else {
  if (pick == 3){
    cat("\n\n")
    filepath<-getwd()
    cat("R will load your data from the directory of \n")
    cat("",filepath,"\n")
    cat("\n")
    cat("\nEnter Data file name(without file extention of .RData)\n")
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