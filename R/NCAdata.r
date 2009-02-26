#input subject, time, test and ref concentration
NCAdata<-function(replicated=FALSE, parallel=FALSE)
{
cat("\n")
file.menu <- c("Input/edit data from keyboard",
               "Import data file with .csv format",
               "Load data File with .RData format",
               "Back to NCA menu",
               "Quit")
cat("\n")

pick <- menu(file.menu, title = " << NCA >> ")
if (pick == 1){
    cat("\n")
     if(parallel){
     description_ParaNCAinput()
     TotalSingledata<-data.frame (subj=c(0), drug=c(0),time=c(0), conc=c(0))
     }
     else{
       if(replicated){
       description_RepNCAinput()
       TotalSingledata<-data.frame (subj=c(0),seq=c(0),prd=c(0),drug=c(0),time=c(0), conc=c(0))
       }
       else{
       description_NCAinput()
       TotalSingledata<-data.frame (subj=c(0), seq=c(0),prd=c(0),time=c(0), conc=c(0))
       }
     }
     TotalSingledata<-edit(TotalSingledata)
     TotalSingledata<- na.omit(TotalSingledata)
     show(TotalSingledata)
     cat("\n")
      cat("Enter the file name:\n")
        TotalSinglename <-readline()
        TotalSinglename<-paste(TotalSinglename,".RData",sep="")
           if(file.exists(TotalSinglename)){
           cat("\n")
           cat("*****************************************************\n")
           cat(" The file have been already existed.                 \n")
           cat(" Would you like to want to overwrite it ? (y/n)      \n")
           cat("*****************************************************\n")
           ans<-readline()
             if (ans == "y" | ans == "Y")
                {
                save(TotalSingledata, file=TotalSinglename)
                cat("\n")
                }
                else{
                cat("\nEnter file name:\n")
                TotalSinglename <-readline()
                TotalSinglename<-paste(TotalSinglename,".RData",sep="")
                repeat{
                    if(file.exists(TotalSinglename))
                      {
                      cat("\n")
                      cat("*******************************************\n")
                      cat(" The file have already been existed.       \n")
                      cat(" Please try another file name.             \n")
                      cat("*******************************************\n")
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
           if(parallel){
            return(ParaNCAanalyze(TotalSingledata))
           }
           else{ 
            if (replicated){
              return(RepNCAanalyze(TotalSingledata))
             }
           else{
              return(NCAanalyze(TotalSingledata))
             }  
         }
      }    
    

else {
  if (pick == 2){
       cat("\n")                                          
      if(parallel){
      description_ParaNCAcsv
      return(ParaNCAcsv())
      }
      else{
        if (replicated){
        description_RepNCAcsv
        return(RepNCAcsv())
        }
        else{
        description_NCAcsv()
        return(NCAcsv())
        }
      }
    }
else {
  if (pick == 3){
      cat("\n")
      description_load()
         TotalSinglename <-readline()
         TotalSinglename<-paste(TotalSinglename,".RData",sep="")
         load(TotalSinglename)
         TotalSingledata<-edit(TotalSingledata)
         TotalSingledata<- na.omit(TotalSingledata)
         if(parallel){
         colnames(TotalSingledata)<-list("subj", "drug","time", "conc")
         }
         else{
           if (replicated){
              colnames(TotalSingledata)<-list("subj", "seq", "prd", "drug","time", "conc")
            }
           else{
              colnames(TotalSingledata)<-list("subj", "seq", "prd", "time", "conc")
            }
          }
         cat("\n\n")
         show(TotalSingledata)
         save(TotalSingledata,file=TotalSinglename)
         cat("\n\n")
         if(parallel){
          return(ParaNCAanalyze(TotalSingledata))
         }
          else{ 
           if (replicated){
             return(RepNCAanalyze(TotalSingledata))
             }
           else{
              return(NCAanalyze(TotalSingledata))
             }
          } 
      }

  else {
  if (pick == 4){
     cat("\n\n")
         if(parallel){
          Paramenu()
          }
         else{
           if (replicated){
              Repmenu()
             }
           else{
               NCAmenu()
             }
          }
      }
  else {
  if (pick == 5){
     cat("\nThank you for using bear!  Bye now. \n\n")
                }
     }
    }
   }
  }
}