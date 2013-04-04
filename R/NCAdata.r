#input subject, time, test and ref concentration
NCAdata<-function(replicated=FALSE, parallel=FALSE, multiple=FALSE)
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
      cat("Enter the file name (no extension!):\n")
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
                saveRDS(TotalSingledata, TotalSinglename)
                cat("\n")
                }
                else{
                cat("\nEnter filename to be saved (no file extension!):\n")
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
              saveRDS(TotalSingledata,TotalSinglename)
           }
        else{
           saveRDS(TotalSingledata,TotalSinglename)
          }
           if(parallel){
             if(multiple){
              return(MultipleParaNCAanalyze(TotalSingledata))
               }
              else{
               return(ParaNCAanalyze(TotalSingledata))
               }
           }
           else{ 
            if (replicated){
              return(RepNCAanalyze(TotalSingledata))
             }
           else{
              if(multiple){
                 return(MultipleNCAanalyze(TotalSingledata))
                  }
               else{ 
                 return(NCAanalyze(TotalSingledata))
                 } 
             }  
         }
      }    
    

else {
  if (pick == 2){
       cat("\n")                                          
      if(parallel){
      description_ParaNCAcsv()
        if(multiple){
        return(MultipleParaNCAcsv())
        }
        else{
         return(ParaNCAcsv())
        }
      }
      else{
        if (replicated){
        description_RepNCAcsv()
        return(RepNCAcsv())
        }
        else{
        description_NCAcsv()
         if(multiple){
                 return(MultipleNCAcsv())
                  }
               else{ 
                  return(NCAcsv())
                 } 
        }
      }
    }
else {
  if (pick == 3){ 
      cat("\n")
      description_load()
##     TotalSinglename <-readline() 
##     TotalSinglename<-paste(TotalSinglename,".RData",sep="")
##     load(TotalSinglename)
     TotalSingledata<-readRDS(file.choose())  # musch better than using load()
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
         TotalSinglename <-readline(" Enter the filename to be saved (no extension):")                           
         TotalSinglename<-paste(TotalSinglename,".RData",sep="")
         saveRDS(TotalSingledata,TotalSinglename)
         cat("\n\n")
         if(parallel){
           if(multiple){
              return(MultipleParaNCAanalyze(TotalSingledata))
               }
              else{
               return(ParaNCAanalyze(TotalSingledata))
               }
         }
          else{ 
           if (replicated){
             return(RepNCAanalyze(TotalSingledata))
             }
            else{
                if(multiple){
                return(MultipleNCAanalyze(TotalSingledata))
                }
                else{ 
                return(NCAanalyze(TotalSingledata))
               } 
             }
          } 
      }

  else {
  if (pick == 4){
     cat("\n\n")
         if(parallel){
            if(multiple){
            MultipleParamenu() 
              }
             else{ 
            Paramenu()
            } 
          }
         else{
           if (replicated){
              Repmenu()
             }
           else{
               if(multiple){
               MultipleNCAmenu()
               }
               else{ 
               NCAmenu()
             }
          }
      }
}
  else {
  if (pick == 5){
     cat("\n   Thank you for using bear!  Bye now. \n\n")
     graphics.off()
                }
     }
    }
   }
  }
}