#NCA-->GLM
NCA.BANOVAdata<-function(replicated=FALSE, parallel=FALSE, multiple=FALSE)
{
cat("\n")
file.menu <- c("Input/edit data from keyboard",
               "Import data file with .csv format",
               "Load data File with .RData format",
               "Back to NCA --> Statistical analysis",
               "Quit")
cat("\n")

pick <- menu(file.menu, title = " << NCA--> Statistical analysis >> ", graphics=TRUE)
if (pick == 1){
cat("\n")
    if(parallel){
    description_ParaNCAinput()
    TotalSingledata<-data.frame (subj=c(0),drug=c(0),time=c(0), conc=c(0))
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
           cat("******************************************\n")
           cat("* The file name have been existed.       *\n")
           cat("* Would you want to overwrite it ? (y/n) *\n")
           cat("******************************************\n")
           ans<-readline()
             if (ans == "y" | ans == "Y")
                {
                saveRDS(TotalSingledata, TotalSinglename)
                cat("\n")
                }
                else{
                cat("\nEnter file name (no extension!):\n")
                TotalSinglename <-readline()
                TotalSinglename<-paste(TotalSinglename,".RData",sep="")
                repeat{
                    if(file.exists(TotalSinglename))
                      {
                      cat("\n")
                      cat("***********************************\n")
                      cat(" The file have been existed.       \n")
                      cat(" Please try another file name.     \n")
                      cat("***********************************\n")
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
            return(MultipleParaNCA.MIXanalyze(TotalSingledata))
            }
            else{
            return(ParaNCA.MIXanalyze(TotalSingledata))
             }
           }
           else{ 
         if(replicated){
              return(RepNCA.MIXanalyze(TotalSingledata))
             }
           else{
              if(multiple){
              return(MultipleNCA.BANOVAanalyze(TotalSingledata))
              }
              else{
              return(NCA.BANOVAanalyze(TotalSingledata))
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
       return(MultipleParaNCA.MIXcsv())
      }
      else{
      return(ParaNCA.MIXcsv())
       }
     }
     else{ 
      if (replicated){
        description_RepNCAcsv()
        return(RepNCA.MIXcsv())
         }
        else{
        description_NCAcsv()
        if(multiple){
        return(MultipleNCA.BANOVAcsv())
        }
        else{
        return(NCA.BANOVAcsv())
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
     TotalSingledata<-readRDS(file.choose())
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
     cat("\n\n")
        if(parallel){
            if(multiple){
            return(MultipleParaNCA.MIXanalyze(TotalSingledata))
            }
            else{
            return(ParaNCA.MIXanalyze(TotalSingledata))
             }
           }
           else{ 
         if(replicated){
              return(RepNCA.MIXanalyze(TotalSingledata))
             }
           else{
              if(multiple){
              return(MultipleNCA.BANOVAanalyze(TotalSingledata))
              }
              else{
              return(NCA.BANOVAanalyze(TotalSingledata))
              }
             }  
         }
      }

  else {
  if (pick == 4){
     cat("\n\n")
      if(parallel){
       if(multiple){
        MultipleParaNCA.MIXmenu()
        }
       else{     
        ParaNCA.MIXmenu()
         }  
       }
      else{  
      if (replicated){
              RepNCA.MIXmenu()
             }
       else{
       if(multiple){
              return(MultipleNCA.BANOVAmenu())
              }
              else{
              return( NCA.BANOVAmenu())
               }
             }
          }
        }  
  else {
  if (pick == 5){
      cat("\n  Thank you for using bear!  Bye now. \n\n")
      graphics.off()
                }
     }
    }
   }
  }
}