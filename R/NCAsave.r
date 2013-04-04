##save the NCA results 
NCAsave<-function(TotalData, replicated=FALSE, parallel=FALSE, multiple=FALSE){

  cat("\n\n")
     cat("\nEnter the file name (without file extention):\n")
               Totalname <-readline()
               Totalname<-paste(Totalname,".RData",sep="")
                 if(file.exists(Totalname)){
                   cat("\n")
                   cat("*****************************************\n")
                   cat(" The file have been existed.             \n")
                   cat(" Would you want to overwrite it ? (y/n)  \n")
                   cat("*****************************************\n")
                   ans<-readline()
                      if (ans == "y" | ans == "Y"){
                      saveRDS(TotalData,Totalname)
                      cat("\n")
                              }
                      else{
                      cat("\nEnter the file name (without file extention):\n")
                      Totalname <-readline()
                      Totalname<-paste(Totalname,".RData",sep="")
                        repeat{
                        if(file.exists(Totalname)){
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
              MultipleParaMIXmenu(TotalData)
              }
              else{
              ParaMIXmenu(TotalData)
                }
              }
             else{
               if(replicated){
                 RepMIXmenu(TotalData)
                 }
                else{
                  if(multiple){
                    MultipleBANOVAmenu(TotalData)
                    }
                    else{
                    BANOVAmenu(TotalData)
                    }
                 }
            }
}

 