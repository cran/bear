NCAsave<-function(TotalData){

  cat("\n\n")
   cat("\nSave NCA outputs (y/n)?\n")
            ans<-readline()
            cat("\n")
              if (ans == "n" | ans == "N"){
               return(NCAmenu())
                      }
              else {
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
                      save(TotalData,file=Totalname)
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
              save(TotalData,file=Totalname)
                }
                else{
                 save(TotalData,file=Totalname)
                  }
              BANOVAmenu(TotalData)
            }
 }
 