NCAselectsave<-function(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis
                        ,rdata.split,tdata.split,BANOVA=FALSE)
{
comdata<-rbind(ref_data,test_data)
cat("****************************************************************************\n")
cat("     drug# 1: Ref.                                                          \n")
cat("     drug# 2: Test                                                          \n")
cat("****************************************************************************\n")
show(comdata)
         cat("\n")
         cat("\nSave selections (y/n)?\n")
            ans<-readline()
            cat("\n")
              if (ans == "n" | ans == "N"){
               return(
                 if(BANOVA){ 
                     NCA.BANOVA(Totalplot,Dose,ref_data, test_data,SingleRdata, SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split)
                      }
                      else{
                     NCA(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split)
                      } )
                     } 
              else {
               cat("\nEnter the file name (without file extention):\n")
               comdataname <-readline()
               comdataname<-paste(comdataname,".RData",sep="")
                 if(file.exists(comdataname)){
                   cat("\n")
                   cat("*****************************************\n")
                   cat(" The file have been existed.      \n")
                   cat(" Would you want to overwrite it ? (y/n)\n")
                   cat("*****************************************\n")
                   ans<-readline()
                      if (ans == "y" | ans == "Y"){
                      save(comdata,file=comdataname)
                      cat("\n")
                              }
                      else{
                      cat("\nEnter the file name (without file extention):\n")
                      comdataname <-readline()
                      comdataname<-paste(comdataname,".RData",sep="")
                        repeat{
                        if(file.exists(comdataname)){
                        cat("\n")
                        cat("***********************************\n")
                        cat(" The file have been existed.\n")
                        cat(" Please try another file name. \n")
                        cat("***********************************\n")
                      comdataname<-readline()
                      comdataname<-paste(comdataname,".RData",sep="")
                         }
                        else{
                         break
                         }
                        }
                      }
              save(comdata,file=comdataname)
                }
                else{
                 save(comdata,file=comdataname)
                  }
            if(BANOVA){ 
             NCA.BANOVA(Totalplot,Dose,ref_data, test_data,SingleRdata, SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split)
             } 
             else{
              NCA(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split)
            }
       }
 }