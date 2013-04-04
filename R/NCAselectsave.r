NCAselectsave<-function(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split,
                        Tau, TlastD,SingleRdata0,SingleTdata0, BANOVA=FALSE, replicated=FALSE, MIX=FALSE, parallel=FALSE, multiple=FALSE)
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
                 if(replicated){
                   if(MIX){
                    RepNCA.MIX(Totalplot,Dose,ref_data, test_data,SingleRdata, SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split)
                   }
                   else{
                    RepNCA(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split)
                   }
                 }
                 else{
                    if(BANOVA){
                       if(multiple){
                        MultipleNCA.BANOVA(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split,Tau, TlastD,SingleRdata0,SingleTdata0)
                       }
                       else{
                        NCA.BANOVA(Totalplot,Dose,ref_data, test_data,SingleRdata, SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split)
                      }
                    }
                    else{
                     if(parallel){
                       if(multiple){
                         if(MIX){
                         MultipleParaNCA.MIX(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split,Tau, TlastD,SingleRdata0,SingleTdata0)
                        }
                         else{
                          MultipleParaNCA(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split,Tau, TlastD,SingleRdata0,SingleTdata0)
                        }
                       }
                       else{
                        if(MIX){
                        ParaNCA.MIX(Totalplot,Dose,ref_data, test_data,SingleRdata, SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split)
                         }
                         else{
                        ParaNCA(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split)
                         }
                        }
                      }
                     else{
                       if(multiple){
                       MultipleNCA(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split,Tau, TlastD,SingleRdata0,SingleTdata0)
                       }
                        else{
                        NCA(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split)
                        }
                      }
                    }
                 }
                ) 
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
                      saveRDS(comdata,comdataname)
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
              saveRDS(comdata,comdataname)
                }
                else{
                 saveRDS(comdata,comdataname)
                  }
             if(replicated){
                   if(MIX){
                    RepNCA.MIX(Totalplot,Dose,ref_data, test_data,SingleRdata, SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split)
                   }
                   else{
                    RepNCA(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split)
                   }
                 }
                 else{
                    if(BANOVA){
                     if(multiple){
                        MultipleNCA.BANOVA(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split,Tau, TlastD,SingleRdata0,SingleTdata0)
                       }
                       else{
                        NCA.BANOVA(Totalplot,Dose,ref_data, test_data,SingleRdata, SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split)
                      }
                    }
                    else{
                     if(parallel){
                       if(multiple){
                        if(MIX){
                         MultipleParaNCA.MIX(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split,Tau, TlastD,SingleRdata0,SingleTdata0)
                        }
                         else{
                         MultipleParaNCA(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split,Tau, TlastD,SingleRdata0,SingleTdata0)
                          } 
                        }
                       else{
                         if(MIX){
                          ParaNCA.MIX(Totalplot,Dose,ref_data, test_data,SingleRdata, SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split) 
                          }
                          else{
                          ParaNCA(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split) 
                         }
                        }
                     }
                     else{
                       if(multiple){
                        MultipleNCA(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split,Tau, TlastD,SingleRdata0,SingleTdata0)
                        }
                        else{
                        NCA(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split)
                        }
                     }
                    }
                 }
            }
 }
 