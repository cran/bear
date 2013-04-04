#choose separator and decimal type
NCAcsv<-function(replicated=FALSE, parallel=FALSE, multiple=FALSE)
{
cat("\n")
file.menu <- c("separator = comma (,) &  decimal = point (.)",
               "separator = semicolon (;) &  decimal = comma (,)",
               "separator = semicolon (;) &  decimal = point (.)",
               "separator = {space} &  decimal = comma (,)",
               "separator = {space} &  decimal = point (.)",
               "separator = {tab} &  decimal = comma (,)",
               "separator = {tab} &  decimal = point (.)",
               "separator = colon (:) &  decimal = comma (,)",
               "separator = colon (:) &  decimal = point (.)",
               "Back to NCAmenu")
cat("\n")
pick <- menu(file.menu, title = " << Separator and decimal formats >> ")
if (pick == 1){
  cat("\n\n")
        description_import()
        ## TotalSingledata.file <-readline()
        ## TotalSingledata.file<-paste(TotalSingledata.file,".csv",sep="")
        if(parallel){
        cnames<-c("subj", "drug","time", "conc")
        }
        else{
         if(replicated){
         cnames<-c("subj", "seq", "prd", "drug","time", "conc")
         }
         else{
         cnames<-c("subj", "seq","prd", "time", "conc")
         }
        } 
        TotalSingledata<-read.csv(file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=",",dec=".")
        TotalSingledata<-edit(TotalSingledata)
        TotalSingledata<-na.omit(TotalSingledata)
        cat("\n\n")
        show(TotalSingledata)
         if(parallel){
             if(multiple){
              return(MultipleParaNCAanalyze(TotalSingledata))
               }
              else{
               return(ParaNCAanalyze(TotalSingledata))
               }
         }
         else{ 
           if(replicated){
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
  cat("\n\n")
        description_import()
        ## TotalSingledata.file <-readline()
        ## TotalSingledata.file<-paste(TotalSingledata.file,".csv",sep="")
        if(parallel){
        cnames<-c("subj", "drug","time", "conc")
        }
        else{
         if(replicated){
         cnames<-c("subj", "seq", "prd", "drug","time", "conc")
         }
         else{
        cnames<-c("subj", "seq","prd", "time", "conc")
         }
        }
        TotalSingledata<-read.csv(file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=";",dec=",")
        TotalSingledata<-edit(TotalSingledata)
        TotalSingledata<-na.omit(TotalSingledata)
        cat("\n\n")
        show(TotalSingledata)
        if(parallel){
         if(multiple){
              return(MultipleParaNCAanalyze(TotalSingledata))
               }
              else{
               return(ParaNCAanalyze(TotalSingledata))
               }
         }
         else{ 
           if(replicated){
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
  if (pick == 3){
  cat("\n\n")
        description_import()
        ## TotalSingledata.file <-readline()
        ## TotalSingledata.file<-paste(TotalSingledata.file,".csv",sep="")
        if(parallel){
        cnames<-c("subj", "drug","time", "conc")
        }
        else{
         if(replicated){
         cnames<-c("subj", "seq", "prd", "drug","time", "conc")
         }
         else{
        cnames<-c("subj", "seq","prd", "time", "conc")
         }
        } 
        TotalSingledata<-read.csv(file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=";",dec=".")
        TotalSingledata<-edit(TotalSingledata)
        TotalSingledata<-na.omit(TotalSingledata)
        cat("\n\n")
        show(TotalSingledata)
        if(parallel){
         if(multiple){
              return(MultipleParaNCAanalyze(TotalSingledata))
               }
              else{
               return(ParaNCAanalyze(TotalSingledata))
               }
         }
         else{ 
           if(replicated){
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
        description_import()
        ## TotalSingledata.file <-readline()
        ## TotalSingledata.file<-paste(TotalSingledata.file,".csv",sep="")
        if(parallel){
        cnames<-c("subj", "drug","time", "conc")
        }
        else{
         if(replicated){
         cnames<-c("subj", "seq", "prd", "drug","time", "conc")
         }
         else{
        cnames<-c("subj", "seq","prd", "time", "conc")
         }
        } 
        TotalSingledata<-read.csv(file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=" ",dec=",")
        TotalSingledata<-edit(TotalSingledata)
        TotalSingledata<-na.omit(TotalSingledata)
        cat("\n\n")
        show(TotalSingledata)
       if(parallel){
         if(multiple){
              return(MultipleParaNCAanalyze(TotalSingledata))
               }
              else{
               return(ParaNCAanalyze(TotalSingledata))
               }
         }
         else{  
           if(replicated){
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
  if (pick == 5){
  cat("\n\n")
        description_import()
        ## TotalSingledata.file <-readline()
        ## TotalSingledata.file<-paste(TotalSingledata.file,".csv",sep="")
        if(parallel){
        cnames<-c("subj", "drug","time", "conc")
        }
        else{
         if(replicated){
         cnames<-c("subj", "seq", "prd", "drug","time", "conc")
         }
         else{
        cnames<-c("subj", "seq","prd", "time", "conc")
         }
        } 
        TotalSingledata<-read.csv(file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=" ",dec=".")
        TotalSingledata<-edit(TotalSingledata)
        TotalSingledata<-na.omit(TotalSingledata)
        cat("\n\n")
        show(TotalSingledata)
        if(parallel){
          if(multiple){
              return(MultipleParaNCAanalyze(TotalSingledata))
               }
              else{
               return(ParaNCAanalyze(TotalSingledata))
               }
         }
         else{ 
           if(replicated){
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
  if (pick == 6){
  cat("\n\n")
        description_import()
        ## TotalSingledata.file <-readline()
        ## TotalSingledata.file<-paste(TotalSingledata.file,".csv",sep="")
        if(parallel){
        cnames<-c("subj", "drug","time", "conc")
        }
        else{
         if(replicated){
         cnames<-c("subj", "seq", "prd", "drug","time", "conc")
         }
         else{
        cnames<-c("subj", "seq","prd", "time", "conc")
         }
        } 
        TotalSingledata<-read.csv(file.choose(),header=TRUE,row.names=NULL,col.names=cnames,  sep="\t",dec=",")
        TotalSingledata<-edit(TotalSingledata)
        TotalSingledata<-na.omit(TotalSingledata)
        cat("\n\n")
        show(TotalSingledata)
         if(parallel){
          if(multiple){
              return(MultipleParaNCAanalyze(TotalSingledata))
               }
              else{
               return(ParaNCAanalyze(TotalSingledata))
               }
         }
         else{ 
           if(replicated){
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
  if (pick == 7){
  cat("\n\n")
        description_import()
        ## TotalSingledata.file <-readline()
        ## TotalSingledata.file<-paste(TotalSingledata.file,".csv",sep="")
        if(parallel){
        cnames<-c("subj", "drug","time", "conc")
        }
        else{
         if(replicated){
         cnames<-c("subj", "seq", "prd", "drug","time", "conc")
         }
         else{
        cnames<-c("subj", "seq","prd", "time", "conc")
         }
        } 
        TotalSingledata<-read.csv(file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep="\t",dec=".")
        TotalSingledata<-edit(TotalSingledata)
        TotalSingledata<-na.omit(TotalSingledata)
        cat("\n\n")
        show(TotalSingledata)
         if(parallel){
          if(multiple){
              return(MultipleParaNCAanalyze(TotalSingledata))
               }
              else{
               return(ParaNCAanalyze(TotalSingledata))
               }
         }
         else{ 
           if(replicated){
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
  if (pick == 8){
  cat("\n\n")
        description_import()
        ## TotalSingledata.file <-readline()
        ## TotalSingledata.file<-paste(TotalSingledata.file,".csv",sep="")
        if(parallel){
         cnames<-c("subj", "drug","time", "conc")
         }
        else{
        if(replicated){
        cnames<-c("subj", "seq", "prd", "drug","time", "conc")
        }
         else{
        cnames<-c("subj", "seq","prd", "time", "conc")
         }
        }
        TotalSingledata<-read.csv(file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=":",dec=",")
        TotalSingledata<-edit(TotalSingledata)
        TotalSingledata<-na.omit(TotalSingledata)
        cat("\n\n")
        show(TotalSingledata)
          if(parallel){
         if(multiple){
              return(MultipleParaNCAanalyze(TotalSingledata))
               }
              else{
               return(ParaNCAanalyze(TotalSingledata))
               }
         }
         else{ 
          if(replicated){
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
  if (pick == 9){
  cat("\n\n")
        description_import()
        ## TotalSingledata.file <-readline()
        ## TotalSingledata.file<-paste(TotalSingledata.file,".csv",sep="")
        if(parallel){
        cnames<-c("subj", "drug","time", "conc")
        }
        else{
         if(replicated){
         cnames<-c("subj", "seq", "prd", "drug","time", "conc")
         }
         else{
         cnames<-c("subj", "seq","prd", "time", "conc")
         }
        } 
        TotalSingledata<-read.csv(file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=":",dec=".")
        TotalSingledata<-edit(TotalSingledata)
        TotalSingledata<-na.omit(TotalSingledata)
        cat("\n\n")
        show(TotalSingledata)
        if(parallel){
         if(multiple){
              return(MultipleParaNCAanalyze(TotalSingledata))
               }
              else{
               return(ParaNCAanalyze(TotalSingledata))
               }
         }
         else{ 
          if(replicated){
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
  if (pick == 10){
     if(parallel){
         if(multiple){
               MultipleParamenu() 
              }
              else{ 
              return(Paramenu())
              } 
         }
         else{ 
      if(replicated){
           return(Repmenu())
           }
         else{
            if(multiple){
               MultipleNCAmenu()
              }
              else{ 
              return (NCAmenu())
              } 
             }
            }
           }
          }
         }
        }
       }
      }
     }
    }
  }
 }
}        