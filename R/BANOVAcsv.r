#choose separator and decimal type
BANOVAcsv<-function(replicated=FALSE, parallel=FALSE)
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
               "Back to Statistical analysis menu")
cat("\n")
pick <- menu(file.menu, title = " << Separator and decimal formats >> ")
if (pick == 1){
  cat("\n\n")
        description_import()
        TotalData.file <-readline()
        TotalData.file<-paste(TotalData.file,".csv",sep="")
        if(parallel){
        cnames<-c("subj","drug","Cmax", "AUC0t", "AUC0INF","lnCmax","lnAUC0t","lnAUC0INF")
        } 
        else{ 
        cnames<-c("subj","drug","seq", "prd","Cmax", "AUC0t", "AUC0INF","lnCmax","lnAUC0t","lnAUC0INF")
         } 
        TotalData<-read.csv(TotalData.file,header=TRUE,row.names=NULL,col.names=cnames, sep=",",dec=".")
        TotalData<-edit(TotalData)
        TotalData<-na.omit(TotalData)
        cat("\n\n")
        show(TotalData)
            
        if(parallel){
        ParaMIXanalyze(TotalData)
        ParaMIXmenu() 
        }
        else{ 
        if(replicated){
         RepMIXanalyze(TotalData)
         RepMIXmenu() 
          }
         else{
         BANOVAanalyze(TotalData)
         BANOVAmenu()
        }
     }
  } 
 else {
  if (pick == 2){
  cat("\n\n")
        description_import()
        TotalData.file <-readline()
        TotalData.file<-paste(TotalData.file,".csv",sep="")
        if(parallel){
        cnames<-c("subj","drug","Cmax", "AUC0t", "AUC0INF","lnCmax","lnAUC0t","lnAUC0INF")
        } 
        else{ 
        cnames<-c("subj","drug","seq", "prd","Cmax", "AUC0t", "AUC0INF","lnCmax","lnAUC0t","lnAUC0INF")
        } 
        TotalData<-read.csv(TotalData.file,header=TRUE,row.names=NULL,col.names=cnames, sep=";",dec=",")
        TotalData<-edit(TotalData)
        TotalData<-na.omit(TotalData)
        cat("\n\n")
        show(TotalData)
        if(parallel){
        ParaMIXanalyze(TotalData)
        ParaMIXmenu() 
        }
        else{ 
        if(replicated){
         RepMIXanalyze(TotalData)
         RepMIXmenu() 
          }
         else{
         BANOVAanalyze(TotalData)
         BANOVAmenu()
        }
       }
     }  
 else {
  if (pick == 3){
  cat("\n\n")
        description_import()
        TotalData.file <-readline()
        TotalData.file<-paste(TotalData.file,".csv",sep="")
        if(parallel){
        cnames<-c("subj","drug","Cmax", "AUC0t", "AUC0INF","lnCmax","lnAUC0t","lnAUC0INF")
        } 
        else{ 
        cnames<-c("subj","drug","seq", "prd","Cmax", "AUC0t", "AUC0INF","lnCmax","lnAUC0t","lnAUC0INF")
        } 
        TotalData<-read.csv(TotalData.file,header=TRUE,row.names=NULL,col.names=cnames, sep=";",dec=".")
        TotalData<-edit(TotalData)
        TotalData<-na.omit(TotalData)
        cat("\n\n")
        show(TotalData)
        if(parallel){
        ParaMIXanalyze(TotalData)
        ParaMIXmenu() 
        }
        else{ 
             if(replicated){
         RepMIXanalyze(TotalData)
         RepMIXmenu() 
          }
         else{
         BANOVAanalyze(TotalData)
         BANOVAmenu()
        }
       }
     }  
else {
  if (pick == 4){
  cat("\n\n")
        description_import()
        TotalData.file <-readline()
        TotalData.file<-paste(TotalData.file,".csv",sep="")
        if(parallel){
        cnames<-c("subj","drug","Cmax", "AUC0t", "AUC0INF","lnCmax","lnAUC0t","lnAUC0INF")
        } 
        else{ 
        cnames<-c("subj","drug","seq", "prd","Cmax", "AUC0t", "AUC0INF","lnCmax","lnAUC0t","lnAUC0INF")
        } 
        TotalData<-read.csv(TotalData.file,header=TRUE,row.names=NULL,col.names=cnames, sep=" ",dec=",")
        TotalData<-edit(TotalData)
        TotalData<-na.omit(TotalData)
        cat("\n\n")
        show(TotalData)
         if(parallel){
        ParaMIXanalyze(TotalData)
        ParaMIXmenu() 
        }
        else{ 
             if(replicated){
         RepMIXanalyze(TotalData)
         RepMIXmenu() 
          }
         else{
         BANOVAanalyze(TotalData)
         BANOVAmenu()
        }
       }
    }   
else {
  if (pick == 5){
  cat("\n\n")
        description_import()
        TotalData.file <-readline()
        TotalData.file<-paste(TotalData.file,".csv",sep="")
        if(parallel){
        cnames<-c("subj","drug","Cmax", "AUC0t", "AUC0INF","lnCmax","lnAUC0t","lnAUC0INF")
        } 
        else{ 
        cnames<-c("subj","drug","seq", "prd","Cmax", "AUC0t", "AUC0INF","lnCmax","lnAUC0t","lnAUC0INF")
        } 
        TotalData<-read.csv(TotalData.file,header=TRUE,row.names=NULL,col.names=cnames, sep=" ",dec=".")
        TotalData<-edit(TotalData)
        TotalData<-na.omit(TotalData)
        cat("\n\n")
        show(TotalData)
        if(parallel){
        ParaMIXanalyze(TotalData)
        ParaMIXmenu() 
        }
        else{ 
              if(replicated){
         RepMIXanalyze(TotalData)
         RepMIXmenu() 
          }
         else{
         BANOVAanalyze(TotalData)
         BANOVAmenu()
        }
      }
    }            
else {
  if (pick == 6){
  cat("\n\n")
        description_import()
        TotalData.file <-readline()
        TotalData.file<-paste(TotalData.file,".csv",sep="")
        if(parallel){
        cnames<-c("subj","drug","Cmax", "AUC0t", "AUC0INF","lnCmax","lnAUC0t","lnAUC0INF")
        } 
        else{ 
        cnames<-c("subj","drug","seq", "prd","Cmax", "AUC0t", "AUC0INF","lnCmax","lnAUC0t","lnAUC0INF")
        } 
        TotalData<-read.csv(TotalData.file,header=TRUE,row.names=NULL,col.names=cnames, sep="\t",dec=",")
        TotalData<-edit(TotalData)
        TotalData<-na.omit(TotalData)
        cat("\n\n")
        show(TotalData)
         if(parallel){
        ParaMIXanalyze(TotalData)
        ParaMIXmenu() 
        }
        else{ 
             if(replicated){
         RepMIXanalyze(TotalData)
         RepMIXmenu() 
          }
         else{
         BANOVAanalyze(TotalData)
         BANOVAmenu()
        }
      }
    }       
else {
  if (pick == 7){
  cat("\n\n")
        description_import()
        TotalData.file <-readline()
        TotalData.file<-paste(TotalData.file,".csv",sep="")
        if(parallel){
        cnames<-c("subj","drug","Cmax", "AUC0t", "AUC0INF","lnCmax","lnAUC0t","lnAUC0INF")
        } 
        else{ 
        cnames<-c("subj","drug","seq", "prd","Cmax", "AUC0t", "AUC0INF","lnCmax","lnAUC0t","lnAUC0INF")
        } 
        TotalData<-read.csv(TotalData.file,header=TRUE,row.names=NULL,col.names=cnames, sep="\t",dec=".")
        TotalData<-edit(TotalData)
        TotalData<-na.omit(TotalData)
        cat("\n\n")
        show(TotalData)
         if(parallel){
        ParaMIXanalyze(TotalData)
        ParaMIXmenu() 
        }
        else{ 
          if(replicated){
         RepMIXanalyze(TotalData)
         RepMIXmenu() 
          }
         else{
         BANOVAanalyze(TotalData)
         BANOVAmenu()
        }
       }
    }       
else {
  if (pick == 8){
  cat("\n\n")
        description_import()
        TotalData.file <-readline()
        TotalData.file<-paste(TotalData.file,".csv",sep="")
        if(parallel){
        cnames<-c("subj","drug","Cmax", "AUC0t", "AUC0INF","lnCmax","lnAUC0t","lnAUC0INF")
        } 
        else{ 
        cnames<-c("subj","drug","seq", "prd","Cmax", "AUC0t", "AUC0INF","lnCmax","lnAUC0t","lnAUC0INF")
        } 
        TotalData<-read.csv(TotalData.file,header=TRUE,row.names=NULL,col.names=cnames, sep=":",dec=",")
        TotalData<-edit(TotalData)
        TotalData<-na.omit(TotalData)
        cat("\n\n")
        show(TotalData)
         if(parallel){
        ParaMIXanalyze(TotalData)
        ParaMIXmenu() 
        }
        else{ 
           if(replicated){
         RepMIXanalyze(TotalData)
         RepMIXmenu() 
          }
         else{
         BANOVAanalyze(TotalData)
         BANOVAmenu()
        }
      }
     }     
else {
  if (pick == 9){
  cat("\n\n")
        description_import()
        TotalData.file <-readline()
        TotalData.file<-paste(TotalData.file,".csv",sep="")
        if(parallel){
        cnames<-c("subj","drug","Cmax", "AUC0t", "AUC0INF","lnCmax","lnAUC0t","lnAUC0INF")
        } 
        else{ 
        cnames<-c("subj","drug","seq", "prd","Cmax", "AUC0t", "AUC0INF","lnCmax","lnAUC0t","lnAUC0INF")
        } 
        TotalData<-read.csv(TotalData.file,header=TRUE,row.names=NULL,col.names=cnames, sep=":",dec=".")
        TotalData<-edit(TotalData)
        TotalData<-na.omit(TotalData)
        cat("\n\n")
        show(TotalData)
         if(parallel){
        ParaMIXanalyze(TotalData)
        ParaMIXmenu() 
        }
        else{ 
             if(replicated){
         RepMIXanalyze(TotalData)
         RepMIXmenu() 
          }
         else{
         BANOVAanalyze(TotalData)
         BANOVAmenu()
        }
       }
      }                
 else {
  if (pick == 10){
       Multiplestatmenu
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