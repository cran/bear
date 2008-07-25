#choose separator and decimal type
GLMcsv<-function()
{
cat("\n")
file.menu <- c("sep = comma (,) &  dec= point (.)",
               "sep = semicolon (;) &  dec= comma (,)",
               "sep = semicolon (;) &  dec= point (.)",
               "sep = {space} &  dec= comma (,)",
               "sep = {space} &  dec= point (.)",
               "sep = {tab} &  dec= comma (,)",
               "sep = {tab} &  dec= point (.)",
               "sep = colon (:) &  dec= comma (,)",
               "sep = colon (:) &  dec= point (.)",
               "Back to ANOVA (lm) menu")
cat("\n")
pick <- menu(file.menu, title = " << Separator type and Decimal type >> ")
if (pick == 1){
  cat("\n\n")
        cat("\nEnter Data file name(without file extention of .csv)\n")
        TotalData.file <-readline()
        TotalData.file<-paste(TotalData.file,".csv",sep="")
        cnames<-c("subj","drug","seq", "prd","Cmax", "AUC0t", "AUC0INF","LnCmax","LnAUC0t","LnAUC0INF")
        TotalData<-read.csv(TotalData.file,header=TRUE,row.names=NULL,col.names=cnames, sep=",",dec=".")
        TotalData<-edit(TotalData)
        TotalData<-na.omit(TotalData)
        cat("\n\n")
        show(TotalData)
            return(GLManalyze(TotalData))
     }

 else {
  if (pick == 2){
  cat("\n\n")
        cat("\nEnter Data file name(without file extention of.csv)\n")
        TotalData.file <-readline()
        TotalData.file<-paste(TotalData.file,".csv",sep="")
        cnames<-c("subj","drug","seq", "prd","Cmax", "AUC0t", "AUC0INF","LnCmax","LnAUC0t","LnAUC0INF")
        TotalData<-read.csv(TotalData.file,header=TRUE,row.names=NULL,col.names=cnames, sep=";",dec=",")
        TotalData<-edit(TotalData)
        TotalData<-na.omit(TotalData)
        cat("\n\n")
        show(TotalData)
            return(GLManalyze(TotalData))
          }
 else {
  if (pick == 3){
  cat("\n\n")
        cat("\nEnter Data file name(without file extention of.csv)\n")
        TotalData.file <-readline()
        TotalData.file<-paste(TotalData.file,".csv",sep="")
        cnames<-c("subj","drug","seq", "prd","Cmax", "AUC0t", "AUC0INF","LnCmax","LnAUC0t","LnAUC0INF")
        TotalData<-read.csv(TotalData.file,header=TRUE,row.names=NULL,col.names=cnames, sep=";",dec=".")
        TotalData<-edit(TotalData)
        TotalData<-na.omit(TotalData)
        cat("\n\n")
        show(TotalData)
            return(GLManalyze(TotalData))
          }
else {
  if (pick == 4){
  cat("\n\n")
        cat("\nEnter Data file name(without file extention of.csv)\n")
        TotalData.file <-readline()
        TotalData.file<-paste(TotalData.file,".csv",sep="")
        cnames<-c("subj","drug","seq", "prd","Cmax", "AUC0t", "AUC0INF","LnCmax","LnAUC0t","LnAUC0INF")
        TotalData<-read.csv(TotalData.file,header=TRUE,row.names=NULL,col.names=cnames, sep=" ",dec=",")
        TotalData<-edit(TotalData)
        TotalData<-na.omit(TotalData)
        cat("\n\n")
        show(TotalData)
            return(GLManalyze(TotalData))
          }
else {
  if (pick == 5){
  cat("\n\n")
        cat("\nEnter Data file name(without file extention of.csv)\n")
        TotalData.file <-readline()
        TotalData.file<-paste(TotalData.file,".csv",sep="")
        cnames<-c("subj","drug","seq", "prd","Cmax", "AUC0t", "AUC0INF","LnCmax","LnAUC0t","LnAUC0INF")
        TotalData<-read.csv(TotalData.file,header=TRUE,row.names=NULL,col.names=cnames, sep=" ",dec=".")
        TotalData<-edit(TotalData)
        TotalData<-na.omit(TotalData)
        cat("\n\n")
        show(TotalData)
             return(GLManalyze(TotalData))
          }          
else {
  if (pick == 6){
  cat("\n\n")
        cat("\nEnter Data file name(without file extention of.csv)\n")
        TotalData.file <-readline()
        TotalData.file<-paste(TotalData.file,".csv",sep="")
        cnames<-c("subj","drug","seq", "prd","Cmax", "AUC0t", "AUC0INF","LnCmax","LnAUC0t","LnAUC0INF")
        TotalData<-read.csv(TotalData.file,header=TRUE,row.names=NULL,col.names=cnames, sep="\t",dec=",")
        TotalData<-edit(TotalData)
        TotalData<-na.omit(TotalData)
        cat("\n\n")
        show(TotalData)
            return(GLManalyze(TotalData))
          }     
else {
  if (pick == 7){
  cat("\n\n")
        cat("\nEnter Data file name(without file extention of.csv)\n")
        TotalData.file <-readline()
        TotalData.file<-paste(TotalData.file,".csv",sep="")
        cnames<-c("subj","drug","seq", "prd","Cmax", "AUC0t", "AUC0INF","LnCmax","LnAUC0t","LnAUC0INF")
        TotalData<-read.csv(TotalData.file,header=TRUE,row.names=NULL,col.names=cnames, sep="\t",dec=".")
        TotalData<-edit(TotalData)
        TotalData<-na.omit(TotalData)
        cat("\n\n")
        show(TotalData)
                    return(GLManalyze(TotalData))
          }    
else {
  if (pick == 8){
  cat("\n\n")
        cat("\nEnter Data file name(without file extention of.csv)\n")
        TotalData.file <-readline()
        TotalData.file<-paste(TotalData.file,".csv",sep="")
        cnames<-c("subj","drug","seq", "prd","Cmax", "AUC0t", "AUC0INF","LnCmax","LnAUC0t","LnAUC0INF")
        TotalData<-read.csv(TotalData.file,header=TRUE,row.names=NULL,col.names=cnames, sep=":",dec=",")
        TotalData<-edit(TotalData)
        TotalData<-na.omit(TotalData)
        cat("\n\n")
        show(TotalData)
            return(GLManalyze(TotalData))
          }    
else {
  if (pick == 9){
  cat("\n\n")
        cat("\nEnter Data file name(without file extention of.csv)\n")
        TotalData.file <-readline()
        TotalData.file<-paste(TotalData.file,".csv",sep="")
        cnames<-c("subj","drug","seq", "prd","Cmax", "AUC0t", "AUC0INF","LnCmax","LnAUC0t","LnAUC0INF")
        TotalData<-read.csv(TotalData.file,header=TRUE,row.names=NULL,col.names=cnames, sep=":",dec=".")
        TotalData<-edit(TotalData)
        TotalData<-na.omit(TotalData)
        cat("\n\n")
        show(TotalData)
            return(GLManalyze(TotalData))
          }               
 else {
  if (pick == 10){
      return (GLMmenu())
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