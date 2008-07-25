#choose separator and decimal type
NCAcsv<-function()
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
               "Back to NCAmenu")
cat("\n")
pick <- menu(file.menu, title = " << Separator type and Decimal type >> ")
if (pick == 1){
  cat("\n\n")
        cat("\nEnter Data file name(without file extention of .csv)\n")
        TotalSingledata.file <-readline()
        TotalSingledata.file<-paste(TotalSingledata.file,".csv",sep="")
        cnames<-c("subj", "seq","prd", "time", "conc")
        TotalSingledata<-read.csv(TotalSingledata.file,header=TRUE,row.names=NULL,col.names=cnames, sep=",",dec=".")
        TotalSingledata<-edit(TotalSingledata)
        TotalSingledata<-na.omit(TotalSingledata)
        cat("\n\n")
        show(TotalSingledata)
           return(NCAanalyze(TotalSingledata))
     }
     
 else {
  if (pick == 2){
  cat("\n\n")
        cat("\nEnter Data file name(without file extention of.csv)\n")
        TotalSingledata.file <-readline()
        TotalSingledata.file<-paste(TotalSingledata.file,".csv",sep="")
        cnames<-c("subj", "seq","prd", "time", "conc")
        TotalSingledata<-read.csv(TotalSingledata.file,header=TRUE,row.names=NULL,col.names=cnames, sep=";",dec=",")
        TotalSingledata<-edit(TotalSingledata)
        TotalSingledata<-na.omit(TotalSingledata)
        cat("\n\n")
        show(TotalSingledata)
           return(NCAanalyze(TotalSingledata))
          }
        
 else {
  if (pick == 3){
  cat("\n\n")
        cat("\nEnter Data file name(without file extention of.csv)\n")
        TotalSingledata.file <-readline()
        TotalSingledata.file<-paste(TotalSingledata.file,".csv",sep="")
        cnames<-c("subj", "seq","prd", "time", "conc")
        TotalSingledata<-read.csv(TotalSingledata.file,header=TRUE,row.names=NULL,col.names=cnames, sep=";",dec=".")
        TotalSingledata<-edit(TotalSingledata)
        TotalSingledata<-na.omit(TotalSingledata)
        cat("\n\n")
        show(TotalSingledata)
           return(NCAanalyze(TotalSingledata))
          }
else {
  if (pick == 4){
  cat("\n\n")
        cat("\nEnter Data file name(without file extention of.csv)\n")
        TotalSingledata.file <-readline()
        TotalSingledata.file<-paste(TotalSingledata.file,".csv",sep="")
        cnames<-c("subj", "seq","prd", "time", "conc")
        TotalSingledata<-read.csv(TotalSingledata.file,header=TRUE,row.names=NULL,col.names=cnames, sep=" ",dec=",")
        TotalSingledata<-edit(TotalSingledata)
        TotalSingledata<-na.omit(TotalSingledata)
        cat("\n\n")
        show(TotalSingledata)
           return(NCAanalyze(TotalSingledata))
          } 
else {
  if (pick == 5){
  cat("\n\n")
        cat("\nEnter Data file name(without file extention of.csv)\n")
        TotalSingledata.file <-readline()
        TotalSingledata.file<-paste(TotalSingledata.file,".csv",sep="")
        cnames<-c("subj", "seq","prd", "time", "conc")
        TotalSingledata<-read.csv(TotalSingledata.file,header=TRUE,row.names=NULL,col.names=cnames, sep=" ",dec=".")
        TotalSingledata<-edit(TotalSingledata)
        TotalSingledata<-na.omit(TotalSingledata)
        cat("\n\n")
        show(TotalSingledata)
           return(NCAanalyze(TotalSingledata))
          }                   
else {
  if (pick == 6){
  cat("\n\n")
        cat("\nEnter Data file name(without file extention of.csv)\n")
        TotalSingledata.file <-readline()
        TotalSingledata.file<-paste(TotalSingledata.file,".csv",sep="")
        cnames<-c("subj", "seq","prd", "time", "conc")
        TotalSingledata<-read.csv(TotalSingledata.file,header=TRUE,row.names=NULL,col.names=cnames,  sep="\t",dec=",")
        TotalSingledata<-edit(TotalSingledata)
        TotalSingledata<-na.omit(TotalSingledata)
        cat("\n\n")
        show(TotalSingledata)
           return(NCAanalyze(TotalSingledata))
          }                             
else {
  if (pick == 7){
  cat("\n\n")
        cat("\nEnter Data file name(without file extention of.csv)\n")
        TotalSingledata.file <-readline()
        TotalSingledata.file<-paste(TotalSingledata.file,".csv",sep="")
        cnames<-c("subj", "seq","prd", "time", "conc")
        TotalSingledata<-read.csv(TotalSingledata.file,header=TRUE,row.names=NULL,col.names=cnames, sep="\t",dec=".")
        TotalSingledata<-edit(TotalSingledata)
        TotalSingledata<-na.omit(TotalSingledata)
        cat("\n\n")
        show(TotalSingledata)
           return(NCAanalyze(TotalSingledata))
          }                   
else {
  if (pick == 8){
  cat("\n\n")
        cat("\nEnter Data file name(without file extention of.csv)\n")
        TotalSingledata.file <-readline()
        TotalSingledata.file<-paste(TotalSingledata.file,".csv",sep="")
        cnames<-c("subj", "seq","prd", "time", "conc")
        TotalSingledata<-read.csv(TotalSingledata.file,header=TRUE,row.names=NULL,col.names=cnames, sep=":",dec=",")
        TotalSingledata<-edit(TotalSingledata)
        TotalSingledata<-na.omit(TotalSingledata)
        cat("\n\n")
        show(TotalSingledata)
           return(NCAanalyze(TotalSingledata))
           }  
else {
  if (pick == 9){
  cat("\n\n")
        cat("\nEnter Data file name(without file extention of.csv)\n")
        TotalSingledata.file <-readline()
        TotalSingledata.file<-paste(TotalSingledata.file,".csv",sep="")
        cnames<-c("subj", "seq","prd", "time", "conc")
        TotalSingledata<-read.csv(TotalSingledata.file,header=TRUE,row.names=NULL,col.names=cnames, sep=":",dec=".")
        TotalSingledata<-edit(TotalSingledata)
        TotalSingledata<-na.omit(TotalSingledata)
        cat("\n\n")
        show(TotalSingledata)
           return(NCAanalyze(TotalSingledata))
           }              
 else {
  if (pick == 10){
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