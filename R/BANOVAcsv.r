### choose separator and decimal type; should this be locale specific?  --YJ
###
### NCAdata(), NCAcsv() -> for ; NCA only' data input;
### BANOVAdata(), BANOVAcsv() -> for 'statistical analysis only' data input;
### NCA.BANOVAdata(), NCA.BANOVAcsv() for 'NCA -> statistical analysis' data input.
### re-coded on 2014/02/19
###
BANOVAcsv<-function(replicated=FALSE, parallel=FALSE, multiple=FALSE)
{
cat("\n")

Fname<-Fname
pAUC<-pAUC

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
cat("\n\n")
description_import()
if(parallel){
  if(multiple){
     if(pAUC){
     cnames<-c("subj","drug","Cmax_ss","AUCtau_ss","partAUC","lnCmax_ss","lnAUCtau_ss","lnpAUC")}
     else{
     cnames<-c("subj","drug","Cmax_ss","AUCtau_ss","lnCmax_ss","lnAUCtau_ss")}
  }
  else{
     if(pAUC){
     cnames<-c("subj","drug","Cmax", "AUC0t","partAUC","AUC0INF","lnCmax","lnAUC0t","lnAUC0INF","lnpAUC")}
     else{
     cnames<-c("subj","drug","Cmax", "AUC0t", "AUC0INF","lnCmax","lnAUC0t","lnAUC0INF")}
  }
} 
else{ 
  if(multiple){
     if(pAUC){
     cnames<-c("subj","drug","seq", "prd","Cmax_ss","AUCtau_ss","partAUC","lnCmax_ss","lnAUCtau_ss","lnpAUC")}
     else{
     cnames<-c("subj","drug","seq", "prd","Cmax_ss","AUCtau_ss","lnCmax_ss","lnAUCtau_ss")}
   }
   else{
     if(pAUC){
     cnames<-c("subj","drug","seq", "prd","Cmax", "AUC0t","partAUC","AUC0INF","lnCmax","lnAUC0t","lnAUC0INF","lnpAUC")}
     else{
     cnames<-c("subj","drug","seq", "prd","Cmax", "AUC0t", "AUC0INF","lnCmax","lnAUC0t","lnAUC0INF")}
   }
} 
pick <- menu(file.menu, title = " << Separator and decimal formats >> ", graphics=TRUE)
if (pick<10){
  if (pick == 1){ #### BANOVAcsv() uses 'TotalData' instead of 'TotalSingledata' as NCAcsv() & NCA.BANOVAcsv() did.
       TotalData<-read.csv(xx<-file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=",",dec=".")}
  if (pick == 2){
       TotalData<-read.csv(xx<-file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=";",dec=",")}
  if (pick == 3){
       TotalData<-read.csv(xx<-file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=";",dec=".")}
  if (pick == 4){
       TotalData<-read.csv(xx<-file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=" ",dec=",")}
  if (pick == 5){
       TotalData<-read.csv(xx<-file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=" ",dec=".")}
  if (pick == 6){
       TotalData<-read.csv(xx<-file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep="\t",dec=",")}
  if (pick == 7){
       TotalData<-read.csv(xx<-file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep="\t",dec=".")}
  if (pick == 8){
       TotalData<-read.csv(xx<-file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=":",dec=",")}
  if (pick == 9){
       TotalData<-read.csv(xx<-file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=":",dec=".")}

     TotalData<-edit(TotalData)
     Fname<<-basename(xx)
     TotalData<-na.omit(TotalData)
     cat("\n\n")
     show(TotalData)
         
     if(parallel){
      if(multiple){
       MultipleParaMIXanalyze(TotalData)
       MultipleParaMIXmenu() 
      }
      else{
       ParaMIXanalyze(TotalData)
       ParaMIXmenu() 
      }
     }
     else{ 
     if(replicated){
      RepMIXanalyze(TotalData)
      RepMIXmenu() 
       }
      else{
      if(multiple){
        MultipleBANOVAanalyze(TotalData)
        MultipleBANOVAmenu() 
        }
        else{
        BANOVAanalyze(TotalData)
        BANOVAmenu() 
        }
     }
    }###
  }
else {   ### back to the upper menu 
  if (pick == 10){
      if(multiple){
        Multiplestat1menu()
             }
       else{
         Multiplestatmenu()
            }   
           }
 }
} ### end of BANOVAcsv()  ###