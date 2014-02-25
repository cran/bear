### choose separator and decimal type
### re-coded on 2014/02/19
###
NCAcsv<-function(replicated=FALSE, parallel=FALSE, multiple=FALSE)
{
cat("\n")
Fname<-Fname
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
pick <- menu(file.menu, title = " << Separator and decimal formats >> ", graphics=TRUE)
cat("\n\n")
description_import()
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
if (pick<10){
if   (pick == 1){
       TotalSingledata<-read.csv(xx<-file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=",",dec=".")}
  if (pick == 2){
       TotalSingledata<-read.csv(xx<-file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=";",dec=",")}
  if (pick == 3){
       TotalSingledata<-read.csv(xx<-file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=";",dec=".")}
  if (pick == 4){
       TotalSingledata<-read.csv(xx<-file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=" ",dec=",")}
  if (pick == 5){
       TotalSingledata<-read.csv(xx<-file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=" ",dec=".")}
  if (pick == 6){
       TotalSingledata<-read.csv(xx<-file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep="\t",dec=",")}
  if (pick == 7){
       TotalSingledata<-read.csv(xx<-file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep="\t",dec=".")}
  if (pick == 8){
       TotalSingledata<-read.csv(xx<-file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=":",dec=",")}
  if (pick == 9){
       TotalSingledata<-read.csv(xx<-file.choose(),header=TRUE,row.names=NULL,col.names=cnames, sep=":",dec=".")}

TotalSingledata<-edit(TotalSingledata)
Fname<<- basename(xx) ### this will not work with 'Fname<<- basename(TotalSingledata)' 
### TotalSingledata<-na.omit(TotalSingledata)   ### v2.6.1
cat("\n\n")
show(TotalSingledata)
if(parallel){
     if(multiple){
         icd.check(TotalSingledata,parallel=TRUE,multiple=TRUE,replicated=FALSE,NCA.only=TRUE)
       }
      else{
         icd.check(TotalSingledata,parallel=TRUE,multiple=FALSE,replicated=FALSE,NCA.only=TRUE)
       }
   }
else{ 
    if (replicated){
         icd.check(TotalSingledata,parallel=FALSE,multiple=FALSE,replicated=TRUE,NCA.only=TRUE)
     }
   else{
      if(multiple){
         icd.check(TotalSingledata,parallel=FALSE,multiple=TRUE,replicated=FALSE,NCA.only=TRUE)
          }
       else{ 
         icd.check(TotalSingledata,parallel=FALSE,multiple=FALSE,replicated=FALSE,NCA.only=TRUE)
         } 
   }  
 }
}
else {   ### back to the upper menu 
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
}  ### end of NCAcsv.()  ###