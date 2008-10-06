#Input assay data Menu for Data Analysis for Single dose
library(reshape)
library(nlme)
options(warn=-1)

NCA.BANOVAanalyze<-function(TotalSingledata, Dose, xaxis,yaxis,separateWindows=TRUE)
{
description_NCA()

with(entertitle(), { 
description_drug()

Singledata<-split(TotalSingledata, list(TotalSingledata$seq, TotalSingledata$prd))
Ref<-rbind(Singledata[[1]],Singledata[[4]])
Refdata<-data.frame(subj=Ref$subj, seq= Ref$seq, prd=Ref$prd, drug=c(1), time=Ref$time, conc=Ref$conc)
SingleRdata<-Refdata[ do.call(order, Refdata) ,]
show(SingleRdata)
SingleRdata1<-Refdata[ do.call(order, Refdata) ,]
SingleRdata1$conc[SingleRdata1$conc == 0] <- NA
SingleRdata1 <- na.omit(SingleRdata1)
cat("\n\n")
Test<-rbind(Singledata[[2]],Singledata[[3]])
Testdata<-data.frame(subj=Test$subj, seq= Test$seq, prd=Test$prd, drug=c(2), time=Test$time, conc=Test$conc)
SingleTdata<-Testdata[ do.call(order, Testdata) ,]
show(SingleTdata)
SingleTdata1<-Testdata[ do.call(order, Testdata) ,]
SingleTdata1$conc[SingleTdata1$conc == 0] <- NA
SingleTdata1 <- na.omit(SingleTdata1)
#'Total" for NCAplot
Totalplot<- rbind(SingleRdata,SingleTdata)

   cat("\n")
   file.menu <- c("Select the exact 3 data points manually",
                  "Load previous selection (the exact 3 data points)",
                  "Use Adjusted Rsq. (ARS) method",
                  "Use the Two-Times-Tmax(TTT) method")
   cat("\n")               
   pick <- menu(file.menu, title = "<< Lambda_z options >>")

   if (pick ==1){
    ########Reference data
     NCAselect.BANOVA(Totalplot,SingleRdata1,SingleTdata1,Dose,SingleRdata,SingleTdata,xaxis, yaxis)
     bye()
     } 

 else {
  if (pick == 2){
     description_load()
     comdataname <-readline()
     comdataname<-paste(comdataname,".RData",sep="")
     load(comdataname)
     comdata<-edit(comdata)
     comdata<- na.omit(comdata)
     colnames(comdata)<-list("subj","time","conc","drug")
     cat("\n\n")
     
       description_drug()
     
       show(comdata)
       save(comdata,file=comdataname)
       cat("\n\n")
       
       Tcomdata<-split(comdata, list(comdata$drug))     
       ref_data<-data.frame(subj=Tcomdata[[1]]$subj,time=Tcomdata[[1]]$time,conc=Tcomdata[[1]]$conc) 
       test_data<-data.frame(subj=Tcomdata[[2]]$subj,time=Tcomdata[[2]]$time,conc=Tcomdata[[2]]$conc)
  
    NCA.BANOVA(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis)
    bye()
  }
 
 else {
  if (pick == 3){ 
     ARS.BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
      bye()
     }
 
 else {
  if (pick == 4){ 
       TTT.BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       bye()
      }           
    } 
   }
  }
 })
}      
 







