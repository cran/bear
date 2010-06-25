#Input assay data Menu for Data Analysis for Single dose
options(warn=-1)
RepNCAanalyze<-function(TotalSingledata, Dose, xaxis,yaxis, separateWindows=TRUE, MIX=FALSE)
{
description_NCA()

with(entertitle(), {

description_drug()

predata<-split(TotalSingledata,  list(TotalSingledata$prd,TotalSingledata$subj))

code<-NULL
presubj<-NULL
preseq<-NULL
preprd<-NULL
predrug<-NULL
pretime<-NULL
preconc<-NULL
precode<-NULL
for (j in 1:length(predata)){
   j=j
   code[[j]]<-j
 LL<-cbind(subj=predata[[j]]$subj,seq=predata[[j]]$seq,prd=predata[[j]]$prd,drug=predata[[j]]$drug,
           time=predata[[j]]$time,conc=predata[[j]]$conc,code=code[j])

 presubj[[j]]<-c(LL[,1])
 preseq[[j]]<-c(LL[,2])
 preprd[[j]]<-c(LL[,3])
 predrug[[j]]<-c(LL[,4])
 pretime[[j]]<-c(LL[,5])
 preconc[[j]]<-c(LL[,6])
 precode[[j]]<-c(LL[,7])
 
 }
setdata<-data.frame(subj=melt(presubj)$value, seq=melt(preseq)$value,prd=melt(preprd)$value,
                    drug=melt(predrug)$value, time=melt(pretime)$value, conc=melt(preconc)$value,
                    code=melt(precode)$value)
show(setdata)                    
Singledata<-split(setdata, list(setdata$drug))

Refdata<-data.frame(subj=Singledata[[1]]$subj, seq= Singledata[[1]]$seq, prd=Singledata[[1]]$prd,
                    drug=Singledata[[1]]$drug, time=Singledata[[1]]$time, conc=Singledata[[1]]$conc,
                    code=Singledata[[1]]$code)
SingleRdata<-Refdata[ do.call(order, Refdata) ,]
show(SingleRdata)
SingleRdata1<-Refdata[ do.call(order, Refdata) ,]
SingleRdata1$conc[SingleRdata1$conc == 0] <- NA
SingleRdata1 <- na.omit(SingleRdata1)
##SingleRdata1-->for select 3 points
cat("\n\n")
Testdata<-data.frame(subj=Singledata[[2]]$subj, seq=Singledata[[2]]$seq, prd=Singledata[[2]]$prd,
                     drug=Singledata[[2]]$drug, time=Singledata[[2]]$time, conc=Singledata[[2]]$conc,
                     code=Singledata[[2]]$code)
SingleTdata<-Testdata[ do.call(order, Testdata) ,]
show(SingleTdata)
SingleTdata1<-Testdata[ do.call(order, Testdata) ,]
SingleTdata1$conc[SingleTdata1$conc == 0] <- NA
SingleTdata1 <- na.omit(SingleTdata1)
##SingleTdata1-->for select 3 points
cat("\n\n")
#'Total" for NCAplot
Totalplot<- rbind(SingleRdata,SingleTdata)

   cat("\n")
   file.menu <- c("Select 2-4 data points manually",
                  "Load previous selection (2-4 data points)",
                  "Use Adjusted R sq. (ARS) method",
                  "Use Akaike information criterion (AIC) method",
                  "Use the Two-Times-Tmax(TTT) method",
                  "Use TTT and ARS method",
                  "Use TTT and AIC method")
   cat("\n")
   pick <- menu(file.menu, title = "<< Lambda_z options >>")

   if (pick ==1){
     if(MIX){
     RepNCAselect.MIX(Totalplot,SingleRdata1,SingleTdata1,Dose,SingleRdata,SingleTdata,xaxis, yaxis)
     bye()
      }
     else{
     RepNCAselect(Totalplot,SingleRdata1,SingleTdata1, Dose,SingleRdata,SingleTdata,xaxis, yaxis)
     }
   }

 else {
  if (pick == 2){
     description_load()
     comdataname <-readline()
     comdataname<-paste(comdataname,".RData",sep="")
     load(comdataname)
     comdata<-edit(comdata)
     comdata<- na.omit(comdata)
     colnames(comdata)<-list("subj", "time", "conc","conc_data","seq", "prd", "drug","code")
     cat("\n\n")
     description_drug()
     show(comdata)
     save(comdata,file=comdataname)
     cat("\n\n")

      Tcomdata<-split(comdata, list(comdata$drug))
      ref_data<-data.frame(subj=Tcomdata[[1]]$subj,time=Tcomdata[[1]]$time,conc=Tcomdata[[1]]$conc, conc_data=Tcomdata[[1]]$conc_data,
                           seq=Tcomdata[[1]]$seq,prd=Tcomdata[[1]]$prd,drug=Tcomdata[[1]]$drug,code=Tcomdata[[1]]$code)
      test_data<-data.frame(subj=Tcomdata[[2]]$subj,time=Tcomdata[[2]]$time,conc=Tcomdata[[2]]$conc, conc_data=Tcomdata[[2]]$conc_data,
                            seq=Tcomdata[[2]]$seq,prd=Tcomdata[[2]]$prd,drug=Tcomdata[[2]]$drug,code=Tcomdata[[2]]$code)

       rdata.split<-split(ref_data,list(ref_data$code))
       tdata.split<-split(test_data,list(test_data$code))

        if(MIX){
         RepNCA.MIX(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split)
         bye()
          }
         else{
          RepNCA(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split)
        }
      }
 else {
  if (pick == 3){
        if(MIX){
         RepARS.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
         bye()
         }
         else{
          RepARS(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       }
      }
  else {
  if (pick == 4){
        if(MIX){
         RepAIC.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
         bye()
         }
         else{
          Repaic(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       }
     }
  else {
  if (pick == 5){
       if(MIX){
         RepTTT.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
         bye()  
         }
         else{
         RepTTT(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
      }
    }
  else {
  if (pick == 6){
       if(MIX){
         RepTTTARS.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
         bye()  
         }
         else{
          RepTTTARS(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
     }
   }
  else {
  if (pick == 7){
      if(MIX){
         RepTTTAIC.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
         bye()   
         }
         else{
          RepTTTAIC(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
         }
        }
       }
      }
     }
    }
   }
  }
 })
}