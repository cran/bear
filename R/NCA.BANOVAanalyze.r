#Input assay data Menu for Data Analysis for Single dose
NCA.BANOVAanalyze<-function(TotalSingledata, Dose, xaxis,yaxis,separateWindows=TRUE, multiple=FALSE)
{
description_NCA()
if (multiple) {
with(Multiplentertitle(),{
description_drug()

   Singledata<-split(TotalSingledata, list(TotalSingledata$seq, TotalSingledata$prd))
   Ref<-rbind(Singledata[[1]],Singledata[[4]])
   Refdata<-data.frame(subj=Ref$subj, seq= Ref$seq, prd=Ref$prd, drug=c(1), 
                    time=Ref$time, conc=Ref$conc)
    SingleRdata0<-Refdata[ do.call(order, Refdata) ,]
    show(SingleRdata0)
    SingleRdata1<-Refdata[ do.call(order, Refdata) ,]
    SingleRdata1$conc[SingleRdata1$conc == 0] <- NA
    SingleRdata1 <- na.omit(SingleRdata1)

##SingleRdata1-->for select 3 points
cat("\n\n")
  
   Test<-rbind(Singledata[[2]],Singledata[[3]])
   Testdata<-data.frame(subj=Test$subj, seq= Test$seq, prd=Test$prd, drug=c(2), 
                     time=Test$time, conc=Test$conc)
     SingleTdata0<-Testdata[ do.call(order, Testdata) ,]
     show(SingleTdata0)
     SingleTdata1<-Testdata[ do.call(order, Testdata) ,]
     SingleTdata1$conc[SingleTdata1$conc == 0] <- NA
     SingleTdata1 <- na.omit(SingleTdata1)

SingleRdata<-subset(SingleRdata0, time >=TlastD)
SingleTdata<-subset(SingleTdata0, time >=TlastD)

##SingleTdata1-->for select 3 points
cat("\n\n")
#'Total" for NCAplot
Totalplot<- rbind(SingleRdata0,SingleTdata0)

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
     MultipleNCAselect.BANOVA(Totalplot,SingleRdata1,SingleTdata1, Dose,SingleRdata,SingleTdata,xaxis, yaxis, Tau, TlastD,SingleRdata0,SingleTdata0) 
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
     colnames(comdata)<-list("subj","time","conc","conc_data","drug")
     cat("\n\n")
     description_drug()
     show(comdata)
     save(comdata,file=comdataname)
     cat("\n\n")
       
      Tcomdata<-split(comdata, list(comdata$drug))     
      ref_data<-data.frame(subj=Tcomdata[[1]]$subj,time=Tcomdata[[1]]$time,conc=Tcomdata[[1]]$conc,conc_data=Tcomdata[[1]]$conc_data) 
      test_data<-data.frame(subj=Tcomdata[[2]]$subj,time=Tcomdata[[2]]$time,conc=Tcomdata[[2]]$conc,conc_data=Tcomdata[[2]]$conc_data)
   
       rdata.split<-split(ref_data,list(ref_data$subj))
       tdata.split<-split(test_data,list(test_data$subj))
   
      MultipleNCA.BANOVA(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split,Tau, TlastD,SingleRdata0,SingleTdata0) 
       bye()
   } 
 else {
  if (pick == 3){     
     MultipleARS.BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0) 
      bye()  
    }
  else {
  if (pick == 4){ 
       MultipleAIC_BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0)          
        bye()
     }
  else {
  if (pick == 5){ 
     MultipleTTT.BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0)    
      bye()
     } 
  else {
  if (pick == 6){ 
     MultipleTTTARS.BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0)    
      bye()
    }  
  else {
  if (pick == 7){ 
       MultipleTTTAIC.BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0)    
       bye()
         }
        }
       }
      } 
     }
    }
   } 
  })
}

else{
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
   file.menu <- c("Select 2-4 data points manually",
                  "Load previous selection (2-4 data points)",
                  "Use Adjusted Rsq. (ARS) method",
                  "Use Akaike information criterion (AIC) method",                     
                  "Use the Two-Times-Tmax(TTT) method",
                  "Use TTT and ARS method",
                  "Use TTT and AIC method")         
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
     colnames(comdata)<-list("subj","time","conc","conc_data","drug")
     cat("\n\n")
     
       description_drug()
     
       show(comdata)
       save(comdata,file=comdataname)
       cat("\n\n")
       
       Tcomdata<-split(comdata, list(comdata$drug))     
       ref_data<-data.frame(subj=Tcomdata[[1]]$subj,time=Tcomdata[[1]]$time,conc=Tcomdata[[1]]$conc,conc_data=Tcomdata[[1]]$conc_data) 
       test_data<-data.frame(subj=Tcomdata[[2]]$subj,time=Tcomdata[[2]]$time,conc=Tcomdata[[2]]$conc,conc_data=Tcomdata[[2]]$conc_data)
  
       rdata.split<-split(ref_data,list(ref_data$subj))
       tdata.split<-split(test_data,list(test_data$subj))
    
    NCA.BANOVA(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split)
    bye()
  }

 else {
  if (pick == 3){ 
     ARS.BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
      bye()
       }   
  
  else {
  if (pick == 4){ 
     AIC_BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
      bye()
       }           
  
  else {
  if (pick == 5){ 
     TTT.BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       bye()  
      }
      
  else {
  if (pick == 6){ 
     TTTARS.BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       bye()  
     }
      
  else {
  if (pick == 7){ 
     TTTAIC.BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       bye()    
         }  
         }
        }
       } 
      }
     }
    } 
  })
 }
}       
 







