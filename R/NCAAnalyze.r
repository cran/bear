###
###  Input assay data Menu for Data Analysis for Single dose (don't analyze anything with this?  YJ)
###  This script has two parts: multiple & else (i.e., single); all are non-replicate; if replicate -> RepNCAanalyze.r  -YJ
### 


NCAanalyze<-function(TotalSingledata,Dose,Tau,TlastD,xaxis,yaxis,separateWindows=TRUE,
                     parallel=FALSE, MIX=FALSE, multiple=FALSE)
{

options(warn=-1)

description_NCA()
if (multiple) {                                    ### first come with multiple. -YJ
with(Multiplentertitle(),{
description_drug()

  if(parallel){
   Singledata<-split(TotalSingledata, list(TotalSingledata$drug))
   Ref<-Singledata[[1]]
   Refdata<-data.frame(subj=Ref$subj,drug=Ref$drug,time=Ref$time, conc=Ref$conc)
    }
   else{
   Singledata<-split(TotalSingledata, list(TotalSingledata$seq, TotalSingledata$prd))
   Ref<-rbind(Singledata[[1]],Singledata[[4]])
   Refdata<-data.frame(subj=Ref$subj, seq= Ref$seq, prd=Ref$prd, drug=c(1), 
                    time=Ref$time, conc=Ref$conc)
    }
    SingleRdata0<-Refdata[ do.call(order, Refdata) ,]
    show(SingleRdata0)
    SingleRdata1<-Refdata[ do.call(order, Refdata) ,]
    SingleRdata1$conc[SingleRdata1$conc == 0] <- NA
    SingleRdata1 <- na.omit(SingleRdata1)

##SingleRdata1-->for select 2-6 points
cat("\n\n")
   if(parallel){
   Test<-rbind(Singledata[[2]])
   Testdata<-data.frame(subj=Test$subj, drug=Test$drug, time=Test$time, conc=Test$conc)
    }
   else{
   Test<-rbind(Singledata[[2]],Singledata[[3]])
   Testdata<-data.frame(subj=Test$subj, seq= Test$seq, prd=Test$prd, drug=c(2), 
                     time=Test$time, conc=Test$conc)
   }
     SingleTdata0<-Testdata[ do.call(order, Testdata) ,]
     show(SingleTdata0)
     SingleTdata1<-Testdata[ do.call(order, Testdata) ,]
     SingleTdata1$conc[SingleTdata1$conc == 0] <- NA
     SingleTdata1 <- na.omit(SingleTdata1)

SingleRdata<-subset(SingleRdata0, time >=TlastD)
SingleTdata<-subset(SingleTdata0, time >=TlastD)

##SingleTdata1-->for select 2-6 points
cat("\n\n")
#'Total" for NCAplot
Totalplot<- rbind(SingleRdata0,SingleTdata0)
###
create.products_sum(Totalplot)
###

   cat("\n")
   file.menu <- c("Select 2-6 data points manually",            
                  "Load previous selection (.Rdata file)",  
                  "Use Adjusted R sq. (ARS) method",
                  "Use Akaike information criterion (AIC) method",                     
                  "Use the Two-Times-Tmax(TTT) method",
                  "Use TTT and ARS method",
                  "Use TTT and AIC method")                 
   cat("\n")               
   pick <- menu(file.menu, title = "<< Estimation Methods for Lambda_z >>", graphics=TRUE)

   if (pick ==1){
     description_pointselect()
     if(parallel){
      if(MIX){
      MultipleParaNCAselect.MIX(Totalplot,SingleRdata1,SingleTdata1,Dose,SingleRdata,SingleTdata,xaxis, yaxis, Tau, TlastD,SingleRdata0,SingleTdata0) 
      bye()
      }
      else{
       MultipleParaNCAselect(Totalplot,SingleRdata1,SingleTdata1, Dose,SingleRdata,SingleTdata,xaxis, yaxis, Tau, TlastD,SingleRdata0,SingleTdata0)  
       }
     }
     else{
     MultipleNCAselect(Totalplot,SingleRdata1,SingleTdata1, Dose,SingleRdata,SingleTdata,xaxis, yaxis, Tau, TlastD,SingleRdata0,SingleTdata0) 
     }
   } 
 else {
  if (pick == 2){
     description_load()
     ##  comdataname <-readline()
     ##  comdataname<-paste(comdataname,".RData",sep="")
     ##  load(comdataname)
     comdata<-readRDS(file.choose())
     comdata<-edit(comdata)
     comdata<- na.omit(comdata)
     colnames(comdata)<-list("subj","time","conc","conc_data","drug")
     cat("\n\n")
     description_drug()
     show(comdata)
##
## no need to save it back again!  since this file has been created previously.-- YJ
##
##     cat("\n\n Enter the file name to be saved (no extension!):\n")
##     comdataname <-readline()
##     comdataname<-paste(comdataname,".RData",sep="")
##     save(comdata,file=comdataname)
     cat("\n\n")
       
      Tcomdata<-split(comdata, list(comdata$drug))     
      ref_data<-data.frame(subj=Tcomdata[[1]]$subj,time=Tcomdata[[1]]$time,conc=Tcomdata[[1]]$conc,conc_data=Tcomdata[[1]]$conc_data) 
      test_data<-data.frame(subj=Tcomdata[[2]]$subj,time=Tcomdata[[2]]$time,conc=Tcomdata[[2]]$conc,conc_data=Tcomdata[[2]]$conc_data)
   
       rdata.split<-split(ref_data,list(ref_data$subj))
       tdata.split<-split(test_data,list(test_data$subj))
   
     if(parallel){
      if(MIX){
      MultipleParaNCA.MIX(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split, Tau, TlastD,SingleRdata0,SingleTdata0)  
      bye()
      }
      else{
      MultipleParaNCA(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split, Tau, TlastD,SingleRdata0,SingleTdata0) 
       }
     }
     else{
      MultipleNCA(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split, Tau, TlastD,SingleRdata0,SingleTdata0)  
    }
   } 
 else {
  if (pick == 3){ 
    if(parallel){
     if(MIX){
     MultipleParaARS.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0)  
       bye()
      }
      else{
     MultipleParaARS(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0) 
       }
     }
     else{
     MultipleARS(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0) 
       }   
    }
  else {
  if (pick == 4){ 
     if(parallel){
      if(MIX){
      MultipleParaAIC.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0) 
       bye()
      }
      else{
       MultipleParaAIC(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0) 
       }
     }
     else{
       Multipleaic(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0) 
       }           
     }
  else {
  if (pick == 5){ 
     if(parallel){
       if(MIX){
     MultipleParaTTT.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0)  
       bye()
      }
      else{
      MultipleParaTTT(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0) 
       }
     }
     else{
     MultipleTTT(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0)    
      }
     } 
  else {
  if (pick == 6){ 
     if(parallel){
       if(MIX){
       MultipleParaTTTARS.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0) 
       bye()
      }
      else{
       MultipleParaTTTARS(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0) 
        }
     }
     else{
     MultipleTTTARS(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0)    
     }
    }  
  else {
  if (pick == 7){ 
     if(parallel){
       if(MIX){
      MultipleParaTTTAIC.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0)  
       bye()
      }
      else{
        MultipleParaTTTAIC(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0) 
       }
     }
     else{
       MultipleTTTAIC(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0)    
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

else{                                             ### then here come with single. -YJ
with(entertitle(),{
description_drug()

if(parallel){
Singledata<-split(TotalSingledata, list(TotalSingledata$drug))
Ref<-Singledata[[1]]
Refdata<-data.frame(subj=Ref$subj,drug=Ref$drug,time=Ref$time, conc=Ref$conc)
 }
else{
Singledata<-split(TotalSingledata, list(TotalSingledata$seq, TotalSingledata$prd))
Ref<-rbind(Singledata[[1]],Singledata[[4]])
Refdata<-data.frame(subj=Ref$subj, seq= Ref$seq, prd=Ref$prd, drug=c(1), 
                    time=Ref$time, conc=Ref$conc)
}
SingleRdata<-Refdata[ do.call(order, Refdata) ,]
show(SingleRdata)
SingleRdata1<-Refdata[ do.call(order, Refdata) ,]
SingleRdata1$conc[SingleRdata1$conc == 0] <- NA
SingleRdata1 <- na.omit(SingleRdata1)

##SingleRdata1-->for select 2-6 points
cat("\n\n")
if(parallel){
Test<-rbind(Singledata[[2]])
Testdata<-data.frame(subj=Test$subj, drug=Test$drug, time=Test$time, conc=Test$conc)
 }
else{
Test<-rbind(Singledata[[2]],Singledata[[3]])
Testdata<-data.frame(subj=Test$subj, seq= Test$seq, prd=Test$prd, drug=c(2), 
                     time=Test$time, conc=Test$conc)
}
SingleTdata<-Testdata[ do.call(order, Testdata) ,]
show(SingleTdata)
SingleTdata1<-Testdata[ do.call(order, Testdata) ,]
SingleTdata1$conc[SingleTdata1$conc == 0] <- NA
SingleTdata1 <- na.omit(SingleTdata1)

##SingleTdata1-->for select 2-6 points
cat("\n\n")
#'Total" for NCAplot
Totalplot<- rbind(SingleRdata,SingleTdata)
###
create.products_sum(Totalplot)
###
   cat("\n")
   file.menu <- c("Select 2-6 data points manually",            
                  "Load previous selection (2-6 data points)",  
                  "Use Adjusted R sq. (ARS) method",
                  "Use Akaike information criterion (AIC) method",                     
                  "Use the Two-Times-Tmax(TTT) method",
                  "Use TTT and ARS method",
                  "Use TTT and AIC method")                 
   cat("\n")               
   pick <- menu(file.menu, title = "<< Estimation Methods for Lambda_z >>", graphics=TRUE)

   if (pick ==1){
     description_pointselect()
     if(parallel){
      if(MIX){
      ParaNCAselect.MIX(Totalplot,SingleRdata1,SingleTdata1,Dose,SingleRdata,SingleTdata,xaxis, yaxis)
      bye()
      }
      else{
       ParaNCAselect(Totalplot,SingleRdata1,SingleTdata1, Dose,SingleRdata,SingleTdata,xaxis, yaxis) 
       }
     }
     else{
     NCAselect(Totalplot,SingleRdata1,SingleTdata1, Dose,SingleRdata,SingleTdata,xaxis, yaxis) 
     }
   } 
 else {
  if (pick == 2){
     description_load()
     ##  comdataname <-readline()
     ##  comdataname<-paste(comdataname,".RData",sep="")
     ##  load(comdataname)
     comdata<-readRDS(file.choose())
     comdata<-edit(comdata)
     comdata<- na.omit(comdata)
     colnames(comdata)<-list("subj","time","conc","conc_data","drug")
     cat("\n\n")
     description_drug()
     show(comdata)
##
## no need to save it again!  since this file has been created previously.-- YJ
##
##     cat("\n\n Enter the file name to be saved (no extension!):\n")
##     comdataname <-readline()
##     comdataname<-paste(comdataname,".RData",sep="")
##     save(comdata,file=comdataname)
     cat("\n\n")
       
      Tcomdata<-split(comdata, list(comdata$drug))     
      ref_data<-data.frame(subj=Tcomdata[[1]]$subj,time=Tcomdata[[1]]$time,conc=Tcomdata[[1]]$conc,conc_data=Tcomdata[[1]]$conc_data) 
      test_data<-data.frame(subj=Tcomdata[[2]]$subj,time=Tcomdata[[2]]$time,conc=Tcomdata[[2]]$conc,conc_data=Tcomdata[[2]]$conc_data)
   
      rdata.split<-split(ref_data,list(ref_data$subj))
      tdata.split<-split(test_data,list(test_data$subj))
   
     if(parallel){
      if(MIX){
      ParaNCA.MIX(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split) 
      bye()
      }
      else{
      ParaNCA(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split) 
       }
     }
     else{
      NCA(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split) 
    }
   } 
 else {
  if (pick == 3){ 
    if(parallel){
     if(MIX){
     ParaARS.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)  
       bye()
      }
      else{
     ParaARS(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1) 
       }
     }
     else{
     ARS(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1) 
       }   
    }
  else {
  if (pick == 4){ 
     if(parallel){
      if(MIX){
      ParaAIC.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1) 
       bye()
      }
      else{
       ParaAIC(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1) 
       }
     }
     else{
     aic(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1) 
       }           
     }
  else {
  if (pick == 5){ 
     if(parallel){
       if(MIX){
     ParaTTT.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)  
       bye()
      }
      else{
      ParaTTT(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1) 
       }
     }
     else{
     TTT(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)    
      }
     } 
  else {
  if (pick == 6){ 
     if(parallel){
       if(MIX){
       ParaTTTARS.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1) 
       bye()
      }
      else{
       ParaTTTARS(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1) 
        }
     }
     else{
     TTTARS(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)    
     }
    }  
  else {
  if (pick == 7){ 
     if(parallel){
       if(MIX){
      ParaTTTAIC.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)  
       bye()
      }
      else{
        ParaTTTAIC(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1) 
       }
     }
     else{
     TTTAIC(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)    
          }  
         }
        }
       }
      } 
     }
    }
   } 
  } )
 }
} 
       
  



