### Input assay data Menu for Data Analysis for Single dose; will be called by NCA(), then BANOVA().  ---YJ
###
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
    ## show(SingleRdata0)  ### close this!  YJ
    SingleRdata1<-Refdata[ do.call(order, Refdata) ,]
    SingleRdata1$conc[SingleRdata1$conc == 0] <- NA
    SingleRdata1 <- na.omit(SingleRdata1)

## SingleRdata1-->for select 2-6 points
cat("\n\n")
  
   Test<-rbind(Singledata[[2]],Singledata[[3]])
   Testdata<-data.frame(subj=Test$subj, seq= Test$seq, prd=Test$prd, drug=c(2), 
                     time=Test$time, conc=Test$conc)
     SingleTdata0<-Testdata[ do.call(order, Testdata) ,]
     ## show(SingleTdata0)  ### close this!  YJ
     SingleTdata1<-Testdata[ do.call(order, Testdata) ,]
     SingleTdata1$conc[SingleTdata1$conc == 0] <- NA
     SingleTdata1 <- na.omit(SingleTdata1)

SingleRdata<-subset(SingleRdata0, time >=TlastD)
SingleTdata<-subset(SingleTdata0, time >=TlastD)
###
### check icd earlier
###
R_subj_no<-SingleRdata$subj
R_subj_no<-unique(R_subj_no)
T_subj_no<-SingleTdata$subj
T_subj_no<-unique(T_subj_no)

if(!identical(R_subj_no,T_subj_no)) {       ### use identical() here to compare two objects to see if they are the same. --YJ
   cat("\n\n The Subject codes of taking Ref. product are as follows:\n")
   cat("-------------------------------------------------------------\n")
   show(R_subj_no)
   cat("\n\n The Subject codes of taking Tets product are as follows:\n")
   cat("-------------------------------------------------------------\n")
   show(T_subj_no)
   icd.check()}

## SingleTdata1-->for select 2-6 points
cat("\n\n")
# "Total" for NCAplot
Totalplot<- rbind(SingleRdata0,SingleTdata0)
    ###
    ### prepare for save sum_all_by_product.csv here... YJ
    TKK<-Totalplot
    ## TKK<-TKK[ do.call(order, TKK) ,]
    ### dump ref data
    SS_ref<-subset(TKK,TKK$drug==1)                                                                                                    
    timeXX<-SS_ref$time                                                                                                                
    timeXX<-unique(timeXX)                                                                                                             
    product_F<-factor(SS_ref$time)   # get mean & sd by time as factor                                                                 
    mean.conc.ref<-tapply(SS_ref$conc,product_F, mean)                                                                                 
    SD.conc.ref<-tapply(SS_ref$conc,product_F, sd)                                                                                     
    SS_test<-subset(TKK,TKK$drug==2)
    timeZZ<-SS_test$time                                                                                                                
    timeZZ<-unique(timeZZ)                                                                                                             
    product_F<-factor(SS_test$time)                                                                                                    
    mean.conc.test<-tapply(SS_test$conc,product_F, mean)                                                                               
    SD.conc.test<-tapply(SS_test$conc,product_F, sd)                                                                                   
    sum_all_Ref<-data.frame(time=timeXX,mean_ref=mean.conc.ref,SD_ref=SD.conc.ref)
    sum_all_test<-data.frame(time=timeZZ,mean_test=mean.conc.test,SD_ref=SD.conc.test)
    write.csv(sum_all_Ref,file="sum_all_Ref.csv",row.names=FALSE)  
    write.csv(sum_all_Ref,file="sum_all_test.csv",row.names=FALSE) ### <-- it's working!!  YJ
    ###
    ### show(sum_all_by_product)  ### try to write.csv() and then read.csv() in NCAoutput() again and show() it,     
    ### cat("\n\n"); readline()   ### finally file.remove("sum_all_by_product.csv") to remove this file.
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
   pick <- menu(file.menu, title = "<< Estimation Methods for Lambda_z >>")

   if (pick ==1){
     description_pointselect()  
     MultipleNCAselect.BANOVA(Totalplot,SingleRdata1,SingleTdata1, Dose,SingleRdata,SingleTdata,xaxis, yaxis, Tau, TlastD,SingleRdata0,SingleTdata0) 
      bye()
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
### show(SingleRdata)  ### close this!  YJ
SingleRdata1<-Refdata[ do.call(order, Refdata) ,]
SingleRdata1$conc[SingleRdata1$conc == 0] <- NA
SingleRdata1 <- na.omit(SingleRdata1)
cat("\n\n")
Test<-rbind(Singledata[[2]],Singledata[[3]])
Testdata<-data.frame(subj=Test$subj, seq= Test$seq, prd=Test$prd, drug=c(2), time=Test$time, conc=Test$conc)
SingleTdata<-Testdata[ do.call(order, Testdata) ,]
## show(SingleTdata)  ### close this!  YJ
SingleTdata1<-Testdata[ do.call(order, Testdata) ,]
SingleTdata1$conc[SingleTdata1$conc == 0] <- NA
SingleTdata1 <- na.omit(SingleTdata1)
###
### check icd earlier
###
R_subj_no<-SingleRdata$subj
R_subj_no<-unique(R_subj_no)
T_subj_no<-SingleTdata$subj
T_subj_no<-unique(T_subj_no)

if(!identical(R_subj_no,T_subj_no)) {       ### use identical() here to compare two objects to see if they are the same. --YJ
   cat("\n\n The Subject codes of taking Ref. product are as follows:\n")
   cat("-------------------------------------------------------------\n")
   show(R_subj_no)
   cat("\n\n The Subject codes of taking Tets product are as follows:\n")
   cat("-------------------------------------------------------------\n")
   show(T_subj_no)
   icd.check()}

#'Total" for NCAplot
Totalplot<- rbind(SingleRdata,SingleTdata)
   ###
   ### prepare for save sum_all_by_product.csv here... YJ
   TKK<-Totalplot
   ### TKK<-TKK[ do.call(order, TKK) ,]
   ### dump ref data
   SS_ref<-subset(TKK,TKK$drug==1)                                                                                                    
   timeXX<-SS_ref$time                                                                                                                
   timeXX<-unique(timeXX)
   timeXX<-sort(timeXX)
   product_F<-factor(SS_ref$time)   # get mean & sd by time as factor                                                                 
   mean.conc.ref<-tapply(SS_ref$conc,product_F, mean)                                                                                 
   SD.conc.ref<-tapply(SS_ref$conc,product_F, sd)                                                                                     
   SS_test<-subset(TKK,TKK$drug==2)
   timeZZ<-SS_test$time                                                                                                                
   timeZZ<-unique(timeZZ)
   timeZZ<-sort(timeZZ)
   product_F<-factor(SS_test$time)                                                                                                    
   mean.conc.test<-tapply(SS_test$conc,product_F, mean)                                                                               
   SD.conc.test<-tapply(SS_test$conc,product_F, sd)                                                                                   
   sum_all_Ref<-data.frame(time=timeXX,mean_ref=mean.conc.ref,SD_ref=SD.conc.ref)
   sum_all_test<-data.frame(time=timeZZ,mean_test=mean.conc.test,SD_ref=SD.conc.test)
   write.csv(sum_all_Ref,file="sum_all_Ref.csv",row.names=FALSE)  
   write.csv(sum_all_Ref,file="sum_all_test.csv",row.names=FALSE) ### <-- it's working!!  YJ
   ###
   ### show(sum_all_by_product)  ### try to write.csv() and then read.csv() again and show() it on NCAoutput(), 
   ### cat("\n\n"); readline()   ### finally file.remove("sum_all_by_product.csv") after show().
   ###

   cat("\n")
   file.menu <- c("Select 2-6 data points manually",
                  "Load previous selection (2-6 data points)",
                  "Use Adjusted Rsq. (ARS) method",
                  "Use Akaike information criterion (AIC) method",                     
                  "Use the Two-Times-Tmax(TTT) method",
                  "Use TTT and ARS method",
                  "Use TTT and AIC method")         
   cat("\n")               
   pick <- menu(file.menu, title = "<<  Estimation Methods for Lambda_z >>")

   if (pick ==1){
      description_pointselect()
    ########Reference data
     NCAselect.BANOVA(Totalplot,SingleRdata1,SingleTdata1,Dose,SingleRdata,SingleTdata,xaxis, yaxis)
     bye()
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
 







