### Input assay data Menu for Data Analysis for Single dose; will be called by NCA(), then BANOVA().  ---YJ
###
NCA.BANOVAanalyze<-function(TotalSingledata,Dose,xaxis,yaxis,separateWindows=TRUE,multiple=FALSE)
{

description_NCA()
### move here since v2.5.9 [2013/11/10 AM 01:01:55] -YJ

lin.AUC<-lin.AUC
pAUC<-pAUC               ### for pAUC
lambda_z_calc<-lambda_z_calc
BE_LL<-BE_LL
BE_UL<-BE_UL
dosez<-dosez
Tlastz<-Tlastz
xlabz<-xlabz
ylabz<-ylabz

### file.menu <- c("Linear-up/log-down Trapezoidal Method (default)",
###                "All with Linear Trapezoidal Method")
### pick <- menu(file.menu, title = " << Method Selections for AUC Calculation>> ", graphics=TRUE)
### lin.AUC<<-ifelse(pick==1,FALSE,TRUE)
###
if (multiple) {
with(Multiplentertitle(),{
description_drug()

   Singledata<-split(TotalSingledata,list(TotalSingledata$seq,TotalSingledata$prd))
   Ref<-rbind(Singledata[[1]],Singledata[[4]])
   Refdata<-data.frame(subj=Ref$subj,seq= Ref$seq,prd=Ref$prd,drug=c(1),
                    time=Ref$time,conc=Ref$conc)
    SingleRdata0<-Refdata[ do.call(order,Refdata) ,]
    ## show(SingleRdata0)  ### close this!  YJ
    SingleRdata1<-Refdata[ do.call(order,Refdata) ,]
    SingleRdata1$conc[SingleRdata1$conc == 0] <- NA
    SingleRdata1 <- na.omit(SingleRdata1)

## SingleRdata1-->for select 2-6 points
cat("\n\n")
  
   Test<-rbind(Singledata[[2]],Singledata[[3]])
   Testdata<-data.frame(subj=Test$subj,seq= Test$seq,prd=Test$prd,drug=c(2),
                     time=Test$time,conc=Test$conc)
     SingleTdata0<-Testdata[ do.call(order,Testdata) ,]
     ## show(SingleTdata0)  ### close this!  YJ
     SingleTdata1<-Testdata[ do.call(order,Testdata) ,]
     SingleTdata1$conc[SingleTdata1$conc == 0] <- NA
     SingleTdata1 <- na.omit(SingleTdata1)

SingleRdata<-subset(SingleRdata0,time >=TlastD)
SingleTdata<-subset(SingleTdata0,time >=TlastD)
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
### write the file creations as a function since it will be called frequently. ---YJ
###
create.products_sum(Totalplot)
###

   cat("\n")
###    file.menu <- c("Select 2-6 data points manually",            
###                   "Load previous selection (2-6 data points)",  
###                   "Use Adjusted R sq. (ARS) method",
###                   "Use Akaike information criterion (AIC) method",                     
###                   "Use the Two-Times-Tmax(TTT) method",
###                   "Use TTT and ARS method",
###                   "Use TTT and AIC method")                 
###    cat("\n")               
###    pick <- menu(file.menu, title = "<<Method Selections for Lambda_z Estimation>>", graphics=TRUE)

   if (lambda_z_calc ==5){
      description_pointselect()  
      MultipleNCAselect.BANOVA(Totalplot,SingleRdata1,SingleTdata1,Dose,SingleRdata,SingleTdata,xaxis,yaxis,Tau,TlastD,SingleRdata0,SingleTdata0) 
      go2menu()
   } 
 else {
   if (lambda_z_calc ==6){
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
       
       Tcomdata<-split(comdata,list(comdata$drug))     
       ref_data<-data.frame(subj=Tcomdata[[1]]$subj,time=Tcomdata[[1]]$time,conc=Tcomdata[[1]]$conc,conc_data=Tcomdata[[1]]$conc_data) 
       test_data<-data.frame(subj=Tcomdata[[2]]$subj,time=Tcomdata[[2]]$time,conc=Tcomdata[[2]]$conc,conc_data=Tcomdata[[2]]$conc_data)
   
       rdata.split<-split(ref_data,list(ref_data$subj))
       tdata.split<-split(test_data,list(test_data$subj))
   
       MultipleNCA.BANOVA(Totalplot,Dose,ref_data,test_data,SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis,yaxis,rdata.split,tdata.split,Tau,TlastD,SingleRdata0,SingleTdata0) 
       go2menu()
   } 
 else {
  if (lambda_z_calc == 0){     
       MultipleARS.BANOVA(Dose,xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1,Tau,TlastD,SingleRdata0,SingleTdata0) 
       go2menu()  
    }
  else {
  if (lambda_z_calc == 1){ 
       MultipleAIC_BANOVA(Dose,xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1,Tau,TlastD,SingleRdata0,SingleTdata0)          
       go2menu()
     }
  else {
  if (lambda_z_calc == 2){ 
       MultipleTTT.BANOVA(Dose,xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1,Tau,TlastD,SingleRdata0,SingleTdata0)    
       go2menu()
     } 
  else {
  if (lambda_z_calc == 3){ 
     MultipleTTTARS.BANOVA(Dose,xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1,Tau,TlastD,SingleRdata0,SingleTdata0)    
      go2menu()
    }  
  else {
  if (lambda_z_calc == 4){ 
       MultipleTTTAIC.BANOVA(Dose,xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1,Tau,TlastD,SingleRdata0,SingleTdata0)    
       go2menu()
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
with(entertitle(),{ 
description_drug()

Singledata<-split(TotalSingledata,list(TotalSingledata$seq,TotalSingledata$prd))
Ref<-rbind(Singledata[[1]],Singledata[[4]])
Refdata<-data.frame(subj=Ref$subj,seq= Ref$seq,prd=Ref$prd,drug=c(1),time=Ref$time,conc=Ref$conc)
SingleRdata<-Refdata[ do.call(order,Refdata) ,]
### show(SingleRdata)  ### close this!  YJ
SingleRdata1<-Refdata[ do.call(order,Refdata) ,]
SingleRdata1$conc[SingleRdata1$conc == 0] <- NA
SingleRdata1 <- na.omit(SingleRdata1)
cat("\n\n")
Test<-rbind(Singledata[[2]],Singledata[[3]])
Testdata<-data.frame(subj=Test$subj,seq= Test$seq,prd=Test$prd,drug=c(2),time=Test$time,conc=Test$conc)
SingleTdata<-Testdata[ do.call(order,Testdata) ,]
## show(SingleTdata)  ### close this!  YJ
SingleTdata1<-Testdata[ do.call(order,Testdata) ,]
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
create.products_sum(Totalplot)
###

###    cat("\n")
###    file.menu <- c("Select 2-6 data points manually",
###                   "Load previous selection (2-6 data points)",
###                   "Use Adjusted Rsq. (ARS) method",
###                   "Use Akaike information criterion (AIC) method",                     
###                   "Use the Two-Times-Tmax(TTT) method",
###                   "Use TTT and ARS method",
###                   "Use TTT and AIC method")         
###    cat("\n")               
###    pick <- menu(file.menu, title = "<<  Estimation Methods for Lambda_z >>")

   if (lambda_z_calc == 5){
      description_pointselect()
    ########Reference data
     NCAselect.BANOVA(Totalplot,SingleRdata1,SingleTdata1,Dose,SingleRdata,SingleTdata,xaxis,yaxis)
     go2menu()
     } 

 else {
  if (lambda_z_calc == 6){
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

       Tcomdata<-split(comdata,list(comdata$drug))     
       ref_data<-data.frame(subj=Tcomdata[[1]]$subj,time=Tcomdata[[1]]$time,conc=Tcomdata[[1]]$conc,conc_data=Tcomdata[[1]]$conc_data) 
       test_data<-data.frame(subj=Tcomdata[[2]]$subj,time=Tcomdata[[2]]$time,conc=Tcomdata[[2]]$conc,conc_data=Tcomdata[[2]]$conc_data)
  
       rdata.split<-split(ref_data,list(ref_data$subj))
       tdata.split<-split(test_data,list(test_data$subj))
    
    NCA.BANOVA(Totalplot,Dose,ref_data,test_data,SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis,yaxis,rdata.split,tdata.split)
    go2menu()
  }

 else {
  if (lambda_z_calc == 0){ 
      ARS.BANOVA(Dose,xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
      go2menu()
       }   
  
  else {
  if (lambda_z_calc == 1){ 
      AIC_BANOVA(Dose,xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
      go2menu()
       }           
  
  else {
  if (lambda_z_calc == 2){ 
      TTT.BANOVA(Dose,xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
      go2menu()  
      }
      
  else {
  if (lambda_z_calc == 3){ 
      TTTARS.BANOVA(Dose,xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
      go2menu()  
     }
      
  else {
  if (lambda_z_calc == 4){ 
      TTTAIC.BANOVA(Dose,xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
      go2menu()    
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
