##
## will be called by NCA.BANOVAmenu(); demo fro "NCA -> ANOVA" (both) ONLY
##
demomenu1<-function(replicated=FALSE, parallel=FALSE, multiple=FALSE)
{
Demo<-Demo  ## set Demo as Global see go.r
Demo<<-TRUE
Replicateddata<-NULL
MultipleParadata<-NULL
Paralleldata<-NULL
Multipledata<-NULL
TotalSingleData<-NULL

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
IndivDP_output<-IndivDP_output

### file.menu <- c("Linear-up/log-down Trapezoidal Method (default)",
###                "All with Linear Trapezoidal Method")
### pick <- menu(file.menu, title = " << Method Selections for AUC Calculation>> ", graphics=TRUE)
### lin.AUC<<-ifelse(pick==1,FALSE,TRUE)
###

### cat("\n")
###   file.menu <- c("Select 2-6 data points manually",
###                  "Use Adjusted R sq. (ARS) method",
###                  "Use Akaike information criterion (AIC) method",
###                  "Use the Two-Times-Tmax(TTT) method",
###                  "Use TTT and ARS method",
###                  "Use TTT and AIC method",
###                  "Back to the previous step",
###                  "Quit")
###  cat("\n")
###   pick <- menu(file.menu, title = " <<Method Selections for Lambda_z Estimation>> ", graphics=TRUE)
if(replicated){
    filelocxx <- system.file("extdata", "Replicateddata.rda", package="bear")
    load(filelocxx)  ## because it is a *.rda data file

    with(entertitle.demo(), {
     description_RepNCAinput()  
     predata<-split(Replicateddata,list(Replicateddata$prd,Replicateddata$subj))
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
    code[j]<-j
 
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
                   
Singledata<-split(setdata, list(setdata$drug))

Refdata<-data.frame(subj=Singledata[[1]]$subj, seq= Singledata[[1]]$seq, prd=Singledata[[1]]$prd,
                    drug=Singledata[[1]]$drug, time=Singledata[[1]]$time, conc=Singledata[[1]]$conc,
                    code=Singledata[[1]]$code)
SingleRdata<-Refdata[ do.call(order, Refdata) ,]
show(SingleRdata);cat("\n\n")
SingleRdata1<-Refdata[ do.call(order, Refdata) ,]
SingleRdata1$conc[SingleRdata1$conc == 0] <- NA
### SingleRdata1 <- na.omit(SingleRdata1)     ### for v2.6.1 but no IDP output function yet
##SingleRdata1-->for select 3 points
cat("\n\n")
Testdata<-data.frame(subj=Singledata[[2]]$subj, seq=Singledata[[2]]$seq, prd=Singledata[[2]]$prd,
                     drug=Singledata[[2]]$drug, time=Singledata[[2]]$time, conc=Singledata[[2]]$conc,
                     code=Singledata[[2]]$code)
SingleTdata<-Testdata[ do.call(order, Testdata) ,]
show(SingleTdata)
SingleTdata1<-Testdata[ do.call(order, Testdata) ,]
SingleTdata1$conc[SingleTdata1$conc == 0] <- NA
### SingleTdata1 <- na.omit(SingleTdata1)     ### for v2.6.1 but no IDP output function yet

Totalplot<- rbind(SingleRdata,SingleTdata)
###
create.products_sum(Totalplot)
###
      
   if (lambda_z_calc == 5){
        description_pointselect()
        RepNCAselectdemo.MIX(Totalplot,SingleRdata1,SingleTdata1,Dose,SingleRdata,SingleTdata,xaxis, yaxis)
        RepNCA.MIXmenu()
        }
    else {
    if (lambda_z_calc == 0){
        RepARS.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
        RepNCA.MIXmenu()
       }
    else {
    if (lambda_z_calc == 1){
        RepAIC.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
        RepNCA.MIXmenu()
       }
    else {
    if (lambda_z_calc == 2){
        RepTTT.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
        RepNCA.MIXmenu()
       }
    else {
    if (lambda_z_calc == 3){
        RepTTTARS.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
        RepNCA.MIXmenu()
         }
   else {
    if (lambda_z_calc == 4){
        RepTTTAIC.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
        RepNCA.MIXmenu()
        }
        }
      }
     }
    }
   }
  })        
 }  
 else{
    if(parallel){
     if(multiple){
      filelocxx <- system.file("extdata", "MultipleParadata.rda", package="bear")
      load(filelocxx)  ## because it is a *.rda data file; leave it as was
      
      with(Multiplentertitle.demo(), {
      description_ParaNCAinput()  
      Singledata<-split(MultipleParadata, list(MultipleParadata$drug))
      Ref<-Singledata[[1]]
      Refdata<-data.frame(subj=Ref$subj,drug=Ref$drug,time=Ref$time, conc=Ref$conc)
      
       SingleRdata0<-Refdata[ do.call(order, Refdata) ,]
       SingleRdata1<-Refdata[ do.call(order, Refdata) ,]
       SingleRdata1$conc[SingleRdata1$conc == 0] <- NA
       ### SingleRdata1 <- na.omit(SingleRdata1)     ### for v2.6.1 IDP output function
      
      Test<-rbind(Singledata[[2]])
      Testdata<-data.frame(subj=Test$subj, drug=Test$drug, time=Test$time, conc=Test$conc)
      
       SingleTdata0<-Testdata[ do.call(order, Testdata) ,]
       SingleTdata1<-Testdata[ do.call(order, Testdata) ,]
       SingleTdata1$conc[SingleTdata1$conc == 0] <- NA
       ### SingleTdata1 <- na.omit(SingleTdata1)     ### for v2.6.1 IDP output function

      SingleRdata<-subset(SingleRdata0, time >=TlastD)
      show(SingleRdata0);cat("\n\n")
        if(IndivDP_output){
        SingleRTdata<-SingleRdata
        indiv_dp.output(SingleRTdata)
        }      
      SingleTdata<-subset(SingleTdata0, time >=TlastD)
      show(SingleTdata0)
        if(IndivDP_output){
        SingleRTdata<-SingleTdata
        indiv_dp.output(SingleRTdata)
        }      
      
      Totalplot<- rbind(SingleRdata,SingleTdata)
###
create.products_sum(Totalplot)
###
      
    if (lambda_z_calc == 5){
        description_pointselect()
        MultipleParaNCAselectdemo.MIX(Totalplot,SingleRdata1,SingleTdata1, Dose,SingleRdata,SingleTdata,xaxis, yaxis, Tau, TlastD,SingleRdata0,SingleTdata0)
        MultipleParaNCA.MIXmenu()
        }
    else {
    if (lambda_z_calc == 0){
       MultipleParaARS.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0)
       MultipleParaNCA.MIXmenu()
       }
    else {
    if (lambda_z_calc == 1){
       MultipleParaAIC.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0)
       MultipleParaNCA.MIXmenu()
       }
    else {
    if (lambda_z_calc == 2){
       MultipleParaTTT.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0)
       MultipleParaNCA.MIXmenu()
       }
    else {
    if (lambda_z_calc == 3){
       MultipleParaTTTARS.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0)
       MultipleParaNCA.MIXmenu()
       }
   else {
    if (lambda_z_calc == 4){
       MultipleParaTTTAIC.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0)
       MultipleParaNCA.MIXmenu()
       }
        }
      }
     }
    }
   }
  })         
 }
    else{
     filelocxx <- system.file("extdata", "Paralleldata.rda", package="bear")
     load(filelocxx)  ## because it is a *.rda data file
     
     with(entertitle.demo(), {
     description_ParaNCAinput()  
      Singledata<-split(Paralleldata, list(Paralleldata$drug))
      Ref<-Singledata[[1]]
      Refdata<-data.frame(subj=Ref$subj,drug=Ref$drug,time=Ref$time, conc=Ref$conc)
      SingleRdata<-Refdata[ do.call(order, Refdata) ,]
      show(SingleRdata);cat("\n\n")
      if(IndivDP_output){
      SingleRTdata<-SingleRdata
      indiv_dp.output(SingleRTdata)
      }
      SingleRdata1<-Refdata[ do.call(order, Refdata) ,]
      SingleRdata1$conc[SingleRdata1$conc == 0] <- NA
      ### SingleRdata1 <- na.omit(SingleRdata1)     ### for v2.6.1 IDP output function
        cat("\n\n")
      Test<-rbind(Singledata[[2]])
      Testdata<-data.frame(subj=Test$subj, drug=Test$drug, time=Test$time, conc=Test$conc)
      SingleTdata<-Testdata[ do.call(order, Testdata) ,]
      show(SingleTdata)
      if(IndivDP_output){
      SingleRTdata<-SingleTdata
      indiv_dp.output(SingleRTdata)
      ###
      }      
      SingleTdata1<-Testdata[ do.call(order, Testdata) ,]
      SingleTdata1$conc[SingleTdata1$conc == 0] <- NA
      ### SingleTdata1 <- na.omit(SingleTdata1)     ### for v2.6.1 IDP output function
      Totalplot<- rbind(SingleRdata,SingleTdata)
###
create.products_sum(Totalplot)
###
      
    if (lambda_z_calc == 5){
        description_pointselect()
        ParaNCAselectdemo.MIX(Totalplot,SingleRdata1,SingleTdata1,Dose,SingleRdata,SingleTdata,xaxis, yaxis)
        ParaNCA.MIXmenu()
        }
    else {
    if (lambda_z_calc == 0){
       ParaARS.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       ParaNCA.MIXmenu()
       }
    else {
    if (lambda_z_calc == 1){
       ParaAIC.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       ParaNCA.MIXmenu()
       }
    else {
    if (lambda_z_calc == 2){
       ParaTTT.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       ParaNCA.MIXmenu()
       }
    else {
    if (lambda_z_calc == 3){
       ParaTTTARS.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       ParaNCA.MIXmenu()
       }
   else {
    if (lambda_z_calc == 4){
       ParaTTTAIC.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       ParaNCA.MIXmenu()
       }
        }
      }
     }
    }
   }
  })    
 }
} 
 else{
     if(multiple){
     filelocxx <- system.file("extdata", "Multipledata.rda", package="bear")
     load(filelocxx)  ## because it is a *.rda data file
     
     with(Multiplentertitle.demo(), {
     description_NCAinput()  
      TotalSingledata<-Multipledata
      Singledata<-split(TotalSingledata, list(TotalSingledata$seq, TotalSingledata$prd))
      Ref<-rbind(Singledata[[1]],Singledata[[4]])
      Refdata<-data.frame(subj=Ref$subj, seq= Ref$seq, prd=Ref$prd, drug=c(1), 
                    time=Ref$time, conc=Ref$conc)
       SingleRdata0<-Refdata[ do.call(order, Refdata) ,]
       SingleRdata1<-Refdata[ do.call(order, Refdata) ,]
       SingleRdata1$conc[SingleRdata1$conc == 0] <- NA
       ### SingleRdata1 <- na.omit(SingleRdata1)     ### for v2.6.1 IDP output function
        cat("\n\n")
      Test<-rbind(Singledata[[2]],Singledata[[3]])
      Testdata<-data.frame(subj=Test$subj, seq= Test$seq, prd=Test$prd, drug=c(2), 
                     time=Test$time, conc=Test$conc)
       SingleTdata0<-Testdata[ do.call(order, Testdata) ,]
       SingleTdata1<-Testdata[ do.call(order, Testdata) ,]
       SingleTdata1$conc[SingleTdata1$conc == 0] <- NA
       ### SingleTdata1 <- na.omit(SingleTdata1)     ### for v2.6.1 IDP output function
      
     SingleRdata<-subset(SingleRdata0, time >=TlastD)
     show(SingleRdata0);cat("\n\n")   ### here show 'SingleRdata0' (original datset) not 'SingleRdata' since it is multiple-dose!
      if(IndivDP_output){
      SingleRTdata<-SingleRdata
      indiv_dp.output(SingleRTdata)
      }     
     SingleTdata<-subset(SingleTdata0, time >=TlastD)
     show(SingleTdata0)
      if(IndivDP_output){
      SingleRTdata<-SingleTdata
      indiv_dp.output(SingleRTdata)
      }     
     Totalplot<- rbind(SingleRdata0,SingleTdata0)
###
create.products_sum(Totalplot)
###
           
    if (lambda_z_calc == 5){
         description_pointselect()
         MultipleNCAselectdemo.BANOVA(Totalplot,SingleRdata1,SingleTdata1, Dose,SingleRdata,SingleTdata,xaxis, yaxis, Tau, TlastD,SingleRdata0,SingleTdata0) 
         MultipleNCA.BANOVAmenu()
        }
    else {
    if (lambda_z_calc == 0){
        MultipleARS.BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0) 
        MultipleNCA.BANOVAmenu()
       }
    else {
    if (lambda_z_calc == 1){
        MultipleAIC_BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0) 
        MultipleNCA.BANOVAmenu()
       }
    else {
    if (lambda_z_calc == 2){
        MultipleTTT.BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0) 
        MultipleNCA.BANOVAmenu()
       }
    else {
    if (lambda_z_calc == 3){
        MultipleTTTARS.BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0) 
        MultipleNCA.BANOVAmenu()
       }
   else {
    if (lambda_z_calc == 4){
       MultipleTTTAIC.BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0) 
       MultipleNCA.BANOVAmenu()
       }
        }
       }
      }
     }
    }
   }) 
 }
   else{
    description_NCAinput()
    filelocxx <- system.file("extdata", "TotalSingledata.rda", package="bear")   ### both work well.
    load(filelocxx)  ## because it is a *.rda data file                      
    ### filelocxx <- system.file("extdata", "Single2x2x2.rda", package="bear")   ### v2.6.1 to test IDP function
    ### TotalSingledata<-readRDS(filelocxx)
    
    cat("\n\n")
     ##NCAanalyze or NCAGLManalyze
     with(entertitle.demo(), {
     description_drug()

      Singledata<-split(TotalSingledata, list(TotalSingledata$seq, TotalSingledata$prd))
      Ref<-rbind(Singledata[[1]],Singledata[[4]])
      Refdata<-data.frame(subj=Ref$subj, seq= Ref$seq, prd=Ref$prd, drug=c(1), time=Ref$time, conc=Ref$conc)
      SingleRdata<-Refdata[ do.call(order, Refdata) ,]
      show(SingleRdata);cat("\n\n")
      if(IndivDP_output){
      SingleRTdata<-SingleRdata
      indiv_dp.output(SingleRTdata)
      }      
      SingleRdata1<-Refdata[ do.call(order, Refdata) ,]
      SingleRdata1$conc[SingleRdata1$conc == 0] <- NA
      ### SingleRdata1 <- na.omit(SingleRdata1)     ### for v2.6.1 IDP output function
      cat("\n\n")
      Test<-rbind(Singledata[[2]],Singledata[[3]])
      Testdata<-data.frame(subj=Test$subj, seq= Test$seq, prd=Test$prd, drug=c(2), time=Test$time, conc=Test$conc)
      SingleTdata<-Testdata[ do.call(order, Testdata) ,]
      show(SingleTdata)
      if(IndivDP_output){
      SingleRTdata<-SingleTdata
      indiv_dp.output(SingleRTdata)
      }      
      SingleTdata1<-Testdata[ do.call(order, Testdata) ,]
      SingleTdata1$conc[SingleTdata1$conc == 0] <- NA
      ### SingleTdata1 <- na.omit(SingleTdata1)     ### for v2.6.1 IDP output function
      Totalplot<- rbind(SingleRdata,SingleTdata)
###
create.products_sum(Totalplot)
###

    if (lambda_z_calc == 5){
        description_pointselect()
        NCAselectdemo.BANOVA(Totalplot,SingleRdata1,SingleTdata1,Dose,SingleRdata,SingleTdata,xaxis, yaxis)
        NCA.BANOVAmenu()
        }

    else {
    if (lambda_z_calc == 0){
        ARS.BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
        NCA.BANOVAmenu()
       }
       
    else {
    if (lambda_z_calc == 1){
        AIC_BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
        NCA.BANOVAmenu()
       }
    else {
    if (lambda_z_calc == 2){
        TTT.BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
        NCA.BANOVAmenu()
       }
    else {
    if (lambda_z_calc == 3){
        TTTARS.BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
        NCA.BANOVAmenu()
         }
   else {
    if (lambda_z_calc == 4){
        TTTAIC.BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
        NCA.BANOVAmenu()
        }
        }
       }
      }
     }
    }
   })
   }
  }
 }
} 