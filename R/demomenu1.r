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
    ### save the dataset for testing...  -YJ
    if(file.exists("SingleRep_demo.csv")){
         write.csv(Replicateddata,file="SingleRep_demo_02.csv",row.names=FALSE)}
    else{write.csv(Replicateddata,file="SingleRep_demo.csv",row.names=FALSE)}
    if(file.exists("SingleRep_demo.RData")){
         saveRDS(Replicateddata,file="SingleRep_demo_02.RData")}
    else{saveRDS(Replicateddata,file="SingleRep_demo.RData")}

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

SingleRdata1<-Refdata[ do.call(order, Refdata) ,]
SingleRdata1$conc[SingleRdata1$conc == 0] <- NA
SingleRdata1 <- na.omit(SingleRdata1)
##SingleRdata1-->for select 3 points
cat("\n\n")
Testdata<-data.frame(subj=Singledata[[2]]$subj, seq=Singledata[[2]]$seq, prd=Singledata[[2]]$prd,
                     drug=Singledata[[2]]$drug, time=Singledata[[2]]$time, conc=Singledata[[2]]$conc,
                     code=Singledata[[2]]$code)
SingleTdata<-Testdata[ do.call(order, Testdata) ,]

SingleTdata1<-Testdata[ do.call(order, Testdata) ,]
SingleTdata1$conc[SingleTdata1$conc == 0] <- NA
SingleTdata1 <- na.omit(SingleTdata1)

Totalplot<- rbind(SingleRdata,SingleTdata)
###
create.products_sum(Totalplot)
###
      
   if (lambda_z_calc == 5){
        description_pointselect()
      cat("\n")
        ## show(SingleRdata)   ### close this!  YJ
        ## show(SingleTdata)
        RepNCAselectdemo.MIX(Totalplot,SingleRdata1,SingleTdata1,Dose,SingleRdata,SingleTdata,xaxis, yaxis)
        RepNCA.MIXmenu()
        }
    else {
    if (lambda_z_calc == 0){
        cat("\n")
        ## show(SingleRdata)   ### close this!  YJ
        ## show(SingleTdata)
        RepARS.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
        RepNCA.MIXmenu()
       }
    else {
    if (lambda_z_calc == 1){
        cat("\n")
        ## show(SingleRdata)   ### close this!  YJ
        ## show(SingleTdata)
        RepAIC.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
        RepNCA.MIXmenu()
       }
    else {
    if (lambda_z_calc == 2){
        cat("\n")
        ## show(SingleRdata)   ### close this!  YJ
        ## show(SingleTdata)
        RepTTT.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
        RepNCA.MIXmenu()
       }
    else {
    if (lambda_z_calc == 3){
        cat("\n")
        ## show(SingleRdata)   ### close this!  YJ
        ## show(SingleTdata)
        RepTTTARS.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
        RepNCA.MIXmenu()
         }
   else {
    if (lambda_z_calc == 4){
        cat("\n")
        ## show(SingleRdata)   ### close this!  YJ
        ## show(SingleTdata)
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
      if(file.exists("MultiplePara_demo.csv")){
         write.csv(MultipleParadata,file="MultiplePara_demo_02.csv",row.names=FALSE)}
      else{write.csv(MultipleParadata,file="MultiplePara_demo.csv",row.names=FALSE)}
      if(file.exists("MultiplePara_demo.RData")){
         saveRDS(MultipleParadata,file="MultiplePara_demo_02.RData")}
      else{saveRDS(MultipleParadata,file="MultiplePara_demo.RData")}
      
      with(Multiplentertitle.demo(), {
      description_ParaNCAinput()  
      Singledata<-split(MultipleParadata, list(MultipleParadata$drug))
      Ref<-Singledata[[1]]
      Refdata<-data.frame(subj=Ref$subj,drug=Ref$drug,time=Ref$time, conc=Ref$conc)
      
       SingleRdata0<-Refdata[ do.call(order, Refdata) ,]
       SingleRdata1<-Refdata[ do.call(order, Refdata) ,]
       SingleRdata1$conc[SingleRdata1$conc == 0] <- NA
       SingleRdata1 <- na.omit(SingleRdata1)
      
      Test<-rbind(Singledata[[2]])
      Testdata<-data.frame(subj=Test$subj, drug=Test$drug, time=Test$time, conc=Test$conc)
      
       SingleTdata0<-Testdata[ do.call(order, Testdata) ,]
       SingleTdata1<-Testdata[ do.call(order, Testdata) ,]
       SingleTdata1$conc[SingleTdata1$conc == 0] <- NA
       SingleTdata1 <- na.omit(SingleTdata1)

      SingleRdata<-subset(SingleRdata0, time >=TlastD)
      SingleTdata<-subset(SingleTdata0, time >=TlastD)
      
      Totalplot<- rbind(SingleRdata,SingleTdata)
###
create.products_sum(Totalplot)
###
      
    if (lambda_z_calc == 5){
        description_pointselect()
        show(SingleRdata)
        show(SingleTdata)
        cat("\n")
        MultipleParaNCAselectdemo.MIX(Totalplot,SingleRdata1,SingleTdata1, Dose,SingleRdata,SingleTdata,xaxis, yaxis, Tau, TlastD,SingleRdata0,SingleTdata0)
        MultipleParaNCA.MIXmenu()
        }
    else {
    if (lambda_z_calc == 0){
      show(SingleRdata)
        show(SingleTdata)
        cat("\n")
       MultipleParaARS.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0)
       MultipleParaNCA.MIXmenu()
       }
    else {
    if (lambda_z_calc == 1){
        show(SingleRdata)
        show(SingleTdata)
        cat("\n")
       MultipleParaAIC.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0)
       MultipleParaNCA.MIXmenu()
       }
    else {
    if (lambda_z_calc == 2){
        show(SingleRdata)
        show(SingleTdata)
        cat("\n")
       MultipleParaTTT.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0)
       MultipleParaNCA.MIXmenu()
       }
    else {
    if (lambda_z_calc == 3){
        show(SingleRdata)
        show(SingleTdata)
        cat("\n")
       MultipleParaTTTARS.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0)
       MultipleParaNCA.MIXmenu()
       }
   else {
    if (lambda_z_calc == 4){
        show(SingleRdata)
        show(SingleTdata)
        cat("\n")
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
      if(file.exists("SinglePara_demo.csv")){
         write.csv(Paralleldata,file="SinglePara_demo_02.csv",row.names=FALSE)}
      else{write.csv(Paralleldata,file="SinglePara_demo.csv",row.names=FALSE)}
      if(file.exists("SinglePara_demo.RData")){
         saveRDS(Paralleldata,file="SinglePara_demo_02.RData")}
      else{saveRDS(Paralleldata,file="SinglePara_demo.RData")}
     
     with(entertitle.demo(), {
     description_ParaNCAinput()  
      Singledata<-split(Paralleldata, list(Paralleldata$drug))
      Ref<-Singledata[[1]]
      Refdata<-data.frame(subj=Ref$subj,drug=Ref$drug,time=Ref$time, conc=Ref$conc)
      SingleRdata<-Refdata[ do.call(order, Refdata) ,]
      
      SingleRdata1<-Refdata[ do.call(order, Refdata) ,]
      SingleRdata1$conc[SingleRdata1$conc == 0] <- NA
      SingleRdata1 <- na.omit(SingleRdata1)
        cat("\n\n")
      Test<-rbind(Singledata[[2]])
      Testdata<-data.frame(subj=Test$subj, drug=Test$drug, time=Test$time, conc=Test$conc)
      SingleTdata<-Testdata[ do.call(order, Testdata) ,]
      
      SingleTdata1<-Testdata[ do.call(order, Testdata) ,]
      SingleTdata1$conc[SingleTdata1$conc == 0] <- NA
      SingleTdata1 <- na.omit(SingleTdata1)
      Totalplot<- rbind(SingleRdata,SingleTdata)
###
create.products_sum(Totalplot)
###
      
    if (lambda_z_calc == 5){
        description_pointselect()
        cat("\n")
        show(SingleRdata)
        show(SingleTdata)
        ParaNCAselectdemo.MIX(Totalplot,SingleRdata1,SingleTdata1,Dose,SingleRdata,SingleTdata,xaxis, yaxis)
        ParaNCA.MIXmenu()
        }
    else {
    if (lambda_z_calc == 0){
      cat("\n")
        show(SingleRdata)
        show(SingleTdata)
       ParaARS.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       ParaNCA.MIXmenu()
       }
    else {
    if (lambda_z_calc == 1){
        cat("\n")
        show(SingleRdata)
        show(SingleTdata)
       ParaAIC.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       ParaNCA.MIXmenu()
       }
    else {
    if (lambda_z_calc == 2){
        cat("\n")
        show(SingleRdata)
        show(SingleTdata) 
       ParaTTT.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       ParaNCA.MIXmenu()
       }
    else {
    if (lambda_z_calc == 3){
        cat("\n")
        show(SingleRdata)
        show(SingleTdata)
       ParaTTTARS.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       ParaNCA.MIXmenu()
       }
   else {
    if (lambda_z_calc == 4){
        cat("\n")
        show(SingleRdata)
        show(SingleTdata)
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
     if(file.exists("Multiple2x2x2_demo.csv")){
        write.csv(Multipledata,file="Multiple2x2x2_demo_02.csv",row.names=FALSE)}
     else{write.csv(Multipledata,file="Multiple2x2x2_demo.csv",row.names=FALSE)}
     if(file.exists("Multiple2x2x2_demo.RData")){
        saveRDS(Multipledata,file="Multiple2x2x2_demo_02.RData")}
     else{saveRDS(Multipledata,file="Multiple2x2x2_demo.RData")}     
     
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
       SingleRdata1 <- na.omit(SingleRdata1)
        cat("\n\n")
      Test<-rbind(Singledata[[2]],Singledata[[3]])
      Testdata<-data.frame(subj=Test$subj, seq= Test$seq, prd=Test$prd, drug=c(2), 
                     time=Test$time, conc=Test$conc)
       SingleTdata0<-Testdata[ do.call(order, Testdata) ,]
       SingleTdata1<-Testdata[ do.call(order, Testdata) ,]
       SingleTdata1$conc[SingleTdata1$conc == 0] <- NA
       SingleTdata1 <- na.omit(SingleTdata1)
      
     SingleRdata<-subset(SingleRdata0, time >=TlastD)
     SingleTdata<-subset(SingleTdata0, time >=TlastD)
     Totalplot<- rbind(SingleRdata0,SingleTdata0)
###
create.products_sum(Totalplot)
###
           
    if (lambda_z_calc == 5){
        description_pointselect()
        show(SingleRdata0)
        show(SingleTdata0)
        cat("\n")                 
         MultipleNCAselectdemo.BANOVA(Totalplot,SingleRdata1,SingleTdata1, Dose,SingleRdata,SingleTdata,xaxis, yaxis, Tau, TlastD,SingleRdata0,SingleTdata0) 
         MultipleNCA.BANOVAmenu()
        }
    else {
    if (lambda_z_calc == 0){
        show(SingleRdata0)
        show(SingleTdata0)
        cat("\n")
       MultipleARS.BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0) 
        MultipleNCA.BANOVAmenu()
       }
    else {
    if (lambda_z_calc == 1){
        show(SingleRdata0)
        show(SingleTdata0)
        cat("\n")
        MultipleAIC_BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0) 
        MultipleNCA.BANOVAmenu()
       }
    else {
    if (lambda_z_calc == 2){
        show(SingleRdata0)
        show(SingleTdata0)
        cat("\n")
        MultipleTTT.BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0) 
        MultipleNCA.BANOVAmenu()
       }
    else {
    if (lambda_z_calc == 3){
        show(SingleRdata0)
        show(SingleTdata0)
        cat("\n")
       MultipleTTTARS.BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1, Tau, TlastD,SingleRdata0,SingleTdata0) 
        MultipleNCA.BANOVAmenu()
       }
   else {
    if (lambda_z_calc == 4){
        show(SingleRdata0)
        show(SingleTdata0)
        cat("\n")
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
    filelocxx <- system.file("extdata", "TotalSingledata.rda", package="bear")
    load(filelocxx)  ## because it is a *.rda data file
    if(file.exists("Single2x2x2_demo.csv")){
       write.csv(TotalSingledata,file="Single2x2x2_demo_02.csv",row.names=FALSE)}
    else{write.csv(TotalSingledata,file="Single2x2x2_demo.csv",row.names=FALSE)}
    if(file.exists("Single2x2x2_demo.RData")){
       saveRDS(TotalSingledata,file="Single2x2x2_demo_02.RData")}
    else{saveRDS(TotalSingledata,file="Single2x2x2_demo.RData")}         
    
    cat("\n\n")
     ##NCAanalyze or NCAGLManalyze
     with(entertitle.demo(), {
     description_drug()

      Singledata<-split(TotalSingledata, list(TotalSingledata$seq, TotalSingledata$prd))
      Ref<-rbind(Singledata[[1]],Singledata[[4]])
      Refdata<-data.frame(subj=Ref$subj, seq= Ref$seq, prd=Ref$prd, drug=c(1), time=Ref$time, conc=Ref$conc)
      SingleRdata<-Refdata[ do.call(order, Refdata) ,]
      
      SingleRdata1<-Refdata[ do.call(order, Refdata) ,]
      SingleRdata1$conc[SingleRdata1$conc == 0] <- NA
      SingleRdata1 <- na.omit(SingleRdata1)
        cat("\n\n")
      Test<-rbind(Singledata[[2]],Singledata[[3]])
      Testdata<-data.frame(subj=Test$subj, seq= Test$seq, prd=Test$prd, drug=c(2), time=Test$time, conc=Test$conc)
      SingleTdata<-Testdata[ do.call(order, Testdata) ,]
      
      SingleTdata1<-Testdata[ do.call(order, Testdata) ,]
      SingleTdata1$conc[SingleTdata1$conc == 0] <- NA
      SingleTdata1 <- na.omit(SingleTdata1)
      Totalplot<- rbind(SingleRdata,SingleTdata)
###
create.products_sum(Totalplot)
###

    if (lambda_z_calc == 5){
      cat("\n")
        description_pointselect()
        show(SingleRdata)
        show(SingleTdata)
        NCAselectdemo.BANOVA(Totalplot,SingleRdata1,SingleTdata1,Dose,SingleRdata,SingleTdata,xaxis, yaxis)
        NCA.BANOVAmenu()
        }

    else {
    if (lambda_z_calc == 0){
        cat("\n")
        show(SingleRdata)
        show(SingleTdata)
        ARS.BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
        NCA.BANOVAmenu()
       }
       
    else {
    if (lambda_z_calc == 1){
        cat("\n")
        show(SingleRdata)
        show(SingleTdata)
        AIC_BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
        NCA.BANOVAmenu()
       }
    else {
    if (lambda_z_calc == 2){
        cat("\n")
        show(SingleRdata)
        show(SingleTdata)
        TTT.BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
        NCA.BANOVAmenu()
       }
    else {
    if (lambda_z_calc == 3){
        cat("\n")
        show(SingleRdata)
        show(SingleTdata)
        TTTARS.BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
        NCA.BANOVAmenu()
         }
   else {
    if (lambda_z_calc == 4){
        cat("\n")
        show(SingleRdata)
        show(SingleTdata)
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