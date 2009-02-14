##demo for NCA
demomenu1<-function(replicated=FALSE)
{
cat("\n")
  file.menu <- c("NCA (the exact 3 data points) --> Statistical analysis",
                 "NCA (ARS) --> Statistical analysis",
                 "NCA (AIC) --> Statistical analysis",
                 "NCA (TTT) --> Statistical analysis",
                 "NCA (TTT and ARS) --> Statistical analysis",
                 "NCA (TTT and AIC) --> Statistical analysis",
                 "Back to the previous step",
                 "Quit")
 cat("\n")
  pick <- menu(file.menu, title = " << NCA --> Statistical analysis>> ")
     if(replicated){
    data(Replicateddata)

    with(entertitle.demo(), {
     description_drug()  
     predata<-split(Replicateddata,  list(Replicateddata$prd,Replicateddata$subj))
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
Totalplot<- rbind(SingleRdata,SingleTdata)              
      
   if (pick == 1){
      cat("\n")
        RepNCAselectdemo.MIX(Totalplot,SingleRdata1,SingleTdata1,Dose,SingleRdata,SingleTdata,xaxis, yaxis)
        RepNCA.MIXmenu()
        }
    else {
    if (pick == 2){
        cat("\n")
        RepARS.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
        RepNCA.MIXmenu()
       }
    else {
    if (pick == 3){
        cat("\n")
        RepAIC.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       RepNCA.MIXmenu()
       }
    else {
    if (pick == 4){
        cat("\n")
       RepTTT.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       RepNCA.MIXmenu()
       }
    else {
    if (pick == 5){
        cat("\n")
        RepTTTARS.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
         RepNCA.MIXmenu()
         }
   else {
    if (pick == 6){
        cat("\n")
       RepTTTAIC.MIX(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
        RepNCA.MIXmenu()
        }
   else {
    if (pick == 7){
         RepNCA.MIXmenu()
         }       
   else {
    if (pick == 8){
       cat("\n")
       cat("\nThank you for using bear!  Bye now. \n")
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
 else{
    description_NCAinput()
    data(TotalSingledata)
    cat("\n\n")

     ##NCAanalyze or NCAGLManalyze
     with(entertitle.demo(), {
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
      Totalplot<- rbind(SingleRdata,SingleTdata)
    if (pick == 1){
      cat("\n")
        
        NCAselectdemo.BANOVA(Totalplot,SingleRdata1,SingleTdata1,Dose,SingleRdata,SingleTdata,xaxis, yaxis)
        NCA.BANOVAmenu()
        }

    else {
    if (pick == 2){
        cat("\n")
        
        ARS.BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
        NCA.BANOVAmenu()
       }
       
    else {
    if (pick == 3){
        cat("\n")
       
       AIC_BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       NCA.BANOVAmenu()
       }
    else {
    if (pick == 4){
        cat("\n")
       
       TTT.BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       NCA.BANOVAmenu()
       }
    else {
    if (pick == 5){
        cat("\n")
       
       TTTARS.BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       NCA.BANOVAmenu()
         }
   else {
    if (pick == 6){
        cat("\n")
       
       TTTAIC.BANOVA(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       NCA.BANOVAmenu()
        }
    else {
    if (pick == 7){
        cat("\n")
      NCA.BANOVAmenu()
         }
     else {
    if (pick == 8){
       cat("\n")
       cat("\nThank you for using bear!  Bye now. \n")
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
} 