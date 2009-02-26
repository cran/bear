##demo for NCA
library(reshape)
demomenu<-function(replicated=FALSE, parallel=FALSE)
{
cat("\n")
  file.menu <- c("lambda_z est. from the exact 3 data points",
                 "lambda_z est. with adjusted R sq. (ARS)",
                 "lambda_z est. with Akaike information criterion (AIC)",
                 "lambda_z est. with Two-Times-Tmax method (TTT)",
                 "lambda_z est. with TTT and ARS",
                 "lambda_z est. with TTT and AIC",
                 "Back to the previous step",
                 "Quit")
 cat("\n")
  pick <- menu(file.menu, title = " << Noncompartmental analysis (NCA)>> ")
    description_NCAinput()
    
    if(replicated){
    data(Replicateddata)

    with(entertitle.demo(), {
     description_RepNCAinput()  
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
      
   if (pick == 1){
        show(SingleRdata)
        show(SingleTdata)
        cat("\n")
        RepNCAselectdemo(Totalplot,SingleRdata1,SingleTdata1,Dose,SingleRdata,SingleTdata,xaxis, yaxis)
        }
    else {
    if (pick == 2){
        show(SingleRdata)
        show(SingleTdata)
        cat("\n")
        RepARSdemo(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       }
    else {
    if (pick == 3){
        show(SingleRdata)
        show(SingleTdata)
        cat("\n")
        RepAICdemo(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       }
    else {
    if (pick == 4){
        show(SingleRdata)
        show(SingleTdata)
        cat("\n")
       RepTTTdemo(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       }
    else {
    if (pick == 5){
        show(SingleRdata)
        show(SingleTdata)
        cat("\n")
        RepTTTARSdemo(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
         }
   else {
    if (pick == 6){
        show(SingleRdata)
        show(SingleTdata)
        cat("\n")
       RepTTTAICdemo(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
        }
   else {
    if (pick == 7){
         Repmenu()
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
     if(parallel){
     data(Paralleldata)

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
      
    if (pick == 1){
        show(SingleRdata)
        show(SingleTdata)
        cat("\n")
        ParaNCAselectdemo(Totalplot,SingleRdata1,SingleTdata1,Dose,SingleRdata,SingleTdata,xaxis, yaxis)
        }
    else {
    if (pick == 2){
      show(SingleRdata)
        show(SingleTdata)
        cat("\n")
       ParaARSdemo(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       }
    else {
    if (pick == 3){
        show(SingleRdata)
        show(SingleTdata)
        cat("\n")
       ParaAICdemo(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       }
    else {
    if (pick == 4){
        show(SingleRdata)
        show(SingleTdata)
        cat("\n")
       ParaTTTdemo(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       }
    else {
    if (pick == 5){
        show(SingleRdata)
        show(SingleTdata)
        cat("\n")
       ParaTTTARSdemo(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       }
   else {
    if (pick == 6){
        show(SingleRdata)
        show(SingleTdata)
        cat("\n")
       ParaTTTAICdemo(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       }
   else {
    if (pick == 7){
        cat("\n")
        Paramenu()
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
    data(TotalSingledata)

     with(entertitle.demo(), {
     description_NCAinput()  
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
      
    if (pick == 1){
        show(SingleRdata)
        show(SingleTdata)
        cat("\n")
        NCAselectdemo(Totalplot,SingleRdata1,SingleTdata1,Dose,SingleRdata,SingleTdata,xaxis, yaxis)
        }
    else {
    if (pick == 2){
        show(SingleRdata)
        show(SingleTdata)
        cat("\n")
       ARSdemo(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       }
    else {
    if (pick == 3){
        show(SingleRdata)
        show(SingleTdata)
        cat("\n")
       AICdemo(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       }
    else {
    if (pick == 4){
        show(SingleRdata)
        show(SingleTdata)
        cat("\n")
       TTTdemo(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       }
    else {
    if (pick == 5){
        show(SingleRdata)
        show(SingleTdata)
        cat("\n")
       TTTARSdemo(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       }
   else {
    if (pick == 6){
        show(SingleRdata)
        show(SingleTdata)
        cat("\n")
       TTTAICdemo(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1)
       }
   else {
    if (pick == 7){
        cat("\n")
        NCAmenu()
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
} 
