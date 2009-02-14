##NCA
NCA<-function(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,
               SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split,
               Demo=FALSE,BANOVA=FALSE,replicated=FALSE,MIX=FALSE)
{

#fitting data with linear regression model
#cat("<<Output: linear regression model: conc. vs. time>>\n")
#split dataframe into sub-dataframe by subject for reference data
#"time.ref" means "kel"

 if(replicated){
       R.split<-split(SingleRdata, list(SingleRdata$code))
       Lm1 <- lmList(conc ~ time |code, data = ref_data) 
         
      subj<-0
      prd<-0
      seq<-0
      code<-0
      AdjR<-0
       for (j in 1:(length(R.split))){
        subj[j]<-R.split[[j]][["subj"]][1]
        prd[j]<-R.split[[j]][["prd"]][1]
        seq[j]<-R.split[[j]][["seq"]][1]
        code[j]<-R.split[[j]][["code"]][1]
        AdjR[j]<-summary(Lm1)$adj.r.squared
       }
    keindex_ref<-data.frame(subj=subj, seq=seq, prd=prd, code=code, time=-2.3*(coef(Lm1)[2]), 
             R_squared=summary(Lm1)$r.sq, Adj_R_squared=melt(AdjR)$value )
  }
 else{
        R.split<-split(SingleRdata, list(SingleRdata$subj))
        Lm1 <- lmList(conc ~ time |subj, data = ref_data)
       
        subj<-0
        AdjR<-0
         for (j in 1:(length(R.split))){
          subj[j]<-R.split[[j]][["subj"]][1]
          AdjR[j]<-summary(Lm1)$adj.r.squared
         }
     keindex_ref<-data.frame(subj=subj, time=-2.3*(coef(Lm1)[2]), 
             R_squared=summary(Lm1)$r.sq, Adj_R_squared=melt(AdjR)$value )  
       }
 
  #calculate AUC
 CmaxRef<-0
 AUCINFRef<-0
 AUCTRef<-0
 TmaxRef<-0
 MRTINFRef<-0
 T12Ref<-0
 VdFRef<-0
 KelRef<-0
 ClFRef<-0

      for (j in 1:length(R.split)){
         #if subject of W.split==subject of kepar, then use ke of kepar to claculate AUC(0~INF)
          
          if(replicated){
          ke<-0
          R_sq<-0
          AR_sq<-0
          su<-0
          se<-0
          pr<-0
            for(x in 1: length(unique( keindex_ref$code))){
              if (R.split[[j]][["code"]][1]==keindex_ref$code[[x]]){
                  ke<- keindex_ref$time[[x]]
                  R_sq<-keindex_ref$R_squared[[x]]
                  AR_sq<-keindex_ref$Adj_R_squared[[x]]
                  su<-keindex_ref$subj[[x]]
                  se<-keindex_ref$seq[[x]]
                  pr<-keindex_ref$prd[[x]]
                 }
               }
           }
          else{
            ke<-0
            R_sq<-0
            AR_sq<-0
            su<-0
            for(x in 1: length(unique( keindex_ref$subj))){
              if (R.split[[j]][["subj"]][1]==keindex_ref$subj[[x]]){
                  ke<- keindex_ref$time[[x]]
                  R_sq<-keindex_ref$R_squared[[x]]
                  AR_sq<-keindex_ref$Adj_R_squared[[x]]
                  su<-keindex_ref$subj[[x]]
                 }
               }
            }    
          auc_ref <-0
          tmax_ref<-0
          Cmax_ref<-0
          aumc_ref<-0
          for(i in 2:length(R.split[[j]][["time"]])){
             #calculate AUC and exclude AUC==NA (auc<-0)
             auc_ref[i]<-(R.split[[j]][["time"]][i]-R.split[[j]][["time"]][i-1])*(R.split[[j]][["conc"]][i]+R.split[[j]][["conc"]][i-1])* 0.5
             auc_ref[i]<-auc_ref[i]+auc_ref[i-1]
             #calculate AUMC
             aumc_ref[i]<-((R.split[[j]][["time"]][i])*(R.split[[j]][["conc"]][i])+(R.split[[j]][["time"]][i-1])*(R.split[[j]][["conc"]][i-1]))*
                          ((R.split[[j]][["time"]][i])-(R.split[[j]][["time"]][i-1]))* 0.5
             aumc_ref[i]<-aumc_ref[i]+aumc_ref[i-1]
             Cmax_ref<-max(R.split[[j]][["conc"]], na.rm = FALSE)
             subgr= c(Cmax_ref)
             tmax_ref<-R.split[[j]][R.split[[j]][["conc"]] %in% subgr,]
              }

              #calculate AUC (0~INF)
               auc.infinity<-R.split[[j]][["conc"]][length(R.split[[j]][["conc"]])]/ke
               aucINF<-auc_ref[length(R.split[[j]][["conc"]])]+auc.infinity

                #calculate AUMC (0~INF)
                  aumc.infinity_1<-(R.split[[j]][["conc"]][length(R.split[[j]][["conc"]])])*(R.split[[j]][["time"]][length(R.split[[j]][["time"]])])/ke
                  aumc.infinity_2<-(R.split[[j]][["conc"]][length(R.split[[j]][["conc"]])]/(ke^2))
                  aumcINF<-aumc_ref[length(R.split[[j]][["conc"]])]+aumc.infinity_1+aumc.infinity_2

                   #for summary result
                  CmaxRef[j]<-Cmax_ref
                  AUCINFRef[j]<-aucINF
                  AUCTRef[j]<-auc_ref[length(R.split[[j]][["conc"]])]
                  TmaxRef[j]<-tmax_ref[,5]
                  MRTINFRef[j]<-aumcINF/aucINF
                  T12Ref[j]<-log(2)/ke
                  VdFRef[j]<-Dose/(aucINF*ke)
                  KelRef[j]<-ke
                  ClFRef[j]<-Dose/aucINF

                 cat("\n")
                 if(replicated){
                 cat("<< NCA Outputs:- Subj.#",su,", Seq",se,", Prd",pr," (Ref.)>>\n")
                    }
                   else{
                 cat("<< NCA Outputs:- Subj.#",su," (Ref.)>>\n")
                    }
                 cat("--------------------------------------------------------------------------\n")
                 output<-data.frame(R.split[[j]][["subj"]],R.split[[j]][["time"]],R.split[[j]][["conc"]],formatC(auc_ref,format="f",digits=3),formatC(aumc_ref,format="f",digits=3))
                 colnames(output)<-list("subj","time","conc", "AUC(0-t)","AUMC(0-t)")
                 show(output)

              cat("\n<<Selected data points for lambda_z estimation>>\n")
              cat("--------------------------------------------------\n")
              show(rdata.split[[j]])
              
              cat("\n<<Final PK Parameters>>\n")
              cat("----------------------------\n")
              cat("           R sq. =",R_sq ,"\n")
              cat("Adj. R sq. (ARS) =",AR_sq ,"\n")
              cat("        lambda_z =",ke ,"\n")
              cat("            Cmax =",Cmax_ref ,"\n")
              cat("            Tmax =",tmax_ref[,5] ,"\n")
              cat("            Cl/F =",Dose/aucINF,"\n")
              cat("            Vd/F =",Dose/(aucINF*ke),"\n")
              cat("         T1/2(z) =",log(2)/ke,"\n")
              cat("        AUC(0-t) =",auc_ref[length(R.split[[j]][["conc"]])],"\n")
              cat("      AUC(0-inf) =",aucINF,"\n")
              cat("       AUMC(0-t) =",aumc_ref[length(R.split[[j]][["conc"]])],"\n")
              cat("     AUMC(0-inf) =",aumcINF,"\n")
              cat("        MRT(0-t) =",(aumc_ref[length(R.split[[j]][["conc"]])])/(auc_ref[length(R.split[[j]][["conc"]])]),"\n")
              cat("      MRT(0-inf) =",aumcINF/aucINF,"\n")
              cat("--------------------------------------------------------------------------\n")
              cat("\n\n")
  }
################################################################# Test linear regression
#split dataframe into sub-dataframe by subject for test data
#"time.test" means "kel"
if(replicated){
       T.split<-split(SingleTdata, list(SingleTdata$code))
       Lm2 <- lmList(conc ~ time |code, data = test_data) 
         
      subj1<-0
      prd1<-0
      seq1<-0
      code1<-0
      AdjT<-0
       for (j in 1:(length(T.split))){
        subj1[j]<-T.split[[j]][["subj"]][1]
        prd1[j]<-T.split[[j]][["prd"]][1]
        seq1[j]<-T.split[[j]][["seq"]][1]
        code1[j]<-T.split[[j]][["code"]][1]
        AdjT[j]<-summary(Lm2)$adj.r.squared
       }
      keindex_test<-data.frame(subj=subj1, seq=seq1, prd=prd1, code=code1, time=-2.3*(coef(Lm2)[2]), 
             R_squared=summary(Lm2)$r.sq, Adj_R_squared=melt(AdjT)$value )
   }
 else{
        T.split<-split(SingleTdata, list(SingleTdata$subj))
        Lm2 <- lmList(conc ~ time |subj, data = test_data)
        
        subj1<-0
        AdjT<-0
         for (j in 1:(length(T.split))){
         subj1[j]<-T.split[[j]][["subj"]][1]
         AdjT[j]<-summary(Lm2)$adj.r.squared
            }
         keindex_test<-data.frame(subj=subj1, time=-2.3*(coef(Lm2)[2]),
              R_squared=summary(Lm2)$r.sq,Adj_R_squared=melt(AdjT)$value)
         }

  #calculate AUC
CmaxTest<-0
AUCINFTest<-0
AUCTTest<-0
TmaxTest<-0
MRTINFTest<-0
T12Test<-0
VdFTest<-0
KelTest<-0
ClFTest<-0
      for (j in 1:length(T.split)){
         #if subject of W.split==subject of kepar, then use ke of kepar to claculate AUC(0~INF)
          
          if(replicated){
          ke1<-0
          R_sq1<-0
          AR_sq1<-0
          su1<-0
          se1<-0
          pr1<-0
            for(x in 1: length(unique( keindex_test$code))){
              if (T.split[[j]][["code"]][1]==keindex_test$code[[x]]){
                  ke1<- keindex_test$time[[x]]
                  R_sq1<-keindex_test$R_squared[[x]]
                  AR_sq1<-keindex_test$Adj_R_squared[[x]]
                  su1<-keindex_test$subj[[x]]
                  se1<-keindex_test$seq[[x]]
                  pr1<-keindex_test$prd[[x]]
                 }
               }
           }
          else{
          ke1<-0
          R_sq1<-0
          AR_sq1<-0
          su1<-0
          for(x in 1: length(unique( keindex_test$subj))){
              if (T.split[[j]][["subj"]][1]==keindex_test$subj[[x]]){
                  ke1<- keindex_test$time[[x]]
                  R_sq1<-keindex_test$R_squared[[x]]
                  AR_sq1<-keindex_test$Adj_R_squared[[x]]
                  su1<-keindex_test$subj[[x]]
                 }
               }
            }    
               
          auc_test <-0
          Cmax_test<-0
          tmax_test<-0
          aumc_test<-0

          for(i in 2:length(T.split[[j]][["time"]])){
             #calculate AUC and exclude AUC==NA (auc<-0)
             auc_test[i]<-(T.split[[j]][["time"]][i]-T.split[[j]][["time"]][i-1])*(T.split[[j]][["conc"]][i]+T.split[[j]][["conc"]][i-1])* 0.5
             auc_test[i]<-auc_test[i]+auc_test[i-1]
             #calculate AUMC
             aumc_test[i]<-((T.split[[j]][["time"]][i])*(T.split[[j]][["conc"]][i])+(T.split[[j]][["time"]][i-1])*(T.split[[j]][["conc"]][i-1]))*
                          ((T.split[[j]][["time"]][i])-(T.split[[j]][["time"]][i-1]))* 0.5
             aumc_test[i]<-aumc_test[i]+aumc_test[i-1]
             Cmax_test<-max(T.split[[j]][["conc"]], na.rm = FALSE)
             subgr= c(Cmax_test)
             tmax_test<-T.split[[j]][T.split[[j]][["conc"]] %in% subgr,]
              }

              #calculate AUC (0~INF)
               auc.infinity<-T.split[[j]][["conc"]][length(T.split[[j]][["conc"]])]/ke1
               aucINF<-auc_test[length(T.split[[j]][["conc"]])]+auc.infinity

                #calculate AUMC (0~INF)
                  aumc.infinity_1<-(T.split[[j]][["conc"]][length(T.split[[j]][["conc"]])])*(T.split[[j]][["time"]][length(T.split[[j]][["time"]])])/ke1
                  aumc.infinity_2<-(T.split[[j]][["conc"]][length(T.split[[j]][["conc"]])]/(ke1^2))
                  aumcINF<-aumc_test[length(T.split[[j]][["conc"]])]+aumc.infinity_1+aumc.infinity_2

                  #for summary result
                  CmaxTest[j]<-Cmax_test
                  AUCINFTest[j]<-aucINF
                  AUCTTest[j]<-auc_test[length(T.split[[j]][["conc"]])]
                  TmaxTest[j]<-tmax_test[,5]
                  MRTINFTest[j]<-aumcINF/aucINF
                  T12Test[j]<-log(2)/ke1
                  VdFTest[j]<-Dose/(aucINF*ke1)
                  KelTest[j]<-ke1
                  ClFTest[j]<-Dose/aucINF

                 cat("\n")
                 if(replicated){
                 cat("<< NCA Outputs:- Subj.#",su1,", Seq",se1,", Prd",pr1," (Test)>>\n")
                    }
                   else{
                 cat("<< NCA Outputs:- Subj.#",su1," (Test)>>\n")
                    }
                 cat("--------------------------------------------------------------------------\n")
                 output<-data.frame(T.split[[j]][["subj"]],T.split[[j]][["time"]],T.split[[j]][["conc"]],formatC(auc_test,format="f",digits=3),formatC(aumc_test,format="f",digits=3) )
                 colnames(output)<-list("subj","time","conc", "AUC(0-t)","AUMC(0-t)")
                 show(output)

              cat("\n<<Selected data points for lambda_z estimation>>\n")
              cat("--------------------------------------------------\n")
              show(tdata.split[[j]]) 
              
              cat("\n<<Final PK Parameters>>\n")
              cat("----------------------------\n")
              cat("           R sq. =",R_sq1 ,"\n")
              cat("Adj. R sq. (ARS) =",AR_sq1 ,"\n")
              cat("        lambda_z =",ke1 ,"\n")
              cat("            Cmax =",Cmax_test ,"\n")
              cat("            Tmax =",tmax_test[,5] ,"\n")
              cat("            Cl/F =",Dose/aucINF,"\n")
              cat("            Vd/F =",Dose/(aucINF*ke1),"\n")
              cat("         T1/2(z) =",log(2)/ke1,"\n")
              cat("        AUC(0-t) =",auc_test[length(T.split[[j]][["conc"]])],"\n")
              cat("      AUC(0-inf) =",aucINF,"\n")
              cat("       AUMC(0-t) =",aumc_test[length(T.split[[j]][["conc"]])],"\n")
              cat("     AUMC(0-inf) =",aumcINF,"\n")
              cat("        MRT(0-t) =",(aumc_test[length(T.split[[j]][["conc"]])])/(auc_test[length(T.split[[j]][["conc"]])]),"\n")
              cat("      MRT(0-inf) =",aumcINF/aucINF,"\n")
              cat("--------------------------------------------------------------------------\n")
              cat("\n\n")
  }


cat("--------------------------------------------------------------------------\n")
cat("<<Output: linear regression model: conc. vs. time>>\n")
cat("\n")
   if(replicated){
      Lm1 <- lmList(conc ~ time |code, data = ref_data) 
    }
     else{
      Lm1 <- lmList(conc ~ time |subj, data = ref_data)
      }
print(Lm1)
cat("-----------------------------------------\n")
cat("Individual PK parameters for Ref. product\n")
show(keindex_ref)
cat("--------------------------------------------------------------------------\n")
cat("<<Output: linear regression model: conc. vs. time>>\n")
cat("\n")
  if(replicated){
     Lm2 <- lmList(conc ~ time |code, data = test_data)
    }
    else{
     Lm2 <- lmList(conc ~ time |subj, data = test_data)
       }
print(Lm2)
cat("-----------------------------------------\n")
cat("Individual PK parameters for Test product\n")
show(keindex_test)
cat("--------------------------------------------------------------------------\n")


 seqR<-0
 for (j in 1:(length(R.split))){
     seqR[j]<-R.split[[j]][["seq"]][1]
     }
 prdR<-0
 for (j in 1:(length(R.split))){
     prdR[j]<-R.split[[j]][["prd"]][1]
     }
 seqT<-0
 for (j in 1:(length(T.split))){
     seqT[j]<-T.split[[j]][["seq"]][1]
     }
 prdT<-0
 for (j in 1:(length(T.split))){
     prdT[j]<-T.split[[j]][["prd"]][1]
     }
 


if(replicated){
 drugR<-0
 for (j in 1:(length(R.split))){
     drugR[j]<-R.split[[j]][["drug"]][1]
     }
 drugT<-0
 for (j in 1:(length(T.split))){
     drugT[j]<-T.split[[j]][["drug"]][1]
     } 
subjR<-0
 for (j in 1:(length(R.split))){
     subjR[j]<-R.split[[j]][["subj"]][1]
     }
subjT<-0
 for (j in 1:(length(T.split))){
     subjT[j]<-T.split[[j]][["subj"]][1]
     }
description_Repdrugcode()
sumindexR<-data.frame(subj=subjR,drug=drugR,seq=seqR,prd=prdR,Cmax=CmaxRef,AUC0t=AUCTRef,AUC0INF=AUCINFRef,
                      Tmax=TmaxRef, MRTINF=MRTINFRef, T12=T12Ref, VdF=VdFRef, Kel=KelRef, ClF=ClFRef)
sumindexT<-data.frame(subj=subjT,drug=drugT,seq=seqT,prd=prdT,Cmax=CmaxTest,AUC0t=AUCTTest,AUC0INF=AUCINFTest,
                     Tmax=TmaxTest,MRTINF=MRTINFTest, T12=T12Test, VdF=VdFTest, Kel=KelTest, ClF=ClFTest) 
   }
else{
description_drugcode()
sumindexR<-data.frame(subj=subj,drug=c(1),seq=seqR,prd=prdR,Cmax=CmaxRef,AUC0t=AUCTRef,AUC0INF=AUCINFRef,
                      Tmax=TmaxRef, MRTINF=MRTINFRef, T12=T12Ref, VdF=VdFRef, Kel=KelRef, ClF=ClFRef)
sumindexT<-data.frame(subj=subj,drug=c(2),seq=seqT,prd=prdT,Cmax=CmaxTest,AUC0t=AUCTTest,AUC0INF=AUCINFTest,
                     Tmax=TmaxTest,MRTINF=MRTINFTest, T12=T12Test, VdF=VdFTest, Kel=KelTest, ClF=ClFTest)
}
#########
Total<-rbind(sumindexR,sumindexT)
TotalData<-data.frame (subj=as.factor(Total$subj), drug=as.factor(Total$drug),seq=as.factor(Total$seq),
                   prd=as.factor(Total$prd),Cmax=Total$Cmax, AUC0t=Total$AUC0t, AUC0INF=Total$AUC0INF,
                   lnCmax=log(Total$Cmax),lnAUC0t=log(Total$AUC0t),lnAUC0INF=log(Total$AUC0INF))
show(TotalData)

#Plot Cp vs Time
#creat 3(row)*2(column) multiple figure array
#Plot LogCp vs Time
#creat 3(row)*2(column) multiple figure array
 if(replicated){
     prdcount<-length(levels(TotalData$prd))
     Totalplot<-Totalplot[ do.call(order, Totalplot) ,]
     s.split<-split(Totalplot,list(Totalplot$subj))
         
     LR<-data.frame(subj=Totalplot$subj,  seq=Totalplot$seq, prd=Totalplot$prd, drug=Totalplot$drug,
                    time=Totalplot$time,  conc=Totalplot$conc, code=Totalplot$code)
     LR$conc[LR$conc == 0] <- NA
     LR <- na.omit(LR)
     Ls.split<-split(LR, list(LR$subj))
        }
    else{
    LR<-data.frame(subj=SingleRdata$subj, time=SingleRdata$time,  conc=SingleRdata$conc)
    LR$conc[LR$conc == 0] <- NA
    LR <- na.omit(LR)
    LR.split<-split(LR, list(LR$subj))

    LT<-data.frame(subj=SingleTdata$subj, time=SingleTdata$time,  conc=SingleTdata$conc)
    LT$conc[LT$conc == 0] <- NA
    LT <- na.omit(LT)
    LT.split<-split(LT, list(LT$subj))
    }

windows(record = TRUE )

if(replicated){
 for(i in seq_along(s.split)){
 main<-paste(c("Subject#", s.split[[i]]$subj[1]),collapse=" ")
     lineplot.CI(s.split[[i]]$time, s.split[[i]]$conc, group = s.split[[i]]$code, cex = 1,
                 xlab=xaxis, ylab=yaxis,cex.lab = 1, x.leg = 100000, bty="l",main=main,
                 font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n")
    # points(s.split[[i]]$time, s.split[[i]]$conc ,pch = c(1,19))[as.numeric(f)]    
   axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,
               105,110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,190,195,200),las=0)
   axis(1,at=0:100,tcl=-.2, labels=FALSE)
   axis(2,yaxp=c(0, 10000, 100),las=1,tcl=-.2, labels=FALSE)
prdcount(i,s.split, prdcount)
 }
for(i in seq_along(Ls.split)){
main<-paste(c("Subject#", Ls.split[[i]]$subj[1]),collapse=" ")
      lineplot.CI(Ls.split[[i]]$time, Ls.split[[i]]$conc, log="y", group = Ls.split[[i]]$code, cex = 1,
                  xlab = "Time", ylab = "Conc. (in log scale)",cex.lab = 1, x.leg = 100000, bty="l",main=main,
                  font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n")
     #  points(Ls.split[[i]]$time,  Ls.split[[i]]$conc ,pch = c(1,19))[as.numeric(f)] 
   axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,
              105,110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,190,195,200),las=0)
   axis(1,at=0:100,tcl=-.2, labels=FALSE)
prdcount(i,s.split, prdcount)
  }
}
else{ 
 for(i in seq_along(LT.split)){
     xx1<-LR.split[[i]]$time
     yy1<-LR.split[[i]]$conc

     xx2<-LT.split[[i]]$time
     yy2<-LT.split[[i]]$conc

        main<-paste(c("Subj. #", LT.split[[i]]$subj[1]),collapse=" ")
        plot(0, 0, log="y",xlim=range(xx1), ylim=range(yy1,yy2), ylab="Conc. (in log scale)", xlab="Time",
         main=main, cex.lab = 1.5, font.lab=2,cex.axis=1,cex.main=1,las=1,pch=19,xaxt="n")

        points(xx1,yy1,pch=19,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
        points(xx2,yy2,pch=1,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)

        lines(xx1,yy1, lty=20)
        lines(xx2,yy2, lwd=1)

        axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
        axis(1,at=0:100,tcl=-.2, labels=FALSE)
        temp <- legend("topright", legend = c("Test", "Ref."),
               text.width = strwidth("1,000,000"),
               lty = 1:2, xjust = 1, yjust = 1)
  }

for(i in seq_along(T.split)){
       xx1<-R.split[[i]]$time
       yy1<-R.split[[i]]$conc

       xx2<-T.split[[i]]$time
       yy2<-T.split[[i]]$conc

         main<-paste(c("Subj. #", T.split[[i]]$subj[1]),collapse=" ")
         plot(0, 0, xlim=range(xx1), ylim=range(yy1,yy2), xlab=xaxis, ylab=yaxis,
         main=main, cex.lab = 1.5, font.lab=2,cex.axis=1,cex.main=1,las=1,pch=1,xaxt="n")

         points(xx1,yy1,pch=19,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
         points(xx2,yy2,pch=1,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)

         lines(xx1,yy1, lty=20)
         lines(xx2,yy2, lwd=1)

         axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
         axis(1,at=0:100,tcl=-.2, labels=FALSE)
         axis(2,yaxp=c(0, 4000, 40),las=1,tcl=-.2, labels=FALSE)
         temp <- legend("topright", legend = c("Test", "Ref."),
               text.width = strwidth("1,000,000"),
               lty = 1:2, xjust = 1, yjust = 1)
  }
 }
##export with txt file
if(replicated){
      RepNCAoutput(sumindexR, sumindexT,R.split, T.split,keindex_ref,keindex_test,Dose,TotalData,rdata.split,tdata.split )
      RepNCAplot(Totalplot,SingleRdata,SingleTdata,TotalData,xaxis,yaxis)
    if (Demo){
         if(MIX){
         #Demo=TRUE, BANOVA=TRUE
         
         RepMIXanalyze(TotalData)
         }
         else{
         #Demo=TRUE, BANOVA=FALSE
         Repmenu()
          } 
        }
       else {
         if(MIX){
         ##Demo=FALSE, BANOVA=TRUE
        
         RepMIXanalyze(TotalData)
         }
          else{
         #Demo=FALSE, BANOVA=FALSE
        RepNCAsave(TotalData)
           }
         }     
    }
     else{
      NCAoutput(sumindexR, sumindexT,R.split, T.split,keindex_ref,keindex_test,Dose,TotalData,rdata.split ,tdata.split )
      NCAplot(Totalplot,SingleRdata,SingleTdata,TotalData,xaxis,yaxis)
       
       if (Demo){
         if(BANOVA){
         #Demo=TRUE, BANOVA=TRUE
         dev.off()
         BANOVAanalyze(TotalData)
         }
         else{
         #Demo=TRUE, BANOVA=FALSE
         NCAmenu()
          } 
        }
       else {
         if(BANOVA){
         ##Demo=FALSE, BANOVA=TRUE
         dev.off()
         BANOVAanalyze(TotalData)
         }
          else{
         #Demo=FALSE, BANOVA=FALSE
         NCAsave(TotalData)
           }
         }     
      }
} 


 