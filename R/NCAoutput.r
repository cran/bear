##########.txt output
options(warn=-1)
NCAoutput<-function(sumindexR, sumindexT, R.split, T.split,keindex_ref,keindex_test,Dose, TotalData)
{
cat("\n")
cat("****************************************************************************\n")
cat("* Output .txt file:                                                        *\n")
cat("*--------------------------------------------------------------------------*\n")
cat("* NCA_PK.txt:                                                              *\n")
cat("*  -->NCA_all: Cmax, Tmax, AUC0t, AUC0inf, AUC0t/AUC0inf, ln(Cmax),        *\n")
cat("*              ln(AUC0t),ln(AUC0inf), MRT0inf, T1/2, Vd/F, lambda, Cl/F    *\n")
cat("* -->PK_summaries                                                          *\n")
cat("*                                                                          *\n")
cat("* plot.pdf                                                                 *\n")
cat("****************************************************************************\n")
#1_Cmax.txt
zz <- file("NCA_PK.txt", open="wt")
sink(zz)
#############################################################################################################################   
#NCA_output
cat("\n<< NCA_all >>\n")
cat("\n")
cat("\n")
cat("                    Reference                      \n")
cat("---------------------------------------------------\n")
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
          ke<-0
          R_sq<-0
          su<-0
          for(x in 1: length(unique( keindex_ref$subj))){
              if (R.split[[j]][["subj"]][1]==keindex_ref$subj[[x]]){
                  ke<- keindex_ref$time.ref[[x]]
                  R_sq<-keindex_ref$R_square[[x]]
                  su<-keindex_ref$subj[[x]]
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
                  T12Ref[j]<-0.693/ke
                  VdFRef[j]<-Dose/(aucINF*ke)
                  KelRef[j]<-ke
                  ClFRef[j]<-Dose/aucINF
                             
                 cat("\n") 
                 cat("<< Summary Table--Ref_Subject:",su,">>\n")
                 cat("--------------------------------------------------------------------------\n")
                 output<-data.frame(R.split[[j]][["subj"]],R.split[[j]][["time"]],R.split[[j]][["conc"]],auc_ref,aumc_ref )
                 colnames(output)<-list("subj","time","conc.ref", "AUC(0~t).ref","AUMC(0~t).ref")
                 show(output)
               
              cat("\n<<Final Parameters>>\n")
              cat("-------------------------\n") 
              cat("R_square =",R_sq ,"\n")
              cat("lambda =",ke ,"\n")
              cat("Cmax =",Cmax_ref ,"\n")
              cat("Tmax =",tmax_ref[,5] ,"\n")
              cat("Cl/F =",Dose/aucINF,"\n")
              cat("Vd/F =", Dose/(aucINF*ke),"\n")
              cat("T1/2 =",0.693/ke,"\n")
              cat("AUC(0~t)=",auc_ref[length(R.split[[j]][["conc"]])],"\n") 
              cat("AUC(0~inf) =" ,aucINF,"\n")
              cat("AUMC(0~t)=",aumc_ref[length(R.split[[j]][["conc"]])],"\n")
              cat("AUMC(0~inf) =" ,aumcINF,"\n")
              cat("MRT(0~t)=",(aumc_ref[length(R.split[[j]][["conc"]])])/(auc_ref[length(R.split[[j]][["conc"]])]),"\n")
              cat("MRT(0~inf)=",aumcINF/aucINF,"\n")
              cat("--------------------------------------------------------------------------\n") 
              cat("\n\n")
  }


cat("                    Test                           \n")
cat("---------------------------------------------------\n")
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
          ke1<-0
          R_sq1<-0
          su1<-0
          for(x in 1: length(unique( keindex_test$subj))){
              if (T.split[[j]][["subj"]][1]==keindex_test$subj[[x]]){
                  ke1<- keindex_test$time.test[[x]]
                  R_sq1<-keindex_test$R_square[[x]]
                  su1<-keindex_test$subj[[x]]
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
                  T12Test[j]<-0.693/ke1
                  VdFTest[j]<-Dose/(aucINF*ke1)
                  KelTest[j]<-ke1
                  ClFTest[j]<-Dose/aucINF
                                   
                 cat("\n") 
                 cat("<< Summary Table--Test_Subject:",su1,">>\n")
                 cat("--------------------------------------------------------------------------\n")
                 output<-data.frame(T.split[[j]][["subj"]],T.split[[j]][["time"]],T.split[[j]][["conc"]],auc_test,aumc_test )
                 colnames(output)<-list("subj","time","conc.test", "AUC(0~t).test","AUMC(0~t).test")
                 show(output)
               
              cat("\n<<Final Parameters>>\n")
              cat("-------------------------\n") 
              cat("R_square =",R_sq1 ,"\n")
              cat("lambda =",ke1 ,"\n")
              cat("Cmax =",Cmax_test ,"\n")
              cat("Tmax =",tmax_test[,5] ,"\n")
              cat("Cl/F =",Dose/aucINF,"\n")
              cat("Vd/F =", Dose/(aucINF*ke1),"\n")
              cat("T1/2 =",0.693/ke1,"\n")
              cat("AUC(0~t)=",auc_test[length(T.split[[j]][["conc"]])],"\n") 
              cat("AUC(0~inf) =" ,aucINF,"\n")
              cat("AUMC(0~t)=",aumc_test[length(T.split[[j]][["conc"]])],"\n")
              cat("AUMC(0~inf) =" ,aumcINF,"\n")
              cat("MRT(0~t)=",(aumc_test[length(T.split[[j]][["conc"]])])/(auc_test[length(T.split[[j]][["conc"]])]),"\n")
              cat("MRT(0~inf)=",aumcINF/aucINF,"\n")
              cat("--------------------------------------------------------------------------\n") 
              cat("\n\n")
  }

cat("\n<< PK_summaries >>\n")
cat("\n")
cat("\n")
cat("                         Cmax                     \n")
cat("--------------------------------------------------\n")
  outputCmax<-data.frame(subject=sumindexR$subj,Test=round(sumindexT$Cmax,3),Ref=round(sumindexR$Cmax,3) ,Ratio=round(sumindexT$Cmax/sumindexR$Cmax,3))
  show(outputCmax)
  cat("\n")
  outputCmaxMean<-data.frame(summary=c("LSMEAN","S.D.","C.V(%)"),
                             Test=c(round(mean(outputCmax$Test),3),round(sd(outputCmax$Test),3),round((sd(outputCmax$Test)/mean(outputCmax$Test))*100,3)),
                             Ref=c(round(mean(outputCmax$Ref),3),round(sd(outputCmax$Ref),3),round((sd(outputCmax$Ref)/mean(outputCmax$Ref))*100,3)),
                             Ratio=c(round(mean(outputCmax$Ratio),3),round(sd(outputCmax$Ratio),3),round((sd(outputCmax$Ratio)/mean(outputCmax$Ratio))*100,3)))
  show(outputCmaxMean)
cat("--------------------------------------------------\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")

#2_Tmax
cat("                         Tmax                     \n")
cat("--------------------------------------------------\n")
  outputTmax<-data.frame(subject=sumindexR$subj,Test=round(sumindexT$Tmax,3),Ref=round(sumindexR$Tmax,3),  Ratio=round(sumindexT$Tmax/sumindexR$Tmax,3))
  show(outputTmax)
  cat("\n")
  outputTmaxMean<-data.frame(summary=c("LSMEAN","S.D.","C.V(%)"),
                             Test=c(round(mean(outputTmax$Test),3),round(sd(outputTmax$Test),3),round((sd(outputTmax$Test)/mean(outputTmax$Test))*100,3)),
                             Ref=c(round(mean(outputTmax$Ref),3),round(sd(outputTmax$Ref),3),round((sd(outputTmax$Ref)/mean(outputTmax$Ref))*100,3)),
                             Ratio=c(round(mean(outputTmax$Ratio),3),round(sd(outputTmax$Ratio),3),round((sd(outputTmax$Ratio)/mean(outputTmax$Ratio))*100,3)))
  show(outputTmaxMean)
cat("--------------------------------------------------\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")

#3_AUC0t
cat("                         AUC0t                     \n")
cat("---------------------------------------------------\n")
  outputAUC0t<-data.frame(subject=sumindexR$subj,Test=round(sumindexT$AUC0t,3),Ref=round(sumindexR$AUC0t,3),Ratio=round(sumindexT$AUC0t/sumindexR$AUC0t,3))
  show(outputAUC0t)
  cat("\n")
  outputAUC0tMean<-data.frame(summary=c("LSMEAN","S.D.","C.V(%)"),
                             Test=c(round(mean(outputAUC0t$Test),3),round(sd(outputAUC0t$Test),3),round((sd(outputAUC0t$Test)/mean(outputAUC0t$Test))*100,3)),
                             Ref=c(round(mean(outputAUC0t$Ref),3),round(sd(outputAUC0t$Ref),3),round((sd(outputAUC0t$Ref)/mean(outputAUC0t$Ref))*100,3)),
                             Ratio=c(round(mean(outputAUC0t$Ratio),3),round(sd(outputAUC0t$Ratio),3),round((sd(outputAUC0t$Ratio)/mean(outputAUC0t$Ratio))*100,3)))
  show(outputAUC0tMean)
cat("---------------------------------------------------\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")

#4_AUC0inf
cat("                         AUC0inf                    \n")
cat("---------------------------------------------------\n")
  outputAUC0INF<-data.frame(subject=sumindexR$subj,Test=round(sumindexT$AUC0INF,3),Ref=round(sumindexR$AUC0INF,3),  Ratio=round(sumindexT$AUC0INF/sumindexR$AUC0INF,3))
  show(outputAUC0INF)
  cat("\n")
  outputAUC0INFMean<-data.frame(summary=c("LSMEAN","S.D.","C.V(%)"),
                             Test=c(round(mean(outputAUC0INF$Test),3),round(sd(outputAUC0INF$Test),3),round((sd(outputAUC0INF$Test)/mean(outputAUC0INF$Test))*100,3)),
                             Ref=c(round(mean(outputAUC0INF$Ref),3),round(sd(outputAUC0INF$Ref),3),round((sd(outputAUC0INF$Ref)/mean(outputAUC0INF$Ref))*100,3)),
                             Ratio=c(round(mean(outputAUC0INF$Ratio),3),round(sd(outputAUC0INF$Ratio),3),round((sd(outputAUC0INF$Ratio)/mean(outputAUC0INF$Ratio))*100,3)))
  show(outputAUC0INFMean)
cat("---------------------------------------------------\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")

#5_AUC0t/AUC0inf
cat("                 (AUC0t/AUC0inf)*100               \n")
cat("---------------------------------------------------\n")
  outputAUC0t_AUC0INF<-data.frame(subject=sumindexR$subj,Test=round((sumindexT$AUC0t/sumindexT$AUC0INF)*100,3)
                           ,Ref=round((sumindexR$AUC0t/sumindexR$AUC0INF)*100,3)
                           ,Ratio=round(((sumindexT$AUC0t/sumindexT$AUC0INF)/(sumindexR$AUC0t/sumindexR$AUC0INF))*100,3))
  show(outputAUC0t_AUC0INF)
  cat("\n")
  outputAUC0t_AUC0INFMean<-data.frame(summary=c("LSMEAN","S.D.","C.V(%)"),
                             Test=c(round(mean(outputAUC0t_AUC0INF$Test),3),round(sd(outputAUC0t_AUC0INF$Test),3),round((sd(outputAUC0t_AUC0INF$Test)/mean(outputAUC0t_AUC0INF$Test))*100,3)),
                             Ref=c(round(mean(outputAUC0t_AUC0INF$Ref),3),round(sd(outputAUC0t_AUC0INF$Ref),3),round((sd(outputAUC0t_AUC0INF$Ref)/mean(outputAUC0t_AUC0INF$Ref))*100,3)),
                             Ratio=c(round(mean(outputAUC0t_AUC0INF$Ratio),3),round(sd(outputAUC0t_AUC0INF$Ratio),3),round((sd(outputAUC0t_AUC0INF$Ratio)/mean(outputAUC0t_AUC0INF$Ratio))*100,3)))
  show(outputAUC0t_AUC0INFMean)
cat("---------------------------------------------------\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
#6_ln(Cmax).txt
cat("                      ln(Cmax)                     \n")
cat("--------------------------------------------------\n")
  outputLnCmax<-data.frame(subject=sumindexR$subj,Test=round(log(sumindexT$Cmax),3),Ref=round(log(sumindexR$Cmax),3),  Ratio=round(log(sumindexT$Cmax)/log(sumindexR$Cmax),3))
  show(outputLnCmax)
  cat("\n")
  outputLnCmaxMean<-data.frame(summary=c("LSMEAN","S.D.","C.V(%)"),
                             Test=c(round(mean(outputLnCmax$Test),3),round(sd(outputLnCmax$Test),3),round((sd(outputLnCmax$Test)/mean(outputLnCmax$Test))*100,3)),
                             Ref=c(round(mean(outputLnCmax$Ref),3),round(sd(outputLnCmax$Ref),3),round((sd(outputLnCmax$Ref)/mean(outputLnCmax$Ref))*100,3)),
                             Ratio=c(round(mean(outputLnCmax$Ratio),3),round(sd(outputLnCmax$Ratio),3),round((sd(outputLnCmax$Ratio)/mean(outputLnCmax$Ratio))*100,3)))
  show(outputLnCmaxMean)
cat("--------------------------------------------------\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
#7_ln(AUC0t)
cat("                       ln(AUC0t)                    \n")
cat("---------------------------------------------------\n")
  outputLnAUC0t<-data.frame(subject=sumindexR$subj,Test=round(log(sumindexT$AUC0t),3),Ref=round(log(sumindexR$AUC0t),3),Ratio=round(log(sumindexT$AUC0t)/log(sumindexR$AUC0t),3))
  show(outputLnAUC0t)
  cat("\n")
  outputLnAUC0tMean<-data.frame(summary=c("LSMEAN","S.D.","C.V(%)"),
                             Test=c(round(mean(outputLnAUC0t$Test),3),round(sd(outputLnAUC0t$Test),3),round((sd(outputLnAUC0t$Test)/mean(outputLnAUC0t$Test))*100,3)),
                             Ref=c(round(mean(outputLnAUC0t$Ref),3),round(sd(outputLnAUC0t$Ref),3),round((sd(outputLnAUC0t$Ref)/mean(outputLnAUC0t$Ref))*100,3)),
                             Ratio=c(round(mean(outputLnAUC0t$Ratio),3),round(sd(outputLnAUC0t$Ratio),3),round((sd(outputLnAUC0t$Ratio)/mean(outputLnAUC0t$Ratio))*100,3)))
  show(outputLnAUC0tMean)
cat("---------------------------------------------------\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
#8_ln(AUC0inf)
cat("                       ln(AUC0inf)                 \n")
cat("---------------------------------------------------\n")
  outputLnAUC0INF<-data.frame(subject=sumindexR$subj,Test=round(log(sumindexT$AUC0INF),3),Ref=round(log(sumindexR$AUC0INF),3),
                             Ratio=round(log(sumindexT$AUC0INF)/log(sumindexR$AUC0INF),3))
  show(outputLnAUC0INF)
  cat("\n")
  outputLnAUC0INFMean<-data.frame(summary=c("LSMEAN","S.D.","C.V(%)"),
                             Test=c(round(mean(outputLnAUC0INF$Test),3),round(sd(outputLnAUC0INF$Test),3),round((sd(outputLnAUC0INF$Test)/mean(outputLnAUC0INF$Test))*100,3)),
                             Ref=c(round(mean(outputLnAUC0INF$Ref),3),round(sd(outputLnAUC0INF$Ref),3),round((sd(outputLnAUC0INF$Ref)/mean(outputLnAUC0INF$Ref))*100,3)),
                             Ratio=c(round(mean(outputLnAUC0INF$Ratio),3),round(sd(outputLnAUC0INF$Ratio),3),round((sd(outputLnAUC0INF$Ratio)/mean(outputLnAUC0INF$Ratio))*100,3)))
  show(outputLnAUC0INFMean)
cat("---------------------------------------------------\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
#9_MRT0inf
cat("                      MRT0inf                      \n")
cat("---------------------------------------------------\n")
  outputMRT0INF<-data.frame(subject=sumindexR$subj,Test=round(sumindexT$MRTINF,3),Ref=round(sumindexR$MRTINF,3),
                             Ratio=round(sumindexT$MRTINF/sumindexR$MRTINF,3))
  show(outputMRT0INF)
  cat("\n")
  outputMRT0INFMean<-data.frame(summary=c("LSMEAN","S.D.","C.V(%)"),
                             Test=c(round(mean(outputMRT0INF$Test),3),round(sd(outputMRT0INF$Test),3),round((sd(outputMRT0INF$Test)/mean(outputMRT0INF$Test))*100,3)),
                             Ref=c(round(mean(outputMRT0INF$Ref),3),round(sd(outputMRT0INF$Ref),3),round((sd(outputMRT0INF$Ref)/mean(outputMRT0INF$Ref))*100,3)),
                             Ratio=c(round(mean(outputMRT0INF$Ratio),3),round(sd(outputMRT0INF$Ratio),3),round((sd(outputMRT0INF$Ratio)/mean(outputMRT0INF$Ratio))*100,3)))
  show(outputMRT0INFMean)
cat("---------------------------------------------------\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
#10_T1/2
cat("                         T1/2                      \n")
cat("---------------------------------------------------\n")
  outputT12<-data.frame(subject=sumindexR$subj,Test=round(sumindexT$T12,3),Ref=round(sumindexR$T12,3),
                             Ratio=round(sumindexT$T12/sumindexR$T12,3))
  show(outputT12)
  cat("\n")
  outputT12Mean<-data.frame(summary=c("LSMEAN","S.D.","C.V(%)"),
                             Test=c(round(mean(outputT12$Test),3),round(sd(outputT12$Test),3),round((sd(outputT12$Test)/mean(outputT12$Test))*100,3)),
                             Ref=c(round(mean(outputT12$Ref),3),round(sd(outputT12$Ref),3),round((sd(outputT12$Ref)/mean(outputT12$Ref))*100,3)),
                             Ratio=c(round(mean(outputT12$Ratio),3),round(sd(outputT12$Ratio),3),round((sd(outputT12$Ratio)/mean(outputT12$Ratio))*100,3)))
  show(outputT12Mean)
cat("---------------------------------------------------\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
#11_Vd/F
cat("                         Vd/F                      \n")
cat("---------------------------------------------------\n")
  outputVdF<-data.frame(subject=sumindexR$subj,Test=round(sumindexT$VdF,3),Ref=round(sumindexR$VdF,3),
                             Ratio=round(sumindexT$VdF/sumindexR$VdF,3))
  show(outputVdF)
  cat("\n")
  outputVdFMean<-data.frame(summary=c("LSMEAN","S.D.","C.V(%)"),
                             Test=c(round(mean(outputVdF$Test),3),round(sd(outputVdF$Test),3),round((sd(outputVdF$Test)/mean(outputVdF$Test))*100,3)),
                             Ref=c(round(mean(outputVdF$Ref),3),round(sd(outputVdF$Ref),3),round((sd(outputVdF$Ref)/mean(outputVdF$Ref))*100,3)),
                             Ratio=c(round(mean(outputVdF$Ratio),3),round(sd(outputVdF$Ratio),3),round((sd(outputVdF$Ratio)/mean(outputVdF$Ratio))*100,3)))  
  show(outputVdFMean)
cat("---------------------------------------------------\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
#12_Lambda
cat("                      Lambda                       \n")
cat("---------------------------------------------------\n")
  outputLambda<-data.frame(subject=sumindexR$subj,Test=sumindexT$Kel,Ref=sumindexR$Kel,
                             Ratio=sumindexT$Kel/sumindexR$Kel)
  show(outputLambda)
  cat("\n")
  outputLambdaMean<-data.frame(summary=c("LSMEAN","S.D.","C.V(%)"),
                             Test=c(mean(outputLambda$Test),sd(outputLambda$Test),(sd(outputLambda$Test)/mean(outputLambda$Test))*100),
                             Ref=c(mean(outputLambda$Ref),sd(outputLambda$Ref),(sd(outputLambda$Ref)/mean(outputLambda$Ref))*100),
                             Ratio=c(mean(outputLambda$Ratio),sd(outputLambda$Ratio),(sd(outputLambda$Ratio)/mean(outputLambda$Ratio))*100))
  show(outputLambdaMean)
cat("---------------------------------------------------\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
#13_Cl/F
cat("                       Cl/F                       \n")
cat("---------------------------------------------------\n")
  outputClF<-data.frame(subject=sumindexR$subj,Test=round(sumindexT$ClF,3),Ref=round(sumindexR$ClF,3),
                             Ratio=round(sumindexT$ClF/sumindexR$ClF,3))
  show(outputClF)
  cat("\n")
  outputClFMean<-data.frame(summary=c("LSMEAN","S.D.","C.V(%)"),
                             Test=c(round(mean(outputClF$Test),3),round(sd(outputClF$Test),3),round((sd(outputClF$Test)/mean(outputClF$Test))*100,3)),
                             Ref=c(round(mean(outputClF$Ref),3),round(sd(outputClF$Ref),3),round((sd(outputClF$Ref)/mean(outputClF$Ref))*100,3)),
                             Ratio=c(round(mean(outputClF$Ratio),3),round(sd(outputClF$Ratio),3),round((sd(outputClF$Ratio)/mean(outputClF$Ratio))*100,3)))
  show(outputClFMean)
cat("---------------------------------------------------\n")
sink()

#############################################################################################################
####for present statistical analysis (GLM result)
Fdata<-split(TotalData, list(TotalData$drug))
RefData<-Fdata[[1]]
TestData<-Fdata[[2]]
Cmax<- lm(Cmax ~ seq + subj + prd + drug , data=TotalData)
AUC0t<- lm(AUC0t ~ seq + subj+ prd + drug , data=TotalData)
AUC0INF<- lm(AUC0INF ~ seq + subj+ prd + drug , data=TotalData)
LnCmax<- lm(LnCmax ~ seq + subj + prd + drug , data=TotalData)
LnAUC0t<- lm(LnAUC0t ~ seq + subj + prd + drug , data=TotalData)
LnAUC0INF<- lm(LnAUC0INF ~ seq + subj + prd + drug , data=TotalData)

#L1(Reference-->Test),L2(Test-->Reference sequence)
SeqLeg<-split(RefData, list(RefData$seq))
L1<-length(SeqLeg[[1]]$seq)
L2<-length(SeqLeg[[2]]$seq)
T<-qt(0.95,(L1+L2-2))

ref_Cmax<-mean(RefData$LnCmax)
ref_AUC0t<-mean(RefData$LnAUC0t)
ref_AUC0INF<-mean(RefData$LnAUC0INF)

test_Cmax<-mean(TestData$LnCmax)
test_AUC0t<-mean(TestData$LnAUC0t)
test_AUC0INF<-mean(TestData$LnAUC0INF)

SE_Cmax<-sqrt((anova(LnCmax)[5,3]/2) * (1/L1+1/L2)) 
SE_AUC0t<-sqrt((anova(LnAUC0t)[5,3]/2) * (1/L1+1/L2))
SE_AUC0INF<-sqrt((anova(LnAUC0INF)[5,3]/2) * (1/L1+1/L2)) 

Z_Cmax<-0.2*(ref_Cmax/SE_Cmax)-qnorm(0.95)
Z_AUC0t<-0.2*(ref_AUC0t/SE_AUC0t)-qnorm(0.95)
Z_AUC0INF<-0.2*(ref_AUC0INF/SE_AUC0INF)-qnorm(0.95)

T_Cmax<-0.2*(ref_Cmax/SE_Cmax)-qt(0.975,L1+L2-2)
T_AUC0t<-0.2*(ref_AUC0t/SE_AUC0t)-qt(0.975,L1+L2-2)
T_AUC0INF<-0.2*(ref_AUC0INF/SE_AUC0INF)-qt(0.975,L1+L2-2)

##90$CI (log transformation)
lowerCmax<-100*exp((test_Cmax-ref_Cmax)-(T*SE_Cmax))
upperCmax<-100*exp((test_Cmax-ref_Cmax)+(T*SE_Cmax))
lowerAUC0t<-100*exp((test_AUC0t-ref_AUC0t)-(T*SE_AUC0t))
UpperAUC0t<-100*exp((test_AUC0t-ref_AUC0t)+(T*SE_AUC0t))
LowerAUC0INF<-100*exp((test_AUC0INF - ref_AUC0INF)-(T*SE_AUC0INF))
UpperAUC0INF<-100*exp((test_AUC0INF - ref_AUC0INF)+(T*SE_AUC0INF))

#Table 1:Statistical Summaries for Bioequivalence Study 
zz <- file("Statistical_summaries.txt", open="wt")
sink(zz)
cat("\n")
cat("\n")
cat("\n<< Statistical summaries >>\n")
cat("\n")
cat("\n")
cat("         Statistical Summaries for Bioequivalence Study (n=",L1+L2,")\n")
cat("--------------------------------------------------------------------------\n")
cat("\n")
 outputS1<-data.frame(parameters=c("Cmax","AUC0-t","AUC0-inf","Ln(Cmax)","Ln(AUC0-t)","Ln(AUC0-inf)" ),
                      Test_Mean=c(round(mean(outputCmax$Test),3),round(mean(outputAUC0t$Test),3),round(mean(outputAUC0INF$Test),3),
                                  round(mean(outputLnCmax$Test),3),round(mean(outputLnAUC0t$Test),3),round(mean(outputLnAUC0INF$Test),3)), 
                      Test_SD=c(round(sd(outputCmax$Test),3),round(sd(outputAUC0t$Test),3),round(sd(outputAUC0INF$Test),3),
                                round(sd(outputLnCmax$Test),3),round(sd(outputLnAUC0t$Test),3),round(sd(outputLnAUC0INF$Test),3)),
                      Ref_Mean=c(round(mean(outputCmax$Ref),3),round(mean(outputAUC0t$Ref),3),round(mean(outputAUC0INF$Ref),3),
                                 round(mean(outputLnCmax$Ref),3),round(mean(outputLnAUC0t$Ref),3),round(mean(outputLnAUC0INF$Ref),3)),
                      Ref_SD=c(round(sd(outputCmax$Ref),3),round(sd(outputAUC0t$Ref),3),round(sd(outputAUC0INF$Ref),3),
                               round(sd(outputLnCmax$Ref),3),round(sd(outputLnAUC0t$Ref),3),round(sd(outputLnAUC0INF$Ref),3)),
                      Ratio=c(round(mean(outputCmax$Ratio),3),round(mean(outputAUC0t$Ratio),3),round(mean(outputAUC0INF$Ratio),3),
                              round(mean(outputLnCmax$Ratio),3),round(mean(outputLnAUC0t$Ratio),3),round(mean(outputLnAUC0INF$Ratio),3)))

show(outputS1)
cat("\n")
cat("\n")
cat("                           Statistical Analysis                           \n")
cat("--------------------------------------------------------------------------\n")
cat("\n")
outputS2<-data.frame(parameters=c("Cmax","AUC0-t","AUC0-inf","Ln(Cmax)","Ln(AUC0-t)","Ln(AUC0-inf)" ),                      
                      F_value=c(round(anova(Cmax)[4,4],3), round(anova(AUC0t)[4,4],3), round(anova(AUC0INF)[4,4],3),
                                round(anova(LnCmax)[4,4],3),round(anova(LnAUC0t)[4,4],3),round(anova(LnAUC0INF)[4,4],3)), 
                      P_value=c(round(anova(Cmax)[4,5],3),round(anova(AUC0t)[4,5],3), round(anova(AUC0INF)[4,5],3),
                          round(anova(LnCmax)[4,5],3),round(anova(LnAUC0t)[4,5],3),round(anova(LnAUC0INF)[4,5],3)),  
                      CI90_lower= c("-","-","-",round(lowerCmax,3),round(lowerAUC0t,3),round(LowerAUC0INF,3)), 
                      CI90_upper= c("-","-","-",round(upperCmax,3),round(UpperAUC0t,3),round(UpperAUC0INF,3)),  
                      power=c("-","-","-",round(pt(T_Cmax,L1+L2-2),3),round(pt(T_AUC0t,L1+L2-2),3),round(pt(T_AUC0INF,L1+L2-2),3))) 


show(outputS2)
cat("\n")
cat("-------------------------------------------------------------------------\n")
cat("Ratio=Test/Reference (mean ratio)\n")
cat("F value and P value obtained from ANOVA\n")
cat("CI90: 90% confidence interval. \n")
cat("Power; required to detect at least 20% of differences\n")
cat("--------------------------------------------------------------------------\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
#Table 2:Summaries for Pharmacokinetic Parameters 
cat("Summaries for Pharmacokinetic Parameters \n")
cat("\n")
cat("\n")
cat("                        Test                     \n")
cat("-------------------------------------------------\n")
cat("\n")
outputTest<-data.frame(parameters=c("Cl/F","Lambda","Tmax","T1/2","Vd/F","MRT0inf","AUCratio" ),
                        Mean=c(round(mean(outputClF$Test),3),round(mean(outputLambda$Test),3),round(mean(outputTmax$Test),3),round(mean(outputT12$Test),3),
                                    round(mean(outputVdF$Test),3),round(mean(outputMRT0INF$Test),3),round(mean(outputAUC0t_AUC0INF$Test),3)),
                        SD=c(round(sd(outputClF$Test),3),round(sd(outputLambda$Test),3),round(sd(outputTmax$Test),3),round(sd(outputT12$Test),3),
                                  round(sd(outputVdF$Test),3),round(sd(outputMRT0INF$Test),3),round(sd(outputAUC0t_AUC0INF$Test),3) ),
                        CV=c(round(((sd(outputClF$Test)/mean(outputClF$Test))*100),3),round(((sd(outputLambda$Test)/mean(outputLambda$Test))*100),3),
                                  round(((sd(outputTmax$Test)/mean(outputTmax$Test))*100),3),round(((sd(outputT12$Test)/mean(outputT12$Test))*100),3),
                                  round(((sd(outputVdF$Test)/mean(outputVdF$Test))*100),3),round(((sd(outputMRT0INF$Test)/mean(outputMRT0INF$Test))*100),3),
                                  round(((sd(outputAUC0t_AUC0INF$Test)/mean(outputAUC0t_AUC0INF$Test))*100),3)))
                        
show(outputTest)
cat("\n")
cat("\n")
cat("                        Reference                   \n")
cat("----------------------------------------------------\n")
cat("\n")
outputRef<-data.frame(parameters=c("Cl/F","Lambda","Tmax","T1/2","Vd/F","MRT0inf","AUCratio" ),
                        Mean=c(round(mean(outputClF$Ref),3),round(mean(outputLambda$Ref),3),round(mean(outputTmax$Ref),3),round(mean(outputT12$Ref),3),
                                   round(mean(outputVdF$Ref),3),round(mean(outputMRT0INF$Ref),3),round(mean(outputAUC0t_AUC0INF$Ref),3)),
                        SD=c(round(sd(outputClF$Ref),3),round(sd(outputLambda$Ref),3),round(sd(outputTmax$Ref),3),round(sd(outputT12$Ref),3), 
                                 round(sd(outputVdF$Ref),3),round(sd(outputMRT0INF$Ref),3),round(sd(outputAUC0t_AUC0INF$Ref),3)),
                        CV=c(round(((sd(outputClF$Ref)/mean(outputClF$Ref))*100),3),round(((sd(outputLambda$Ref)/mean(outputLambda$Ref))*100),3), 
                                 round(((sd(outputTmax$Ref)/mean(outputTmax$Ref))*100),3),round(((sd(outputT12$Ref)/mean(outputT12$Ref))*100),3),
                                 round(((sd(outputVdF$Ref)/mean(outputVdF$Ref))*100),3),round(((sd(outputMRT0INF$Ref)/mean(outputMRT0INF$Ref))*100),3),
                                 round(((sd(outputAUC0t_AUC0INF$Ref)/mean(outputAUC0t_AUC0INF$Ref))*100),3)))
show(outputRef)
cat("\n")
cat("-----------------------------------------------------\n")
cat("AUC ratio: (AUC0-t/AUC0-inf)*100 \n")
cat("-----------------------------------------------------\n")
sink()
}
 
 