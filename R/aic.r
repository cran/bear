###Aceto revised NCA codes for lambda_z (WinNolin)
options(warn=-1)
aic<-function(Dose, xaxis,yaxis,Totalplot,SingleRdata,SingleTdata,SingleRdata1,SingleTdata1,Tau, TlastD,SingleRdata0,SingleTdata0,
              separateWindows=TRUE,Demo=FALSE, BANOVA=FALSE,replicated=FALSE, MIX=FALSE, parallel=FALSE, multiple=FALSE)
{
description_AIC()
#split dataframe into sub-dataframe by subject for reference data
 if(replicated){
       R.split<-split(SingleRdata, list(SingleRdata$code))
                
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
       }
   }
  else{
        R.split<-split(SingleRdata, list(SingleRdata$subj))
               
        subj<-0
        AdjR<-0
         for (j in 1:(length(R.split))){
          subj[j]<-R.split[[j]][["subj"]][1]
         }
       }

     #fitting data with linear regression model
     #cat("<<Output: linear regression model: conc. vs. time>>\n")
     #calculate AUC
      CmaxRef<-0
      CminRef<-0
      AUCINFRef<-0
      AUCTRef<-0
      TmaxRef<-0
      MRTINFRef<-0
      T12Ref<-0
      VdFRef<-0
      KelRef<-0
      ClFRef<-0
      CavRef<-0
      FluRef<-0
      ke<-0
      R_sq<-0
      AR_sq<-0
      aic<-0
      for(j in seq_along(R.split)){
      
         xr<-which.max(R.split[[j]]$conc)
         f1 <-  function(xr) return(cbind((nrow(R.split[[j]])-xr+1),
               (extractAIC(lm(log(conc)~time,R.split[[j]][xr:nrow(R.split[[j]]),])))[2],
               summary(lm(log(conc)~time,R.split[[j]][xr:nrow(R.split[[j]]),]))$adj.r.squared))
         overview <- as.data.frame(do.call(rbind,lapply((xr+1):(nrow(R.split[[j]])-2),f1)))
         names(overview) <- c("n","AIC","adjR2")

          n_AIC<-overview$n[which.min(overview$AIC)]
          Lm1<-lm(log(conc)~time,R.split[[j]][(nrow(R.split[[j]])-n_AIC+1):nrow(R.split[[j]]),])

           ke[j]<-(-coef(Lm1)[2])
           R_sq[j]<-summary(Lm1)$r.sq
           AR_sq[j]<-summary(Lm1)$adj.r.squared
           aic[j]<-min(overview$AIC)
           
             auc_ref <-0
             tmax_ref<-0
             Cmax_ref<-0
             Cmin_ref<-0
             aumc_ref<-0
            for(i in 2:length(R.split[[j]][["time"]])){
             #calculate AUC and exclude AUC==NA (auc<-0)
             auc_ref[i]<-(R.split[[j]][["time"]][i]-R.split[[j]][["time"]][i-1])*(R.split[[j]][["conc"]][i]+R.split[[j]][["conc"]][i-1])* 0.5
             auc_ref[i]<-auc_ref[i]+auc_ref[i-1]
             #calculate AUMC
              if(multiple){
                aumc_ref[i]<-((R.split[[j]][["time"]][i]-TlastD)*(R.split[[j]][["conc"]][i])+(R.split[[j]][["time"]][i-1]-TlastD)*(R.split[[j]][["conc"]][i-1]))*
                          ((R.split[[j]][["time"]][i]-TlastD)-(R.split[[j]][["time"]][i-1]-TlastD))* 0.5
               }
              else{
                aumc_ref[i]<-((R.split[[j]][["time"]][i])*(R.split[[j]][["conc"]][i])+(R.split[[j]][["time"]][i-1])*(R.split[[j]][["conc"]][i-1]))*
                          ((R.split[[j]][["time"]][i])-(R.split[[j]][["time"]][i-1]))* 0.5
                }
             aumc_ref[i]<-aumc_ref[i]+aumc_ref[i-1]
             Cmax_ref<-max(R.split[[j]][["conc"]], na.rm = FALSE)
             Cmin_ref<-min(R.split[[j]][["conc"]], na.rm = FALSE)
              }

              #calculate AUC (0~INF)
               auc.infinity<-R.split[[j]][["conc"]][length(R.split[[j]][["conc"]])]/ke[j]
               aucINF<-auc_ref[length(R.split[[j]][["conc"]])]+auc.infinity

                #calculate AUMC (0~INF)
                  if(multiple){
                  aumc.infinity_1<-(R.split[[j]][["conc"]][length(R.split[[j]][["conc"]])])*(R.split[[j]][["time"]][length(R.split[[j]][["time"]])]-TlastD)/ke[j]
                   }
                  else{
                  aumc.infinity_1<-(R.split[[j]][["conc"]][length(R.split[[j]][["conc"]])])*(R.split[[j]][["time"]][length(R.split[[j]][["time"]])])/ke[j]
                   }
                  aumc.infinity_2<-(R.split[[j]][["conc"]][length(R.split[[j]][["conc"]])]/(ke[j]^2))
                  aumcINF<-aumc_ref[length(R.split[[j]][["conc"]])]+aumc.infinity_1+aumc.infinity_2

                   #for summary result
                  CmaxRef[j]<-Cmax_ref
                  CminRef[j]<-Cmin_ref
                  AUCINFRef[j]<-aucINF
                  AUCTRef[j]<-auc_ref[length(R.split[[j]][["conc"]])]
                  TmaxRef[j]<-R.split[[j]]$time[xr] #R.split[[j]][xr,5]->R.split[[j]]$time[xr]
                  T12Ref[j]<-log(2)/ke[j]
                  KelRef[j]<-ke[j]
                                         
                   if(multiple){
                       VdFRef[j]<-Dose/((auc_ref[length(R.split[[j]][["conc"]])])*ke[j])
                       ClFRef[j]<-Dose/(auc_ref[length(R.split[[j]][["conc"]])])
                       MRTINFRef[j]<-(aumc_ref[length(R.split[[j]][["conc"]])])/(auc_ref[length(R.split[[j]][["conc"]])])
                       CavRef[j]<-(auc_ref[length(R.split[[j]][["conc"]])])/Tau
                       FluRef[j]<-((Cmax_ref-Cmin_ref)/((auc_ref[length(R.split[[j]][["conc"]])])/Tau))*100
                      }
                      else{
                       VdFRef[j]<-Dose/(aucINF*ke[j])
                       ClFRef[j]<-Dose/aucINF
                       MRTINFRef[j]<-aumcINF/aucINF 
                      }

                 cat("\n")
                 if(replicated){
                 cat("<< NCA Outputs:- Subj.#",R.split[[j]][["subj"]][1],", Seq",R.split[[j]][["seq"]][1],", Prd",R.split[[j]][["prd"]][1]," (Ref.)>>\n")
                    }
                   else{
                 cat("<< NCA Outputs:- Subj.#",R.split[[j]][["subj"]][1]," (Ref.)>>\n")
                    }
                 cat("--------------------------------------------------------------------------\n")
                 output<-data.frame(R.split[[j]][["subj"]],R.split[[j]][["time"]],R.split[[j]][["conc"]],formatC(auc_ref,format="f",digits=3),formatC(aumc_ref,format="f",digits=3))
                 if(multiple){
                  colnames(output)<-list("subj","time","conc", "AUC(tau)ss","AUMC(tau)ss")
                  }
                  else{
                  colnames(output)<-list("subj","time","conc", "AUC(0-t)","AUMC(0-t)")
                  }
                 show(output)

                 cat("--------------------------------------------------------------------------\n")
                 cat("<<Output: linear regression model: conc. vs. time>>\n")
                 print( summary(Lm1))

                 cat("\n<<Selected data points for lambda_z estimation>>\n")
                 cat("--------------------------------------------------\n")
                 show(R.split[[j]][(nrow(R.split[[j]])-n_AIC+1):nrow(R.split[[j]]),])


              cat("\n<<Final PK Parameters>>\n")
              cat("----------------------------\n")
               if(multiple){
              cat("           R sq. =",R_sq[j] ,"\n")
              cat("Adj. R sq. (ARS) =",AR_sq[j] ,"\n")
              cat("        lambda_z =",ke[j] ,"\n")
              cat("         Cmax_ss =",Cmax_ref ,"\n")
              cat("         Cmin_ss =",Cmin_ref ,"\n")
              cat("         Tmax_ss =",TmaxRef[j] ,"\n")
              cat("             Cav =",(auc_ref[length(R.split[[j]][["conc"]])])/Tau ,"\n")
              cat("  Fluctuation(%) =",((Cmax_ref-Cmin_ref)/((auc_ref[length(R.split[[j]][["conc"]])])/Tau))*100 ,"\n")
              cat("            Cl/F =",Dose/(auc_ref[length(R.split[[j]][["conc"]])]),"\n")
              cat("            Vd/F =",Dose/((auc_ref[length(R.split[[j]][["conc"]])])*ke[j]),"\n")
              cat("         T1/2(z) =",log(2)/ke[j],"\n")
              cat("      AUC(tau)ss =",auc_ref[length(R.split[[j]][["conc"]])],"\n")
              cat("     AUMC(tau)ss =",aumc_ref[length(R.split[[j]][["conc"]])],"\n")
              cat("             MRT =",(aumc_ref[length(R.split[[j]][["conc"]])])/(auc_ref[length(R.split[[j]][["conc"]])]),"\n")
              cat("--------------------------------------------------------------------------\n")
              cat("\n\n")
                }
              else{
              cat("           R sq. =",R_sq[j] ,"\n")
              cat("Adj. R sq. (ARS) =",AR_sq[j] ,"\n")
              cat("             AIC =",aic[j] ,"\n")
              cat("        lambda_z =",ke[j] ,"\n")
              cat("            Cmax =",Cmax_ref ,"\n")
              cat("            Tmax =",TmaxRef[j] ,"\n")
              cat("            Cl/F =",Dose/aucINF,"\n")
              cat("            Vd/F =",Dose/(aucINF*ke[j]),"\n")
              cat("         T1/2(z) =",log(2)/ke[j],"\n")
              cat("        AUC(0-t) =",auc_ref[length(R.split[[j]][["conc"]])],"\n")
              cat("      AUC(0-inf) =",aucINF,"\n")
              cat("       AUMC(0-t) =",aumc_ref[length(R.split[[j]][["conc"]])],"\n")
              cat("     AUMC(0-inf) =",aumcINF,"\n")
              cat("        MRT(0-t) =",(aumc_ref[length(R.split[[j]][["conc"]])])/(auc_ref[length(R.split[[j]][["conc"]])]),"\n")
              cat("      MRT(0-inf) =",aumcINF/aucINF,"\n")
              cat("--------------------------------------------------------------------------\n")
              cat("\n\n")
                }
              }
ke_melt<-melt(ke)
R_melt<-melt(R_sq)
AR_melt<-melt(AR_sq)
aic_melt<-melt(aic)

#split dataframe into sub-dataframe by subject for test data
if(replicated){
       T.split<-split(SingleTdata, list(SingleTdata$code))
         
      subj1<-0
      prd1<-0
      seq1<-0
      code1<-0
       for (j in 1:(length(T.split))){
        subj1[j]<-T.split[[j]][["subj"]][1]
        prd1[j]<-T.split[[j]][["prd"]][1]
        seq1[j]<-T.split[[j]][["seq"]][1]
        code1[j]<-T.split[[j]][["code"]][1]
       }
   }
 else{
        T.split<-split(SingleTdata, list(SingleTdata$subj))
               
        subj1<-0
        for (j in 1:(length(T.split))){
         subj1[j]<-T.split[[j]][["subj"]][1]
            }
         }

#fitting data with linear regression model
#cat("<<Output: linear regression model: conc. vs. time>>\n")
  #calculate AUC
    CmaxTest<-0
    CminTest<-0
    AUCINFTest<-0
    AUCTTest<-0
    TmaxTest<-0
    MRTINFTest<-0
    T12Test<-0
    VdFTest<-0
    KelTest<-0
    ClFTest<-0
    CavTest<-0
    FluTest<-0
    ke1<-0
    R_sq1<-0
    AR_sq1<-0
    aic1<-0
    for(j in seq_along(T.split)){
      xt<-which.max(T.split[[j]]$conc)
      f2 <-  function(xt) return(cbind((nrow(T.split[[j]])-xt+1),
            (extractAIC(lm(log(conc)~time,T.split[[j]][xt:nrow(T.split[[j]]),])))[2],
             summary(lm(log(conc)~time,T.split[[j]][xt:nrow(T.split[[j]]),]))$adj.r.squared))
      overview <- as.data.frame(do.call(rbind,lapply((xt+1):(nrow(T.split[[j]])-2),f2)))
      names(overview) <- c("n","AIC","adjR2")

       n_AIC<-overview$n[which.min(overview$AIC)]
       Lm2<-lm(log(conc)~time,T.split[[j]][(nrow(T.split[[j]])-n_AIC+1):nrow(T.split[[j]]),])

         ke1[j]<-(-coef(Lm2)[2])
         R_sq1[j]<-summary(Lm2)$r.sq
         AR_sq1[j]<-summary(Lm2)$adj.r.squared
         aic1[j]<-min(overview$AIC)
          auc_test <-0
          Cmax_test<-0
          tmax_test<-0
          aumc_test<-0

          for(i in 2:length(T.split[[j]][["time"]])){
             #calculate AUC and exclude AUC==NA (auc<-0)
             auc_test[i]<-(T.split[[j]][["time"]][i]-T.split[[j]][["time"]][i-1])*(T.split[[j]][["conc"]][i]+T.split[[j]][["conc"]][i-1])* 0.5
             auc_test[i]<-auc_test[i]+auc_test[i-1]
             #calculate AUMC
             if(multiple){
             aumc_test[i]<-((T.split[[j]][["time"]][i]-TlastD)*(T.split[[j]][["conc"]][i])+(T.split[[j]][["time"]][i-1]-TlastD)*(T.split[[j]][["conc"]][i-1]))*
                          ((T.split[[j]][["time"]][i]-TlastD)-(T.split[[j]][["time"]][i-1]-TlastD))* 0.5
              }
             else{
             aumc_test[i]<-((T.split[[j]][["time"]][i])*(T.split[[j]][["conc"]][i])+(T.split[[j]][["time"]][i-1])*(T.split[[j]][["conc"]][i-1]))*
                          ((T.split[[j]][["time"]][i])-(T.split[[j]][["time"]][i-1]))* 0.5
                }
             aumc_test[i]<-aumc_test[i]+aumc_test[i-1]
             Cmax_test<-max(T.split[[j]][["conc"]], na.rm = FALSE)
             Cmin_test<-min(T.split[[j]][["conc"]], na.rm = FALSE)
             }

              #calculate AUC (0~INF)
               auc.infinity<-T.split[[j]][["conc"]][length(T.split[[j]][["conc"]])]/ke1[j]
               aucINF<-auc_test[length(T.split[[j]][["conc"]])]+auc.infinity

                #calculate AUMC (0~INF)
                  if(multiple){
                   aumc.infinity_1<-(T.split[[j]][["conc"]][length(T.split[[j]][["conc"]])])*(T.split[[j]][["time"]][length(T.split[[j]][["time"]])]-TlastD)/ke1[j]
                   }
                   else{
                  aumc.infinity_1<-(T.split[[j]][["conc"]][length(T.split[[j]][["conc"]])])*(T.split[[j]][["time"]][length(T.split[[j]][["time"]])])/ke1[j]
                  }
                  aumc.infinity_2<-(T.split[[j]][["conc"]][length(T.split[[j]][["conc"]])]/(ke1[j]^2))
                  aumcINF<-aumc_test[length(T.split[[j]][["conc"]])]+aumc.infinity_1+aumc.infinity_2

                  #for summary result
                  CmaxTest[j]<-Cmax_test
                  CminTest[j]<-Cmin_test
                  AUCINFTest[j]<-aucINF
                  AUCTTest[j]<-auc_test[length(T.split[[j]][["conc"]])]
                  TmaxTest[j]<-T.split[[j]]$time[xt] #T.split[[j]][xt,5] -->T.split[[j]]$time[xt]
                  T12Test[j]<-log(2)/ke1[j]
                  KelTest[j]<-ke1[j]
                     if(multiple){
                  VdFTest[j]<-Dose/((auc_test[length(T.split[[j]][["conc"]])])*ke1[j])
                  ClFTest[j]<-Dose/(auc_test[length(T.split[[j]][["conc"]])])
                  MRTINFTest[j]<-(aumc_test[length(T.split[[j]][["conc"]])])/(auc_test[length(T.split[[j]][["conc"]])])
                  CavTest[j]<-(auc_test[length(T.split[[j]][["conc"]])])/Tau
                  FluTest[j]<-((Cmax_test-Cmin_test)/((auc_test[length(T.split[[j]][["conc"]])])/Tau))*100
                    }
                    else{
                  VdFTest[j]<-Dose/(aucINF*ke1[j])
                  ClFTest[j]<-Dose/aucINF
                  MRTINFTest[j]<-aumcINF/aucINF     
                        }
                 cat("\n")
                 if(replicated){
                 cat("<< NCA Outputs:- Subj.#",T.split[[j]][["subj"]][1],", Seq",T.split[[j]][["seq"]][1],", Prd",T.split[[j]][["prd"]][1]," (Test)>>\n")
                    }
                   else{
                 cat("<< NCA Outputs:- Subj.#",T.split[[j]][["subj"]][1]," (Test)>>\n")
                    }
                 cat("--------------------------------------------------------------------------\n")
                 output<-data.frame(T.split[[j]][["subj"]],T.split[[j]][["time"]],T.split[[j]][["conc"]],formatC(auc_test,format="f",digits=3),formatC(aumc_test,format="f",digits=3) )
                 if(multiple){
                  colnames(output)<-list("subj","time","conc", "AUC(tau)ss","AUMC(tau)ss")
                  }
                  else{
                  colnames(output)<-list("subj","time","conc", "AUC(0-t)","AUMC(0-t)")
                  }
                 show(output)

                 cat("--------------------------------------------------------------------------\n")
                 cat("<<Output: linear regression model: conc. vs. time>>\n")
                 print(summary(Lm2))

                 cat("\n<<Selected data points for lambda_z estimation>>\n")
                 cat("--------------------------------------------------\n")
                 show(T.split[[j]][(nrow(T.split[[j]])-n_AIC+1):nrow(T.split[[j]]),])


              cat("\n<<Final PK Parameters>>\n")
              cat("----------------------------\n")
              if(multiple){
              cat("           R sq. =",R_sq1[j] ,"\n")
              cat("Adj. R sq. (ARS) =",AR_sq1[j] ,"\n")
              cat("        lambda_z =",ke1[j] ,"\n")
              cat("         Cmax_ss =",Cmax_test ,"\n")
              cat("         Cmin_ss =",Cmin_test ,"\n")
              cat("         Tmax_ss =",TmaxTest[j] ,"\n")
              cat("             Cav =",(auc_test[length(T.split[[j]][["conc"]])])/Tau ,"\n")
              cat("  Fluctuation(%) =",((Cmax_test-Cmin_test)/((auc_test[length(T.split[[j]][["conc"]])])/Tau))*100 ,"\n")
              cat("            Cl/F =",Dose/(auc_test[length(T.split[[j]][["conc"]])]),"\n")
              cat("            Vd/F =",Dose/((auc_test[length(T.split[[j]][["conc"]])])*ke1[j]),"\n")
              cat("         T1/2(z) =",log(2)/ke1[j],"\n")
              cat("      AUC(tau)ss =",auc_test[length(T.split[[j]][["conc"]])],"\n")
              cat("     AUMC(tau)ss =",aumc_test[length(T.split[[j]][["conc"]])],"\n")
              cat("             MRT =",(aumc_test[length(T.split[[j]][["conc"]])])/(auc_test[length(T.split[[j]][["conc"]])]),"\n")
              cat("--------------------------------------------------------------------------\n")
              cat("\n\n") 
              }
              else{  
              cat("           R sq. =",R_sq1[j] ,"\n")
              cat("Adj. R sq. (ARS) =",AR_sq1[j] ,"\n")
              cat("             AIC =",aic1[j] ,"\n")
              cat("        lambda_z =",ke1[j] ,"\n")
              cat("            Cmax =",Cmax_test ,"\n")
              cat("            Tmax =",TmaxTest[j] ,"\n")
              cat("            Cl/F =",Dose/aucINF,"\n")
              cat("            Vd/F =",Dose/(aucINF*ke1[j]),"\n")
              cat("         T1/2(z) =",log(2)/ke1[j],"\n")
              cat("        AUC(0-t) =",auc_test[length(T.split[[j]][["conc"]])],"\n")
              cat("      AUC(0-inf) =",aucINF,"\n")
              cat("       AUMC(0-t) =",aumc_test[length(T.split[[j]][["conc"]])],"\n")
              cat("     AUMC(0-inf) =",aumcINF,"\n")
              cat("        MRT(0-t) =",(aumc_test[length(T.split[[j]][["conc"]])])/(auc_test[length(T.split[[j]][["conc"]])]),"\n")
              cat("      MRT(0-inf) =",aumcINF/aucINF,"\n")
              cat("--------------------------------------------------------------------------\n")
              cat("\n\n")
              }
 }
ke1_melt<-melt(ke1)
R1_melt<-melt(R_sq1)
AR1_melt<-melt(AR_sq1)
aic1_melt<-melt(aic1)

if(replicated){
keindex_ref<-data.frame(subj=subj, seq=seq, prd=prd, code=code,  time=ke_melt$value, R_squared=R_melt$value,Adj_R_squared=AR_melt$value,AIC=aic_melt$value)
#"time.test" means "kel"
keindex_test<-data.frame(subj=subj1, seq=seq1, prd=prd1, code=code1,time=ke1_melt$value,R_squared=R1_melt$value,Adj_R_squared=AR1_melt$value, AIC=aic1_melt$value)
 }
else{
#"time.ref" means "kel"
keindex_ref<-data.frame(subj=subj, time=ke_melt$value, R_squared=R_melt$value,Adj_R_squared=AR_melt$value,AIC=aic_melt$value)
#"time.test" means "kel"
keindex_test<-data.frame(subj=subj1, time=ke1_melt$value,R_squared=R1_melt$value,Adj_R_squared=AR1_melt$value, AIC=aic1_melt$value)
}
cat("-------------------------------------------------------\n")
cat("Individual PK parameters for Ref. product\n")
cat("\n")
show(keindex_ref)
cat("-------------------------------------------------------\n")
cat("Individual PK parameters for Test product\n")
cat("\n")
show(keindex_test)
cat("-------------------------------------------------------\n")

subjR<-0
 for (j in 1:(length(R.split))){
     subjR[j]<-R.split[[j]][["subj"]][1]
     }
subjT<-0
 for (j in 1:(length(T.split))){
     subjT[j]<-T.split[[j]][["subj"]][1]
     }
if(parallel){
  if(multiple){
  description_Multipledrugcode() 
  sumindexR<-data.frame(subj=subjR,drug=c(1),Cmax=CmaxRef,AUC0t=AUCTRef,Cmin=CminRef,
                       Tmax=TmaxRef, MRTINF=MRTINFRef, T12=T12Ref, VdF=VdFRef, Kel=KelRef, ClF=ClFRef, Cav=CavRef, Flu=FluRef)
  sumindexT<-data.frame(subj=subjT,drug=c(2),Cmax=CmaxTest,AUC0t=AUCTTest,Cmin=CminTest,
                       Tmax=TmaxTest,MRTINF=MRTINFTest, T12=T12Test, VdF=VdFTest, Kel=KelTest, ClF=ClFTest, Cav=CavTest, Flu=FluTest)
  }
  else{
sumindexR<-data.frame(subj=subjR,drug=c(1),Cmax=CmaxRef,AUC0t=AUCTRef,AUC0INF=AUCINFRef,
                      Tmax=TmaxRef, MRTINF=MRTINFRef, T12=T12Ref, VdF=VdFRef, Kel=KelRef, ClF=ClFRef)
sumindexT<-data.frame(subj=subjT,drug=c(2),Cmax=CmaxTest,AUC0t=AUCTTest,AUC0INF=AUCINFTest,
                     Tmax=TmaxTest,MRTINF=MRTINFTest, T12=T12Test, VdF=VdFTest, Kel=KelTest, ClF=ClFTest) 
  }
}
else{
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
description_Repdrugcode()
sumindexR<-data.frame(subj=subjR,drug=drugR,seq=seqR,prd=prdR,Cmax=CmaxRef,AUC0t=AUCTRef,AUC0INF=AUCINFRef,
                      Tmax=TmaxRef, MRTINF=MRTINFRef, T12=T12Ref, VdF=VdFRef, Kel=KelRef, ClF=ClFRef)
sumindexT<-data.frame(subj=subjT,drug=drugT,seq=seqT,prd=prdT,Cmax=CmaxTest,AUC0t=AUCTTest,AUC0INF=AUCINFTest,
                     Tmax=TmaxTest,MRTINF=MRTINFTest, T12=T12Test, VdF=VdFTest, Kel=KelTest, ClF=ClFTest) 
   }
else{

if(multiple){
        description_Multipledrugcode()
          sumindexR<-data.frame(subj=subjR,drug=c(1),seq=seqR,prd=prdR,Cmax=CmaxRef,AUC0t=AUCTRef,Cmin=CminRef,
                      Tmax=TmaxRef, MRTINF=MRTINFRef, T12=T12Ref, VdF=VdFRef, Kel=KelRef, ClF=ClFRef, Cav=CavRef, Flu=FluRef)
          sumindexT<-data.frame(subj=subjT,drug=c(2),seq=seqT,prd=prdT,Cmax=CmaxTest,AUC0t=AUCTTest,Cmin=CminTest,
                     Tmax=TmaxTest,MRTINF=MRTINFTest, T12=T12Test, VdF=VdFTest, Kel=KelTest, ClF=ClFTest, Cav=CavTest, Flu=FluTest)
         }
        else{
        description_drugcode() 
          sumindexR<-data.frame(subj=subjR,drug=c(1),seq=seqR,prd=prdR,Cmax=CmaxRef,AUC0t=AUCTRef,AUC0INF=AUCINFRef,
                      Tmax=TmaxRef, MRTINF=MRTINFRef, T12=T12Ref, VdF=VdFRef, Kel=KelRef, ClF=ClFRef)
          sumindexT<-data.frame(subj=subjT,drug=c(2),seq=seqT,prd=prdT,Cmax=CmaxTest,AUC0t=AUCTTest,AUC0INF=AUCINFTest,
                     Tmax=TmaxTest,MRTINF=MRTINFTest, T12=T12Test, VdF=VdFTest, Kel=KelTest, ClF=ClFTest)
        }
 }
} 
#########
Total<-rbind(sumindexR,sumindexT)
if(parallel){
  if(multiple){
  TotalData<-data.frame (subj=as.factor(Total$subj), drug=as.factor(Total$drug),Cmax_ss=Total$Cmax, AUCtau_ss=Total$AUC0t,
                         lnCmax_ss=log(Total$Cmax),lnAUCtau_ss=log(Total$AUC0t))
  }
  else{
TotalData<-data.frame (subj=as.factor(Total$subj), drug=as.factor(Total$drug),Cmax=Total$Cmax, AUC0t=Total$AUC0t, AUC0INF=Total$AUC0INF,
                      lnCmax=log(Total$Cmax),lnAUC0t=log(Total$AUC0t),lnAUC0INF=log(Total$AUC0INF))
  }
}
else{
 if(multiple){
  TotalData<-data.frame (subj=as.factor(Total$subj), drug=as.factor(Total$drug),seq=as.factor(Total$seq),
                   prd=as.factor(Total$prd),Cmax_ss=Total$Cmax, AUCtau_ss=Total$AUC0t,lnCmax_ss=log(Total$Cmax),lnAUCtau_ss=log(Total$AUC0t))
  }
  else{
   TotalData<-data.frame (subj=as.factor(Total$subj), drug=as.factor(Total$drug),seq=as.factor(Total$seq),
                   prd=as.factor(Total$prd),Cmax=Total$Cmax, AUC0t=Total$AUC0t, AUC0INF=Total$AUC0INF,
                   lnCmax=log(Total$Cmax),lnAUC0t=log(Total$AUC0t),lnAUC0INF=log(Total$AUC0INF))
   }
}
show(TotalData)

#Plot Cp vs Time
 #creat 3(row)*2(column) multiple figure array
 #Plot LogCp vs Time
   #creat 3(row)*2(column) multiple figure array
Totalplot<-Totalplot[ do.call(order, Totalplot) ,]
s.split<-split(Totalplot,list(Totalplot$subj))
if(parallel){
 if(multiple){
    paraR.split<-split(SingleRdata1, list(SingleRdata1$subj))
    paraT.split<-split(SingleTdata1, list(SingleTdata1$subj))
    
    RR.split<-split(SingleRdata0, list(SingleRdata0$subj))
    TT.split<-split(SingleTdata0, list(SingleTdata0$subj))
   }
   else{
    Totalplot$conc[Totalplot$conc == 0] <- NA
    Totalplot <- na.omit(Totalplot)
    Totalplot.split<-split(Totalplot, list(Totalplot$subj))
 
    Totalplotpara<-split( Totalplot, list(Totalplot$drug))
    paraR.split<-split( Totalplotpara[[1]], list(Totalplotpara[[1]]$subj))
    paraT.split<-split( Totalplotpara[[2]], list(Totalplotpara[[2]]$subj))  
     }
  }
 else{
 if(replicated){
     prdcount<-length(levels(TotalData$prd))
     #f <- factor(Totalplot$drug)
     LR<-data.frame(subj=Totalplot$subj,  seq=Totalplot$seq, prd=Totalplot$prd, drug=Totalplot$drug,
                    time=Totalplot$time,  conc=Totalplot$conc, code=Totalplot$code)
     LR$conc[LR$conc == 0] <- NA
     LR <- na.omit(LR)
     Ls.split<-split(LR, list(LR$subj))
        }
  else{
     if(multiple){
     LR<-data.frame(subj=SingleRdata0$subj, time=SingleRdata0$time,  conc=SingleRdata0$conc)
     LR$conc[LR$conc == 0] <- NA
     LR <- na.omit(LR)
     LR.split<-split(LR, list(LR$subj))
     
     LT<-data.frame(subj=SingleTdata0$subj, time=SingleTdata0$time,  conc=SingleTdata0$conc)
     LT$conc[LT$conc == 0] <- NA
     LT <- na.omit(LT)
     LT.split<-split(LT, list(LT$subj))
         RR.split<-split(SingleRdata0, list(SingleRdata0$subj))
         TT.split<-split(SingleTdata0, list(SingleTdata0$subj))
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
   }
}   
windows(record = TRUE )
if(replicated){
    plotsingle.Rep(Ls.split, s.split,xaxis,yaxis,i, prdcount ) 
   }
else{ 
 if(parallel){
    if(multiple){
       Multipleplotsingle.para(R.split,T.split,paraR.split,paraT.split,xaxis,yaxis,RR.split,TT.split,TlastD )
       }
      else{
        plotsingle.para(R.split, T.split, paraR.split,paraT.split,xaxis,yaxis )
      }
  }
  else{ 
    if(multiple){
       Multipleplotsingle(LR.split,LT.split,RR.split,TT.split,xaxis,yaxis,TlastD )
       }
      else{
       plotsingle(LR.split,LT.split,xaxis,yaxis,R.split,T.split )
      }
  }
}  
##export with txt file
if(replicated){
RepAICoutput(sumindexR, sumindexT,R.split,T.split,keindex_ref,keindex_test,Dose,TotalData)
RepNCAplot(Totalplot,SingleRdata,SingleTdata,TotalData,xaxis,yaxis)
    if (Demo){
         #Demo=TRUE, MIX=FALSE
         Repmenu() 
        }
     else {
         if(MIX){
         ##Demo=FALSE, MIX=TRUE
         RepMIXanalyze(TotalData)
         }
          else{
         #Demo=FALSE, MIX=FALSE
         RepNCAsave(TotalData)
           }
         }     
 }
else{
##export with txt file
 if(parallel){
    if(multiple){
        SingleRdata<-SingleRdata0
        SingleTdata<-SingleTdata0
        MultipleParaAICoutput(sumindexR, sumindexT,R.split, T.split,keindex_ref,keindex_test,Dose,TotalData, Tau, TlastD)
        MultipleParaNCAplot(Totalplot,SingleRdata,SingleTdata,TotalData,xaxis,yaxis,TlastD)
          if (Demo){
          #Demo=TRUE, BANOVA=FALSE
          MultipleParamenu()
          }
           else {
             if(MIX){
            ##Demo=FALSE, BANOVA=TRUE
             MultipleParaMIXanalyze(TotalData)
                }
             else{
             #Demo=FALSE, BANOVA=FALSE
             MultipleParaNCAsave(TotalData)
               }
             }
        }
    else{ 
      ParaAICoutput(sumindexR, sumindexT,R.split, T.split,keindex_ref,keindex_test,Dose,TotalData )
      ParaNCAplot(Totalplot,SingleRdata,SingleTdata,TotalData,xaxis,yaxis) 
      if (Demo){
         #Demo=TRUE, MIX=FALSE
         Paramenu() 
        }
       else {
         if(MIX){
         ##Demo=FALSE, MIX=TRUE
         
         ParaMIXanalyze(TotalData)
         }
          else{
         #Demo=FALSE, MIX=FALSE
         ParaNCAsave(TotalData)
           }
         }     
      }
    }  
      else{
        if(multiple){
        SingleRdata<-SingleRdata0
        SingleTdata<-SingleTdata0
        MultipleAICoutput(sumindexR, sumindexT,R.split, T.split,keindex_ref,keindex_test,Dose,TotalData, Tau, TlastD)
        MultipleNCAplot(Totalplot,SingleRdata,SingleTdata,TotalData,xaxis,yaxis,TlastD)
          if (Demo){
          #Demo=TRUE, BANOVA=FALSE
          MultipleNCAmenu()
          }
           else {
             if(BANOVA){
            ##Demo=FALSE, BANOVA=TRUE
             dev.off()
             MultipleBANOVAanalyze(TotalData)
                }
             else{
             #Demo=FALSE, BANOVA=FALSE
             MultipleNCAsave(TotalData)
               }
             }
        }
      else{
       AICoutput(sumindexR, sumindexT,R.split, T.split,keindex_ref,keindex_test,Dose,TotalData )
       NCAplot(Totalplot,SingleRdata,SingleTdata,TotalData,xaxis,yaxis)

       if (Demo){
        #Demo=TRUE, BANOVA=FALSE
        NCAmenu()
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
   }
} 
