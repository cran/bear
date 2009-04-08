##########.txt output
options(warn=-1)
NCAoutput<-function(sumindexR, sumindexT, R.split, T.split, keindex_ref, keindex_test, Dose, TotalData,
                    rdata.split,tdata.split,
                    NCA=TRUE,
                    ARS=FALSE,
                    TTT=FALSE,
                    aic=FALSE,
                    TTTARS=FALSE,
                    TTTAIC=FALSE,
                    replicated=FALSE,
                    parallel=FALSE)
{
filepath<-getwd()
cat("\n")
cat("****************************************************************************\n")
cat(" Note: Files have been output to the directory of                           \n")
cat(" ",filepath,".                                                              \n")
cat("----------------------------------------------------------------------------\n")
cat(" 1. NCA_PK.txt:                                                             \n")
cat("       --> Cmax, Tmax, AUC0t, AUC0inf, AUC0t/AUC0inf, ln(Cmax), ln(AUC0t),  \n")
cat("           ln(AUC0inf), MRT0inf, T1/2(z), Vd/F, lambda, and Cl/F            \n")
cat("       --> PK parameter summary (NCA outputs)                               \n")
cat("                                                                            \n")
cat(" 2. NCAplots.pdf: individual linear and semilog plots, spaghetti plots, and \n")
cat("         mean conc. plots.                                                  \n")
cat("                                                                            \n")
cat(" 3. Statistical_summaries.txt                                               \n")
cat("****************************************************************************\n")

#1_Cmax.txt
zz <- file("NCA_PK.txt", open="wt")
sink(zz)
#############################################################################################################################   
#NCA_output
description_version()
cat("\n")
         if (NCA){
              cat("Lambda_z is calculated with manual selection of the exact 3 data points.\n") 
              cat("----------------------------------------------------------------------------\n") 
                 }
                else {
                 if(ARS){
                 cat("Lambda_z is calculated using the adjusted R squared (ARS) method\n") 
                 cat("without including the data point of (Tmax, Cmax).\n")
                 cat("--------------------------------------------------------------------------\n") 
                 }
                else{
                  if(TTT){
                  cat("Lambda_z is calculated using the Two-Times-Tmax (TTT) method.            \n")
                  cat("ref.:Scheerans C, Derendorf H and C Kloft. Proposal for a Standardised   \n")
									cat("     Identification of the Mono-Exponential Terminal Phase for Orally    \n")
									cat("     Administered Drugs. Biopharm Drug Dispos 29, 145-157 (2008).        \n") 
                  cat("--------------------------------------------------------------------------\n")    
                     }
                else {
                 if(aic){
                 cat("Lambda_z is calculated using the Akaike information criterion (AIC) method\n") 
                 cat("without including the data point of (Tmax, Cmax).\n")
                 cat("--------------------------------------------------------------------------\n") 
                 }
                 else {
                 if(TTTARS){
                 cat("Lambda_z is calculated using the Two-Times-Tmax (TTT) and adjusted R squared\n") 
                 cat("(ARS) method without including the data point of (Tmax, Cmax).\n")
                 cat("--------------------------------------------------------------------------\n") 
                 }
                 else {
                 if(TTTAIC){
                 cat("Lambda_z is calculated using Two-Times-Tmax (TTT) and Akaike information \n") 
                 cat("criterion (AIC) method without including the data point of (Tmax, Cmax).\n")
                 cat("--------------------------------------------------------------------------\n") 
                       } 
                      } 
                     }
                   }       
                 }
                } 
cat("\n")
cat("\n")
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
                  TmaxRef[j]<-tmax_ref$time 
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
                if (NCA){
                 show(rdata.split[[j]])  
                  }
                else {
                 if(ARS){
                   n_lambda=0
                   r.adj=0
                   xr<-which.max(R.split[[j]]$conc)
                     for (i in (length(R.split[[j]]$conc)-2):(which.max(R.split[[j]]$conc)+1)) {
                      if (r.adj - summary(lm(log(conc)~time,R.split[[j]][i:nrow(R.split[[j]]),]))$adj.r.squared <
                         (0.0001)) {
                         n_lambda <- nrow(R.split[[j]])-i+1
                         r.adj <- summary(lm(log(conc)~time,R.split[[j]][i:nrow(R.split[[j]]),]))$adj.r.squared
                        }
                      }
                    show(R.split[[j]][(nrow(R.split[[j]])-n_lambda+1):nrow(R.split[[j]]),])
                    }
                 else{
                  if(TTT){
                    xr<-which.max(R.split[[j]]$conc)
                    show(R.split[[j]][R.split[[j]]$time>=(R.split[[j]]$time[xr]*2),])
                     }
                 else{
                  if(aic){
                    xr<-which.max(R.split[[j]]$conc)
                    f1 <-  function(xr) return(cbind((nrow(R.split[[j]])-xr+1),
                          (extractAIC(lm(log(conc)~time,R.split[[j]][xr:nrow(R.split[[j]]),])))[2],
                          summary(lm(log(conc)~time,R.split[[j]][xr:nrow(R.split[[j]]),]))$adj.r.squared))
                          overview <- as.data.frame(do.call(rbind,lapply((xr+1):(nrow(R.split[[j]])-2),f1)))
                          names(overview) <- c("n","AIC","adjR2")
                          n_AIC<-overview$n[which.min(overview$AIC)]
                     show(R.split[[j]][(nrow(R.split[[j]])-n_AIC+1):nrow(R.split[[j]]),])
                     }   
                 else{
                  if(TTTARS){
                    n_TTT_ARS=0
                    r.adj2=0
                   for (i in (nrow(R.split[[j]])-2):(min(seq_along(R.split[[j]]$time)[R.split[[j]]$time>=R.split[[j]]$time[which.max(R.split[[j]]$conc)]*2]))) {
                      if (r.adj2 - summary(lm(log(conc)~time,R.split[[j]][i:nrow(R.split[[j]]),]))$adj.r.squared <(0.0001)) {
                      n_TTT_ARS = nrow(R.split[[j]])-i+1
                        r.adj2 = summary(lm(log(conc)~time,R.split[[j]][i:nrow(R.split[[j]]),]))$adj.r.squared
                         }
                      }
                    show(R.split[[j]][(nrow(R.split[[j]])-n_TTT_ARS+1):nrow(R.split[[j]]),])
                     }
                 else{
                  if(TTTAIC){
                     for (i in (nrow(R.split[[j]])-2):(which.max(R.split[[j]]$conc)+1)) {
                      f11 <-  function(i) return(cbind((nrow(R.split[[j]])-i+1),
                             (extractAIC(lm(log(conc)~time,R.split[[j]][i:nrow(R.split[[j]]),])))[2]))
                             overview <- as.data.frame(do.call(rbind,lapply((i+1):(nrow(R.split[[j]])-2),f11)))
                              names(overview) <- c("n","AIC")
                            }
                           n_TTT_AIC<-overview$n[which.min(overview$AIC)]
                      show(R.split[[j]][(nrow(R.split[[j]])-n_TTT_AIC+1):nrow(R.split[[j]]),])
                     }       
                    }
                   }
                  }     
                 }
                }     
              cat("\n")
              cat("\n<<Final PK Parameters>>\n")
              cat("----------------------------\n") 
              cat("           R sq. =",R_sq ,"\n")
              cat("Adj. R sq. (ARS) =",AR_sq ,"\n")
                  if (aic){
                  xr<-which.max(R.split[[j]]$conc)
                    f1 <-  function(xr) return(cbind((nrow(R.split[[j]])-xr+1),
                          (extractAIC(lm(log(conc)~time,R.split[[j]][xr:nrow(R.split[[j]]),])))[2],
                          summary(lm(log(conc)~time,R.split[[j]][xr:nrow(R.split[[j]]),]))$adj.r.squared))
                          overview <- as.data.frame(do.call(rbind,lapply((xr+1):(nrow(R.split[[j]])-2),f1)))
                          names(overview) <- c("n","AIC","adjR2")
                          n_AIC<-overview$n[which.min(overview$AIC)]
              cat("             AIC =",min(overview$AIC) ,"\n")
                    
                  }
                else {
                 if(TTTAIC){
                   for (i in (nrow(R.split[[j]])-2):(which.max(R.split[[j]]$conc)+1)) {
                      f11 <-  function(i) return(cbind((nrow(R.split[[j]])-i+1),
                             (extractAIC(lm(log(conc)~time,R.split[[j]][i:nrow(R.split[[j]]),])))[2]))
                             overview <- as.data.frame(do.call(rbind,lapply((i+1):(nrow(R.split[[j]])-2),f11)))
                              names(overview) <- c("n","AIC")
                            }
                      n_TTT_AIC<-overview$n[which.min(overview$AIC)]
              cat("             AIC =",min(overview$AIC) ,"\n")
                      }
                    }        
              cat("        lambda_z =",ke ,"\n")
              cat("            Cmax =",Cmax_ref ,"\n")
              cat("            Tmax =",TmaxRef[j] ,"\n")
              cat("            Cl/F =",Dose/aucINF,"\n")
              cat("            Vd/F =",Dose/(aucINF*ke),"\n")
              cat("         T1/2(z) =",log(2)/ke,"\n")
              cat("        AUC(0-t) =",auc_ref[length(R.split[[j]][["conc"]])],"\n") 
              cat("      AUC(0-inf) =",aucINF,"\n")
              cat("       AUMC(0-t) =",aumc_ref[length(R.split[[j]][["conc"]])],"\n")
              cat("     AUMC(0-inf) =",aumcINF,"\n")
              cat("        MRT(0-t) =",(aumc_ref[length(R.split[[j]][["conc"]])])/(auc_ref[length(R.split[[j]][["conc"]])]),"\n")
              cat("      MRT(0-inf) =",aumcINF/aucINF,"\n")
              cat("----------------------------\n") 
  }  
cat("\n")
cat("\n")
cat("\n")
cat("\n")

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
                  TmaxTest[j]<-tmax_test$time 
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
                 colnames(output)<-list("subj","time","conc", "AUC(0~t)","AUMC(0~t)")
                 show(output)
               
              
                cat("\n<<Selected data points for lambda_z estimation>>\n")
                cat("--------------------------------------------------\n")
                if (NCA){
                 show(tdata.split[[j]])  
                  }
                else {
                 if(ARS){
                   n_lambda=0
                   r.adj=0
                   xt<-which.max(T.split[[j]]$conc)
                     for (i in (length(T.split[[j]]$conc)-2):(which.max(T.split[[j]]$conc)+1)) {
                       if (r.adj - summary(lm(log(conc)~time,T.split[[j]][i:nrow(T.split[[j]]),]))$adj.r.squared <
                       (0.0001)) {
                       n_lambda <- nrow(T.split[[j]])-i+1
                       r.adj <- summary(lm(log(conc)~time,T.split[[j]][i:nrow(T.split[[j]]),]))$adj.r.squared
                       }
                         }
                     show(T.split[[j]][(nrow(T.split[[j]])-n_lambda+1):nrow(T.split[[j]]),])
                   }
                   
                 else{
                  if(TTT){
                    xt<-which.max(T.split[[j]]$conc)
                    show(T.split[[j]][T.split[[j]]$time>=(T.split[[j]]$time[xt]*2),])
                     }
                     
                 else{
                  if(aic){
                    xt<-which.max(T.split[[j]]$conc)
                    f2 <-  function(xt) return(cbind((nrow(T.split[[j]])-xt+1),
                          (extractAIC(lm(log(conc)~time,T.split[[j]][xt:nrow(T.split[[j]]),])))[2],
                           summary(lm(log(conc)~time,T.split[[j]][xt:nrow(T.split[[j]]),]))$adj.r.squared))
                    overview <- as.data.frame(do.call(rbind,lapply((xt+1):(nrow(T.split[[j]])-2),f2)))
                    names(overview) <- c("n","AIC","adjR2")
                    n_AIC<-overview$n[which.min(overview$AIC)]
                    show(T.split[[j]][(nrow(T.split[[j]])-n_AIC+1):nrow(T.split[[j]]),])
                     }
                        
                 else{
                  if(TTTARS){
                      n_TTT_ARS=0
                      r.adj2=0
                   for (i in (nrow(T.split[[j]])-2):(min(seq_along(T.split[[j]]$time)[T.split[[j]]$time>=T.split[[j]]$time[which.max(T.split[[j]]$conc)]*2]))) {
                      if (r.adj2 - summary(lm(log(conc)~time,T.split[[j]][i:nrow(T.split[[j]]),]))$adj.r.squared <(0.0001)) {
                      n_TTT_ARS = nrow(T.split[[j]])-i+1
                      r.adj2 = summary(lm(log(conc)~time,T.split[[j]][i:nrow(T.split[[j]]),]))$adj.r.squared
                            }
                       }
                   show(T.split[[j]][(nrow(T.split[[j]])-n_TTT_ARS+1):nrow(T.split[[j]]),])
                     }
                 else{
                  if(TTTAIC){
                    for (i in (nrow(T.split[[j]])-2):(which.max(T.split[[j]]$conc)+1)) {
                        f22 <-  function(i) return(cbind((nrow(T.split[[j]])-i+1),
                        (extractAIC(lm(log(conc)~time,T.split[[j]][i:nrow(T.split[[j]]),])))[2]))
                         overview <- as.data.frame(do.call(rbind,lapply((i+1):(nrow(T.split[[j]])-2),f22)))
                         names(overview) <- c("n","AIC")
                         }
                        n_TTT_AIC<-overview$n[which.min(overview$AIC)]
                    show(T.split[[j]][(nrow(T.split[[j]])-n_TTT_AIC+1):nrow(T.split[[j]]),])
                     }       
                    }
                   }
                  }     
                 }
                }     
               cat("\n")
              cat("\n<<Final PK Parameters>>\n")
              cat("----------------------------\n") 
              cat("           R sq. =",R_sq1 ,"\n")
              cat("Adj. R sq. (ARS) =",AR_sq1 ,"\n")
                  if (aic){
                    xt<-which.max(T.split[[j]]$conc)
                    f2 <-  function(xt) return(cbind((nrow(T.split[[j]])-xt+1),
                          (extractAIC(lm(log(conc)~time,T.split[[j]][xt:nrow(T.split[[j]]),])))[2],
                           summary(lm(log(conc)~time,T.split[[j]][xt:nrow(T.split[[j]]),]))$adj.r.squared))
                    overview <- as.data.frame(do.call(rbind,lapply((xt+1):(nrow(T.split[[j]])-2),f2)))
                    names(overview) <- c("n","AIC","adjR2")
                    n_AIC<-overview$n[which.min(overview$AIC)]
              cat("             AIC =",min(overview$AIC) ,"\n")
                    
                  }
                else {
                 if(TTTAIC){
                   for (i in (nrow(T.split[[j]])-2):(which.max(T.split[[j]]$conc)+1)) {
                        f22 <-  function(i) return(cbind((nrow(T.split[[j]])-i+1),
                        (extractAIC(lm(log(conc)~time,T.split[[j]][i:nrow(T.split[[j]]),])))[2]))
                         overview <- as.data.frame(do.call(rbind,lapply((i+1):(nrow(T.split[[j]])-2),f22)))
                         names(overview) <- c("n","AIC")
                         }
                        n_TTT_AIC<-overview$n[which.min(overview$AIC)]
              cat("             AIC =",min(overview$AIC) ,"\n")
                      }
                    }        
              cat("        lambda_z =",ke1 ,"\n")
              cat("            Cmax =",Cmax_test ,"\n")
              cat("            Tmax =",TmaxTest[j] ,"\n")
              cat("            Cl/F =",Dose/aucINF,"\n")
              cat("            Vd/F =", Dose/(aucINF*ke1),"\n")
              cat("         T1/2(z) =",log(2)/ke1,"\n")
              cat("        AUC(0-t) =",auc_test[length(T.split[[j]][["conc"]])],"\n") 
              cat("      AUC(0-inf) =",aucINF,"\n")
              cat("       AUMC(0-t) =",aumc_test[length(T.split[[j]][["conc"]])],"\n")
              cat("     AUMC(0-inf) =",aumcINF,"\n")
              cat("        MRT(0-t) =",(aumc_test[length(T.split[[j]][["conc"]])])/(auc_test[length(T.split[[j]][["conc"]])]),"\n")
              cat("      MRT(0-inf) =",aumcINF/aucINF,"\n")
              cat("----------------------------\n")              
  }  
 if(replicated){
  sumindexRR<-split(sumindexR,list(sumindexR$subj))
  sumindexTT<-split(sumindexT,list(sumindexT$subj))
    subj<-0
    CmaxR<-0
    TmaxR<-0
    AUC0tR<-0
    AUC0INFR<-0
    MRT0INFR<-0
    T12R<-0
    VdFR<-0
    kelR<-0
    ClFR<-0
     for (j in 1:(length(sumindexRR))){
      subj[j]<-sumindexRR[[j]][["subj"]][1]
      CmaxR[j]<-as.numeric(formatC(mean(sumindexRR[[j]][[5]]),format="f",digits=3))
      TmaxR[j]<-as.numeric(formatC(mean(sumindexRR[[j]][[8]]),format="f",digits=3))
      AUC0tR[j]<-as.numeric(formatC(mean(sumindexRR[[j]][[6]]),format="f",digits=3))
      AUC0INFR[j]<-as.numeric(formatC(mean(sumindexRR[[j]][[7]]),format="f",digits=3))
      MRT0INFR[j]<-as.numeric(formatC(mean(sumindexRR[[j]][[9]]),format="f",digits=3))
      T12R[j]<-as.numeric(formatC(mean(sumindexRR[[j]][[10]]),format="f",digits=3))
      VdFR[j]<-as.numeric(formatC(mean(sumindexRR[[j]][[11]]),format="f",digits=3))
      kelR[j]<-as.numeric(formatC(mean(sumindexRR[[j]][[12]]),format="f",digits=3))
      ClFR[j]<-as.numeric(formatC(mean(sumindexRR[[j]][[13]]),format="f",digits=3))
     }
   CmaxT<-0
   TmaxT<-0
   AUC0tT<-0
   AUC0INFT<-0
   MRT0INFT<-0
   T12T<-0
   VdFT<-0
   kelT<-0
   ClFT<-0
   for (j in 1:(length(sumindexTT))){
      CmaxT[j]<-as.numeric(formatC(mean(sumindexTT[[j]][[5]]),format="f",digits=3))
      TmaxT[j]<-as.numeric(formatC(mean(sumindexTT[[j]][[8]]),format="f",digits=3))
      AUC0tT[j]<-as.numeric(formatC(mean(sumindexTT[[j]][[6]]),format="f",digits=3))
      AUC0INFT[j]<-as.numeric(formatC(mean(sumindexTT[[j]][[7]]),format="f",digits=3))
      MRT0INFT[j]<-as.numeric(formatC(mean(sumindexTT[[j]][[9]]),format="f",digits=3))
      T12T[j]<-as.numeric(formatC(mean(sumindexTT[[j]][[10]]),format="f",digits=3))
      VdFT[j]<-as.numeric(formatC(mean(sumindexTT[[j]][[11]]),format="f",digits=3))
      kelT[j]<-as.numeric(formatC(mean(sumindexTT[[j]][[12]]),format="f",digits=3))
      ClFT[j]<-as.numeric(formatC(mean(sumindexTT[[j]][[13]]),format="f",digits=3))
     } 
   }

cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n<< PK parameter summaries >>\n")
cat("\n")
cat("\n")
cat("                         Cmax                     \n")
cat("--------------------------------------------------\n")
if(replicated){ 
  outputCmax<-data.frame(subject=subj, Test=CmaxT,Ref=CmaxR,Ratio=as.numeric(formatC((CmaxT/CmaxR),format="f",digits=3))) 
  }
   else{
  outputCmax<-data.frame(subject=sumindexR$subj,Test=as.numeric(formatC(sumindexT$Cmax,format="f",digits=3)),
                        Ref=as.numeric(formatC(sumindexR$Cmax,format="f",digits=3)),Ratio=as.numeric(formatC(sumindexT$Cmax/sumindexR$Cmax,format="f",digits=3))) 
  }
  show(outputCmax)
  cat("\n")
  outputCmaxMean<-data.frame(summary=c("LSMEAN","S.D.","C.V(%)"),
                             Test=c(formatC(mean(outputCmax$Test),format="f",digits=3),formatC(sd(outputCmax$Test),format="f",digits=3),formatC((sd(outputCmax$Test)/mean(outputCmax$Test))*100,format="f",digits=3)),
                             Ref=c(formatC(mean(outputCmax$Ref),format="f",digits=3),formatC(sd(outputCmax$Ref),format="f",digits=3),formatC((sd(outputCmax$Ref)/mean(outputCmax$Ref))*100,format="f",digits=3)),
                             Ratio=c(formatC(mean(outputCmax$Ratio),format="f",digits=3),formatC(sd(outputCmax$Ratio),format="f",digits=3),formatC((sd(outputCmax$Ratio)/mean(outputCmax$Ratio))*100,format="f",digits=3)))
  show(outputCmaxMean)
cat("--------------------------------------------------\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")

#2_Tmax
cat("                         Tmax                     \n")
cat("--------------------------------------------------\n")
  if(replicated){
  outputTmax<-data.frame(subject=subj, Test=TmaxT, Ref=TmaxR,Ratio=as.numeric(formatC(TmaxT/TmaxR,format="f",digits=3))) 
  }
   else{
  outputTmax<-data.frame(subject=sumindexR$subj,Test=as.numeric(formatC(sumindexT$Tmax,format="f",digits=3)),
                        Ref=as.numeric(formatC(sumindexR$Tmax,format="f",digits=3)),Ratio=as.numeric(formatC(sumindexT$Tmax/sumindexR$Tmax,format="f",digits=3))) 
  }
  show(outputTmax)
  cat("\n")
  outputTmaxMean<-data.frame(summary=c("LSMEAN","S.D.","C.V(%)"),
                             Test=c(formatC(mean(outputTmax$Test),format="f",digits=3),formatC(sd(outputTmax$Test),format="f",digits=3),formatC((sd(outputTmax$Test)/mean(outputTmax$Test))*100,format="f",digits=3)),
                             Ref=c(formatC(mean(outputTmax$Ref),format="f",digits=3),formatC(sd(outputTmax$Ref),format="f",digits=3),formatC((sd(outputTmax$Ref)/mean(outputTmax$Ref))*100,format="f",digits=3)),
                             Ratio=c(formatC(mean(outputTmax$Ratio),format="f",digits=3),formatC(sd(outputTmax$Ratio),format="f",digits=3),formatC((sd(outputTmax$Ratio)/mean(outputTmax$Ratio))*100,format="f",digits=3)))
  show(outputTmaxMean)
cat("--------------------------------------------------\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")

#3_AUC0t
cat("                         AUC0t                     \n")
cat("---------------------------------------------------\n")
  if(replicated){
  outputAUC0t<-data.frame(subject=subj, Test=AUC0tT, Ref=AUC0tR,Ratio=as.numeric(formatC(AUC0tT/AUC0tR,format="f",digits=3))) 
  }
   else{
  outputAUC0t<-data.frame(subject=sumindexR$subj,Test=as.numeric(formatC(sumindexT$AUC0t,format="f",digits=3)),Ref=as.numeric(formatC(sumindexR$AUC0t,format="f",digits=3)),Ratio=as.numeric(formatC(sumindexT$AUC0t/sumindexR$AUC0t,format="f",digits=3))) 
  }
  show(outputAUC0t)
  cat("\n")
  outputAUC0tMean<-data.frame(summary=c("LSMEAN","S.D.","C.V(%)"),
                             Test=c(formatC(mean(outputAUC0t$Test),format="f",digits=3),formatC(sd(outputAUC0t$Test),format="f",digits=3),formatC((sd(outputAUC0t$Test)/mean(outputAUC0t$Test))*100,format="f",digits=3)),
                             Ref=c(formatC(mean(outputAUC0t$Ref),format="f",digits=3),formatC(sd(outputAUC0t$Ref),format="f",digits=3),formatC((sd(outputAUC0t$Ref)/mean(outputAUC0t$Ref))*100,format="f",digits=3)),
                             Ratio=c(formatC(mean(outputAUC0t$Ratio),format="f",digits=3),formatC(sd(outputAUC0t$Ratio),format="f",digits=3),formatC((sd(outputAUC0t$Ratio)/mean(outputAUC0t$Ratio))*100,format="f",digits=3)))
  show(outputAUC0tMean)
cat("---------------------------------------------------\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")

#4_AUC0inf
cat("                         AUC0inf                    \n")
cat("---------------------------------------------------\n")
  if(replicated){
  outputAUC0INF<-data.frame(subject=subj, Test=AUC0INFT, Ref=AUC0INFR,Ratio=as.numeric(formatC(AUC0INFT/AUC0INFR,format="f",digits=3)))
  }
   else{
  outputAUC0INF<-data.frame(subject=sumindexR$subj,Test=as.numeric(formatC(sumindexT$AUC0INF,format="f",digits=3)),
                           Ref=as.numeric(formatC(sumindexR$AUC0INF,format="f",digits=3)),
                           Ratio=as.numeric(formatC(sumindexT$AUC0INF/sumindexR$AUC0INF,format="f",digits=3))) 
  }
  show(outputAUC0INF)
  cat("\n")
  outputAUC0INFMean<-data.frame(summary=c("LSMEAN","S.D.","C.V(%)"),
                             Test=c(formatC(mean(outputAUC0INF$Test),format="f",digits=3),formatC(sd(outputAUC0INF$Test),format="f",digits=3),formatC((sd(outputAUC0INF$Test)/mean(outputAUC0INF$Test))*100,format="f",digits=3)),
                             Ref=c(formatC(mean(outputAUC0INF$Ref),format="f",digits=3),formatC(sd(outputAUC0INF$Ref),format="f",digits=3),formatC((sd(outputAUC0INF$Ref)/mean(outputAUC0INF$Ref))*100,format="f",digits=3)),
                             Ratio=c(formatC(mean(outputAUC0INF$Ratio),format="f",digits=3),formatC(sd(outputAUC0INF$Ratio),format="f",digits=3),formatC((sd(outputAUC0INF$Ratio)/mean(outputAUC0INF$Ratio))*100,format="f",digits=3)))
  show(outputAUC0INFMean)
cat("---------------------------------------------------\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")

#5_AUC0t/AUC0inf
cat("                 (AUC0t/AUC0inf)*100               \n")
cat("---------------------------------------------------\n")
  if(replicated){
  outputAUC0t_AUC0INF<-data.frame(subject=subj,Test=as.numeric(formatC((AUC0tT/AUC0INFT)*100,format="f",digits=3)) 
                           ,Ref=as.numeric(formatC((AUC0tR/AUC0INFR)*100,format="f",digits=3)) 
                           ,Ratio=as.numeric(formatC(((AUC0tT/AUC0INFT)/(AUC0tR/AUC0INFR))*100,format="f",digits=3))) 
  }
  else{
  outputAUC0t_AUC0INF<-data.frame(subject=sumindexR$subj,Test=as.numeric(formatC((sumindexT$AUC0t/sumindexT$AUC0INF)*100,format="f",digits=3)) 
                                 ,Ref=as.numeric(formatC((sumindexR$AUC0t/sumindexR$AUC0INF)*100,format="f",digits=3)) 
                                 ,Ratio=as.numeric(formatC(((sumindexT$AUC0t/sumindexT$AUC0INF)/(sumindexR$AUC0t/sumindexR$AUC0INF))*100,format="f",digits=3))) 
  }
  show(outputAUC0t_AUC0INF)
  cat("\n")
  outputAUC0t_AUC0INFMean<-data.frame(summary=c("LSMEAN","S.D.","C.V(%)"),
                             Test=c(formatC(mean(outputAUC0t_AUC0INF$Test),format="f",digits=3),formatC(sd(outputAUC0t_AUC0INF$Test),format="f",digits=3),formatC((sd(outputAUC0t_AUC0INF$Test)/mean(outputAUC0t_AUC0INF$Test))*100,format="f",digits=3)),
                             Ref=c(formatC(mean(outputAUC0t_AUC0INF$Ref),format="f",digits=3),formatC(sd(outputAUC0t_AUC0INF$Ref),format="f",digits=3),formatC((sd(outputAUC0t_AUC0INF$Ref)/mean(outputAUC0t_AUC0INF$Ref))*100,format="f",digits=3)),
                             Ratio=c(formatC(mean(outputAUC0t_AUC0INF$Ratio),format="f",digits=3),formatC(sd(outputAUC0t_AUC0INF$Ratio),format="f",digits=3),formatC((sd(outputAUC0t_AUC0INF$Ratio)/mean(outputAUC0t_AUC0INF$Ratio))*100,format="f",digits=3)))
  show(outputAUC0t_AUC0INFMean)
cat("---------------------------------------------------\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
#6_ln(Cmax).txt
cat("                      ln(Cmax)                     \n")
cat("--------------------------------------------------\n")
  if(replicated){
  outputlnCmax<-data.frame(subject=subj,Test=as.numeric(formatC(log(CmaxT),format="f",digits=3)) 
                           ,Ref=as.numeric(formatC(log(CmaxR),format="f",digits=3)) 
                           ,Ratio=as.numeric(formatC((log(CmaxT)/log(CmaxR)),format="f",digits=3))) 
  }
  else{
  outputlnCmax<-data.frame(subject=sumindexR$subj,Test=as.numeric(formatC(log(sumindexT$Cmax),format="f",digits=3)),
                           Ref=as.numeric(formatC(log(sumindexR$Cmax),format="f",digits=3)), 
                           Ratio=as.numeric(formatC(log(sumindexT$Cmax)/log(sumindexR$Cmax),format="f",digits=3))) 
  }
  show(outputlnCmax)
  cat("\n")
  outputlnCmaxMean<-data.frame(summary=c("LSMEAN","S.D.","C.V(%)"),
                             Test=c(formatC(mean(outputlnCmax$Test),format="f",digits=3),formatC(sd(outputlnCmax$Test),format="f",digits=3),formatC((sd(outputlnCmax$Test)/mean(outputlnCmax$Test))*100,format="f",digits=3)),
                             Ref=c(formatC(mean(outputlnCmax$Ref),format="f",digits=3),formatC(sd(outputlnCmax$Ref),format="f",digits=3),formatC((sd(outputlnCmax$Ref)/mean(outputlnCmax$Ref))*100,format="f",digits=3)),
                             Ratio=c(formatC(mean(outputlnCmax$Ratio),format="f",digits=3),formatC(sd(outputlnCmax$Ratio),format="f",digits=3),formatC((sd(outputlnCmax$Ratio)/mean(outputlnCmax$Ratio))*100,format="f",digits=3)))
  show(outputlnCmaxMean)
cat("--------------------------------------------------\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
#7_ln(AUC0t)
cat("                       ln(AUC0t)                    \n")
cat("---------------------------------------------------\n")
  if(replicated){
  outputlnAUC0t<-data.frame(subject=subj,Test=as.numeric(formatC(log(AUC0tT),format="f",digits=3)) 
                           ,Ref=as.numeric(formatC(log(AUC0tR),format="f",digits=3)) 
                           ,Ratio=as.numeric(formatC((log(AUC0tT)/log(AUC0tR)),format="f",digits=3))) 
   }
  else{
  outputlnAUC0t<-data.frame(subject=sumindexR$subj,Test=as.numeric(formatC(log(sumindexT$AUC0t),format="f",digits=3)),
                            Ref=as.numeric(formatC(log(sumindexR$AUC0t),format="f",digits=3)),
                            Ratio=as.numeric(formatC(log(sumindexT$AUC0t)/log(sumindexR$AUC0t),format="f",digits=3))) 
  }
  show(outputlnAUC0t)
  cat("\n")
  outputlnAUC0tMean<-data.frame(summary=c("LSMEAN","S.D.","C.V(%)"),
                             Test=c(formatC(mean(outputlnAUC0t$Test),format="f",digits=3),formatC(sd(outputlnAUC0t$Test),format="f",digits=3),formatC((sd(outputlnAUC0t$Test)/mean(outputlnAUC0t$Test))*100,format="f",digits=3)),
                             Ref=c(formatC(mean(outputlnAUC0t$Ref),format="f",digits=3),formatC(sd(outputlnAUC0t$Ref),format="f",digits=3),formatC((sd(outputlnAUC0t$Ref)/mean(outputlnAUC0t$Ref))*100,format="f",digits=3)),
                             Ratio=c(formatC(mean(outputlnAUC0t$Ratio),format="f",digits=3),formatC(sd(outputlnAUC0t$Ratio),format="f",digits=3),formatC((sd(outputlnAUC0t$Ratio)/mean(outputlnAUC0t$Ratio))*100,format="f",digits=3)))
  show(outputlnAUC0tMean)
cat("---------------------------------------------------\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
#8_ln(AUC0inf)
cat("                       ln(AUC0inf)                 \n")
cat("---------------------------------------------------\n")
  if(replicated){
  outputlnAUC0INF<-data.frame(subject=subj,Test=as.numeric(formatC(log(AUC0INFT),format="f",digits=3)) 
                           ,Ref=as.numeric(formatC(log(AUC0INFR),format="f",digits=3)) 
                           ,Ratio=as.numeric(formatC((log(AUC0INFT)/log(AUC0INFR)),format="f",digits=3))) 
   }
  else{
   outputlnAUC0INF<-data.frame(subject=sumindexR$subj,Test=as.numeric(formatC(log(sumindexT$AUC0INF),format="f",digits=3)),
                               Ref=as.numeric(formatC(log(sumindexR$AUC0INF),format="f",digits=3)),
                               Ratio=as.numeric(formatC(log(sumindexT$AUC0INF)/log(sumindexR$AUC0INF),format="f",digits=3))) 
 } 
  show(outputlnAUC0INF)
  cat("\n")
  outputlnAUC0INFMean<-data.frame(summary=c("LSMEAN","S.D.","C.V(%)"),
                             Test=c(formatC(mean(outputlnAUC0INF$Test),format="f",digits=3),formatC(sd(outputlnAUC0INF$Test),format="f",digits=3),formatC((sd(outputlnAUC0INF$Test)/mean(outputlnAUC0INF$Test))*100,format="f",digits=3)),
                             Ref=c(formatC(mean(outputlnAUC0INF$Ref),format="f",digits=3),formatC(sd(outputlnAUC0INF$Ref),format="f",digits=3),formatC((sd(outputlnAUC0INF$Ref)/mean(outputlnAUC0INF$Ref))*100,format="f",digits=3)),
                             Ratio=c(formatC(mean(outputlnAUC0INF$Ratio),format="f",digits=3),formatC(sd(outputlnAUC0INF$Ratio),format="f",digits=3),formatC((sd(outputlnAUC0INF$Ratio)/mean(outputlnAUC0INF$Ratio))*100,format="f",digits=3)))
  show(outputlnAUC0INFMean)
cat("---------------------------------------------------\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
#9_MRT0inf
cat("                      MRT0inf                      \n")
cat("---------------------------------------------------\n")
  if(replicated){
  outputMRT0INF<-data.frame(subject=subj,Test=MRT0INFT,Ref=MRT0INFR,Ratio=as.numeric(formatC(MRT0INFT/MRT0INFR,format="f",digits=3))) 
   }
  else{
  outputMRT0INF<-data.frame(subject=sumindexR$subj,Test=as.numeric(formatC(sumindexT$MRTINF,format="f",digits=3)),
                            Ref=as.numeric(formatC(sumindexR$MRTINF,format="f",digits=3)),
                            Ratio=as.numeric(formatC(sumindexT$MRTINF/sumindexR$MRTINF,format="f",digits=3))) 
  }
  show(outputMRT0INF)
  cat("\n")
  outputMRT0INFMean<-data.frame(summary=c("LSMEAN","S.D.","C.V(%)"),
                             Test=c(formatC(mean(outputMRT0INF$Test),format="f",digits=3),formatC(sd(outputMRT0INF$Test),format="f",digits=3),formatC((sd(outputMRT0INF$Test)/mean(outputMRT0INF$Test))*100,format="f",digits=3)),
                             Ref=c(formatC(mean(outputMRT0INF$Ref),format="f",digits=3),formatC(sd(outputMRT0INF$Ref),format="f",digits=3),formatC((sd(outputMRT0INF$Ref)/mean(outputMRT0INF$Ref))*100,format="f",digits=3)),
                             Ratio=c(formatC(mean(outputMRT0INF$Ratio),format="f",digits=3),formatC(sd(outputMRT0INF$Ratio),format="f",digits=3),formatC((sd(outputMRT0INF$Ratio)/mean(outputMRT0INF$Ratio))*100,format="f",digits=3)))
  show(outputMRT0INFMean)
cat("---------------------------------------------------\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
#10_T1/2(z)
cat("                         T1/2(z)                   \n")
cat("---------------------------------------------------\n")
  if(replicated){
  outputT12<-data.frame(subject=subj,Test=T12T,Ref=T12R,Ratio=as.numeric(formatC(T12T/T12R,format="f",digits=3))) 
   }
  else{
  outputT12<-data.frame(subject=sumindexR$subj,Test=as.numeric(formatC(sumindexT$T12,format="f",digits=3)),
                        Ref=as.numeric(formatC(sumindexR$T12,format="f",digits=3)),
                        Ratio=as.numeric(formatC(sumindexT$T12/sumindexR$T12,format="f",digits=3)) )
  }
  show(outputT12)
  cat("\n")
  outputT12Mean<-data.frame(summary=c("LSMEAN","S.D.","C.V(%)"),
                             Test=c(formatC(mean(outputT12$Test),format="f",digits=3),formatC(sd(outputT12$Test),format="f",digits=3),formatC((sd(outputT12$Test)/mean(outputT12$Test))*100,format="f",digits=3)),
                             Ref=c(formatC(mean(outputT12$Ref),format="f",digits=3),formatC(sd(outputT12$Ref),format="f",digits=3),formatC((sd(outputT12$Ref)/mean(outputT12$Ref))*100,format="f",digits=3)),
                             Ratio=c(formatC(mean(outputT12$Ratio),format="f",digits=3),formatC(sd(outputT12$Ratio),format="f",digits=3),formatC((sd(outputT12$Ratio)/mean(outputT12$Ratio))*100,format="f",digits=3)))
  show(outputT12Mean)
cat("---------------------------------------------------\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
#11_Vd/F
cat("                         Vd/F                      \n")
cat("---------------------------------------------------\n")
  if(replicated){
  outputVdF<-data.frame(subject=subj,Test=VdFT,Ref=VdFR,Ratio=as.numeric(formatC(VdFT/VdFR,format="f",digits=3))) 
   }
  else{
  outputVdF<-data.frame(subject=sumindexR$subj,Test=as.numeric(formatC(sumindexT$VdF,format="f",digits=3)),
                        Ref=as.numeric(formatC(sumindexR$VdF,format="f",digits=3)),
                        Ratio=as.numeric(formatC(sumindexT$VdF/sumindexR$VdF,format="f",digits=3)))
  }
  show(outputVdF)
  cat("\n")
  outputVdFMean<-data.frame(summary=c("LSMEAN","S.D.","C.V(%)"),
                             Test=c(formatC(mean(outputVdF$Test),format="f",digits=3),formatC(sd(outputVdF$Test),format="f",digits=3),formatC((sd(outputVdF$Test)/mean(outputVdF$Test))*100,format="f",digits=3)),
                             Ref=c(formatC(mean(outputVdF$Ref),format="f",digits=3),formatC(sd(outputVdF$Ref),format="f",digits=3),formatC((sd(outputVdF$Ref)/mean(outputVdF$Ref))*100,format="f",digits=3)),
                             Ratio=c(formatC(mean(outputVdF$Ratio),format="f",digits=3),formatC(sd(outputVdF$Ratio),format="f",digits=3),formatC((sd(outputVdF$Ratio)/mean(outputVdF$Ratio))*100,format="f",digits=3)))  
  show(outputVdFMean)
cat("---------------------------------------------------\n")
cat("\n")
cat("\n")
cat("\n")
cat("\n")
#12_Lambda
cat("                      Lambda_z                       \n")
cat("---------------------------------------------------\n")
  if(replicated){
  outputLambda<-data.frame(subject=subj,Test=kelT,Ref=kelR,Ratio=as.numeric(formatC(kelT/kelR,format="f",digits=3))) 
   }
  else{
  outputLambda<-data.frame(subject=sumindexR$subj,Test=sumindexT$Kel,Ref=sumindexR$Kel,
                             Ratio=sumindexT$Kel/sumindexR$Kel)
  }
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
  if(replicated){
  outputClF<-data.frame(subject=subj,Test=ClFT,Ref=ClFR,Ratio=as.numeric(formatC(ClFT/ClFR,format="f",digits=3))) 
   }
  else{
  outputClF<-data.frame(subject=sumindexR$subj,Test=as.numeric(formatC(sumindexT$ClF,format="f",digits=3)),
                        Ref=as.numeric(formatC(sumindexR$ClF,format="f",digits=3)),
                        Ratio=as.numeric(formatC(sumindexT$ClF/sumindexR$ClF,format="f",digits=3)))
  }
  show(outputClF)
  cat("\n")
  outputClFMean<-data.frame(summary=c("LSMEAN","S.D.","C.V(%)"),
                             Test=c(formatC(mean(outputClF$Test),format="f",digits=3),formatC(sd(outputClF$Test),format="f",digits=3),formatC((sd(outputClF$Test)/mean(outputClF$Test))*100,format="f",digits=3)),
                             Ref=c(formatC(mean(outputClF$Ref),format="f",digits=3),formatC(sd(outputClF$Ref),format="f",digits=3),formatC((sd(outputClF$Ref)/mean(outputClF$Ref))*100,format="f",digits=3)),
                             Ratio=c(formatC(mean(outputClF$Ratio),format="f",digits=3),formatC(sd(outputClF$Ratio),format="f",digits=3),formatC((sd(outputClF$Ratio)/mean(outputClF$Ratio))*100,format="f",digits=3)))
  show(outputClFMean)
cat("---------------------------------------------------\n")
sink()

#############################################################################################################
####for present statistical analysis (GLM result)

Fdata<-split(TotalData, list(TotalData$drug))
RefData<-Fdata[[1]]
TestData<-Fdata[[2]]

ref_Cmax<-mean(RefData$lnCmax)
ref_AUC0t<-mean(RefData$lnAUC0t)
ref_AUC0INF<-mean(RefData$lnAUC0INF)

test_Cmax<-mean(TestData$lnCmax)
test_AUC0t<-mean(TestData$lnAUC0t)
test_AUC0INF<-mean(TestData$lnAUC0INF)

if(replicated){
SeqLeg<-split(RefData, list(RefData$seq))
SeqLeg1 <- reshape(SeqLeg[[1]], idvar=c("subj", "drug","seq"), timevar =
"prd",direction = "wide")
SeqLeg2 <- reshape(SeqLeg[[2]], idvar=c("subj", "drug","seq"), timevar =
"prd",direction = "wide")  
L1<-length(SeqLeg1$subj)
L2<-length(SeqLeg2$subj)

prdcount<-length(levels(TotalData$prd)) #count periods

modCmax<-lme(Cmax ~ seq +  prd + drug , random=~1|subj, data=TotalData, method="REML" )
modAUC0t<-lme(AUC0t ~ seq +  prd + drug , random=~1|subj, data=TotalData, method="REML" )
modAUC0INF<-lme(AUC0INF ~ seq +  prd + drug , random=~1|subj, data=TotalData, method="REML" )
modlnCmax<-lme(lnCmax ~ seq +  prd + drug , random=~1|subj, data=TotalData, method="REML" )
modlnAUC0t<-lme(lnAUC0t ~ seq +  prd + drug , random=~1|subj, data=TotalData, method="REML" )
modlnAUC0INF<-lme(lnAUC0INF ~ seq +  prd + drug , random=~1|subj, data=TotalData, method="REML" )

if(prdcount==3){
upperCmax<-100*exp(summary(modlnCmax)[20][[1]][5,1])*exp(qt(0.95,summary(modlnCmax)[20][[1]][5,3])*summary(modlnCmax)[20][[1]][5,2])
lowerCmax<-100*exp(summary(modlnCmax)[20][[1]][5,1])*exp(-qt(0.95,summary(modlnCmax)[20][[1]][5,3])*summary(modlnCmax)[20][[1]][5,2])
upperAUC0t<-100*exp(summary(modlnAUC0t)[20][[1]][5,1])*exp(qt(0.95,summary(modlnAUC0t)[20][[1]][5,3])*summary(modlnAUC0t)[20][[1]][5,2])
lowerAUC0t<-100*exp(summary(modlnAUC0t)[20][[1]][5,1])*exp(-qt(0.95,summary(modlnAUC0t)[20][[1]][5,3])*summary(modlnAUC0t)[20][[1]][5,2])
upperAUC0INF<-100*exp(summary(modlnAUC0INF)[20][[1]][5,1])*exp(qt(0.95,summary(modlnAUC0INF)[20][[1]][5,3])*summary(modlnAUC0INF)[20][[1]][5,2])
lowerAUC0INF<-100*exp(summary(modlnAUC0INF)[20][[1]][5,1])*exp(-qt(0.95,summary(modlnAUC0INF)[20][[1]][5,3])*summary(modlnAUC0INF)[20][[1]][5,2])
 }
 if (prdcount==4){
upperCmax<-100*exp(summary(modlnCmax)[20][[1]][6,1])*exp(qt(0.95,summary(modlnCmax)[20][[1]][6,3])*summary(modlnCmax)[20][[1]][6,2])
lowerCmax<-100*exp(summary(modlnCmax)[20][[1]][6,1])*exp(-qt(0.95,summary(modlnCmax)[20][[1]][6,3])*summary(modlnCmax)[20][[1]][6,2])
upperAUC0t<-100*exp(summary(modlnAUC0t)[20][[1]][6,1])*exp(qt(0.95,summary(modlnAUC0t)[20][[1]][6,3])*summary(modlnAUC0t)[20][[1]][6,2])
lowerAUC0t<-100*exp(summary(modlnAUC0t)[20][[1]][6,1])*exp(-qt(0.95,summary(modlnAUC0t)[20][[1]][6,3])*summary(modlnAUC0t)[20][[1]][6,2])
upperAUC0INF<-100*exp(summary(modlnAUC0INF)[20][[1]][6,1])*exp(qt(0.95,summary(modlnAUC0INF)[20][[1]][6,3])*summary(modlnAUC0INF)[20][[1]][6,2])
lowerAUC0INF<-100*exp(summary(modlnAUC0INF)[20][[1]][6,1])*exp(-qt(0.95,summary(modlnAUC0INF)[20][[1]][6,3])*summary(modlnAUC0INF)[20][[1]][6,2])
  }
  if (prdcount==5){
upperCmax<-100*exp(summary(modlnCmax)[20][[1]][7,1])*exp(qt(0.95,summary(modlnCmax)[20][[1]][7,3])*summary(modlnCmax)[20][[1]][7,2])
lowerCmax<-100*exp(summary(modlnCmax)[20][[1]][7,1])*exp(-qt(0.95,summary(modlnCmax)[20][[1]][7,3])*summary(modlnCmax)[20][[1]][7,2])
upperAUC0t<-100*exp(summary(modlnAUC0t)[20][[1]][7,1])*exp(qt(0.95,summary(modlnAUC0t)[20][[1]][7,3])*summary(modlnAUC0t)[20][[1]][7,2])
lowerAUC0t<-100*exp(summary(modlnAUC0t)[20][[1]][7,1])*exp(-qt(0.95,summary(modlnAUC0t)[20][[1]][7,3])*summary(modlnAUC0t)[20][[1]][7,2])
upperAUC0INF<-100*exp(summary(modlnAUC0INF)[20][[1]][7,1])*exp(qt(0.95,summary(modlnAUC0INF)[20][[1]][7,3])*summary(modlnAUC0INF)[20][[1]][7,2])
lowerAUC0INF<-100*exp(summary(modlnAUC0INF)[20][[1]][7,1])*exp(-qt(0.95,summary(modlnAUC0INF)[20][[1]][7,3])*summary(modlnAUC0INF)[20][[1]][7,2])
  }
 if (prdcount==6){
upperCmax<-100*exp(summary(modlnCmax)[20][[1]][8,1])*exp(qt(0.95,summary(modlnCmax)[20][[1]][8,3])*summary(modlnCmax)[20][[1]][8,2])
lowerCmax<-100*exp(summary(modlnCmax)[20][[1]][8,1])*exp(-qt(0.95,summary(modlnCmax)[20][[1]][8,3])*summary(modlnCmax)[20][[1]][8,2])
upperAUC0t<-100*exp(summary(modlnAUC0t)[20][[1]][8,1])*exp(qt(0.95,summary(modlnAUC0t)[20][[1]][8,3])*summary(modlnAUC0t)[20][[1]][8,2])
lowerAUC0t<-100*exp(summary(modlnAUC0t)[20][[1]][8,1])*exp(-qt(0.95,summary(modlnAUC0t)[20][[1]][8,3])*summary(modlnAUC0t)[20][[1]][8,2])
upperAUC0INF<-100*exp(summary(modlnAUC0INF)[20][[1]][8,1])*exp(qt(0.95,summary(modlnAUC0INF)[20][[1]][8,3])*summary(modlnAUC0INF)[20][[1]][8,2])
lowerAUC0INF<-100*exp(summary(modlnAUC0INF)[20][[1]][8,1])*exp(-qt(0.95,summary(modlnAUC0INF)[20][[1]][8,3])*summary(modlnAUC0INF)[20][[1]][8,2])
  }
}  

else{
 if(parallel){
L1<-length(sumindexR$subj)
L2<-length(sumindexT$subj)  

modCmax<-lme(Cmax ~ drug , random=~1|subj, data=TotalData, method="REML" )
modAUC0t<-lme(AUC0t ~ drug , random=~1|subj, data=TotalData, method="REML" )
modAUC0INF<-lme(AUC0INF ~ drug , random=~1|subj, data=TotalData, method="REML" )
modlnCmax<-lme(lnCmax ~ drug , random=~1|subj, data=TotalData, method="REML" )
modlnAUC0t<-lme(lnAUC0t ~ drug , random=~1|subj, data=TotalData, method="REML" )
modlnAUC0INF<-lme(lnAUC0INF ~ drug , random=~1|subj, data=TotalData, method="REML" ) 
 
upperCmax<-100*exp(summary(modlnCmax)[20][[1]][2,1])*exp(qt(0.95,summary(modlnCmax)[20][[1]][2,3])*summary(modlnCmax)[20][[1]][2,2])
lowerCmax<-100*exp(summary(modlnCmax)[20][[1]][2,1])*exp(-qt(0.95,summary(modlnCmax)[20][[1]][2,3])*summary(modlnCmax)[20][[1]][2,2])
upperAUC0t<-100*exp(summary(modlnAUC0t)[20][[1]][2,1])*exp(qt(0.95,summary(modlnAUC0t)[20][[1]][2,3])*summary(modlnAUC0t)[20][[1]][2,2])
lowerAUC0t<-100*exp(summary(modlnAUC0t)[20][[1]][2,1])*exp(-qt(0.95,summary(modlnAUC0t)[20][[1]][2,3])*summary(modlnAUC0t)[20][[1]][2,2])
upperAUC0INF<-100*exp(summary(modlnAUC0INF)[20][[1]][2,1])*exp(qt(0.95,summary(modlnAUC0INF)[20][[1]][2,3])*summary(modlnAUC0INF)[20][[1]][2,2])
lowerAUC0INF<-100*exp(summary(modlnAUC0INF)[20][[1]][2,1])*exp(-qt(0.95,summary(modlnAUC0INF)[20][[1]][2,3])*summary(modlnAUC0INF)[20][[1]][2,2]) 
 }
 else{
#L1(Reference-->Test),L2(Test-->Reference sequence)
SeqLeg<-split(RefData, list(RefData$seq))
L1<-length(SeqLeg[[1]]$seq)
L2<-length(SeqLeg[[2]]$seq)

Cmax<- lm(Cmax ~ seq + subj + prd + drug , data=TotalData)
AUC0t<- lm(AUC0t ~ seq + subj+ prd + drug , data=TotalData)
AUC0INF<- lm(AUC0INF ~ seq + subj+ prd + drug , data=TotalData)
lnCmax<- lm(lnCmax ~ seq + subj + prd + drug , data=TotalData)
lnAUC0t<- lm(lnAUC0t ~ seq + subj + prd + drug , data=TotalData)
lnAUC0INF<- lm(lnAUC0INF ~ seq + subj + prd + drug , data=TotalData)

T<-qt(0.95,(L1+L2-2))


SE_Cmax<-sqrt((anova(lnCmax)[5,3]/2) * (1/L1+1/L2)) 
SE_AUC0t<-sqrt((anova(lnAUC0t)[5,3]/2) * (1/L1+1/L2))
SE_AUC0INF<-sqrt((anova(lnAUC0INF)[5,3]/2) * (1/L1+1/L2)) 

Z_Cmax<-0.2*(ref_Cmax/SE_Cmax)-qnorm(0.95)
Z_AUC0t<-0.2*(ref_AUC0t/SE_AUC0t)-qnorm(0.95)
Z_AUC0INF<-0.2*(ref_AUC0INF/SE_AUC0INF)-qnorm(0.95)

T_Cmax<-0.2*(ref_Cmax/SE_Cmax)-qt(0.975,L1+L2-2)
T_AUC0t<-0.2*(ref_AUC0t/SE_AUC0t)-qt(0.975,L1+L2-2)
T_AUC0INF<-0.2*(ref_AUC0INF/SE_AUC0INF)-qt(0.975,L1+L2-2)

PowerCmaxT<-pt(T_Cmax,L1+L2-2)
PowerTAUC0t<-pt(T_AUC0t,L1+L2-2)
PowerTAUC0INF<-pt(T_AUC0INF,L1+L2-2)

##90$CI (log transformation)
lowerCmax<-100*exp((test_Cmax-ref_Cmax)-(T*SE_Cmax))
upperCmax<-100*exp((test_Cmax-ref_Cmax)+(T*SE_Cmax))
lowerAUC0t<-100*exp((test_AUC0t-ref_AUC0t)-(T*SE_AUC0t))
UpperAUC0t<-100*exp((test_AUC0t-ref_AUC0t)+(T*SE_AUC0t))
LowerAUC0INF<-100*exp((test_AUC0INF - ref_AUC0INF)-(T*SE_AUC0INF))
UpperAUC0INF<-100*exp((test_AUC0INF - ref_AUC0INF)+(T*SE_AUC0INF))
 }
} 
#Table 1:Statistical Summaries for Bioequivalence Study 
zz <- file("Statistical_summaries.txt", open="wt")
sink(zz)
description_version()
cat("\n")
cat("\n")
cat("\n")
cat("Statistical Summaries for Pivotal Parameters of Bioequivalence Study (N=",L1+L2,")\n")
cat("--------------------------------------------------------------------------\n")
cat("\n")
 outputS1<-data.frame(Parameters=c("Cmax","AUC0-t","AUC0-inf","ln(Cmax)","ln(AUC0-t)","ln(AUC0-inf)" ),
                      Test_Mean=c(formatC(mean(outputCmax$Test),format="f",digits=3),formatC(mean(outputAUC0t$Test),format="f",digits=3),formatC(mean(outputAUC0INF$Test),format="f",digits=3),
                                  formatC(mean(outputlnCmax$Test),format="f",digits=3),formatC(mean(outputlnAUC0t$Test),format="f",digits=3),formatC(mean(outputlnAUC0INF$Test),format="f",digits=3)), 
                      Test_SD=c(formatC(sd(outputCmax$Test),format="f",digits=3),formatC(sd(outputAUC0t$Test),format="f",digits=3),formatC(sd(outputAUC0INF$Test),format="f",digits=3),
                                formatC(sd(outputlnCmax$Test),format="f",digits=3),formatC(sd(outputlnAUC0t$Test),format="f",digits=3),formatC(sd(outputlnAUC0INF$Test),format="f",digits=3)),
                      Ref_Mean=c(formatC(mean(outputCmax$Ref),format="f",digits=3),formatC(mean(outputAUC0t$Ref),format="f",digits=3),formatC(mean(outputAUC0INF$Ref),format="f",digits=3),
                                 formatC(mean(outputlnCmax$Ref),format="f",digits=3),formatC(mean(outputlnAUC0t$Ref),format="f",digits=3),formatC(mean(outputlnAUC0INF$Ref),format="f",digits=3)),
                      Ref_SD=c(formatC(sd(outputCmax$Ref),format="f",digits=3),formatC(sd(outputAUC0t$Ref),format="f",digits=3),formatC(sd(outputAUC0INF$Ref),format="f",digits=3),
                               formatC(sd(outputlnCmax$Ref),format="f",digits=3),formatC(sd(outputlnAUC0t$Ref),format="f",digits=3),formatC(sd(outputlnAUC0INF$Ref),format="f",digits=3))) 
                                                

show(outputS1)
cat("\n")
cat("\n")
cat("Statistical Summaries for Pivotal Parameters of Bioequivalence Study (N=",L1+L2,") (cont'd)\n")
cat("-------------------------------------------------------------------------------------\n")
cat("\n")
if(replicated){
RlowerCmax<-formatC(lowerCmax,format="f",digits=3)
RlowerAUC0t<-formatC(lowerAUC0t,format="f",digits=3)
RlowerAUC0INF<-formatC(lowerAUC0INF,format="f",digits=3)

 if(prdcount==3){
SlnCmax<-formatC(100*exp(summary(modlnCmax)[20][[1]][5,1]),format="f",digits=3)
SlnAUC0t<-formatC(100*exp(summary(modlnAUC0t)[20][[1]][5,1]),format="f",digits=3)
SlnAUC0INF<-formatC(100*exp(summary(modlnAUC0INF)[20][[1]][5,1]),format="f",digits=3)
 }
  if (prdcount==4){
SlnCmax<-formatC(100*exp(summary(modlnCmax)[20][[1]][6,1]),format="f",digits=3)
SlnAUC0t<-formatC(100*exp(summary(modlnAUC0t)[20][[1]][6,1]),format="f",digits=3)
SlnAUC0INF<-formatC(100*exp(summary(modlnAUC0INF)[20][[1]][6,1]),format="f",digits=3)
  }
  if (prdcount==5){
SlnCmax<-formatC(100*exp(summary(modlnCmax)[20][[1]][7,1]),format="f",digits=3)
SlnAUC0t<-formatC(100*exp(summary(modlnAUC0t)[20][[1]][7,1]),format="f",digits=3)
SlnAUC0INF<-formatC(100*exp(summary(modlnAUC0INF)[20][[1]][7,1]),format="f",digits=3)
  }
  if (prdcount==6){
SlnCmax<-formatC(100*exp(summary(modlnCmax)[20][[1]][8,1]),format="f",digits=3)
SlnAUC0t<-formatC(100*exp(summary(modlnAUC0t)[20][[1]][8,1]),format="f",digits=3)
SlnAUC0INF<-formatC(100*exp(summary(modlnAUC0INF)[20][[1]][8,1]),format="f",digits=3)
  } 

RupperCmax<-formatC(upperCmax,format="f",digits=3)
RupperAUC0t<-formatC(upperAUC0t,format="f",digits=3)
RupperAUC0INF<-formatC(upperAUC0INF,format="f",digits=3)

outputS2<-data.frame(Parameters=c("ln(Cmax)","ln(AUC0-t)","ln(AUC0-inf)" ),                      
                     CI90_lower=c(RlowerCmax,RlowerAUC0t,RlowerAUC0INF), 
                     Point_estimated=c(SlnCmax,SlnAUC0t,SlnAUC0INF),
                     CI90_upper=c(RupperCmax,RupperAUC0t,RupperAUC0INF))  
                      


show(outputS2)
cat("\n")
cat("-------------------------------------------------------------------------\n")
cat("CI90: 90% confidence interval \n")
cat("--------------------------------------------------------------------------\n")
}
else{
  if(parallel){
  RlowerCmax<-formatC(lowerCmax,format="f",digits=3)
  RlowerAUC0t<-formatC(lowerAUC0t,format="f",digits=3)
  RlowerAUC0INF<-formatC(lowerAUC0INF,format="f",digits=3)
  
  RupperCmax<-formatC(upperCmax,format="f",digits=3)
  RupperAUC0t<-formatC(upperAUC0t,format="f",digits=3)
  RupperAUC0INF<-formatC(upperAUC0INF,format="f",digits=3)
  
  SlnCmax<-formatC(100*exp(summary(modlnCmax)[20][[1]][2,1]),format="f",digits=3)
  SlnAUC0t<-formatC(100*exp(summary(modlnAUC0t)[20][[1]][2,1]),format="f",digits=3)
  SlnAUC0INF<-formatC(100*exp(summary(modlnAUC0INF)[20][[1]][2,1]),format="f",digits=3)
  
  outputS2<-data.frame(Parameters=c("ln(Cmax)","ln(AUC0-t)","ln(AUC0-inf)" ),                      
                     CI90_lower=c(RlowerCmax,RlowerAUC0t,RlowerAUC0INF), 
                     Point_estimated=c(SlnCmax,SlnAUC0t,SlnAUC0INF),
                     CI90_upper=c(RupperCmax,RupperAUC0t,RupperAUC0INF))  
                      


show(outputS2)
cat("\n")
cat("-------------------------------------------------------------------------\n")
cat("CI90: 90% confidence interval \n")
cat("--------------------------------------------------------------------------\n")
  }
  else{
outputS2<-data.frame(Parameters=c("Cmax","AUC0-t","AUC0-inf","ln(Cmax)","ln(AUC0-t)","ln(AUC0-inf)" ),                      
                      F_value=c(formatC(anova(Cmax)[4,4],format="f",digits=3), formatC(anova(AUC0t)[4,4],format="f",digits=3), formatC(anova(AUC0INF)[4,4],format="f",digits=3),
                                formatC(anova(lnCmax)[4,4],format="f",digits=3),formatC(anova(lnAUC0t)[4,4],format="f",digits=3),formatC(anova(lnAUC0INF)[4,4],format="f",digits=3)), 
                      P_value=c(formatC(anova(Cmax)[4,5],format="f",digits=3),formatC(anova(AUC0t)[4,5],format="f",digits=3), formatC(anova(AUC0INF)[4,5],format="f",digits=3),
                          formatC(anova(lnCmax)[4,5],format="f",digits=3),formatC(anova(lnAUC0t)[4,5],format="f",digits=3),formatC(anova(lnAUC0INF)[4,5],format="f",digits=3)),  
                      CI90_lower= c("-","-","-",formatC(lowerCmax,format="f",digits=3),formatC(lowerAUC0t,format="f",digits=3),formatC(LowerAUC0INF,format="f",digits=3)), 
                      Point_estimated=c("-","-","-",formatC(100*exp(test_Cmax-ref_Cmax),format="f",digits=3),formatC(100*exp(test_AUC0t-ref_AUC0t),format="f",digits=3),formatC(100*exp(test_AUC0INF - ref_AUC0INF),format="f",digits=3)),
                      CI90_upper= c("-","-","-",formatC(upperCmax,format="f",digits=3),formatC(UpperAUC0t,format="f",digits=3),formatC(UpperAUC0INF,format="f",digits=3)))  
                      #Power=c("-","-","-",formatC(PowerCmaxT,format="f",digits=3),formatC(PowerTAUC0t,format="f",digits=3),formatC(PowerTAUC0INF,format="f",digits=3))) 


show(outputS2)
cat("\n")
cat("-------------------------------------------------------------------------\n")
cat("Both F values and P values were obtained from ANOVA,respectively.\n")
cat("CI90: 90% confidence interval \n")
cat("Please note: no posterior power calculated. Ref.: Hoenig JM and Heisey DM. \n")
cat("The abuse of power: the pervasive fallacy of power calculations for data \n")
cat("analysis. The American Statistician 55/1, 19-24 (2001). Also the discussions \n")
cat("at Bebac Forum --> http://forum.bebac.at for more info.\n")
cat("--------------------------------------------------------------------------\n")
#cat("Power: required to detect at least (1-Power) of differences (%).\n")
  }
}
cat("\n")
cat("\n")
cat("\n")
cat("\n")
#Table 2:Summaries for Pharmacokinetic Parameters 
cat("Summaries for Misc. Pharmacokinetic Parameters (N=",L1+L2,")\n")
cat("\n")
cat("\n")
cat("                      Test                Reference        \n")
cat("------------------------------------------------------\n")
cat("\n")
outputTest<-data.frame(Parameters=c("Cl/F","Lambda_z","Tmax","T1/2(z)","Vd/F","MRT0inf","AUCratio" ),
                        Mean_T=c(formatC(mean(outputClF$Test),format="f",digits=3),formatC(mean(outputLambda$Test),format="f",digits=3),formatC(mean(outputTmax$Test),format="f",digits=3),formatC(mean(outputT12$Test),format="f",digits=3),
                                    formatC(mean(outputVdF$Test),format="f",digits=3),formatC(mean(outputMRT0INF$Test),format="f",digits=3),formatC(mean(outputAUC0t_AUC0INF$Test),format="f",digits=3)),
                        SD_T=c(formatC(sd(outputClF$Test),format="f",digits=3),formatC(sd(outputLambda$Test),format="f",digits=3),formatC(sd(outputTmax$Test),format="f",digits=3),formatC(sd(outputT12$Test),format="f",digits=3),
                                  formatC(sd(outputVdF$Test),format="f",digits=3),formatC(sd(outputMRT0INF$Test),format="f",digits=3),formatC(sd(outputAUC0t_AUC0INF$Test),format="f",digits=3) ),
                        CV_T=c(formatC(((sd(outputClF$Test)/mean(outputClF$Test))*100),format="f",digits=3),formatC(((sd(outputLambda$Test)/mean(outputLambda$Test))*100),format="f",digits=3),
                                  formatC(((sd(outputTmax$Test)/mean(outputTmax$Test))*100),format="f",digits=3),formatC(((sd(outputT12$Test)/mean(outputT12$Test))*100),format="f",digits=3),
                                  formatC(((sd(outputVdF$Test)/mean(outputVdF$Test))*100),format="f",digits=3),formatC(((sd(outputMRT0INF$Test)/mean(outputMRT0INF$Test))*100),format="f",digits=3),
                                  formatC(((sd(outputAUC0t_AUC0INF$Test)/mean(outputAUC0t_AUC0INF$Test))*100),format="f",digits=3)),
                                 
                        Mean_R=c(formatC(mean(outputClF$Ref),format="f",digits=3),formatC(mean(outputLambda$Ref),format="f",digits=3),formatC(mean(outputTmax$Ref),format="f",digits=3),formatC(mean(outputT12$Ref),format="f",digits=3),
                                   formatC(mean(outputVdF$Ref),format="f",digits=3),formatC(mean(outputMRT0INF$Ref),format="f",digits=3),formatC(mean(outputAUC0t_AUC0INF$Ref),format="f",digits=3)),
                        SD_R=c(formatC(sd(outputClF$Ref),format="f",digits=3),formatC(sd(outputLambda$Ref),format="f",digits=3),formatC(sd(outputTmax$Ref),format="f",digits=3),formatC(sd(outputT12$Ref),format="f",digits=3), 
                                 formatC(sd(outputVdF$Ref),format="f",digits=3),formatC(sd(outputMRT0INF$Ref),format="f",digits=3),formatC(sd(outputAUC0t_AUC0INF$Ref),format="f",digits=3)),
                        CV_R=c(formatC(((sd(outputClF$Ref)/mean(outputClF$Ref))*100),format="f",digits=3),formatC(((sd(outputLambda$Ref)/mean(outputLambda$Ref))*100),format="f",digits=3), 
                                 formatC(((sd(outputTmax$Ref)/mean(outputTmax$Ref))*100),format="f",digits=3),formatC(((sd(outputT12$Ref)/mean(outputT12$Ref))*100),format="f",digits=3),
                                 formatC(((sd(outputVdF$Ref)/mean(outputVdF$Ref))*100),format="f",digits=3),formatC(((sd(outputMRT0INF$Ref)/mean(outputMRT0INF$Ref))*100),format="f",digits=3),
                                 formatC(((sd(outputAUC0t_AUC0INF$Ref)/mean(outputAUC0t_AUC0INF$Ref))*100),format="f",digits=3)))          
                        
colnames(outputTest)<- c("Parameters"," Mean"," SD"," CV","    Mean"," SD"," CV")
show(outputTest)
cat("\n")
cat("-----------------------------------------------------\n")
cat("AUCratio: (AUC0-t/AUC0-inf)*100 \n")
cat("-----------------------------------------------------\n")
sink()
}
 
 