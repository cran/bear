###
### NCA - for manual data point selection - YJ
### other methods like this: ARS(), aic(), TTT(), TTTAIC(), TTTARS(). 
###
NCA<-function(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,
               SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split,
               Tau, TlastD,SingleRdata0,SingleTdata0,
               Demo=TRUE,BANOVA=FALSE,replicated=FALSE,MIX=FALSE, parallel=FALSE, multiple=FALSE)  
               ## switch Demo=TRUE to allow Demo to save Lambda_z_selection as .csv & it works. --YJ
{
options(warn=-1)
lin.AUC<-lin.AUC
pAUC<-pAUC               ### for pAUC
pAUC_start<-pAUC_start
pAUC_end<-pAUC_end
lambda_z_calc<-lambda_z_calc
BE_LL<-BE_LL
BE_UL<-BE_UL

lambda_z_regr_select_ref<- lambda_z_regr_select_ref
lambda_z_regr_select_test<- lambda_z_regr_select_test
###
### label which AUC calculation method has been used. -YJ
###
cat("\n")
if(lin.AUC){
cat("*** The linear trapezoidal method is used to calculate AUC and AUMC.\n")}
else{
cat("*** The linear-up/log-down trapezoidal method is used to calculate\n    AUC and AUMC.\n")
}
### 
###
### we try to plot regr lines for lambda(z); it works great. we need test with replicated BE. -YJ
###
cat("\n\n Warning: now is to save all linear regression plots\n for lambda_z estimation.\n\n");
readline(" It may take a while to finish. Press Enter to proceed...")

if(multiple){
  if(replicated){
    NCAreglplot(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,
                SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split,
                Tau, TlastD,SingleRdata0,SingleTdata0,
                Demo=TRUE,BANOVA=FALSE,replicated=TRUE,MIX=FALSE, parallel=FALSE, multiple=TRUE)}
   else {
    NCAreglplot(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,
                SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split,
                Tau, TlastD,SingleRdata0,SingleTdata0,
                Demo=TRUE,BANOVA=FALSE,replicated=FALSE,MIX=FALSE, parallel=FALSE, multiple=TRUE)}
    }
else{
 if(replicated){
    NCAreglplot(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,
               SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split,
               Tau, TlastD,SingleRdata0,SingleTdata0,
               Demo=TRUE,BANOVA=FALSE,replicated=TRUE,MIX=FALSE, parallel=FALSE, multiple=FALSE)}
  else{
    NCAreglplot(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,
               SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split,
               Tau, TlastD,SingleRdata0,SingleTdata0,
               Demo=TRUE,BANOVA=FALSE,replicated=FALSE,MIX=FALSE, parallel=FALSE, multiple=FALSE)}
}
###
#fitting data with linear regression model
#cat("<<Output: linear regression model: conc. vs. time>>\n")
#split dataframe into sub-dataframe by subject for reference data
#"time.ref" means "kel"
SingleRdata<-na.omit(SingleRdata)  ### v2.6.1
SingleTdata<-na.omit(SingleTdata)  ### v2.6.1

if(replicated){
       R.split<-split(SingleRdata, list(SingleRdata$code))
       write.csv(data.frame(subj=ref_data$subj,seq=ref_data$seq,period=ref_data$prd,drug=ref_data$drug,
                 code=ref_data$code,time=ref_data$time,conc=formatC(ref_data$conc_data,format="f",digits=3)),
                 file=lambda_z_regr_select_ref,row.names=FALSE)
       Lm1 <- lmList(conc ~ time |code, data = ref_data)   ## lmList() is from nlme; to list lm objects; here 'conc' is log10(conc), 
                                                           ## not untransformed conc.(conc_data).

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
        write.csv(data.frame(subj=ref_data$subj,time=ref_data$time,conc=ref_data$conc_data),
                  file=lambda_z_regr_select_ref,row.names=FALSE)

        Lm1 <- lmList(conc ~ time |subj, data = ref_data)  ##lmList() is from nlme; to list lm objects;
                                                           ## not untransformed conc.(conc_data).
        ### cat("\n\n show ref_data as follows:\n\n");show(ref_data);cat("\n\n");readline("Pause...")
       
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
 CminRef<-0
 AUCINFRef<-0
 AUCTRef<-0
 if(pAUC){pAUCTRef<-0}  ### for pAUC
 TmaxRef<-0
 MRTINFRef<-0
 T12Ref<-0
 VdFRef<-0
 KelRef<-0
 ClFRef<-0
 CavRef<-0
 FluRef<-0
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
          Cmin_ref<-0
          aumc_ref<-0
          if(pAUC){             ### calc. pAUC here if pAUC is TRUE; ### for pAUC
          pauc_ref<-0           ### need to calc one more parameters with pAUC.  --YJ
          pauc_ref_range<-""    ### for labeling the range of pAUC with '***'
          }
          
          for(i in 2:length(R.split[[j]][["time"]])){
             #calculate AUC and exclude AUC==NA (auc<-0) -HS
             ###
             ### original is all linear trapezoidal; -- YJ
             ### now we add lin-up/log-down trapezoidal method. -YJ
             ###
             if(lin.AUC){
                  auc_ref[i]<-(R.split[[j]][["time"]][i]-R.split[[j]][["time"]][i-1])*(R.split[[j]][["conc"]][i]+R.split[[j]][["conc"]][i-1])* 0.5}
             else{
                if(R.split[[j]][["conc"]][i]>R.split[[j]][["conc"]][i-1] || R.split[[j]][["conc"]][i] == R.split[[j]][["conc"]][i-1]){
                  auc_ref[i]<-(R.split[[j]][["time"]][i]-R.split[[j]][["time"]][i-1])*(R.split[[j]][["conc"]][i]+R.split[[j]][["conc"]][i-1])* 0.5}
                else{
                  auc_ref[i]<-(R.split[[j]][["time"]][i]-R.split[[j]][["time"]][i-1])*(R.split[[j]][["conc"]][i]-R.split[[j]][["conc"]][i-1])/
                              log(R.split[[j]][["conc"]][i]/R.split[[j]][["conc"]][i-1])   ### lin-up/log-down trapezoidal --YJ
                    }
             }
             if(pAUC){                 ### for pAUC
                    if(R.split[[j]][["time"]][i]<=pAUC_start||R.split[[j]][["time"]][i]>pAUC_end){  ### to exclude not pAUC part.
                      pauc_ref[i]<-0
                      if(R.split[[j]][["time"]][i]==pAUC_start) {pauc_ref_range[i]<-"***"}  ### must be since it is counting from i (=2) not (i-1)
                      else{pauc_ref_range[i]<-""}
                    }
                    else{
                      pauc_ref[i]<-auc_ref[i];pauc_ref_range[i]<-"***"
                    }
                    if(R.split[[j]][["time"]][1]==pAUC_start) pauc_ref_range[1]<-"***"
                  }             
             ### 
             auc_ref[i]<-auc_ref[i]+auc_ref[i-1]
             if(pAUC){                ### for pAUC
               if(R.split[[j]][["time"]][i]>pAUC_start && R.split[[j]][["time"]][i]<= pAUC_end){
                  pauc_ref[i]<-pauc_ref[i]+pauc_ref[i-1]}
                else{
                  pauc_ref[i]<-0}
             }             
             #calculate AUMC
             if(multiple){
                if (lin.AUC){    ### linear trapezoidal way  --YJ
                aumc_ref[i]<-((R.split[[j]][["time"]][i]-TlastD)*(R.split[[j]][["conc"]][i])+(R.split[[j]][["time"]][i-1]-TlastD)*(R.split[[j]][["conc"]][i-1]))*
                          ((R.split[[j]][["time"]][i]-TlastD)-(R.split[[j]][["time"]][i-1]-TlastD))* 0.5}
                else{            ### still linear trapezoidal  --YJ
                if(R.split[[j]][["conc"]][i]>R.split[[j]][["conc"]][i-1] || R.split[[j]][["conc"]][i] == R.split[[j]][["conc"]][i-1]){
                aumc_ref[i]<-((R.split[[j]][["time"]][i]-TlastD)*(R.split[[j]][["conc"]][i])+(R.split[[j]][["time"]][i-1]-TlastD)*(R.split[[j]][["conc"]][i-1]))*
                          ((R.split[[j]][["time"]][i]-TlastD)-(R.split[[j]][["time"]][i-1]-TlastD))* 0.5}
                else{            ### logarithmic trapezoidal  --YJ
                aumc_ref[i]<-((R.split[[j]][["time"]][i]-TlastD)-(R.split[[j]][["time"]][i-1]-TlastD))*((R.split[[j]][["time"]][i]-TlastD)*(R.split[[j]][["conc"]][i])-
                             (R.split[[j]][["time"]][i-1]-TlastD)*(R.split[[j]][["conc"]][i-1]))/log(R.split[[j]][["conc"]][i]/R.split[[j]][["conc"]][i-1])-
                             ((R.split[[j]][["time"]][i]-TlastD)-(R.split[[j]][["time"]][i-1]-TlastD))^2*(R.split[[j]][["conc"]][i]-R.split[[j]][["conc"]][i-1])/
                             log(R.split[[j]][["conc"]][i]/R.split[[j]][["conc"]][i-1])^2
                }
               }
             }
             else{
             if (lin.AUC){    ### linear trapezoidal way  --YJ
                aumc_ref[i]<-((R.split[[j]][["time"]][i])*(R.split[[j]][["conc"]][i])+(R.split[[j]][["time"]][i-1])*(R.split[[j]][["conc"]][i-1]))*
                          ((R.split[[j]][["time"]][i])-(R.split[[j]][["time"]][i-1]))* 0.5}
                else{
                if(R.split[[j]][["conc"]][i]>R.split[[j]][["conc"]][i-1] || R.split[[j]][["conc"]][i] == R.split[[j]][["conc"]][i-1]){
                aumc_ref[i]<-((R.split[[j]][["time"]][i])*(R.split[[j]][["conc"]][i])+(R.split[[j]][["time"]][i-1])*(R.split[[j]][["conc"]][i-1]))*
                          ((R.split[[j]][["time"]][i])-(R.split[[j]][["time"]][i-1]))* 0.5}
                else{
                aumc_ref[i]<-((R.split[[j]][["time"]][i])-(R.split[[j]][["time"]][i-1]))*((R.split[[j]][["time"]][i])*(R.split[[j]][["conc"]][i])-
                             (R.split[[j]][["time"]][i-1])*(R.split[[j]][["conc"]][i-1]))/log(R.split[[j]][["conc"]][i]/R.split[[j]][["conc"]][i-1])-
                             ((R.split[[j]][["time"]][i])-(R.split[[j]][["time"]][i-1]))^2*(R.split[[j]][["conc"]][i]-R.split[[j]][["conc"]][i-1])/
                             log(R.split[[j]][["conc"]][i]/R.split[[j]][["conc"]][i-1])^2
                }
                }
               }
             aumc_ref[i]<-aumc_ref[i]+aumc_ref[i-1]
             Cmax_ref<-max(R.split[[j]][["conc"]], na.rm = FALSE)
             Cmin_ref<-min(R.split[[j]][["conc"]], na.rm = FALSE)
             subgr= c(Cmax_ref)
             tmax_ref<-R.split[[j]][R.split[[j]][["conc"]] %in% subgr,]
              }

              #calculate AUC (0~INF)
               auc.infinity<-R.split[[j]][["conc"]][length(R.split[[j]][["conc"]])]/ke
               aucINF<-auc_ref[length(R.split[[j]][["conc"]])]+auc.infinity

                #calculate AUMC (0~INF)
                  if(multiple){
                  aumc.infinity_1<-(R.split[[j]][["conc"]][length(R.split[[j]][["conc"]])])*(R.split[[j]][["time"]][length(R.split[[j]][["time"]])]-TlastD)/ke
                  }
                  else{
                  aumc.infinity_1<-(R.split[[j]][["conc"]][length(R.split[[j]][["conc"]])])*(R.split[[j]][["time"]][length(R.split[[j]][["time"]])])/ke
                  }
                  aumc.infinity_2<-(R.split[[j]][["conc"]][length(R.split[[j]][["conc"]])]/(ke^2))
                  aumcINF<-aumc_ref[length(R.split[[j]][["conc"]])]+aumc.infinity_1+aumc.infinity_2

                  #for summary result
                  CmaxRef[j]<-Cmax_ref
                  CminRef[j]<-Cmin_ref
                  AUCINFRef[j]<-aucINF
                  AUCTRef[j]<-auc_ref[length(R.split[[j]][["conc"]])]
                  if(pAUC){pAUCTRef[j]<-max(pauc_ref)}   ### the final pauc_ref.  --YJ    ### for pAUC
                  TmaxRef[j]<-ifelse(multiple, tmax_ref$time - TlastD, tmax_ref$time) #revised: tmax_ref[,5]  # -YJ
                  T12Ref[j]<-log(2)/ke
                  KelRef[j]<-ke
                 
                  if(multiple){
                       VdFRef[j]<-Dose/((auc_ref[length(R.split[[j]][["conc"]])])*ke)
                       ClFRef[j]<-Dose/(auc_ref[length(R.split[[j]][["conc"]])])
                       MRTINFRef[j]<-(aumc_ref[length(R.split[[j]][["conc"]])])/(auc_ref[length(R.split[[j]][["conc"]])])
                       CavRef[j]<-(auc_ref[length(R.split[[j]][["conc"]])])/Tau
                       FluRef[j]<-((Cmax_ref-Cmin_ref)/((auc_ref[length(R.split[[j]][["conc"]])])/Tau))*100
                      }
                      else{
                       VdFRef[j]<-Dose/(aucINF*ke)
                       ClFRef[j]<-Dose/aucINF
                       MRTINFRef[j]<-aumcINF/aucINF 
                      }

                 cat("\n")
                 if(replicated){
                 cat("<< NCA Outputs:- Subj.#",su,", Seq",se,", Prd",pr," (Ref.)>>\n")
                    }
                   else{
                 cat("<< NCA Outputs:- Subj.#",su," (Ref.)>>\n")
                    }
                 cat("--------------------------------------------------------------------------\n")
                 if(pAUC){        ### for pAUC
                         ### pAUC_output<-data.frame(R.split[[j]][["subj"]],R.split[[j]][["time"]],R.split[[j]][["conc"]],
                         ### formatC(pauc_ref,format="f",digits=3),pauc_ref_range)
                         output<-data.frame(R.split[[j]][["subj"]],R.split[[j]][["time"]],R.split[[j]][["conc"]],
                         formatC(auc_ref,format="f",digits=3),formatC(aumc_ref,format="f",digits=3),
                         formatC(pauc_ref,format="f",digits=3),pauc_ref_range)
                         } ### insert pAUC here
                    else {
                         output<-data.frame(R.split[[j]][["subj"]],R.split[[j]][["time"]],R.split[[j]][["conc"]],
                         formatC(auc_ref,format="f",digits=3),formatC(aumc_ref,format="f",digits=3))
                  }
                 if(multiple){
                    if(pAUC){     ### for pAUC
                          pAUC_label<-""
                          pAUC_label<-paste("pAUC(",pAUC_start,"-",pAUC_end,")",sep="")
                          colnames(output)<-list("subj","time","conc","AUC(tau)ss","AUMC(tau)ss",pAUC_label," * ")
                          }
                      else{
                          colnames(output)<-list("subj","time","conc","AUC(tau)ss","AUMC(tau)ss")
                          }
                  }
                  else{
                   if(pAUC){            ### for pAUC
                         pAUC_label<-""
                         pAUC_label<-paste("pAUC(",pAUC_start,"-",pAUC_end,")",sep="")
                         colnames(output)<-list("subj","time","conc","AUC(0-t)","AUMC(0-t)",pAUC_label," * ")
                           }
                      else {
                         colnames(output)<-list("subj","time","conc","AUC(0-t)","AUMC(0-t)")
                        }
                  }
                 show(output)
                 cat("----------------------------------------------------------\n")
                 if(pAUC){cat("***: the covered range of pAUC\n\n")}else{cat("\n\n")}
                 ### readline(" ... pause here\n")   ### for debugging   ### for pAUC
                 
                 cat("---------------------------------------------------------\n\n")
              cat("\n<<Selected data points for lambda_z estimation>>\n")
              cat("--------------------------------------------------\n")
              show(rdata.split[[j]])
              
              cat("\n<<Final PK Parameters>>\n")
              cat("----------------------------\n")
              if(multiple){
              cat("           R sq. =",R_sq ,"\n")
              cat("Adj. R sq. (ARS) =",AR_sq ,"\n")
              cat("        lambda_z =",ke ,"\n")
              cat("         Cmax_ss =",Cmax_ref ,"\n")
              cat("         Cmin_ss =",Cmin_ref ,"\n")
              cat("         Tmax_ss =",TmaxRef[j] ,"\n")
              cat("             Cav =",(auc_ref[length(R.split[[j]][["conc"]])])/Tau ,"\n")
              cat("    Fluctuation% =",((Cmax_ref-Cmin_ref)/((auc_ref[length(R.split[[j]][["conc"]])])/Tau))*100 ,"\n")
              cat("            Cl/F =",Dose/(auc_ref[length(R.split[[j]][["conc"]])]),"\n")
              cat("            Vd/F =",Dose/((auc_ref[length(R.split[[j]][["conc"]])])*ke),"\n")
              cat("         T1/2(z) =",log(2)/ke,"\n")
              if(pAUC){                 ### for pAUC
              cat("           *pAUC =",pAUCTRef[j],"\n")}              
              cat("      AUC(tau)ss =",auc_ref[length(R.split[[j]][["conc"]])],"\n")
              cat("     AUMC(tau)ss =",aumc_ref[length(R.split[[j]][["conc"]])],"\n")
              cat("             MRT =",(aumc_ref[length(R.split[[j]][["conc"]])])/(auc_ref[length(R.split[[j]][["conc"]])]),"\n")
                }
              else{
              cat("           R sq. =",R_sq ,"\n")
              cat("Adj. R sq. (ARS) =",AR_sq ,"\n")
              cat("        lambda_z =",ke ,"\n")
              cat("            Cmax =",Cmax_ref ,"\n")
              cat("            Tmax =",TmaxRef[[j]] ,"\n")
              cat("            Cl/F =",Dose/aucINF,"\n")
              cat("            Vd/F =",Dose/(aucINF*ke),"\n")
              cat("         T1/2(z) =",log(2)/ke,"\n")
              if(pAUC){                  ### for pAUC
              cat("           *pAUC =",pAUCTRef[j],"\n")}              
              cat("        AUC(0-t) =",auc_ref[length(R.split[[j]][["conc"]])],"\n")
              cat("      AUC(0-inf) =",aucINF,"\n")
              cat("       AUMC(0-t) =",aumc_ref[length(R.split[[j]][["conc"]])],"\n")
              cat("     AUMC(0-inf) =",aumcINF,"\n")
              cat("        MRT(0-t) =",(aumc_ref[length(R.split[[j]][["conc"]])])/(auc_ref[length(R.split[[j]][["conc"]])]),"\n")
              cat("      MRT(0-inf) =",aumcINF/aucINF,"\n")
             }
              cat("----------------------------\n")
              if(pAUC) cat(" where *pAUC means ",pAUC_label,sep="")  ### make a note for 'pAUC'; ### for pAUC
              cat("\n\n")             
  }
#### linear regression for Test product
#split dataframe into sub-dataframe by subject for test data
#"time.test" means "kel"
if(replicated){
      T.split<-split(SingleTdata, list(SingleTdata$code))
      write.csv(data.frame(subj=test_data$subj,seq=test_data$seq,period=test_data$prd,drug=test_data$drug,
                 code=test_data$code,time=test_data$time,conc=formatC(test_data$conc_data,format="f",digits=3)),
                 file=lambda_z_regr_select_test,row.names=FALSE)

      Lm2 <- lmList(conc ~ time |code, data = test_data) 
         
      subj1<-0
      prd1<-0
      seq1<-0
      code1<-0
      AdjT<-0
      j<-0

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
        write.csv(data.frame(subj=test_data$subj,time=test_data$time,conc=test_data$conc_data),
                  file=lambda_z_regr_select_test,row.names=FALSE)
                    
        Lm2 <- lmList(conc ~ time |subj, data = test_data)
        
        subj1<-0
        AdjT<-0
        j<-0
        
        for (j in 1:(length(T.split))){
        subj1[j]<-T.split[[j]][["subj"]][1]
        AdjT[j]<-summary(Lm2)$adj.r.squared
                }
        
         keindex_test<-data.frame(subj=subj1, time=-2.3*(coef(Lm2)[2]),
             R_squared=summary(Lm2)$r.sq,Adj_R_squared=melt(AdjT)$value)
          
         }

#calculate AUC
CmaxTest<-0
CminTest<-0
AUCINFTest<-0
AUCTTest<-0
if(pAUC) pAUCTTest<-0     ### for pAUC
TmaxTest<-0
MRTINFTest<-0
T12Test<-0
VdFTest<-0
KelTest<-0
ClFTest<-0
CavTest<-0
FluTest<-0
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
          Cmin_test<-0
          tmax_test<-0
          aumc_test<-0
       if(pAUC){                 ### calc. pAUC here if pAUC is TRUE ### for pAUC
          pauc_test<-0           ### need to calc one more parameters with pAUC.  --YJ
          pauc_test_range<-""    ### for labeling the range of pAUC with '***'
        }          

          for(i in 2:length(T.split[[j]][["time"]])){
             ###
             ### original is all linear trapezoidal; -- YJ
             ### now we add lin-up/log-down trapezoidal method. -YJ
             ###
             if(lin.AUC){
                  auc_test[i]<-(T.split[[j]][["time"]][i]-T.split[[j]][["time"]][i-1])*(T.split[[j]][["conc"]][i]+T.split[[j]][["conc"]][i-1])* 0.5}
             else{
                if(T.split[[j]][["conc"]][i]>T.split[[j]][["conc"]][i-1] || T.split[[j]][["conc"]][i] == T.split[[j]][["conc"]][i-1]){
                  auc_test[i]<-(T.split[[j]][["time"]][i]-T.split[[j]][["time"]][i-1])*(T.split[[j]][["conc"]][i]+T.split[[j]][["conc"]][i-1])* 0.5}
                else{
                  auc_test[i]<-(T.split[[j]][["time"]][i]-T.split[[j]][["time"]][i-1])*(T.split[[j]][["conc"]][i]-T.split[[j]][["conc"]][i-1])/
                              log(T.split[[j]][["conc"]][i]/T.split[[j]][["conc"]][i-1])   ### lin-up/log-down trapezoidal --YJ
                    }
             }
             if(pAUC){
                    if(T.split[[j]][["time"]][i]<=pAUC_start||T.split[[j]][["time"]][i]>pAUC_end){  ### to exclude not pAUC part.
                      pauc_test[i]<-0
                      if(T.split[[j]][["time"]][i]==pAUC_start) {pauc_test_range[i]<-"***"}
                      else{pauc_test_range[i]<-""}
                    }
                    else{
                      pauc_test[i]<-auc_test[i];pauc_test_range[i]<-"***"
                    }
                    if(T.split[[j]][["time"]][1]==pAUC_start) pauc_test_range[1]<-"***"
                  }             
             ###
             ### auc_test[i]<-(T.split[[j]][["time"]][i]-T.split[[j]][["time"]][i-1])*(T.split[[j]][["conc"]][i]+T.split[[j]][["conc"]][i-1])* 0.5
             auc_test[i]<-auc_test[i]+auc_test[i-1]
             if(pAUC){      ### for pAUC
               if(T.split[[j]][["time"]][i]>pAUC_start && T.split[[j]][["time"]][i]<= pAUC_end){
                  pauc_test[i]<-pauc_test[i]+pauc_test[i-1]}
                else{
                  pauc_test[i]<-0}
             }             
             #calculate AUMC
             if(multiple){
                if (lin.AUC){    ### linear trapezoidal way  --YJ
                aumc_test[i]<-((T.split[[j]][["time"]][i]-TlastD)*(T.split[[j]][["conc"]][i])+(T.split[[j]][["time"]][i-1]-TlastD)*(T.split[[j]][["conc"]][i-1]))*
                          ((T.split[[j]][["time"]][i]-TlastD)-(T.split[[j]][["time"]][i-1]-TlastD))* 0.5}
                else{            ### still linear trapezoidal  --YJ
                if(T.split[[j]][["conc"]][i]>T.split[[j]][["conc"]][i-1] || T.split[[j]][["conc"]][i] == T.split[[j]][["conc"]][i-1]){
                aumc_test[i]<-((T.split[[j]][["time"]][i]-TlastD)*(T.split[[j]][["conc"]][i])+(T.split[[j]][["time"]][i-1]-TlastD)*(T.split[[j]][["conc"]][i-1]))*
                          ((T.split[[j]][["time"]][i]-TlastD)-(T.split[[j]][["time"]][i-1]-TlastD))* 0.5}
                else{            ### logarithmic trapezoidal  --YJ
                aumc_test[i]<-((T.split[[j]][["time"]][i]-TlastD)-(T.split[[j]][["time"]][i-1]-TlastD))*((T.split[[j]][["time"]][i]-TlastD)*(T.split[[j]][["conc"]][i])-
                             (T.split[[j]][["time"]][i-1]-TlastD)*(T.split[[j]][["conc"]][i-1]))/log(T.split[[j]][["conc"]][i]/T.split[[j]][["conc"]][i-1])-
                             ((T.split[[j]][["time"]][i]-TlastD)-(T.split[[j]][["time"]][i-1]-TlastD))^2*(T.split[[j]][["conc"]][i]-T.split[[j]][["conc"]][i-1])/
                             log(T.split[[j]][["conc"]][i]/T.split[[j]][["conc"]][i-1])^2
                }
               }
             }
             else{
             if (lin.AUC){    ### linear trapezoidal way  --YJ
                aumc_test[i]<-((T.split[[j]][["time"]][i])*(T.split[[j]][["conc"]][i])+(T.split[[j]][["time"]][i-1])*(T.split[[j]][["conc"]][i-1]))*
                          ((T.split[[j]][["time"]][i])-(T.split[[j]][["time"]][i-1]))* 0.5}
                else{
                if(T.split[[j]][["conc"]][i]>T.split[[j]][["conc"]][i-1] || T.split[[j]][["conc"]][i] == T.split[[j]][["conc"]][i-1]){
                aumc_test[i]<-((T.split[[j]][["time"]][i])*(T.split[[j]][["conc"]][i])+(T.split[[j]][["time"]][i-1])*(T.split[[j]][["conc"]][i-1]))*
                          ((T.split[[j]][["time"]][i])-(T.split[[j]][["time"]][i-1]))* 0.5}
                else{
                aumc_test[i]<-((T.split[[j]][["time"]][i])-(T.split[[j]][["time"]][i-1]))*((T.split[[j]][["time"]][i])*(T.split[[j]][["conc"]][i])-
                             (T.split[[j]][["time"]][i-1])*(T.split[[j]][["conc"]][i-1]))/log(T.split[[j]][["conc"]][i]/T.split[[j]][["conc"]][i-1])-
                             ((T.split[[j]][["time"]][i])-(T.split[[j]][["time"]][i-1]))^2*(T.split[[j]][["conc"]][i]-T.split[[j]][["conc"]][i-1])/
                             log(T.split[[j]][["conc"]][i]/T.split[[j]][["conc"]][i-1])^2
                }
                }
               }
             aumc_test[i]<-aumc_test[i]+aumc_test[i-1]
             Cmax_test<-max(T.split[[j]][["conc"]], na.rm = FALSE)
             Cmin_test<-min(T.split[[j]][["conc"]], na.rm = FALSE)
             subgr= c(Cmax_test)
             tmax_test<-T.split[[j]][T.split[[j]][["conc"]] %in% subgr,]
              }

              #calculate AUC (0~INF)
               auc.infinity<-T.split[[j]][["conc"]][length(T.split[[j]][["conc"]])]/ke1
               aucINF<-auc_test[length(T.split[[j]][["conc"]])]+auc.infinity

                #calculate AUMC (0~INF)
                   if(multiple){
                   aumc.infinity_1<-(T.split[[j]][["conc"]][length(T.split[[j]][["conc"]])])*(T.split[[j]][["time"]][length(T.split[[j]][["time"]])]-TlastD)/ke1
                   }
                   else{
                   aumc.infinity_1<-(T.split[[j]][["conc"]][length(T.split[[j]][["conc"]])])*(T.split[[j]][["time"]][length(T.split[[j]][["time"]])])/ke1
                   }
                  aumc.infinity_2<-(T.split[[j]][["conc"]][length(T.split[[j]][["conc"]])]/(ke1^2))
                  aumcINF<-aumc_test[length(T.split[[j]][["conc"]])]+aumc.infinity_1+aumc.infinity_2

                  #for summary result
                  CmaxTest[j]<-Cmax_test
                  CminTest[j]<-Cmin_test
                  AUCINFTest[j]<-aucINF
                  AUCTTest[j]<-auc_test[length(T.split[[j]][["conc"]])]
                  if(pAUC){pAUCTTest[j]<-max(pauc_test)}   ### the final pauc_ref.  ### for pAUC
                  TmaxTest[j]<-ifelse(multiple, tmax_test$time - TlastD, tmax_test$time)  ### for multiple, shoud -TlastD. -YJ
                  T12Test[j]<-log(2)/ke1
                  KelTest[j]<-ke1
                 
                       if(multiple){
                  VdFTest[j]<-Dose/((auc_test[length(T.split[[j]][["conc"]])])*ke1)
                  ClFTest[j]<-Dose/(auc_test[length(T.split[[j]][["conc"]])])
                  MRTINFTest[j]<-(aumc_test[length(T.split[[j]][["conc"]])])/(auc_test[length(T.split[[j]][["conc"]])])
                  CavTest[j]<-(auc_test[length(T.split[[j]][["conc"]])])/Tau
                  FluTest[j]<-((Cmax_test-Cmin_test)/((auc_test[length(T.split[[j]][["conc"]])])/Tau))*100
                    }
                    else{
                  VdFTest[j]<-Dose/(aucINF*ke1)
                  ClFTest[j]<-Dose/aucINF
                  MRTINFTest[j]<-aumcINF/aucINF     
                        }
                 cat("\n")
                 if(replicated){
                 cat("<< NCA Outputs:- Subj.#",su1,", Seq",se1,", Prd",pr1," (Test)>>\n")
                    }
                   else{
                 cat("<< NCA Outputs:- Subj.#",su1," (Test)>>\n")
                    }
                 cat("----------------------------------------------------------\n")
                 if(pAUC){              ### for pAUC
                         ### pAUC_output<-data.frame(R.split[[j]][["subj"]],R.split[[j]][["time"]],R.split[[j]][["conc"]],
                         ### formatC(pauc_ref,format="f",digits=3),pauc_ref_range)
                         output<-data.frame(T.split[[j]][["subj"]],T.split[[j]][["time"]],T.split[[j]][["conc"]],
                         formatC(auc_test,format="f",digits=3),formatC(aumc_test,format="f",digits=3),
                         formatC(pauc_test,format="f",digits=3),pauc_test_range)
                         } ### insert pAUC here
                    else {
                         output<-data.frame(T.split[[j]][["subj"]],T.split[[j]][["time"]],T.split[[j]][["conc"]],
                         formatC(auc_test,format="f",digits=3),formatC(aumc_test,format="f",digits=3))
                  }
                 if(multiple){
                    if(pAUC){           ### for pAUC
                          pAUC_label<-""
                          pAUC_label<-paste("pAUC(",pAUC_start,"-",pAUC_end,")",sep="")
                          colnames(output)<-list("subj","time","conc","AUC(tau)ss","AUMC(tau)ss",pAUC_label," * ")
                          }
                      else{
                          colnames(output)<-list("subj","time","conc","AUC(tau)ss","AUMC(tau)ss")
                          }
                  }
                  else{
                   if(pAUC){            ### for pAUC
                         pAUC_label<-""
                         pAUC_label<-paste("pAUC(",pAUC_start,"-",pAUC_end,")",sep="")
                         colnames(output)<-list("subj","time","conc","AUC(0-t)","AUMC(0-t)",pAUC_label," * ")
                           }
                      else {
                         colnames(output)<-list("subj","time","conc","AUC(0-t)","AUMC(0-t)")
                        }
                  }
                 show(output)
                 cat("----------------------------------------------------------\n")
                 if(pAUC){cat("***: the covered range of pAUC\n\n")}else{cat("\n\n")}
                 ### readline(" ... pause here\n")   ### for debugging

                 
                 cat("----------------------------------------------------------\n")

              cat("\n<<Selected data points for lambda_z estimation>>\n")
              cat("--------------------------------------------------\n")
              show(tdata.split[[j]]) 
              
              cat("\n<<Final PK Parameters>>\n")
              cat("----------------------------\n")
              if(multiple){
              cat("           R sq. =",R_sq1 ,"\n")
              cat("Adj. R sq. (ARS) =",AR_sq1 ,"\n")
              cat("        lambda_z =",ke1 ,"\n")
              cat("         Cmax_ss =",Cmax_test ,"\n")
              cat("         Cmin_ss =",Cmin_test ,"\n")
              cat("         Tmax_ss =",TmaxTest[j] ,"\n")
              cat("             Cav =",(auc_test[length(T.split[[j]][["conc"]])])/Tau ,"\n")
              cat("    Fluctuation% =",((Cmax_test-Cmin_test)/((auc_test[length(T.split[[j]][["conc"]])])/Tau))*100 ,"\n")
              cat("            Cl/F =",Dose/(auc_test[length(T.split[[j]][["conc"]])]),"\n")
              cat("            Vd/F =",Dose/((auc_test[length(T.split[[j]][["conc"]])])*ke1),"\n")
              cat("         T1/2(z) =",log(2)/ke1,"\n")
              if(pAUC){         ### for pAUC
              cat("           *pAUC =",pAUCTTest[j],"\n")}
              cat("      AUC(tau)ss =",auc_test[length(T.split[[j]][["conc"]])],"\n")
              cat("     AUMC(tau)ss =",aumc_test[length(T.split[[j]][["conc"]])],"\n")
              cat("             MRT =",(aumc_test[length(T.split[[j]][["conc"]])])/(auc_test[length(T.split[[j]][["conc"]])]),"\n")
              }
              else{  
              cat("           R sq. =",R_sq1 ,"\n")
              cat("Adj. R sq. (ARS) =",AR_sq1 ,"\n")
              cat("        lambda_z =",ke1 ,"\n")
              cat("            Cmax =",Cmax_test ,"\n")
              cat("            Tmax =",TmaxTest[j] ,"\n")
              cat("            Cl/F =",Dose/aucINF,"\n")
              cat("            Vd/F =",Dose/(aucINF*ke1),"\n")
              cat("         T1/2(z) =",log(2)/ke1,"\n")
              if(pAUC){       ### for pAUC
              cat("           *pAUC =",pAUCTTest[j],"\n")}
              cat("        AUC(0-t) =",auc_test[length(T.split[[j]][["conc"]])],"\n")
              cat("      AUC(0-inf) =",aucINF,"\n")
              cat("       AUMC(0-t) =",aumc_test[length(T.split[[j]][["conc"]])],"\n")
              cat("     AUMC(0-inf) =",aumcINF,"\n")
              cat("        MRT(0-t) =",(aumc_test[length(T.split[[j]][["conc"]])])/(auc_test[length(T.split[[j]][["conc"]])]),"\n")
              cat("      MRT(0-inf) =",aumcINF/aucINF,"\n")
    }
              cat("----------------------------\n")
              if(pAUC) cat(" where *pAUC means ",pAUC_label,sep="")  ### make a note for 'pAUC'; ### for pAUC
              cat("\n\n")     
  }


cat("--------------------------------------------------------------------------\n")
cat("<<Output: linear regression model (for Lambda_z): conc. vs. time>>\n")
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
cat("\n")
show(keindex_ref)
cat("--------------------------------------------------------------------------\n")
cat("<<Output: linear regression model (for Lambda_z): conc. vs. time>>\n")
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
cat("\n")
show(keindex_test)
cat("--------------------------------------------------------------------------\n")

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
  if(pAUC){
  sumindexR<-data.frame(subj=subjR,drug=c(1),Cmax=CmaxRef,AUC0t=AUCTRef,Cmin=CminRef,partAUC=pAUCTRef,    ### add pAUC as 'partAUC'
                       Tmax=TmaxRef,MRTINF=MRTINFRef,T12=T12Ref,VdF=VdFRef,Kel=KelRef,ClF=ClFRef,Cav=CavRef,Flu=FluRef)
  sumindexT<-data.frame(subj=subjT,drug=c(2),Cmax=CmaxTest,AUC0t=AUCTTest,Cmin=CminTest,partAUC=pAUCTTest,### add pAUC as 'partAUC'
                       Tmax=TmaxTest,MRTINF=MRTINFTest,T12=T12Test,VdF=VdFTest,Kel=KelTest,ClF=ClFTest,Cav=CavTest,Flu=FluTest)
  }
  else{
  sumindexR<-data.frame(subj=subjR,drug=c(1),Cmax=CmaxRef,AUC0t=AUCTRef,Cmin=CminRef,
                       Tmax=TmaxRef,MRTINF=MRTINFRef,T12=T12Ref,VdF=VdFRef,Kel=KelRef,ClF=ClFRef,Cav=CavRef,Flu=FluRef)
  sumindexT<-data.frame(subj=subjT,drug=c(2),Cmax=CmaxTest,AUC0t=AUCTTest,Cmin=CminTest,
                       Tmax=TmaxTest,MRTINF=MRTINFTest,T12=T12Test,VdF=VdFTest,Kel=KelTest,ClF=ClFTest,Cav=CavTest,Flu=FluTest)
      }
  }
  else{
  description_Repdrugcode()
   if(pAUC){
   sumindexR<-data.frame(subj=subjR,drug=c(1),Cmax=CmaxRef,AUC0t=AUCTRef,AUC0INF=AUCINFRef,partAUC=pAUCTRef,     ### add pAUC as 'partAUC'
                      Tmax=TmaxRef,MRTINF=MRTINFRef,T12=T12Ref,VdF=VdFRef,Kel=KelRef,ClF=ClFRef)
   sumindexT<-data.frame(subj=subjT,drug=c(2),Cmax=CmaxTest,AUC0t=AUCTTest,AUC0INF=AUCINFTest,partAUC=pAUCTTest, ### add pAUC as 'partAUC'
                     Tmax=TmaxTest,MRTINF=MRTINFTest,T12=T12Test,VdF=VdFTest,Kel=KelTest,ClF=ClFTest)
   }
   else {
   sumindexR<-data.frame(subj=subjR,drug=c(1),Cmax=CmaxRef,AUC0t=AUCTRef,AUC0INF=AUCINFRef,
                      Tmax=TmaxRef,MRTINF=MRTINFRef,T12=T12Ref,VdF=VdFRef,Kel=KelRef,ClF=ClFRef)
   sumindexT<-data.frame(subj=subjT,drug=c(2),Cmax=CmaxTest,AUC0t=AUCTTest,AUC0INF=AUCINFTest,
                     Tmax=TmaxTest,MRTINF=MRTINFTest,T12=T12Test,VdF=VdFTest,Kel=KelTest,ClF=ClFTest)
        }
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
     if(pAUC){              ### for pAUC  
     sumindexR<-data.frame(subj=subjR,drug=drugR,seq=seqR,prd=prdR,Cmax=CmaxRef,AUC0t=AUCTRef,AUC0INF=AUCINFRef,
                partAUC=pAUCTRef,Tmax=TmaxRef, MRTINF=MRTINFRef,T12=T12Ref,VdF=VdFRef,Kel=KelRef,ClF=ClFRef)
     sumindexT<-data.frame(subj=subjT,drug=drugT,seq=seqT,prd=prdT,Cmax=CmaxTest,AUC0t=AUCTTest,AUC0INF=AUCINFTest,
                partAUC=pAUCTTest,Tmax=TmaxTest,MRTINF=MRTINFTest,T12=T12Test,VdF=VdFTest,Kel=KelTest,ClF=ClFTest)
     }
     else{
     sumindexR<-data.frame(subj=subjR,drug=drugR,seq=seqR,prd=prdR,Cmax=CmaxRef,AUC0t=AUCTRef,AUC0INF=AUCINFRef,
                      Tmax=TmaxRef, MRTINF=MRTINFRef,T12=T12Ref,VdF=VdFRef,Kel=KelRef,ClF=ClFRef)
     sumindexT<-data.frame(subj=subjT,drug=drugT,seq=seqT,prd=prdT,Cmax=CmaxTest,AUC0t=AUCTTest,AUC0INF=AUCINFTest,
                     Tmax=TmaxTest,MRTINF=MRTINFTest,T12=T12Test,VdF=VdFTest,Kel=KelTest,ClF=ClFTest)
        }
   }
else{

if(multiple){
   description_Multipledrugcode()
          if(pAUC){         ### for pAUC
          sumindexR<-data.frame(subj=subjR,drug=c(1),seq=seqR,prd=prdR,Cmax=CmaxRef,AUC0t=AUCTRef,Cmin=CminRef,partAUC=pAUCTRef,
                      Tmax=TmaxRef,MRTINF=MRTINFRef,T12=T12Ref,VdF=VdFRef,Kel=KelRef,ClF=ClFRef,Cav=CavRef,Flu=FluRef)
          sumindexT<-data.frame(subj=subjT,drug=c(2),seq=seqT,prd=prdT,Cmax=CmaxTest,AUC0t=AUCTTest,Cmin=CminTest,partAUC=pAUCTTest,
                     Tmax=TmaxTest,MRTINF=MRTINFTest,T12=T12Test,VdF=VdFTest,Kel=KelTest,ClF=ClFTest,Cav=CavTest,Flu=FluTest)          
          }
          else {  
          sumindexR<-data.frame(subj=subjR,drug=c(1),seq=seqR,prd=prdR,Cmax=CmaxRef,AUC0t=AUCTRef,Cmin=CminRef,
                      Tmax=TmaxRef,MRTINF=MRTINFRef,T12=T12Ref,VdF=VdFRef,Kel=KelRef,ClF=ClFRef,Cav=CavRef,Flu=FluRef)
          sumindexT<-data.frame(subj=subjT,drug=c(2),seq=seqT,prd=prdT,Cmax=CmaxTest,AUC0t=AUCTTest,Cmin=CminTest,
                     Tmax=TmaxTest,MRTINF=MRTINFTest,T12=T12Test,VdF=VdFTest,Kel=KelTest,ClF=ClFTest,Cav=CavTest,Flu=FluTest)
          }
}
 else{ 
 description_drugcode()
          if(pAUC){         ### for pAUC
          sumindexR<-data.frame(subj=subjR,drug=c(1),seq=seqR,prd=prdR,Cmax=CmaxRef,AUC0t=AUCTRef,AUC0INF=AUCINFRef,
                     partAUC=pAUCTRef,Tmax=TmaxRef,MRTINF=MRTINFRef,T12=T12Ref,VdF=VdFRef,Kel=KelRef,ClF=ClFRef)
          sumindexT<-data.frame(subj=subjT,drug=c(2),seq=seqT,prd=prdT,Cmax=CmaxTest,AUC0t=AUCTTest,AUC0INF=AUCINFTest,
                     partAUC=pAUCTTest,Tmax=TmaxTest,MRTINF=MRTINFTest,T12=T12Test,VdF=VdFTest,Kel=KelTest,ClF=ClFTest)
          }
          else{
          sumindexR<-data.frame(subj=subjR,drug=c(1),seq=seqR,prd=prdR,Cmax=CmaxRef,AUC0t=AUCTRef,AUC0INF=AUCINFRef,
                      Tmax=TmaxRef,MRTINF=MRTINFRef,T12=T12Ref,VdF=VdFRef,Kel=KelRef,ClF=ClFRef)
          sumindexT<-data.frame(subj=subjT,drug=c(2),seq=seqT,prd=prdT,Cmax=CmaxTest,AUC0t=AUCTTest,AUC0INF=AUCINFTest,
                     Tmax=TmaxTest,MRTINF=MRTINFTest,T12=T12Test,VdF=VdFTest,Kel=KelTest,ClF=ClFTest)
          }
   }
 }
} 
#########
Total<-rbind(sumindexR,sumindexT)
lnpAUC<-0
if(parallel){
  if(multiple){
  if(pAUC){         ### for pAUC
  TotalData<-data.frame (subj=as.factor(Total$subj),drug=as.factor(Total$drug),Cmax_ss=Total$Cmax,AUCtau_ss=Total$AUC0t,
                         partAUC=Total$partAUC,lnCmax_ss=log(Total$Cmax),lnAUCtau_ss=log(Total$AUC0t),lnpAUC=log(Total$partAUC))  
  }
  else {
  TotalData<-data.frame (subj=as.factor(Total$subj),drug=as.factor(Total$drug),Cmax_ss=Total$Cmax,AUCtau_ss=Total$AUC0t,
                         lnCmax_ss=log(Total$Cmax),lnAUCtau_ss=log(Total$AUC0t))
  }
  }
  else{
  if(pAUC){         ### for pAUC
  TotalData<-data.frame (subj=as.factor(Total$subj),drug=as.factor(Total$drug),Cmax=Total$Cmax,AUC0t=Total$AUC0t,
                         AUC0INF=Total$AUC0INF,partAUC=Total$partAUC,lnCmax=log(Total$Cmax),lnAUC0t=log(Total$AUC0t),
                         lnAUC0INF=log(Total$AUC0INF),lnpAUC=log(Total$partAUC))  
  }
  else {
  TotalData<-data.frame (subj=as.factor(Total$subj),drug=as.factor(Total$drug),Cmax=Total$Cmax,AUC0t=Total$AUC0t,
                         AUC0INF=Total$AUC0INF,lnCmax=log(Total$Cmax),lnAUC0t=log(Total$AUC0t),lnAUC0INF=log(Total$AUC0INF))
   }
 }
}
else{
  if(multiple){
  if(pAUC){         ### for pAUC
  TotalData<-data.frame (subj=as.factor(Total$subj),drug=as.factor(Total$drug),seq=as.factor(Total$seq),
                        prd=as.factor(Total$prd),Cmax_ss=Total$Cmax,AUCtau_ss=Total$AUC0t,partAUC=Total$partAUC,
                        lnCmax_ss=log(Total$Cmax),lnAUCtau_ss=log(Total$AUC0t),lnpAUC=log(Total$partAUC))  
  }
  else {
  TotalData<-data.frame (subj=as.factor(Total$subj),drug=as.factor(Total$drug),seq=as.factor(Total$seq),
                        prd=as.factor(Total$prd),Cmax_ss=Total$Cmax,AUCtau_ss=Total$AUC0t,lnCmax_ss=log(Total$Cmax),
                        lnAUCtau_ss=log(Total$AUC0t))
  }
  }
  else{
  if(pAUC){         ### for pAUC
  TotalData<-data.frame (subj=as.factor(Total$subj),drug=as.factor(Total$drug),seq=as.factor(Total$seq),
                         prd=as.factor(Total$prd),Cmax=Total$Cmax,AUC0t=Total$AUC0t,AUC0INF=Total$AUC0INF,
                         partAUC=Total$partAUC,lnCmax=log(Total$Cmax),lnAUC0t=log(Total$AUC0t),
                         lnAUC0INF=log(Total$AUC0INF),lnpAUC=log(Total$partAUC))  
  }
  else {
  TotalData<-data.frame (subj=as.factor(Total$subj),drug=as.factor(Total$drug),seq=as.factor(Total$seq),
                         prd=as.factor(Total$prd),Cmax=Total$Cmax,AUC0t=Total$AUC0t,AUC0INF=Total$AUC0INF,
                         lnCmax=log(Total$Cmax),lnAUC0t=log(Total$AUC0t),lnAUC0INF=log(Total$AUC0INF))
  }
 }
}
### the following lines were for the individual plots on screen. <--- YJ
### Totalplot<-Totalplot[ do.call(order, Totalplot) ,]
### s.split<-split(Totalplot,list(Totalplot$subj))
### cat("\n\n Raw dataset:-\n\n");show(Totalplot);cat("\n\n")
### 
### #Plot Cp vs Time
### #creat 3(row)*2(column) multiple figure array
### #Plot LogCp vs Time
### #creat 3(row)*2(column) multiple figure array
### Totalplot<-Totalplot[ do.call(order, Totalplot) ,]
### s.split<-split(Totalplot,list(Totalplot$subj))
### if(parallel){
###  if(multiple){
###     paraR.split<-split(SingleRdata1, list(SingleRdata1$subj))
###     paraT.split<-split(SingleTdata1, list(SingleTdata1$subj))
###     
###     RR.split<-split(SingleRdata0, list(SingleRdata0$subj))
###     TT.split<-split(SingleTdata0, list(SingleTdata0$subj))
###    }
###    else{
###     Totalplot$conc[Totalplot$conc == 0] <- NA
###     Totalplot <- na.omit(Totalplot)
###     Totalplot.split<-split(Totalplot, list(Totalplot$subj))
###  
###     Totalplotpara<-split( Totalplot, list(Totalplot$drug))
###     paraR.split<-split( Totalplotpara[[1]], list(Totalplotpara[[1]]$subj))
###     paraT.split<-split( Totalplotpara[[2]], list(Totalplotpara[[2]]$subj))  
###      }
###  }
### else{
###  if(replicated){
###      prdcount<-length(levels(TotalData$prd))
###      LR<-data.frame(subj=Totalplot$subj,  seq=Totalplot$seq, prd=Totalplot$prd, drug=Totalplot$drug,
###                     time=Totalplot$time,  conc=Totalplot$conc, code=Totalplot$code)
###      LR$conc[LR$conc == 0] <- NA
###      LR <- na.omit(LR)
###      Ls.split<-split(LR, list(LR$subj))
###         }
###   else{
###     if(multiple){
###      LR<-data.frame(subj=SingleRdata0$subj, time=SingleRdata0$time,  conc=SingleRdata0$conc)
###      LR$conc[LR$conc == 0] <- NA
###      LR <- na.omit(LR)
###      LR.split<-split(LR, list(LR$subj))
###      
###      LT<-data.frame(subj=SingleTdata0$subj, time=SingleTdata0$time,  conc=SingleTdata0$conc)
###      LT$conc[LT$conc == 0] <- NA
###      LT <- na.omit(LT)
###      LT.split<-split(LT, list(LT$subj))
###          RR.split<-split(SingleRdata0, list(SingleRdata0$subj))
###          TT.split<-split(SingleTdata0, list(SingleTdata0$subj))
###     }
###       else{
###     LR<-data.frame(subj=SingleRdata$subj, time=SingleRdata$time,  conc=SingleRdata$conc)
###     LR$conc[LR$conc == 0] <- NA
###     LR <- na.omit(LR)
###     LR.split<-split(LR, list(LR$subj))
### 
###     LT<-data.frame(subj=SingleTdata$subj, time=SingleTdata$time,  conc=SingleTdata$conc)
###     LT$conc[LT$conc == 0] <- NA
###     LT <- na.omit(LT)
###     LT.split<-split(LT, list(LT$subj))
###       }
###     }
###  }
### windows(record = TRUE )   ### not req. any more. --- may consider to delete plotsingle(), etc. same for AIC, AIC-TTT, etc.
### if(replicated){
###     plotsingle.Rep(Ls.split, s.split,xaxis,yaxis,i, prdcount ) 
###    }
### else{ 
###  if(parallel){
###     if(multiple){
###        Multipleplotsingle.para(R.split,T.split,paraR.split,paraT.split,xaxis,yaxis,RR.split,TT.split,TlastD )
###        }
###       else{
###         plotsingle.para(R.split, T.split, paraR.split,paraT.split,xaxis,yaxis )
###       }
###   }
###   ### else{                 ## no more plot on screen now.  -YJ (v2.5.5.1)
###   ###    if(multiple){
###   ###      ### Multipleplotsingle(LR.split,LT.split,RR.split,TT.split,xaxis,yaxis,TlastD ) ### not req. any more; all log into .pdf
###   ###      }
###   ###     else{
###   ###      ### plotsingle(LR.split,LT.split,xaxis,yaxis,R.split,T.split,multiple=FALSE)  ### this will show on screen instantly, not log into .pdf. -YJ
###   ###     }
###   ### }
### }
###

##export with txt file
if(replicated){
      RepNCAoutput(sumindexR, sumindexT,R.split, T.split,keindex_ref,keindex_test,Dose,TotalData,rdata.split,tdata.split )
      RepNCAplot(Totalplot,SingleRdata,SingleTdata,TotalData,xaxis,yaxis)
    if (Demo){
        if(MIX){
         ##Demo=TRUE, MIX=TRUE
         RepMIXanalyze(TotalData)
        }
        else{
         #Demo=TRUE, MIX=FALSE
         Repmenu()
        }
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
   if(parallel){
      if(multiple){
        SingleRdata<-SingleRdata0
        SingleTdata<-SingleTdata0
        MultipleParaNCAoutput(sumindexR, sumindexT,R.split, T.split,keindex_ref,keindex_test,Dose,TotalData, rdata.split,tdata.split, Tau, TlastD)
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
      ParaNCAoutput(sumindexR, sumindexT,R.split, T.split,keindex_ref,keindex_test,Dose,TotalData,rdata.split ,tdata.split )
      ParaNCAplot(Totalplot,SingleRdata,SingleTdata,TotalData,xaxis,yaxis) 
      if (Demo){
        if(MIX){
         ##Demo=TRUE, MIX=TRUE
         ParaMIXanalyze(TotalData)
        }
        else{
         #Demo=TRUE, MIX=FALSE
         Paramenu()
        }
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
        MultipleNCAoutput(sumindexR, sumindexT,R.split, T.split,keindex_ref,keindex_test,Dose,TotalData,Tau,TlastD,rdata.split,tdata.split )
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
      NCAoutput(sumindexR, sumindexT,R.split, T.split,keindex_ref,keindex_test,Dose,TotalData, rdata.split,tdata.split)
      NCAplot(Totalplot,SingleRdata,SingleTdata,TotalData,xaxis,yaxis,TlastD)
       
       if (Demo){
        if(BANOVA){
        ##Demo=TRUE, BANOVA=TRUE
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
