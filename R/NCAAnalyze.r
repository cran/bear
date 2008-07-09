#Input assay data Menu for Data Analysis for Single dose
library(reshape)
library(nlme)
options(warn=-1)

NCAanalyze<-function(TotalSingledata, Dose, xaxis,yaxis, separateWindows=TRUE)
{
cat("****************************************************************************\n")
cat("*                      Noncompartmental Analysis (NCA)                     *\n")
cat("*--------------------------------------------------------------------------*\n")
cat("* We provide noncompartmental analysis (NCA) approach to compute AUCs and  *\n")
cat("* terminal elimination rate constant (kel) for plasma concentration.  Here *\n")
cat("* we provide extravascular model.  Linear trapezoidal rule is used to      *\n")
cat("* calculate AUC.(time = 0 to infinity)                                     *\n")
cat("****************************************************************************\n")
cat("\n\n")
cat("Enter Dose\n")
Dose<- scan(nlines=1,quiet=TRUE)
cat("\n\n")
cat("\nEnter the title of x-axis(Time)\n")
cat("(or a blank line to use default)\n\n") 
xaxis<-readline()
if (substr(xaxis, 1, 1) == "")  xaxis<-"Time"  else xaxis<-xaxis
cat("\nEnter the title of y-axis(Conc.)\n")
cat("(or a blank line to use default)\n\n") 
yaxis<-readline()
 #cat("\n\n Please Wait.  Data is Processing. \n")
if (substr(yaxis, 1, 1) == "")  yaxis<-"Conc."  else yaxis<-yaxis
#cat("\n\n Please Wait.  Data is Processing. \n")

cat("****************************************************************************\n")
cat("*       drug 1:Reference                                                   *\n")
cat("*       drug 2:Test                                                        *\n")
cat("****************************************************************************\n")   

Singledata<-split(TotalSingledata, list(TotalSingledata$seq, TotalSingledata$prd))
Ref<-rbind(Singledata[[1]],Singledata[[4]])
Refdata<-data.frame(subj=Ref$subj, seq= Ref$seq, prd=Ref$prd, drug=c(1), time=Ref$time, conc=Ref$conc)
SingleRdata<-Refdata[ do.call(order, Refdata) ,]
show(SingleRdata)
cat("\n\n")
Test<-rbind(Singledata[[2]],Singledata[[3]])
Testdata<-data.frame(subj=Test$subj, seq= Test$seq, prd=Test$prd, drug=c(2), time=Test$time, conc=Test$conc)
SingleTdata<-Testdata[ do.call(order, Testdata) ,]
show(SingleTdata)

#'Total" for NCAplot
Totalplot<- rbind(SingleRdata,SingleTdata)


########Reference data
cat("\n\n")
cat("****************************************************************************\n")
cat("* Referenece Data:                                                         *\n")          
cat("*--------------------------------------------------------------------------*\n")
cat("* calculate AUC(0~t), AUC(0~inf), AUMC(0~t), AUMC(0~inf),                  *\n")
cat("*            lamda, Cl/F, Vd, MRT, half life (T1/2)                        *\n")
cat("* Calculation Method: linear trapezoidal                                   *\n")
cat("*                                                                          *\n")
cat("****************************************************************************\n")
cat("\n\n")
#split dataframe into sub-dataframe by subject for reference data
   R.split<-split(SingleRdata, list(SingleRdata$subj))
  
subj<-0 
     for (j in 1:(length(R.split))){
     subj[j]<-R.split[[j]][["subj"]][1]
     }
     

get(getOption("device"))()
par(mfrow=c(2,2))
#calculate kel for reference data 
co_data1<-NULL
for(i in seq_along(R.split)){
  #  get(getOption("device"))()

 xx1<-R.split[[i]]$time
 yy1<-R.split[[i]]$conc
 main<-paste(c("Please, select 3 points. Subject# ref_", R.split[[i]]$subj[1]),collapse=" ")
 plot(0,0, xlim=range(xx1), ylim=range(yy1),xlab=xaxis, ylab=yaxis, main=main,
      cex.lab = 1.5,pch=19,lab=c(20,20,30))
 points(xx1,yy1,pch=19,col=i,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
 lines(xx1,yy1, lty=20,col=i)
 
 co_data1[[i]]<-identify(R.split[[i]]$time,  R.split[[i]]$conc, n=3)
}

 r_melt<-melt(co_data1) 
 Y.split<-split(r_melt,list(r_melt$L1))
    
xy1<-NULL
s1<-NULL
d1<-NULL
d2<-NULL
 for(j in seq_along(Y.split)){
     tx<-NULL
    for(i in 1:length(Y.split[[j]][["value"]])){
       tx[[i]]<-Y.split[[j]][["value"]][i]
      }
           xy1[[j]]<- R.split[[j]][tx, , ]
           s1[[j]]<-c(xy1[[j]]$subj)
           d1[[j]]<-c(xy1[[j]]$time)  
           d2[[j]]<-c(xy1[[j]]$conc) 
           }  
y0<-melt(s1) 
y1<-melt(d1)
y2<-melt(d2)  

#fitting data with linear regression model
#cat("<<Output: linear regression model: time.ref vs. conc.ref>>\n")
ref_data<-data.frame(subj=y0$L1,time.ref=y1$value,conc.ref=log10(y2$value))
Lm1 <- lmList(conc.ref ~ time.ref |subj, data = ref_data)

#"time.ref" means "kel"
keindex_ref<-data.frame(subj=subj, time.ref=-2.3*(coef(Lm1)[2]), R_square=summary(Lm1)$r.sq)    

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
                 colnames(output)<-list("subj","time","conc", "AUC(0~t)","AUMC(0~t)")
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

  
######Test data
cat("****************************************************************************\n")
cat("* Test Data:                                                               *\n")          
cat("*--------------------------------------------------------------------------*\n")
cat("* calculate AUC(0~t), AUC(0~inf), AUMC(0~t), AUMC(0~inf),                  *\n")
cat("*            lamda, Cl/F, Vd, MRT, half life (T1/2)                        *\n")
cat("* Calculation Method: linear trapezoidal                                   *\n")
cat("*                                                                          *\n")
cat("****************************************************************************\n")
cat("\n\n")


#split dataframe into sub-dataframe by subject for test data
   T.split<-split(SingleTdata, list(SingleTdata$subj))
  
subj1<-0 
     for (j in 1:(length(T.split))){
     subj1[j]<-T.split[[j]][["subj"]][1]
     }
#calculate kel for test data 
co_data2<-NULL
get(getOption("device"))()
par(mfrow=c(2,2))
for(i in seq_along(T.split)){
   
 xx2<-T.split[[i]]$time
 yy2<-T.split[[i]]$conc
 main<-paste(c("Please, select 3 points. Subject# test_", T.split[[i]]$subj[1]),collapse=" ")
  plot(0,0, xlim=range(xx2), ylim=range(yy2),xlab=xaxis, ylab=yaxis, main=main ,
      cex.lab = 1.5,pch=1,lab=c(20,20,30))
  points(xx2,yy2,pch=1,col=i,bty="l", font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
  lines(xx2,yy2,lwd=1,col=i)

 co_data2[[i]]<-identify(T.split[[i]]$time,  T.split[[i]]$conc, n=3)
}

 t_melt<-melt(co_data2) 
 YY.split<-split(t_melt,list(t_melt$L1))

xy2<-NULL
ss1<-NULL
dd1<-NULL
dd2<-NULL
 for(j in seq_along(YY.split)){
     tx1<-NULL
    for(i in 1:length(YY.split[[j]][["value"]])){
       tx1[[i]]<-YY.split[[j]][["value"]][i]
      }           
                  
           xy2[[j]]<- T.split[[j]][tx1, , ]
           ss1[[j]]<-c(xy2[[j]]$subj)
           dd1[[j]]<-c(xy2[[j]]$time)  
           dd2[[j]]<-c(xy2[[j]]$conc) 
           }  

yy0<-melt(ss1) 
yy1<-melt(dd1)
yy2<-melt(dd2)  

#fitting data with linear regression model
test_data<-data.frame(subj=yy0$L1,time.test=yy1$value,conc.test=log10(yy2$value))
Lm2 <- lmList(conc.test ~ time.test |subj, data = test_data)
Lm2

#"time.test" means "kel"
keindex_test<-data.frame(subj=subj1, time.test=-2.3*(coef(Lm2)[2]), R_square=summary(Lm2)$r.sq)    
keindex_test

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
                 colnames(output)<-list("subj","time","conc", "AUC(0~t)","AUMC(0~t)")
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
cat("--------------------------------------------------------------------------\n") 
cat("<<Output: linear regression model: time.ref vs. conc.ref>>\n")
cat("\n")
ref_data<-data.frame(subj=y0$L1,time.ref=y1$value,conc.ref=log10(y2$value))
Lm1 <- lmList(conc.ref ~ time.ref |subj, data = ref_data)
print(Lm1)
cat("\n")
cat("--------------------------------------------------------------------------\n") 
cat("<<Output: linear regression model: time.test vs. conc.test>>\n")
cat("\n")
test_data<-data.frame(subj=yy0$L1,time.test=yy1$value,conc.test=log10(yy2$value))
Lm2 <- lmList(conc.test ~ time.test |subj, data = test_data)
print(Lm2)
cat("--------------------------------------------------------------------------\n") 
 
cat("\n")
cat("****************************************************************************\n")
cat("*<<Plots >>                                                                *\n")
cat("* Plasma concentration (Ref and Test) vs. Time                             *\n")
cat("* Log Transformation_Plasma concentration (Ref and Test) vs. Time          *\n")
cat("****************************************************************************\n")
cat("\n")

#Plot Cp vs Time
 #creat 3(row)*2(column) multiple figure array
 #Plot LogCp vs Time
   #creat 3(row)*2(column) multiple figure array
LR<-data.frame(subj=SingleRdata$subj, time=SingleRdata$time,  conc=SingleRdata$conc)
LR$conc[LR$conc == 0] <- NA
LR <- na.omit(LR)
LR.split<-split(LR, list(LR$subj))

LT<-data.frame(subj=SingleTdata$subj, time=SingleTdata$time,  conc=SingleTdata$conc)
LT$conc[LT$conc == 0] <- NA
LT <- na.omit(LT)	
LT.split<-split(LT, list(LT$subj))

 for(i in seq_along(LT.split)){
  get(getOption("device"))()   
     xx1<-LR.split[[i]]$time
     yy1<-LR.split[[i]]$conc
  
     xx2<-LT.split[[i]]$time
     yy2<-LT.split[[i]]$conc

        main<-paste(c("Subject #", LT.split[[i]]$subj[1]),collapse=" ")
        plot(0, 0, log="y",xlim=range(xx1), ylim=range(yy1,yy2), ylab="Conc.(Log10 Scale)", xlab="Time", main=main, cex.lab = 1.5,pch=19,lab=c(20,20,30))
 
        points(xx1,yy1,pch=19,col=i,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
        points(xx2,yy2,pch=1,col=i,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
 
        lines(xx1,yy1, lty=20,col=i)
        lines(xx2,yy2, lwd=1,col=i)
        temp <- legend("topright", legend = c("Test", "Reference"),
               text.width = strwidth("1,000,000"),
               lty = 1:2, xjust = 1, yjust = 1)
  }
  
for(i in seq_along(T.split)){
     get(getOption("device"))()
  
       xx1<-R.split[[i]]$time
       yy1<-R.split[[i]]$conc
  
       xx2<-T.split[[i]]$time
       yy2<-T.split[[i]]$conc

         main<-paste(c("Subject #", T.split[[i]]$subj[1]),collapse=" ")
         plot(0, 0, xlim=range(xx1), ylim=range(yy1,yy2), xlab=xaxis, ylab=yaxis,,  main=main, cex.lab = 1.5,pch=1,lab=c(20,20,30))
 
         points(xx1,yy1,pch=19,col=i,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
         points(xx2,yy2,pch=1,col=i,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
 
         lines(xx1,yy1, lty=20,col=i)
         lines(xx2,yy2, lwd=1,col=i)
         temp <- legend("topright", legend = c("Test", "Reference"),
               text.width = strwidth("1,000,000"),
               lty = 1:2, xjust = 1, yjust = 1) 
  
  }

  
cat("****************************************************************************\n")
cat("*<<Summary Result >>                                                       *\n")
cat("* AUC(0~t), AUC(0~inf), Cmax for Reference and Test data                   *\n")
cat("*--------------------------------------------------------------------------*\n")
cat("* TestAUC0t: area under the predicted plasma concentration time curve       *\n")
cat("*           for test data. (time = 0 to t)                                 *\n")
cat("* TestAUC0inf: area under the predicted plasma concentration time curve     *\n")
cat("*            for test data. (time = 0 to infinity)                         *\n")
cat("* RefAUC0t: area under the predicted plasma concentration time curve        *\n")
cat("*           for reference data. (time = 0 to t)                            *\n")
cat("* RefAUC0inf: area under the predicted plasma concentration time curve      *\n")
cat("*            for reference data. (time = 0 to infinity)                    *\n")
cat("****************************************************************************\n")
cat("\n")
 
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

cat("\n\n")
cat("****************************************************************************\n")
cat("* Coded Data:                                                              *\n")          
cat("*--------------------------------------------------------------------------*\n")
cat("* Drug:                                                                    *\n")
cat("*     1:Reference                                                          *\n")
cat("*     2:Test                                                               *\n")                                       
cat("* Sequence:                                                                *\n") 
cat("*     1:Reference-->Test sequence                                          *\n")
cat("*     2:Test-->Reference sequence                                          *\n")
cat("* Period:                                                                  *\n") 
cat("*     1:first treatment period                                             *\n")
cat("*     2:second treatmetn period                                            *\n")                                    
cat("****************************************************************************\n")
cat("\n\n")
sumindexR<-data.frame(subj=subj,drug=c(1),seq=seqR,prd=prdR,Cmax=CmaxRef,AUC0t=AUCTRef,AUC0INF=AUCINFRef,
                      Tmax=TmaxRef, MRTINF=MRTINFRef, T12=T12Ref, VdF=VdFRef, Kel=KelRef, ClF=ClFRef)
sumindexT<-data.frame(subj=subj,drug=c(2),seq=seqT,prd=prdT,Cmax=CmaxTest,AUC0t=AUCTTest,AUC0INF=AUCINFTest,
                     Tmax=TmaxTest,MRTINF=MRTINFTest, T12=T12Test, VdF=VdFTest, Kel=KelTest, ClF=ClFTest)

#########
Total<-rbind(sumindexR,sumindexT)
TotalData<-data.frame (subj=as.factor(Total$subj), drug=as.factor(Total$drug),seq=as.factor(Total$seq),
                   prd=as.factor(Total$prd),Cmax=Total$Cmax, AUC0t=Total$AUC0t, AUC0INF=Total$AUC0INF, 
                   LnCmax=log(Total$Cmax),LnAUC0t=log(Total$AUC0t),LnAUC0INF=log(Total$AUC0INF)) 
show(TotalData)

##export with txt file
NCAoutput(sumindexR, sumindexT,R.split, T.split,keindex_ref,keindex_test,Dose,TotalData )

NCAplot(Totalplot,SingleRdata,SingleTdata,TotalData,xaxis,yaxis)

cat("\n\n")   
cat("\nSave data (y/n) ?\n")
            ans<-readline()
            cat("\n")
              if (ans == "n" | ans == "N"){
               return(NCAdata())
                      }
              else {
               cat("Enter name you want to call this data\n")
               Totalname <-readline()
               Totalname<-paste(Totalname,".RData",sep="")
                 if(file.exists(Totalname)){
                   cat("\n")
                   cat("*****************************************\n")
                   cat("* The file name have been existed.      *\n")
                   cat("* Would you want to overwrite it ? (y/n)*\n")
                   cat("*****************************************\n")
                   ans<-readline()
                      if (ans == "y" | ans == "Y"){
                      save(TotalData,file=Totalname)
                      cat("\n")
                              }
                      else{
                      cat("\nEnter name you want to call this data\n")
                      Totalname <-readline()
                      Totalname<-paste(Totalname,".RData",sep="")
                        repeat{
                        if(file.exists(Totalname)){
                        cat("\n")
                        cat("***********************************\n")
                        cat("* The file name have been existed *\n")
                        cat("* Enter name again, OK.           *\n")
                        cat("***********************************\n")
                      Totalname<-readline()
                      Totalname<-paste(Totalname,".RData",sep="")
                         }
                        else{
                         break
                         }
                        }
                      }
              save(TotalData,file=Totalname)
                }
                else{
                 save(TotalData,file=Totalname)
                  }
            }
cat("\n\n")
cat("****************************************************************************\n")
cat("*         Now, Go to Generalized Linear Models (GLM)                       *\n")
cat("****************************************************************************\n\n") 
 GLMmenu(TotalData)              
}          
  



