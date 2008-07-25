#Demo for NCAGLM
library(reshape)
library(nlme)
options(warn=-1)
demoNCAGLM<-function()
{

##input or import data (NCAdata)
cat("****************************************************************************\n")
cat("*Input/Edit Data                                                           *\n")
cat("*--------------------------------------------------------------------------*\n")
cat("*   ->subject no.(subj)                                                    *\n")
cat("*   ->sequence (seq)                                                       *\n")
cat("*       Sequence 1:Reference-->Test sequence                               *\n")
cat("*       Sequence 2:Test-->Reference sequence                               *\n")
cat("*   ->period (prd)                                                         *\n")
cat("*       Period 1: first treatmetn period                                   *\n")
cat("*       Period 2: second treatmetn period                                  *\n")
cat("*   ->time                                                                 *\n")
cat("*   ->concentration (conc)                                                 *\n")
cat("****************************************************************************\n")
cat("\n")
TotalSingledata<-data.frame(subj=c(1,1,1,1,1,1,1,1,1,1,1,1,
                                   1,1,1,1,1,1,1,1,1,1,1,1,
                                   2,2,2,2,2,2,2,2,2,2,2,
                                   2,2,2,2,2,2,2,2,2,2,2,2,
                                   3,3,3,3,3,3,3,3,3,3,3,
                                   3,3,3,3,3,3,3,3,3,3,3,3,
                                   4,4,4,4,4,4,4,4,4,4,4,
                                   4,4,4,4,4,4,4,4,4,4,4,4,
                                   5,5,5,5,5,5,5,5,5,5,5,5,
                                   5,5,5,5,5,5,5,5,5,5,5,5,
                                   6,6,6,6,6,6,6,6,6,6,6,
                                   6,6,6,6,6,6,6,6,6,6,6),
                             seq=c(2,2,2,2,2,2,2,2,2,2,2,2,
                                   2,2,2,2,2,2,2,2,2,2,2,2,
                                   1,1,1,1,1,1,1,1,1,1,1,
                                   1,1,1,1,1,1,1,1,1,1,1,1,
                                   2,2,2,2,2,2,2,2,2,2,2,
                                   2,2,2,2,2,2,2,2,2,2,2,2,
                                   1,1,1,1,1,1,1,1,1,1,1,
                                   1,1,1,1,1,1,1,1,1,1,1,1,
                                   2,2,2,2,2,2,2,2,2,2,2,2,
                                   2,2,2,2,2,2,2,2,2,2,2,2,
                                   1,1,1,1,1,1,1,1,1,1,1,
                                   1,1,1,1,1,1,1,1,1,1,1),
                             prd=c(2,2,2,2,2,2,2,2,2,2,2,2,
                                   1,1,1,1,1,1,1,1,1,1,1,1,
                                   1,1,1,1,1,1,1,1,1,1,1,
                                   2,2,2,2,2,2,2,2,2,2,2,2,
                                   2,2,2,2,2,2,2,2,2,2,2,
                                   1,1,1,1,1,1,1,1,1,1,1,1,
                                   1,1,1,1,1,1,1,1,1,1,1,
                                   2,2,2,2,2,2,2,2,2,2,2,2,
                                   2,2,2,2,2,2,2,2,2,2,2,2,
                                   1,1,1,1,1,1,1,1,1,1,1,1,
                                   1,1,1,1,1,1,1,1,1,1,1,
                                   2,2,2,2,2,2,2,2,2,2,2),
                             time=c(0,0.25,0.5,0.75,1,1.5,2,3,4,8,12,24,
                                    0,0.25,0.5,0.75,1,1.5,2,3,4,8,12,24,
                                    0,0.5,0.75,1,1.5,2,3,4,8,12,24,
                                    0,0.25,0.5,0.75,1,1.5,2,3,4,8,12,24,
                                    0,0.5,0.75,1,1.5,2,3,4,8,12,24,
                                    0,0.25,0.5,0.75,1,1.5,2,3,4,8,12,24,
                                    0,0.5,0.75,1,1.5,2,3,4,8,12,24,
                                    0,0.25,0.5,0.75,1,1.5,2,3,4,8,12,24,
                                    0,0.25,0.5,0.75,1,1.5,2,3,4,8,12,24,
                                    0,0.25,0.5,0.75,1,1.5,2,3,4,8,12,24,
                                    0,0.5,0.75,1,1.5,2,3,4,8,12,24,
                                    0,0.5,0.75,1,1.5,2,3,4,8,12,24),
                             conc=c(0,36.1,125,567,932,1343,1739,1604,1460,797,383,72,
                                    0,84.5,192,629,873,1246,1633,1375,1006,616,379,84.4,
                                    0,69.7,167,602,1023,1388,1481,1346,658,336,84,
                                    0,30.1,211,1221,1485,1837,1615,1621,1411,763,424,109,
                                    0,38.2,277,631,1002,1780,1776,1618,782,466,89.7,
                                    0,32.8,181,271,402,783,2073,1842,1610,883,389,75.8,
                                    0,37.2,306,758,1124,1374,1129,1043,576,325,75.9,
                                    0,30.8,198,395,906,1413,1629,1501,1383,713,403,87.2,
                                    0,37.9,171,795,1167,1403,1541,1555,1292,716,412,80.1,
                                    0,25.8,151,523,1031,1294,1385,1291,1143,571,334,83.3,
                                    0,213,799,987,1301,1756,1665,1529,772,461,82.6,
                                    0,140,643,909,1073,1252,1522,1375,795,403,74.1))
show( TotalSingledata)
cat("\n\n")

##NCAanalyze or NCAGLManalyze
cat("Enter Dose\n")
cat("80000\n")
Dose <- 80000
cat("\n")

cat("\nEnter the title of x-axis(time)\n")
cat("(or a blank line to use default)\n\n")
cat("Time(hr)\n")
xaxis<-"Time(hr)"
cat("\n")

cat("\nEnter the title of y-axis(Cp)\n")
cat("(or a blank line to use default)\n\n")
cat("Conc.(ng/mL)\n")
yaxis<-"Conc.(ng/mL)"
cat("\n")

cat("****************************************************************************\n")
cat("*     drug# 1: Reference                                                   *\n")
cat("*     drug# 2: Test                                                        *\n")
cat("****************************************************************************\n")   

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
########Reference data
cat("\n\n")
cat("****************************************************************************\n")
cat("* Data for the Ref. Products:                                              *\n")
cat("*--------------------------------------------------------------------------*\n")
cat("* To calculate AUC(0-t), AUC(0-inf), AUMC(0-t), AUMC(0-inf),               *\n")
cat("*            lambda, Cl/F, Vd, MRT, and half-life (T1/2)                   *\n")
cat("* AUC(0-t) was calculated with the linear trapezoidal.                     *\n")
cat("*                                                                          *\n")
cat("****************************************************************************\n")
cat("\n\n")
#split dataframe into sub-dataframe by subject for reference data
   R.split<-split(SingleRdata1, list(SingleRdata1$subj))

subj<-0
     for (j in 1:(length(R.split))){
     subj[j]<-R.split[[j]][["subj"]][1]
     }


windows(record = TRUE )
par(mfrow=c(2,2))
#calculate kel for reference data 
co_data1<-NULL
for(i in seq_along(R.split)){
  #  get(getOption("device"))()

 xx1<-R.split[[i]]$time
 yy1<-R.split[[i]]$conc
 main<-paste(c("Please, select 3 points. Subject# ref_", R.split[[i]]$subj[1]),collapse=" ")
 plot(xx1,yy1,log="y", xlim=range(xx1), ylim=range(yy1),xlab="Time", ylab= "Conc.(Log10 Scale)", main=main,
      cex.lab = 1.5,pch=19,lab=c(20,20,30), xaxt="n")
 lines(xx1,yy1, lty=20)
   axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
   axis(1,at=0:100,tcl=-.2, labels=FALSE)
   
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
                 cat("<< NCA Summary Table--Subject# ",su," (Ref.)>>\n")
                 cat("--------------------------------------------------------------------------\n")
                 output<-data.frame(R.split[[j]][["subj"]],R.split[[j]][["time"]],R.split[[j]][["conc"]],auc_ref,aumc_ref )
                 colnames(output)<-list("subj","time","conc", "AUC(0-t)","AUMC(0-t)")
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
              cat("AUC(0-t)=",auc_ref[length(R.split[[j]][["conc"]])],"\n")
              cat("AUC(0-inf) =" ,aucINF,"\n")
              cat("AUMC(0-t)=",aumc_ref[length(R.split[[j]][["conc"]])],"\n")
              cat("AUMC(0-inf) =" ,aumcINF,"\n")
              cat("MRT(0-t)=",(aumc_ref[length(R.split[[j]][["conc"]])])/(auc_ref[length(R.split[[j]][["conc"]])]),"\n")
              cat("MRT(0-inf)=",aumcINF/aucINF,"\n")
              cat("--------------------------------------------------------------------------\n")
              cat("\n\n")
  }


######Test data
cat("****************************************************************************\n")
cat("* Data for the Test Products:                                              *\n")          
cat("*--------------------------------------------------------------------------*\n")
cat("* To calculate AUC(0-t), AUC(0-inf), AUMC(0-t), AUMC(0-inf),               *\n")
cat("*            lambda, Cl/F, Vd, MRT, and half-life (T1/2)                   *\n")
cat("* AUC(0-t) was calculated with the linear trapezoidal.                     *\n")
cat("*                                                                          *\n")
cat("****************************************************************************\n")
cat("\n\n")


#split dataframe into sub-dataframe by subject for test data
   T.split<-split(SingleTdata1, list(SingleTdata1$subj))

subj1<-0
     for (j in 1:(length(T.split))){
     subj1[j]<-T.split[[j]][["subj"]][1]
     }
#calculate kel for test data
co_data2<-NULL

par(mfrow=c(2,2))
for(i in seq_along(T.split)){
   
 xx2<-T.split[[i]]$time
 yy2<-T.split[[i]]$conc
 main<-paste(c("Please, select 3 points. Subject# test_", T.split[[i]]$subj[1]),collapse=" ")
  plot(xx2,yy2, log="y",xlim=range(xx2), ylim=range(yy2),xlab="Time", ylab= "Conc.(Log10 Scale)", main=main ,
      cex.lab = 1.5,pch=1,lab=c(20,20,30), xaxt="n")
  lines(xx2,yy2,lwd=1)
   axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
   axis(1,at=0:100,tcl=-.2, labels=FALSE) 
   
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
dev.off()
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
                 cat("<< NCA Summary Table--Subject# ",su1," (Test) >>\n")
                 cat("--------------------------------------------------------------------------\n")
                 output<-data.frame(T.split[[j]][["subj"]],T.split[[j]][["time"]],T.split[[j]][["conc"]],auc_test,aumc_test )
                 colnames(output)<-list("subj","time","conc", "AUC(0-t)","AUMC(0-t)")
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
              cat("AUC(0-t)=",auc_test[length(T.split[[j]][["conc"]])],"\n")
              cat("AUC(0-inf) =" ,aucINF,"\n")
              cat("AUMC(0-t)=",aumc_test[length(T.split[[j]][["conc"]])],"\n")
              cat("AUMC(0-inf) =" ,aumcINF,"\n")
              cat("MRT(0-t)=",(aumc_test[length(T.split[[j]][["conc"]])])/(auc_test[length(T.split[[j]][["conc"]])]),"\n")
              cat("MRT(0-inf)=",aumcINF/aucINF,"\n")
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
cat("*<<Conc.-Time Plots >>                                                     *\n")
cat("* Plasma concentration (Ref. and Test) vs. Time                            *\n")
cat("* Log transformaed drug plasma concentration (Ref. and Test) vs. Time      *\n")
cat("*--------------------------------------------------------------------------*\n")
cat("* Please use PageUp/PageDown to scroll up and down these plots.            *\n")
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

windows(record = TRUE )
 for(i in seq_along(LT.split)){
     xx1<-LR.split[[i]]$time
     yy1<-LR.split[[i]]$conc
  
     xx2<-LT.split[[i]]$time
     yy2<-LT.split[[i]]$conc

        main<-paste(c("Subject #", LT.split[[i]]$subj[1]),collapse=" ")
        plot(0, 0, log="y",xlim=range(xx1), ylim=range(yy1,yy2), ylab="Conc.(Log10 Scale)", xlab="Time",
         main=main, cex.lab = 1.5, font.lab=2,cex.axis=1,cex.main=1,las=1,pch=19,xaxt="n")
 
        points(xx1,yy1,pch=19,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
        points(xx2,yy2,pch=1,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
 
        lines(xx1,yy1, lty=20)
        lines(xx2,yy2, lwd=1)
        
        axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
        axis(1,at=0:100,tcl=-.2, labels=FALSE)
        temp <- legend("topright", legend = c("Test", "Reference"),
               text.width = strwidth("1,000,000"),
               lty = 1:2, xjust = 1, yjust = 1)
  }


for(i in seq_along(T.split)){
       xx1<-R.split[[i]]$time
       yy1<-R.split[[i]]$conc
  
       xx2<-T.split[[i]]$time
       yy2<-T.split[[i]]$conc

         main<-paste(c("Subject #", T.split[[i]]$subj[1]),collapse=" ")
         plot(0, 0, xlim=range(xx1), ylim=range(yy1,yy2), xlab="Time", ylab= "Conc.",
         main=main, cex.lab = 1.5, font.lab=2,cex.axis=1,cex.main=1,las=1,pch=1,xaxt="n")
 
         points(xx1,yy1,pch=19,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
         points(xx2,yy2,pch=1,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
 
         lines(xx1,yy1, lty=20)
         lines(xx2,yy2, lwd=1)
         
         axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
         axis(1,at=0:100,tcl=-.2, labels=FALSE)
         axis(2,yaxp=c(0, 4000, 40),las=1,tcl=-.2, labels=FALSE)
         temp <- legend("topright", legend = c("Test", "Reference"),
               text.width = strwidth("1,000,000"),
               lty = 1:2, xjust = 1, yjust = 1) 
  
  }
  
cat("****************************************************************************\n")
cat("*<<Summary Result >>                                                       *\n")
cat("* AUC(0-t), AUC(0-inf), Cmax for the Reference and Test product            *\n")
cat("*--------------------------------------------------------------------------*\n")
cat("* TestAUC0t: area under the plasma concentration time curve for Test       *\n")
cat("*            product. (time = 0 to t)                                      *\n")
cat("* TestAUC0inf: area under the plasma concentration time curve for Test     *\n")
cat("*              product. (time = 0 to infinity)                             *\n")
cat("* RefAUC0t: area under the plasma concentration time curve for Reference   *\n")
cat("*           product. (time = 0 to t)                                       *\n")
cat("* RefAUC0inf: area under the predicted plasma concentration time curve     *\n")
cat("*             for Reference product. (time = 0 to infinity)                *\n")
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

cat("****************************************************************************\n")
cat("* Data Codes:                                                               *\n")          
cat("*--------------------------------------------------------------------------*\n")
cat("* Drug:                                                                    *\n")
cat("*     1: Reference                                                         *\n")
cat("*     2: Test                                                              *\n")                                       
cat("* Sequence:                                                                *\n") 
cat("*     1: Reference --> Test                                                *\n")
cat("*     2: Test --> Reference                                                *\n")
cat("* Period:                                                                  *\n") 
cat("*     1: lst treatment period                                              *\n")
cat("*     2: 2nd treatment period                                              *\n")                                    
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
#############################################################################################
#Generalized Linear Models (GLM)
cat("****************************************************************************\n")
cat("*                                  ANOVA (lm)                              *\n")
cat("*--------------------------------------------------------------------------*\n")
cat("* With a two-treatment, two-period, two-sequence randomized crossover      *\n")
cat("* design, ANOVA statistical model includes factors of the following sources*\n")
cat("* :sequence, subjects nested in sequences, period and treatment. ANOVA will*\n")
cat("* be applied to obtain estimates for the adjusted differences between      *\n")
cat("* treatment means and the standard error associated with these differences.*\n")
cat("* Log-transformed BA measures will also be analyzed.                       *\n")
cat("****************************************************************************\n")
Fdata<-split(TotalData, list(TotalData$drug))
RefData<-Fdata[[1]]
TestData<-Fdata[[2]]

#represent GLM
cat("****************************************************************************\n")
cat("*                           ANOVA (lm) Procedure                           *\n") 
cat("*--------------------------------------------------------------------------*\n")
cat("  Dependent Variable: Cmax                                                  \n")
cat("\n")
Cmax<- lm(Cmax ~ seq + subj + prd + drug , data=TotalData)
summary(Cmax)
show(anova(Cmax))
cat("\n")
cat(" Tests of Hypothesis for SUBJECT(SEQUENCE) as an error term\n")
cat("\n")
Cmax1<- lm(Cmax ~ seq , data=TotalData)
show(anova(Cmax1))
cat("****************************************************************************\n")

cat("\n")
cat("\n")
cat("****************************************************************************\n")
cat("*                           ANOVA (lm) Procedure                           *\n") 
cat("*--------------------------------------------------------------------------*\n")
cat("  Dependent Variable: AUC0t                                                \n")
cat("\n")
AUC0t<- lm(AUC0t ~ seq + subj+ prd + drug , data=TotalData)
summary(AUC0t)
show(anova(AUC0t))
cat("\n")
cat(" Tests of Hypothesis for SUBJECT(SEQUENCE) as an error term\n")
cat("\n")
AUC0t1<- lm(AUC0t ~ seq , data=TotalData)
show(anova(AUC0t1))
cat("****************************************************************************\n")

cat("\n")
cat("\n")
cat("****************************************************************************\n")
cat("*                           ANOVA (lm) Procedure                           *\n") 
cat("*--------------------------------------------------------------------------*\n")
cat("  Dependent Variable: AUC0inf                                               \n")
cat("\n")
AUC0INF<- lm(AUC0INF ~ seq + subj+ prd + drug , data=TotalData)
summary(AUC0INF)
show(anova(AUC0INF))
cat("\n")
cat(" Tests of Hypothesis for SUBJECT(SEQUENCE) as an error term\n")
cat("\n")
AUC0INF1<- lm(AUC0INF ~ seq , data=TotalData)
show(anova(AUC0INF1))
cat("****************************************************************************\n")

cat("\n")
cat("\n")
cat("****************************************************************************\n")
cat("*                           ANOVA (lm) Procedure                           *\n") 
cat("*--------------------------------------------------------------------------*\n")
cat("  Dependent Variable: LnCmax                                               \n")
cat("\n")
LnCmax<- lm(LnCmax ~ seq + subj + prd + drug , data=TotalData)
summary(LnCmax)
show(anova(LnCmax))
cat("\n")
cat(" Tests of Hypothesis for SUBJECT(SEQUENCE) as an error term\n")
cat("\n")
LnCmax1<- lm(LnCmax ~ seq , data=TotalData)
show(anova(LnCmax1))
cat("****************************************************************************\n")

cat("\n")
cat("\n")
cat("****************************************************************************\n")
cat("*                           ANOVA (lm) Procedure                           *\n") 
cat("*--------------------------------------------------------------------------*\n")
cat("  Dependent Variable: LnAUC0t                                               \n")
cat("\n")
LnAUC0t<- lm(LnAUC0t ~ seq + subj + prd + drug , data=TotalData)
summary(LnAUC0t)
show(anova(LnAUC0t))
cat("\n")
cat(" Tests of Hypothesis for SUBJECT(SEQUENCE) as an error term\n")
cat("\n")
LnAUC0t1<- lm(LnAUC0t ~ seq , data=TotalData)
show(anova(LnAUC0t1))
cat("****************************************************************************\n")

cat("\n")
cat("\n")
cat("****************************************************************************\n")
cat("*                           ANOVA (lm) Procedure                           *\n") 
cat("*--------------------------------------------------------------------------*\n")
cat("  Dependent Variable: LnAUC0inf                                             \n")
cat("\n")
LnAUC0INF<- lm(LnAUC0INF ~ seq + subj + prd + drug , data=TotalData)
summary(LnAUC0INF)
show(anova(LnAUC0INF))
cat("\n")
cat(" Tests of Hypothesis for SUBJECT(SEQUENCE) as an error term\n")
cat("\n")
LnAUC0INF1<- lm(LnAUC0INF ~ seq , data=TotalData)
show(anova(LnAUC0INF1))
cat("****************************************************************************\n")
########REPORT
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

cat("\n")
cat("\n")
cat("****************************************************************************\n")
cat("*                            BE Summary Report                             *\n") 
cat("*--------------------------------------------------------------------------*\n")
cat("  Dependent Variable: LnCmax                                               \n")
cat("*--------------------------------------------------------------------------*\n")
cat("n1(R=>T)=", L1 , "\n")
cat("n2(T=>R)=", L2 , "\n")
cat("N(n1+n2)=", L1+L2 , "\n")
cat("LSM-ref=", ref_Cmax, "\n")
cat("LSM-test=",test_Cmax, "\n")
cat("MSE=",anova(LnCmax)[5,3], "\n")
cat("SE=",SE_Cmax, "\n")
cat("Diff. (test-ref)=", test_Cmax-ref_Cmax, "\n")
cat("t(0.95,N-2)=",T , "\n")
cat("Z(beta)=",Z_Cmax, "\n")
cat("Power(1-beta)_Z=", pnorm(abs(Z_Cmax)),"\n")
cat("t(beta)=",T_Cmax, "\n")
cat("Power(1-beta)_t=", pt(T_Cmax,L1+L2-2) ,"\n")
cat("\n")
lowerCmax<-100*exp((test_Cmax-ref_Cmax)-(T*SE_Cmax))
upperCmax<-100*exp((test_Cmax-ref_Cmax)+(T*SE_Cmax))
cat("************************90% C.I. for Ln (Cmax)****************************\n")
cat("90% CI Lower=", round(lowerCmax,3) ,"\n")
cat("90% CI Upper=", round(upperCmax,3) ,"\n")
cat("****************************************************************************\n")

cat("\n")
cat("\n")
cat("****************************************************************************\n")
cat("*                            BE Summary Report                             *\n") 
cat("*--------------------------------------------------------------------------*\n")
cat("  Dependent Variable: LnAUC0t                                               \n")
cat("*--------------------------------------------------------------------------*\n")
cat("n1(R=>T)=", L1 , "\n")
cat("n2(T=>R)=", L2 , "\n")
cat("N(n1+n2)=", L1+L2 , "\n")
cat("LSM-ref=", ref_AUC0t, "\n")
cat("LSM-test=",test_AUC0t, "\n")
cat("MSE=",anova(LnAUC0t)[5,3], "\n")
cat("SE=",SE_AUC0t, "\n")
cat("Diff. (test-ref)=", test_AUC0t-ref_AUC0t, "\n")
cat("t(0.95,N-2)=",T , "\n")
cat("Z(beta)=",Z_AUC0t, "\n")
cat("Power(1-beta)_Z=", pnorm(abs(Z_AUC0t)),"\n")
cat("t(beta)=",T_AUC0t, "\n")
cat("Power(1-beta)_t=",pt(T_AUC0t,L1+L2-2),"\n")
cat("\n")
lowerAUC0t<-100*exp((test_AUC0t-ref_AUC0t)-(T*SE_AUC0t))
UpperAUC0t<-100*exp((test_AUC0t-ref_AUC0t)+(T*SE_AUC0t))
cat("************************90% C.I. for Ln (AUC0t)****************************\n")
cat("90% CI Lower=", round(lowerAUC0t,3) ,"\n")
cat("90% CI Upper=", round(UpperAUC0t,3) ,"\n")
cat("****************************************************************************\n")

cat("\n")
cat("\n")
cat("****************************************************************************\n")
cat("*                            BE Summary Report                             *\n") 
cat("*--------------------------------------------------------------------------*\n")
cat("  Dependent Variable: AUC0inf                                               \n")
cat("*--------------------------------------------------------------------------*\n")
cat("n1(R=>T)=", L1 , "\n")
cat("n2(T=>R)=", L2 , "\n")
cat("N(n1+n2)=", L1+L2 , "\n")
cat("LSM-ref=", ref_AUC0INF, "\n")
cat("LSM-test=",test_AUC0INF, "\n")
cat("MSE=",anova(LnAUC0INF)[5,3], "\n")
cat("SE=",SE_AUC0INF, "\n")
cat("Diff. (test-ref)=", test_AUC0INF - ref_AUC0INF, "\n")
cat("t(0.95,N-2)=",T , "\n")
cat("Z(beta)=",Z_AUC0INF, "\n")
cat("Power(1-beta)_Z=",pnorm(abs(Z_AUC0INF)),"\n")
cat("t(beta)=",T_AUC0INF, "\n")
cat("Power(1-beta)_t=",pt(T_AUC0INF,L1+L2-2),"\n")
cat("\n")
LowerAUC0INF<-100*exp((test_AUC0INF - ref_AUC0INF)-(T*SE_AUC0INF))
UpperAUC0INF<-100*exp((test_AUC0INF - ref_AUC0INF)+(T*SE_AUC0INF))
cat("************************90% C.I. for Ln (AUC0inf)***************************\n")
cat("90% CI Lower=", round(LowerAUC0INF,3) ,"\n")
cat("90% CI Upper=", round(UpperAUC0INF,3) ,"\n")
cat("****************************************************************************\n")
NCAGLMmenu()
}
