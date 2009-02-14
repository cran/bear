NCAselect<-function(Totalplot,SingleRdata1,SingleTdata1, Dose,SingleRdata,SingleTdata,xaxis, yaxis,
                    Demo=FALSE, BANOVA=FALSE, replicated=FALSE, MIX=FALSE)
{
cat("\n\n")
    cat("****************************************************************************\n")
    cat(" Data for the Ref. Products:                                                \n")
    cat("----------------------------------------------------------------------------\n")
    cat(" 1. AUC(0-t), AUC(0-inf), AUMC(0-t), AUMC(0-inf), lambda_z, Cl/F, Vd, MRT,  \n")
    cat("    and half-life (T1/2(z)) were calculated.                                \n")
    cat(" 2. AUC(0-t) was calculated using the linear trapezoidal method.            \n")
    cat("                                                                            \n")
    cat("****************************************************************************\n")
    cat("\n\n")
      #split dataframe into sub-dataframe by subject for reference data
      if(replicated){
       R.split<-split(SingleRdata1, list(SingleRdata1$code))
       }
       else{
       R.split<-split(SingleRdata1, list(SingleRdata1$subj))
       }
      

       subj<-0
       prd<-0
       seq<-0
       for (j in 1:(length(R.split))){
        subj[j]<-R.split[[j]][["subj"]][1]
        prd[j]<-R.split[[j]][["prd"]][1]
        seq[j]<-R.split[[j]][["seq"]][1]
       }

      windows(record = TRUE )
      par(mfrow=c(2,2))
       #calculate kel for reference data
       co_data1<-NULL
       for(i in seq_along(R.split)){
         xx1<-R.split[[i]]$time
         yy1<-R.split[[i]]$conc
         if(replicated){
          main<-paste(c("Subj# Ref_",R.split[[i]]$subj[1],
                         ", Seq_",R.split[[i]]$seq[1],
                         ", Prd_",R.split[[i]]$prd[1]),collapse=" ")
          }
            else{
             main<-paste(c("Select the exact 3 data points. Subj# Ref_",R.split[[i]]$subj[1]),collapse=" ")
           }
         
         plot(xx1,yy1,log="y", xlim=range(xx1), ylim=range(yy1),xlab="Time", ylab= "Conc. (in log scale)", main=main,
         cex.lab = 1.2,cex.main = 1,pch=19,lab=c(20,20,30), xaxt="n")
         lines(xx1,yy1, lty=20)
         axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)   
         axis(1,at=0:100,tcl=-.2, labels=FALSE)
         co_data1[[i]]<-identify(R.split[[i]]$time,  R.split[[i]]$conc, n=3)
         }

          r_melt<-melt(co_data1)
          Y.split<-split(r_melt,list(r_melt$L1))

if(replicated){          
 xy1<-NULL
 s1<-NULL
 d1<-NULL
 d2<-NULL
 d3<-NULL
 d4<-NULL
 d5<-NULL
 d6<-NULL
 for(j in seq_along(Y.split)){
     tx<-NULL
    for(i in 1:length(Y.split[[j]][["value"]])){
       tx[[i]]<-Y.split[[j]][["value"]][i]
      }
           xy1[[j]]<- R.split[[j]][tx, , ]
           s1[[j]]<-c(xy1[[j]]$subj)
           d1[[j]]<-c(xy1[[j]]$time)
           d2[[j]]<-c(xy1[[j]]$conc)
           d3[[j]]<-c(xy1[[j]]$seq)
           d4[[j]]<-c(xy1[[j]]$prd)
           d5[[j]]<-c(xy1[[j]]$drug)
           d6[[j]]<-c(xy1[[j]]$code)
           }
y0<-melt(s1)
y1<-melt(d1)
y2<-melt(d2)
y3<-melt(d3)
y4<-melt(d4)
y5<-melt(d5)
y6<-melt(d6)
ref_data<-data.frame(subj=y0$value,time=y1$value,conc=log10(y2$value), seq=y3$value,
                     prd=y4$value,drug=y5$value, code=y6$value)
rdata<-data.frame(subj=y0$value,time=y1$value,conc=y2$value, seq=y3$value,
                     prd=y4$value,drug=y5$value, code=y6$value)
rdata.split<-split(rdata,list(rdata$code))
}
       else{
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
   ref_data<-data.frame(subj=y0$L1,time=y1$value,conc=log10(y2$value), drug=c(1))
   rdata<-data.frame(subj=y0$L1,time=y1$value,conc=y2$value)
   rdata.split<-split(rdata,list(rdata$subj))
   }
      ######Test data
      cat("****************************************************************************\n")
      cat(" Data for the Test Products:                                                \n")
      cat("----------------------------------------------------------------------------\n")
      cat(" 1. AUC(0-t), AUC(0-inf), AUMC(0-t), AUMC(0-inf), lambda_z, Cl/F, Vd, MRT,  \n")
      cat("    and half-life (T1/2(z)) were calculated.                                \n")
      cat(" 2. AUC(0-t) was calculated using the linear trapezoidal method.            \n")
      cat("                                                                            \n")
      cat("****************************************************************************\n")
      cat("\n\n")
        #split dataframe into sub-dataframe by subject for test data
         if(replicated){
          T.split<-split(SingleTdata1, list(SingleTdata1$code))
         }
         else{
          T.split<-split(SingleTdata1, list(SingleTdata1$subj))
         }
        

         subj1<-0
         prd1<-0
         seq1<-0
         for (j in 1:(length(T.split))){
          subj1[j]<-T.split[[j]][["subj"]][1]
          prd1[j]<-T.split[[j]][["prd"]][1]
          seq1[j]<-T.split[[j]][["seq"]][1]
         }
            #calculate kel for test data
             co_data2<-NULL

            par(mfrow=c(2,2))
            for(i in seq_along(T.split)){
              xx2<-T.split[[i]]$time
              yy2<-T.split[[i]]$conc
              if(replicated){
                  main<-paste(c("Subj# Test_",T.split[[i]]$subj[1],
                                "Seq_",T.split[[i]]$seq[1],
                                "Prd_",T.split[[i]]$prd[1]),collapse=" ")
                 }
                 else{
                  main<-paste(c("Select the exact 3 data points. Subj# Test_",T.split[[i]]$subj[1]),collapse=" ")
               }
              
              plot(xx2,yy2, log="y",xlim=range(xx2), ylim=range(yy2),xlab="Time", ylab= "Conc. (in log scale)", main=main ,
              cex.lab = 1.2,cex.main = 1,pch=1,lab=c(20,20,30), xaxt="n")
              lines(xx2,yy2,lwd=1)
              axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
              axis(1,at=0:100,tcl=-.2, labels=FALSE)
              co_data2[[i]]<-identify(T.split[[i]]$time,T.split[[i]]$conc, n=3)
              }

              t_melt<-melt(co_data2)
              YY.split<-split(t_melt,list(t_melt$L1))
              
if(replicated){          
 xy2<-NULL
 ss1<-NULL
 dd1<-NULL
 dd2<-NULL
 dd3<-NULL
 dd4<-NULL
 dd5<-NULL
 dd6<-NULL
 for(j in seq_along(YY.split)){
     tx1<-NULL
    for(i in 1:length(YY.split[[j]][["value"]])){
       tx1[[i]]<-YY.split[[j]][["value"]][i]
      }
           xy2[[j]]<- T.split[[j]][tx1, , ]
           ss1[[j]]<-c(xy2[[j]]$subj)
           dd1[[j]]<-c(xy2[[j]]$time)
           dd2[[j]]<-c(xy2[[j]]$conc)
           dd3[[j]]<-c(xy2[[j]]$seq)
           dd4[[j]]<-c(xy2[[j]]$prd)
           dd5[[j]]<-c(xy2[[j]]$drug)
           dd6[[j]]<-c(xy2[[j]]$code)
           }
            dev.off()
yy0<-melt(ss1)
yy1<-melt(dd1)
yy2<-melt(dd2)
yy3<-melt(dd3)
yy4<-melt(dd4)
yy5<-melt(dd5)
yy6<-melt(dd6)
test_data<-data.frame(subj=yy0$value,time=yy1$value,conc=log10(yy2$value), seq=yy3$value,
                     prd=yy4$value,drug=yy5$value, code=yy6$value)
tdata<-data.frame(subj=yy0$value,time=yy1$value,conc=yy2$value, seq=yy3$value,
                     prd=yy4$value,drug=yy5$value, code=yy6$value)
tdata.split<-split(tdata,list(tdata$code))
}
       else{              
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
test_data<-data.frame(subj=yy0$L1,time=yy1$value,conc=log10(yy2$value), drug=c(2))
tdata<-data.frame(subj=yy0$L1,time=yy1$value,conc=yy2$value)
tdata.split<-split(tdata,list(tdata$subj))
 }
if(replicated){
  if (Demo){
    if(MIX){
     #Demo=TRUE, MIX=TRUE
     RepNCAdemo.MIX(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split)
     } 
     else{
     #Demo=TRUE, MIX=FALSE
     RepNCAdemo(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split)
        }
     }   
  else {
     if(MIX){
     ##Demo=FALSE, MIX=TRUE
     RepNCAselectsave.MIX(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split)
     }
      else{
     #Demo=FALSE, BANOVA=FALSE
       RepNCAselectsave(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split)              
         }
      }
    }

else{
 if (Demo){
    if(BANOVA){
     #Demo=TRUE, BANOVA=TRUE
     NCAdemo.BANOVA(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split)
    } 
     else{
     #Demo=TRUE, BANOVA=FALSE
     NCAdemo(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split)
        }
     }   
  else {
     if(BANOVA){
     ##Demo=FALSE, BANOVA=TRUE
     NCAselectsave.BANOVA(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split)
     }
      else{
     #Demo=FALSE, BANOVA=FALSE
       NCAselectsave(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split)
      }
    }
   } 
}