NCAselect<-function(Totalplot,SingleRdata1,SingleTdata1, Dose,SingleRdata,SingleTdata,xaxis, yaxis,
                    Demo=FALSE, BANOVA=FALSE)
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
         xx1<-R.split[[i]]$time
         yy1<-R.split[[i]]$conc
         main<-paste(c("Select the exact 3 data points. Subj# Ref_",R.split[[i]]$subj[1]),collapse=" ")
         plot(xx1,yy1,log="y", xlim=range(xx1), ylim=range(yy1),xlab="Time", ylab= "Conc. (in log scale)", main=main,
         cex.lab = 1.2,cex.main = 1,pch=19,lab=c(20,20,30), xaxt="n")
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
ref_data<-data.frame(subj=y0$L1,time=y1$value,conc=log10(y2$value), drug=c(1))
rdata<-data.frame(subj=y0$L1,time=y1$value,conc=y2$value)
rdata.split<-split(rdata,list(rdata$subj))

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
              main<-paste(c("Select the exact 3 data points. Subj# Test_",T.split[[i]]$subj[1]),collapse=" ")
              plot(xx2,yy2, log="y",xlim=range(xx2), ylim=range(yy2),xlab="Time", ylab= "Conc. (in log scale)", main=main ,
              cex.lab = 1.2,cex.main = 1,pch=1,lab=c(20,20,30), xaxt="n")
              lines(xx2,yy2,lwd=1)
              axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),las=0)
              axis(1,at=0:100,tcl=-.2, labels=FALSE)
              co_data2[[i]]<-identify(T.split[[i]]$time,T.split[[i]]$conc, n=3)
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
test_data<-data.frame(subj=yy0$L1,time=yy1$value,conc=log10(yy2$value), drug=c(2))
tdata<-data.frame(subj=yy0$L1,time=yy1$value,conc=yy2$value)
tdata.split<-split(tdata,list(tdata$subj))


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