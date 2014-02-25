###
### for graph-mode data point selection  -- YJ
###

NCAselect<-function(Totalplot,SingleRdata1,SingleTdata1, Dose,SingleRdata,SingleTdata,xaxis, yaxis, Tau, 
                    TlastD,SingleRdata0,SingleTdata0,Demo=FALSE, BANOVA=FALSE, replicated=FALSE, MIX=FALSE,
                    parallel=FALSE, multiple=FALSE)   ### switch 'Demo=TRUE' for testing -YJ
{
options(warn=-1)
### plots of regression line for lambda_z_estimation
### lambda_z_regression_lines<-lambda_z_regression_lines   ### will move to NCA() to generate this pdf. -YJ
###
### windows(record=TRUE)   ### not working in linux or Mac OS X;
description_pointselect()
dev.new()                  ### works great for all platforms
par(mfrow=c(1,1),las=1, ask=TRUE)
### par(mai=c(0.9,0.9,0.9,0.9))   ### best-fit for my plots and suitable for ivivc for R and bear. -YJ
### par(mai=c(1.,1.,1.,1.))       ### testing... not work well. -YJ

cat("\n\n")
    cat("****************************************************************************\n")
    cat(" Data for the Ref. Products:                                                \n")
    cat("----------------------------------------------------------------------------\n")
    if(multiple){
    cat(" Cmax_ss, Tmax_ss, AUC(tau)ss, ln(Cmax_ss), ln(AUC(tau)ss), T1/2(z),  \n")
    cat("  Vd/F, MRT, lambda(z), Cl/F, Cav and Fluctuations will be calculated.\n")
    }
    else{
    cat(" AUC(0-t), AUC(0-inf), AUMC(0-t), AUMC(0-inf), lambda_z, Cl/F, Vd, MRT,  \n")
    cat("  and half-life (T1/2(z))                                                \n")
    }
    cat("                                                                            \n")
    cat("****************************************************************************\n")
    cat("\n\n")
###
### different from ARC(), aic()..., etc., here we trim 'SingleRdata1' and 'SingleTdata1' since v2.6.1
###
SingleRdata1<-na.omit(SingleRdata1)  ### v2.6.1 since the original dataset was used to output IDP, not trim at all 
SingleTdata1<-na.omit(SingleTdata1)  ### before arriving here, so we trim it now; otherwise it causes errors. -YJ

#split dataframe into sub-dataframe by subject for reference data
if(replicated){
       R.split<-split(SingleRdata1, list(SingleRdata1$code))
        subj<-0
        prd<-0
        seq<-0
        for (j in 1:(length(R.split))){
         subj[j]<-R.split[[j]][["subj"]][1]
         prd[j]<-R.split[[j]][["prd"]][1]
         seq[j]<-R.split[[j]][["seq"]][1]
         }
       }
       else{
       R.split<-split(SingleRdata1, list(SingleRdata1$subj))
       
        subj<-0
        for (j in 1:(length(R.split))){
        subj[j]<-R.split[[j]][["subj"]][1]
         }
       }

if(multiple){
 par(mfrow=c(1,1))  ### one pic for each window; old-man version
       #calculate kel for reference data
       co_data1<-NULL
       for(i in seq_along(R.split)){
         xx1<-R.split[[i]]$time-TlastD   ###  watch for this!! for multiple dose, we have to substrate TlastD for one dosing tau.
         yy1<-R.split[[i]]$conc
         ### cat("\n\n show yy1:");show(yy1);cat("\n\n")
         main<-paste(c("[Manual Selection] Subj#",R.split[[i]]$subj[1],"-Ref."),collapse=" ")
         plot(xx1,yy1, log="y",axes=FALSE,xlim=range(0, 1.2*Tau), ylim=c(1e-3,1e+5),
              xlab=xaxis, ylab= paste(yaxis,"(as log10 scale)"),        ### log="y" as semilog plot here (YJ)
         main=main,las=1, cex.lab = 1.2,cex.main = 1.0,lab=c(15,15,40),pch=19) ### ,frame.plot=FALSE) ### remove plot frame with 'frame.plot=FALSE' here  -YJ
         lines(xx1,yy1, lty=20)
         ### if (min(yy1)<1) {axis(1, pos=2)}
         ###    else {axis(1, pos=min(yy1))}  ### for x-axis
         axis(1, pos=0.001)
         axis(2, pos=0,las=1)             ### for y-axis
         co_data1[[i]]<-identify(xx1, yy1, n=6)    ### set max. select = 6 here! with subj(i)

###         
### co_data1[[i]] is data.frame for # of data points, e.g. 8, 9, 10
###
### convert the data point # back to (x,y) data; same as the next step but this step is for single subj ONLY
###      
         
         rr_melt<-melt(co_data1)
         ## show(rr_melt)
         YY.split<-split(rr_melt,list(rr_melt$L1))
         ## show(YY.split)
         xy1<-NULL
         s1<-NULL
         d1<-NULL
         d2<-NULL
         y0<-NULL
         y1<-NULL
         y2<-NULL
         for(j in seq_along(YY.split)){
               tx<-NULL
                for(k in 1:length(YY.split[[i]][["value"]])){   ## Yes! replace YY.split[[j]] with YY.split[[i]] for this & next line
                  tx[[k]]<-YY.split[[i]][["value"]][k]
                  }
                   xy1[[j]]<-R.split[[i]][tx, , ]               ## replace r.split 'j' with 'i' is correct way!
                   s1[[j]]<-c(xy1[[j]]$subj)
                   d1[[j]]<-c(xy1[[j]]$time)
                   d2[[j]]<-c(xy1[[j]]$conc)
                     }
         y0<-melt(s1)
         y1<-melt(d1)
         y2<-melt(d2)
         
         refx1_data<-data.frame(subj=y0$value,time=y1$value,conc=log10(y2$value),conc_data=y2$value,drug=c(1))
         refx1_data<-unique(refx1_data)   ### don't know why there are duplicated data; unique() remove all duplicates.
### show(refx1_data)  ### for debug; make sure it chooses the correct data points here
###
### above refx1_data is only for current subj
###         
         xxx1<-refx1_data$time-TlastD    ###  watch for this!! for multiple dose, we have to substrate TlastD for one dosing tau.
         yyy1<-refx1_data$conc
         conc_plot<-refx1_data$conc_data
         points(xxx1,conc_plot, pch="X", type="p", col="blue",lwd=2,cex=1.5)  ### here we have to use original data (conc) to plot selected points;
         lm_this_subj<-lm(yyy1~xxx1)                                  ### but use long10(conc.) to do linear regression.
         ## add a regression line here
         abline(lm_this_subj,col="red",lwd=2,untf=FALSE)  ## WORKING! Bravo~  has to set 'untf=FALSE' here
         ### add text here
         leg_txt<-"log10(Conc.) ="
         leg_txt<-paste(leg_txt,formatC(lm_this_subj$coefficients[[1]],format="f",digits=3),sep=" ")
         leg_txt<-paste(leg_txt,"+ (",sep=" ")
         leg_txt<-paste(leg_txt,formatC(lm_this_subj$coefficients[[2]],format="f",digits=5),sep="")
         leg_txt<-paste(leg_txt,")*Time",sep="")
         leg_txt<-paste(leg_txt,"  ",sep="")
         leg_txt<-paste(leg_txt,"\nR_sq =",sep="")
         leg_txt<-paste(leg_txt,formatC(summary(lm_this_subj)$r.squared,format="f",digits=4),sep=" ")
         ## show(leg_txt)
         ### add legend here
         legend("top",leg_txt,xjust=0,yjust=0,box.col="white")  ## find the appropriate position, shrink font size for legend with cex=0.7         
       }
  }
else{ 
       #calculate kel for reference data
       co_data1<-NULL
       for(i in seq_along(R.split)){
         xx1<-R.split[[i]]$time
         yy1<-R.split[[i]]$conc
         if(replicated){
          main<-paste(c("[Manual Selection] Subj#",R.split[[i]]$subj[1],
                         ", Period#",R.split[[i]]$prd[1],
                         ", Seq#",R.split[[i]]$seq[1],"-Ref."),collapse=" ")
          }
          else{
           main<-paste(c("[Manual Selection] Subj#",R.split[[i]]$subj[1],"-Ref."),collapse=" ")
           }
         plot(xx1,yy1,log="y", xlim=range(xx1), ylim=c(1e-3,1e+5),
              xlab=xaxis, ylab= paste(yaxis,"(as log10 scale)",sep=" "), main=main,
              cex.lab = 1.2,cex.main = 1,pch=19,lab=c(15,15,40),xaxt="n",frame.plot=FALSE)   ### remove plot frame with 'frame.plot=FALSE' here  -YJ
         lines(xx1,yy1, lty=20)
         axis(1,tcl=-.2,labels=TRUE)
         co_data1[[i]]<-identify(xx1, yy1, n=6)   ### set max. select = 6 here! with subj(i)

###         
### co_data1[[i]] is data.frame for # of data points, e.g. 8, 9, 10
###
### convert the data point # back to (x,y) data; same as the next step but this step is for single subj ONLY
###
         
         rr_melt<-melt(co_data1)
         ## show(rr_melt)
         YY.split<-split(rr_melt,list(rr_melt$L1))
         ## show(YY.split)
         xy1<-NULL
         s1<-NULL
         d1<-NULL
         d2<-NULL
         y0<-NULL
         y1<-NULL
         y2<-NULL
         for(j in seq_along(YY.split)){
               tx<-NULL
                for(k in 1:length(YY.split[[i]][["value"]])){   ## Yes! replace YY.split[[j]] with YY.split[[i]] for this & next line
                  tx[[k]]<-YY.split[[i]][["value"]][k]
                  }
                   xy1[[j]]<-R.split[[i]][tx, , ]               ## replace r.split 'j' with 'i' is correct way!
                   s1[[j]]<-c(xy1[[j]]$subj)
                   d1[[j]]<-c(xy1[[j]]$time)
                   d2[[j]]<-c(xy1[[j]]$conc)
                     }
         y0<-melt(s1)
         y1<-melt(d1)
         y2<-melt(d2)
         
         refx1_data<-data.frame(subj=y0$value,time=y1$value,conc=log10(y2$value),conc_data=y2$value,drug=c(1))
         refx1_data<-unique(refx1_data)   ### don't know why there are duplicated data; unique() remove all duplicates.
## show(refx1_data)  ### for debug? make sure it chooses the correct data points here
###
### above refx1_data is only for current subj
###
         xxx1<-refx1_data$time
         yyy1<-refx1_data$conc
         conc_plot<-refx1_data$conc_data
         points(xxx1,conc_plot, pch="X", type="p", col="blue",lwd=2,cex=1.5)  ### here we have to use original data (conc) to plot selected points;
         lm_this_subj<-lm(yyy1~xxx1)                                  ### but use long10(conc.) to do linear regression.
         ## add a regression line here
         abline(lm_this_subj,col="red",lwd=2,untf=FALSE)  ## WORKING! Bravo~  has to set 'untf=FALSE' here
         ### add text here
         leg_txt<-"log10(Conc.) ="
         leg_txt<-paste(leg_txt,formatC(lm_this_subj$coefficients[[1]],format="f",digits=3),sep=" ")
         leg_txt<-paste(leg_txt,"+ (",sep=" ")
         leg_txt<-paste(leg_txt,formatC(lm_this_subj$coefficients[[2]],format="f",digits=5),sep="")
         leg_txt<-paste(leg_txt,")*Time",sep="")
         leg_txt<-paste(leg_txt,"  ",sep="")
         leg_txt<-paste(leg_txt,"\nR_sq =",sep="")
         leg_txt<-paste(leg_txt,formatC(summary(lm_this_subj)$r.squared,format="f",digits=4),sep=" ")
         ## show(leg_txt)
         ### add legend here
         legend(x=min(xx1),y=min(yy1)/10,leg_txt,xjust=0,yjust=0,box.col="white")  ### set box.col="white" to remove legend box frame...  - YJ         
       }
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
ref_data<-data.frame(subj=y0$value,time=y1$value,conc=log10(y2$value),conc_data=y2$value,seq=y3$value,
                     prd=y4$value,drug=y5$value, code=y6$value)
rdata<-data.frame(subj=y0$value,time=y1$value,conc=y2$value, seq=y3$value,conc_data=y2$value, 
                     prd=y4$value,drug=y5$value, code=y6$value)    ### adding 'conc_data' here for replicate BE study. -YJ
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
   
   ref_data<-data.frame(subj=y0$value,time=y1$value,conc=log10(y2$value), conc_data=y2$value, drug=c(1))   #increase column:lnConc=log10(y2$value), conc=y2$value
   rdata<-data.frame(subj=y0$value, time=y1$value, conc=y2$value, conc_data=y2$value)  ### add ' conc_data=y2$value' -YJ
   rdata.split<-split(rdata,list(rdata$subj))
   }
      ######Test data
      cat("****************************************************************************\n")
      cat(" Data for the Test Products:                                                \n")
      cat("----------------------------------------------------------------------------\n")
      if(multiple){
      cat(" 1. Cmax_ss, Tmax_ss, AUC(tau)ss, ln(Cmax_ss), ln(AUC(tau)ss), T1/2(z),  \n")
      cat("    Vd/F, MRT, lambda, Cl/F, Cav and Fluctuation  were calculated.\n")
      cat(" 2. AUC(tau)ss was calculated using the linear trapezoidal method.       \n")
        }
      else{
      cat(" 1. AUC(0-t), AUC(0-inf), AUMC(0-t), AUMC(0-inf), lambda_z, Cl/F, Vd, MRT,  \n")
      cat("    and half-life (T1/2(z))                                \n")
      cat(" 2. AUC(0-t) was calculated using the linear trapezoidal method.            \n")
       }
      cat("                                                                            \n")
      cat("****************************************************************************\n")
      cat("\n\n")
        #split dataframe into sub-dataframe by subject for test data
         if(replicated){
          T.split<-split(SingleTdata1, list(SingleTdata1$code))
          subj1<-0
          prd1<-0
          seq1<-0
          for (j in 1:(length(T.split))){
           subj1[j]<-T.split[[j]][["subj"]][1]
           prd1[j]<-T.split[[j]][["prd"]][1]
           seq1[j]<-T.split[[j]][["seq"]][1]
          }
         }
         else{
          T.split<-split(SingleTdata1, list(SingleTdata1$subj))
          subj1<-0
          for (j in 1:(length(T.split))){
           subj1[j]<-T.split[[j]][["subj"]][1]
          }
         }
         
            #calculate kel for test data
             co_data2<-NULL
if(multiple){
         for(i in seq_along(T.split)){
           xx2<-T.split[[i]]$time-TlastD   ###  watch for this!! for multiple dose, we have to substrate TlastD for one dosing tau.
           yy2<-T.split[[i]]$conc
               main<-paste(c("[Manual Selection] Subj#",T.split[[i]]$subj[1],"-Test"),collapse=" ")
                  
           plot(xx2,yy2, log="y", axes=FALSE,xlim=range(0, 1.2*Tau), ylim=c(1e-3,1e+5),
               xlab=xaxis, ylab= paste(yaxis,"(as log10 scale)",sep=" "),   ## log="y" is to set Y-axis as log10() scale
           main=main,las=1, cex.lab = 1.2,cex.main = 1.0,lab=c(15,15,40),pch=19) ### ,frame.plot=FALSE)   ### remove plot frame with 'frame.plot=FALSE' here  -YJ)
           lines(xx2,yy2, lty=20)
           ### if (min(yy1)<1) {axis(1, pos=2)}
           ###    else {axis(1, pos=min(yy1))}  ### for x-axis
           axis(1, pos=0.001)
           axis(2, pos=0,las=1)     
           co_data2[[i]]<-identify(xx2,yy2, n=6)   ### set max. select = 6 here! with subj(i)

###         
###  co_data1[[i]] is data.frame for # of data points, e.g. 8, 9, 10
###
### convert the data point # back to (x,y) data; same as the next step but this step is for single subj ONLY
###      
         
         tt_melt<-melt(co_data2)
         ## show(tt_melt)
         YY.split<-split(tt_melt,list(tt_melt$L1))
         ## show(YY.split)
         xy1<-NULL
         s1<-NULL
         d1<-NULL
         d2<-NULL
         y0<-NULL
         y1<-NULL
         y2<-NULL
         for(j in seq_along(YY.split)){
               tx<-NULL
                for(k in 1:length(YY.split[[i]][["value"]])){   ## Yes! replace YY.split[[j]] with YY.split[[i]] for this & next line
                  tx[[k]]<-YY.split[[i]][["value"]][k]
                  }
                   xy1[[j]]<-T.split[[i]][tx, , ]               ## replace r.split 'j' with 'i' is correct way!
                   s1[[j]]<-c(xy1[[j]]$subj)
                   d1[[j]]<-c(xy1[[j]]$time)
                   d2[[j]]<-c(xy1[[j]]$conc)
                     }
         y0<-melt(s1)
         y1<-melt(d1)
         y2<-melt(d2)
         
         testx1_data<-data.frame(subj=y0$value,time=y1$value,conc=log10(y2$value),conc_data=y2$value,drug=c(1))
         testx1_data<-unique(testx1_data)   ### don't know why there are duplicated data; unique() remove all duplicates.
### show(testx1_data)   ### for debug; make sure it chooses the correct data points here
###
### above testx1_data is only for current subj
###
         xxx1<-testx1_data$time-TlastD  ###  watch for this!! for multiple dose, we have to substrate TlastD for one dosing tau.
         yyy1<-testx1_data$conc
         conc_plot<-testx1_data$conc_data
         points(xxx1,conc_plot, pch="X", type="p", col="blue",lwd=2,cex=1.5)  ### here we have to use original data (conc) to plot selected points;
         lm_this_subj<-lm(yyy1~xxx1)                                  ### but use long10(conc.) to do linear regression.
         ## add a regression line here
         abline(lm_this_subj,col="red",lwd=2,untf=FALSE)  ## WORKING! Bravo~  has to set 'untf=FALSE' here
         ### add text here
         leg_txt<-"log10(Conc.) ="
         leg_txt<-paste(leg_txt,formatC(lm_this_subj$coefficients[[1]],format="f",digits=3),sep=" ")
         leg_txt<-paste(leg_txt,"+ (",sep=" ")
         leg_txt<-paste(leg_txt,formatC(lm_this_subj$coefficients[[2]],format="f",digits=5),sep="")
         leg_txt<-paste(leg_txt,")*Time",sep="")
         leg_txt<-paste(leg_txt,"  ",sep="")
         leg_txt<-paste(leg_txt,"\nR_sq =",sep="")
         leg_txt<-paste(leg_txt,formatC(summary(lm_this_subj)$r.squared,format="f",digits=4),sep=" ")
         ## show(leg_txt)
         legend("top",leg_txt,xjust=0,yjust=0,box.col="white")  ## find the appropriate position, shrink font size for legend with cex=0.7              
      }
}
else{
            for(i in seq_along(T.split)){
              xx2<-T.split[[i]]$time
              yy2<-T.split[[i]]$conc
              if(replicated){
                  main<-paste(c("[Manual Selection] Subj#",T.split[[i]]$subj[1],
                                "Period#",T.split[[i]]$prd[1],
                                "Seq#",T.split[[i]]$seq[1],"-Test"),collapse=" ")
                 }
                 else{
                  main<-paste(c("[Manual Selection] Subj#",T.split[[i]]$subj[1],"-Test"),collapse=" ")
               }
              
              plot(xx2,yy2, log="y",xlim=range(xx2), ylim=c(1e-3,1e+5),
                   xlab=xaxis, ylab= paste(yaxis,"(as log10 scale)",sep=" "), main=main ,
                   cex.lab = 1.2,cex.main = 1,pch=1,lab=c(15,15,40), xaxt="n",frame.plot=FALSE)   ### remove plot frame with 'frame.plot=FALSE' here  -YJ)
              lines(xx2,yy2,lty=20)
              axis(1,tcl=-.2,labels=TRUE)
              co_data2[[i]]<-identify(xx2,yy2, n=6)  ### set max. select = 6 here! with subj(i)
###         
###  co_data1[[i]] is data.frame for # of data points, e.g. 8, 9, 10
###
### convert the data point # back to (x,y) data; same as the next step but this step is for single subj ONLY
###      
         
         tt_melt<-melt(co_data2)
         ## show(tt_melt)
         YY.split<-split(tt_melt,list(tt_melt$L1))
         ## show(YY.split)
         xy1<-NULL
         s1<-NULL
         d1<-NULL
         d2<-NULL
         y0<-NULL
         y1<-NULL
         y2<-NULL
         for(j in seq_along(YY.split)){
               tx<-NULL
                for(k in 1:length(YY.split[[i]][["value"]])){   ## Yes! replace YY.split[[j]] with YY.split[[i]] for this & next line
                  tx[[k]]<-YY.split[[i]][["value"]][k]
                  }
                   xy1[[j]]<-T.split[[i]][tx, , ]               ## replace r.split 'j' with 'i' is correct way!
                   s1[[j]]<-c(xy1[[j]]$subj)
                   d1[[j]]<-c(xy1[[j]]$time)
                   d2[[j]]<-c(xy1[[j]]$conc)
                     }
         y0<-melt(s1)
         y1<-melt(d1)
         y2<-melt(d2)
         
         testx1_data<-data.frame(subj=y0$value,time=y1$value,conc=log10(y2$value),conc_data=y2$value,drug=c(1))
         testx1_data<-unique(testx1_data)   ### don't know why there are duplicated data; unique() remove all duplicates.
###  show(testx1_data)   ### for debug; make sure it chooses the correct data points here
###
### above testx1_data is only for current subj
###
         xxx1<-testx1_data$time
         yyy1<-testx1_data$conc
         conc_plot<-testx1_data$conc_data
         points(xxx1,conc_plot, pch="X", type="p", col="blue",lwd=2,cex=1.5)  ### here we have to use original data (conc) to plot selected points;
         lm_this_subj<-lm(yyy1~xxx1)                                  ### but use long10(conc.) to do linear regression.
         ## add a regression line here
         abline(lm_this_subj,col="red",lwd=2,untf=FALSE)  ## WORKING! Bravo~  has to set 'untf=FALSE' here
         ### add text here
         leg_txt<-"log10(Conc.) ="
         leg_txt<-paste(leg_txt,formatC(lm_this_subj$coefficients[[1]],format="f",digits=3),sep=" ")
         leg_txt<-paste(leg_txt,"+ (",sep=" ")
         leg_txt<-paste(leg_txt,formatC(lm_this_subj$coefficients[[2]],format="f",digits=5),sep="")
         leg_txt<-paste(leg_txt,")*Time",sep="")
         leg_txt<-paste(leg_txt,"  ",sep="")
         leg_txt<-paste(leg_txt,"\nR_sq =",sep="")
         leg_txt<-paste(leg_txt,formatC(summary(lm_this_subj)$r.squared,format="f",digits=4),sep=" ")
         ## show(leg_txt)
         legend(x=min(xx1),y=min(yy1)/10,leg_txt,xjust=0,yjust=0,box.col="white")  ### set box.col="white" to remove legend box frame...  - YJ
      }
}
###

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
dev.off()   ### don't know why it is here...  -YJ
yy0<-melt(ss1)
yy1<-melt(dd1)
yy2<-melt(dd2)
yy3<-melt(dd3)
yy4<-melt(dd4)
yy5<-melt(dd5)
yy6<-melt(dd6)
test_data<-data.frame(subj=yy0$value,time=yy1$value,conc=log10(yy2$value),conc_data=yy2$value, seq=yy3$value,
                     prd=yy4$value,drug=yy5$value, code=yy6$value)
tdata<-data.frame(subj=yy0$value,time=yy1$value,conc=yy2$value, seq=yy3$value, conc_data=yy2$value,  
                  prd=yy4$value,drug=yy5$value, code=yy6$value)   ### adding 'conc_data' here for replicate BE. -YJ
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
                      dev.off()   ### don't know why it is here...  -YJ
                      yy0<-melt(ss1)
                      yy1<-melt(dd1)
                      yy2<-melt(dd2)

#fitting data with linear regression model
test_data<-data.frame(subj=yy0$value,time=yy1$value, conc=log10(yy2$value),conc_data=yy2$value,drug=c(2))
tdata<-data.frame(subj=yy0$value, time=yy1$value, conc=yy2$value, 
                  conc_data=yy2$value) ### add '...,conc_data=yy2$value', it works great. -YJ
tdata.split<-split(tdata,list(tdata$subj))
 }
###

if(replicated){
 if (Demo){
   if(MIX){
    #Demo=TRUE, MIX=TRUE   ### then go on...
    RepNCAdemo.MIX(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split)
    ### cat("\n\n");go2menu()
    } 
    else{
    #Demo=TRUE, MIX=FALSE  ### then stop here or it will cause loop running...
    ### RepNCAdemo(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split)
    cat("\n\n");go2menu()
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
 if(parallel){
    if(multiple){
     if (Demo){
       if(MIX){
       #Demo=TRUE, BANOVA=TRUE
       MultipleParaNCAdemo.MIX(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split,
                               Tau, TlastD,SingleRdata0,SingleTdata0)
       ### cat("\n\n");go2menu()
        } 
       else{
       #Demo=TRUE, BANOVA=FALSE
       ### MultipleParaNCAdemo(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split,
       ### Tau, TlastD,SingleRdata0,SingleTdata0)
       cat("\n\n");go2menu()
        }
      }   
     else {
       if(MIX){
       ##Demo=FALSE, BANOVA=TRUE
       MultipleParaNCAselectsave.MIX(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split,
       Tau, TlastD,SingleRdata0,SingleTdata0)
       }
       else{
       #Demo=FALSE, BANOVA=FALSE
       MultipleParaNCAselectsave(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split,
       Tau, TlastD,SingleRdata0,SingleTdata0)
        }
      }
    }
    else{
    if (Demo){
       if(MIX){
        #Demo=TRUE, BANOVA=TRUE
        ParaNCAdemo.MIX(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split)
        ### cat("\n\n");go2menu()
        } 
        else{
       #Demo=TRUE, BANOVA=FALSE
       ### ParaNCAdemo(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split)
       cat("\n\n");go2menu()
       }
      }   
    else {
        if(MIX){
        ##Demo=FALSE, BANOVA=TRUE
        ParaNCAselectsave.MIX(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split)
        }
        else{
        #Demo=FALSE, BANOVA=FALSE
        ParaNCAselectsave(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split)
        }
      }
    }
   }   
else{
 if(multiple){
 if (Demo){
   if(BANOVA){
    #Demo=TRUE, BANOVA=TRUE
    MultipleNCAdemo.BANOVA(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split,
                           Tau, TlastD,SingleRdata0,SingleTdata0)
    ### cat("\n\n");go2menu()
   } 
    else{
    #Demo=TRUE, BANOVA=FALSE
    ### MultipleNCAdemo(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split,
    ### Tau, TlastD,SingleRdata0,SingleTdata0)
    cat("\n\n");go2menu()
       }
    }   
 else {
     if(BANOVA){
     ##Demo=FALSE, BANOVA=TRUE
     MultipleNCAselectsave.BANOVA(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split,
     Tau, TlastD,SingleRdata0,SingleTdata0)
     }
      else{
       #Demo=FALSE, BANOVA=FALSE
       MultipleNCAselectsave(Totalplot, Dose, ref_data, test_data, SingleRdata,SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split,
       Tau, TlastD,SingleRdata0,SingleTdata0)
        }
      }
     }
 else{
 if (Demo){
   if(BANOVA){
    #Demo=TRUE, BANOVA=TRUE   ### then go on...
    NCAdemo.BANOVA(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split)
    ### cat("\n\n");go2menu()
   } 
    else{
    #Demo=TRUE, BANOVA=FALSE  ### then stop here
    ### NCAdemo(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split)
    cat("\n\n");go2menu()
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
  }
}