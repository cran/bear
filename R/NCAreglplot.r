###
### This function is for plotting regression lines of data manual selection for lambda_z,
### from previously saved data point selection file (.RData).
### The function of on-screen data point selection plots is within NCAselec().
### Hoep this will work. Basicaly I copy and paste all codes from NCAselect() first and then
### to revise the codes to the situation. The only difference is that this function has
### included already selected data point as ref_data & test_data.  -YJ [04/26/2013]
###

NCAreglplot<-function(Totalplot,Dose, ref_data, test_data, SingleRdata, SingleRdata1,
               SingleTdata,SingleTdata1,xaxis, yaxis,rdata.split,tdata.split,
               Tau, TlastD,SingleRdata0,SingleTdata0,
               Demo=TRUE,BANOVA=FALSE,replicated=FALSE,MIX=FALSE, parallel=FALSE, multiple=FALSE)  
               ## switch Demo=TRUE to allow Demo to save Lambda_z_selection as .csv & it works. --YJ
{
options(warn=-1)
### plots of regression line for lambda_z_estimation
lambda_z_regression_lines<-lambda_z_regression_lines
###
par(mfrow=c(1,1),las=1)   ### set 'ask=FASLE' (as default) to generate pdf file quickly, no more stop here. -YJ
pdf(lambda_z_regression_lines,paper="a4")  ### now prepare to save as pdf here.  -YJ
logo_plot_desc()
###

#split dataframe into sub-dataframe by subject for reference data
if(replicated){
       R.split<-split(SingleRdata1, list(SingleRdata1$code)) ### <- replicated data should split with 'code'
       LR.split<-split(ref_data, list(ref_data$code))        ### <- replicated data should split with 'code'
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
       R.split<-split(SingleRdata1, list(SingleRdata1$subj))  ### others split with 'subj'
       LR.split<-split(ref_data,list(ref_data$subj))   ### <--- show the data from ref_data which comes from .RData
       
        subj<-0
        for (j in 1:(length(R.split))){
        subj[j]<-R.split[[j]][["subj"]][1]
         }
       }

### plot ref data first 

if(multiple){
       #calculate kel for reference data
       co_data1<-NULL
       for(i in seq_along(R.split)){
         xx1<-R.split[[i]]$time-TlastD   ###  watch for this!! for multiple dose, we have to substrata TlastD for one dosing tau.
         yy1<-R.split[[i]]$conc
         main<-paste(c("[Manual Selection] Subj#",R.split[[i]]$subj[1],"- Ref."),collapse=" ")
         plot(xx1,yy1, log="y", axes=FALSE,xlim=range(0, 1.2*Tau), ylim=c(1e-3,1e+5),
              xlab=xaxis, ylab= paste(yaxis,"(as log10 scale)",sep=" "),     ## log="y" as semilog plot here (YJ)
         main=main,las=1, cex.lab = 1.2,cex.main = 1.0,lab=c(15,15,40),pch=19) ### ,frame.plot=FALSE)   ### remove plot frame with 'frame.plot=FALSE' here  -YJ
         lines(xx1,yy1, lty=20)
         axis(1, pos=0.001)
         axis(2, pos=0,las=1)
         xxx1<-LR.split[[i]]$time-TlastD              ### can be error here... -YJ
         yyy1<-LR.split[[i]]$conc                     ### here 'conc' is log10(conc) for regression. -YJ
         yyyy1<-LR.split[[i]]$conc_data               ### conc_data is untransformed data and is for plot.
         ### co_data1[i]<-data.frame(xxx1,yyy1)       ### Ah! this means data from previously saved .RData.
         ### cat("\n\n show yyy1:\n\n");show(yyy1);cat("\n\n")
         ### cat("\n\n show yyyy1:\n\n");show(yyyy1);cat("\n\n")
         points(xxx1,yyyy1, pch="X", type="p", 
                col="blue",lwd=2,cex=1.5,untf=FALSE)  ### here we have to use original data (conc) to plot selected points;
         lm_this_subj<-lm(yyy1~xxx1)                  ### but use long10(conc.) to do linear regression.
         ## add a regression line here
         abline(lm_this_subj,col="red",lwd=2,untf=FALSE)  ## WORKING! Bravo~  has to set 'untf=FALSE' here
         
         ### Add legend HERE [2013/6/26 AM 06:41:29] -YJ
         ###
         ### if want to convert to ln() format: 2.303674*LLm$coefficients[[2]]
         ### set box.col="white" to remove legend box frame...- YJ
         ###
         A<-formatC(lm_this_subj$coefficients[[1]],format="f",digits=3)
         B<-formatC(lm_this_subj$coefficients[[2]],format="f",digits=4)
         C<-formatC(summary(lm_this_subj)$adj.r.squared,format="f",digits=4)
         
         legend("top",legend= as.expression(c(bquote(paste("log10(Conc.) = ",.(A),"+ (",.(B),")*Time;")),
                                              bquote(paste(" Adj. ",R^2," = ",.(C))))),
                xjust=0,yjust=0,box.col="white",box.lwd=0,cex=1.2)
         ###
         ### end of legend
         ###
       }
  }
else{ 
      par(mfrow=c(1,1))
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
         plot(xx1,yy1,log="y", xlim=range(xx1), ylim=c(1e-3,1e+5),xlab=xaxis, ylab= paste(yaxis,"(as log10 scale)",sep=" "), main=main,
         cex.lab = 1.2,cex.main = 1,pch=19,lab=c(15,15,40), xaxt="n",frame.plot=FALSE)   ### remove plot frame with 'frame.plot=FALSE' here  -YJ
         lines(xx1,yy1, lty=20)
         axis(1,tcl=-.2,labels=TRUE)
         xxx1<-LR.split[[i]]$time                     ### works great now. -YJ    
         yyy1<-LR.split[[i]]$conc                     ### here 'conc' is log10(conc) for regression. -YJ
         yyyy1<-LR.split[[i]]$conc_data               ### conc_data is untransformed data and is for plot.
         points(xxx1,yyyy1, pch="X", type="p", 
                col="blue",lwd=2,cex=1.5,untf=FALSE)  ### here we have to use original data (conc) to plot selected points;
         lm_this_subj<-lm(yyy1~xxx1)                  ### but use long10(conc.) to do linear regression.
         ## add a regression line here
         abline(lm_this_subj,col="red",lwd=2,untf=FALSE)  ## WORKING! Bravo~  has to set 'untf=FALSE' here
         
         ### Add legend HERE [2013/6/26 AM 06:41:29] -YJ
         ###
         ### if want to convert to ln() format: 2.303674*LLm$coefficients[[2]]
         ### set box.col="white" to remove legend box frame...- YJ
         ###
         A<-formatC(lm_this_subj$coefficients[[1]],format="f",digits=3)
         B<-formatC(lm_this_subj$coefficients[[2]],format="f",digits=4)
         C<-formatC(summary(lm_this_subj)$adj.r.squared,format="f",digits=4)
         
         legend("top",legend= as.expression(c(bquote(paste("log10(Conc.) = ",.(A),"+ (",.(B),")*Time;")),
                                              bquote(paste(" Adj. ",R^2," = ",.(C))))),
                xjust=0,yjust=0,box.col="white",box.lwd=0,cex=1.2)
         ###
         ### end of legend
         ###
       }
  }    
  
######Test data
if(replicated){
       T.split<-split(SingleTdata1, list(SingleTdata1$code)) ### <- replicated data should split with 'code'
       LT.split<-split(test_data, list(test_data$code))      ### <- replicated data should split with 'code'
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
          T.split<-split(SingleTdata1, list(SingleTdata1$subj))  ### others split with 'subj'
          LT.split<-split(test_data,list(test_data$subj))   ### <--- show the data from ref_data which comes from .RData
          subj1<-0
          for (j in 1:(length(T.split))){
           subj1[j]<-T.split[[j]][["subj"]][1]
          }
         }
         
### now plot test data
             
if(multiple){
   par(mfrow=c(1,1))
   co_data2<-NULL
   for(i in seq_along(T.split)){
         xx2<-T.split[[i]]$time-TlastD   ###  watch for this!! for multiple dose, we have to substrata TlastD for one dosing tau.
         yy2<-T.split[[i]]$conc
         main<-paste(c("[Manual Selection] Subj#",T.split[[i]]$subj[1],"- Test"),collapse=" ")
         plot(xx2,yy2, log="y", axes=FALSE,xlim=range(0, 1.2*Tau), ylim=c(1e-3,1e+5),
              xlab=xaxis, ylab= paste(yaxis,"(as log10 scale)",sep=" "),   ## log="y" is to set Y-axis as log10() scale
              main=main,las=1, cex.lab = 1.2,cex.main = 1.0,lab=c(15,15,40),pch=19) ### ,frame.plot=FALSE)   ### remove plot frame with 'frame.plot=FALSE' here  -YJ)
         lines(xx2,yy2, lty=20)
         axis(1, pos=0.001)
         axis(2, pos=0,las=1)
         ### co_data2[[i]]<-data.frame(xxx1,yyy1)     ### Ah! this means data from previously saved .RData.
         xxx1<-LT.split[[i]]$time-TlastD              ### OK now. -YJ
         yyy1<-LT.split[[i]]$conc                     ### here 'conc' is log10(conc) for regression. -YJ
         yyyy1<-LT.split[[i]]$conc_data               ### conc_data is untransformed data and is for plot.
         points(xxx1,yyyy1, pch="X", type="p", 
               col="blue",lwd=2,cex=1.5,untf=FALSE)   ### here we have to use original data (conc) to plot selected points;
         lm_this_subj<-lm(yyy1~xxx1)                  ### but use long10(conc.) to do linear regression.
         ## add a regression line here
         abline(lm_this_subj,col="red",lwd=2,untf=FALSE)  ## WORKING! Bravo~  has to set 'untf=FALSE' here
         
         ### Add legend HERE [2013/6/26 AM 06:41:29] -YJ
         ###
         ### if want to convert to ln() format: 2.303674*LLm$coefficients[[2]]
         ### set box.col="white" to remove legend box frame...- YJ
         ###
         A<-formatC(lm_this_subj$coefficients[[1]],format="f",digits=3)
         B<-formatC(lm_this_subj$coefficients[[2]],format="f",digits=4)
         C<-formatC(summary(lm_this_subj)$adj.r.squared,format="f",digits=4)
         
         legend("top",legend= as.expression(c(bquote(paste("log10(Conc.) = ",.(A),"+ (",.(B),")*Time;")),
                                              bquote(paste(" Adj. ",R^2," = ",.(C))))),
                xjust=0,yjust=0,box.col="white",box.lwd=0,cex=1.2)
         ###
         ### end of legend
         ###         
     }
}
else{
          for(i in seq_along(T.split)){
          xx2<-T.split[[i]]$time
          yy2<-T.split[[i]]$conc
          if(replicated){
              main<-paste(c("[Manual Selection] Subj#",T.split[[i]]$subj[1],
                            "Period#",T.split[[i]]$prd[1],
                            "Seq#",T.split[[i]]$seq[1],"- Test"),collapse=" ")
             }
             else{
              main<-paste(c("[Manual Selection] Subj#",T.split[[i]]$subj[1],"- Test"),collapse=" ")
           }
              
         plot(xx2,yy2, log="y",xlim=range(xx2), ylim=c(1e-3,1e+5),xlab=xaxis, ylab= paste(yaxis,"(as log10 scale)",sep=" "), main=main ,
         cex.lab = 1.2,cex.main = 1,pch=1,lab=c(15,15,40), xaxt="n",frame.plot=FALSE)   ### remove plot frame with 'frame.plot=FALSE' here  -YJ)
         lines(xx2,yy2,lty=20)
         axis(1,tcl=-.2,labels=TRUE)
         xxx1<-LT.split[[i]]$time                     ### OK now. -YJ
         yyy1<-LT.split[[i]]$conc                     ### here 'conc' is log10(conc) for regression. -YJ
         yyyy1<-LT.split[[i]]$conc_data               ### conc_data is untransformed data and is for plot.
         points(xxx1,yyyy1, pch="X", type="p", 
               col="blue",lwd=2,cex=1.5,untf=FALSE)   ### here we have to use original data (conc) to plot selected points;
         lm_this_subj<-lm(yyy1~xxx1)                                  ### but use long10(conc.) to do linear regression.
         ## add a regression line here
         abline(lm_this_subj,col="red",lwd=2,untf=FALSE)  ## WORKING! Bravo~  has to set 'untf=FALSE' here

         ### Add legend HERE [2013/6/26 AM 06:41:29] -YJ
         ###
         ### if want to convert to ln() format: 2.303674*LLm$coefficients[[2]]
         ### set box.col="white" to remove legend box frame...- YJ
         ###
         A<-formatC(lm_this_subj$coefficients[[1]],format="f",digits=3)
         B<-formatC(lm_this_subj$coefficients[[2]],format="f",digits=4)
         C<-formatC(summary(lm_this_subj)$adj.r.squared,format="f",digits=4)
         
         legend("top",legend= as.expression(c(bquote(paste("log10(Conc.) = ",.(A),"+ (",.(B),")*Time;")),
                                              bquote(paste(" Adj. ",R^2," = ",.(C))))),
                xjust=0,yjust=0,box.col="white",box.lwd=0,cex=1.2)
         ###
         ### end of legend
         ###                  
    }
}
### close dev() now
dev.off()  ## to close pdf device now... YJ
graphics.off()
###                              
}