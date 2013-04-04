##export with pdf file
NCAplot<-function(Totalplot,SingleRdata,SingleTdata,TotalData,xaxis,yaxis,TlastD,
                  replicated=FALSE,parallel=FALSE,multiple=FALSE)
{
options(warn=-1)
nca_plot_xfile<- nca_plot_xfile   ## to avoid "not visible binding..." error message with codetool
  
  R.split<-split(SingleRdata, list(SingleRdata$subj))
  T.split<-split(SingleTdata, list(SingleTdata$subj))

Totalplot<-Totalplot[ do.call(order, Totalplot) ,]
s.split<-split(Totalplot,list(Totalplot$subj))
Totalplot$conc[Totalplot$conc == 0] <- NA
Totalplot <- na.omit(Totalplot)            ### Ahhh... here to remove conc. < BLLQ  --YJ
### for debug here
### write.csv(Totalplot,file="Totalplot.csv",row.names=FALSE)   # it's total raw input data; can be different with replicate crossover or parallel
###

    if(replicated){
     LR<-data.frame(subj=Totalplot$subj,  seq=Totalplot$seq, prd=Totalplot$prd, drug=Totalplot$drug,
                    time=Totalplot$time,  conc=Totalplot$conc, code=Totalplot$code)
     LR$conc[LR$conc == 0] <- NA
     LR <- na.omit(LR)
     Ls.split<-split(LR, list(LR$subj))
          
     Totals.split<-split(TotalData, list(TotalData$drug))
     
     Fdata<-split(TotalData, list(TotalData$drug))
     RefData<-Fdata[[1]]
     TestData<-Fdata[[2]]
      SeqLeg<-split(RefData, list(RefData$seq))
      SeqLeg1 <- reshape(SeqLeg[[1]], idvar=c("subj", "drug","seq"), timevar =
      "prd",direction = "wide")
      SeqLeg2 <- reshape(SeqLeg[[2]], idvar=c("subj", "drug","seq"), timevar =
      "prd",direction = "wide")  
      L1<-length(SeqLeg1$subj)
      L2<-length(SeqLeg2$subj)
     
     prdcount<-length(levels(TotalData$prd))
       }
     else{ 
       if(parallel){        
        LR<-data.frame(subj=Totalplot$subj,  drug=Totalplot$drug, time=Totalplot$time,  conc=Totalplot$conc)
        Totals.split<-split(TotalData, list(TotalData$drug))
        L1<-length(Totals.split[[1]]$subj) #ref
        L2<-length(Totals.split[[2]]$subj) #test

        Totalplot.split<-split(Totalplot, list(Totalplot$subj))
        Totalplotpara<-split( Totalplot, list(Totalplot$drug))  
         if(multiple){
           SingleRdata0<-SingleRdata
           SingleRdata0$conc[SingleRdata0$conc == 0] <- NA
           SingleRdata1 <- na.omit(SingleRdata0)
           paraR.split<-split(SingleRdata1, list(SingleRdata1$subj))
         
            SingleTdata0<-SingleTdata
            SingleTdata0$conc[SingleTdata0$conc == 0] <- NA
            SingleTdata1 <- na.omit(SingleTdata0)
            paraT.split<-split(SingleTdata1, list(SingleTdata1$subj))
            Totalplot<- rbind(SingleRdata,SingleTdata)
        
        }
        else{
        
        paraR.split<-split( Totalplotpara[[1]], list(Totalplotpara[[1]]$subj))
        paraT.split<-split( Totalplotpara[[2]], list(Totalplotpara[[2]]$subj))
          }
        }
       else{
         LR<-data.frame(subj=Totalplot$subj,  seq=Totalplot$seq, prd=Totalplot$prd, drug=Totalplot$drug,
               time=Totalplot$time,  conc=Totalplot$conc)
         Totals.split<-split(TotalData, list(TotalData$drug))
         L<-length(Totals.split[[1]]$subj)
         
         }
     LR$conc[LR$conc == 0] <- NA
     LR <- na.omit(LR)
     Ls.split<-split(LR, list(LR$subj))
     }

pdf(nca_plot_xfile, paper = "a4", bg = "white")

#0.
description_plot()
par(mai=c(1.3,2,1.3,1.8)) 
#1.#################################individual subject for test vs ref (Cp vs time)
if(parallel){
#ref.  
for(i in seq_along(R.split)){
     if(multiple){
        xx1<-R.split[[i]]$time-TlastD
        yy1<-R.split[[i]]$conc     
        main<-paste(c("Subj. #", R.split[[i]]$subj[1]),collapse=" ")
        plot(0, 0, axes=FALSE,xlim=range(min(xx1)+(min(xx1)/2), 2*max(xx1)), ylim=range(0,max(yy1)+(max(yy1)/2)), 
                 cex = 1, xlab=xaxis, ylab=yaxis,cex.lab = 1, x.leg = 100000, bty="l",main=main,
                 font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n")  
        lines(xx1,yy1, lty=2)
        points(xx1,yy1,pch=19,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
        axis(1, pos=0)  #tick for x-axis
        axis(2, pos=0,las=1)  #tick for y-axis
        
        temp <- legend("topleft", legend = c("Ref."),
               text.width = strwidth("1000"),
               lty = 2, xjust = 1, yjust = 1)
         }
      else{
        xx1<-R.split[[i]]$time
        yy1<-R.split[[i]]$conc     
        main<-paste(c("Subj. #", R.split[[i]]$subj[1]),collapse=" ")
        plot(0, 0,xlim=range(0, xx1+ (xx1/5)), ylim=range(yy1), 
                 xlab=xaxis, ylab=yaxis,cex.lab = 1, x.leg = 100000, bty="l",main=main,
                 font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n")  
        lines(xx1,yy1, lty=2)
        points(xx1,yy1,pch=19,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
         xtick(xx1) #tick for x-axis
         ytick(yy1) #tick for y-axis
        temp <- legend("topright", legend = c("Ref."),
               text.width = strwidth("1000"),
               lty = 2, xjust = 1, yjust = 1)
         }
      }                                                     
#test
for(i in seq_along(T.split)){
    if(multiple){
         xx1<-T.split[[i]]$time-TlastD
         yy1<-T.split[[i]]$conc     
         main<-paste(c("Subj. #", T.split[[i]]$subj[1]),collapse=" ")
         plot(0, 0, axes=FALSE,xlim=range(min(xx1)+(min(xx1)/2), 2*max(xx1)), ylim=range(0,max(yy1)+(max(yy1)/2)), cex = 1,
                 xlab=xaxis, ylab=yaxis,cex.lab = 1, x.leg = 100000, bty="l",main=main,
                 font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n")  
         lines(xx1,yy1, lty=1)
         points(xx1,yy1,pch=1,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
         axis(1, pos=0)
         axis(2, pos=0,las=1) 
        temp <- legend("topleft", legend = c("Test"),
               text.width = strwidth("1000"),
               lty = 1, xjust = 1, yjust = 1)
        }
     else{ 
         xx1<-T.split[[i]]$time
         yy1<-T.split[[i]]$conc     
         main<-paste(c("Subj. #", T.split[[i]]$subj[1]),collapse=" ")
         
         plot(0, 0, xlim=range(0, xx1+ (xx1/5)), ylim=range(yy1), xcex = 1,
                 xlab=xaxis, ylab=yaxis,cex.lab = 1, x.leg = 100000, bty="l",main=main,
                 font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n")  
         lines(xx1,yy1, lty=1)
         points(xx1,yy1,pch=1,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
         xtick(xx1) #tick for x-axis
         ytick(yy1) #tick for y-axis
         temp <- legend("topright", legend = c("Test"),
               text.width = strwidth("1000"),
               lty = 1, xjust = 1, yjust = 1)
        }
      }                                     
}
else{
for(i in seq_along(s.split)){
    if(replicated){
      xx1<-s.split[[i]]$time
      yy1<-s.split[[i]]$conc     
       main<-paste(c("Subject#", s.split[[i]]$subj[1]),collapse=" ")
       lineplot.CI(s.split[[i]]$time, s.split[[i]]$conc, group = s.split[[i]]$code, cex = 1,
##                 xlim=range(0, xx1+ (xx1/5)),xlab=xaxis, ylab=yaxis,cex.lab = 1, x.leg = 100000, bty="l",main=main,
                 xlim=range(0, xx1+ (xx1/5)),xlab=xaxis, ylab=yaxis,cex.lab = 1, bty="l",main=main,
                 font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n",legend=FALSE)  
      xtick(xx1) #tick for x-axis
      ytick(yy1) #tick for y-axis
      }
      else{
        if(multiple){
         xx1<-s.split[[i]]$time-TlastD
         yy1<-s.split[[i]]$conc     
         main<-paste(c("Subject#", s.split[[i]]$subj[1]),collapse=" ")
         lineplot.CI(xx1, yy1, group = s.split[[i]]$drug, cex = 1,
            axes=FALSE,xlim=range(min(xx1)+(min(xx1)/2), 2*max(xx1)), ylim=range(0,max(yy1)+(max(yy1)/2)), 
##            xlab=xaxis, ylab=yaxis,cex.lab = 1, x.leg = 100000, bty="l",main=main,
            xlab=xaxis, ylab=yaxis,cex.lab = 1, bty="l",main=main,
            font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n",legend=FALSE)
         axis(1, pos=0)
         axis(2, pos=0,las=1) 
        }
        else{
        xx1<-s.split[[i]]$time
        yy1<-s.split[[i]]$conc     
        main<-paste(c("Subject#", s.split[[i]]$subj[1]),collapse=" ")
        lineplot.CI(s.split[[i]]$time, s.split[[i]]$conc, group = s.split[[i]]$drug, cex = 1,
##            xlim=range(0, xx1+ (xx1/5)),xlab=xaxis, ylab=yaxis,cex.lab = 1, x.leg = 100000, bty="l",main=main,
            xlim=range(0, xx1+ (xx1/5)),xlab=xaxis, ylab=yaxis,cex.lab = 1, bty="l",main=main,
            font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n",legend=FALSE)
        xtick(xx1) #tick for x-axis
        ytick(yy1) #tick for y-axis
        }     
 }            
  
if(replicated){
  prdcount(i,s.split, prdcount)
 }
else{
  if(multiple){
   temp <- legend("topleft",legend = c("Test", "Ref"),
               text.width = strwidth("1000"),
               lty = 1:2, xjust = 1, yjust = 1)
   }
   else{
   temp <- legend("topright",legend = c("Test", "Ref"),
               text.width = strwidth("1000"),
               lty = 1:2, xjust = 1, yjust = 1)
     }
   }
  } 
}
#2.#####################################individual subject for test vs ref (lnCp vs time) -semilog plots (YJ)
if(parallel){
 #Ref.
 for(i in seq_along(paraR.split)){
   if(multiple){  
     xx1<-paraR.split[[i]]$time-TlastD
     yy1<-paraR.split[[i]]$conc   
     main<-paste(c("Subj. #", paraR.split[[i]]$subj[1]),collapse=" ")
        plot(0, 0, log="y",axes=FALSE,xlim=range(min(xx1)+(min(xx1)/2), 2*max(xx1)), ylim=range(1, 4*max(yy1)), cex = 1,
                  xlab = "Time", ylab = "Conc. (in log scale)",cex.lab = 1, x.leg = 100000, bty="l",main=main,
                  font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n")

        points(xx1,yy1,pch=19,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
        lines(xx1,yy1, lty=2)
        axis(1, pos=1)
        axis(2, pos=0,las=1) 
         
         temp <- legend("topleft", legend = c("Ref."),
               text.width = strwidth("1000"),
               lty = 2, xjust = 1, yjust = 1)
         }
      else{
        xx1<-paraR.split[[i]]$time
        yy1<-paraR.split[[i]]$conc     
        main<-paste(c("Subj. #", paraR.split[[i]]$subj[1]),collapse=" ")
        plot(0, 0, log="y",xlim=range(0, xx1+ (xx1/5)), ylim=range(yy1), cex = 1,
                  xlab = "Time", ylab = "Conc. (in log scale)",cex.lab = 1, x.leg = 100000, bty="l",main=main,
                  font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n")

        points(xx1,yy1,pch=19,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
        lines(xx1,yy1, lty=2)
         xtick(xx1) #tick for x-axis
        
        temp <- legend("topright", legend = c("Ref."),
               text.width = strwidth("1000"),
               lty = 2, xjust = 1, yjust = 1)
        }
      }                                                     
#Test
for(i in seq_along(paraT.split)){
    if(multiple){  
     xx1<-paraT.split[[i]]$time-TlastD
     yy1<-paraT.split[[i]]$conc     
     main<-paste(c("Subj. #", paraT.split[[i]]$subj[1]),collapse=" ")
        plot(0, 0, log="y",axes=FALSE,xlim=range(min(xx1)+(min(xx1)/2), 2*max(xx1)), ylim=range(1, 4*max(yy1)),cex = 1,
                  xlab = "Time", ylab = "Conc. (in log scale)",cex.lab = 1, x.leg = 100000, bty="l",main=main,
                  font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n")
        points(xx1,yy1,pch=1,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
        lines(xx1,yy1, lty=1)
        axis(1, pos=1)
        axis(2, pos=0,las=1) 
        
         temp <- legend("topleft", legend = c("Test"),
               text.width = strwidth("1000"),
               lty = 1, xjust = 1, yjust = 1)
         }
      else
         {
        xx1<-paraT.split[[i]]$time
        yy1<-paraT.split[[i]]$conc     
        main<-paste(c("Subj. #", paraT.split[[i]]$subj[1]),collapse=" ")
        plot(0, 0, log="y",xlim=range(xx1, xx1+ (xx1/5)), ylim=range(yy1), cex = 1,
                  xlab = "Time", ylab = "Conc. (in log scale)",cex.lab = 1, x.leg = 100000, bty="l",main=main,
                  font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n")
        points(xx1,yy1,pch=1,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
        lines(xx1,yy1, lty=1)
        xtick(xx1) #tick for x-axis
        temp <- legend("topright", legend = c("Test"),
               text.width = strwidth("1000"),
               lty = 1, xjust = 1, yjust = 1)
        }
      }                                            
}
else{
 for(i in seq_along(Ls.split)){
  
   if(replicated){
      xx1<-Ls.split[[i]]$time
      yy1<-Ls.split[[i]]$conc   
      main<-paste(c("Subject#", Ls.split[[i]]$subj[1]),collapse=" ")
      lineplot.CI(Ls.split[[i]]$time, Ls.split[[i]]$conc, log="y", group = Ls.split[[i]]$code, cex = 1,
                  xlim=range(0, Ls.split[[i]]$time+ (Ls.split[[i]]$time/5)),ylim=range(1, 4*max(yy1)),
                  xlab = "Time", ylab = "Conc. (in log scale)",cex.lab = 1, x.leg = 100000, bty="l",main=main,
                  font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n",legend=FALSE)
      xtick(xx1) #tick for x-axis
    }
    else{
      if(multiple){
       xx1<-Ls.split[[i]]$time-TlastD
       yy1<-Ls.split[[i]]$conc   
       main<-paste(c("Subject#", Ls.split[[i]]$subj[1]),collapse=" ")
       lineplot.CI(xx1, yy1, log="y", group = Ls.split[[i]]$drug, cex = 1,
          ,axes=FALSE,xlim=range(min(xx1)+(min(xx1)/2), 2*max(xx1)),ylim=range(1, 4*max(yy1)),
          xlab = "Time", ylab = "Conc. (in log scale)",cex.lab = 1, x.leg = 100000, bty="l",main=main,
          font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n",legend=FALSE)
        axis(1, pos=1)
        axis(2, pos=0,las=1) 
        }
      else{
       xx1<-Ls.split[[i]]$time
       yy1<-Ls.split[[i]]$conc   
       main<-paste(c("Subject#", Ls.split[[i]]$subj[1]),collapse=" ")
       lineplot.CI(Ls.split[[i]]$time, Ls.split[[i]]$conc, log="y", group = Ls.split[[i]]$drug, cex = 1,
          ,xlim=range(0, Ls.split[[i]]$time+ (Ls.split[[i]]$time/5)),ylim=range(1, 4*max(yy1)),
          xlab = "Time", ylab = "Conc. (in log scale)",cex.lab = 1, x.leg = 100000, bty="l",main=main,
          font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n",legend=FALSE)
        xtick(xx1) #tick for x-axis
        }
      } 
      
  if(replicated){
      prdcount(i,s.split, prdcount)
     }
  else{
      if(multiple){
      temp <- legend("topleft",legend = c("Test", "Ref"),
               text.width = strwidth("1000"),
               lty = 1:2, xjust = 1, yjust = 1)
      }
      else{
      temp <- legend("topright",legend = c("Test", "Ref"),
               text.width = strwidth("1000"),
               lty = 1:2, xjust = 1, yjust = 1)
    }
  }
 }
}
#3. ###############################show all subjects in one plot at the same time for test
## main1<-"Spaghetti plot - Test Product"    ### cause err!
if(replicated){
xx1<-SingleTdata$time
yy1<-SingleTdata$conc  
lineplot.CI(SingleTdata$time, SingleTdata$conc, group = SingleTdata$code, type="l",  ## the leading ',' in the next line is necessar to have connected lines!
            ,xlim=range(xx1, xx1+ (xx1/5)), xlab=xaxis, ylab=yaxis,ylim=c(0,max(Totalplot$conc)),cex.lab = 1,x.leg =1000000, bty="l", lty=1,las=1,
            font.lab=2,cex.axis=1,cex.main=1,x.cont=TRUE,xaxt="n",legend=FALSE)
            xtick(xx1) #tick for x-axis
            ytick(yy1) #tick for y-axis
}
else{
   if(multiple){
   xx1<-SingleTdata$time-TlastD
   yy1<-SingleTdata$conc  
   lineplot.CI(xx1, yy1, group = SingleTdata$subj, type="l",axes=FALSE,  ## the leading ',' in the next line is necessar to have connected lines!
            ,xlim=range(min(xx1)+(min(xx1)/2),2*max(xx1)),ylim=range(0, 1.5*max(yy1)), 
            xlab=xaxis, ylab=yaxis,cex.lab = 1,x.leg =1000000, bty="l", lty=1,las=1,
            font.lab=2,cex.axis=1,cex.main=1,x.cont=TRUE,xaxt="n",legend=FALSE)
            axis(1, pos=0)
            axis(2, pos=0,las=1) 
   }
   else{
    xx1<-SingleTdata$time
    yy1<-SingleTdata$conc 
    ## show(SingleTdata) 
##
## crached here... one more same place for this ##############################
##
    lineplot.CI(xx1, yy1, group = SingleTdata$subj, type="l", ## the leading ',' in the next line is necessar to have connected lines!
            ,xlim=range(0, max(xx1)*1.25), xlab=xaxis, ylab=yaxis,ylim=c(0,max(yy1)*1.25),cex.lab = 1,x.leg =1000000, bty="l", lty=1,las=1,
            font.lab=2,cex.axis=1,cex.main=1,x.cont=TRUE,xaxt="n",legend=FALSE)
      xtick(xx1) #tick for x-axis
      ytick(yy1) #tick for y-axis
      }
    }         
         mtext("Spaghetti plot - Test Product",side=3,cex=1.2,las=0)  # print this after 'plot'!!
 

#4. ###############################show all subjects in one plot at the same time for ref
### main1<-"Spaghetti plot - Ref. Product"  ### cause err! don't know why? YJ
###
if(replicated){
xx1<-SingleRdata$time
yy1<-SingleRdata$conc  
lineplot.CI(SingleRdata$time, SingleRdata$conc, group = SingleRdata$code, type="l",   ## the leading ',' in the next line is necessar to have connected lines!
            ,xlim=range(xx1, xx1+ (xx1/5)), xlab=xaxis, ylab=yaxis,ylim=c(0,max(Totalplot$conc)),cex.lab = 1,x.leg =100000, bty="l",lty=1,las=1,
            font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n",legend=FALSE)
            xtick(xx1) #tick for x-axis
            ytick(yy1) #tick for y-axis
}
else{
   if(multiple){
   xx1<-SingleRdata$time -TlastD
   yy1<-SingleRdata$conc  
   lineplot.CI(xx1, yy1, group = SingleRdata$subj, type="l",axes=FALSE,  ## the leading ',' in the next line is necessar to have connected lines!
            ,xlim=range(min(xx1)+(min(xx1)/2),2*max(xx1)),ylim=range(0, 1.5*max(yy1)), 
            xlab=xaxis, ylab=yaxis,cex.lab = 1,x.leg =100000, bty="l",lty=1,las=1,
            font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n",legend=FALSE)
            axis(1, pos=0)
            axis(2, pos=0,las=1)
            
   }
   else{
   xx1<-SingleRdata$time
   yy1<-SingleRdata$conc  
   lineplot.CI(SingleRdata$time, SingleRdata$conc, group = SingleRdata$subj, type="l",  ## the leading ',' in the next line is necessar to have connected lines!
            ,xlim=range(0, xx1+ (xx1/5)), xlab=xaxis, ylab=yaxis,ylim=c(0,max(Totalplot$conc)),cex.lab = 1,x.leg =100000, bty="l",lty=1,las=1,
            font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n",legend=FALSE)
             xtick(xx1) #tick for x-axis
             ytick(yy1) #tick for y-axis
  }
}             
             
            mtext("Spaghetti plot - Ref. Product",side=3,cex=1.2,las=0)  ## must be after plot


#5. ########################## mean+*-sd plot for test and ref drug  ## lineplot.CI() is mean+/- se (not sd!) --YJ
### semilog plots here and followed by linear plots  -YJ

if(replicated){
main1<-paste(c("Obs. Mean Drug Plasma Conc. (in log10 scale), N=",L1+L2),collapse=" ")
main2<-paste(c("Obs. Mean Drug Plasma Conc. (in log10 scale with SD), N=",L1+L2),collapse=" ")
xx1<-Totalplot$time
yy1<-Totalplot$conc
yy1<-na.omit(yy1)

##        
## shrink the error bar to none; set width of whiskers = 0 && erro.col="white"; not to show error bar with this plot
## 
lineplot.CI(xx1, yy1, group = Totalplot$drug, cex = 1, main=main1,log="y",  ## y in log scale; don't set any xlim or ylim! for replicate
            xlab=xaxis, ylab=yaxis,cex.lab = 1,x.leg =100000, bty="l", las=1,
            font.lab=2,cex.axis=1,cex.main=1,x.cont=TRUE,xaxt="n" ,err.lty=1,err.col="white",err.width=0.,legend=FALSE
             )
            xtick(xx1) #tick for x-axis
            ytick(yy1) #tick for y-axis

            ## add legned here
            temp <- legend("topright",legend = c("Test", "Ref."),
               text.width = strwidth("1000"),
               lty = 1:2, xjust = 1, yjust = 1) 
###
### show error bar with whiscker bar
###               
lineplot.CI(xx1, yy1, group = Totalplot$drug, cex = 1, main=main2,log="y",  ## y in log scale; don't set any xlim or ylim! for replicate
            xlab=xaxis, ylab=yaxis,cex.lab = 1,x.leg =100000, bty="l", las=1,
            font.lab=2,cex.axis=1,cex.main=1,x.cont=TRUE,xaxt="n" ,err.lty=1,err.width=0.05,legend=FALSE
             )
            xtick(xx1) #tick for x-axis
            ytick(yy1) #tick for y-axis
}
else{
 if(parallel){
 main1<-paste(c("Obs. Mean Drug Plasma Conc. (in log10 Scale, N =",L1+L2),collapse=" ")
 main2<-paste(c("Obs. Mean Drug Plasma Conc. (in log10 Scalewith SD), N =",L1+L2),collapse=" ")
    if(multiple){                       ### very special case with this; don't work; data caused? so plot as linear (not semilog plot!  YJ)
    xx1<-Totalplot$time -TlastD
    yy1<-Totalplot$conc
    yy1<-na.omit(yy1)
    Ymax=max(yy1)
    Xmax=max(xx1)
##        
## shrink the error bar to none; set width of whiskers = 0 && erro.col="white"; not to show error bar with this plot
##     
    lineplot.CI(xx1, yy1, group = Totalplot$drug, cex = 1, main=main1, log="y",  ## sometimes don't plot at all for this; don't know why (YJ)
            axes=FALSE, xlim=range(min(xx1)+(min(xx1)/2),2*max(xx1)),ylim=range(0, 0.9*max(yy1)),
            xlab=xaxis, ylab=yaxis,cex.lab = 1,x.leg =100000, bty="l", las=1,
            font.lab=2,cex.axis=1,cex.main=1,x.cont=TRUE,xaxt="n" ,err.lty=1,err.col="white",err.width=0.,legend=FALSE)
            axis(1, pos=0)
            axis(2, pos=0,las=1)
                                     
            ## add legend here
            temp <- legend("topleft",legend = c("Test", "Ref."),
               text.width = strwidth("1000"),
               lty = 1:2, xjust = 1, yjust = 1) 
               
    lineplot.CI(xx1, yy1, group = Totalplot$drug, cex = 1, main=main2, log="y",  ## sometimes don't plot at all for this; don't know why (YJ)
            axes=FALSE, xlim=range(min(xx1)+(min(xx1)/2),2*max(xx1)),ylim=range(0, 0.9*max(yy1)),
            xlab=xaxis, ylab=yaxis,cex.lab = 1,x.leg =100000, bty="l", las=1,
            font.lab=2,cex.axis=1,cex.main=1,x.cont=TRUE,xaxt="n" ,err.lty=1,err.width=0.05,legend=FALSE)
            axis(1, pos=0)
            axis(2, pos=0,las=1)
                }
    else{
    xx1<-Totalplot$time
    yy1<-Totalplot$conc
    yy1<-na.omit(yy1)

    ##        
    ## shrink the error bar to none; set width of whiskers = 0 && erro.col="white"; not to show error bar with this plot
    ## 
    lineplot.CI(xx1, yy1, group = Totalplot$drug, cex = 1, main=main1,log="y",  ## y in log scale; don't set any xlim or ylim!
            xlab=xaxis, ylab=yaxis,cex.lab = 1,x.leg =100000, bty="l", las=1,
            font.lab=2,cex.axis=1,cex.main=1,x.cont=TRUE,xaxt="n" ,err.lty=1,err.col="white",err.width=0.,legend=FALSE
             )
            xtick(xx1) #tick for x-axis
            ytick(yy1) #tick for y-axis
            
            ## add legend here
            temp <- legend("topright",legend = c("Test", "Ref."),
               text.width = strwidth("1000"),
               lty = 1:2, xjust = 1, yjust = 1) 
              
    lineplot.CI(xx1, yy1, group = Totalplot$drug, cex = 1, main=main2,log="y",  ## y in log scale; don't set any xlim or ylim!
            xlab=xaxis, ylab=yaxis,cex.lab = 1,x.leg =100000, bty="l", las=1,
            font.lab=2,cex.axis=1,cex.main=1,x.cont=TRUE,xaxt="n" ,err.lty=1,err.width=0.05,legend=FALSE
             )
            xtick(xx1) #tick for x-axis
            ytick(yy1) #tick for y-axis
    }
  }
 else{
  main1<-paste(c("Obs. Mean Drug Plasma Conc.(in log10 scale), N =",L),collapse=" ")
  main2<-paste(c("Obs. Mean Drug Plasma Conc. (in log10 scale, with SD), N =",L),collapse=" ")
    if(multiple){
       Totalplot[Totalplot$conc<1] <-NA
       xx1<-Totalplot$time-TlastD 
       yy1<-Totalplot$conc
       yy1<-na.omit(yy1)
       
    ##        
    ## shrink the error bar to none; set width of whiskers = 0 && erro.col="white"; not to show error bar with this plot
    ## 
    lineplot.CI(xx1, yy1, group = Totalplot$drug, cex = 1, main=main1,log="y",  ## y in log scale; don't set any xlim or ylim!
            axes=FALSE, ## xlim=range(min(xx1)+(min(xx1)/2),2*max(xx1)),ylim=range(0, max(yy1)), 
            xlab=xaxis, ylab=yaxis,cex.lab = 1,x.leg =100000, bty="l", las=1,
            font.lab=2,cex.axis=1,cex.main=1,x.cont=TRUE,xaxt="n",err.lty=1,err.col="white",err.width=0.,
            legend=FALSE)
            axis(1, pos=0)
            axis(2, pos=0,las=1)
            xtick(xx1) #tick for x-axis
            ## ytick(yy1) #tick for y-axis  ## don't need this for multiple; otherwise, ugly...

            ### add legend here
            temp <- legend("topleft",legend = c("Test", "Ref."),
               text.width = strwidth("1000"),
               lty = 1:2, xjust = 1, yjust = 1)
               
    lineplot.CI(xx1, yy1, group = Totalplot$drug, cex = 1, main=main2,log="y",  ## y in log scale; don't set any xlim or ylim!
            axes=FALSE, ## xlim=range(min(xx1)+(min(xx1)/2), 2*max(xx1)), ylim=range(0, max(yy1)), 
            xlab=xaxis, ylab=yaxis,cex.lab = 1,x.leg =100000, bty="l", las=1,
            font.lab=2,cex.axis=1,cex.main=1,x.cont=TRUE,xaxt="n",err.lty=1,err.width=0.05,
            legend=FALSE)
            axis(1, pos=0)
            axis(2, pos=0,las=1) 
            xtick(xx1) #tick for x-axis
            ## ytick(yy1) #tick for y-axis ## don't need this for multiple; otherwise, ugly...
    }
    else{
    xx1<-Totalplot$time
    yy1<-Totalplot$conc
    yy1<-na.omit(yy1)
    
    ##        
    ## shrink the error bar to none; set width of whiskers = 0 && erro.col="white"; not to show error bar with this plot
    ## 
    lineplot.CI(xx1, yy1, group = Totalplot$drug, cex = 1, main=main1,log="y",  ## y in log scale; don't set any xlim or ylim!
            xlab=xaxis, ylab=yaxis,cex.lab = 1,x.leg =100000, bty="l", las=1,
            font.lab=2,cex.axis=1,cex.main=1,x.cont=TRUE,xaxt="n",err.lty=1,err.col="white", err.width=0.,
            legend=FALSE)
            xtick(xx1) #tick for x-axis
            ytick(yy1) #tick for y-axis

            ### add legend here ...YJ  
            temp <- legend("topright",legend = c("Test", "Ref."),
               text.width = strwidth("1000"),
               lty = 1:2, xjust = 1, yjust = 1) 
    ##        
    ## show the error bar with this plot
    ## 
    lineplot.CI(xx1, yy1, group = Totalplot$drug, cex = 1, main=main2,log="y",  ## y in log scale; don't set any xlim or ylim!
            xlab=xaxis, ylab=yaxis,cex.lab = 1,x.leg =100000, bty="l", las=1,
            font.lab=2,cex.axis=1,cex.main=1,x.cont=TRUE,xaxt="n" ,err.lty=1,err.width=0.05,legend=FALSE
             )
            xtick(xx1) #tick for x-axis
            ytick(yy1) #tick for y-axis

    }   
   }
 }

    if(multiple){
             temp <- legend("topleft",legend = c("Test", "Ref."),
               text.width = strwidth("1000"),
               lty = 1:2, xjust = 1, yjust = 1)
            }
    else{
             temp <- legend("topright",legend = c("Test", "Ref."),
               text.width = strwidth("1000"),
               lty = 1:2, xjust = 1, yjust = 1) 
       }       
###
### linear plots from here
###

if(replicated){
main1<-paste(c("Obs. Mean Drug Plasma Conc., N=",L1+L2),collapse=" ")
main2<-paste(c("Obs. Mean Drug Plasma Conc. (with SD), N=",L1+L2),collapse=" ")
xx1<-Totalplot$time
yy1<-Totalplot$conc
yy1<-na.omit(yy1)

##        
## shrink the error bar to none; set width of whiskers = 0 && erro.col="white"; not to show error bar with this plot
## 
lineplot.CI(xx1, yy1, group = Totalplot$drug, cex = 1, main=main1, ## log="y",  ## y in log scale; don't set any xlim or ylim! for replicate
            xlab=xaxis, ylab=yaxis,cex.lab = 1,x.leg =100000, bty="l", las=1,
            font.lab=2,cex.axis=1,cex.main=1,x.cont=TRUE,xaxt="n" ,err.lty=1,err.col="white",err.width=0.,legend=FALSE
             )
            xtick(xx1) #tick for x-axis
            ytick(yy1) #tick for y-axis

            ## add legned here
            temp <- legend("topright",legend = c("Test", "Ref."),
               text.width = strwidth("1000"),
               lty = 1:2, xjust = 1, yjust = 1) 
###
### show error bar with whiscker bar
###               
lineplot.CI(xx1, yy1, group = Totalplot$drug, cex = 1, main=main2, ## log="y",  ## y in log scale; don't set any xlim or ylim! for replicate
            xlab=xaxis, ylab=yaxis,cex.lab = 1,x.leg =100000, bty="l", las=1,
            font.lab=2,cex.axis=1,cex.main=1,x.cont=TRUE,xaxt="n" ,err.lty=1,err.width=0.05,legend=FALSE
             )
            xtick(xx1) #tick for x-axis
            ytick(yy1) #tick for y-axis
}
else{
 if(parallel){
 main1<-paste(c("Obs. Mean Drug Plasma Conc., N =",L1+L2),collapse=" ")
 main2<-paste(c("Obs. Mean Drug Plasma Conc.(with SD), N =",L1+L2),collapse=" ")
    if(multiple){                       ### very special case with this; don't work; data caused? so plot as linear (not semilog plot!  YJ)
    xx1<-Totalplot$time -TlastD
    yy1<-Totalplot$conc
    yy1<-na.omit(yy1)
    Ymax=max(yy1)
    Xmax=max(xx1)
##        
## shrink the error bar to none; set width of whiskers = 0 && erro.col="white"; not to show error bar with this plot
##     
    lineplot.CI(xx1, yy1, group = Totalplot$drug, cex = 1, main=main1, ## log="y",  ## don't plot at all for this; don't know why (YJ)
            axes=FALSE, xlim=range(min(xx1)+(min(xx1)/2),2*max(xx1)),ylim=range(0, 0.9*max(yy1)),
            xlab=xaxis, ylab=yaxis,cex.lab = 1,x.leg =100000, bty="l", las=1,
            font.lab=2,cex.axis=1,cex.main=1,x.cont=TRUE,xaxt="n" ,err.lty=1,err.col="white",err.width=0.,legend=FALSE)
            axis(1, pos=0)
            axis(2, pos=0,las=1)
                                     
            ## add legend here
            temp <- legend("topleft",legend = c("Test", "Ref."),
               text.width = strwidth("1000"),
               lty = 1:2, xjust = 1, yjust = 1) 
               
    lineplot.CI(xx1, yy1, group = Totalplot$drug, cex = 1, main=main2,## log="y",  ## don't plot at all for this; don't know why (YJ)
            axes=FALSE, xlim=range(min(xx1)+(min(xx1)/2),2*max(xx1)),ylim=range(0, 0.9*max(yy1)),
            xlab=xaxis, ylab=yaxis,cex.lab = 1,x.leg =100000, bty="l", las=1,
            font.lab=2,cex.axis=1,cex.main=1,x.cont=TRUE,xaxt="n" ,err.lty=1,err.width=0.05,legend=FALSE)
            axis(1, pos=0)
            axis(2, pos=0,las=1)
                }
    else{
    xx1<-Totalplot$time
    yy1<-Totalplot$conc
    yy1<-na.omit(yy1)

    ##        
    ## shrink the error bar to none; set width of whiskers = 0 && erro.col="white"; not to show error bar with this plot
    ## 
    lineplot.CI(xx1, yy1, group = Totalplot$drug, cex = 1, main=main1, ## log="y",  ## y in log scale; don't set any xlim or ylim!
            xlab=xaxis, ylab=yaxis,cex.lab = 1,x.leg =100000, bty="l", las=1,
            font.lab=2,cex.axis=1,cex.main=1,x.cont=TRUE,xaxt="n" ,err.lty=1,err.col="white",err.width=0.,legend=FALSE
             )
            xtick(xx1) #tick for x-axis
            ytick(yy1) #tick for y-axis
            
            ## add legend here
            temp <- legend("topright",legend = c("Test", "Ref."),
               text.width = strwidth("1000"),
               lty = 1:2, xjust = 1, yjust = 1) 
              
    lineplot.CI(xx1, yy1, group = Totalplot$drug, cex = 1, main=main2, ## log="y",  ## y in log scale; don't set any xlim or ylim!
            xlab=xaxis, ylab=yaxis,cex.lab = 1,x.leg =100000, bty="l", las=1,
            font.lab=2,cex.axis=1,cex.main=1,x.cont=TRUE,xaxt="n" ,err.lty=1,err.width=0.05,legend=FALSE
             )
            xtick(xx1) #tick for x-axis
            ytick(yy1) #tick for y-axis
    }
  }
 else{
  main1<-paste(c("Obs. Mean Drug Plasma Conc., N =",L),collapse=" ")
  main2<-paste(c("Obs. Mean Drug Plasma Conc. (with SD), N =",L),collapse=" ")
    if(multiple){
       Totalplot[Totalplot$conc<1] <-NA
       xx1<-Totalplot$time-TlastD 
       yy1<-Totalplot$conc
       yy1<-na.omit(yy1)
       
    ##        
    ## shrink the error bar to none; set width of whiskers = 0 && erro.col="white"; not to show error bar with this plot
    ## 
    lineplot.CI(xx1, yy1, group = Totalplot$drug, cex = 1, main=main1, ## log="y",  ## y in log scale; don't set any xlim or ylim!
            axes=FALSE, ## xlim=range(min(xx1)+(min(xx1)/2),2*max(xx1)),ylim=range(0, max(yy1)), 
            xlab=xaxis, ylab=yaxis,cex.lab = 1,x.leg =100000, bty="l", las=1,
            font.lab=2,cex.axis=1,cex.main=1,x.cont=TRUE,xaxt="n",err.lty=1,err.col="white",err.width=0.,
            legend=FALSE)
            axis(1, pos=0)
            axis(2, pos=0,las=1)
            ## xtick(xx1) #tick for x-axis
            ## ytick(yy1) #tick for y-axis  ## don't need this for multiple; otherwise, ugly...

            ### add legend here
            temp <- legend("topleft",legend = c("Test", "Ref."),
               text.width = strwidth("1000"),
               lty = 1:2, xjust = 1, yjust = 1)
               
    lineplot.CI(xx1, yy1, group = Totalplot$drug, cex = 1, main=main2, ## log="y",  ## y in log scale; don't set any xlim or ylim!
            axes=FALSE, ## xlim=range(min(xx1)+(min(xx1)/2), 2*max(xx1)), ylim=range(0, max(yy1)), 
            xlab=xaxis, ylab=yaxis,cex.lab = 1,x.leg =100000, bty="l", las=1,
            font.lab=2,cex.axis=1,cex.main=1,x.cont=TRUE,xaxt="n",err.lty=1,err.width=0.05,
            legend=FALSE)
            axis(1, pos=0)
            axis(2, pos=0,las=1) 
            ## xtick(xx1) #tick for x-axis
            ## ytick(yy1) #tick for y-axis ## don't need this for multiple; otherwise, ugly...
    }
    else{
    xx1<-Totalplot$time
    yy1<-Totalplot$conc
    yy1<-na.omit(yy1)
    
    ##        
    ## shrink the error bar to none; set width of whiskers = 0 && erro.col="white"; not to show error bar with this plot
    ## 
    lineplot.CI(xx1, yy1, group = Totalplot$drug, cex = 1, main=main1, ## log="y",  ## y in log scale; don't set any xlim or ylim!
            xlab=xaxis, ylab=yaxis,cex.lab = 1,x.leg =100000, bty="l", las=1,
            font.lab=2,cex.axis=1,cex.main=1,x.cont=TRUE,xaxt="n",err.lty=1,err.col="white", err.width=0.,
            legend=FALSE)
            xtick(xx1) #tick for x-axis
            ytick(yy1) #tick for y-axis

            ### add legend here ...YJ  
            temp <- legend("topright",legend = c("Test", "Ref."),
               text.width = strwidth("1000"),
               lty = 1:2, xjust = 1, yjust = 1) 
    ##        
    ## show the error bar with this plot
    ## 
    lineplot.CI(xx1, yy1, group = Totalplot$drug, cex = 1, main=main2, ## log="y",  ## y in log scale; don't set any xlim or ylim!
            xlab=xaxis, ylab=yaxis,cex.lab = 1,x.leg =100000, bty="l", las=1,
            font.lab=2,cex.axis=1,cex.main=1,x.cont=TRUE,xaxt="n" ,err.lty=1,err.width=0.05,legend=FALSE
             )
            xtick(xx1) #tick for x-axis
            ytick(yy1) #tick for y-axis

    }   
   }
 }

    if(multiple){
             temp <- legend("topleft",legend = c("Test", "Ref."),
               text.width = strwidth("1000"),
               lty = 1:2, xjust = 1, yjust = 1)
            }
    else{
             temp <- legend("topright",legend = c("Test", "Ref."),
               text.width = strwidth("1000"),
               lty = 1:2, xjust = 1, yjust = 1) 
       }

dev.off()

}