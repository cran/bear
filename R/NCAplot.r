##export with pdf file
library(sciplot)
NCAplot<-function(Totalplot,SingleRdata,SingleTdata,TotalData,xaxis,yaxis,
                  replicated=FALSE,parallel=FALSE)
{
options(warn=-1)

Totalplot<-Totalplot[ do.call(order, Totalplot) ,]
s.split<-split(Totalplot,list(Totalplot$subj))

R.split<-split(SingleRdata, list(SingleRdata$subj))
T.split<-split(SingleTdata, list(SingleTdata$subj))

Totalplot$conc[Totalplot$conc == 0] <- NA
Totalplot <- na.omit(Totalplot)

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
        
        paraR.split<-split( Totalplotpara[[1]], list(Totalplotpara[[1]]$subj))
        paraT.split<-split( Totalplotpara[[2]], list(Totalplotpara[[2]]$subj))
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
 
pdf("NCAplots.pdf")

#0.
description_plot()
par(mai=c(1.3,2,1.3,1.8)) 
#1.#################################individual subject for test vs ref (Cp vs time)
if(parallel){
#ref.  
for(i in seq_along(R.split)){
     xx1<-R.split[[i]]$time
     yy1<-R.split[[i]]$conc     
     main<-paste(c("Subj. #", R.split[[i]]$subj[1]),collapse=" ")
        plot(0, 0, xlim=range(xx1), ylim=range(yy1), cex = 1,
                 xlab=xaxis, ylab=yaxis,cex.lab = 1, x.leg = 100000, bty="l",main=main,
                 font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n")  
        lines(xx1,yy1, lty=2)
        points(xx1,yy1,pch=19,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
         xtick(xx1) #tick for x-axis
         ytick(yy1) #tick for y-axis
        temp <- legend("topright", legend = c("Ref."),
               text.width = strwidth("1,000,000"),
               lty = 2, xjust = 1, yjust = 1)
      }                                                     
#test
for(i in seq_along(T.split)){
     xx1<-T.split[[i]]$time
     yy1<-T.split[[i]]$conc     
     main<-paste(c("Subj. #", T.split[[i]]$subj[1]),collapse=" ")
        plot(0, 0, xlim=range(xx1), ylim=range(yy1), xcex = 1,
                 xlab=xaxis, ylab=yaxis,cex.lab = 1, x.leg = 100000, bty="l",main=main,
                 font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n")  
        lines(xx1,yy1, lty=1)
        points(xx1,yy1,pch=1,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
         xtick(xx1) #tick for x-axis
         ytick(yy1) #tick for y-axis
        temp <- legend("topright", legend = c("Test"),
               text.width = strwidth("1,000,000"),
               lty = 1, xjust = 1, yjust = 1)
      }                                     
}
else{
for(i in seq_along(s.split)){
     xx1<-s.split[[i]]$time
     yy1<-s.split[[i]]$conc     
    main<-paste(c("Subject#", s.split[[i]]$subj[1]),collapse=" ")
    if(replicated){
     lineplot.CI(s.split[[i]]$time, s.split[[i]]$conc, group = s.split[[i]]$code, cex = 1,
                 xlab=xaxis, ylab=yaxis,cex.lab = 1, x.leg = 100000, bty="l",main=main,
                 font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n")  
      }
      else{
        lineplot.CI(s.split[[i]]$time, s.split[[i]]$conc, group = s.split[[i]]$drug, cex = 1,
            xlab=xaxis, ylab=yaxis,cex.lab = 1, x.leg = 100000, bty="l",main=main,
            font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n"
             )
 }            
  xtick(xx1) #tick for x-axis
  ytick(yy1) #tick for y-axis
if(replicated){
  prdcount(i,s.split, prdcount)
 }
else{
 temp <- legend("topright",legend = c("Test", "Ref"),
               text.width = strwidth("1,000,000"),
               lty = 1:2, xjust = 1, yjust = 1)
   }
  } 
}
#2.#####################################individual subject for test vs ref (lnCp vs time)
if(parallel){
 #Ref.
 for(i in seq_along(paraR.split)){
     xx1<-paraR.split[[i]]$time
     yy1<-paraR.split[[i]]$conc     
     main<-paste(c("Subj. #", paraR.split[[i]]$subj[1]),collapse=" ")
        plot(0, 0, log="y",xlim=range(xx1), ylim=range(yy1), cex = 1,
                  xlab = "Time", ylab = "Conc. (in log scale)",cex.lab = 1, x.leg = 100000, bty="l",main=main,
                  font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n")

        points(xx1,yy1,pch=19,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
        lines(xx1,yy1, lty=2)
         xtick(xx1) #tick for x-axis
        
        temp <- legend("topright", legend = c("Ref."),
               text.width = strwidth("1,000,000"),
               lty = 2, xjust = 1, yjust = 1)
      }                                                     
#Test
for(i in seq_along(paraT.split)){
     xx1<-paraT.split[[i]]$time
     yy1<-paraT.split[[i]]$conc     
     main<-paste(c("Subj. #", paraT.split[[i]]$subj[1]),collapse=" ")
        plot(0, 0, log="y",xlim=range(xx1), ylim=range(yy1), cex = 1,
                  xlab = "Time", ylab = "Conc. (in log scale)",cex.lab = 1, x.leg = 100000, bty="l",main=main,
                  font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n")
        points(xx1,yy1,pch=1,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
        lines(xx1,yy1, lty=1)
         xtick(xx1) #tick for x-axis
        
        temp <- legend("topright", legend = c("Test"),
               text.width = strwidth("1,000,000"),
               lty = 1, xjust = 1, yjust = 1)
      }                                            
}
else{
 for(i in seq_along(Ls.split)){
  xx1<-Ls.split[[i]]$time
  yy1<-Ls.split[[i]]$conc   
  main<-paste(c("Subject#", Ls.split[[i]]$subj[1]),collapse=" ")
   if(replicated){
      lineplot.CI(Ls.split[[i]]$time, Ls.split[[i]]$conc, log="y", group = Ls.split[[i]]$code, cex = 1,
                  xlab = "Time", ylab = "Conc. (in log scale)",cex.lab = 1, x.leg = 100000, bty="l",main=main,
                  font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n")
    }
    else{
      lineplot.CI(Ls.split[[i]]$time, Ls.split[[i]]$conc, log="y", group = Ls.split[[i]]$drug, cex = 1,
          xlab = "Time", ylab = "Conc. (in log scale)",cex.lab = 1, x.leg = 100000, bty="l",main=main,
          font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n"
             )
      } 
      xtick(xx1) #tick for x-axis

    if(replicated){
      prdcount(i,s.split, prdcount)
     }
    else{
      temp <- legend("topright",legend = c("Test", "Ref"),
               text.width = strwidth("1,000,000"),
               lty = 1:2, xjust = 1, yjust = 1)
  }
 }
}
#3. ###############################show all subjects in one plot at the same time for test
if(replicated){
xx1<-SingleTdata$time
yy1<-SingleTdata$conc  
lineplot.CI(SingleTdata$time, SingleTdata$conc, group = SingleTdata$code, type="l",
           xlab=xaxis, ylab=yaxis,ylim=c(0,max(Totalplot$conc)),cex.lab = 1,x.leg =1000000, bty="l", lty=1,las=1,
            font.lab=2,cex.axis=1,cex.main=1,x.cont=TRUE,xaxt="n" 
             )
}
else{
xx1<-SingleTdata$time
yy1<-SingleTdata$conc  
lineplot.CI(SingleTdata$time, SingleTdata$conc, group = SingleTdata$subj, type="l",
           xlab=xaxis, ylab=yaxis,ylim=c(0,max(Totalplot$conc)),cex.lab = 1,x.leg =1000000, bty="l", lty=1,las=1,
            font.lab=2,cex.axis=1,cex.main=1,x.cont=TRUE,xaxt="n" 
             )
    }         
            xtick(xx1) #tick for x-axis
            ytick(yy1) #tick for y-axis
            mtext("Test",side=3,cex=2,las=0)  #�n��bplot����
 

#4. ###############################show all subjects in one plot at the same time for ref
if(replicated){
xx1<-SingleRdata$time
yy1<-SingleRdata$conc  
lineplot.CI(SingleRdata$time, SingleRdata$conc, group = SingleRdata$code, type="l",
            xlab=xaxis, ylab=yaxis,ylim=c(0,max(Totalplot$conc)),cex.lab = 1,x.leg =100000, bty="l",lty=1,las=1,
            font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n" 
             )
}
else{
xx1<-SingleRdata$time
yy1<-SingleRdata$conc  
lineplot.CI(SingleRdata$time, SingleRdata$conc, group = SingleRdata$subj, type="l",
            xlab=xaxis, ylab=yaxis,ylim=c(0,max(Totalplot$conc)),cex.lab = 1,x.leg =100000, bty="l",lty=1,las=1,
            font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n" 
             )
}             
             xtick(xx1) #tick for x-axis
             ytick(yy1) #tick for y-axis
            mtext("Ref.",side=3,cex=2,las=0)  #�n��bplot����


#5. ##########################mean+*-sd plot for test and ref drug

if(replicated){
main<-paste(c("Observed mean drug plasma concentration, N=",L1+L2),collapse=" ")
}
else{
 if(parallel){
 main<-paste(c("Observed mean drug plasma concentration, N=",L1+L2),collapse=" ")
 }
 else{
   main<-paste(c("Observed mean drug plasma concentration, N=",L),collapse=" ")
   }
 }
xx1<-Totalplot$time
yy1<-Totalplot$conc  
lineplot.CI(Totalplot$time, Totalplot$conc, group = Totalplot$drug, cex = 1, main=main,
            xlab=xaxis, ylab=yaxis,cex.lab = 1,x.leg =100000, bty="l", las=1,
            font.lab=2,cex.axis=1,cex.main=1,x.cont=TRUE,xaxt="n" ,err.lty=1,err.width=0.05
             )
            xtick(xx1) #tick for x-axis
            ytick(yy1) #tick for y-axis
temp <- legend("topright",legend = c("Test", "Ref."),
               text.width = strwidth("1,000,000"),
               lty = 1:2, xjust = 1, yjust = 1)


dev.off()

}