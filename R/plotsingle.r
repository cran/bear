#subject plot for individual person for BA and ln(BA) -->crossover study
plotsingle<-function(LR.split,LT.split,xaxis,yaxis,R.split,T.split,RR.split,TT.split,TlastD,multiple=FALSE )
{
#for ln (BA)
 for(i in seq_along(LT.split)){
     if(multiple){
     xx1<-LR.split[[i]]$time-TlastD
     yy1<-LR.split[[i]]$conc

     xx2<-LT.split[[i]]$time-TlastD
     yy2<-LT.split[[i]]$conc

     xx3<-min(xx1,xx2)
     xx4<-max(xx1,xx2)
     yy3<-max(yy1,yy2)
        main<-paste(c("Subj. #", LT.split[[i]]$subj[1]),collapse=" ")
        plot(0, 0, log="y", axes=FALSE,xlim=range(xx3+(xx3/2), 2*xx4), ylim=range(1, 4*max(yy3)), cex = 1,
                  xlab = "Time", ylab = "Conc. (in log scale)",cex.lab = 1, x.leg = 100000, bty="l",main=main,
                  font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n")
         points(xx1,yy1,pch=19,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
         points(xx2,yy2,pch=1,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
         lines(xx1,yy1, lty=20)
         lines(xx2,yy2, lwd=1)
         axis(1, pos=1)
         axis(2, pos=0,las=1)         
        temp <- legend("topleft", legend = c("Test", "Ref."),
               text.width = strwidth("1000"),
               lty = 1:2, xjust = 1, yjust = 1)
     }
     else{
     xx1<-LR.split[[i]]$time
     yy1<-LR.split[[i]]$conc

     xx2<-LT.split[[i]]$time
     yy2<-LT.split[[i]]$conc

        main<-paste(c("Subj. #", LT.split[[i]]$subj[1]),collapse=" ")
        plot(0, 0, log="y",xlim=range(xx1, xx1+ (xx1/5)), ylim=range(yy1,yy2), cex = 1,
                  xlab = "Time", ylab = "Conc. (in log scale)",cex.lab = 1, x.leg = 100000, bty="l",main=main,
                  font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n")
         points(xx1,yy1,pch=19,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
         points(xx2,yy2,pch=1,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
         lines(xx1,yy1, lty=20)
         lines(xx2,yy2, lwd=1)

        xtick(xx1) #tick for x-axis
        
        temp <- legend("topright", legend = c("Test", "Ref."),
               text.width = strwidth("1000"),
               lty = 1:2, xjust = 1, yjust = 1)
       }
      }
 #for(BA)
  if(multiple){
   for(i in seq_along(TT.split)){
      
       xx1<-RR.split[[i]]$time-TlastD
       yy1<-RR.split[[i]]$conc

       xx2<-TT.split[[i]]$time-TlastD
       yy2<-TT.split[[i]]$conc
       
       xx3<-min(xx1,xx2)
       xx4<-max(xx1,xx2)
       yy3<-max(yy1,yy2)
         main<-paste(c("Subj. #", TT.split[[i]]$subj[1]),collapse=" ")
         plot(0, 0, axes=FALSE,xlim=range(xx3+(xx3/2), 2*xx4), ylim=range(0, max(yy3)+(max(yy3)/2)), cex = 1,
                 xlab=xaxis, ylab=yaxis,cex.lab = 1, x.leg = 100000, bty="l",main=main,
                 font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n")  
          points(xx1,yy1,pch=19,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
          points(xx2,yy2,pch=1,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
          lines(xx1,yy1, lty=20)
          lines(xx2,yy2, lwd=1)
          axis(1, pos=0)
          axis(2, pos=0,las=1)       
         
         temp <- legend("topleft", legend = c("Test", "Ref."),
               text.width = strwidth("1000"),
               lty = 1:2, xjust = 1, yjust = 1)
       }
     }  
     else{
       for(i in seq_along(T.split)){
       xx1<-R.split[[i]]$time
       yy1<-R.split[[i]]$conc

       xx2<-T.split[[i]]$time
       yy2<-T.split[[i]]$conc

         main<-paste(c("Subj. #", T.split[[i]]$subj[1]),collapse=" ")
         plot(0, 0, xlim=range(xx1, xx1+ (xx1/5)), ylim=range(yy1,yy2), cex = 1,
                 xlab=xaxis, ylab=yaxis,cex.lab = 1, x.leg = 100000, bty="l",main=main,
                 font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n")  
          points(xx1,yy1,pch=19,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
          points(xx2,yy2,pch=1,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
          lines(xx1,yy1, lty=20)
          lines(xx2,yy2, lwd=1)
         xtick(xx1) #tick for x-axis
         ytick(yy1) #tick for y-axis
         
         temp <- legend("topright", legend = c("Test", "Ref."),
               text.width = strwidth("1000"),
               lty = 1:2, xjust = 1, yjust = 1)
        
      }
     }
  }
  
  

 