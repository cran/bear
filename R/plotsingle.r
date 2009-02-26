#subject plot for individual person for BA and ln(BA) -->crossover study
plotsingle<-function(LR.split,LT.split,R.split,T.split,xaxis,yaxis )
{
#for ln (BA)
 for(i in seq_along(LT.split)){
     xx1<-LR.split[[i]]$time
     yy1<-LR.split[[i]]$conc

     xx2<-LT.split[[i]]$time
     yy2<-LT.split[[i]]$conc

        main<-paste(c("Subj. #", LT.split[[i]]$subj[1]),collapse=" ")
        plot(0, 0, log="y",xlim=range(xx1), ylim=range(yy1,yy2), cex = 1,
                  xlab = "Time", ylab = "Conc. (in log scale)",cex.lab = 1, x.leg = 100000, bty="l",main=main,
                  font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n")

        points(xx1,yy1,pch=19,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
        points(xx2,yy2,pch=1,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)

        lines(xx1,yy1, lty=20)
        lines(xx2,yy2, lwd=1)

        axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,
              105,110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,190,195,200),las=0)
        axis(1,at=0:100,tcl=-.2, labels=FALSE)
        temp <- legend("topright", legend = c("Test", "Ref."),
               text.width = strwidth("1,000,000"),
               lty = 1:2, xjust = 1, yjust = 1)
      }
 #for(BA)
   for(i in seq_along(T.split)){
       xx1<-R.split[[i]]$time
       yy1<-R.split[[i]]$conc

       xx2<-T.split[[i]]$time
       yy2<-T.split[[i]]$conc

         main<-paste(c("Subj. #", T.split[[i]]$subj[1]),collapse=" ")
         plot(0, 0, xlim=range(xx1), ylim=range(yy1,yy2), cex = 1,
                 xlab=xaxis, ylab=yaxis,cex.lab = 1, x.leg = 100000, bty="l",main=main,
                 font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n")  

         points(xx1,yy1,pch=19,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)
         points(xx2,yy2,pch=1,bty="l",font.lab=2,cex.lab=1,cex.axis=1,cex.main=1)

         lines(xx1,yy1, lty=20)
         lines(xx2,yy2, lwd=1)

         axis(1,at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,
              105,110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,190,195,200),las=0)
         axis(1,at=0:100,tcl=-.2, labels=FALSE)
         axis(2,yaxp=c(0, 10000, 100),las=1,tcl=-.2, labels=FALSE)
         temp <- legend("topright", legend = c("Test", "Ref."),
               text.width = strwidth("1,000,000"),
               lty = 1:2, xjust = 1, yjust = 1)
     }
  }