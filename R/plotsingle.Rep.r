#subject plot for individual person for BA and ln(BA) -->replicated study
plotsingle.Rep<-function(Ls.split, s.split,xaxis,yaxis,i, prdcount )
{
#ln(BA)
 for(i in seq_along(Ls.split)){
  xx1<-Ls.split[[i]]$time
  yy1<-Ls.split[[i]]$conc
  main<-paste(c("Subject#", Ls.split[[i]]$subj[1]),collapse=" ")
      lineplot.CI(Ls.split[[i]]$time, Ls.split[[i]]$conc, log="y", group = Ls.split[[i]]$code, cex = 1,
                  xlab = "Time", ylab = "Conc. (in log scale)",cex.lab = 1, x.leg = 100000, bty="l",main=main,
                  font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n")
     #  points(Ls.split[[i]]$time,  Ls.split[[i]]$conc ,pch = c(1,19))[as.numeric(f)]
   xtick(xx1) #tick for x-axis
   prdcount(i,s.split, prdcount)
  }
 #BA
 for(i in seq_along(s.split)){
 xx1<-s.split[[i]]$time
 yy1<-s.split[[i]]$conc
 main<-paste(c("Subject#", s.split[[i]]$subj[1]),collapse=" ")
     lineplot.CI(s.split[[i]]$time, s.split[[i]]$conc, group = s.split[[i]]$code, cex = 1,
                 xlab=xaxis, ylab=yaxis,cex.lab = 1, x.leg = 100000, bty="l",main=main,
                 font.lab=2,cex.axis=1,cex.main=1,las=1,x.cont=TRUE,xaxt="n")
    # points(s.split[[i]]$time, s.split[[i]]$conc ,pch = c(1,19))[as.numeric(f)]
    xtick(xx1) #tick for x-axis
    ytick(yy1) #tick for y-axis
    prdcount(i,s.split, prdcount)
 }
}