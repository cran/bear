library(plotrix) 
BANOVAplot<-function(IntraInterlnCmax00, IntraInterlnAUC0t00,IntraInterlnAUC0INF00,
                     IntraInterlnCmaxseq11,IntraInterlnCmaxseq22,
                     IntraInterlnAUC0tseq11,IntraInterlnAUC0tseq22,
                     IntraInterlnAUC0INFseq11,IntraInterlnAUC0INFseq22)
{

#par(mfrow=c(2,2))
##lnCmax
qqnorm(IntraInterlnCmax00$Stud_Intra,
main = "Normal Probability Plot of lnCmax (intrasubj)",
xlab = "Normal scores", ylab = "Studentized intra-subject residuals",las=1)
qqline(IntraInterlnCmax00$Stud_Intra)
abline(h=0, col = "gray60")
abline(v=0, col = "gray60")

qqnorm(IntraInterlnCmax00$Stud_Inter,
main = "Normal Probability Plot of lnCmax (intersubj)",
xlab = "Normal scores", ylab = "Studentized inter-subject residuals",las=1)
qqline(IntraInterlnCmax00$Stud_Inter)
abline(h=0, col = "gray60")
abline(v=0, col = "gray60")

lnCmaxseq11_plotlabels<-c(IntraInterlnCmaxseq11$subj)
lnCmaxseq22_plotlabels<-c(IntraInterlnCmaxseq22$subj)
##intra-subject residuals: lnCmax(expected value) vs studentized residuals 
x <- IntraInterlnCmax00[,3]
y <- IntraInterlnCmax00[,5]

plot(x, y,asp=NA,col="lightgray", xlab="lnCmax (expected value)", ylab="studentized residuals", 
     main="intra-subject residuals",las=1,xlim=c(min(x),max(x)),
     ylim=c(min(y)-3,max(y)+3))
abline(h=0, col = "gray60")
abline(h=2, col = "green")
abline(h=-2, col = "green")
abline(h=3, col = "red")
abline(h=-3, col = "red")
points(IntraInterlnCmaxseq11$Exp, IntraInterlnCmaxseq11$Stud_Intra, col=4, pch=15)
thigmophobe.labels(IntraInterlnCmaxseq11$Exp,IntraInterlnCmaxseq11$Stud_Intra,lnCmaxseq11_plotlabels, col="black",text.pos=TRUE) 

points(IntraInterlnCmaxseq22$Exp, IntraInterlnCmaxseq22$Stud_Intra, col=5, pch=16)
thigmophobe.labels(IntraInterlnCmaxseq22$Exp,IntraInterlnCmaxseq22$Stud_Intra,lnCmaxseq22_plotlabels, col="black",text.pos=TRUE)

temp <- legend("topright",legend = c("sequence 1", "sequence 2"),
               text.width = strwidth("1,000,000000"),
               pch = 15:16, col = 4:5, xjust = 1, yjust = 1)

##intersubject; lnCmax(expected value) vs studentized residuals 
x <- IntraInterlnCmax00[,3]
y <- IntraInterlnCmax00[,7]

plot(x, y,asp=NA,col="lightgray", xlab="lnCmax (expected value)", ylab="studentized residuals", 
     main="inter-subject residuals",las=1,xlim=c(min(x),max(x)),
     ylim=c(min(y)-3,max(y)+3))
abline(h=0, col = "gray60")
abline(h=2, col = "green")
abline(h=-2, col = "green")
abline(h=3, col = "red")
abline(h=-3, col = "red")
points(IntraInterlnCmaxseq11$Exp, IntraInterlnCmaxseq11$Stud_Inter, col=4, pch=15)
thigmophobe.labels(IntraInterlnCmaxseq11$Exp,IntraInterlnCmaxseq11$Stud_Inter,lnCmaxseq11_plotlabels, col="black",text.pos=TRUE) 

points(IntraInterlnCmaxseq22$Exp, IntraInterlnCmaxseq22$Stud_Inter, col=5, pch=16)
thigmophobe.labels(IntraInterlnCmaxseq22$Exp,IntraInterlnCmaxseq22$Stud_Inter,lnCmaxseq22_plotlabels, col="black",text.pos=TRUE)

temp <- legend("topright",legend = c("sequence 1", "sequence 2"),
               text.width = strwidth("1,000,000000"),
               pch = 15:16, col = 4:5, xjust = 1, yjust = 1)

#lnAUC0t
qqnorm(IntraInterlnAUC0t00$Stud_Intra,
main = "Normal Probability Plot of lnAUC0t (intrasubj)",
xlab = "Normal scores", ylab = "Studentized intra-subject residuals",las=1)
qqline(IntraInterlnAUC0t00$Stud_Intra)
abline(h=0, col = "gray60")
abline(v=0, col = "gray60")

qqnorm(IntraInterlnAUC0t00$Stud_Inter,
main = "Normal Probability Plot of lnAUC0t (intersubj)",
xlab = "Normal scores", ylab = "Studentized inter-subject residuals",las=1)
qqline(IntraInterlnAUC0t00$Stud_Inter)
abline(h=0, col = "gray60")
abline(v=0, col = "gray60")

lnAUC0tseq11_plotlabels<-c(IntraInterlnAUC0tseq11$subj)
lnAUC0tseq22_plotlabels<-c(IntraInterlnAUC0tseq22$subj)
##intra-subject residuals: lnAUC0t(expected value) vs studentized residuals
x <- IntraInterlnAUC0t00[,3]
y <- IntraInterlnAUC0t00[,5]

plot(x, y,asp=NA,col="lightgray", xlab="lnAUC0t (expected value)", ylab="studentized residuals",
     main="intra-subject residuals",las=1,xlim=c(min(x),max(x)),
     ylim=c(min(y)-3,max(y)+3))
abline(h=0, col = "gray60")
abline(h=2, col = "green")
abline(h=-2, col = "green")
abline(h=3, col = "red")
abline(h=-3, col = "red")
points(IntraInterlnAUC0tseq11$Exp, IntraInterlnAUC0tseq11$Stud_Intra, col=4, pch=15)
thigmophobe.labels(IntraInterlnAUC0tseq11$Exp,IntraInterlnAUC0tseq11$Stud_Intra,lnAUC0tseq11_plotlabels, col="black",text.pos=TRUE)

points(IntraInterlnAUC0tseq22$Exp, IntraInterlnAUC0tseq22$Stud_Intra, col=5, pch=16)
thigmophobe.labels(IntraInterlnAUC0tseq22$Exp,IntraInterlnAUC0tseq22$Stud_Intra,lnAUC0tseq22_plotlabels, col="black",text.pos=TRUE)

temp <- legend("topright",legend = c("sequence 1", "sequence 2"),
               text.width = strwidth("1,000,000000"),
               pch = 15:16, col = 4:5, xjust = 1, yjust = 1)

##intersubject; lnAUC0t(expected value) vs studentized residuals
x <- IntraInterlnAUC0t00[,3]
y <- IntraInterlnAUC0t00[,7]

plot(x, y,asp=NA,col="lightgray", xlab="lnAUC0t (expected value)", ylab="studentized residuals",
     main="inter-subject residuals",las=1,xlim=c(min(x),max(x)),
     ylim=c(min(y)-3,max(y)+3))
abline(h=0, col = "gray60")
abline(h=2, col = "green")
abline(h=-2, col = "green")
abline(h=3, col = "red")
abline(h=-3, col = "red")
points(IntraInterlnAUC0tseq11$Exp, IntraInterlnAUC0tseq11$Stud_Inter, col=4, pch=15)
thigmophobe.labels(IntraInterlnAUC0tseq11$Exp,IntraInterlnAUC0tseq11$Stud_Inter,lnAUC0tseq11_plotlabels, col="black",text.pos=TRUE)

points(IntraInterlnAUC0tseq22$Exp, IntraInterlnAUC0tseq22$Stud_Inter, col=5, pch=16)
thigmophobe.labels(IntraInterlnAUC0tseq22$Exp,IntraInterlnAUC0tseq22$Stud_Inter,lnAUC0tseq22_plotlabels, col="black",text.pos=TRUE)

temp <- legend("topright",legend = c("sequence 1", "sequence 2"),
               text.width = strwidth("1,000,000000"),
               pch = 15:16, col = 4:5, xjust = 1, yjust = 1)
               
#lnAUC0inf
qqnorm(IntraInterlnAUC0INF00$Stud_Intra,
main = "Normal Probability Plot of lnAUC0INF (intrasubj)",
xlab = "Normal scores", ylab = "Studentized intra-subject residuals",las=1)
qqline(IntraInterlnAUC0INF00$Stud_Intra)
abline(h=0, col = "gray60")
abline(v=0, col = "gray60")

qqnorm(IntraInterlnAUC0INF00$Stud_Inter,
main = "Normal Probability Plot of lnAUC0INF (intersubj)",
xlab = "Normal scores", ylab = "Studentized inter-subject residuals",las=1)
qqline(IntraInterlnAUC0INF00$Stud_Inter)
abline(h=0, col = "gray60")
abline(v=0, col = "gray60")

lnAUC0INFseq11_plotlabels<-c(IntraInterlnAUC0INFseq11$subj)
lnAUC0INFseq22_plotlabels<-c(IntraInterlnAUC0INFseq22$subj)
##intra-subject residuals: lnAUC0INF(expected value) vs studentized residuals
x <- IntraInterlnAUC0INF00[,3]
y <- IntraInterlnAUC0INF00[,5]

plot(x, y,asp=NA,col="lightgray", xlab="lnAUC0INF (expected value)", ylab="studentized residuals",
     main="intra-subject residuals",las=1,xlim=c(min(x),max(x)),
     ylim=c(min(y)-3,max(y)+3))
abline(h=0, col = "gray60")
abline(h=2, col = "green")
abline(h=-2, col = "green")
abline(h=3, col = "red")
abline(h=-3, col = "red")
points(IntraInterlnAUC0INFseq11$Exp, IntraInterlnAUC0INFseq11$Stud_Intra, col=4, pch=15)
thigmophobe.labels(IntraInterlnAUC0INFseq11$Exp,IntraInterlnAUC0INFseq11$Stud_Intra,lnAUC0INFseq11_plotlabels, col="black",text.pos=TRUE)

points(IntraInterlnAUC0INFseq22$Exp, IntraInterlnAUC0INFseq22$Stud_Intra, col=5, pch=16)
thigmophobe.labels(IntraInterlnAUC0INFseq22$Exp,IntraInterlnAUC0INFseq22$Stud_Intra,lnAUC0INFseq22_plotlabels, col="black",text.pos=TRUE)

temp <- legend("topright",legend = c("sequence 1", "sequence 2"),
               text.width = strwidth("1,000,000000"),
               pch = 15:16, col = 4:5, xjust = 1, yjust = 1)

##intersubject; lnAUC0INF(expected value) vs studentized residuals
x <- IntraInterlnAUC0INF00[,3]
y <- IntraInterlnAUC0INF00[,7]

plot(x, y,asp=NA,col="lightgray", xlab="lnAUC0INF (expected value)", ylab="studentized residuals",
     main="inter-subject residuals",las=1,xlim=c(min(x),max(x)),
     ylim=c(min(y)-3,max(y)+3))
abline(h=0, col = "gray60")
abline(h=2, col = "green")
abline(h=-2, col = "green")
abline(h=3, col = "red")
abline(h=-3, col = "red")
points(IntraInterlnAUC0INFseq11$Exp, IntraInterlnAUC0INFseq11$Stud_Inter, col=4, pch=15)
thigmophobe.labels(IntraInterlnAUC0INFseq11$Exp,IntraInterlnAUC0INFseq11$Stud_Inter,lnAUC0INFseq11_plotlabels, col="black",text.pos=TRUE)

points(IntraInterlnAUC0INFseq22$Exp, IntraInterlnAUC0INFseq22$Stud_Inter, col=5, pch=16)
thigmophobe.labels(IntraInterlnAUC0INFseq22$Exp,IntraInterlnAUC0INFseq22$Stud_Inter,lnAUC0INFseq22_plotlabels, col="black",text.pos=TRUE)

temp <- legend("topright",legend = c("sequence 1", "sequence 2"),
               text.width = strwidth("1,000,000000"),
               pch = 15:16, col = 4:5, xjust = 1, yjust = 1)

##plot boxplot
op <- par(mfrow=c(1,3))
boxplot(IntraInterlnCmax00$Stud_Intra,col="lightgray", main="lnCmax (intrasubj)", ylab="studentized residuals",las=1)
boxplot(IntraInterlnAUC0t00$Stud_Intra,col="lightgray", main="lnAUC0t (intrasubj)", ylab="studentized residuals",las=1)
boxplot(IntraInterlnAUC0INF00$Stud_Intra,col="lightgray", main="lnAUC0INF (intrasubj)", ylab="studentized residuals",las=1)

boxplot(IntraInterlnCmax00$Stud_Inter,col="lightgray", main="lnCmax (intersubj)", ylab="studentized residuals",las=1)
boxplot(IntraInterlnAUC0t00$Stud_Inter,col="lightgray", main="lnAUC0t (intersubj)", ylab="studentized residuals",las=1)
boxplot(IntraInterlnAUC0INF00$Stud_Inter,col="lightgray", main="lnAUC0INF (intersubj)", ylab="studentized residuals",las=1)
}