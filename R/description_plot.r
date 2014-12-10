description_plot<-function()
{
## pdf("test.pdf", paper = "a4", bg = "white") ## for test purposes
par(mar=c(0, 0, 0, 0)) 
plot(0, xlim=c(0, 210), ylim=c(0, 297), col="white")
text(52, 210,"
.b                  
88                 
888oooo         .ooooo.        eoooo.      oooood8b
d88       88   d8(        )8b             )8b     888     8)
888       88   888ooo88b   o8o89888     888
888       88   888              88(        88     888
 o8ooo8         88bod8P     doooo888   d888b", cex=0.8, adj=c(0,0))
text(100,180,"This report was generated using bear v2.6.4",cex = 1.2)
text(70,170,"on:",cex=1.2)
text(110,170,Sys.time(),cex = 1.2)
text(100,150,
"bear is developed by Hsin-ya Lee & Yung-jin Lee.",cex = 1.2)
text(100,140,
"Kaohsiung, Taiwan",cex = 1.2)
text(100,120,
"bear is under license of GPL-2|GPL-3.",cex = 1.2)
## dev.off()  ## remarmed this after testing
}