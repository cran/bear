logo_plot_desc<-function()
{
## pdf("test.pdf", paper = "a4", bg = "white") ## for test purposes
### require(png)
### require(grid)
par(mar=c(0, 0, 0, 0))     ### this will cause a blank graphic window showing up with nothing there; cannot avoid this. -YJ
plot(0, xlim=c(0, 210), ylim=c(0, 297), col="white")
logo<-readPNG(system.file("img","bear_logo-2013.png",package="bear"),TRUE)
grid.raster(logo,width=unit(1,"npc"),height=unit(1,"npc"))     ### this one works great without showing border. -YJ
text(100,100,"This file was generated using bear v2.6.0",cex = 1.2)
text(70,90,"on:",cex=1.2)
text(110,90,Sys.time(),cex = 1.2)
text(100,70,
"bear is developed by Hsin-ya Lee (HY) & Yung-jin Lee (YJ).",cex = 1.2)
text(100,60,
"Kaohsiung Veterans General Hospital (HY) & ptpc inc. (YJ).",cex = 1.2)
text(100,50,
"Kaohsiung, Taiwan",cex = 1.2)
par(mai=c(0.9,0.9,0.9,0.9))   ### best-fit for my plots and suitable for ivivc for R and bear. -YJ
## dev.off()  ## remarmed this after testing
}