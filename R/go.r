# logo display of bear (BE/BA for R, bear)
go<-function()
{
options(warn=-1)
options(width=100,digits=5)

####
par(mar=c(0, 0, 0, 0)) 
plot(0, xlim=c(0, 210), ylim=c(0, 297), col="white")  ### will show ugly border line. too bad!  -YJ
logo<-readPNG(system.file("img","bear_logo-2013.png",package="bear"),TRUE)
grid.raster(logo,width=unit(1,"npc"),height=unit(1,"npc"))     ### this one works great without showing border. -YJ
text(100,100,"Welcome to bear v2.6.3",cex = 1.2)
text(100,70,"bear is developed by Hsin-ya Lee (HY) & Yung-jin Lee (YJ).",cex = 1.2)
text(100,60,"Kaohsiung Veterans General Hospital (HY) & ptpc inc. (YJ)",cex = 1.2)
text(100,50,"Kaohsiung, Taiwan",cex = 1.2)
text(100,30,"bear is under license of GPL-2|GPL-3.",cex = 1.2)
text(100,10,"Read or generate set-up files now. Please wait...",cex = 1.2)
### readline(" Press Enter to continue...");dev.off()
Sys.sleep(3);dev.off()
#### 
go2menu()
}
 