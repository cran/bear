description_plot<-function()
{

par(mar=c(0, 0, 0, 0)) 
plot(0, xlim=c(0, 210), ylim=c(0, 297), col="white")
text(100, 260,
"~~~  This report was generated using bear v2.4.2. ~~~",cex = .8)
text(76,240,"and it was created on (local time): ",cex = .8)
text(136,240,Sys.time(),cex = .8)
text(100, 220,
"bear was developed by Hsin-ya Lee & Yung-jin Lee.",cex = .8)
text(100, 210,
"College of Pharmacy, Kaohsiung Medical University &",cex = .8)
text(100, 200,
"PharmaTek Pharmaceutical Consulting Ltd.,",cex = .8)
text(100, 190,
"Kaohsiung City, Taiwan",cex = .8)
text(100, 180,
"contact: Yung-jin Lee <mobilePK@gmail.com>",cex = .8)
text(100, 170,
"Internet websites:",cex = .8)
text(100, 160,
"bear's website: http://pkpd.kmu.edu.tw/bear",cex = .8)
text(100, 150,
"R website: http://www.r-project.org" ,cex = .8)
text(100, 140,
"Bebac Forum: R for BE/BA --> http://forum.bebac.at/?category=19" ,cex = .8)
}