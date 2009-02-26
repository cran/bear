description_plot<-function()
{

# reset margins
par(mar=c(0,0,0,0)) 
plot(0, xlim=c(0, 210), ylim=c(0, 297), col="white")
text(100, 250,
"~~~  This report is generated using bear v2.3.0 for R. ~~~",cex = .8)
text(100, 240,
"Authors: Hsin-ya Lee, Yung-jin Lee",cex = .8)
text(100, 230,
"College of Pharmacy, Kaohsiung Medical University",cex = .8)
text(100, 220,
"Kaohsiung, Taiwan 80708",cex = .8)
text(100, 210,
"E-mail: hsinyalee@gmail.com, mobilePK@gmail.com",cex = .8)
text(100, 200,
"bear's website: http://pkpd.kmu.edu.tw/bear",cex = .8)
text(100, 190,
"R website: http://www.r-project.org" ,cex = .8)

#par(mar=c(4,4,4,4)) # reset margins
}