description_plot<-function()
{

par(mar=c(0,0,0,0)) # reset margins
plot(0, xlim=c(0, 210), ylim=c(0, 297), col="white")
text(100, 250,
"~~~  This report is generated by bear v2.1.0 for R ~~~",cex = .8)
text(100, 240,
"Authors: Hsin-ya Lee, Yung-jin Lee",cex = .8)
text(100, 230,
"#100, Shih-chuan 1st Rd., Kaoshiung, Taiwan 80708",cex = .8)
text(100, 220,
"College of Pharmacy,Kaohsiung Medical University",cex = .8)
text(100, 210,
"E-mail: hsinyalee@gmail.com, pkpd.taiwan@gmail.com",cex = .8)
text(100, 200,
"bear's website: http://pkpd.kmu.edu.tw/bear",cex = .8)
text(100, 190,
"R website: www.r-project.org" ,cex = .8)

par(mar=c(4,4,4,4)) # reset margins
}