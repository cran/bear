description_version<-function(){
cat("~~~  This report was generated using bear v2.5.0 ~~~\n\n")
reportdate<-as.POSIXct(Sys.time())
cat("and was created on (local time):- "); show(reportdate); cat("\n")
cat("bear was developed by Hsin-ya Lee & Yung-jin Lee.\n")
cat("Contact: Yung-jin Lee <mobilePK@gmail.com> \n")
cat("College of Pharmacy, Kaohsiung Medical University (HY)&\n")
cat("PharmaTek Pharmaceutical Consulting Ltd. (YJ),\n")
cat("Kaohsiung City, Taiwan\n")
cat("Internet websites:\n")
cat("bear's website: http://pkpd.kmu.edu.tw/bear\n")
cat("R website: http://www.r-project.org\n")
cat("Users are strongly recommended to browse Bebac Forum: R for BE/BA ->\n") 
cat("http://forum.bebac.at/?category=19 for more details of bear.\n")
cat("-------------------------------------------------------------------------\n")
}