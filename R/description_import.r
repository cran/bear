description_import<-function(){
   cat("\n\n")
   filepath<-getwd()
   cat("bear will read and import your data file (*.csv) from\n")
   cat("",filepath,"by default.\n\n")
   readline("  Press Enter to select file...")
}