description_load<-function(){
    cat("\n\n")
    filepath<-getwd()
    cat("bear will read and load your data file (*.Rdata) from\n")
    cat("",filepath,"by default.\n\n")
    cat(" Please note that selected data points saved as .RData before\n")
    cat(" bear v2.5.4 was not supported any more. Users need to re-do\n")
    cat(" data points selection if it is the the case.\n\n")
    alarm();alarm()
    readline("  Press Enter to select file...")
}