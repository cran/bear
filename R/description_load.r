description_load<-function(){
    cat("\n\n")
    filepath<-getwd()
    cat("bear will read and load your data file (*.Rdata) from\n")
    cat("",filepath,"by default.\n\n")
    readline("  Press Enter to select file...")
}