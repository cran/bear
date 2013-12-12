# List of Sample size estimation Menu
sizemenu<-function()
{
designtrace<-designtrace
dt_old<-""
dt_old<-designtrace

cat("\n\n*** You have selected the following ->\n",designtrace,"\n")

  file.menu <- c("Cross-over study: Sample size estimation",
                 "Cross-over study: Run demo",
                 "Parallel study: Sample size estimation",
                 "Parallel study: Run demo",
                 "Back to top menu",
                 "Quit")
   cat("\n")
  pick <- menu(file.menu, title = " << Sample size estimation>> ", graphics=TRUE)
    if (pick == 1){
        cat("\n")
        dt_old<-gsub("Cross-over studies.","",dt_old,fixed=TRUE)   ### find and replace characters in a string... -YJ
        dt_old<-gsub("Parallel study.","",dt_old,fixed=TRUE)       ### find and replace characters in a string... -YJ
        dt_old<-trim(dt_old)                                       ### Remove leading and trailing spaces from character strings
        designtrace<<-dt_old
        designtrace<<-paste(designtrace,"Cross-over studies.",sep=" ")
        logdata()
        }
    else {
    if (pick == 2){
        cat("\n")
       demosize() 
       }
    else {
    if (pick == 3){
        cat("\n")
        dt_old<-gsub("Cross-over studies.","",dt_old,fixed=TRUE)   ### find and replace characters in a string... -YJ
        dt_old<-gsub("Parallel study.","",dt_old,fixed=TRUE)       ### find and replace characters in a string... -YJ
        dt_old<-trim(dt_old)                                       ### Remove leading and trailing spaces from character strings
        designtrace<<-dt_old
        designtrace<<-paste(designtrace,"Parallel study.",sep=" ")
        Paradata() 
         }
    else {
    if (pick == 4){
        cat("\n")
        demopara() 
         }
    else {
    if (pick == 5){
        cat("\n")
        go2menu() 
         }          
    else {
    if (pick == 6){
        cat("\n")
        cat("\n  Thank you for using bear!  Bye now. \n\n")
        graphics.off()
           }
         }
       }
     }
   }
  }
 }  