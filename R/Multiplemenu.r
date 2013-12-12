# menu for NCA non-replicated and replicated study <-  Wow!  how can I guess this?  totally different.
Multiplemenu<-function(multiple=FALSE)
{
designtrace<-designtrace
dt_old<-""
dt_old<-designtrace

cat("*** You have selected the following ->\n",designtrace,"\n")
 #mutiple dose menu
  if(multiple){
  file.menu <- c("NCA for 2x2x2 crossover study",
                 "NCA for parallel study", 
                 "Back to the previous step",
                 "Quit")
cat("\n")
  pick <- menu(file.menu, title = " << Noncompartmental analysis (NCA)>> ", graphics=TRUE)
    if (pick == 1){
      cat("\n")
        designtrace<<-paste(designtrace,"2x2x2 crossover,",sep=" ")
        MultipleNCAmenu()
        }
    else {
    if (pick == 2){
       designtrace<<-paste(designtrace,"parallel study,",sep=" ")
       MultipleParamenu() 
       } 
    else {
    if (pick == 3){
        cat("\n")
        dt_old<-gsub("NCA only,","",dt_old,fixed=TRUE)   ### find and replace characters in a string... -YJ
        dt_old<-trim(dt_old)                             ### Remove leading and trailing spaces from character strings 
        designtrace<<-dt_old
        Multiplego()
      }    
    else {
    if (pick == 4){
        cat("\n")
        cat("\n  Thank you for using bear!  Bye now. \n")}
        graphics.off()
          }
         } 
       }
      }
  #single dose menu
 else{
  file.menu <- c("NCA for 2x2x2 crossover study",
                 "NCA for replicated study",
                 "NCA for parallel study", 
                 "Back to the previous step",
                 "Quit")
 cat("\n")
  pick <- menu(file.menu, title = " << Noncompartmental analysis (NCA)>> ", graphics=TRUE)
    if (pick == 1){
        cat("\n")
        designtrace<<-paste(designtrace,"2x2x2 crossover,",sep=" ")
        NCAmenu()
        }
    else {
    if (pick == 2){
        cat("\n")
        designtrace<<-paste(designtrace,"replicated study,",sep=" ")
        Repmenu()
      } 
    else {
    if (pick == 3){
        cat("\n")
        designtrace<<-paste(designtrace,"parallel study,",sep=" ")
        Paramenu()
      }    
    else {
    if (pick == 4){
        cat("\n")
        dt_old<-gsub("NCA only,","",dt_old,fixed=TRUE)   ### find and replace characters in a string... -YJ
        dt_old<-trim(dt_old)                             ### Remove leading and trailing spaces from character strings 
        designtrace<<-dt_old
        Singlego()
      } 
    else {
    if (pick == 5){
        cat("\n")
        cat("\n   Thank you for using bear!  Bye now. \n")
        graphics.off()
           }
         } 
       }
      }
    }
  }
}