# for statistical analysis
statmenu<-function(multiple=FALSE)
{

designtrace<-designtrace
dt_old<-""
dt_old<-designtrace
run.demo<-run.demo             ### FALSE (0, default) or TRUE(1)
study.type<-study.type         ### 2x2x2 crossover (0, default), replicate (1) or parallel (2)
dose.type<-dose.type           ### single-dose (0, default) or multiple-dose (1)

cat("*** You have selected the following ->\n",designtrace,"\n")
if(multiple){
  ### file.menu <- c("Statistical analysis for 2x2x2 crossover study",
  ###                "Statistical analysis for parallel study",
  ###                "Back to the previous step",
  ###                "Quit")
  ### cat("\n")
  ### pick <- menu(file.menu, title = " << Statistical analysis menu>> ", graphics=TRUE)
  if (study.type == 0){
      cat("\n")
        designtrace<<-paste(designtrace,"2x2x2 crossover,",sep=" ")
        MultipleBANOVAmenu()
        }
  else {
    if (study.type == 2){
        cat("\n")
        designtrace<<-paste(designtrace,"parallel study,",sep=" ")
        MultipleParaMIXmenu()
       }
    }
  }
###     else {
###     if (pick == 3){
###         cat("\n")
###         dt_old<-gsub("stat analysis only,","",dt_old,fixed=TRUE)   ### find and replace characters in a string... -YJ
###         dt_old<-trim(dt_old)                                       ### Remove leading and trailing spaces from character strings 
###         designtrace<<-dt_old
###         Multiplego()
###        }
###     else {
###     if (pick == 4){
###         cat("\n")
###         cat("\n  Thank you for using bear!  Bye now. \n")
###         graphics.off()
###          }
###        }
###       }
###     }
else{
   ### file.menu <- c("Statistical analysis for 2x2x2 crossover study",
   ###                 "Statistical analysis for replicate study",
   ###                 "Statistical analysis for parallel study",
   ###                 "Back to the previous step",
   ###                 "Quit")
   ### cat("\n")
   ### pick <- menu(file.menu, title = " << Statistical analysis menu>> ", graphics=TRUE)
    if (study.type == 0){
      cat("\n")
        designtrace<<-paste(designtrace,"2x2x2 crossover,",sep=" ")
        BANOVAmenu()
        }
    else {
    if (study.type == 1){
        cat("\n")
        designtrace<<-paste(designtrace,"replicated study,",sep=" ")
        RepMIXmenu()
       }
    else {
    if (study.type == 2){
        cat("\n")
        designtrace<<-paste(designtrace,"parallel study,",sep=" ")
        ParaMIXmenu()
       }
     }
   }
 }
}
###     else {
###     if (pick == 4){
###         cat("\n")
###         dt_old<-gsub("stat analysis only,","",dt_old,fixed=TRUE)   ### find and replace characters in a string... -YJ
###         dt_old<-trim(dt_old)                                       ### Remove leading and trailing spaces from character strings 
###         designtrace<<-dt_old
###         Singlego()
###        }
###     else {
###     if (pick == 5){
###       cat("\n")
###       cat("\n  Thank you for using bear!  Bye now. \n")
###       graphics.off()}
###          } 
###        }
###       }
###     }
###    }
###  }