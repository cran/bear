### List of Replicated study lme menu for replicate/nparallel; non-replicate/non-parallel -> BANOVAmenu()
###
# Replicated study lme menu
RepMIXmenu<-function(TotalData, parallel=FALSE, multiple=FALSE)
{
designtrace<-designtrace
dt_old<-""
dt_old<-designtrace

cat("\n\n*** You have selected the following ->\n",designtrace,"\n")

  file.menu <- c("Statistical analysis (lme, 90%CI...)",
                 "Run demo: Statistical analysis (lme, 90%CI...)",
                 "Back to the previous step",
                 "Quit")
 cat("\n")
  pick <- menu(file.menu, title = " << Statistical analysis (lme, 90%CI...) menu >> ", graphics=TRUE)
    if (pick == 1){
        dt_old<-gsub("stat analysis only,","",dt_old,fixed=TRUE)   ### remove first if there is any and then will put it back again.
        dt_old<-trim(dt_old)                                       ### Remove leading and trailing spaces from character strings
        dt_old<-paste(dt_old,"stat analysis only,",sep=" ")        ### if there is no 'stat analysis only,' following NCA only.
                                                                   ### otherwise, it may be duplicated. --> avoid being duplicated!        
        designtrace<<-dt_old
        cat("\n")
        if(parallel){
          if(multiple){
             MultipleParaMIXdata(TotalData)
          }
          else{
             ParaMIXdata(TotalData)
          }
        }
        else{ 
             RepMIXdata(TotalData)
          } 
        }
    else {
    if (pick == 2){
        cat("\n")
        if(parallel){
           if(multiple){
             MultipleParademoMIX()
          }
          else{
             ParademoMIX()
          }
        }
        else{ 
             RepdemoMIX()
        } 
       }
    else {
    if (pick == 3){
        cat("\n")
       if(multiple){
         dt_old<-gsub("replicated study,","",dt_old,fixed=TRUE)     ### find and replace characters in a string... -YJ
         dt_old<-gsub("parallel study,","",dt_old,fixed=TRUE)       ### find and replace characters in a string... -YJ
         dt_old<-trim(dt_old)                                       ### Remove leading and trailing spaces from character strings 
         designtrace<<-dt_old
         stat1menu()
       }
       else{
         dt_old<-gsub("replicated study,","",dt_old,fixed=TRUE)     ### find and replace characters in a string... -YJ
         dt_old<-gsub("parallel study,","",dt_old,fixed=TRUE)       ### find and replace characters in a string... -YJ
         dt_old<-trim(dt_old)                                       ### Remove leading and trailing spaces from character strings 
         designtrace<<-dt_old
         statmenu()
         }
       }
   else {
    if (pick == 4){
        cat("\n  Thank you for using bear!  Bye now. \n\n")
        graphics.off()
             }
           }
       }
     }
}