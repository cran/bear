### List of Generalized Linear Models (GLM) for non-replicated (2x2x2); replicated/parallel --> RepNCA.MIXmenu()  --YJ
###
NCA.BANOVAmenu<-function(multiple=FALSE)
{
designtrace<-designtrace
dt_old<-""
dt_old<-designtrace

cat("*** You have selected the following ->\n",designtrace,"\n")

  file.menu <- c("NCA --> Statistical analysis (ANOVA(lm), 90%CI...)",
                 "Run demo for NCA --> Statistical analysis",
                 "Back to the previous step",
                 "Back to the top menu",
                 "Quit")
 cat("\n")
  pick <- menu(file.menu, title = " << NCA --> Stat Analysis for 2x2x2 Crossover Study >>", graphics=TRUE)
  if(multiple){
   if (pick == 1){
        dt_old<-gsub("NCA - stat analysis,","",dt_old,fixed=TRUE)   ### remove first if there is any and then will put it back again.
        dt_old<-trim(dt_old)                                        ### Remove leading and trailing spaces from character strings
        dt_old<-paste(dt_old,"NCA - stat analysis,",sep=" ")        ### if there is no 'stat analysis only,' following NCA only.
                                                                    ### otherwise, it may be duplicated. --> avoid being duplicated!
        designtrace<<-dt_old
        cat("\n")
        MultipleNCA.BANOVAdata()
        }
    else {
    if (pick == 2){
        cat("\n")
        Multipledemomenu1()
       }
    else {
    if (pick == 3){
        cat("\n")
        dt_old<-gsub("NCA - stat analysis,","",dt_old,fixed=TRUE)    ### find and replace characters in a string... -YJ
        dt_old<-trim(dt_old)                                         ### Remove leading and trailing spaces from character strings
        designtrace<<-dt_old
        graphics.off()
        Multiplestat1menu()
         }
   else {
    if (pick == 4){
        graphics.off()
        go2menu()
              }
   else {
     if (pick== 5){
        cat("\n   Thank you for using bear!  Bye now. \n\n")
        graphics.off()
                  }
           }
        }
      }
    }
  } 
  else{ 
    if (pick == 1){
      cat("\n")
        NCA.BANOVAdata()
        }
    else {
    if (pick == 2){
        cat("\n")
        demomenu1()
       }
    else {
    if (pick == 3){
        cat("\n")
        graphics.off()
        dt_old<-gsub("2x2x2 crossover,","",dt_old,fixed=TRUE)   ### find and replace characters in a string... -YJ
        dt_old<-trim(dt_old)                                    ### Remove leading and trailing spaces from character strings
        designtrace<<-dt_old
        Multiplestatmenu()
         }
   else {
 if (pick == 4){
        graphics.off()
        go2menu()
              }
   else {
     if (pick== 5){
        cat("\n   Thank you for using bear!  Bye now. \n\n")
        graphics.off()
               }
           }
         }
       }
    }
  }
}
  