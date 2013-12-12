### List of Generalized Linear Models (GLM) for replicated/parallel; non-replicated/non-parallel (2x2x2) --> NCA.BANOVAmenu()  --YJ
###
RepNCA.MIXmenu<-function(parallel=FALSE, multiple=FALSE)
{
designtrace<-designtrace
dt_old<-""
dt_old<-designtrace

cat("*** You have selected the following ->\n",designtrace,"\n")

  if(parallel){
  file.menu <- c("NCA --> Statistical analysis (lm, 90%CI...)",
                 "Run demo for NCA --> Statistical analysis",
                 "Back to the previous step",
                 "Quit")
 cat("\n")}
 else{
 file.menu <- c("NCA --> Statistical analysis (lme, 90%CI...)",
                "Run demo for NCA --> Statistical analysis",
                "Back to the previous step",
                "Quit")
 cat("\n")}
  if(parallel){
  pick <- menu(file.menu, title = "<< NCA--> Statistical Analysis for Parallel Study >> ", graphics=TRUE)
   }
  else{
  pick <- menu(file.menu, title = "<< NCA--> Statistical Analysis for Replicated Crossover Study >> ", graphics=TRUE)
  }  
    if (pick == 1){
      cat("\n")
          if(parallel){
            if(multiple){
            MultipleParaNCA.MIXdata()  
            }
            else{
            ParaNCA.MIXdata()  
            }
          }
          else{
            RepNCA.MIXdata()
            }
       } 
    else {
    if (pick == 2){
        cat("\n")
         if(parallel){
           if(multiple){
            MultipleParademomenu1()
            }
            else{       
            Parademomenu1()   
            }
          }
          else{
            Repdemomenu1()
          }
      } 
    else {
    if (pick == 3){
        cat("\n")
        dt_old<-gsub("replicated study,","",dt_old,fixed=TRUE)    ### find and replace characters in a string... -YJ
        dt_old<-gsub("parallel study,","",dt_old,fixed=TRUE)      ### find and replace characters in a string... -YJ
        dt_old<-trim(dt_old)                                      ### Remove leading and trailing spaces from character strings
        designtrace<<-dt_old
        if(multiple){
             Multiplestat1menu()
        }
         else{
             Multiplestatmenu()
          }
         }
   else {
    if (pick == 4){
        cat("\n   Thank you for using bear!  Bye now. \n\n")
        graphics.off()
              }
           }
       }
    }
}
