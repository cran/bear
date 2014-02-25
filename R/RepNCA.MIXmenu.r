### 
### List of Generalized Linear Models (GLM) for replicated/parallel; non-replicated/non-parallel (2x2x2) --> NCA.BANOVAmenu()  --YJ
###
RepNCA.MIXmenu<-function(parallel=FALSE, multiple=FALSE)
{
designtrace<-designtrace
dt_old<-""
dt_old<-designtrace
run.demo<-run.demo             ### FALSE (0, default) or TRUE(1)
study.type<-study.type         ### 2x2x2 crossover (0, default), replicate (1) or parallel (2)
dose.type<-dose.type           ### single-dose (0, default) or multiple-dose (1)

back.from.banova<-back.from.banova

cat("*** You have selected the following ->\n",designtrace,"\n")

###   if(parallel){
###   file.menu <- c("NCA --> Statistical analysis (lm, 90%CI...)",
###                  "Run demo for NCA --> Statistical analysis",
###                  "Back to the previous step",
###                  "Quit")
###  cat("\n")}
###  else{
###  file.menu <- c("NCA --> Statistical analysis (lme, 90%CI...)",
###                 "Run demo for NCA --> Statistical analysis",
###                 "Back to the previous step",
###                 "Quit")
###  cat("\n")}
###   if(parallel){
###   pick <- menu(file.menu, title = "<< NCA--> Statistical Analysis for Parallel Study >> ", graphics=TRUE)
###    }
###   else{
###   pick <- menu(file.menu, title = "<< NCA--> Statistical Analysis for Replicated Crossover Study >> ", graphics=TRUE)
###   }  
if (run.demo){ ###
        cat("\n")
         if(parallel){
           if(multiple){
            if(back.from.banova) {go2menu()} ### yes! this one works perfectly. also in NCA.BANOVAmenu()
            else{MultipleParademomenu1()}
            }
            else{
            if(back.from.banova) {go2menu()} ### yes! this one works perfectly. also in NCA.BANOVAmenu()
            else {Parademomenu1()}
            }
          }
          else{
            if(back.from.banova) {go2menu()} ### yes! this one works perfectly. also in NCA.BANOVAmenu()
            else{Repdemomenu1()}
          }    
       } ###
else { ###
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
  } ###
}
### 
###     else {
###     if (pick == 3){
###         cat("\n")
###         dt_old<-gsub("replicated study,","",dt_old,fixed=TRUE)    ### find and replace characters in a string... -YJ
###         dt_old<-gsub("parallel study,","",dt_old,fixed=TRUE)      ### find and replace characters in a string... -YJ
###         dt_old<-trim(dt_old)                                      ### Remove leading and trailing spaces from character strings
###         designtrace<<-dt_old
###         if(multiple){
###              Multiplestat1menu()
###         }
###          else{
###              Multiplestatmenu()
###           }
###          }
###    else {
###     if (pick == 4){
###         cat("\n   Thank you for using bear!  Bye now. \n\n")
###         graphics.off()
###               }
###            }
###        }
###     }
### }
### 