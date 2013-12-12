# List of Noncompartment Analysis (NCA) for 2x2 crossover 
NCAmenu<-function(replicated=FALSE, parallel=FALSE, multiple=FALSE)
{

designtrace<-designtrace
dt_old<-""
dt_old<-designtrace

cat("\n\n*** You have selected the following ->\n",designtrace,"\n")
### no tracing here... 

  file.menu <- c("Run NCA",
                 "Run NCA demo",
                 "Back to the previous step",
                 "Quit")
 cat("\n")
if(parallel){
       pick <- menu(file.menu, title = " << NCA for Parallel Study>> ", graphics=TRUE)
   }
else{
   if(replicated ){
       pick <- menu(file.menu, title = " << NCA for Replicated Crossover Study>> ", graphics=TRUE)
    } 
    else{ 
       pick <- menu(file.menu, title = " << NCA for 2x2x2 Crossover Study>> ", graphics=TRUE)
    }
} 
    if (pick == 1){
         dt_old<-gsub("NCA only,","",dt_old,fixed=TRUE)   ### remove first if there is any and then will put it back again.
         dt_old<-trim(dt_old)                             ### Remove leading and trailing spaces from character strings
         dt_old<-paste(dt_old,"NCA only,",sep=" ")        ### if there is no 'stat analysis only,' following NCA only.
                                                          ### otherwise, it may be duplicated. --> avoid being duplicated!
         designtrace<<-dt_old
      cat("\n")
      if(multiple){
         if(parallel){
         MultipleParaNCAdata()
         }
       else{
          MultipleNCAdata()
          }
         } 
      else{
        if(parallel){
         ParaNCAdata()
         }
         else{
          if(replicated ){
          RepNCAdata()
          } 
          else{ 
          NCAdata()
          }
         } 
        }
       } 
    else {
    if (pick == 2){
        cat("\n")
      if(multiple){
        if(parallel){
       MultipleParademomenu()
       }
       else{
         Multipledemomenu() 
          }
         } 
       else{
         if(parallel){
          Parademomenu()
          }
          else{
           if(replicated ){
            Repdemomenu()
             } 
            else{ 
            demomenu()}
          } 
         }
        } 
    else {
    if (pick == 3){
        cat("\n")
       if(multiple){
       dt_old<-gsub("2x2x2 crossover,","",dt_old,fixed=TRUE)   ### find and replace characters in a string... -YJ
       dt_old<-gsub("parallel study,","",dt_old,fixed=TRUE)    ### find and replace characters in a string... -YJ
       dt_old<-trim(dt_old)                                    ### Remove leading and trailing spaces from character strings
       designtrace<<-dt_old
       Multiple1menu()
       
       }
       else{
       dt_old<-gsub("2x2x2 crossover,","",dt_old,fixed=TRUE)   ### find and replace characters in a string... -YJ
       dt_old<-gsub("replicated study,","",dt_old,fixed=TRUE)  ### find and replace characters in a string... -YJ
       dt_old<-gsub("parallel study,","",dt_old,fixed=TRUE)    ### find and replace characters in a string... -YJ
       dt_old<-trim(dt_old)                                    ### Remove leading and trailing spaces from character strings
       designtrace<<-dt_old
       Multiplemenu() 
       }
      } 
    else {
    if (pick == 4){
        cat("\n")
        cat("\n  Thank you for using bear!  Bye now. \n")
        graphics.off()
        }
       }      
      }
    }
 }
   