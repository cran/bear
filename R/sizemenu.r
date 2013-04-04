# List of Sample size estimation Menu
sizemenu<-function()
{
cat("\n")
  file.menu <- c("Cross-over study: Sample size estimation",
                 "Cross-over study: Run demo",
                 "Parallel study: Sample size estimation",
                 "Parallel study: Run demo",
                 "Back to top menu",
                 "Quit")
   cat("\n")
  pick <- menu(file.menu, title = " << Sample size estimation>> ")
    if (pick == 1){
      cat("\n")
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
       go() 
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