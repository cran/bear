# List of Sample size estimation Menu
sizemenu<-function()
{
cat("\n")
  file.menu <- c("Sample size estimation ",
                 "Run demo for sample size estimation ",
                 "Back to top menu",
                 "Quit")
   cat("\n")
  pick <- menu(file.menu, title = " << Sample size estimation menu >> ")
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
       go() 
         }
   else {
    if (pick == 4){
        cat("\n")
        cat("\nBye~~ \n\n")
         }
    
       }
     }
  }
}