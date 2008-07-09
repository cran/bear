# List of Sample size estimation Menu
sizemenu<-function()
{
cat("\n")
  file.menu <- c("Sample size estimation (Raw data)",
                 "Sample size estimation (Log transformation)",
                 "Demo for sample size estimation (Raw data)",
                 "Back to go menu",
                 "Quit")
   cat("\n")
  pick <- menu(file.menu, title = " << Sample size estimation menu >> ")
    if (pick == 1){
      cat("\n")
        rawdata()
        }
    else {
    if (pick == 2){
        cat("\n")
        logdata()
       }
    else {
    if (pick == 3){
        cat("\n")
        demosize()
         }
   else {
    if (pick == 4){
        cat("\n")
        go()
         }
    else {
    if (pick == 5){
        cat("\nBye~~ \n\n")}
         } 
       }
     }
  }
}