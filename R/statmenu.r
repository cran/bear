# for statistical analysis
statmenu<-function()
{
cat("\n")
  file.menu <- c("Statistical analysis for 2x2x2 crossover study",
                 "Statistical analysis for replicate study",
                 "Back to the previous step",
                 "Quit")
 cat("\n")
  pick <- menu(file.menu, title = " << Statistical analysis menu>> ")
    if (pick == 1){
      cat("\n")
        BANOVAmenu()
        }
    else {
    if (pick == 2){
        cat("\n")
        RepMIXmenu()
       }
    else {
    if (pick == 3){
        cat("\n")
       go()
       }
    else {
    if (pick == 4){
        cat("\n")
      cat("\nThank you for using bear!  Bye now. \n")}
       }
      }
    }
 }