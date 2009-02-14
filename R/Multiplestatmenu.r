#NCA-->statistical analysis
Multiplestatmenu<-function()
{
cat("\n")
  file.menu <- c("NCA --> Statistical analysis for 2x2x2 crossover study",
                 "NCA --> Statistical analysis for replicate study",
                 "Back to the previous step",
                 "Quit")
 cat("\n")
  pick <- menu(file.menu, title = " <<NCA --> Statistical Analysis>> ")
    if (pick == 1){
      cat("\n")
        NCA.BANOVAmenu()
        }
    else {
    if (pick == 2){
        cat("\n")
        RepNCA.MIXmenu()
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