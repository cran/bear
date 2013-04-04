#NCA-->statistical analysis
Multiplestatmenu<-function(multiple=FALSE)
{
cat("\n")
if(multiple){
file.menu <- c("NCA --> Statistical analysis for 2x2x2 crossover study",
               "NCA --> Statistical analysis for parallel study",
               "Back to the previous step",
               "Quit")
 cat("\n")
  pick <- menu(file.menu, title = " <<NCA --> Statistical Analysis>> ")
    if (pick == 1){
      cat("\n")
        MultipleNCA.BANOVAmenu()
        }
    else {
    if (pick == 2){
        cat("\n")
        MultipleParaNCA.MIXmenu()
       }
    else {
    if (pick == 3){
        cat("\n")
         Multiplego()
       }
    else {
    if (pick == 4){
       cat("\n")
      cat("\n    Thank you for using bear!  Bye now. \n")
      graphics.off()
         }   
        }
      }
    }
   }
else{ 
  file.menu <- c("NCA --> Statistical analysis for 2x2x2 crossover study",
                 "NCA --> Statistical analysis for replicate study",
                 "NCA --> Statistical analysis for parallel study",
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
        ParaNCA.MIXmenu()
       }
    else {
    if (pick == 4){
        cat("\n")
        Singlego()
       }   
    else {
    if (pick == 5){
      cat("\n")
      cat("\n    Thank you for using bear!  Bye now. \n")}
      graphics.off()
       }
      }
    }
   }
 } 
} 