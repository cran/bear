# List of bear (Bioequivalence and Bioavailability) Menu
go<-function()
{
options(warn=-1)
if (noquote(unlist(format(.Platform)))[1] == "unix") {
        windows <<- function(record) {
        }
     }  
    
      
cat("\n")
  file.menu <- c("Sample size estimation for ABE ",
                 "Noncompartmental analysis (NCA)",
                 "Statistical analysis (ANOVA(lm), 90%CI...)",
                 "NCA --> Statistical analysis (ANOVA(lm), 90%CI...)",
                 "Quit")
   cat("\n")
  pick <- menu(file.menu, title = " << Top menu >> ")
    if (pick == 1){
      cat("\n")
        sizemenu()}

    else {
    if (pick == 2){
        cat("\n")
        NCAmenu()
       }
    else {
    if (pick == 3){
        cat("\n")
        BANOVAmenu()
         }
   else {
    if (pick == 4){
        NCA.BANOVAmenu()
         }
    else {
    if (pick == 5){
        cat("\nThank you for using bear!  Bye now. \n\n")}
         }  
      }
   }
 }
}  