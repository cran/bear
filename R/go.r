# List of bear (Bioequivalence and Bioavailability) Menu
go<-function()
{
options(warn=-1)
if (noquote(unlist(format(.Platform)))[1] == "unix") {
        windows <<- function(record) {
        }
     }  
     
cat("\n")
  file.menu <- c("Single dose study ",
                 "Multiple dose study",
                 "Quit")
   cat("\n")
  pick <- menu(file.menu, title = " << Top menu >> ")
    if (pick == 1){
      cat("\n")
        Singlego()}

    else {
    if (pick == 2){
        cat("\n")
        Multiplego()
       }
    else {
    if (pick == 3){
        cat("\nThank you for using bear!  Bye now. \n\n")}
         }  
      }
}
 