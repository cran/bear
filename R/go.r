# Menu List of bear (BE/BA for R, bear)
go<-function()
{
options(warn=-1)

OutputFilez()
Demo<-NULL
Demo<<-FALSE
ODAnalysis<-NULL
ODAnalysis<<-FALSE

if (noquote(unlist(format(.Platform)))[1] == "unix") {
        windows <<- function(record) {
        }
     }  
     
cat("\n")
  file.menu <- c("- Single dose study ",
                 "- Multiple dose study",
                 "- *Single dose study   (with ODA)",
                 "- *Multiple dose study (with ODA)",
                 "- Quit",
                 "ps. ODA: outlier detection analysis 
   (absolutely not recommended for routine use!)")
   cat("\n")
  pick <- menu(file.menu, title = " << Top menu (Select from 1~5) >> ")
    if (pick == 1){
       cat("\n")
       Singlego()}
    
    if (pick == 2){
        cat("\n")
        Multiplego()
       }
       
    if (pick == 3){
       cat("\n\n")
       ODAnalysis<<-TRUE    ## here needs "<<-"; otherwise it won't work.
       alarm()
       cat("\n For now, ODA is only for non-replicated crossover and     \n")
       cat(" non-parallel. Thus, if you choose to run with a replicated  \n")
       cat(" or parallel dataset later, ODA will not be performed.       \n")
       readline(" Press Enter to continue...\n")
       Singlego()
       }
       
    if (pick == 4){
        cat("\n\n")
        ODAnalysis<<-TRUE   ## here needs "<<-"; otherwise it won't work.
        alarm()
        cat("\n For now, ODA is only for non-replicated crossover and     \n")
        cat(" non-parallel. Thus, if you choose to run with a replicated  \n")
        cat(" or parallel dataset later, ODA will not be performed.       \n")
        readline(" Press Enter to continue...\n")
        Multiplego()
       }
       
    if (pick == 5){
        cat("\n>  Thank you for using bear!  Bye now. \n\n")
        graphics.off()
       }
       
    if (pick == 6){
        cat("\n")
        alarm()
        readline("... Uhh? invalid selection. Press Enter and try again.\n")
        graphics.off()
        go()
       }
}
 