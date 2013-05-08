# Menu List of bear (BE/BA for R, bear)
go<-function()
{
options(warn=-1)
options(width=100)

OutputFilez()
Demo<-NULL
Demo<<-FALSE
ODAnalysis<-NULL
ODAnalysis<<-FALSE

if (noquote(unlist(format(.Platform)))[1] == "unix") {
        windows <<- function(record) {
        }
     }  
####
par(mar=c(0, 0, 0, 0)) 
plot(0, xlim=c(0, 210), ylim=c(0, 297), col="white")  ### will show ugly border line. too bad!  -YJ
logo<-readPNG(system.file("img","bear_logo-2013.png",package="bear"),TRUE)
grid.raster(logo,width=unit(1,"npc"),height=unit(1,"npc"))     ### this one works great without showing border. -YJ
text(100,100,"Welcome to using bear v2.5.5",cex = 1.2)
text(100,70,"bear was developed by Hsin-ya Lee (HY) & Yung-jin Lee (YJ).",cex = 1.2)
text(100,60,"Kaohsiung Veterans General Hospital (HY) &",cex = 1.2)
text(100,50,"ptpc inc. (YJ),",cex = 1.2)
text(100,40,"Kaohsiung City, Taiwan",cex = 1.2)
### readline(" Press Enetr to continue...");dev.off()
Sys.sleep(3.6);dev.off()
####     
cat("\n")
  file.menu <- c("- Single dose study ",
                 "- Multiple dose study",
                 "- *Single dose study   (with ODA)",
                 "- *Multiple dose study (with ODA)",
                 "- Quit",
                 "ps. ODA: outlier detection analysis 
   (absolutely not recommended for routine use!)")
   cat("\n")
  pick <- menu(file.menu, title = " << Top menu (Select from 1~5) >> ", graphics=TRUE)
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
 