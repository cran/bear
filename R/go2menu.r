go2menu<- function() {

bear.setup()

ODAnalysis<-ODAnalysis
lin.AUC<-lin.AUC
lambda_z_calc<-lambda_z_calc
BE_LL<-BE_LL
BE_UL<-BE_UL
dosez<-dosez
DosingTau<-DosingTau
Tlastz<-Tlastz
xlabz<-xlabz
ylabz<-ylabz
pAUC<-pAUC
pAUC_start<-pAUC_start
pAUC_end<-pAUC_end
designtrace<-designtrace
IndivDP_output<-IndivDP_output

### for debug ###
## cat("\n BE_UL =",BE_UL,"\n\n")
## cat(" BE_LL =",BE_LL,"\n\n")
## show(lin.AUC);cat("\n\n")
## cat("lambda_z_calc =",lambda_z_calc,"\n\n");readline()
## show(xlabz);show(ylabz)
###

  cat("\n*** You have selected the following -> (new)\n")
  file.menu <- c("* Single dose study",
                 "- Multiple dose study",
                 "* Single dose study (with ODA)",
                 "- Multiple dose study (with ODA)",
                 "* Edit the setup files (if necessary)",
                 "- Genearate/export all demo datasets",
                 "# Quit",
                 "* View cheatsheet for bear setting")
   cat("\n")
  pick <- menu(file.menu, title = " << Top menu (select from 1~7) >> ", graphics=TRUE)
    if (pick == 1){
       cat("\n")
       designtrace<<-paste(designtrace,"Single-dosed,",sep=" ")
       Singlego()}
    
    if (pick == 2){
        cat("\n")
        designtrace<<-paste(designtrace,"Multiple-dosed,",sep=" ")
        Multiplego()
       }
       
    if (pick == 3){
       cat("\n\n")
       ODAnalysis<<-TRUE    ## here needs "<<-"; otherwise it won't work.
       alarm()
       cat("\n For now, ODA is only for non-replicated crossover and     \n")
       cat(" non-parallel. Thus, if you choose to run with a replicated  \n")
       cat(" or parallel dataset later, ODA will not be performed.       \n")
       readline(" Press Enter to proceed...\n")
       designtrace<<-paste(designtrace,"Single-dosed, with ODA,\n",sep=" ")
       Singlego()
       }
       
    if (pick == 4){
        cat("\n\n")
        ODAnalysis<<-TRUE   ## here needs "<<-"; otherwise it won't work.
        alarm()
        cat("\n For now, ODA is only for non-replicated crossover and     \n")
        cat(" non-parallel. Thus, if you choose to run with a replicated  \n")
        cat(" or parallel dataset later, ODA will not be performed.       \n")
        readline(" Press Enter to proceed...\n")
        designtrace<<-paste(designtrace,"Multiple-dosed, with ODA,\n",sep=" ")
        Multiplego()
       }
       
    if (pick == 5){
        graphics.off()
        ### readme.1st.bear<- system.file("extdata", "bear_setup_readme.txt", package="bear")
        ### file.show(readme.1st.bear,title="How to setup bear",encoding="UTF-8")    ### linux OS cannot work properly... -YJ
        bear.set<-readRDS("bear.setup.rds");bear.set<-edit(bear.set)
        while(bear.set[8,2]>bear.set[9,2]){
        readline("\n Error! pAUC_start must be less than pAUC_end!\n Press Enter to fix it.\n")
        bear.set<-edit(bear.set)}
        if(bear.set[1,2]<0 || bear.set[1,2]>6) bear.set[1,2]<-0
        if(bear.set[2,2]<0) {bear.set[2,2]<-0}
         else {if(bear.set[2,2]>0) bear.set[2,2]<-1}
        if(bear.set[10,2]<0) bear.set[10,2]<-0
         else {if(bear.set[10,2]>0) bear.set[10,2]<-1}
        if(bear.set[3,2]<50 || bear.set[3,2]>90) bear.set[3,2]<-80
        if(bear.set[7,2]<0 || bear.set[7,2]>0) bear.set[7,2]<-1
        while(bear.set[4,2]<0 || bear.set[5,2]<0 || bear.set[6,2]<0){
        readline("\n Error! Dose, Dosing interval or the last time of dosing\n cannot be less than zero! Press Enter to fix it.\n")
        bear.set<-edit(bear.set)}
        saveRDS(bear.set,"bear.setup.rds")
        plotz.set<-readRDS("plot.setup.rds")
        secondColumn<-as.character(plotz.set[,2])    ### to remove 'level' from a list of data.frame(). ---YJ
        xlabzz<-readline( "             The label of x-axis (time): ")
        if(xlabzz=="") xlabzz<-secondColumn[[1]]     ### if just press Enter key, no change will be made.
        ylabzz<-readline( " the label of y-axis (drug plasma conc): ")
        if(ylabzz=="") ylabzz<-secondColumn[[2]]     ### if just press Enter key, no change will be made.
        plotz.set<-data.frame(axis_label=c("x-axis","y-axis"),Setting=c(xlabzz,ylabzz))
        saveRDS(plotz.set,"plot.setup.rds");cat("\n\n")
        ### close("R Information")   ### don't work?  how to close this?
        go2menu()
       }

    if (pick == 6){
        demo_datasets_gen()
        graphics.off()
        go2menu()
       }
          
    if (pick == 7){
        cat("\n>  Thank you for using bear!  Bye now. \n\n")
        graphics.off()
       }
       
    if (pick == 8){
        cat("\n")
        ### alarm();readline("... Uhh? invalid selection. Press Enter and try again.\n")
        readme.1st.bear<- system.file("extdata", "bear_setup_readme.txt", package="bear")
        file.show(readme.1st.bear,title="How to setup bear",encoding="UTF-8")    ### linux OS cannot work properly... -YJ
        readline("\n\n... Press Enter to continue.")
        graphics.off()
        go2menu()
       }
}