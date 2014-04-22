go2menu<- function() {

bear.setup()
options(warn=-1)

ODAnalysis<-ODAnalysis        ### No (0, default) or Yes (1)
lin.AUC<-lin.AUC              ### linear-up/log-down (0, default), all linear (1)
lambda_z_calc<-lambda_z_calc
BE_LL<-BE_LL
BE_UL<-BE_UL
dosez<-dosez
DosingTau<-DosingTau
Tlastz<-Tlastz
xlabz<-xlabz
ylabz<-ylabz
pAUC<-pAUC                     ### No (0, default) or Yes (1)
pAUC_start<-pAUC_start
pAUC_end<-pAUC_end
designtrace<-designtrace
IndivDP_output<-IndivDP_output ### No (0, default) or Yes (1)
run.demo<-run.demo             ### No (0, default) or Yes (1)
study.type<-study.type         ### 2x2x2 crossover (0, default), replicate (1) or parallel (2)
dose.type<-dose.type           ### single-dose (0, default) or multiple-dose (1)
multiple<-NULL
replicate<-NULL
parallel<-NULL
if(study.type==1) replicated=TRUE
if(study.type==2) parallel=TRUE
multiple=ifelse(dose.type==1, TRUE, FALSE)

back.from.banova<-NULL
back.from.banova<<-FALSE

Fname<-NULL                   ### Fname: the name of the input dataset file 
Fname<<-""

###
  cat("\n*** You have selected the following -> (new)\n")
  file.menu <- c("* Start data analysis     ",
                 "* Edit setup files        ",
                 "* Export all demo dataset ",
                 "# Quit                    ")
   cat("\n")
  pick <- menu(file.menu, title = " << Top menu (select from 1~4) >> ", graphics=TRUE)
    if (pick == 1){
       if(dose.type==0){  ### this is for the single-dose
       cat("\n")
       if(ODAnalysis){
          designtrace<<-paste(designtrace,"Single-dosed, with ODA,\n",sep=" ")}
       else{
          designtrace<<-paste(designtrace,"Single-dosed,",sep=" ")}
          Singlego()}
       else{             ### this is for the multiple-dose
        cat("\n")
        if(ODAnalysis){
          designtrace<<-paste(designtrace,"Multiple-dosed, with ODA,\n",sep=" ")}
        else{
          designtrace<<-paste(designtrace,"Multiple-dosed,",sep=" ")}
          Multiplego()   ### this will call Singlego() with 'multiple = TRUE'  -YJ
       }
    }
       
    if (pick == 2){
        graphics.off()
        ### readme.1st.bear<- system.file("extdata", "bear_setup_readme.txt", package="bear")
        ### file.show(readme.1st.bear,title="How to setup bear",encoding="UTF-8")    ### linux OS cannot work properly... -YJ
        ### display cheatsheet as graphic; well great under Windows but not under linux... too bad.
        par(mar=c(0, 0, 0, 0)) 
        plot(0, xlim=c(0, 210), ylim=c(0, 297), col="white")  ### will show ugly border line. too bad!  -YJ
        bear_setup_display<-readPNG(system.file("img","bear_setup.png",package="bear"),TRUE)
        ### bear_setup_display<-readPNG("bear_setup.png",TRUE)  ### for testing
        grid.raster(bear_setup_display,width=unit(1,"npc"),height=unit(1,"npc"))   ### this one works great without showing border. -YJ
        ### ### readline(" Press Enter to continue...");graphics.off()  ### for testing
        ###
        bear.set<-readRDS("bear.setup.rds");bear.set<-edit(bear.set)
        while(bear.set[12,2]>=bear.set[13,2]){
        readline("\n Error! pAUC_start must be less than pAUC_end!\n Press Enter to fix it.\n")
        bear.set<-edit(bear.set)}
        if(bear.set[1,2]<0 || bear.set[1,2]>1) bear.set[1,2]<-0
        if(bear.set[2,2]<0 || bear.set[2,2]>2) bear.set[2,2]<-0
        if(bear.set[3,2]<0 || bear.set[3,2]>1) bear.set[3,2]<-0
        if(bear.set[4,2]<0 || bear.set[4,2]>6) bear.set[4,2]<-0
        if(bear.set[5,2]<0 || bear.set[5,2]>1) bear.set[5,2]<-0
        if(bear.set[7,2]<0 || bear.set[7,2]>1) bear.set[7,2]<-0
        if(bear.set[14,2]<0 || bear.set[14,2]>1) bear.set[14,2]<-0
        if(bear.set[6,2]<50 || bear.set[6,2]>90) bear.set[6,2]<-80
        if(bear.set[11,2]<0 || bear.set[11,2]>1) bear.set[11,2]<-0
        while(bear.set[8,2]<0 || bear.set[9,2]<0 || bear.set[10,2]<0){
        readline("\n Error! The dose, dosing interval or the last time of dosing\n cannot be less than zero! Press Enter to fix it.\n")
        bear.set<-edit(bear.set)}
        while(bear.set[2,2]==1 && bear.set[3,2]==1){
        readline("\n Error! The replicated crossover cannot have multiple-dose\n design. Press Enter to fix it.\n")
        bear.set<-edit(bear.set)}
        saveRDS(bear.set,"bear.setup.rds");graphics.off()
        plotz.set<-readRDS("plot.setup.rds")
        secondColumn<-as.character(plotz.set[,2])    ### to remove 'level' from a list of data.frame(). ---YJ
        ### xlabzz<-readline( "             The label of x-axis (time): ")
        cat(" Press Enter if not to change these.\n\n")
        xlabzz<-readline(paste("  [",secondColumn[[1]],"] (time) -> "))
        if(xlabzz=="") xlabzz<-secondColumn[[1]]     ### if just press Enter key, no change will be made.
        ### ylabzz<-readline( " the label of y-axis (drug plasma conc): ")
        ylabzz<-readline(paste("  [",secondColumn[[2]],"] (conc.) -> "))
        if(ylabzz=="") ylabzz<-secondColumn[[2]]     ### if just press Enter key, no change will be made.
        plotz.set<-data.frame(axis_label=c("x-axis","y-axis"),Setting=c(xlabzz,ylabzz))
        saveRDS(plotz.set,"plot.setup.rds");cat("\n\n")
        ### close("R Information")   ### don't work?  how to close this?
        ### graphics.off()   ### if there is any opened graphic on screen
        go2menu()
       }

    if (pick == 3){
        demo_datasets_gen()
        graphics.off()
        go2menu()
       }
          
    if (pick == 4){
        cat("\n>  Thank you for using bear!  Bye now. \n\n")
        graphics.off()
       }
}