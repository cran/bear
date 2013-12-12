bear.setup<- function() {
options(warn=-1)
OutputFilez()
Demo<-NULL
Demo<<-FALSE
ODAnalysis<-NULL
ODAnalysis<<-FALSE
designtrace<-NULL
designtrace<<-""
lin.AUC<-NULL
lin.AUC<<-FALSE
lambda_z_calc<-NULL
BE_LL<-NULL
BE_UL<-NULL
dosez<-NULL
xlabz<-NULL
ylabz<-NULL
DosingTau<-NULL
Tlastz<-NULL
pAUC<-NULL
pAUC<<-FALSE
pAUC_start<-NULL
pAUC_end<-NULL
req.closezz<-FALSE

if(!file.exists("bear_setup_readme.txt")){
     req.closezz<-TRUE
     zz <- file("bear_setup_readme.txt", open="wt")
     sink(zz,split=TRUE)
}  ### set 'split=TRUE' to see output on the screen simultaneously. -YJ

if(file.exists("bear.setup.rds")){
     cat("\n\n  How to setup bear:\n\n")
     cat("----------- lambda_z estimation ---------- \n")
     cat(" Methods                            Values \n")
     cat("------------------------------------------ \n")
     cat(" Adjusted R sq. (ARS)................. 0   \n")
     cat(" Akaike information criterion (AIC)... 1   \n")
     cat(" Two-Times-Tmax(TTT).................. 2   \n")
     cat(" TTT and ARS.......................... 3   \n")
     cat(" TTT and AIC.......................... 4   \n")
     cat(" Select 2-6 data points manually...... 5   \n")
     cat(" Load previous selection (.RData)..... 6   \n")
     cat("------------------------------------------\n\n")
     cat("------------- AUC calculation ------------ \n")
     cat(" Methods                            Values \n")
     cat("------------------------------------------ \n")
     cat(" linear-up/log-down trapezoidal....... 0   \n")
     cat(" all linear trapezoidal.............else   \n")
     cat("------------------------------------------ \n\n")
     cat("-------------- BE criterion -------------- \n")
     cat(" lower limit (LL as %).......such as..80   \n")
     cat(" upper limit (UL) = 1/LL; so no need to set.\n")
     cat("------------------------------------------ \n\n")
     cat("----------- multiple-dose only -----------\n")
     cat("*Tlast: the time of the last dose given for\n")
     cat(" the multiple-dose study; will be ignored  \n")
     cat(" when it is a single-dose study; same as   \n")
     cat(" Dosing interval.                          \n")
     cat("*x-axis label and y-axis label will be used\n")
     cat(" in the plots of drug plasma conc. vs. time.\n")
     cat("------------------------------------------ \n\n")
     cat("-------------- partial AUC  -------------- \n")
     cat(" pAUC: use partial AUC? (0 - No, else - Yes)\n")
     cat(" pAUC_start: the starting time of pAUC      \n")
     cat(" pAUC_end: the end time of pAUC             \n")
     cat(" pAUC_start and pAUC_end will be ignored if \n")
     cat(" pAUC is not used (= 0).                    \n")
     cat("------------------------------------------ \n\n")
     bear.set<-readRDS("bear.setup.rds")
  }
 else{
     cat("\n\n  How to setup bear:\n\n")
     cat("------------ lambda_z estimate ----------- \n")
     cat(" Methods                            Values \n")
     cat("------------------------------------------ \n")
     cat(" Adjusted R sq. (ARS)................. 0   \n")
     cat(" Akaike information criterion (AIC)... 1   \n")
     cat(" Two-Times-Tmax(TTT).................. 2   \n")
     cat(" TTT and ARS.......................... 3   \n")
     cat(" TTT and AIC.......................... 4   \n")
     cat(" Select 2-6 data points manually...... 5   \n")
     cat(" Load previous selection (.RData)..... 6   \n")
     cat("------------------------------------------\n\n")
     cat("------------- trapezoidal AUC ------------ \n")
     cat(" Methods                            Values \n")
     cat("------------------------------------------ \n")
     cat(" linear-up/log-down trapezoidal....... 0   \n")
     cat(" all linear trapezoidal.............else   \n")
     cat("------------------------------------------ \n\n")
     cat("-------------- BE criterion -------------- \n")
     cat(" lower limit (LL as %).......such as..80   \n")
     cat(" upper limit (UL) = 1/LL; so no need to set.\n")
     cat("------------------------------------------ \n\n")
     cat("*Tlast: the time of the last dose given for\n")
     cat(" the multiple-dose study; will be ignored  \n")
     cat(" when it is a single-dose study; same as   \n")
     cat(" Dosing interval; leave it as it was.      \n")
     cat("*x-axis label and y-axis label will be used\n")
     cat(" in the plots of drug plasma conc. vs. time.\n\n")
     cat("-------------- partial AUC  -------------- \n")
     cat(" pAUC: use partial AUC? (0 - No, else - Yes)\n")
     cat(" pAUC_start: the starting time of pAUC      \n")
     cat(" pAUC_end: the end time of pAUC             \n")
     cat(" pAUC_start and pAUC_end will be ignored if \n")
     cat(" pAUC is not used (= 0).                    \n")
     cat("------------------------------------------ \n\n")

     bear.set<-data.frame(Methods=c("lambda_z estimate","trapezoidal AUC","BE criterion (LL)","Dose","Dosing Interval","Tlast",
                                    "pAUC","pAUC_start","pAUC_end"),
                          Setting=c(0,0,80,100,24,120,0,0,12))
     saveRDS(bear.set,"bear.setup.rds")
}
if(file.exists("plot.setup.rds")){
     plotz.set<-readRDS("plot.setup.rds")
     secondColumn<-as.character(plotz.set[,2])    ### to remove 'level' from a list of data.frame(). ---YJ
        ### ref. link: http://stackoverflow.com/questions/16770753/remove-level-from-a-list-extract-to-a-data-fram (THX!)
     
}
else{
    plotz.set<-data.frame(axis_label=c("x-axis","y-axis"),Setting=c("Time after Dosing (hr)","Drug X Plasma Conc. (ng/mL)"))
    saveRDS(plotz.set,"plot.setup.rds")
    secondColumn<-as.character(plotz.set[,2])   ### to remove 'level' from a list of data.frame(). ---YJ
    }   ### ref. link: http://stackoverflow.com/questions/16770753/remove-level-from-a-list-extract-to-a-data-fram (THX!)
  
  lambda_z_calc<<-bear.set[1,2]
  lin.AUC<<-ifelse(bear.set[2,2]==0, FALSE, TRUE)
  BE_LL<<- bear.set[3,2]/100
  BE_UL<<- 1./(bear.set[3,2]/100)   ### here cannot write as "BE_UL<<- 1./BE_LL"; otherwise, BE_UL is NaN!  --YJ
  dosez<<- bear.set[4,2];DosingTau<<- bear.set[5,2];Tlastz<<- bear.set[6,2]
  pAUC<<-ifelse(bear.set[7,2]==0, FALSE, TRUE)
  pAUC_start<<-bear.set[8,2];pAUC_end<<-bear.set[9,2]
  xlabz<<- secondColumn[[1]];ylabz<<- secondColumn[[2]]

  lambda_z_txt<-""
  lin.AUC_txt<-""
  BE_criteria_txt<-"lower limit"
  Dose_txt<-"dose given"
  Tau_txt<-"*multiple-dose only"
  Tlast_txt<-"*multiple-dose only"
  pAUC_txt<-""
  pAUC_start_txt<-"the starting time of pAUC"
  pAUC_end_txt<-"the end time of pAUC"
  
  if(bear.set[1,2]==0) lambda_z_txt ="adj. R sq. (ARS)"
  if(bear.set[1,2]==1) lambda_z_txt ="Akaike info. criterion (AIC)"
  if(bear.set[1,2]==2) lambda_z_txt ="Two-Times-Tmax(TTT)"
  if(bear.set[1,2]==3) lambda_z_txt ="TTT and adj. ARS"
  if(bear.set[1,2]==4) lambda_z_txt ="TTT and AIC"
  if(bear.set[1,2]==5) lambda_z_txt ="manual selection"
  if(bear.set[1,2]==6) lambda_z_txt ="load previous selection (.RData)"
  if(bear.set[1,2]<0 || bear.set[1,2]>6) lambda_z_txt ="error setting!"
  lin.AUC_txt<-ifelse(bear.set[2,2]==0, "linear-up/log-down", "all linear")
  pAUC_txt<-ifelse(bear.set[7,2]==0,"full AUC","partial AUC")
  
  bear.set_txt<-data.frame(Methods=c("lambda_z estimate","trapezoidal AUC","BE criterion (LL)","Dose","Dosing Interval","Tlast",
                                    "pAUC","pAUC_start","pAUC_end"),
                          Setting=c(bear.set[1,2],bear.set[2,2],bear.set[3,2],bear.set[4,2],bear.set[5,2],bear.set[6,2],bear.set[7,2],
                                    bear.set[8,2],bear.set[9,2]),
                          which_is=c(lambda_z_txt,lin.AUC_txt,BE_criteria_txt,Dose_txt,Tau_txt,Tlast_txt,pAUC_txt,pAUC_start_txt,
                                     pAUC_end_txt))

  cat("\n --- Current settings ---\n\n");show(bear.set_txt);cat("\n")
  cat(paste(c(" The plot label of x-axis ->",secondColumn[[1]],"\n  and the label of y-axis ->",secondColumn[[2]],"\n\n")))  ### here cannot put 'xlabz' & 'ylabz'!  --YJ
  cat("(1) User can change these settings from the top menu.\n")
  cat("    Resize your R console or terminal if you cannot  \n")
  cat("    see all settings.\n")
  cat("(2) Select [# Edit the setup files] from the top menu \n")
  cat("    and then scroll up this terminal to see more.\n")
  cat("(3) All these settings are not completely applied when\n")
  cat("    running with demo dataset.\n")
  cat("(4) *** Please be careful to set up correct methods; \n")
  cat("    otherwise it may cause bear crashed accidentally.\n\n")
  if(req.closezz){sink();close(zz)}
}