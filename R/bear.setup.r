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
IndivDP_output<-NULL
IndivDP_output<<-FALSE
run.demo<-NULL
run.demo<<-FALSE
study.type<-NULL
dose.type<-NULL

### readme.1st.bear<- system.file("extdata", "bear_setup_readme.txt", package="bear")
### file.show(readme.1st.bear,encoding="UTF-8")
cat("\n If the next step goes crahsed, please make sure the working directory\n has writing permission. Use setwd() to set your working directory.\n\n")

if(file.exists("bear.setup.rds")){
     bear.set<-readRDS("bear.setup.rds")
     ### if(nrow(bear.set)==9) {              ### when it was the previous version; then add the last row.  --YJ
     ### cat("\n  The previous bear settig file has been modified.\n  IDP_output setting has been added to current setting.\n\n")
     ### df<-data.frame(Methods=c("IndivDP_output"),Setting=c(0))
     ### bear.set<-rbind(bear.set,df)
     ### saveRDS(bear.set,"bear.setup.rds")   ### this will keep the same setting as previously saved.
     ### }
     if(nrow(bear.set)<14){           ### total 14 items; this will erase all previous setting
     cat("\n  The previous set-up file has been updated.\n  Please edit the set-up file again.\n\n")
     bear.set<-data.frame(Methods=c("run demo","study design","single-/multiple-dose","lambda_z estimate",
                                    "trapezoidal AUC","BE criterion (LL)","ODA","dose","dosing interval",
                                    "Tlast","pAUC","pAUC_start","pAUC_end","indivDP_output"),
                          Setting=c(0,0,0,0,0,80,0,1e+05,24,120,0,121,128,0))
     saveRDS(bear.set,"bear.setup.rds")
     }
  }
 else{
     bear.set<-data.frame(Methods=c("run demo","study design","single-/multiple-dose","lambda_z estimate",
                                    "trapezoidal AUC","BE criterion (LL)","ODA","dose","dosing interval",
                                    "Tlast","pAUC","pAUC_start","pAUC_end","indivDP_output"),
                          Setting=c(0,0,0,0,0,80,0,1e+05,24,120,0,121,128,0))
     saveRDS(bear.set,"bear.setup.rds")
}
if(file.exists("plot.setup.rds")){
     plotz.set<-readRDS("plot.setup.rds")
     secondColumn<-as.character(plotz.set[,2])    ### to remove 'level' from a list of data.frame(). ---YJ
     ### ref. link: http://stackoverflow.com/questions/16770753/remove-level-from-a-list-extract-to-a-data-fram (THX!)
}
else{
    plotz.set<-data.frame(axis_label=c("x-axis","y-axis"),Setting=c("Time after Dosing (hr)",
                                       "DrugX Plasma Conc. (ng/mL)"))
    saveRDS(plotz.set,"plot.setup.rds")
    secondColumn<-as.character(plotz.set[,2])   ### to remove 'level' from a list of data.frame(). ---YJ
}   ### ref. link: http://stackoverflow.com/questions/16770753/remove-level-from-a-list-extract-to-a-data-fram (THX!)

  run.demo<<-ifelse(bear.set[1,2]==0, FALSE, TRUE)  ### FALSE (0) or TRUE(1)
  study.type<<-bear.set[2,2]
  dose.type<<-bear.set[3,2]
  lambda_z_calc<<-bear.set[4,2]
  lin.AUC<<-ifelse(bear.set[5,2]==0, FALSE, TRUE)
  ODAnalysis<<-ifelse(bear.set[7,2]==0, FALSE, TRUE)
  IndivDP_output<<-ifelse(bear.set[14,2]==0, FALSE, TRUE)
  BE_LL<<- bear.set[6,2]/100
  BE_UL<<- 1./(bear.set[6,2]/100)   ### here cannot write as "BE_UL<<- 1./BE_LL"; otherwise, BE_UL will become 'NaN'!  --YJ
  dosez<<- bear.set[8,2];DosingTau<<- bear.set[9,2];Tlastz<<- bear.set[10,2]
  pAUC<<-ifelse(bear.set[11,2]==0, FALSE, TRUE)
  pAUC_start<<-bear.set[12,2];pAUC_end<<-bear.set[13,2]
  xlabz<<- secondColumn[[1]];ylabz<<- secondColumn[[2]]

  run.demo_txt<-"no"
  study.type_txt<-"2x2x2 crossover"
  dose.type_txt<-"single-dose"
  lambda_z_txt<-""
  lin.AUC_txt<-""
  ODAnalysis_txt<-""
  BE_criteria_txt<-"lower limit"
  Dose_txt<-"dose given"
  Tau_txt<-"*multiple-dose only"
  Tlast_txt<-"*multiple-dose only"
  pAUC_txt<-""
  pAUC_start_txt<-"the starting time of pAUC"
  pAUC_end_txt<-"the end time of pAUC"
  IndivDP_output_txt<-""
  
  if(bear.set[1,2]!=0) run.demo_txt   ="yes"
  if(bear.set[2,2]==1) study.type_txt ="replicate study"
  if(bear.set[2,2]==2) study.type_txt ="parallel study"
  if(bear.set[3,2]!=0) dose.type_txt  ="multiple dose"
  
  if(bear.set[4,2]==0) lambda_z_txt ="adj. R sq. (ARS)"
  if(bear.set[4,2]==1) lambda_z_txt ="Akaike info. criterion (AIC)"
  if(bear.set[4,2]==2) lambda_z_txt ="Two-Times-Tmax(TTT)"
  if(bear.set[4,2]==3) lambda_z_txt ="TTT and adj. ARS"
  if(bear.set[4,2]==4) lambda_z_txt ="TTT and AIC"
  if(bear.set[4,2]==5) lambda_z_txt ="manual selection"
  if(bear.set[4,2]==6) lambda_z_txt ="load previous selection (.RData)"
  
  lin.AUC_txt<-ifelse(bear.set[5,2]==0, "linear-up/log-down", "all linear")
  ODAnalysis_txt<-ifelse(bear.set[7,2]==0, "no", "yes")
  pAUC_txt<-ifelse(bear.set[11,2]==0,"all AUC","truncated/partial AUC")
  IndivDP_output_txt<-ifelse(bear.set[14,2]==0,"no","yes")
  
  bear.set_txt<-data.frame(Methods=c("run demo?","study design?","single-/multiple-dose","lambda_z estimate",
                                     "trapezoidal AUC","BE criterion (LL)","ODA?","dose?","dosing interval",
                                     "Tlast","pAUC?","pAUC_start","pAUC_end","IDP output?"),
                          Setting=c(bear.set[1,2],bear.set[2,2],bear.set[3,2],bear.set[4,2],bear.set[5,2],
                                    bear.set[6,2],bear.set[7,2],bear.set[8,2],bear.set[9,2],bear.set[10,2],
                                    bear.set[11,2],bear.set[12,2],bear.set[13,2],bear.set[14,2]),
                          which_is=c(run.demo_txt,study.type_txt,dose.type_txt,lambda_z_txt,lin.AUC_txt,
                                     BE_criteria_txt,ODAnalysis_txt,Dose_txt,Tau_txt,Tlast_txt,pAUC_txt,
                                     pAUC_start_txt,pAUC_end_txt,IndivDP_output_txt))

  cat("\n -------------------  Current settings ------------------\n\n");show(bear.set_txt);cat("\n")
  cat(paste(c(" The plot label of x-axis ->",secondColumn[[1]],"\n  and the label of y-axis ->",secondColumn[[2]])))  ### here cannot put 'xlabz' & 'ylabz'!  --YJ
  cat("\n --------------------------------------------------------\n\n")
}