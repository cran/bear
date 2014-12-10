description_version<-function(){
cat("\n")
Fname<-Fname
cat("..................................................\n\n")
cat(" .b                                                \n")
cat(" 88                                                \n")
cat(" 888oooo    .ooooo.   eoooo.   oooood8b            \n")
cat(" d88    88 d8(   )8b       )8b  888  8)            \n")
cat(" 888    88 888ooo88b o8o89888   888                \n")
cat(" 888    88 888       88(   88   888                \n")
cat("   o8ooo8   88bod8P   doooo8b  d888b             \n\n")
cat(" This report was generated using bear v2.6.4\n")
cat(" on:-",date(),"\n\n")
username<-Sys.info()[['user']]
cat(" R version:",gsub("R version ","",R.Version()[['version.string']],fixed=TRUE),"\n")
osname_version<-c(paste(Sys.info()[['sysname']],"-",Sys.info()[['version']],"\n",
                  Sys.info()[['release']],",",Sys.info()[['machine']]))
cat(" system OS:",osname_version,"\n")
cat(" user id:",username,"\n\n")
cat(" bear is developed by Hsin-ya Lee & Yung-jin Lee,\n")
cat(" and is under license of GPL-2|GPL-3.\n")
cat(" contact: Yung-jin Lee <mobilepk at gmail.com> \n\n")
cat(" citation:\n")
cat("  Lee, Hsin-ya and Lee, Yung-jin (2014). bear: Data Analysis Tool for\n")
cat("  Average Bioequivalence and Bioavailability. R package version 2.6.4,\n")
cat("  <URL: http://CRAN.R-project.org/package=bear>.\n")
cat("..................................................\n\n")
cat(" input data:",Fname,"\n\n")
### read setting files and display it ###
bear.set<-readRDS("bear.setup.rds")
plotz.set<-readRDS("plot.setup.rds")
secondColumn<-as.character(plotz.set[,2])
run.demo_txt<-"no"
study.type_txt<-"2x2x2 crossover"
dose.type_txt<-"single-dose"
lambda_z_txt<-""
lin.AUC_txt<-""
ODAnalysis_txt<-""
BE_criteria_txt<-"lower limit (%)"
Dose_txt<-"dose given"
Tau_txt<-"*multiple-dose only"
Tlast_txt<-"*multiple-dose only"
pAUC_txt<-""
pAUC_start_txt<-"the starting time of pAUC"
pAUC_end_txt<-"the end time of pAUC"
IndivDP_output_txt<-""

  if(bear.set[1,2]!=0) run.demo_txt   ="yes"
  if(bear.set[2,2]==1) study.type_txt ="replicate crossover"
  if(bear.set[2,2]==2) study.type_txt ="parallel study"
  if(bear.set[3,2]!=0) dose.type_txt  ="multiple dose"
  
  if(bear.set[4,2]==0) lambda_z_txt ="adj. R sq. (ARS)"
  if(bear.set[4,2]==1) lambda_z_txt ="Akaike info. criterion (AIC)"
  if(bear.set[4,2]==2) lambda_z_txt ="Two-Times-Tmax(TTT)"
  if(bear.set[4,2]==3) lambda_z_txt ="TTT and adj. ARS"
  if(bear.set[4,2]==4) lambda_z_txt ="TTT and AIC"
  if(bear.set[4,2]==5) lambda_z_txt ="manual selection"
  if(bear.set[4,2]==6) lambda_z_txt ="loaded from saved selection (.RData)"
  
  lin.AUC_txt<-ifelse(bear.set[5,2]==0, "linear-up/log-down", "all linear")
  ODAnalysis_txt<-ifelse(bear.set[7,2]==0, "no", "yes")
  pAUC_txt<-ifelse(bear.set[11,2]==0,"all AUC","truncated/partial AUC")
  IndivDP_output_txt<-ifelse(bear.set[14,2]==0,"no","yes")
  
  bear.set_txt<-data.frame(Methods=c("run demo","study design","single-/multiple-dose","lambda_z estimate",
                                     "trapezoidal AUC","BE criterion (LL)","ODA","dose","dosing interval*",
                                     "Tlast*","AUC method","pAUC_start#","pAUC_end#","IDP output"),
                           Setting=c(bear.set[1,2],bear.set[2,2],bear.set[3,2],bear.set[4,2],bear.set[5,2],
                                    bear.set[6,2],bear.set[7,2],bear.set[8,2],bear.set[9,2],bear.set[10,2],
                                    bear.set[11,2],bear.set[12,2],bear.set[13,2],bear.set[14,2]),
                           which_is=c(run.demo_txt,study.type_txt,dose.type_txt,lambda_z_txt,lin.AUC_txt,
                                     BE_criteria_txt,ODAnalysis_txt,Dose_txt,Tau_txt,Tlast_txt,pAUC_txt,
                                     pAUC_start_txt,pAUC_end_txt,IndivDP_output_txt))
                        
cat(" -------------------  Project Settings ------------------\n\n");show(bear.set_txt);cat("\n")
### cat(paste(c(" The plot label of x-axis ->",secondColumn[[1]],"\n  and the label of y-axis ->",secondColumn[[2]])))
### cat(" input dataset:",Fname,"\n")  ### moved to description_version(), since v2.6.2
cat(" *: for multiple-dose study only.\n")
cat(" #: for truncated/partial AUC only.\n")
cat(" --------------------------------------------------------\n\n")
### end of read setting files and display it ###
}