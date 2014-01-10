OutputFilez<-function(){

##
## these lines are to avoid the error messages of "not visible binding ..." with codetool
##
   anova_output_xfile<-NULL                            ### BANOVAoutput.r
   statSum_output_xfile<-NULL                          ### NCAoutput.r
   nca_output_xfile<-NULL                              ### NCAoutput.r
   lm_stat_xfile<-NULL                                 ### RepMIXoutput.r
   lme_stat_xfile<-NULL                                ### RepMIXoutput.r
   pivotal_output_xfile<-NULL                          ### NCAoutput.r
   misc_pk_output_xfile<-NULL                          ### NCAoutput.r
   nca_plot_xfile<-NULL                                ### NCAplot.r
   ODplot_output_xfile<-NULL                           ### BANOVAoutput.r
   oda_output_xfile<-NULL                              ### BANOVA.r
   lambda_z_regression_lines<-NULL                     ### NCAselect.r (or NCA.r) & ARS.r & TTT.r &...
   lambda_z_regr_select_ref<-NULL                      ### NCA.r, ARS.r, aic.r, TTT.r, TTTAIC.r & TTTARS.r
   lambda_z_regr_select_test<-NULL                     ### NCA.r, ARS.r, aic.r, TTT.r, TTTAIC.r & TTTARS.r
   NCAsave_pivotal_param_RData_export<-NULL            ### NCAsave.r, NCAoutput.r
   NCAsave_pivotal_param_csv_export<-NULL              ### NCAsave.r, NCAoutput.r
   IDP_output_ref<-NULL                                ### Indiv_dp.output()
   IDP_output_test<-NULL                               ### Indiv_dp.output()
   
## create random batch number & system.date() for the head of output files

   xFile_ext<-""
   xFile_ext<-paste(xFile_ext,sample(1001001:9786999,1,replace=F),"_",Sys.Date(),"_",sep="") ## as random run batch#
   ### xFile_ext<-paste(xFile_ext,"_",sep="")
   ### xFile_ext<-paste(xFile_ext,Sys.Date(),sep="")                          ## followed by the system date
   ### xFile_ext<-paste(xFile_ext,"_",sep="")
   
## start to assign middle part of output file names now.

   anova_output_xfile<-paste(xFile_ext,"anova",sep="")
   statSum_output_xfile<-paste(xFile_ext,"stat_sum",sep="")
   nca_output_xfile<-paste(xFile_ext,"nca_pk",sep="")
   lm_stat_xfile<-paste(xFile_ext,"lm_stat",sep="")
   lme_stat_xfile<-paste(xFile_ext,"lme_stat",sep="")
   oda_output_xfile<-paste(xFile_ext,"oda_stat",sep="")
   pivotal_output_xfile<-paste(xFile_ext,"pivotal",sep="")
   misc_pk_output_xfile<-paste(xFile_ext,"misc_pk",sep="")
   lambda_z_regr_select_ref<-paste(xFile_ext,"lambda_z_regr_select_Ref",sep="")
   lambda_z_regr_select_test<-paste(xFile_ext,"lambda_z_regr_select_Test",sep="")
   lambda_z_regression_lines<-paste(xFile_ext,"lambda_z_regr_lines_plots",sep="")
   nca_plot_xfile<-paste(xFile_ext,"conc_time_plots",sep="")
   ODplot_output_xfile<-paste(xFile_ext,"oda_plots",sep="")
   NCAsave_pivotal_param_RData_export<-paste(xFile_ext,"NCA_pivotal_param_export",sep="")
   NCAsave_pivotal_param_csv_export<-paste(xFile_ext,"NCA_pivotal_param_export",sep="")
   IDP_output_ref<-paste(xFile_ext,"ref_IDP_output",sep="")
   IDP_output_test<-paste(xFile_ext,"test_IDP_output",sep="")
   
## start to add file extension for output files (.txt, .csv, .pdf, etc)

   anova_output_xfile<<-paste(anova_output_xfile,".txt",sep="")
   statSum_output_xfile<<-paste(statSum_output_xfile,".txt",sep="")
   nca_output_xfile<<-paste(nca_output_xfile,".txt",sep="")
   lm_stat_xfile<<-paste(lm_stat_xfile,".txt",sep="")
   lme_stat_xfile<<-paste(lme_stat_xfile,".txt",sep="")
   oda_output_xfile<<-paste(oda_output_xfile,".txt",sep="")
   pivotal_output_xfile<<-paste(pivotal_output_xfile,".csv",sep="")
   misc_pk_output_xfile<<-paste(misc_pk_output_xfile,".csv",sep="")
   lambda_z_regr_select_ref<<-paste(lambda_z_regr_select_ref,".csv",sep="")
   lambda_z_regr_select_test<<-paste(lambda_z_regr_select_test,".csv",sep="")
   lambda_z_regression_lines<<-paste(lambda_z_regression_lines,".pdf",sep="")
   nca_plot_xfile<<-paste(nca_plot_xfile,".pdf",sep="")
   ODplot_output_xfile<<-paste(ODplot_output_xfile,".pdf",sep="")
   NCAsave_pivotal_param_RData_export<<-paste(NCAsave_pivotal_param_RData_export,".RData",sep="")
   NCAsave_pivotal_param_csv_export<<-paste(NCAsave_pivotal_param_csv_export,".csv",sep="")
   IDP_output_ref<<-paste(IDP_output_ref,".csv",sep="")
   IDP_output_test<<-paste(IDP_output_test,".csv",sep="")
}