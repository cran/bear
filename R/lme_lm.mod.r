lme_lm.mod<-function(target, inputdata, lme.switch)
{

dose.type<-dose.type
study.type<-study.type
####
ctrl <- lmeControl(opt='optim', msMaxIter=1000)
        ### added 'msMaxIter=1000' since v.2.6.4 because it still causes converge limit reached...
        ### default was 'nlminb' for lme(); but it will fail to converge frequently;
        ### changed back to old default 'optim' & try
        ### ref. link: ??lmeControl --> click 'nlem::lmeControl' for more inf.
        ### & https://stats.stackexchange.com/questions/40647/lme-error-iteration-limit-reached
        ### same for following lme().  --YJ
####

if(study.type==2){  ### means parallel study; no matter what it is single-dose or multiple-dose
  target.lm<-lm(target ~  drug, data=inputdata)
}
else{ 
  ### target.lme<-lme(target ~ seq +  prd + drug , random=~drug - 1|subj, control=ctrl, ### doesn't for lme()!  weird...  -YJ
  ###              weights=varIdent(form = ~ 1 | drug), 
  ###              data=inputdata, method="REML")
  switch(lme.switch,    ### the following cannot use numeric as expression, such 1, 2, 3,...  --YJ
    A  = {target.lme<-lme(Cmax ~ seq +  prd + drug , random=~drug - 1|subj, control=ctrl,
                        weights=varIdent(form = ~ 1 | drug),
                           data=inputdata, method="REML")},
    B  = {target.lme<-lme(AUC0t ~ seq +  prd + drug , random=~drug - 1|subj, control=ctrl,
                        weights=varIdent(form = ~ 1 | drug),
                           data=inputdata, method="REML")},
    C  = {target.lme<-lme(AUC0INF ~ seq +  prd + drug , random=~drug - 1|subj, control=ctrl,
                        weights=varIdent(form = ~ 1 | drug),
                           data=inputdata, method="REML")},
    D  = {target.lme<-lme(partAUC ~ seq +  prd + drug , random=~drug - 1|subj, control=ctrl,
                        weights=varIdent(form = ~ 1 | drug),
                           data=inputdata, method="REML")},
    A1 = {target.lme<-lme(log(Cmax) ~ seq +  prd + drug , random=~drug - 1|subj, control=ctrl,
                        weights=varIdent(form = ~ 1 | drug),
                           data=inputdata, method="REML")},
    B1 = {target.lme<-lme(log(AUC0t) ~ seq +  prd + drug , random=~drug - 1|subj, control=ctrl,
                        weights=varIdent(form = ~ 1 | drug),
                           data=inputdata, method="REML")},
    C1 = {target.lme<-lme(log(AUC0INF) ~ seq +  prd + drug , random=~drug - 1|subj, control=ctrl,
                        weights=varIdent(form = ~ 1 | drug),
                           data=inputdata, method="REML")},
    D1 = {target.lme<-lme(log(partAUC) ~ seq +  prd + drug , random=~drug - 1|subj, control=ctrl,
                        weights=varIdent(form = ~ 1 | drug),
                           data=inputdata, method="REML")})
} 
if(study.type==2){ ### parallel, single- or multiple-does
  print(summary(target.lm));cat("\n")
}
else{  ### replicated, single-dose; replicated study does not have multiple-dose in bear. -YJ
  print(summary(target.lme));cat("\n")
### if(study.type==1){
  cat("Type I Tests of Fixed Effects\n")
  print(anova(target.lme)[2:4,]);cat("\n")
  cat("Type III Tests of Fixed Effects\n")
  print(anova(target.lme, type="marginal")[2:4,]);cat("\n\n")
}
###
### testing ...
### added plots for lme()
#  pdf(file="lme_resid_plots.pdf", paper = "a4", bg = "white")
#  par(mai=c(1.3,2,1.3,1.8)) 
#  res_lme=residuals(modlnCmax)
#  plot(res_lme, main="residual plot of lnCmax")
#  qqnorm(res_lme, main="QQ plot of lnCmax");qqline(res_lme)
#  plot(modlnCmax, resid(., type = "p") ~ fitted(.) | drug, abline = 0, main="plot of lme model")
#  plot(ranef(modlnCmax), main="plot of random effects")
#  dev.off()   ### end of output to pdf
#  readline("\n\n ..pause here to see lme plots.\n")
###
### modlnCmax2<-lme(log(Cmax) ~ seq +  prd + drug , random=~drug - 1|subj, 
###                weights=varIdent(form = ~ 1 | drug), 
###                data=TotalData, method="ML" )                  ### testing this new model  -YJ
               
###
### calc sigma_sq value?  -YJ
###                                      
}