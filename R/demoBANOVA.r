#Demo for Statistical analysis (ANOVA(lm), 90%CI...)

demoBANOVA<-function(replicated=FALSE,parallel=FALSE, multiple=FALSE)
{

TotalData<-NULL
Fname<-Fname
pAUC<-pAUC
if(pAUC){
cat("\n Since the demo dataset does not include any pAUC parameter,\n pAUC temporarily switches to FALSE now.\n")
readline(" Press Enter to continue...\n\n")
pAUC<<-FALSE     ### BANOVA demo dataset never have pAUC parameters included for 'statistical only.' -YJ
}

options(warn=-1)
if(parallel){
   if(multiple){
     TotalData<-data.frame (subj=as.factor(c(1,17,2,18,3,20,7,23,8,24,9,25,10,26,
                                             11,27,28,13,29,14,15,31,16,32)),
                            drug=as.factor(c(1,2,1,2,1,2,1,2,1,2,1,2,1,2,
                                             1,2,2,1,2,2,1,2,1,2)),
                             Cmax_ss=c(3350,2820,1730,2340,1320,2740,1390,2600,2240,1680,1930,1980,3140,1810,
                                       1440,3400,1610,3050,1760,2020,3170,2290,1400,1260), 
                             AUCtau_ss=c(30274,28887,20479,20184,15254,29773,17221,39278,28572,19162,27240,22226,30424,22697,
                                          16370,36194,15703,35397,31870,47644,47379,28487,19454,14661),
                             lnCmax_ss=c(8.12,7.94,7.46,7.76,7.19,7.92,7.24,7.86,7.71,7.74,7.57,7.59,8.05,7.5,
                                         7.27,8.13,7.38,8.02,7.47,8.44,8.06,7.74,7.24,7.14),
                             lnAUCtau_ss=c(10.32,10.27,9.93,9.91,9.63,10.3,9.75,10.58,10.26,9.86,10.21,10.01,10.32,10.03,
                                            9.7,10.5,9.66,10.47,10.37,10.13,10.77,10.26,9.88,9.59))               
      show(TotalData);cat("\n\n")
      Fname<<-"MultiplePara_stat_demo.csv"
      ###
      ### disable since v2.6.2; similar for the followings.  --YJ
      ###
      ### if(file.exists("MultiplePara_stat_demo.csv")){
      ###    write.csv(TotalData,file="MultiplePara_stat_demo_02.csv",row.names=FALSE)}
      ### else{write.csv(TotalData,file="MultiplePara_stat_demo.csv",row.names=FALSE)}
      ### if(file.exists("MultiplePara_stat_demo.RData")){
      ###    saveRDS(TotalData,file="MultiplePara_stat_demo_02.RData")}
      ### else{saveRDS(TotalData,file="MultiplePara_stat_demo.RData")}
      ### cat("\n")
      ### cat("\n")
      MultipleParaMIXanalyze(TotalData) 
      ### MultipleParaMIXmenu()    ### will cause loop-run; changed since v2.6.2
      graphics.off();cat("\n\n")
      go2menu()
    } 
 else{
  TotalData<-data.frame (subj=c(1,2,3,4,5,6,7,8,9,10,
                              11,12,13,14,15,16,17,18,19,20), 
                       drug=c(1,1,1,1,1,1,1,1,1,1,
                              2,2,2,2,2,2,2,2,2,2), 
                       Cmax=c(88.6,52.5,92,56,84,84.8,83,96.4,68.1,33.5,
                              70.3,73.5,50.2,62.2,74.1,60.4,60.4,75.3,76.8,82.9), 
                       AUC0t=c(1510,883,1650,1015,1556,1412,1353,1443,1299,560,
                               1284,1391,873,1211,1233,1172,1172,1336,1348,1419), 
                       AUC0INF=c(1530,890,1670,1050,1570,1432,1356,1450,1305,570,
                                 1290,1340,890,1230,1255,1182,1185,1355,1355,1425),         
                       lnCmax=c(4.4841,3.9608,4.5218,4.0254,4.4308,4.4403,4.4188,4.5685,4.2210,3.5115,
                                4.2528,4.2973,3.9160,4.1304,4.3054,4.1010,4.1010,4.3215,4.3412,4.4176),
                       lnAUC0t=c(7.3199,6.7833,7.4085,6.9226,7.3499,7.2528,7.2101,7.2745,7.1694,6.3279,
                                 7.1577,7.2378,6.7719,7.0992,7.1172,7.0665,7.0665,7.1974,7.2064,7.2577), 
                       lnAUC0INF=c(7.333,6.7912,7.2226,6.9565,7.3588,7.2668,7.2123,7.2793,7.1740,6.3456,
                                   7.1624,7.2004,6.792,7.1148,7.1349,7.0750,7.0775,7.2116,7.2116,7.2619))          
     show(TotalData);cat("\n\n") 
     Fname<<-"SinglePara_stat_demo.csv"
     ### if(file.exists("SinglePara_stat_demo.csv")){
     ###    write.csv(TotalData,file="SinglePara_stat_demo_02.csv",row.names=FALSE)}
     ### else{write.csv(TotalData,file="SinglePara_stat_demo.csv",row.names=FALSE)}
     ### if(file.exists("SinglePara_stat_demo.RData")){
     ###   saveRDS(TotalData,file="SinglePara_stat_demo_02.RData")}
     ### else{saveRDS(TotalData,file="SinglePara_stat_demo.RData")}
     ### cat("\n")
     ### cat("\n")
     ParaMIXanalyze(TotalData) 
     ### ParaMIXmenu()    ### cause loop-run; changed this since v2.6.2
     graphics.off();cat("\n\n")
     go2menu()
   }
 }
else{ 
  if(replicated){
  TotalData<-data.frame (subj=as.factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,
                                        1,2,3,4,5,6,7,8,9,10,11,12,13,14,
                                        1,2,3,4,5,6,7,8,9,10,11,12,13,14,
                                        1,2,3,4,5,6,7,8,9,10,11,12,13,14)),
                       drug=as.factor(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                        2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                                        1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                        2,2,2,2,2,2,2,2,2,2,2,2,2,2)),
                       seq=as.factor (c(2,1,2,1,2,1,2,1,2,1,2,1,2,1,
                                        2,1,2,1,2,1,2,1,2,1,2,1,2,1,
                                        2,1,2,1,2,1,2,1,2,1,2,1,2,1,
                                        2,1,2,1,2,1,2,1,2,1,2,1,2,1)),
                       prd=as.factor (c(2,1,2,1,2,1,2,1,2,1,2,1,2,1,
                                        1,2,1,2,1,2,1,2,1,2,1,2,1,2,
                                        3,4,3,4,3,4,3,4,3,4,3,4,3,4,
                                        4,3,4,3,4,3,4,3,4,3,4,3,4,3)),
                       Cmax=c(1739,1481,1780,1374,1555,1756,1566,1939,1475,1388,1127,1542,1235,1598,
                              1633,1837,2073,1629,1385,1522,1643,1615,1759,1483,1682,1247,1605,1718,
                              1700,1461,1790,1364,1545,1746,1576,1949,1485,1398,1117,1532,1245,1588,
                              1641,1847,2083,1619,1386,1512,1653,1625,1769,1493,1692,1257,1615,1728),
                       AUC0t=c(14445,12516,15371,11063,13971,15376,13442,13442,12410,13310,9353,15015,9723,14977,
                               12294,15299,15184,13982,11852,13838,12361,14347,15804,11711,15371,10609,15428,17803,
                               13995,12296,15551,10864,13810,15341,13609,13410,12501,13512,9307,15090,9786,14861,
                               12049,15532,15289,13963,11671,13869,12559,14120,15873,11745,15260,10696,15583,17642),
                       AUC0INF=c(14933,13185,16032,11668,14557,15964,14068,14001,12915,13985,9750,15757,10375,15916,
                                 12972,16209,15691,14650,12550,14343,12979,14681,16565,12544,16029,11093,16308,18870,
                                 14469,12938,16218,11398,14401,15908,14200,13944,12950,14238,9677,15843,10343,15780,
                                 12706,16568,15783,14610,12352,14347,13086,14438,16749,12513,15933,11121,16587,18729),
                       lnCmax=c(7.46,7.30,7.48,7.23,7.35,7.47,7.36,7.57,7.30,7.24,7.03,7.34,7.12,7.38,
                                7.40,7.52,7.64,7.40,7.23,7.33,7.40,7.39,7.47,7.30,7.43,7.13,7.38,7.45,
                                7.43,7.28,7.48,7.21,7.34,7.46,7.36,7.57,7.30,7.24,7.01,7.33,7.12,7.37,
                                7.40,7.52,7.64,7.38,7.23,7.32,7.41,7.39,7.47,7.30,7.43,7.13,7.38,7.45),
                       lnAUC0t=c(9.58,9.43,9.64,9.31,9.54,9.64,9.51,9.50,9.43,9.50,9.14,9.62,9.18,9.61,
                                 9.42,9.64,9.63,9.55,9.38,9.54,9.42,9.57,9.67,9.37,9.64,9.27,9.64,9.79,
                                 9.54,9.41,9.65,9.29,9.53,9.63,9.51,9.50,9.43,9.51,9.13,9.62,9.18,9.60,
                                 9.39,9.65,9.63,9.54,9.36,9.53,9.43,9.55,9.67,9.37,9.63,9.27,9.65,9.77),
                     lnAUC0INF=c(9.61,9.49,9.68,9.36,9.59,9.68,9.55,9.55,9.47,9.55,9.19,9.67,9.25,9.68,
                                 9.47,9.69,9.66,9.59,9.44,9.57,9.47,9.59,9.72,9.44,9.68,9.31,9.70,9.85,
                                 9.57,9.46,9.69,9.34,9.57,9.67,9.56,9.54,9.46,9.56,9.17,9.67,9.24,9.66,
                                 9.44,9.71,9.66,9.58,9.42,9.57,9.47,9.57,9.72,9.43,9.67,9.31,9.71,9.83))
     show(TotalData);cat("\n\n") 
     Fname<<-"SingleRep_stat_demo.csv"
     ### if(file.exists("SingleRep_stat_demo.csv")){
     ###     write.csv(TotalData,file="SingleRep_stat_demo_02.csv",row.names=FALSE)}
     ### else{write.csv(TotalData,file="SingleRep_stat_demo.csv",row.names=FALSE)}
     ### if(file.exists("SingleRep_stat_demo.RData")){
     ###     saveRDS(TotalData,file="SingleRep_stat_demo_02.RData")}
     ### else{saveRDS(TotalData,file="SingleRep_stat_demo.RData")}
     ### 
     ### cat("\n")
     ### cat("\n")
     RepMIXanalyze(TotalData) 
     ### RepMIXmenu()    ### cause loop-run; changed this since v2.6.2
     graphics.off();cat("\n\n")
     go2menu()
  }
 else{
    if(multiple){
     TotalData<-data.frame (subj=as.factor(c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,
                                             11,11,12,12,13,13,14,14,15,15,16,16)),
                            drug=as.factor(c(1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,
                                             1,2,1,2,1,2,1,2,1,2,1,2)),
                             seq=as.factor(c(1,1,1,1,2,2,1,1,2,2,2,2,2,2,1,1,2,2,1,1,
                                             2,2,1,1,1,1,2,2,1,1,2,2)),
                             prd=as.factor(c(1,2,1,2,2,1,1,2,2,1,2,1,2,1,1,2,2,1,1,2,
                                             2,1,1,2,1,2,2,1,1,2,2,1)),
                             Cmax_ss=c(3350,2820,1730,2340,1320,954,3960,2740,768,960,815,934,1390,2600,2240,1680,1930,1980,
                                       3140,1810,1440,3400,3760,1610,3050,1760,2020,4610,3170,2290,1400,1260), 
                             AUCtau_ss=c(30274,28887,20479,20184,15254,10131,42414,29773,5267,9845,5240,9782,17221,39278,28572,19162,27240,22226,
                                          30424,22697,16370,36194,25952,15703,35397,31870,25006,47644,47379,28487,19454,14661),
                             lnCmax_ss=c(8.12,7.94,7.46,7.76,7.19,6.86,8.28,7.92,6.64,6.87,6.7,6.84,7.24,7.86,7.71,7.74,7.57,7.59,
                                         8.05,7.5,7.27,8.13,8.23,7.38,8.02,7.47,7.61,8.44,8.06,7.74,7.24,7.14),
                             lnAUCtau_ss=c(10.32,10.27,9.93,9.91,9.63,9.22,10.66,10.3,8.57,9.19,8.56,9.19,9.75,10.58,10.26,9.86,10.21,10.01,
                                            10.32,10.03,9.7,10.5,10.16,9.66,10.47,10.37,10.13,10.77,10.77,10.26,9.88,9.59))               
      show(TotalData);cat("\n\n") 
      Fname<<-"MultipleRep_stat_demo.csv"
      ### if(file.exists("MultipleRep_stat_demo.csv")){
      ###    write.csv(TotalData,file="MultipleRep_stat_demo_02.csv",row.names=FALSE)}
      ### else{write.csv(TotalData,file="MultipleRep_stat_demo.csv",row.names=FALSE)}
      ### if(file.exists("MultipleRep_stat_demo.RData")){
      ###    saveRDS(TotalData,file="MultipleRep_stat_demo_02.RData")}
      ### else{saveRDS(TotalData,file="MultipleRep_stat_demo.RData")}
      ### cat("\n")
      ### cat("\n")
      MultipleBANOVAanalyze(TotalData) 
      ### MultipleBANOVAmenu()    ### cause loop-run; changed this since v2.6.2
      graphics.off();cat("\n\n")
      go2menu()
    } 
 else{
   TotalData<-data.frame (subj=as.factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,
                                      1,2,3,4,5,6,7,8,9,10,11,12,13,14)),
                       drug=as.factor(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                      2,2,2,2,2,2,2,2,2,2,2,2,2,2)),
                       seq=as.factor (c(2,1,2,1,2,1,2,1,2,1,2,1,2,1,
                                      2,1,2,1,2,1,2,1,2,1,2,1,2,1)),
                       prd=as.factor (c(2,1,2,1,2,1,2,1,2,1,2,1,2,1,
                                      1,2,1,2,1,2,1,2,1,2,1,2,1,2)),
                       Cmax=c(1739,1481,1780,1374,1555,1756,1566,1939,1475,1388,1127,1542,1235,1598,
                              1633,1837,2073,1629,1385,1522,1643,1615,1759,1483,1682,1247,1605,1718),
                       AUC0t=c(14445,12516,15371,11063,13971,15376,13442,13422,12410,13310,9353,15015,9723,14977,
                               12294,15299,15184,13982,11852,13838,12361,14347,15804,11711,15371,10609,15428,17803),
                       AUC0INF=c(14933,13185,16032,11668,14557,15964,14068,14001,12915,13985,9750,15757,10375,15916,
                                 12972,16209,15691,14650,12550,14343,12979,14681,16565,12544,16029,11093,16308,18870),
                       lnCmax=c(7.46,7.30,7.48,7.23,7.35,7.47,7.36,7.57,7.30,7.24,7.03,7.34,7.12,7.38,
                                7.40,7.52,7.64,7.40,7.23,7.33,7.40,7.39,7.47,7.30,7.43,7.13,7.38,7.45),
                       lnAUC0t=c(9.58,9.43,9.64,9.31,9.54,9.64,9.51,9.50,9.43,9.50,9.14,9.62,9.18,9.61,
                                 9.42,9.64,9.63,9.55,9.38,9.54,9.42,9.57,9.67,9.37,9.64,9.27,9.64,9.79),
                       lnAUC0INF=c(9.61,9.49,9.68,9.36,9.59,9.68,9.55,9.55,9.47,9.55,9.19,9.67,9.25,9.68,
                                  9.47,9.69,9.66,9.59,9.44,9.57,9.47,9.59,9.72,9.44,9.68,9.31,9.70,9.85))
      show(TotalData);cat("\n\n") 
      Fname<<-"single2x2x2_stat_demo.csv"
      ### if(file.exists("single2x2x2_stat_demo.csv")){
      ###    write.csv(TotalData,file="single2x2x2_stat_demo_02.csv",row.names=FALSE)}
      ### else{write.csv(TotalData,file="single2x2x2_stat_demo.csv",row.names=FALSE)}
      ### if(file.exists("single2x2x2_stat_demo.RData")){
      ###    saveRDS(TotalData,file="single2x2x2_stat_demo_02.RData")}
      ### else{saveRDS(TotalData,file="single2x2x2_stat_demo.RData")}
      ### cat("\n")
      ### cat("\n")
      BANOVAanalyze(TotalData) 
      ### BANOVAmenu()   ### cause loop-run; changed this since v2.6.2
      graphics.off();cat("\n\n")
      go2menu()
     }
   }
  }
}
  