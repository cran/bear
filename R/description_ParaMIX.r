#description for lme (replicated study)
description_ParaMIX<-function(){
cat("****************************************************************************\n")
cat("          Statistical analysis (lme, 90%CI...) - parallel BE study          \n")
cat("--------------------------------------------------------------------------  \n")
cat(" With a two-treatment, two-sequence, one-period, parallel design, bear      \n")
cat(" deploys the linear mixed effect model (lme).  The statistical model        \n")
cat(" includes a factor regarding only one source of variation - treatment.      \n")
cat(" There are no sources of variation associated with sequence or period       \n")
cat(" because there are no sequences or periods in a parallel design.  Moreover, \n")
cat(" lme in nlme package is used to obtain estimates for the adjusted           \n")
cat(" differences between treatment means and the standard error.                \n")
cat(" Log-transformed BA measures will also be analyzed.                         \n")
cat("****************************************************************************\n")
cat("\n")
}