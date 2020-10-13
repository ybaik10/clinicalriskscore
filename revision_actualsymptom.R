# Library set up
library(tidyverse); library(mice); library(glmnet); library(pROC)
# Data set up
newfd_xpert_completecase %>% count(n_tbsymp_cat1)
actualsymptom <- newfd_xpert_completecase %>% dplyr::select(n_tbsymp_cat1, symp_fac___1cat, symp_fac___2cat, symp_fac___3cat, symp_fac___4cat, symp_2wks, agecat3, sexcat, hivcat1, dbcat, n_other_sympcat, edu8, pasttb, eversmoke, lungcat, xpert)
# Performing MI on the original samples
actualsymptom[] <- lapply(actualsymptom, function(x){return(as.factor(x))})
# Most missing found in symp_2wks (5%)
actualsymptom %>% count(symp_2wks)
actualsymptom.im <- mice(actualsymptom,5,pri=F)
# Extract MI datasets to each dataset
actualsymptom.im.dt <- vector("list",5)    
for (i in 1:5){
  actualsymptom.im.dt[[i]] <- mice::complete(actualsymptom.im,i) 
} 
# Lasso regression
actcoef=list()
actcoeftotal=matrix(nrow=17)
for(i in 1:5){
  d <- actualsymptom.im.dt[[i]][,c(-12,-15)]
  d$xpert <- as.numeric(as.character(d$xpert)) # Make the class of the oucome factor to numeric
  x_vars <- model.matrix(xpert~. , d)[,-1]
  y_var <- d$xpert
  cv <- cv.glmnet(x_vars, y_var, nfold=10, alpha = 1, family="binomial") # Estimate the shrinkage factor ('nfold=10' means splitting into 10 subsamples, building Lasso based on 9, and testing/estimating from 1)
  actlambda <- cv$lambda.min # Get the best shrinkage factor
  actlasso_best <- glmnet(x_vars, y_var, alpha = 1, lambda = actlambda, family="binomial") # Lasso regression (coefficient must be the mean across the 20 bootstrapping samples)
  actcoef[[i]] <- coef(actlasso_best)
  actcoeftotal <- cbind(actcoeftotal,actcoef[[i]])
}
actcoeftotal <- actcoeftotal[,-1]
round(rowMeans(actcoeftotal)/median(c( 0.6317891, 0.7123291 , 0.5558685, 0.8667435, 1.0857714 , 0.7513067)))
# Scoring system
# (Intercept)   n_tbsymp_cat11 symp_fac___1cat1 symp_fac___2cat1 symp_fac___3cat1 symp_fac___4cat1       symp_2wks1         agecat31         agecat32         agecat33         agecat34 
# -3                0               -1                0                2                1                1                1                1                1                0 
# sexcat1         hivcat11           dbcat1 n_other_sympcat1          pasttb1       eversmoke1 
# 1                1                1                0                0                0 
# Edit data and back to 'mids'
actualsymptom.im.new <- complete(actualsymptom.im, action="long", include=T)
actualsymptom.im.new$score810 <- -1*ifelse(actualsymptom.im.new$symp_fac___1cat==1,1,0) + 2*ifelse(actualsymptom.im.new$symp_fac___3cat==1,1,0) + ifelse(actualsymptom.im.new$symp_fac___4cat==1,1,0) + ifelse(actualsymptom.im.new$symp_2wks==1,1,0) + ifelse(actualsymptom.im.new$sexcat==1,1,0) + ifelse(actualsymptom.im.new$hivcat1==1,1,0) + ifelse(actualsymptom.im.new$dbcat==1,1,0) +
  ifelse(actualsymptom.im.new$agecat3=="4"|actualsymptom.im.new$agecat3=="0",0,1)
actualsymptom.im.new$score810[actualsymptom.im.new$score810==8] <-7 
actualsymptom.im.back <- as.mids(actualsymptom.im.new, .imp=1)
# ROC with a score range -1 to 7
rocn7 <- with(actualsymptom.im.back, roc(xpert~ score810, ci=T))$analyses #glm(xpert~score, family=binomial())$fitted.values, plot=T, ci=T, col=2))
rocn7.stomp <- roc(xpert~ scoren7, data=external, ci=T)

png(filename = "ROC_actualsymptom_score810.png")
par(pty='s')
plot.roc(rocn7[[1]], col=1, type="o",print.thres=c(-1,0,1,2,3,4,5,6,7), print.thres.pattern = (c('-1','0','1','2','3','4','5','6','7+')), cex.axis=1.2, cex.lab=1.5, main="E\n", print.thres.adj = c(0.85,-0.5))
for(i in 2:7){plot(rocn7[[i]], col=1, add=T)}
plot(rocn7.stomp, col=2, add=T, type="o",print.thres=c(0,1,2,3,4,5,6,7), print.thres.pattern = (c('0','1','2','3','4','5','6','7+')), print.thres.col="red")
par(pty='m')
legend("bottomright", legend=c("Derivation data, c-statistic=0.84(0.81-0.86)", "Validation data, c-statistic=0.77(0.72-0.82)"),
       col=c(1,2), lty=1:1, cex=1.0, box.lty=0, bty = "n")
dev.off()
