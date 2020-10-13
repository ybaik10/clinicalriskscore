newfd_xpert_completecase %>% filter(symp_fac___1 == 1) %>% group_by(xpert) %>% count(symp_2wks)
newfd_xpert_completecase$chron.cough <- ifelse((newfd_xpert_completecase$symp_fac___1==1&newfd_xpert_completecase$symp_2wks==1),1,0)
newfd_xpert_completecase %>% count(chron.cough)
newfd_xpert_completecase %>% group_by(xpert)%>% count(chron.cough)
newfd_xpert_completecase_chroncough <- newfd_xpert_completecase %>% filter(chron.cough==1)
newfd_xpert_completecase_chroncough %>% count(score)

# reappendix <- newfd_xpert_completecase[!is.na(newfd_xpert_completecase$n_tbsymp),] %>% dplyr::select(chron.cough, agecat3, sexcat, hivcat1, dbcat, n_other_sympcat, edu8, pasttb, eversmoke, lungcat, xpert) #symp_fac___1cat, symp_fac___2cat, symp_fac___3cat, symp_fac___4cat, 
# # Performing MI on the original samples
# reappendix[] <- lapply(reappendix, function(x){return(as.factor(x))})
# Imput.reapp <- mice(reappendix,7,pri=F)
# # Extract MI datasets to each dataset
# data.imputed.Boot.reapp <- vector("list",7)    
# for (i in 1:7){
#   data.imputed.Boot.reapp[[i]]=complete(Imput.reapp,i) 
# } 
# 
#
# ### Get the coefficients and intercept from Lasso regression
# coef=list()
# coeftotal=matrix(nrow=12)
# for(i in 1:7){
#   d <- data.imputed.Boot.reapp[[i]][,c(-7,-10)]
#   d$xpert <- as.numeric(as.character(d$xpert)) # Make the class of the oucome factor to numeric
#   x_vars <- model.matrix(xpert~. , d)[,-1]
#   y_var <- d$xpert
#   cv <- cv.glmnet(x_vars, y_var, nfold=10, alpha = 1, family="binomial") # Estimate the shrinkage factor ('nfold=10' means splitting into 10 subsamples, building Lasso based on 9, and testing/estimating from 1)
#   lambda <- cv$lambda.min # Get the best shrinkage factor
#   lasso_best <- glmnet(x_vars, y_var, alpha = 1, lambda = lambda, family="binomial") # Lasso regression (coefficient must be the mean across the 20 bootstrapping samples)
#   coef[[i]] <- coef(lasso_best)
#   coeftotal <- cbind(coeftotal,coef[[i]])
# }
# coeftotal <- coeftotal[,-1]
# rowMeans(coeftotal)
# round(rowMeans(coeftotal)/median(c(0.75033254, 0.77444310, 0.86155692, 1.3330141, 0.67620528, 0.64462148)))
# 
# 
# data.imputed.reapp <- complete(Imput.reapp, action="long", include=T)
# data.imputed.reapp$score <- ifelse(data.imputed.reapp$agecat3=="2"|data.imputed.reapp$agecat3=="1",1,0) + ifelse(data.imputed.reapp$sexcat==1,1,0) + ifelse(data.imputed.reapp$hivcat1==1,2,0) + ifelse(data.imputed.reapp$chron.cough==1,1,0) + ifelse(data.imputed.reapp$dbcat==1,1,0) +
#   0*ifelse(data.imputed.reapp$n_other_sympcat==1,0,0) + 0*ifelse(data.imputed.reapp$edu8==1,0,0) + 0*ifelse(data.imputed.reapp$pasttb==1,0,0) + 0*ifelse(data.imputed.reapp$eversmoke==1,0,0) + 0*ifelse(data.imputed.reapp$lungcat==1,0,0)
# max(data.imputed.reapp$score,na.rm=T)
# Imput.reapp.back <- as.mids(data.imputed.reapp, .imp=1)
# 
# reapp.kharitode <- with(Imput.reapp.back, roc(xpert~ score, ci=T))$analyses
# 
external$chron.cough <- ifelse((external$symptoms_past_week___1==1&external$symp_2wks==1),1,0)
external %>% count(chron.cough)
external %>% group_by(xpert) %>% count(chron.cough)
external_chroncough <- external %>% filter(chron.cough==1)
external_chroncough$score <- ifelse(external_chroncough$agecat1=="[25,35)"|external_chroncough$agecat1=="[35,45)",1,0) + external_chroncough$sexcat + 2*external_chroncough$hivcat1 + external_chroncough$dbcat + as.numeric(as.character(external_chroncough$n_tbsymp)) + as.numeric(as.character(external_chroncough$symp_2wks)) 
external_chroncough %>% count(score)

# external %>% count(agecat1)
# external$score <- ifelse(external$agecat1=="[25,35)"|external$agecat1=="[35,45)",1,0) + external$sexcat + 2*external$hivcat1 + external$dbcat + external$chron.cough
# max(external$score,na.rm=T)
# 
# reapp.STOMP <- roc(xpert~ score, data=external, ci=T)
# 
# png(filename = "revision-ROC_chroncough_scoring_063020.png")
# par(pty='s')
# plot(reapp.kharitode[[1]], col=1, type="b")
# for(i in 2:7){plot(reapp.kharitode[[i]], col=1, add=T)}
# plot(reapp.STOMP, col=2, add=T, type="b")
# legend("bottomright", legend=c("Derivation data, c-statistic=0.73(0.70-0.75)", "Validation data, c-statistic=0.65(0.59-0.71)"),
#        col=c(1,2), lty=1:1, cex=0.8, box.lty=0, bty = "n")
# dev.off()

png(filename = "revision-ROC_Chroncoughpop_070720.png")
par(pty='s')
plot(roc(xpert~score,data=newfd_xpert_completecase_chroncough,ci=T),col=1,type="b")
plot(roc(xpert~score,data=external_chroncough,ci=T),col=2,add=T,type="b")
legend("bottomright", legend=c("Derivation data, c-statistic=0.76(0.72-0.80)", "Validation data, c-statistic=0.69(0.62-0.75)"),
       col=c(1,2), lty=1:1, cex=0.8, box.lty=0, bty = "n")
dev.off()




png(filename = "DCA_re-revision_externalvalidation_10prev_biggercex_070720-size.png", width=600, height=500)
par(mar=c(10,5,1,2))
plot(100*dca.validation$derived.data[c(1:101),]$thresholds, dca.validation$derived.data[c(1:101),]$sNB, type='l', ylim=c(-0.2,1), col=2, bty='n', xaxt='n', ylab='Standardized Net Benefit', xlab='', cex.lab=1.4, cex.axis=1.2) #, main="Decision curve analysis in the validation population \n(10% TB prevalence scenario)", bty='n', xaxt='n', ylab='Standardized Net Benefit', xlab='', cex.main=0.99
abline(h=0, col=1, lty=2)
xtick <- seq(0.0*100,1.0*100, by=0.20*100)
axis(side=1,at=xtick, cex.axis=1.2) #mtext(c("Risk threshold","Risk:Benefit Ratio"),pos=c(0.4,-0.7))
axis(side=1,at=c(1,2,4,9,17,37,43,48), label=NA,cex.axis=0.8)
text(1,-0.2,"1",cex=0.75);text(2,-0.2,"2",cex=0.75);text(4,-0.2,"4",cex=0.75);text(9,-0.2,"9",cex=0.75);text(17,-0.2,"17",cex=0.75);text(37,-0.2,"37",cex=0.75);text(43,-0.2,"43",cex=0.75);text(48,-0.2,"48",cex=0.75);
axis(side=1,at=xtick, labels=c("1:100","1:4","2:3","3:2","4:1","100:1"), cex.axis=1.2, pos=-0.45) 
arrows(0.06*100,-0.2,0.06*100,0.5,length =0,lty=3,col=2)
arrows(0.23*100,-0.2,0.23*100,0.0,length =0,lty=3,col=2)
text(6,-0.2,"6",cex=0.75,col=2,font=3)
text(23,-0.2,"23",cex=0.75,col=2,font=3)
mtext("%, threshold probability (for treating TB)", side=1, line=2, cex=1.2)
mtext("Risk:Benefit ratio", side=1, line=6.0, cex=1.2)
lines(100*dca.validation$derived.data[c(1:101),]$thresholds, dca.validation$derived.data[c(1:101),]$sNB_lower, type='l', ylim=c(-0.1,1), col=2, lty=2)
lines(100*dca.validation$derived.data[c(1:101),]$thresholds, dca.validation$derived.data[c(1:101),]$sNB_upper, type='l', ylim=c(-0.1,1), col=2, lty=2)
lines(100*dca.validation$derived.data[c(102:114),]$thresholds, dca.validation$derived.data[c(102:114),]$sNB, type='l', ylim=c(-0.05,1), col=4)
lines(100*dca.validation$derived.data[c(102:114),]$thresholds, dca.validation$derived.data[c(102:114),]$sNB_lower, type='l', ylim=c(-0.05,1), col=4, lty=2)
lines(100*dca.validation$derived.data[c(102:114),]$thresholds, dca.validation$derived.data[c(102:114),]$sNB_upper, type='l', ylim=c(-0.05,1), col=4, lty=2)
lines(100*dca.validation$derived.data[c(203:303),]$thresholds, dca.validation$derived.data[c(203:303),]$sNB, type='l', ylim=c(-0.1,1), col=1, lty=1, lwd=3)
legend("right", legend=c("Treatment based on clinical risk score","Treatment for all","No empirical treatment"),
       col=c(2,4,1), lty=c(1,1,1), lwd=c(1,1,3),cex=1.1, bty='n')
dev.off()


dca.validation$derived.data[c(1:101),]$thresholds
dca.validation$derived.data[c(1:101),]$sNB_lower - dca.validation$derived.data[c(102:202),]$sNB_upper # 0.06
dca.validation$derived.data[c(1:101),]$sNB_lower - dca.validation$derived.data[c(203:303),]$sNB # 0.23




png(filename = "DCA_re-revision_externalvalidation_10prev_biggercex_071020-score-size.png", width=900, height=620)
par(mar=c(10,5,1,2))
plot(100*dca.validation$derived.data[c(1:101),]$thresholds, dca.validation$derived.data[c(1:101),]$sNB, type='l', xlim=c(-10,100), ylim=c(-0.2,1), col=2, bty='n', xaxt='n', ylab='Standardized Net Benefit', xlab='', cex.lab=1.4, cex.axis=1.2) #, main="Decision curve analysis in the validation population \n(10% TB prevalence scenario)", bty='n', xaxt='n', ylab='Standardized Net Benefit', xlab='', cex.main=0.99
abline(h=0, col=1, lty=2)
xtick <- seq(0.0*100,1.0*100, by=0.20*100)
axis(side=1,at=xtick, cex.axis=1.2) #mtext(c("Risk threshold","Risk:Benefit Ratio"),pos=c(0.4,-0.7))
axis(side=1,at=c(1,2,4,9,17,37,43,48), label=NA,cex.axis=0.8)
text(-5,-0.15,"Score",font=2); 
text(1,-0.15,"1");text(2,-0.15,"2");text(4,-0.15,"3");text(9,-0.15,"4" );text(17,-0.15,"5" );text(37,-0.15,"6" );text(43,-0.15,"7" );text(48,-0.15,"8+" )
text(-6.5,-0.2,"Post-test\nprobability" ,font=2)
text(1,-0.2,"1" );text(2,-0.2,"2" );text(4,-0.2,"4" );text(9,-0.2,"9" );text(17,-0.2,"17" );text(37,-0.2,"37" );text(43,-0.2,"43" );text(48,-0.2,"48" );
axis(side=1,at=xtick, labels=c("1:100","1:4","2:3","3:2","4:1","100:1"), cex.axis=1.2, pos=-0.45) 
arrows(0.06*100,-0.2,0.06*100,0.5,length =0,lty=3,col=2)
arrows(0.23*100,-0.2,0.23*100,0.0,length =0,lty=3,col=2)
text(6,-0.2,"6",cex=0.75,col=2,font=3)
text(23,-0.2,"23",cex=0.75,col=2,font=3)
mtext("%, threshold probability (for treating TB)", side=1, line=2, cex=1.2)
mtext("Risk:Benefit ratio", side=1, line=7, cex=1.2)
lines(100*dca.validation$derived.data[c(1:101),]$thresholds, dca.validation$derived.data[c(1:101),]$sNB_lower, type='l', ylim=c(-0.1,1), col=2, lty=2)
lines(100*dca.validation$derived.data[c(1:101),]$thresholds, dca.validation$derived.data[c(1:101),]$sNB_upper, type='l', ylim=c(-0.1,1), col=2, lty=2)
lines(100*dca.validation$derived.data[c(102:114),]$thresholds, dca.validation$derived.data[c(102:114),]$sNB, type='l', ylim=c(-0.05,1), col=4)
lines(100*dca.validation$derived.data[c(102:114),]$thresholds, dca.validation$derived.data[c(102:114),]$sNB_lower, type='l', ylim=c(-0.05,1), col=4, lty=2)
lines(100*dca.validation$derived.data[c(102:114),]$thresholds, dca.validation$derived.data[c(102:114),]$sNB_upper, type='l', ylim=c(-0.05,1), col=4, lty=2)
lines(100*dca.validation$derived.data[c(203:303),]$thresholds, dca.validation$derived.data[c(203:303),]$sNB, type='l', ylim=c(-0.1,1), col=1, lty=1, lwd=3)
legend("right", legend=c("Treatment based on clinical risk score","Treatment for all","No empirical treatment"),
       col=c(2,4,1), lty=c(1,1,1), lwd=c(1,1,3),cex=1.1, bty='n')
dev.off()



write.csv(dca.validation$derived.data,"dcavalidation.csv")
View(dca.validation)

png(filename = "DCA_re-revision_externalvalidation_10prev_092520.png", width=1000, height=600)
par(mar=c(10,5,1,2))
plot(100*dca.validation$derived.data[c(1:101),]$thresholds, dca.validation$derived.data[c(1:101),]$sNB, type='l', xlim=c(-2,100), ylim=c(-0.1,1), col=2, bty='n', xaxt='n', ylab='Standardized Net Benefit', xlab='', cex.lab=1.2, cex.axis=1.0, lwd=2) #, main="Decision curve analysis in the validation population \n(10% TB prevalence scenario)", bty='n', xaxt='n', ylab='Standardized Net Benefit', xlab='', cex.main=0.99
abline(h=0, col=1, lty=2)
# xtick <- seq(0.0*100,1.0*100, by=0.20*100)
xtick1 <- c(0,1,2,4,6,9,17,23,37,43,48,100)
axis(side=1,at=xtick1, cex.axis=0.9) #mtext(c("Risk threshold","Risk:Benefit Ratio"),pos=c(0.4,-0.7))
# axis(side=1,at=c(1,2,43,50), label=NA,cex.axis=0.8)
text(-2,-0.1,"Score",font=2,cex=0.9); 
text(1,-0.1,"1",cex=0.9);
text(2,-0.1,"2",cex=0.9);text(4,-0.1,"3",cex=0.9);text(9,-0.1,"4",cex=0.9);text(17,-0.1,"5",cex=0.9);text(37,-0.1,"6",cex=0.9);text(43,-0.1,"7",cex=0.9);text(48,-0.1,"8+",cex=0.9)
# text(-6.5,-0.2,"Post-test\nprobability" ,font=2)
# text(1,-0.2,"1",cex=0.8);text(2,-0.2,"2" ,cex=0.8);text(4,-0.2,"4" ,cex=0.8);text(9,-0.2,"9",cex=0.8 );text(17,-0.2,"17",cex=0.8 );text(37,-0.2,"37",cex=0.8 );text(43,-0.2,"43",cex=0.8 );text(48,-0.2,"48" ,cex=0.8);
# axis(side=1,at=xtick, labels=c("1:100","1:4","2:3","3:2","4:1","100:1"), cex.axis=1.0, pos=-0.45) 
arrows(0.06*100,-0.25,0.06*100,0.5,length =0,lty=3,col=2)
arrows(0.23*100,-0.25,0.23*100,0.0,length =0,lty=3,col=2)
# text(6,-0.2,"6",cex=0.80,col=2,font=3)
# text(23,-0.2,"23",cex=0.80,col=2,font=3)
mtext("%, threshold probability (for treating TB)", side=1, line=2, cex=1.2)
# mtext("Risk:Benefit ratio", side=1, line=7, cex=1.0)
lines(100*dca.validation$derived.data[c(1:101),]$thresholds, dca.validation$derived.data[c(1:101),]$sNB_lower, type='l', ylim=c(-0.1,1), col=2, lty=2)
lines(100*dca.validation$derived.data[c(1:101),]$thresholds, dca.validation$derived.data[c(1:101),]$sNB_upper, type='l', ylim=c(-0.1,1), col=2, lty=2)
lines(100*dca.validation$derived.data[c(102:114),]$thresholds, dca.validation$derived.data[c(102:114),]$sNB, type='l', ylim=c(-0.05,1), col=4, lwd=2)
lines(100*dca.validation$derived.data[c(102:114),]$thresholds, dca.validation$derived.data[c(102:114),]$sNB_lower, type='l', ylim=c(-0.05,1), col=4, lty=2)
lines(100*dca.validation$derived.data[c(102:114),]$thresholds, dca.validation$derived.data[c(102:114),]$sNB_upper, type='l', ylim=c(-0.05,1), col=4, lty=2)
lines(100*dca.validation$derived.data[c(203:303),]$thresholds, dca.validation$derived.data[c(203:303),]$sNB, type='l', ylim=c(-0.1,1), col=1, lty=1, lwd=3)
legend("right", legend=c("Treatment based on clinical risk score","Treatment for all","No empirical treatment"),
       col=c(2,4,1), lty=c(1,1,1), lwd=c(1,1,3),cex=1.1, bty='n')
dev.off()







dca.validation.5$derived.data[c(1:101),]$thresholds
dca.validation.5$derived.data[c(1:101),]$sNB_lower - dca.validation.5$derived.data[c(102:202),]$sNB_upper # 0.03
dca.validation.5$derived.data[c(1:101),]$sNB_lower - dca.validation.5$derived.data[c(203:303),]$sNB # 0.11

png(filename = "DCA_re-revision_externalvalidation_5prev_biggercex_070720-size.png", width=600, height=500)
par(mar=c(10,5,1,2))
plot(100*dca.validation.5$derived.data[c(1:101),]$thresholds, dca.validation.5$derived.data[c(1:101),]$sNB, type='l', ylim=c(-0.2,1), col=2, bty='n', xaxt='n', ylab='Standardized Net Benefit', xlab='', cex.lab=1.4, cex.axis=1.2) #, main="Decision curve analysis in the validation population \n(10% TB prevalence scenario)", bty='n', xaxt='n', ylab='Standardized Net Benefit', xlab='', cex.main=0.99
abline(h=0, col=1, lty=2)
xtick <- seq(0.0*100,1.0*100, by=0.20*100)
axis(side=1,at=xtick, cex.axis=1.2) #mtext(c("Risk threshold","Risk:Benefit Ratio"),pos=c(0.4,-0.7))
axis(side=1,at=c(0,1,2,5,9,22,26,30), label=NA,cex.axis=0.8)
text(0,-0.2,"0",cex=0.75);text(1,-0.2,"1",cex=0.75);text(2,-0.2,"2",cex=0.75);text(5,-0.2,"5",cex=0.75);text(9,-0.2,"9",cex=0.75);text(22,-0.2,"22",cex=0.75);text(26,-0.2,"26",cex=0.75);text(30,-0.2,"30",cex=0.75);
axis(side=1,at=xtick, labels=c("1:100","1:4","2:3","3:2","4:1","100:1"), cex.axis=1.2, pos=-0.45) 
arrows(0.03*100,-0.2,0.03*100,0.5,length =0,lty=3,col=2)
arrows(0.11*100,-0.2,0.11*100,0.0,length =0,lty=3,col=2)
text(3,-0.2,"3",cex=0.75,col=2,font=3)
text(11,-0.2,"11",cex=0.75,col=2,font=3)
mtext("%, threshold probability (for treating TB)", side=1, line=2, cex=1.2)
mtext("Risk:Benefit ratio", side=1, line=6.0, cex=1.2)
lines(100*dca.validation.5$derived.data[c(1:101),]$thresholds, dca.validation.5$derived.data[c(1:101),]$sNB_lower, type='l', ylim=c(-0.1,1), col=2, lty=2)
lines(100*dca.validation.5$derived.data[c(1:101),]$thresholds, dca.validation.5$derived.data[c(1:101),]$sNB_upper, type='l', ylim=c(-0.1,1), col=2, lty=2)
lines(100*dca.validation.5$derived.data[c(102:114),]$thresholds, dca.validation.5$derived.data[c(102:114),]$sNB, type='l', ylim=c(-0.05,1), col=4)
lines(100*dca.validation.5$derived.data[c(102:114),]$thresholds, dca.validation.5$derived.data[c(102:114),]$sNB_lower, type='l', ylim=c(-0.05,1), col=4, lty=2)
lines(100*dca.validation.5$derived.data[c(102:114),]$thresholds, dca.validation.5$derived.data[c(102:114),]$sNB_upper, type='l', ylim=c(-0.05,1), col=4, lty=2)
lines(100*dca.validation.5$derived.data[c(203:303),]$thresholds, dca.validation.5$derived.data[c(203:303),]$sNB, type='l', ylim=c(-0.1,1), col=1, lty=1, lwd=3)
legend("right", legend=c("Treatment based on clinical risk score","Treatment for all","No empirical treatment"),
       col=c(2,4,1), lty=c(1,1,1), lwd=c(1,1,3),cex=1.1, bty='n')
dev.off()






external %>% count(clinical_diagnosis.factor)
main %>% count(clinical_diagnosis.factor)

empcase <- main %>% filter(case.control=="CC phase enrolled case"|case.control=="CC phase reclassified case"|
                  ((case.control=="COM phase enrolled case"|case.control=="COM phase reclassified case")&com_screening_method_case==1)) %>% filter(clinical_diagnosis.factor=="clinical") 

empcase %>% count(why_a_case_xpert.factor, why_a_case_culture.factor, why_a_case_smear.factor, why_a_case_other___1.factor, why_a_case_other___2.factor, why_a_case_other___3.factor, why_a_case_other___5.factor, why_a_case_specifyother)
empcase %>% count(newtest_xpert.factor, newtest_smear.factor, newtest_smear_5.factor, culture_liq_growth.factor, culture_solid_growth.factor)

#AGE
empcase$agecat1 <- ifelse(empcase$age_years>=25 & empcase$age_years<35,1,0)
empcase$agecat1[empcase$age_years>=15 & empcase$age_years<25]<-"[15,25)"
empcase$agecat1[empcase$age_years>=25 & empcase$age_years<35]<-"[25,35)"
empcase$agecat1[empcase$age_years>=35 & empcase$age_years<45]<-"[35,45)"
empcase$agecat1[empcase$age_years>=45 & empcase$age_years<55]<-"[45,55)"
empcase$agecat1[empcase$age_years>=55]<-"[55,99)"
#SEX
empcase$sexcat <- ifelse(empcase$sex_female.factor=="Male",1,0)
# HIV
empcase$hiv <- ifelse(!is.na(empcase$labhiv.factor),empcase$labhiv.factor,
                 ifelse(!is.na(empcase$hiv_test_result.factor),empcase$hiv_test_result.factor,
                        ifelse(!is.na(empcase$pretbhiv.factor),empcase$pretbhiv.factor,
                               ifelse(!is.na(empcase$medical_conditions___5),empcase$medical_conditions___5,NA))))
# empcase %>% count(labhiv.factor, hiv_test_result.factor, pretbhiv.factor, medical_conditions___5, hiv)
empcase$hivcat1 <- ifelse(empcase$hiv==1,1,0)
# Diabetes
empcase$db <- ifelse(empcase$medical_conditions___8==0,
                      ifelse(is.na(empcase$hba1c_informed.factor),empcase$medical_conditions___3,empcase$hba1c_informed.factor), NA)
# empcase %>% count(medical_conditions___8, medical_conditions___3, hba1c_informed.factor, db)
empcase$dbcat1 <- empcase$dbcat <- ifelse(empcase$db==1,1,0)
# TB symptoms
empcase$cough <- ifelse(empcase$symptoms_past_week___9==0, ifelse((empcase$symptoms_past_week___1==1|empcase$symptoms_past_week___2==1),1,0), NA)
empcase$fever <- ifelse(empcase$symptoms_past_week___9==0, ifelse(empcase$symptoms_past_week___3==1, 1, 0), NA)
empcase$weight_loss
empcase$nightsweats <- ifelse(empcase$symptoms_past_week___9==0, ifelse(empcase$symptoms_past_week___5==1, 1, 0), NA)
empcase$n_tbsymp <- empcase$n_symp <- empcase$cough + empcase$fever + empcase$weight_loss + empcase$nightsweats
empcase$n_tbsymp1 <- empcase$n_tbsymp <- cut(empcase$n_symp, breaks=c(0,1,2,3,4,5), right=F, labels = c(0,1,2,3,4))
# Duration of TB symptoms (including hemoptysis and taking longer symptom period is different from Kharitode question)
empcase$symps_weeks <- pmax(empcase$cough_weeks, empcase$hemoptysis_weeks, empcase$weightloss_weeks, empcase$nightsweats_weeks, empcase$feverchills_weeks, na.rm=TRUE)
empcase$symp_2wks[empcase$symps_weeks <=2] <-0
empcase$symp_2wks[empcase$symps_weeks >2] <-1
# Non-TB symptoms
empcase$chestpain <- ifelse(empcase$symptoms_past_week___9==0, ifelse(empcase$symptoms_past_week___7==1, 1, 0), NA)
empcase$fatigue <- ifelse(empcase$symptoms_past_week___9==0, ifelse(empcase$symptoms_past_week___4==1, 1, 0), NA)
empcase$shortbreath <- ifelse(empcase$symptoms_past_week___9==0, ifelse(empcase$symptoms_past_week___6==1, 1, 0), NA)
empcase$others <- ifelse(empcase$symptoms_past_week___9==0, ifelse(empcase$symptoms_past_week___8==1, 1, 0), NA)
empcase$n_nontb_symps <- empcase$chestpain + empcase$fatigue + empcase$shortbreath + empcase$others
empcase$n_other_sympcat <- ifelse(empcase$n_nontb_symps==0,0,1)

empcase$score <- ifelse(empcase$agecat=="[25,35)"|empcase$agecat=="[35,45)",1,0) + empcase$sexcat + 2*empcase$hivcat1 + empcase$dbcat + as.numeric(as.character(empcase$n_tbsymp)) + as.numeric(as.character(empcase$symp_2wks)) 
empcase %>% filter(!why_a_case_xpert==1) %>% summarise(min(score, na.rm=T), max(score,na.rm=T), median(score,na.rm=T), quantile(score,0.25,na.rm=T), quantile(score,0.75,na.rm=T))
boxplot(empcase[!(empcase$why_a_case_xpert==1),]$score, ylim=c(0,10),main="(a) Risk score distribution among empiric TB cases", xlab="Empiric TB cases (N=27)", ylab="Risk score")
boxplot(external[external$xpert==1,]$score, ylim=c(0,10),main="(b) Risk score distribution among Xpert-confirmed TB cases", xlab="Xpert-confirmed TB cases (N=106)", ylab="Risk score")
boxplot(external[external$xpert==0,]$score, ylim=c(0,10),main="(c) Risk score distribution among Xpert-negative", xlab="Xpert-negative (N=281)", ylab="Risk score")

external[external$xpert==0,] %>% summarise(median(score, na.rm=T), quantile(score,0.25,na.rm=T), quantile(score,0.75,na.rm=T))

# main %>% filter(case.control=="CC phase control"|(case.control=="COM phase control"&com_ctrl_type=="1")) %>% count(why_a_ctrl_xpert.factor, why_a_ctrl_culture.factor, why_a_ctrl_smear.factor, why_a_ctrl_other___1.factor, why_a_ctrl_other___2.factor, why_a_ctrl_other___5.factor)
empcase$xpert <- 1
empxpop <- rbind((external %>% dplyr::select(xpert, score)),(empcase %>% dplyr::select(xpert, score)))

png(filename = "appendix-ROC-empirical-1_xpert_063020.png")
par(pty='s')
plot(roc(xpert~ score, data=empxpop, ci=T), col=1, type="b")
legend("bottomright", legend=c("Validation data, c-statistic=0.76 (0.71-0.80)"),
       cex=1.0, box.lty=0, bty = "n")
dev.off()


empcase$xpert <- 0
empxpop <- rbind((external %>% dplyr::select(xpert, score)),(empcase %>% dplyr::select(xpert, score)))

png(filename = "appendix-ROC-empirical-0_xpert_063020.png")
par(pty='s')
plot(roc(xpert~ score, data=empxpop, ci=T), col=1, type="b")
legend("bottomright", legend=c("Validation data, c-statistic=0.72 (0.67-0.77)"),
       cex=1.0, box.lty=0, bty = "n")
dev.off()


main %>% filter(case.control=="CC phase enrolled case"|case.control=="CC phase reclassified case"|((case.control=="COM phase enrolled case"|case.control=="COM phase reclassified case")&com_screening_method_case==1)|case.control=="CC phase control"|(case.control=="COM phase control"&com_ctrl_type==1)) %>% count()

main %>% filter(case.control=="CC phase enrolled case"|case.control=="CC phase reclassified case"|((case.control=="COM phase enrolled case"|case.control=="COM phase reclassified case")&com_screening_method_case==1)) %>%
  count(ineligible, caseagreed_screening.factor, com_caseagreed_screening.factor)

main %>% filter(case.control=="CC phase enrolled case"|case.control=="CC phase reclassified case"|((case.control=="COM phase enrolled case"|case.control=="COM phase reclassified case")&com_screening_method_case==1)) %>%
  filter(caseagreed_screening.factor=="Yes"|com_caseagreed_screening.factor=="Yes") %>% count(why_a_case_xpert.factor, com_why_a_case_xpert.factor)

main %>% filter(case.control=="CC phase enrolled case"|case.control=="CC phase reclassified case") %>%
  filter(caseagreed_screening.factor=="Yes") %>% count(why_a_case_xpert.factor, why_a_case_xpert.factor, why_a_case_culture.factor, why_a_case_smear.factor, why_a_case_other___1.factor, why_a_case_other___2.factor, why_a_case_other___3.factor, why_a_case_other___5.factor, why_a_case_specifyother, clinical_diagnosis.factor) %>% view()

main %>% filter((case.control=="COM phase enrolled case"|case.control=="COM phase reclassified case")&com_screening_method_case==1) %>%
  filter(com_caseagreed_screening.factor=="Yes") %>% count(com_why_a_case_xpert.factor, com_why_a_case_xpert.factor, com_why_a_case_culture.factor, com_why_a_case_smear.factor, com_why_a_case_other___1.factor, com_why_a_case_other___2.factor, com_why_a_case_other___3.factor, com_why_a_case_other___5.factor, clinical_diagnosis.factor) %>% view()


main %>% filter(case.control=="CC phase control"|(case.control=="COM phase control"&com_ctrl_type==1)) %>%
  count(ineligible, agreed_screening_ctrl.factor, com_agreed_screening_ctrl.factor)

main %>% filter(case.control=="CC phase control") %>%
  filter(agreed_screening_ctrl.factor=="Yes") %>% count(why_a_ctrl_xpert.factor, why_a_ctrl_culture.factor, why_a_ctrl_smear.factor, why_a_ctrl_other___1.factor, why_a_ctrl_other___2.factor, why_a_ctrl_other___5.factor)

main %>% filter((case.control=="COM phase control"&com_ctrl_type==1)) %>%
  filter((com_agreed_screening_ctrl.factor=="Yes")) %>% count(com_why_a_ctrl_xpert.factor, com_why_a_ctrl_culture.factor, com_why_a_ctrl_smear.factor, com_why_a_ctrl_other___1.factor, com_why_a_ctrl_other___2.factor, com_why_a_ctrl_other___5.factor)


kh_merge %>% filter(age1>=15) %>% filter(!(symp_fac___1==0&symp_fac___2==0&symp_fac___3==0&symp_fac___4==0)) %>% filter(!is.na(symp_fac___1)&!is.na(symp_fac___2)&!is.na(symp_fac___3)&!is.na(symp_fac___4)) %>% count(xpert_status_fac)
kh_merge %>% filter(age1>=15)%>% filter(consent==1&new_suspect_fac==1&(ic_adult_fac==1|(ic_adult_fac==77&ic_adol_fac==1))) %>% count(xpert_status_fac)
kh_merge %>% filter(!(symp_fac___1==0&symp_fac___2==0&symp_fac___3==0&symp_fac___4==0)) %>% filter(age1>=15) %>% count(xpert_status_fac)
kh_merge %>% filter(!(symp_fac___1==0&symp_fac___2==0&symp_fac___3==0&symp_fac___4==0)) %>% filter(age1>=15) %>% filter (consent==1&enrollment_form_complete==2&new_suspect_fac==1&(ic_adult_fac==1|(ic_adult_fac==77&ic_adol_fac==1))&facility_crf_complete==2) %>% count(xpert_status_fac)

newfd_xpert_completecase %>% filter(!is.na(n_tbsymp)) %>% count(xpert)

newfd_xpert_completecase %>% filter(!(symp_fac___1==0&symp_fac___2==0&symp_fac___3==0&symp_fac___4==0)) %>% count(xpert, xpert_status_fac, xpert_status_fac_1, xpert_faclog.factor)
newfd_xpert_completecase %>% filter(age1>=15) %>% filter (consent==1&enrollment_form_complete==2&new_suspect_fac==1&(ic_adult_fac==1|(ic_adult_fac==77&ic_adol_fac==1))&facility_crf_complete==2) %>% filter(!is.na(n_tbsymp)) %>% count(xpert, xpert_status_fac, xpert_status_fac_1, xpert_faclog.factor) 
newfd %>% filter(!(symp_fac___1==0&symp_fac___2==0&symp_fac___3==0&symp_fac___4==0)) %>% filter(age1>=15) %>% filter (consent==1&enrollment_form_complete==2&new_suspect_fac==1&(ic_adult_fac==1|(ic_adult_fac==77&ic_adol_fac==1))&facility_crf_complete==2) %>% count(n_tbsymp)


newfd %>% count(xpert) 
newfd %>% filter(!is.na(n_tbsymp)) %>% count(xpert) 
newfd %>% filter(!is.na(n_tbsymp)) %>% filter (consent==1&enrollment_form_complete==2&new_suspect_fac==1&(ic_adult_fac==1|(ic_adult_fac==77&ic_adol_fac==1))&facility_crf_complete==2) %>% count(xpert) #xpert_status_fac, xpert_status_fac_1, xpert_faclog.factor

newfd %>% filter(!(symp_fac___1==0&symp_fac___2==0&symp_fac___3==0&symp_fac___4==0)) %>% filter(!is.na(n_tbsymp))%>% count(xpert)
newfd %>% filter(!(symp_fac___1==0&symp_fac___2==0&symp_fac___3==0&symp_fac___4==0)) %>% filter(!is.na(n_tbsymp))%>% group_by(xpert) %>% count(hiv_status_fac) # xpert
newfd %>% filter(!(symp_fac___1==0&symp_fac___2==0&symp_fac___3==0&symp_fac___4==0)) %>% filter(!is.na(n_tbsymp))%>% group_by(xpert) %>% count(hivcat) # xpert


newfd_xpert_completecase %>% count(xpert)
newfd_xpert_completecase %>% filter(!is.na(n_tbsymp)) %>% count(xpert)
newfd_xpert_completecase %>% filter(!is.na(n_tbsymp)) %>% group_by(xpert)%>% count(hiv_status_fac, hivcat1)
newfd_xpert_completecase %>% filter(!is.na(n_tbsymp)) %>% count(hiv_status_fac, hivcat1)
newfd_xpert_completecase %>% filter(!is.na(n_tbsymp)) %>% group_by(sexcat) %>% count(eversmoke)
newfd_xpert_completecase %>% filter(!is.na(n_tbsymp)) %>% group_by(hivcat1) %>% count(pasttb)
newfd_xpert_completecase %>% filter(!is.na(n_tbsymp)) %>% group_by(pasttb) %>% count(hivcat1)
