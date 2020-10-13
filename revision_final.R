### Data setup
newfd_xpert_completecase <- newfd[which(!is.na(newfd$xpert)),] # Delete the missing outcome
newfd_xpert_completecase <- newfd_xpert_completecase %>% filter(is.na(n_tbsymp)|n_tbsymp==1|n_tbsymp==2|n_tbsymp==3|n_tbsymp==4)
# If variable management is needed...
newfd_xpert_completecase$agecat3[newfd_xpert_completecase$agecat1=="[15,25)"] <- "1"
newfd_xpert_completecase$agecat3[newfd_xpert_completecase$agecat1=="[25,35)"] <- "2"
newfd_xpert_completecase$agecat3[newfd_xpert_completecase$agecat1=="[35,45)"] <- "3"
newfd_xpert_completecase$agecat3[newfd_xpert_completecase$agecat1=="[45,55)"] <- "4"
newfd_xpert_completecase$agecat3[newfd_xpert_completecase$agecat1=="[55,99)"] <- "0"
# Choose the columns to use from the (original) data
newfd.original <- newfd_xpert_completecase %>% dplyr::select(n_tbsymp, symp_2wks, agecat3, sexcat, hivcat1, dbcat, n_other_sympcat, edu8, pasttb, eversmoke, lungcat, xpert) #symp_fac___1cat, symp_fac___2cat, symp_fac___3cat, symp_fac___4cat, 
# Performing MI on the original samples
newfd.original[] <- lapply(newfd.original, function(x){return(as.factor(x))})
Imput.own <- mice(newfd.original,7,pri=F)
# Extract MI datasets to each dataset
data.imputed.Boot.own <- vector("list",7)    
for (i in 1:7){
  data.imputed.Boot.own[[i]]=complete(Imput.own,i) 
} 



### Get the coefficients and intercept from Lasso regression
coef=list()
coeftotal=matrix(nrow=15)
for(i in 1:7){
d <- data.imputed.Boot.own[[i]][,c(-8,-11)]
d$xpert <- as.numeric(as.character(d$xpert)) # Make the class of the oucome factor to numeric
x_vars <- model.matrix(xpert~. , d)[,-1]
y_var <- d$xpert
cv <- cv.glmnet(x_vars, y_var, nfold=10, alpha = 1, family="binomial") # Estimate the shrinkage factor ('nfold=10' means splitting into 10 subsamples, building Lasso based on 9, and testing/estimating from 1)
lambda <- cv$lambda.min # Get the best shrinkage factor
lasso_best <- glmnet(x_vars, y_var, alpha = 1, lambda = lambda, family="binomial") # Lasso regression (coefficient must be the mean across the 20 bootstrapping samples)
coef[[i]] <- coef(lasso_best)
coeftotal <- cbind(coeftotal,coef[[i]])
}
coeftotal <- coeftotal[,-1]
round(rowMeans(coeftotal)/median(c(0.6877477, 0.8537912, 0.9183592, 0.5077977, 0.9620264, 1.2181799, 0.7590437)))
# confint(lasso_best)
# Make a simple scoring system
# median(0.66786770,0.83973696,0.81594505,0.92502417,1.20714530 ,0.70729174) #0.6678677
# round(rowMeans(coeftotal)/0.6678677)





### Actual coefficient scoring system based on the Lasso regression --> BELOW AGAIN!
# (Intercept)        n_tbsymp2        n_tbsymp3        n_tbsymp4       symp_2wks1         agecat31         agecat32 
# -2.67308092       0.66786770       1.68024123       2.27979890       0.83973696       0.28886628       0.81594505 
# agecat33         agecat34          sexcat1         hivcat11           dbcat1 n_other_sympcat1            edu81 
# 0.41984499       0.14742784       0.92502417       1.20714530       0.70729174       0.25531847      -0.07751068 
# pasttb1       eversmoke1         lungcat1 
# 0.12307903      -0.19609719      -0.14438743 

# THIS IS REAL!
# (Intercept)        n_tbsymp2        n_tbsymp3        n_tbsymp4       symp_2wks1         agecat31         agecat32 
# -2.8088942        0.6877477        1.7048544        2.3072742        0.8537912        0.3904556        0.9183592 
# agecat33         agecat34          sexcat1         hivcat11           dbcat1 n_other_sympcat1          pasttb1 
# 0.5077977        0.2130624        0.9620264        1.2181799        0.7590437        0.2613170        0.1331025 
# eversmoke1 
# -0.2316981 




### Simple scoring system based on the Lasso regression
# (Intercept)        n_tbsymp2        n_tbsymp3        n_tbsymp4       symp_2wks1         agecat31         agecat32 
# -4                1                3                3                1                0                1 
# agecat33         agecat34          sexcat1         hivcat11           dbcat1 n_other_sympcat1            edu81 
# 1                0                1                2                1                0                0 
# pasttb1       eversmoke1         lungcat1 
# 0                0                0 

# > round(rowMeans(coeftotal)/median(c(0.6877477, 0.8537912, 0.9183592, 0.5077977, 0.9620264, 1.2181799, 0.7590437)))
# (Intercept)        n_tbsymp2        n_tbsymp3        n_tbsymp4       symp_2wks1         agecat31         agecat32 
# -3                1                2                3                1                0                1 
# agecat33         agecat34          sexcat1         hivcat11           dbcat1 n_other_sympcat1          pasttb1 
# 1                0                1                1                1                0                0 
# eversmoke1 
# 0 


### Adjusting factor
pre_tb <- 106/387 # STOMP
post_tb <- 707/1409 # Kharitode



### Edit data and back to 'mids'
data.imputed.own.new <- complete(Imput.own, action="long", include=T)
data.imputed.own.new$score <- ifelse(data.imputed.own.new$agecat3=="2",1,0) + ifelse(data.imputed.own.new$sexcat==1,1,0) + ifelse(data.imputed.own.new$hivcat1==1,2,0) + as.numeric(as.character(data.imputed.own.new$n_tbsymp)) + ifelse(data.imputed.own.new$symp_2wks==1,1,0) + ifelse(data.imputed.own.new$dbcat==1,1,0) +
  0*ifelse(data.imputed.own.new$n_other_sympcat==1,0,0) + 0*ifelse(data.imputed.own.new$edu8==1,0,0) + 0*ifelse(data.imputed.own.new$pasttb==1,0,0) + 0*ifelse(data.imputed.own.new$eversmoke==1,0,0) + 0*ifelse(data.imputed.own.new$lungcat==1,0,0)
data.imputed.own.new$score.c<-ifelse(data.imputed.own.new$n_tbsymp==2,0.66786770,0) + ifelse(data.imputed.own.new$n_tbsymp==3,1.68024123,0) + ifelse(data.imputed.own.new$n_tbsymp==4,2.27979890,0) + 
                  ifelse(data.imputed.own.new$symp_2wks==1,0.83973696,0) + ifelse(data.imputed.own.new$agecat3=="1",0.28886628,0) + ifelse(data.imputed.own.new$agecat3=="2",0.81594505,0) + ifelse(data.imputed.own.new$agecat3=="3",0.41984499,0) + ifelse(data.imputed.own.new$agecat3=="4",0.14742784,0) +
                  ifelse(data.imputed.own.new$sexcat==1,0.92502417,0) + ifelse(data.imputed.own.new$hivcat1==1,1.20714530,0) + ifelse(data.imputed.own.new$dbcat==1,0.70729174,0) +
                  ifelse(data.imputed.own.new$n_other_sympcat==1,0.25531847,0) + ifelse(data.imputed.own.new$edu8==1,-0.07751068,0) + ifelse(data.imputed.own.new$pasttb==1,0.12307903,0) + 
                  ifelse(data.imputed.own.new$eversmoke==1,-0.19609719,0) + ifelse(data.imputed.own.new$lungcat==1,-0.14438743,0)
data.imputed.own.new$score810 <- data.imputed.own.new$score
data.imputed.own.new$score810[data.imputed.own.new$score810==8] <-10
data.imputed.own.new$score810[data.imputed.own.new$score810==9] <-10 
Imput.own.back <- as.mids(data.imputed.own.new, .imp=1)





# Score distribution
data.imputed.own.new %>% group_by(xpert) %>% summarize(mean(score, na.rm=T), quantile(score, probs=0.25, na.rm=T), quantile(score, probs=0.75, na.rm=T))
external %>% group_by(xpert) %>% summarize(mean(score, na.rm=T), quantile(score, probs=0.25, na.rm=T), quantile(score, probs=0.75, na.rm=T))

                      
                      
                      
                      
# test if this Imput.own.back is working as same as below work ### IT WORKED WELL!
fit.mi <- with(Imput.own.back, glm(xpert ~ score, family = binomial()))$analyses
pred.mi <- fit.mi %>% predict(external.subset, type="response")
pred.mi.mean <- rowMeans(matrix(unlist(pred.mi),nrow=387, ncol=7))
cal<-val.prob.ci.2(pred.mi.mean, as.numeric(as.character(external.subset$xpert)), logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black")

png(filename = "calibration_noadjusted.png")
par(margin(t=2,b=2,l=2,r=2))
val.prob.ci.2(pred.mi.mean, as.numeric(as.character(external.subset$xpert)), logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black", cex.leg = 1.1, cex=1.1, cex.axis=1.3, cex.d01 = 1.1, cex.lab=1.3, main="A")
dev.off()

fit.adj.mi <- with(Imput.own.back, glm(xpert ~ score, family=binomial()))$analyses
for(i in 1:7){fit.adj.mi[[i]]$coefficients[['(Intercept)']] <- fit.adj.mi[[i]]$coefficients[['(Intercept)']] + log((pre_tb/(1-pre_tb))/(post_tb/(1-post_tb)))}
pred.adj.mi <- fit.adj.mi %>% predict(external.subset, type="response")
pred.adj.mi.mean <- rowMeans(matrix(unlist(pred.adj.mi), nrow=387, ncol=7))
cal.adj<-val.prob.ci.2(pred.adj.mi.mean, as.numeric(as.character(external.subset.c$xpert)), logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black", cex.leg = 1.1, cex=1.0,  cex.d01 = 1.1, cex.lab=1.2)

png(filename = "calibration_adjusted.png")
par(pty='m')
val.prob.ci.2(pred.adj.mi.mean, as.numeric(as.character(external.subset.c$xpert)), logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black", cex.leg = 1.1, cex=1.1, cex.axis=1.3, cex.d01 = 1.3, cex.lab=1.3, main="A")
dev.off()


fit.c.mi <- with(Imput.own.back, glm(xpert ~ score.c, family=binomial()))$analyses
pred.c.mi <- fit.c.mi[[1]] %>% predict(external.subset.c, type="response")
pred.c.mi.mean <- rowMeans(matrix(unlist(pred.c.mi), nrow=387, ncol=7))

png(filename="calibration_coeff_noadjusted.png")
val.prob.ci.2(pred.c.mi.mean, as.numeric(as.character(external.subset.c$xpert)), logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black", cex.leg = 1.1, cex=1.1, cex.axis=1.3, cex.d01 = 1.3, cex.lab=1.3, main="B")
dev.off()


fit.c.adj.mi <- with(Imput.own.back, glm(xpert ~ score.c, family=binomial()))$analyses
for(i in 1:7){fit.c.adj.mi[[i]]$coefficients[['(Intercept)']] <- fit.c.adj.mi[[i]]$coefficients[['(Intercept)']] + log((pre_tb/(1-pre_tb))/(post_tb/(1-post_tb)))}
pred.c.adj.mi <- fit.c.adj.mi %>% predict(external.subset.c, type="response")
pred.c.adj.mi.mean <- rowMeans(matrix(unlist(pred.c.adj.mi), nrow=387, ncol=7))

png(filename="calibration_coeff_adjusted.png")
par(margin(t=2,b=2,l=2,r=2))
val.prob.ci.2(pred.c.adj.mi.mean, as.numeric(as.character(external.subset.c$xpert)), logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black",cex.leg = 1.1, cex=1.1, cex.axis=1.3, cex.d01 = 1.3, cex.lab=1.3)
dev.off()


# Score 8+ Prediction vs. Obs
fit810 <- with(Imput.own.back, glm(xpert ~ score810, family=binomial()))$analyses
for(i in 1:7){fit.c.adj.mi[[i]]$coefficients[['(Intercept)']] <- fit.c.adj.mi[[i]]$coefficients[['(Intercept)']] + log((pre_tb/(1-pre_tb))/(post_tb/(1-post_tb)))}
pred810 <- fit810 %>% stats::predict(external, type="response")
meanpred810 <- rowMeans(matrix(unlist(pred810), nrow=387, ncol=7))

socre810predobs <- as.data.frame(cbind(external$score810, meanpred810))
socre810predobs %>% group_by(V1) %>% summarize(mean(meanpred810, na.rm=T))
external %>% group_by(xpert)%>% count(score810)


# fit.adj.mi.810 <- with(Imput.own.back[[1]], glm(xpert ~ score810, family=binomial()))
# for(i in 1:7){fit.adj.mi.810[[i]]$coefficients[['(Intercept)']] <- fit.adj.mi.810[[i]]$coefficients[['(Intercept)']] + log((pre_tb/(1-pre_tb))/(post_tb/(1-post_tb)))}
# pred.adj.mi.810 <- fit.adj.mi.810 %>% predict(external.subset, type="response")
# pred.adj.mi.mean.810 <- rowMeans(matrix(unlist(pred.adj.mi.810), nrow=387, ncol=7))
# val.prob.ci.2(pred.adj.mi.mean.810, as.numeric(as.character(external.subset.c$xpert)), logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
#                        ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
#                        lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black", cex.leg = 1.1, cex=1.0,  cex.d01 = 1.1, cex.lab=1.2)
# 


# ### Make a score based on the simple scoring system WITHOUT adjustment
# score <- with(Imput.own, score<-ifelse(agecat3=="2",1,0) + ifelse(sexcat==1,1,0) + ifelse(hivcat1==1,2,0) + as.numeric(as.character(n_tbsymp)) + ifelse(symp_2wks==1,1,0) + ifelse(dbcat==1,1,0) +
#                 0*ifelse(n_other_sympcat==1,0,0) + 0*ifelse(edu8==1,0,0) + 0*ifelse(pasttb==1,0,0) + 0*ifelse(eversmoke==1,0,0) + 0*ifelse(lungcat==1,0,0))$analyses
# rowMeans(matrix(unlist(score),nrow=1409, ncol=7))[rowMeans(matrix(unlist(score),nrow=1409, ncol=7))>8]
# # Don't apply if score as continuous
# for(i in 1:7){return(as.factor(score[[i]]))}
# # Don't apply if score as continuous
# for(i in 1:7){score[[i]][score[[i]]==8|score[[i]]==9]<-'8+'}
# 
# for(i in 1:7){data.imputed.Boot.own[[i]]$score <- score[[i]]}
# # for(i in 1:7); fit <- glm(xpert ~ as.factor(score), data=data.imputed.Boot.own[[1]], family=binomial())
# fit <- glm(xpert ~ score, data=data.imputed.Boot.own[[6]], family=binomial())
# 
# # external.subset <- external[,c("score","xpert")]
# # external.subset$score <- as.numeric(as.character(external.subset$score)) #Back to continuous if score as continuous
# for(i in 1:7); 
# pred6 <- fit %>% predict(external.subset, type="response")
# pred <- cbind(pred1,pred2,pred3,pred4,pred5,pred6,pred7)
# pred.mean <- rowMeans(matrix(unlist(pred),nrow=387, ncol=7))
# 
# png(filename = "Simple scoring noadjustment.png")
# val.prob.ci.2(pred.mean, as.numeric(as.character(external.subset$xpert)), logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
#               ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
#               lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black")
# dev.off()
# 
# 
# 
# ### Make a score based on the simple scoring system WITH adjustment
# pre_tb <- 106/387
# post_tb <- 707/1409
# for(i in 1:7){data.imputed.Boot.own[[i]]$score <- score[[i]]}
# for(i in 1:7);
# # fit.adj <- glm(xpert ~ as.factor(score), data=data.imputed.Boot.own[[1]], family=binomial())
# fit.adj <- glm(xpert ~ score, data=data.imputed.Boot.own[[7]], family=binomial())
# fit.adj$coefficients[['(Intercept)']] <- fit.adj$coefficients[['(Intercept)']] + log((pre_tb/(1-pre_tb))/(post_tb/(1-post_tb)))
# 
# # external.subset <- external[,c("score","xpert")]
# for(i in 1:7); 
# pred.adj7 <- fit.adj %>% predict(external.subset, type="response")
# pred.adj <- cbind(pred.adj1,pred.adj2,pred.adj3,pred.adj4,pred.adj5,pred.adj6,pred.adj7)
# pred.adj.mean <- rowMeans(matrix(unlist(pred.adj),nrow=387, ncol=7))
# 
# png(filename = "Simple scoring adjusted.png")
# val.prob.ci.2(pred.adj.mean, as.numeric(as.character(external.subset$xpert)), logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
#               ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
#               lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black")
# dev.off()
# 
# 
# 
# 
# ### Make a score based on the actual coefficient scoring system WITHOUT adjustment
# score.c <- with(Imput.own, score.c<-ifelse(n_tbsymp==2,0.66786770,0) + ifelse(n_tbsymp==3,1.68024123,0) + ifelse(n_tbsymp==4,2.27979890,0) + 
#                 ifelse(symp_2wks==1,0.83973696,0) + ifelse(agecat3=="1",0.28886628,0) + ifelse(agecat3=="2",0.81594505,0) + ifelse(agecat3=="3",0.41984499,0) + ifelse(agecat3=="4",0.14742784,0) +
#                 ifelse(sexcat==1,0.92502417,0) + ifelse(hivcat1==1,1.20714530,0) + ifelse(dbcat==1,0.70729174,0) +
#                 ifelse(n_other_sympcat==1,0.25531847,0) + ifelse(edu8==1,-0.07751068,0) + ifelse(pasttb==1,0.12307903,0) + 
#                 ifelse(eversmoke==1,-0.19609719,0) + ifelse(lungcat==1,-0.14438743,0))$analyses
# rowMeans(matrix(unlist(score.c),nrow=1409, ncol=7)); max(rowMeans(matrix(unlist(score.c),nrow=1409, ncol=7)))
# for(i in 1:7){return(as.factor(score.c[[i]]))}
# 
# for(i in 1:7){data.imputed.Boot.own[[i]]$score.c <- score.c[[i]]}
# for(i in 1:7); 
# fit.c <- glm(xpert ~ score.c, data=data.imputed.Boot.own[[7]], family=binomial())
# 
# # external.subset.c <- external[,c("score.c","xpert")]
# for(i in 1:7); 
# pred.c7 <- fit.c %>% predict(external.subset.c, type="response")
# pred.c <- cbind(pred.c1,pred.c2,pred.c3,pred.c4,pred.c5,pred.c6,pred.c7)
# pred.c.mean <- rowMeans(matrix(unlist(pred.c),nrow=387, ncol=7))
# 
# png(filename = "Coefficient scoring noadjustment.png")
# val.prob.ci.2(pred.c.mean, as.numeric(as.character(external.subset$xpert)), logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
#               ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
#               lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black")
# dev.off()
# 
# 
# 
# ### Make a score based on the actual coefficient scoring system WITH adjustment
# pre_tb <- 106/387
# post_tb <- 707/1409
# for(i in 1:7){data.imputed.Boot.own[[i]]$score.c <- score.c[[i]]}
# for(i in 1:7); 
# fit.c.adj <- glm(xpert ~ score.c, data=data.imputed.Boot.own[[7]], family=binomial())
# fit.c.adj$coefficients[['(Intercept)']] <- fit.c.adj$coefficients[['(Intercept)']] + log((pre_tb/(1-pre_tb))/(post_tb/(1-post_tb)))
# 
# # external.subset.c <- external[,c("score.c","xpert")]
# for(i in 1:7); 
# pred.c.adj7 <- fit.c.adj %>% predict(external.subset.c, type="response")
# pred.c.adj <- cbind(pred.c.adj1,pred.c.adj2,pred.c.adj3,pred.c.adj4,pred.c.adj5,pred.c.adj6,pred.c.adj7)
# pred.c.adj.mean <- rowMeans(matrix(unlist(pred.c.adj),nrow=387, ncol=7))
# 
# png(filename = "Coefficient scoring adjusted.png")
# val.prob.ci.2(pred.c.adj.mean, as.numeric(as.character(external.subset.c$xpert)), logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
#               ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
#               lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black")
# dev.off()






### ROC
# Sinlge, own derivation data (multiple imputation)
roc.single.kharitode <- with(Imput.own.back, roc(xpert~ score, ci=T))$analyses #glm(xpert~score, family=binomial())$fitted.values, plot=T, ci=T, col=2))
# Single, external data
roc.single.STOMP <- roc(xpert~ score810, data=external, ci=T)
# Test in 10% prevalence - single, external data
roc.single.STOMP.10 <- roc(xpert~ score, data=external.subset.10)
# Plot single scoring ROCs
png(filename = "ROC_sinlge scoring.png")
par(pty='s')
plot(roc.single.kharitode[[1]], col=1, type="b")
for(i in 2:7){plot(roc.single.kharitode[[i]], col=1, add=T)}
plot(roc.single.STOMP, col=2, add=T, type="b")
legend("bottomright", legend=c("Derivation data, c-statistic=0.82(0.81-0.82)", "Validation data, c-statistic=0.75(0.69-0.80)"),
       col=c(1,2), lty=1:1, cex=0.8, box.lty=0, bty = "n")
dev.off()


### Score 8+
roc.single.kharitode.810 <- with(Imput.own.back, roc(xpert~ score810, ci=T))$analyses #glm(xpert~score, family=binomial())$fitted.values, plot=T, ci=T, col=2))
roc.single.STOMP.810 <- roc(xpert~ score810, data=external, ci=T)

png(filename = "ROC_sinlge scoring_score810.png")
par(pty='s')
plot.roc(roc.single.kharitode.810[[1]], col=1, type="o",print.thres=c(1,2,3,4,5,6,7,10), print.thres.pattern = (c('1','2','3','4','5','6','7','8+')), cex.axis=1.2, cex.lab=1.5, main="B\n")
for(i in 2:7){plot(roc.single.kharitode[[i]], col=1, add=T)}
plot(roc.single.STOMP, col=2, add=T, type="o",print.thres=c(1,2,3,4,5,6,7,10), print.thres.pattern = (c('1','2','3','4','5','6','7','8+')), print.thres.col="red")
par(pty='m')
legend("bottomright", legend=c("Derivation data, c-statistic=0.82(0.81-0.82)", "Validation data, c-statistic=0.75(0.69-0.80)"),
       col=c(1,2), lty=1:1, cex=1.0, box.lty=0, bty = "n")
dev.off()


plot.roc(roc.single.kharitode.810[[7]], col=1, type="o",print.thres=c(1,2,3,4,5,6,7,10))
for(i in 2:7){plot(roc.single.kharitode[[i]], col=1, add=T)}
plot(roc.single.STOMP, col=2, add=T, type="o",print.thres=c(1,2,3,4,5,6,7,10),  print.thres.col="red")



par(mar=c(10,5,1,1))
plot.roc(roc.single.STOMP, type="o",print.thres=c(1,2,3,4,5,6,7,10), col=2,print.thres.pattern = (c('1','2','3','4','5','6','7','8+')), print.auc=T)


class(external.subset810$score810)
external.subset810 %>% count(score810)
roc810<-roc(xpert ~ score810, data=external.subset810, ci=T, levels=base::levels(as.factor(external.subset810$xpert)), plot=T)
roc810$thresholds <- c(1,2,3,4,5,6,7,8)
plot.roc(roc810, type="o")
roc(xpert ~ score810, data=external.subset810, ci=T)$sensitivities[-1]
roc(xpert ~ score810, data=external.subset810, ci=T)$specificities[-1]
table(roc(xpert ~ score810, data=external.subset810, ci=T)$predictor, roc(xpert ~ score810, data=external.subset810, ci=T)$original.response)
plot(1-roc(xpert ~ score810, data=external.subset810, ci=T)$specificities[-1], roc(xpert ~ score810, data=external.subset810, ci=T)$sensitivities[-1], type='o')




# Get pooled mean and SD for multiple imputation (sinlge, own derivation data)
roc.single.kharitode.auc <- with(Imput.own.back, roc(xpert~ score, ci=T)$auc)
me = mean(unlist(roc.single.kharitode.auc$analyses))
m = 7
Vb = var(unlist(roc.single.kharitode.auc$analyses)); for(i in 1:7){Vw0[[i]] <- (roc.single.kharitode.auc$analyses[[i]]-me)^2}; Vw = (1/(m-1))*sum(Vw0)
SEp = sqrt(sum(Vb, Vw, Vb/m))
t = (2.080 + 2.086)/2 # lambda = sum(Vb, Vb/m)/sum(Vb, Vw, Vb/m) # dfo = (m-1)/(lambda)^2 # dfb = ((1634-15)+1)/((1634-15)+3)*(1634-15)*(1-lambda) # dfa = dfo*dfb/(dfo+dfb)
l95ci = me - t*SEp; h95ci = me + t*SEp




# Coefficient, own derivation data (multiple imputation)
roc.coeff.kharitode <- with(Imput.own.back, roc(xpert~ score.c, ci=T))$analyses
# Coefficient, external data
roc.coeff.STOMP <- roc(xpert~ score.c, data=external.subset.c, ci=T)
# Plot coefficient scoring ROCs
png(filename = "ROC_coeffcient scoring_cex.png")
par(pty='s')
plot(roc.coeff.kharitode[[1]], col=1, cex.lab=1.5, cex.axis=1.3, main="A")
for(i in 2:7){plot(roc.coeff.kharitode[[i]], col=1, add=T)}
plot(roc.coeff.STOMP, col=2, add=T)
par(pty='m')
legend("bottomright", legend=c("Derivation data, c-statistic=0.83(0.82-0.83)", "Validation data, c-statistic=0.77(0.71-0.82)"),
       col=c(1,2), lty=1:1, cex=1.2, box.lty=0, bty = "n")
dev.off()
# Get pooled mean and SD for multiple imputation (sinlge, own derivation data)
roc.coeff.kharitode.auc <- with(Imput.own.back, roc(xpert~ score.c, ci=T)$auc)
me = mean(unlist(roc.coeff.kharitode.auc$analyses))
m = 7
Vb = var(unlist(roc.coeff.kharitode.auc$analyses)); for(i in 1:7){Vw0[[i]] <- (roc.coeff.kharitode.auc$analyses[[i]]-me)^2}; Vw = (1/(m-1))*sum(Vw0)
SEp = sqrt(sum(Vb, Vw, Vb/m))
t = (2.080 + 2.086)/2 # lambda = sum(Vb, Vb/m)/sum(Vb, Vw, Vb/m) # dfo = (m-1)/(lambda)^2 # dfb = ((1634-15)+1)/((1634-15)+3)*(1634-15)*(1-lambda) # dfa = dfo*dfb/(dfo+dfb)
l95ci = me - t*SEp; h95ci = me + t*SEp





### Kharitode, 5%, 10%, and 20% predicted probabilities by score
table.mi.810 <- with(Imput.own.back, prop.table(table(score810, xpert),2))$analyses
sens.mi.810 <-list(); for(i in 1:7){sens.mi.810 <- cbind(sens.mi.810, table.mi.810[[i]][1:8,2])}
sens.mi.dt.810 <- as.data.frame(rowMeans(matrix(unlist(sens.mi.810),nrow=8,ncol=7)))
inverspec.mi.810 <-list(); for(i in 1:7){inverspec.mi.810 <- cbind(inverspec.mi.810, table.mi.810[[i]][1:8,1])}
inverspec.mi.dt.810 <- as.data.frame(rowMeans(matrix(unlist(inverspec.mi.810),nrow=8,ncol=7)))
spec.mi.dt.810 <- 1 - inverspec.mi.dt.810

prevalence <- 707/1409
predicted.probability.kharitode.810 <- (prevalence*sens.mi.dt.810) / (prevalence*sens.mi.dt.810 + (1-prevalence)*(1-spec.mi.dt.810))

prevalence <- 0.025
predicted.probability.01.810 <- (prevalence*sens.mi.dt.810) / (prevalence*sens.mi.dt.810 + (1-prevalence)*(1-spec.mi.dt.810))

prevalence <- 0.05
predicted.probability.05.810 <- (prevalence*sens.mi.dt.810) / (prevalence*sens.mi.dt.810 + (1-prevalence)*(1-spec.mi.dt.810))

prevalence <- 0.10
predicted.probability.10.810 <- (prevalence*sens.mi.dt.810) / (prevalence*sens.mi.dt.810 + (1-prevalence)*(1-spec.mi.dt.810))

prevalence <- 0.15
predicted.probability.15.810 <- (prevalence*sens.mi.dt.810) / (prevalence*sens.mi.dt.810 + (1-prevalence)*(1-spec.mi.dt.810))

prevalence <- 0.20
predicted.probability.20.810 <- (prevalence*sens.mi.dt.810) / (prevalence*sens.mi.dt.810 + (1-prevalence)*(1-spec.mi.dt.810))

Figure2.810 <- cbind(predicted.probability.05.810, predicted.probability.10.810, predicted.probability.20.810)
write.csv(Figure2.810, 'Figure2revision_810.csv')

Figure2.810.062820 <- cbind(predicted.probability.01.810, predicted.probability.05.810, predicted.probability.10.810, predicted.probability.20.810)
write.csv(Figure2.810.062820, 'Figure2revision_810_062820.csv')





### DCA in validation population (10% prevalence)
# data setup
external.subset %>% count(xpert) # 106*9/281 times expansion of xpert negative population needed
external.subset.10 <- rbind(external.subset, external.subset %>% filter(xpert==0), external.subset %>% filter(xpert==0), external.subset %>% filter(xpert==0)) # 9% TB prevalence

# no need to show in the model derivation population but still set up 10% prevalence data
# newfd_xpert_completecase %>% count(xpert) # 707*9/702 times
# data.imputed.own.new.10 <- complete(Imput.own.back, action="long", include=T)
# data.imputed.own.new.10.10 <- rbind(data.imputed.own.new.10, data.imputed.own.new.10 %>% filter(xpert==0), data.imputed.own.new.10 %>% filter(xpert==0), data.imputed.own.new.10 %>% filter(xpert==0), data.imputed.own.new.10 %>% filter(xpert==0), data.imputed.own.new.10 %>% filter(xpert==0), data.imputed.own.new.10 %>% filter(xpert==0), data.imputed.own.new.10 %>% filter(xpert==0), data.imputed.own.new.10 %>% filter(xpert==0)) # 10% TB prevalence
# Imput.own.back.10 <- as.mids(data.imputed.own.new.10.10, .imp=1)
# for(i in 1:7){Imput.own.back.10 <- rbind()}

# DCA
dca.validation <- decision_curve(xpert~score, data=external.subset.10, family=binomial, fitted.risk = FALSE,
                    thresholds = seq(0, 1, by = 0.01), study.design = 'case-control', confidence.intervals = 0.95, bootstraps = 500)
# png(filename = "DCA_revision_externalvalidation_10prev.png")
# plot_decision_curve(list(dca.validation), curve.names = ("External validation data"),
#                     col = ("red"), confidence.intervals = TRUE, cost.benefit.axis = TRUE) 
# dev.off()
png(filename = "DCA_revision_externalvalidation_10prev_biggercex.png", width=787, height=634)
par(mar=c(10,5,1,2))
plot(dca.validation$derived.data[c(1:101),]$thresholds, dca.validation$derived.data[c(1:101),]$sNB, type='l', ylim=c(-0.2,1), col=2, bty='n', xaxt='n', ylab='Standardized Net Benefit', xlab='', cex.lab=1.4, cex.axis=1.2) #, main="Decision curve analysis in the validation population \n(10% TB prevalence scenario)", bty='n', xaxt='n', ylab='Standardized Net Benefit', xlab='', cex.main=0.99
abline(h=0, col=1, lty=2)
xtick <- seq(0.0,1.0, by=0.20)
axis(side=1,at=xtick, cex.axis=1.2) #mtext(c("Risk threshold","Cost:Benefit Ratio"),pos=c(0.4,-0.7))
axis(side=1,at=xtick, labels=c("1:100","1:4","2:3","3:2","4:1","100:1"), cex.axis=1.2, pos=-0.55) 
mtext("Threshold probability (for treating TB)", side=1, line=2, cex=1.2)
mtext("Cost:Benefit ratio", side=1, line=6.0, cex=1.2)
lines(dca.validation$derived.data[c(1:101),]$thresholds, dca.validation$derived.data[c(1:101),]$sNB_lower, type='l', ylim=c(-0.1,1), col=2, lty=2)
lines(dca.validation$derived.data[c(1:101),]$thresholds, dca.validation$derived.data[c(1:101),]$sNB_upper, type='l', ylim=c(-0.1,1), col=2, lty=2)
lines(dca.validation$derived.data[c(102:202),]$thresholds, dca.validation$derived.data[c(102:202),]$sNB, type='l', ylim=c(-0.1,1), col=4)
lines(dca.validation$derived.data[c(102:202),]$thresholds, dca.validation$derived.data[c(102:202),]$sNB_lower, type='l', ylim=c(-0.1,1), col=4, lty=2)
lines(dca.validation$derived.data[c(102:202),]$thresholds, dca.validation$derived.data[c(102:202),]$sNB_upper, type='l', ylim=c(-0.1,1), col=4, lty=2)
lines(dca.validation$derived.data[c(203:303),]$thresholds, dca.validation$derived.data[c(203:303),]$sNB, type='l', ylim=c(-0.1,1), col=1, lty=1, lwd=3)
legend("right", legend=c("Treatment based on clinical risk score","Treatment for all","Treatment for none"),
       col=c(2,4,1), lty=c(1,1,1), lwd=c(1,1,3),cex=1.1, bty='n')
dev.off()

png(filename = "DCA_re-revision_externalvalidation_10prev_biggercex_063020.png", width=787, height=634)
par(mar=c(10,5,1,2))
plot(100*dca.validation$derived.data[c(1:101),]$thresholds, dca.validation$derived.data[c(1:101),]$sNB, type='l', ylim=c(-0.2,1), col=2, bty='n', xaxt='n', ylab='Standardized Net Benefit', xlab='', cex.lab=1.4, cex.axis=1.2) #, main="Decision curve analysis in the validation population \n(10% TB prevalence scenario)", bty='n', xaxt='n', ylab='Standardized Net Benefit', xlab='', cex.main=0.99
abline(h=0, col=1, lty=2)
xtick <- seq(0.0*100,1.0*100, by=0.20*100)
axis(side=1,at=xtick, cex.axis=1.2) #mtext(c("Risk threshold","Risk:Benefit Ratio"),pos=c(0.4,-0.7))
axis(side=1,at=xtick, labels=c("1:100","1:4","2:3","3:2","4:1","100:1"), cex.axis=1.2, pos=-0.40) 
mtext("%, threshold probability (for treating TB)", side=1, line=2, cex=1.2)
mtext("Risk:Benefit ratio", side=1, line=6.0, cex=1.2)
lines(100*dca.validation$derived.data[c(1:101),]$thresholds, dca.validation$derived.data[c(1:101),]$sNB_lower, type='l', ylim=c(-0.1,1), col=2, lty=2)
lines(100*dca.validation$derived.data[c(1:101),]$thresholds, dca.validation$derived.data[c(1:101),]$sNB_upper, type='l', ylim=c(-0.1,1), col=2, lty=2)
lines(100*dca.validation$derived.data[c(102:202),]$thresholds, dca.validation$derived.data[c(102:202),]$sNB, type='l', ylim=c(-0.1,1), col=4)
lines(100*dca.validation$derived.data[c(102:202),]$thresholds, dca.validation$derived.data[c(102:202),]$sNB_lower, type='l', ylim=c(-0.1,1), col=4, lty=2)
lines(100*dca.validation$derived.data[c(102:202),]$thresholds, dca.validation$derived.data[c(102:202),]$sNB_upper, type='l', ylim=c(-0.1,1), col=4, lty=2)
lines(100*dca.validation$derived.data[c(203:303),]$thresholds, dca.validation$derived.data[c(203:303),]$sNB, type='l', ylim=c(-0.1,1), col=1, lty=1, lwd=3)
legend("right", legend=c("Treatment based on clinical risk score","Treatment for all","No empirical treatment"),
       col=c(2,4,1), lty=c(1,1,1), lwd=c(1,1,3),cex=1.1, bty='n')
dev.off()


# 5% DCA
external.subset.5 %>% count(xpert) # 106*9/281 times expansion of xpert negative population needed
external.subset.5 <- rbind(external.subset, external.subset %>% filter(xpert==0), external.subset %>% filter(xpert==0), external.subset %>% filter(xpert==0), external.subset %>% filter(xpert==0), external.subset %>% filter(xpert==0), external.subset %>% filter(xpert==0)) # 9% TB prevalence

dca.validation.5 <- decision_curve(xpert~score, data=external.subset.5, family=binomial, fitted.risk = FALSE,
                                 thresholds = seq(0, 1, by = 0.01), study.design = 'case-control', confidence.intervals = 0.95, bootstraps = 500)


png(filename = "DCA_re-revision_externalvalidation_5prev_biggercex_063020.png", width=787, height=634)
par(mar=c(10,5,1,2))
plot(100*dca.validation.5$derived.data[c(1:101),]$thresholds, dca.validation.5$derived.data[c(1:101),]$sNB, type='l', ylim=c(-0.2,1), col=2, bty='n', xaxt='n', ylab='Standardized Net Benefit', xlab='', cex.lab=1.4, cex.axis=1.2) #, main="Decision curve analysis in the validation population \n(10% TB prevalence scenario)", bty='n', xaxt='n', ylab='Standardized Net Benefit', xlab='', cex.main=0.99
abline(h=0, col=1, lty=2)
xtick <- seq(0.0*100,1.0*100, by=0.20*100)
axis(side=1,at=xtick, cex.axis=1.2) #mtext(c("Risk threshold","Risk:Benefit Ratio"),pos=c(0.4,-0.7))
axis(side=1,at=xtick, labels=c("1:100","1:4","2:3","3:2","4:1","100:1"), cex.axis=1.2, pos=-0.40) 
mtext("%, threshold probability (for treating TB)", side=1, line=2, cex=1.2)
mtext("Risk:Benefit ratio", side=1, line=6.0, cex=1.2)
lines(100*dca.validation.5$derived.data[c(1:101),]$thresholds, dca.validation.5$derived.data[c(1:101),]$sNB_lower, type='l', ylim=c(-0.1,1), col=2, lty=2)
lines(100*dca.validation.5$derived.data[c(1:101),]$thresholds, dca.validation.5$derived.data[c(1:101),]$sNB_upper, type='l', ylim=c(-0.1,1), col=2, lty=2)
lines(100*dca.validation.5$derived.data[c(102:202),]$thresholds, dca.validation.5$derived.data[c(102:202),]$sNB, type='l', ylim=c(-0.1,1), col=4)
lines(100*dca.validation.5$derived.data[c(102:202),]$thresholds, dca.validation.5$derived.data[c(102:202),]$sNB_lower, type='l', ylim=c(-0.1,1), col=4, lty=2)
lines(100*dca.validation.5$derived.data[c(102:202),]$thresholds, dca.validation.5$derived.data[c(102:202),]$sNB_upper, type='l', ylim=c(-0.1,1), col=4, lty=2)
lines(100*dca.validation.5$derived.data[c(203:303),]$thresholds, dca.validation.5$derived.data[c(203:303),]$sNB, type='l', ylim=c(-0.1,1), col=1, lty=1, lwd=3)
legend("right", legend=c("Treatment based on clinical risk score","Treatment for all","No empirical treatment"),
       col=c(2,4,1), lty=c(1,1,1), lwd=c(1,1,3),cex=1.1, bty='n')
dev.off()


## sens, spec, PPV, NPV plot
roc.sens810 <- with(Imput.own.back, roc(xpert~ score810)$sensitivities)
roc.spec810 <- with(Imput.own.back, roc(xpert~ score810)$specificities)

roc.sens.data810 <- as.data.frame.matrix(matrix(unlist(roc.sens810$analyses),nrow=9, ncol=7))
roc.spec.data810 <- as.data.frame.matrix(matrix(unlist(roc.spec810$analyses), nrow=9, ncol=7))

sensitivity810 <-rowMeans(roc.sens.data810)
specificity810 <-rowMeans(roc.spec.data810)

prevalence = 0.05
ppv05.810 = (sensitivity810*prevalence)/((sensitivity810*prevalence)+(1-specificity810)*(1-prevalence))
npv05.810 = (specificity810*(1-prevalence))/(((1-sensitivity810)*prevalence)+(specificity810*(1-prevalence)))

prevalence = 0.10
ppv10.810 = (sensitivity810*prevalence)/((sensitivity810*prevalence)+(1-specificity810)*(1-prevalence))
npv10.810 = (specificity810*(1-prevalence))/(((1-sensitivity810)*prevalence)+(specificity810*(1-prevalence)))

prevalence = 0.20
ppv20.810 = (sensitivity810*prevalence)/((sensitivity810*prevalence)+(1-specificity810)*(1-prevalence))
npv20.810 = (specificity810*(1-prevalence))/(((1-sensitivity810)*prevalence)+(specificity810*(1-prevalence)))

cutoff <- c(1,2,3,4,5,6,7,8)

curvedt <- as.data.frame(matrix(c(cutoff, sensitivity810[-9], specificity810[-9], ppv20.810[-9], npv20.810[-9], ppv10.810[-9], npv10.810[-9], ppv05.810[-9], npv05.810[-9]), nrow=8, ncol=9))

png(filename = "Model_goldstandard_score810-size.png", width=850, height=850)
plot(100*curvedt$V2~curvedt$V1, col=1, lty=1, type="o", xlab="Risk score cutoff", ylab="Percent", xlim=c(1,8),
     frame=F, pch=18, xaxt='n', cex.lab=1.3, cex.axis=1.0, yaxt='n', ylim=c(0,100))
xtick<-seq(1,8, by=1)
# # par(pty='m')
ytick<-seq(0,100,by=20)
axis(side=1, at=xtick, labels=c('1','2','3','4','5','6','7','8+'))
axis(side=2, at=ytick, labels=c('0%','20%','40%','60%','80%','100%'),las=2)
lines(100*curvedt$V3, col=1, lty=2, type="o", pch=18)
lines(100*curvedt$V4, col=2, lty=1, type="o", pch=18)
lines(100*curvedt$V5, col=4, lty=1, type="o", pch=18)
lines(100*curvedt$V6, col=2, lty=2, lwd=2,  type="o", pch=18)
lines(100*curvedt$V7, col=4, lty=2, lwd=2, type="o", pch=18)
lines(100*curvedt$V8, col=2, lty=3, lwd=2, type="o", pch=18)
lines(100*curvedt$V9, col=4, lty=3, lwd=2, type="o", pch=18)
# legend("left", legend=c("Sensitivity","Specificity","PPV(prev=20%)","NPV(prev=20%)","PPV(prev=10%)","NPV(prev=10%)","PPV(prev=5%)","NPV(prev=5%)"),
#        col=c(1,2,3,4,3,4,3,4), lty=c(1,1,1,1,2,2,3,3), cex=0.6)
dev.off()





### Single imputation (mode) on the existing model
singleimputation <- newfd_xpert_completecase %>% dplyr::select(n_tbsymp, symp_2wks, agecat3, sexcat, hivcat1, dbcat, n_other_sympcat, edu8, pasttb, eversmoke, lungcat, xpert)

singleimputation$n_tbsymp_rep <- ifelse(is.na(singleimputation$n_tbsymp),1,singleimputation$n_tbsymp)
singleimputation$symp_2wks_rep <- ifelse(is.na(singleimputation$symp_2wks),1,singleimputation$symp_2wks)
singleimputation$dbcat_rep <- ifelse(is.na(singleimputation$dbcat),0,singleimputation$dbcat)
singleimputation$n_other_sympcat_rep <- ifelse(is.na(singleimputation$n_other_sympcat),1,singleimputation$n_other_sympcat)
singleimputation$edu8_rep <- ifelse(is.na(singleimputation$edu8),0,singleimputation$edu8)
singleimputation$pasttb_rep <- ifelse(is.na(singleimputation$pasttb),0,singleimputation$pasttb)
singleimputation$eversmoke_rep <- ifelse(is.na(singleimputation$eversmoke),0,singleimputation$eversmoke)
singleimputation$lungcat_rep <- ifelse(is.na(singleimputation$lungcat),0,singleimputation$lungcat)

singleimputation$score <- singleimputation$n_tbsymp_rep + singleimputation$symp_2wks_rep + ifelse(singleimputation$agecat3==2,1,0) + singleimputation$sexcat + 2*singleimputation$hivcat1 + singleimputation$dbcat_rep 
singleimputation$score.c <-ifelse(singleimputation$n_tbsymp_rep==2,0.66786770,0) + ifelse(singleimputation$n_tbsymp_rep==3,1.68024123,0) + ifelse(singleimputation$n_tbsymp_rep==4,2.27979890,0) + 
  ifelse(singleimputation$symp_2wks_rep==1,0.83973696,0) + ifelse(singleimputation$agecat3=="1",0.28886628,0) + ifelse(singleimputation$agecat3=="2",0.81594505,0) + ifelse(singleimputation$agecat3=="3",0.41984499,0) + ifelse(singleimputation$agecat3=="4",0.14742784,0) +
  ifelse(singleimputation$sexcat==1,0.92502417,0) + ifelse(singleimputation$hivcat1==1,1.20714530,0) + ifelse(singleimputation$dbcat_rep==1,0.70729174,0) +
  ifelse(singleimputation$n_other_sympcat_rep==1,0.25531847,0) + ifelse(singleimputation$edu8_rep==1,-0.07751068,0) + ifelse(singleimputation$pasttb_rep==1,0.12307903,0) + 
  ifelse(singleimputation$eversmoke_rep==1,-0.19609719,0) + ifelse(singleimputation$lungcat_rep==1,-0.14438743,0)
external.subset.c.rep <- external.subset

png(filename = "Single imputation ROC on the existing model_cex.png")
par(pty='s')
roc(xpert~ score, data=singleimputation, ci=T, col=2, lty=1, plot=T, cex.lab=1.5, cex.axis=1.4)
roc(xpert~ score.c, data=singleimputation, ci=T, col=1, lty=1, plot=T, add=T)
par(pty='m')
legend("bottomright", legend=c("Simple scoring system, \nc-statistic=0.82(0.79-0.84)", "\nCoefficient scoring system, \nc-statistic=0.83(0.81-0.85)\n\n"),
       col=c(2,1), lty=1:1, cex=1.2, box.lty=0, bty = "n")
dev.off()






### Single imputation (mode) from the model derivation step
singleimputation1 <- singleimputation %>% dplyr::select(n_tbsymp_rep, symp_2wks_rep, agecat3, sexcat, hivcat1, dbcat_rep, n_other_sympcat_rep, pasttb_rep, eversmoke_rep, xpert) #edu8_rep, lungcat_rep
singleimputation1$n_tbsymp_rep <- as.factor(singleimputation1$n_tbsymp_rep)

x_vars <- model.matrix(xpert~. , singleimputation1)[,-1]
y_var <- singleimputation1$xpert
lambda <- cv.glmnet(x_vars, y_var, nfold=10, alpha = 1, family="binomial")$lambda.min
lasso_best <- glmnet(x_vars, y_var, alpha = 1, lambda = lambda, family="binomial") 
coef(lasso_best)
round(coef(lasso_best)/median(c(0.7369956, 0.8140023, 0.9289158,  0.9748884, 1.2200826, 0.7422489)))


# Actual coefficient scoring system
# (Intercept)         -2.77751385   n_tbsymp_rep2        0.73394762   n_tbsymp_rep3        1.75329639
# n_tbsymp_rep4        2.37461251   symp_2wks_rep        0.81263463   agecat31             0.35380844
# agecat32             0.88368297   agecat33             0.48474498   agecat34             0.20626423
# sexcat               0.96635333   hivcat1              1.21848394   dbcat_rep            0.73883474
# n_other_sympcat_rep  0.24823684   edu8_rep            -0.05952317   pasttb_rep           0.14853784
# eversmoke_rep       -0.24226227   lungcat_rep         -0.19100971   


### THIS IS REAL
# (Intercept)         -2.8331373
# n_tbsymp_rep2        0.7369956
# n_tbsymp_rep3        1.7560489
# n_tbsymp_rep4        2.3726688
# symp_2wks_rep        0.8140023
# agecat31             0.3982130
# agecat32             0.9289158
# agecat33             0.5212273
# agecat34             0.2321851
# sexcat               0.9748884
# hivcat1              1.2200826
# dbcat_rep            0.7422489
# n_other_sympcat_rep  0.2488332
# pasttb_rep           0.1449279
# eversmoke_rep       -0.2511918

# Simple scoring system
# (Intercept)         -3
# n_tbsymp_rep2        1
# n_tbsymp_rep3        2
# n_tbsymp_rep4        3
# symp_2wks_rep        1
# agecat31             0
# agecat32             1
# agecat33             1
# agecat34             0
# sexcat               1
# hivcat1              1
# dbcat_rep            1
# n_other_sympcat_rep  0
# pasttb_rep           0
# eversmoke_rep        0

singleimputation1$score <- singleimputation$score
singleimputation1$score.c <-ifelse(singleimputation1$n_tbsymp_rep==2,0.73394762,0) + ifelse(singleimputation1$n_tbsymp_rep==3,1.75329639,0) + ifelse(singleimputation1$n_tbsymp_rep==4,2.37461251,0) + 
  ifelse(singleimputation1$symp_2wks_rep==1,0.81263463,0) + ifelse(singleimputation1$agecat3=="1",0.35380844,0) + ifelse(singleimputation1$agecat3=="2",0.88368297,0) + ifelse(singleimputation1$agecat3=="3",0.48474498,0) + ifelse(singleimputation1$agecat3=="4",0.20626423,0) +
  ifelse(singleimputation1$sexcat==1,0.96635333,0) + ifelse(singleimputation1$hivcat1==1,1.21848394,0) + ifelse(singleimputation1$dbcat_rep==1,0.73883474,0) +
  ifelse(singleimputation1$n_other_sympcat_rep==1,0.24823684,0)  + ifelse(singleimputation1$pasttb_rep==1,0.14853784,0) + 
  ifelse(singleimputation1$eversmoke_rep==1,-0.24226227,0) #+ ifelse(singleimputation1$edu8_rep==1,-0.05952317,0) + ifelse(singleimputation1$lungcat_rep==1,-0.19100971,0) 

# ROC
png(filename = "Single imputation ROC on the newly derived model_cex.png")
par(pty='s')
roc(xpert~ score, data=singleimputation1, ci=T, col=1, lty=1, plot=T, cex.lab=1.5, cex.axis=1.4)
roc(xpert~ score.c, data=singleimputation1, ci=T, col=1, lty=2, plot=T, add=T)
roc(xpert~ score, data=external, ci=T, col=2, lty=1, plot=T, add=T)
roc(xpert~ score.c.rep, data=external, ci=T, col=2, lty=2, plot=T, add=T)
par(pty='m')
legend("bottomright", legend=c("Derivation simple score, \nc-statistic=0.82(0.79-0.84)", "\nDerivation coefficient score, \nc-statistic=0.83(0.80-0.85)", "\nValidation simple score, \nc-statistic=0.75(0.69-0.80)", "\nValidation coefficient score, \nc-statistic=0.76(0.71,0.82)\n"),
       col=c(1,1,2,2), lty=c(1,2,1,2), cex=1.2, box.lty=0, bty = "n")
dev.off()








### Age in the restricted cubic spline 
png(filename = "Age restricted cubic spline.png")
par(pty='m')
rcspline.plot(x=newfd_xpert_completecase$age1, y=newfd_xpert_completecase$xpert, model="logistic",
              xlab="Age in years", plim=c(0,1), smooth=T, statloc=c(8,1.05))
dev.off()


png(filename = "Probability - Age restricted cubic spline_cex.png")
par(mar=c(5,3.1,2,2))
rcspline.plot(x=newfd_xpert_completecase$age1, y=newfd_xpert_completecase$xpert, model="logistic",
              xlab="", ylab=, show="prob", ylim=c(0,0.8), smooth=T, statloc = F, main="B")
mtext("Age (years)", side=1, line=2, cex=1.5)
ytick <- seq(0.0,0.8, by=0.2)
axis(side=1,at=ytick, cex.axis=1.2)
mtext("Probability", side=2, line=2, cex=1.5)
dev.off()

glm(xpert ~ rcs(age1,5), data=newfd_xpert_completecase, family=binomial())

RCS <- glm(newfd_xpert_completecase$xpert~rcs(newfd_xpert_completecase$age1, quantile(newfd_xpert_completecase$age1, c(0, .05, .275, .5, .775, .95, 1),
                              include.lowest = TRUE)))
plot(newfd_xpert_completecase$age1,RCS$fitted.values,
     col = "red",
     xlim = c(min(newfd_xpert_completecase$age1),max(newfd_xpert_completecase$age1)),
     ylim = c(min(newfd_xpert_completecase$xpert),max(newfd_xpert_completecase$xpert)))
points(newfd_xpert_completecase$age1,newfd_xpert_completecase$xpert)



  d <- newfd_xpert_completecase[,c("age1","hivcat1","n_tbsymp_rep","symp_2wks_rep","xpert")]
  d$xpert <- as.numeric(as.character(d$xpert)) # Make the class of the oucome factor to numeric
  x_vars <- model.matrix(xpert~rms::rcs(age1,5)+hivcat1+n_tbsymp_rep+symp_2wks_rep , d)[,-1]
  y_var <- d$xpert
  cv <- cv.glmnet(x_vars, y_var, nfold=10, alpha = 1, family="binomial") # Estimate the shrinkage factor ('nfold=10' means splitting into 10 subsamples, building Lasso based on 9, and testing/estimating from 1)
  lambda <- cv$lambda.min # Get the best shrinkage factor
  lasso_best <- glmnet(x_vars, y_var, alpha = 1, lambda = lambda, family="binomial") # Lasso regression (coefficient must be the mean across the 20 bootstrapping samples)
  coef(lasso_best)

  
  # Choose the columns to use from the (original) data
  agercs <- newfd_xpert_completecase %>% dplyr::select(n_tbsymp, symp_2wks, age1, sexcat, hivcat1, dbcat, n_other_sympcat, edu8, pasttb, eversmoke, lungcat, xpert) #symp_fac___1cat, symp_fac___2cat, symp_fac___3cat, symp_fac___4cat, 
  # Performing MI on the original samples
  agercs[] <- lapply(agercs, function(x){return(as.factor(x))})
  agercs.imput <- mice(agercs,7,pri=F)
  # Extract MI datasets to each dataset
  agercs.imput.com <- vector("list",7)    
  for (i in 1:7){
    agercs.imput.com[[i]]=mice::complete(agercs.imput,i) 
  } 
  
  
  
  ### Get the coefficients and intercept from Lasso regression and age in the RCS form
  coef.agercs=list()
  coeftotal.agercs=matrix(nrow=15)
  for(i in 1:7){ }
    i=7
    d <- agercs.imput.com[[i]][,c(-8,-11)]
    d$xpert <- as.numeric(as.character(d$xpert)) # Make the class of the oucome factor to numeric
    x_vars <- model.matrix(xpert~n_tbsymp+symp_2wks+rms::rcs(as.numeric(as.character(age1)),5)+hivcat1+n_other_sympcat+pasttb+eversmoke , d)[,-1]
    y_var <- d$xpert
    cv <- cv.glmnet(x_vars, y_var, nfold=10, alpha = 1, family="binomial") # Estimate the shrinkage factor ('nfold=10' means splitting into 10 subsamples, building Lasso based on 9, and testing/estimating from 1)
    lambda <- cv$lambda.min # Get the best shrinkage factor
    lasso_best <- glmnet(x_vars, y_var, alpha = 1, lambda = lambda, family="binomial") # Lasso regression (coefficient must be the mean across the 20 bootstrapping samples)
    # coef.agercs7 <- coef(lasso_best)
    # coeftotal.agercs <- cbind(coef.agercs1,coef.agercs2,coef.agercs3,coef.agercs4,coef.agercs5,coef.agercs6,coef.agercs7)
    # rowMeans(coeftotal.agercs)
    roc.agecontinuous7<-roc(y_var, as.numeric(predict(lasso_best, x_vars, type = "response")), ci=T) #0.82 (0.80-0.84)
    
    external.agenum <- external %>% select(xpert,n_tbsymp,symp_2wks,age_years,hivcat1,n_other_sympcat,pasttb,eversmoke)
    external.agenum[] <- lapply(external.agenum, function(x){return(as.factor(x))})
    x_vars_external <- model.matrix(xpert~n_tbsymp+symp_2wks+rms::rcs(as.numeric(as.character(age_years)),5)+hivcat1+n_other_sympcat+pasttb+eversmoke, external.agenum)[,-1]
    y_var_external <- external.agenum$xpert
    roc.agecontinuous.external<-roc(y_var_external, as.numeric(predict(lasso_best, x_vars_external, type = "response")), ci=T) #0.69 (0.64-0.75)
    
#   # (Intercept)                                          n_tbsymp2 
#     -2.75844025                                         0.73301922 
#     n_tbsymp3                                          n_tbsymp4 
#     1.72824150                                         2.30920418 
#     symp_2wks1    rms::rcs(as.numeric(as.character(age1)), 5)age1 
#     0.82693820                                         0.04019074 
#     rms::rcs(as.numeric(as.character(age1)), 5)age1'  rms::rcs(as.numeric(as.character(age1)), 5)age1'' 
#                                        -0.16922104                                         0.04736049 
# rms::rcs(as.numeric(as.character(age1)), 5)age1'''                                           hivcat11 
#     0.92458739                                                                              1.12083588 
#     n_other_sympcat1                                                                        pasttb1 
#     0.25429485                                                                              0.21956442 
#     eversmoke1 
#     0.34491162 
    median(c(0.82693820,0.92458739,0.73301922,1.12083588))
    round(rowMeans(coeftotal.agercs)/median(c(0.82693820,0.92458739,0.73301922,1.12083588)))
# 
#     (Intercept)                                          n_tbsymp2 
#     -3                                                  1 
#     n_tbsymp3                                          n_tbsymp4 
#     2                                                  3 
#     symp_2wks1    rms::rcs(as.numeric(as.character(age1)), 5)age1 
#     1                                                  0 
#     rms::rcs(as.numeric(as.character(age1)), 5)age1'  rms::rcs(as.numeric(as.character(age1)), 5)age1'' 
#                                                  0                                                  0 
# rms::rcs(as.numeric(as.character(age1)), 5)age1'''                                           hivcat11 
#     1                                                  1 
#     n_other_sympcat1                                            pasttb1 
#     0                                                  0 
#     eversmoke1 
#     0 
    # Plot single scoring ROCs
    png(filename = "ROC_age as continuous_to_coefficient_cex.png")
    par(mar=c(2,2,2,2))
    par(pty='s')
    plot(roc.agecontinuous1, col=1, lty=1, cex.lab=1.5, cex.axis=1.3, main="C")
    plot(roc.agecontinuous7, col=1, lty=1, add=T)
    plot(roc.agecontinuous.external, col=2, lty=1, add=T)
    plot(roc.coeff.kharitode[[1]], col=1, lty=2, add=T)
    for(i in 2:7){plot(roc.coeff.kharitode[[i]], col=1, lty=2, add=T)}
    plot(roc.coeff.STOMP, col=2, add=T, lty=2)
    par(pty='m')
    legend("bottomright", legend=c("Derivation (age as RCS form), c-statistic=0.82(0.80-0.84)", "Validation (age as RCS form), c-statistic=0.70(0.64-0.75)",
                                   "Derivation (age as categorical), c-statistic=0.83(0.82-0.83)", "Validation (age as categorical), c-statistic=0.77(0.71-0.82)"),
           col=c(1,2,1,2), lty=c(1,1,2,2), cex=1.0, box.lty=0, bty = "n")
    dev.off()

    

    
    
## Before submission, file format conversion
figure4 <- readPNG("/Users/ys/Documents/Kharitode/submission_BMJ/Figure4.png")
writeJPEG(figure4, target = "/Users/ys/Documents/Kharitode/submission_BMJ/figure4_conv.jpeg", quality = 1)

png(filename="figure1combine.png")
par(mfrow=c(1,2))
par(pty='m')
val.prob.ci.2(pred.adj.mi.mean, as.numeric(as.character(external.subset.c$xpert)), logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black", cex.leg = 1.0, cex=1.0, cex.axis=1.3, cex.d01 = 1.3, cex.lab=1.3, main="A")
par(pty='s')
plot.roc(roc.single.kharitode.810[[1]], col=1, type="o",print.thres=c(1,2,3,4,5,6,7,10), print.thres.pattern = (c('1','2','3','4','5','6','7','8+')), cex.axis=1.2, cex.lab=1.5, main="B\n")
for(i in 2:7){plot(roc.single.kharitode[[i]], col=1, add=T)}
plot(roc.single.STOMP, col=2, add=T, type="o",print.thres=c(1,2,3,4,5,6,7,10), print.thres.pattern = (c('1','2','3','4','5','6','7','8+')), print.thres.col="red")
par(pty='m')
legend("bottomright", legend=c("Derivation data, c-statistic=0.82(0.81-0.82)", "Validation data, c-statistic=0.75(0.69-0.80)"),
       col=c(1,2), lty=1:1, cex=1.0, box.lty=0, bty = "n")
dev.off()



    