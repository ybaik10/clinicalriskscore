newfd_xpert_completecase <- newfd[which(!is.na(newfd$xpert)),] # Delete the imputed outcome

### Choose the columns to use from the (original) data
newfd.original <- newfd_xpert_completecase %>% dplyr::select(n_tbsymp, symp_2wks, agecat1, sexcat, hivcat1, dbcat, n_other_sympcat, edu8, pasttb, eversmoke, lungcat, xpert) #symp_fac___1cat, symp_fac___2cat, symp_fac___3cat, symp_fac___4cat, 

### Performing MI on the original samples
newfd.original[] <- lapply(newfd.original, function(x){return(as.factor(x))})
Imput.own <- mice(newfd.original,15,pri=F)

### Extract MI datasets to each dataset
data.imputed.Boot.own <- vector("list",15)    
for (i in 1:15){
  data.imputed.Boot.own[[i]]=complete(Imput.own,i) 
}   

data.imputed.Boot.own

### Coefficient of lasso from the mutiply imputed datasets
data.imputed.Boot.own[[1]]$xpert <- as.numeric(as.character(data.imputed.Boot.own[[1]]$xpert)) # Make the class of the oucome factor to numeric

x_vars <- model.matrix(xpert~. , data.imputed.Boot.own[[1]])[,-1]
y_var <- data.imputed.Boot.own[[1]]$xpert

cv <- cv.glmnet(x_vars, y_var, nfold=10, alpha = 1, family="binomial") # Estimate the shrinkage factor ('nfold=10' means splitting into 10 subsamples, building Lasso based on 9, and testing/estimating from 1)
lambda <- cv$lambda.min # Get the best shrinkage factor

lasso_best <- glmnet(x_vars, y_var, alpha = 1, lambda = lambda) # Lasso regression (coefficient must be the mean across the 20 bootstrapping samples)

coef.own1 <- coef(lasso_best)


### Lasso regression coefficients - taking the mean from 15 models (from the 15 imputed datasets)
coeff.own.total <- cbind(coef.own1, coef.own2, coef.own3, coef.own4, coef.own5, coef.own6, coef.own7, coef.own8,
                         coef.own9, coef.own10, coef.own11, coef.own12, coef.own13, coef.own14, coef.own15)
rowMeans(coeff.own.total)

### Test two different scoring systems
# 1. Coefficient scoring system absed on the Lasso regression
rowMeans(coeff.own.total)
# 2. Simple scoring system based on the Lasso regression coefficient 
median(0.137390143,0.152527673,0.160757287,0.134388684) #0.1373901
rowMeans(coeff.own.total)/0.1373901

rowMeans(coeff.own.total)/median(rowMeans(coeff.own.total))
round(rowMeans(coeff.own.total)/median(rowMeans(coeff.own.total)))
round(rowMeans(coeff.own.total)/mean(rowMeans(coeff.own.total)))
# 3. Our FINAL scoring system
ifelse(agecat1=="[25,35)",1,0) + 1*sexcat + 1*hivcat1 + 1*n_tbsymp + 1*symp_2wks + 1*dbcat + 
  0*n_other_sympcat + 0*edu8 + 0*pasttb + 0*eversmoke + 0*lungcat

### Prepare the validation data
external.subset <- external[,c("agecat1","sexcat","hivcat1","n_tbsymp","symp_2wks","dbcat","n_other_sympcat","pasttb","edu8","eversmoke","lungcat","xpert")]
x_test <- model.matrix(xpert~.,external.subset)[,-1]

external.subset.simple <- external[,c("score","xpert")]
x_test.simple <- model.matrix(xpert~.,external.subset.simple)

external.subset.re <- external[,c("agecat2","sexcat1","hivcat2","n_tbsymp1","symp_2wks1","dbcat1","xpert")]
x_test.re <- model.matrix(xpert~.,external.subset.re)[,-1]

### Predict in the original multiple imputed Kharitode dataset (NOT bootstrapped samples)
# lambda_min.own<-with(Imput.own, cv.glmnet((model.matrix(xpert~agecat1+sexcat +hivcat1 +n_tbsymp +symp_2wks + dbcat +
#                                                   n_other_sympcat + edu8 + pasttb + eversmoke + lungcat)[,-1]),(xpert),alpha=1,nfold=10,family="binomial")$lambda.min)
# cv_out_lambda.own <-with(Imput.own, cv.glmnet((model.matrix(xpert~agecat1+sexcat +hivcat1 +n_tbsymp +symp_2wks + dbcat + 
#                                                       n_other_sympcat + edu8 + pasttb + eversmoke + lungcat)[,-1]),(xpert),alpha=1,nfold=10,family="binomial")$lambda.1se)
# lambda_min_mean.own <- mean(unlist(lambda_min.own$analyses))
# cv_out_lambda_mean.own <- mean(unlist(cv_out_lambda.own$analyses))
# lasso_best.own <- with(Imput.own, glmnet((model.matrix(xpert~agecat1+sexcat +hivcat1 +n_tbsymp +symp_2wks + dbcat + 
#                                                  n_other_sympcat + edu8 + pasttb + eversmoke + lungcat)[,-1]),(xpert), alpha = 1, lambda = lambda_min_mean.own, family="binomial"))$analyses
# # best<-glmnet((model.matrix(Imputown1$xpert~Imputown1$agecat1+Imputown1$sexcat +Imputown1$hivcat1 +Imputown1$n_tbsymp +Imputown1$symp_2wks + Imputown1$dbcat + 
# #                      Imputown1$n_other_sympcat + Imputown1$edu8 + Imputown1$pasttb + Imputown1$eversmoke + Imputown1$lungcat)[,c(-17,-1)]),(Imputown1$xpert), alpha = 1, lambda = lambda_min_mean.own, family="binomial", data=Imputown1)
# lasso_prob.own <- predict(lasso_best.own, newx=x_test, type="response")
# # lasso_best.own %>% predict(newx=x_test_mx, type="response")
# # predict(best, x_test, type="response")

### Predict in the original multiple imputed dataset with the simple scoring system
# lambda_min.own.simple<-with(Imput.own, cv.glmnet((model.matrix(xpert~ifelse(agecat1=="[25,35)",1,0)+ifelse(sexcat==1,1,0)+ifelse(hivcat1==1,1,0)+as.numeric(as.character(n_tbsymp))+ifelse(symp_2wks==1,1,0)+ifelse(dbcat==1,1,0)+ 0*ifelse(n_other_sympcat==1,1,0) + 0*ifelse(edu8==1,1,0) + 0*ifelse(pasttb==1,1,0) + 0*ifelse(eversmoke==1,1,0) + 0*ifelse(lungcat==1,1,0))[,-1]),(xpert),alpha=1,nfold=10,family="binomial")$lambda.min)
# cv_out_lambda.own.simple <-with(Imput.own, cv.glmnet((model.matrix(xpert~ifelse(agecat1=="[25,35)",1,0)+ifelse(sexcat==1,1,0)+ifelse(hivcat1==1,1,0)+as.numeric(as.character(n_tbsymp))+ifelse(symp_2wks==1,1,0)+ifelse(dbcat==1,1,0)+ 0*ifelse(n_other_sympcat==1,1,0) + 0*ifelse(edu8==1,1,0) + 0*ifelse(pasttb==1,1,0) + 0*ifelse(eversmoke==1,1,0) + 0*ifelse(lungcat==1,1,0))[,-1]),(xpert),alpha=1,nfold=10,family="binomial")$lambda.1se)
# lambda_min_mean.own.simple <- mean(unlist(lambda_min.own.simple$analyses))
# cv_out_lambda_mean.ownsimple <- mean(unlist(cv_out_lambda.own.simple$analyses))
# lasso_best.own.simple <- with(Imput.own, glmnet((model.matrix(xpert~ifelse(agecat1=="[25,35)",1,0)+ifelse(sexcat==1,1,0)+ifelse(hivcat1==1,1,0)+as.numeric(as.character(n_tbsymp))+ifelse(symp_2wks==1,1,0)+ifelse(dbcat==1,1,0)+ 0*ifelse(n_other_sympcat==1,1,0) + 0*ifelse(edu8==1,1,0) + 0*ifelse(pasttb==1,1,0) + 0*ifelse(eversmoke==1,1,0) + 0*ifelse(lungcat==1,1,0))[,-1]),(xpert), alpha = 1, lambda = lambda_min_mean.own.simple, family="binomial"))$analyses
# lasso_prob.own.simple <- predict(lasso_best.own.simple, newx=x_test.simple, type="response")

Imputown3 <- complete(Imput.own, 3)
Imputown3$agecat2 <- ifelse(Imputown3$agecat1=="[25,35)",1,0) 
Imputown3$sexcat1 <- ifelse(Imputown3$sexcat==1,1,0)
Imputown3$hivcat2 <- ifelse(Imputown3$hivcat1==1,1,0)
Imputown3$n_tbsymp1 <- as.numeric(as.character(Imputown3$n_tbsymp))
Imputown3$symp_2wks1 <- ifelse(Imputown3$symp_2wks==1,1,0)
Imputown3$dbcat1 <- ifelse(Imputown3$dbcat==1,1,0)

lambda<-cv.glmnet((model.matrix(Imputown3$xpert~Imputown3$agecat2+Imputown3$sexcat1+Imputown3$hivcat2+Imputown3$n_tbsymp1+
                                  Imputown3$symp_2wks1+Imputown3$dbcat1)[,-1]),(Imputown3$xpert),nfold=10,alpha=1,family="binomial")
lasso_best_regression <- glmnet((model.matrix(Imputown3$xpert~Imputown3$agecat2+Imputown3$sexcat1+Imputown3$hivcat2+Imputown3$n_tbsymp1+
                                                Imputown3$symp_2wks1+Imputown3$dbcat1)[,-1]),(Imputown3$xpert),nfold=10,alpha=1,lambda = lambda$lambda.min,family="binomial")
lasso_best_predict <- predict(lasso_best_regression, newx=x_test.re, s=lambda$lambda.min, type="response")
val.prob.ci.2(lasso_best_predict, external.subset.re$xpert, logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black")
val.prob.ci.2(lasso_best_predict*(106/387/(765/1614)), external.subset.re$xpert, logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black")


mean_lasso_best_regression<-rowMeans(cbind(lasso_best_predict1, lasso_best_predict2, best3, best4, best5, best6, best7, best8, best9, best10,
                                           best11, best12, best13, best14, best15))
val.prob.ci.2(mean_lasso_best_regression, external.subset.re$xpert, logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black")
val.prob.ci.2(mean_lasso_best_regression*(106/387/(765/1614)), external.subset.re$xpert, logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black")
adjusted_mean_lasso_best_regression <- mean_lasso_best_regression*(106/387/(765/1614))
glm(xpert ~ offset(mean_lasso_best_regression), family=binomial, data=external)
glm(xpert ~ mean_lasso_best_regression, family=binomial, data=external)



score <- with(Imput.own, score<-ifelse(agecat1=="[25,35)",1,0) + ifelse(sexcat==1,1,0) + ifelse(hivcat1==1,1,0) + as.numeric(as.character(n_tbsymp)) + ifelse(symp_2wks==1,1,0) + ifelse(dbcat==1,1,0) +
       0*ifelse(n_other_sympcat==1,1,0) + 0*ifelse(edu8==1,1,0) + 0*ifelse(pasttb==1,1,0) + 0*ifelse(eversmoke==1,1,0) + 0*ifelse(lungcat==1,1,0))$analyses

Imputown1 <- complete(Imput.own, 1)
Imputown1$score <- score[[1]]
Imputown1.1 <- Imputown1[,c("score","xpert")]

lambda<-cv.glmnet((model.matrix(Imputown1.1$xpert~Imputown1.1$score)),(Imputown1.1$xpert),nfold=10,alpha=1,family="binomial")
lasso_best_regression <- glmnet(model.matrix(Imputown1.1$xpert~Imputown1.1$score),(Imputown1.1$xpert), alpha = 1, nfold=10,lambda = lambda$lambda.min, family="binomial")
lasso_best_predict <- predict(lasso_best_regression, newx=x_test.simple, s=lambda$lambda.min, type="response")
val.prob.ci.2(lasso_best_predict, external.subset.simple$xpert, logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black")
val.prob.ci.2(lasso_best_predict*(106/387/(765/1614)), external.subset.simple$xpert, logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black")



score1 <- with(Imput.own, score<-ifelse(agecat1=="[25,35)",1,0) + ifelse(sexcat==1,2,0) + ifelse(hivcat1==1,2,0) + as.numeric(as.character(n_tbsymp)) + ifelse(symp_2wks==1,2,0) + ifelse(dbcat==1,1,0) +
                 ifelse(n_other_sympcat==1,1,0) + 0*ifelse(edu8==1,1,0) + 0*ifelse(pasttb==1,1,0) + 0*ifelse(eversmoke==1,1,0) + 0*ifelse(lungcat==1,1,0))$analyses

Imputown2 <- complete(Imput.own, 2)
Imputown2$score <- score1[[2]]
Imputown2.1 <- Imputown2[,c("score","xpert")]

lambda<-cv.glmnet((model.matrix(Imputown2.1$xpert~Imputown2.1$score)),(Imputown2.1$xpert),alpha=1,nfold=10,family="binomial")
lasso_best_regression <- glmnet(model.matrix(Imputown2.1$xpert~Imputown2.1$score),(Imputown2.1$xpert), alpha = 1, nfold=10,lambda = lambda$lambda.min, family="binomial")
lasso_best_predict <- predict(lasso_best_regression, newx=x_test.simple, s=lambda$lambda.min, type="response")
val.prob.ci.2(lasso_best_predict, external.subset.simple$xpert, logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black")
val.prob.ci.2(lasso_best_predict*((106/387)/(765/1614)), external.subset.simple$xpert, logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black")

### Calibration


### Calibration when both settings have the equal sampling fraction 


### Fit the COEFFICIENT scoring model and predict with the external validation data and CALIBRATION
lasso_prob.own
lasso_prob.own.unlist <- unlist(lasso_prob.own)
lasso_prob.own.matrix <- matrix(lasso_prob.own.unlist, nrow=387, ncol=15)
lasso_prob.own.score <- rowMeans(lasso_prob.own.matrix)
val.prob.ci.2(lasso_prob.own.score, external$xpert, logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black")
val.prob.ci.2(lasso_prob.own.score*(106/387/(765/1614)), external$xpert, logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black")

adjusted.lasso.score.own <- lasso_prob.own.score*(106/387/(765/1614))

glm(xpert ~ offset(adjusted.lasso.score.own), family=binomial, data=external)
glm(xpert ~ adjusted.lasso.score.own, family=binomial, data=external)








### ARCHIVE FROM ORDINARY_CALIBRATION.R
# pooled coefficients?
for(i in 1:15){
  i<-13; d<-data.imputed.Boot.own[[13]]; d$score <- score[[13]]; 
  fit13 <- glm(xpert ~ as.factor(score), data=d, family=binomial())$coefficients
}
fitcoef <-cbind(fit1,fit2,fit3,fit4,fit5,fit6,fit7,fit8,fit9,fit10,fit11,fit12,fit13,fit14,fit15)
fitcoef.mean<-rowMeans(matrix(unlist(fitcoef),nrow=10, ncol=15))

### Trial - different adjustment methods
## First
d<-data.imputed.Boot.own[[15]]
d$score <- score[[15]]
fit <- glm(xpert ~ as.factor(score), data=d, family=binomial()) #as factor
fit <- glm(xpert ~ score, data=d, family=binomial()) #as linear
# fit$coefficients[['(Intercept)']] <- fit$coefficients[['(Intercept)']]*(0.2764858/(1-0.2764858))/(0.4739777/(1-0.4739777))
fit$coefficients[['as.factor(score)1']] <- fit$coefficients[['as.factor(score)1']]*(0.2764858/(1-0.2764858))/(0.4739777/(1-0.4739777))
fit$coefficients[['as.factor(score)2']] <- fit$coefficients[['as.factor(score)2']]*(0.2764858/(1-0.2764858))/(0.4739777/(1-0.4739777))
fit$coefficients[['as.factor(score)3']] <- fit$coefficients[['as.factor(score)3']]*(0.2764858/(1-0.2764858))/(0.4739777/(1-0.4739777))
fit$coefficients[['as.factor(score)4']] <- fit$coefficients[['as.factor(score)4']]*(0.2764858/(1-0.2764858))/(0.4739777/(1-0.4739777))
fit$coefficients[['as.factor(score)5']] <- fit$coefficients[['as.factor(score)5']]*(0.2764858/(1-0.2764858))/(0.4739777/(1-0.4739777))
fit$coefficients[['as.factor(score)6']] <- fit$coefficients[['as.factor(score)6']]*(0.2764858/(1-0.2764858))/(0.4739777/(1-0.4739777))
fit$coefficients[['as.factor(score)7']] <- fit$coefficients[['as.factor(score)7']]*(0.2764858/(1-0.2764858))/(0.4739777/(1-0.4739777))
fit$coefficients[['as.factor(score)8']] <- fit$coefficients[['as.factor(score)8']]*(0.2764858/(1-0.2764858))/(0.4739777/(1-0.4739777))
fit$coefficients[['as.factor(score)9']] <- fit$coefficients[['as.factor(score)9']]*(0.2764858/(1-0.2764858))/(0.4739777/(1-0.4739777))

pred15 <- fit %>% predict(external.subset, type="response")
val.prob.ci.2(pred15, as.numeric(as.character(external.subset$xpert)), logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black")

## Second
d<-data.imputed.Boot.own[[15]]
d$score <- score[[15]]
fit <- glm(xpert ~ as.factor(score), data=d, family=binomial()) 
fit$coefficients[['(Intercept)']] <- fit$coefficients[['(Intercept)']] -0.43
fit$coefficients[['as.factor(score)1']] <- fit$coefficients[['as.factor(score)1']]*0.64
fit$coefficients[['as.factor(score)2']] <- fit$coefficients[['as.factor(score)2']]*0.64
fit$coefficients[['as.factor(score)3']] <- fit$coefficients[['as.factor(score)3']]*0.64
fit$coefficients[['as.factor(score)4']] <- fit$coefficients[['as.factor(score)4']]*0.64
fit$coefficients[['as.factor(score)5']] <- fit$coefficients[['as.factor(score)5']]*0.64
fit$coefficients[['as.factor(score)6']] <- fit$coefficients[['as.factor(score)6']]*0.64
fit$coefficients[['as.factor(score)7']] <- fit$coefficients[['as.factor(score)7']]*0.64
fit$coefficients[['as.factor(score)8']] <- fit$coefficients[['as.factor(score)8']]*0.64
fit$coefficients[['as.factor(score)9']] <- fit$coefficients[['as.factor(score)9']]*0.64

pred15 <- fit %>% predict(external.subset, type="response")
val.prob.ci.2(pred15, as.numeric(as.character(external.subset$xpert)), logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black")


## Third
d<-data.imputed.Boot.own[[15]]
d$score <- score[[15]]
fit <- glm(xpert ~ as.factor(score), data=d, family=binomial()) 
fit$coefficients[['(Intercept)']] <- fit$coefficients[['(Intercept)']] - (0.2764858/(1-0.2764858))/(0.4739777/(1-0.4739777))
fit$coefficients[['as.factor(score)1']] <- fit$coefficients[['as.factor(score)1']]*(0.2764858/(1-0.2764858))/(0.4739777/(1-0.4739777))
fit$coefficients[['as.factor(score)2']] <- fit$coefficients[['as.factor(score)2']]*(0.2764858/(1-0.2764858))/(0.4739777/(1-0.4739777))
fit$coefficients[['as.factor(score)3']] <- fit$coefficients[['as.factor(score)3']]*(0.2764858/(1-0.2764858))/(0.4739777/(1-0.4739777))
fit$coefficients[['as.factor(score)4']] <- fit$coefficients[['as.factor(score)4']]*(0.2764858/(1-0.2764858))/(0.4739777/(1-0.4739777))
fit$coefficients[['as.factor(score)5']] <- fit$coefficients[['as.factor(score)5']]*(0.2764858/(1-0.2764858))/(0.4739777/(1-0.4739777))
fit$coefficients[['as.factor(score)6']] <- fit$coefficients[['as.factor(score)6']]*(0.2764858/(1-0.2764858))/(0.4739777/(1-0.4739777))
fit$coefficients[['as.factor(score)7']] <- fit$coefficients[['as.factor(score)7']]*(0.2764858/(1-0.2764858))/(0.4739777/(1-0.4739777))
fit$coefficients[['as.factor(score)8']] <- fit$coefficients[['as.factor(score)8']]*(0.2764858/(1-0.2764858))/(0.4739777/(1-0.4739777))
fit$coefficients[['as.factor(score)9']] <- fit$coefficients[['as.factor(score)9']]*(0.2764858/(1-0.2764858))/(0.4739777/(1-0.4739777))


pred15 <- fit %>% predict(external.subset, type="response")
val.prob.ci.2(pred15, as.numeric(as.character(external.subset$xpert)), logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black")








# ### Simple scoring. With adjustment
# mi.fit <- with(Imput.own, glm(xpert ~ agecat3 + sexcat + hivcat1 + dbcat + n_tbsymp + symp_2wks + n_other_sympcat + edu8 + pasttb + eversmoke + lungcat, family=binomial()))$analyses
# for(i in 1:15){
#   # mi.fit[[i]]$coefficients[['(Intercept)']] <- - #round((-2.961713874 + log((0.2764858/(1-0.2764858))/(0.4739777/(1-0.4739777))))/0.789008315)
#   mi.fit[[i]]$coefficients[['(Intercept)']] <- round(-2.961713874/0.789008315) + log((0.2764858/(1-0.2764858))/(0.4739777/(1-0.4739777)))
#   mi.fit[[i]]$coefficients[['agecat31']] <- 0
#   mi.fit[[i]]$coefficients[['agecat32']] <- 1
#   mi.fit[[i]]$coefficients[['agecat33']] <- 0
#   mi.fit[[i]]$coefficients[['agecat34']] <- 0
#   mi.fit[[i]]$coefficients[['sexcat1']] <- 1
#   mi.fit[[i]]$coefficients[['hivcat11']] <- 2
#   mi.fit[[i]]$coefficients[['dbcat1']] <- 1
#   mi.fit[[i]]$coefficients[['n_tbsymp1']] <- 1
#   mi.fit[[i]]$coefficients[['n_tbsymp2']] <- 2
#   mi.fit[[i]]$coefficients[['n_tbsymp3']] <- 3
#   mi.fit[[i]]$coefficients[['n_tbsymp4']] <- 4
#   mi.fit[[i]]$coefficients[['symp_2wks1']] <- 1
#   mi.fit[[i]]$coefficients[['n_other_sympcat1']] <- 0
#   mi.fit[[i]]$coefficients[['edu81']] <- 0
#   mi.fit[[i]]$coefficients[['pasttb1']] <- 0
#   mi.fit[[i]]$coefficients[['eversmoke1']] <- 0
#   mi.fit[[i]]$coefficients[['lungcat1']] <- 0
# }
# 
# external.subset.1 <- external[,c("agecat3","sexcat","hivcat1","n_tbsymp","symp_2wks","dbcat","n_other_sympcat","pasttb","edu8","eversmoke","lungcat","xpert")]
# 
# prob.adj <- mi.fit %>% predict(external.subset.1, type="response")
# prob.adjmean <- rowMeans(matrix(unlist(prob.adj),nrow=387, ncol=15))
# 
# val.prob.ci.2(prob.adjmean, as.numeric(as.character(external.subset$xpert)), logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
#               ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
#               lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black")

