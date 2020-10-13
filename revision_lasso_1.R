newfd_xpert_completecase <- newfd[which(!is.na(newfd$xpert)),] # Delete the imputed outcome

### Choose the columns to use from the (original) data
newfd.original <- newfd_xpert_completecase %>% dplyr::select(n_tbsymp, symp_2wks, agecat1, sexcat, hivcat1, dbcat, n_other_sympcat, edu8, pasttb, eversmoke, lungcat, xpert) #symp_fac___1cat, symp_fac___2cat, symp_fac___3cat, symp_fac___4cat, 

### Obtaining 20 bootstrap samples from the incomplete (original) data
resamples <- lapply(1:20, function(i) sample(nrow(newfd.original), replace=T))

### Peformimg MI on the 20 bootstrap samples
newfd.original[] <- lapply(newfd.original, function(x){return(as.factor(x))})
Imput <- mice(newfd.original[unlist(resamples),], 15, pri = FALSE) 

### Performing MI on the original samples
Imput.own <- mice(newfd.original,15,pri=F)

### Extract MI datasets to each dataset
data.imputed.Boot <- vector("list",15)    
for (i in 1:15){
  data.imputed.Boot[[i]]=complete(Imput,i) 
}   

data.imputed.Boot


data.imputed.Boot.own <- vector("list",15)    
for (i in 1:15){
  data.imputed.Boot.own[[i]]=complete(Imput.own,i) 
}   

data.imputed.Boot.own

### Take out each of the mutiply imputed datasets
coeff <- data.frame(
  Intercept         = numeric(0),
  n_tbsymp1         = numeric(0),
  n_tbsymp2         = numeric(0),
  n_tbsymp3         = numeric(0),
  n_tbsymp4         = numeric(0),
  symp_2wks1        = numeric(0),
  agecat11          = numeric(0),
  agecat12          = numeric(0),
  agecat13          = numeric(0),
  agecat14          = numeric(0),
  sexcat1           = numeric(0),
  hivcat11          = numeric(0),
  dbcat1            = numeric(0),
  n_other_sympcat1  = numeric(0),
  edu81             = numeric(0),
  pasttb1           = numeric(0),
  eversmoke1        = numeric(0),
  lungcat1          = numeric(0))

for (i in 1:15){
data.imputed.Boot[[i]]$xpert <- as.numeric(as.character(data.imputed.Boot[[i]]$xpert)) # Make the class of the oucome factor to numeric

x_vars <- model.matrix(xpert~. , data.imputed.Boot[[i]])[,-1]
y_var <- data.imputed.Boot[[i]]$xpert

cv_output <- cv.glmnet(x_vars, y_var, nfold=10, alpha = 1, family="binomial") # Estimate the shrinkage factor ('nfold=10' means splitting into 10 subsamples, building Lasso based on 9, and testing/estimating from 1)

best_lam <- cv_output$lambda.min # Get the best shrinkage factor

lasso_best <- glmnet(x_vars, y_var, alpha = 1, lambda = best_lam) # Lasso regression (coefficient must be the mean across the 20 bootstrapping samples)
coef[i] <- coef(lasso_best)
}

coef

### Lasso regression coefficients - taking the mean from 15 models (from the 15 imputed datasets)
coefftotal <- cbind(coeffmatrix1,coeffmatrix2,coeffmatrix3,coeffmatrix4,coeffmatrix5,coeffmatrix6,coeffmatrix7,
                    coeffmatrix8,coeffmatrix9,coeffmatrix10,coeffmatrix11,coeffmatrix12,coeffmatrix13,coeffmatrix14,coeffmatrix15)
coefftotal # A couple age categories and lungcat shrinked to 0 one or a few times
rowMeans(coefftotal) # None of the variables' mean coefficients have not shrinked to 0
                     # None of the variables will be left out


coeff.own.total <- cbind(coef.own1, coef.own2, coef.own3, coef.own4, coef.own5, coef.own6, coef.own7, coef.own8,
                         coef.own9, coef.own10, coef.own11, coef.own12, coef.own13, coef.own14, coef.own15)
rowMeans(coeff.own.total)

### Test two different scoring systems
# 1. Coefficient scoring system absed on the Lasso regression
rowMeans(coefftotal)
# 2. Simple scoring system based on the Lasso regression coefficient (OUR PRIMARY SCORING SYSTEM)
rowMeans(coefftotal)/median(rowMeans(coefftotal))
round(rowMeans(coefftotal)/mean(rowMeans(coefftotal)))
round(rowMeans(coefftotal)/median(rowMeans(coefftotal)))


rowMeans(coeff.own.total)/median(rowMeans(coeff.own.total))
round(rowMeans(coeff.own.total)/median(rowMeans(coeff.own.total)))
round(rowMeans(coeff.own.total)/mean(rowMeans(coeff.own.total)))

# 3. Our FINAL scoring system
ifelse(agecat1=="[25,35)",1,0) + 1*sexcat + 1*hivcat1 + 1*n_tbsymp + 1*symp_2wks + 1*dbcat + 
  0*n_other_sympcat + 0*edu8 + 0*pasttb + 0*eversmoke + 0*lungcat

### Fit the FINAL scoring model and predict with the external validation data 
# fit <- with(Imput, glm(xpert~as.numeric(as.character(n_tbsymp))+symp_2wks+ifelse(agecat1=="[25,35)",1,0)+sexcat+hivcat1+dbcat, family=binomial()))$analyses
# pred <- fit %>% predict(external, type="response")
# Prepare the validation data
external.subset <- external[,c("agecat1","sexcat","hivcat1","n_tbsymp","symp_2wks","dbcat","n_other_sympcat","pasttb","edu8","eversmoke","lungcat","xpert")]
x_test <- model.matrix(xpert~.,external.subset)[,-1]
# x_test_1 <- cbind(x_test, rep(1,387))
# x_test_df <- as.data.frame(x_test, row.names = F)
# x_test_df$n_tbsymp1 <- 0
# x_test_mx <- as.matrix(x_test_df)

external.subset.simple <- external[,c("score","xpert")]
x_test.simple <- model.matrix(xpert~.,external.subset.simple)

# Predict
lambda_min<-with(Imput, cv.glmnet((model.matrix(xpert~agecat1+sexcat +hivcat1 +n_tbsymp +symp_2wks + dbcat + 
                                                n_other_sympcat + edu8 + pasttb + eversmoke + lungcat)[,-1]),(xpert),alpha=1,nfold=10,family="binomial")$lambda.min)
cv_out_lambda <-with(Imput, cv.glmnet((model.matrix(xpert~agecat1+sexcat +hivcat1 +n_tbsymp +symp_2wks + dbcat + 
                                                      n_other_sympcat + edu8 + pasttb + eversmoke + lungcat)[,-1]),(xpert),alpha=1,nfold=10,family="binomial")$lambda.1se)
lambda_min_mean <- mean(unlist(lambda_min$analyses))
cv_out_lambda_mean <- mean(unlist(cv_out_lambda$analyses))
lasso_best <- with(Imput, glmnet((model.matrix(xpert~agecat1+sexcat +hivcat1 +n_tbsymp +symp_2wks + dbcat + 
                                                 n_other_sympcat + edu8 + pasttb + eversmoke + lungcat)[,-1]),(xpert), alpha = 1, lambda = lambda_min_mean, family="binomial"))$analyses
lasso_prob <- predict(lasso_best, newx=x_test, type="response")

lasso_prob.unlist <- unlist(lasso_prob)
lasso_prob.matrix <- matrix(lasso_prob.unlist, nrow=387, ncol=15)
lasso_prob.score <- rowMeans(lasso_prob.matrix)



val.prob.ci.2(lasso_prob.score*(16871*106/(15409*281)), external$xpert, logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black")

adjusted.lasso.score <- lasso_prob.score*((15409*281)/(16871*106))

glm(xpert ~ offset(lasso_prob.score), family=binomial, data=external)
glm(xpert ~ adjusted.lasso.score, family=binomial, data=external)


# Predict in the original multiple imputed Kharitode dataset (NOT bootstrapped samples)
lambda_min.own<-with(Imput.own, cv.glmnet((model.matrix(xpert~agecat1+sexcat +hivcat1 +n_tbsymp +symp_2wks + dbcat + 
                                                  n_other_sympcat + edu8 + pasttb + eversmoke + lungcat)[,-1]),(xpert),alpha=1,nfold=10,family="binomial")$lambda.min)
cv_out_lambda.own <-with(Imput.own, cv.glmnet((model.matrix(xpert~agecat1+sexcat +hivcat1 +n_tbsymp +symp_2wks + dbcat + 
                                                      n_other_sympcat + edu8 + pasttb + eversmoke + lungcat)[,-1]),(xpert),alpha=1,nfold=10,family="binomial")$lambda.1se)
lambda_min_mean.own <- mean(unlist(lambda_min.own$analyses))
cv_out_lambda_mean.own <- mean(unlist(cv_out_lambda.own$analyses))
lasso_best.own <- with(Imput.own, glmnet((model.matrix(xpert~agecat1+sexcat +hivcat1 +n_tbsymp +symp_2wks + dbcat + 
                                                 n_other_sympcat + edu8 + pasttb + eversmoke + lungcat)[,-1]),(xpert), alpha = 1, lambda = lambda_min_mean.own, family="binomial"))$analyses
# best<-glmnet((model.matrix(Imputown1$xpert~Imputown1$agecat1+Imputown1$sexcat +Imputown1$hivcat1 +Imputown1$n_tbsymp +Imputown1$symp_2wks + Imputown1$dbcat + 
#                      Imputown1$n_other_sympcat + Imputown1$edu8 + Imputown1$pasttb + Imputown1$eversmoke + Imputown1$lungcat)[,c(-17,-1)]),(Imputown1$xpert), alpha = 1, lambda = lambda_min_mean.own, family="binomial", data=Imputown1)
lasso_prob.own <- predict(lasso_best.own, newx=x_test, type="response")
# lasso_best.own %>% predict(newx=x_test_mx, type="response")
# predict(best, x_test, type="response")

# Predict in the original multiple imputed dataset with the simple scoring system
lambda_min.own.simple<-with(Imput.own, cv.glmnet((model.matrix(xpert~ifelse(agecat1=="[25,35)",1,0)+ifelse(sexcat==1,1,0)+ifelse(hivcat1==1,1,0)+as.numeric(as.character(n_tbsymp))+ifelse(symp_2wks==1,1,0)+ifelse(dbcat==1,1,0)+ 0*ifelse(n_other_sympcat==1,1,0) + 0*ifelse(edu8==1,1,0) + 0*ifelse(pasttb==1,1,0) + 0*ifelse(eversmoke==1,1,0) + 0*ifelse(lungcat==1,1,0))[,-1]),(xpert),alpha=1,nfold=10,family="binomial")$lambda.min)
cv_out_lambda.own.simple <-with(Imput.own, cv.glmnet((model.matrix(xpert~ifelse(agecat1=="[25,35)",1,0)+ifelse(sexcat==1,1,0)+ifelse(hivcat1==1,1,0)+as.numeric(as.character(n_tbsymp))+ifelse(symp_2wks==1,1,0)+ifelse(dbcat==1,1,0)+ 0*ifelse(n_other_sympcat==1,1,0) + 0*ifelse(edu8==1,1,0) + 0*ifelse(pasttb==1,1,0) + 0*ifelse(eversmoke==1,1,0) + 0*ifelse(lungcat==1,1,0))[,-1]),(xpert),alpha=1,nfold=10,family="binomial")$lambda.1se)
lambda_min_mean.own.simple <- mean(unlist(lambda_min.own.simple$analyses))
cv_out_lambda_mean.ownsimple <- mean(unlist(cv_out_lambda.own.simple$analyses))
lasso_best.own.simple <- with(Imput.own, glmnet((model.matrix(xpert~ifelse(agecat1=="[25,35)",1,0)+ifelse(sexcat==1,1,0)+ifelse(hivcat1==1,1,0)+as.numeric(as.character(n_tbsymp))+ifelse(symp_2wks==1,1,0)+ifelse(dbcat==1,1,0)+ 0*ifelse(n_other_sympcat==1,1,0) + 0*ifelse(edu8==1,1,0) + 0*ifelse(pasttb==1,1,0) + 0*ifelse(eversmoke==1,1,0) + 0*ifelse(lungcat==1,1,0))[,-1]),(xpert), alpha = 1, lambda = lambda_min_mean.own.simple, family="binomial"))$analyses
lasso_prob.own.simple <- predict(lasso_best.own.simple, newx=x_test.simple, type="response")


score <- with(Imput.own, score<-ifelse(agecat1=="[25,35)",1,0) + ifelse(sexcat==1,1,0) + ifelse(hivcat1==1,1,0) + as.numeric(as.character(n_tbsymp)) + ifelse(symp_2wks==1,1,0) + ifelse(dbcat==1,1,0) +
       0*ifelse(n_other_sympcat==1,1,0) + 0*ifelse(edu8==1,1,0) + 0*ifelse(pasttb==1,1,0) + 0*ifelse(eversmoke==1,1,0) + 0*ifelse(lungcat==1,1,0))$analyses
score.matrix <- matrix(unlist(score), nrow=1614, ncol=15)
scoremean <- rowMeans(score.matrix)
matrix(unlist(Imput.own$data), nrow=1614, ncol=12)
lambda_min.own.simple1 <- with(Imput.own1, cv.glmnet((model.)))


Imputown1 <- complete(Imput.own, 1)
Imputown1$score <- score[[1]]
Imputown1.1 <- Imputown1[,c("score","xpert")]

cv.glmnet(model.matrix(Imputown1.1$xpert~Imputown1.1$score)[,-1],(Imputown1.1$xpert),alpha=1,family="binomial")
cv_out_lambda.own.simple <-with(Imput.own, cv.glmnet((model.matrix(xpert~ifelse(agecat1=="[25,35)",1,0)+ifelse(sexcat==1,1,0)+ifelse(hivcat1==1,1,0)+as.numeric(as.character(n_tbsymp))+ifelse(symp_2wks==1,1,0)+ifelse(dbcat==1,1,0)+ 0*ifelse(n_other_sympcat==1,1,0) + 0*ifelse(edu8==1,1,0) + 0*ifelse(pasttb==1,1,0) + 0*ifelse(eversmoke==1,1,0) + 0*ifelse(lungcat==1,1,0))[,-1]),(xpert),alpha=1,nfold=10,family="binomial")$lambda.1se)
lambda_min_mean.own.simple <- mean(unlist(lambda_min.own.simple$analyses))
cv_out_lambda_mean.ownsimple <- mean(unlist(cv_out_lambda.own.simple$analyses))
lasso_best_regression <- glmnet(model.matrix(Imputown1.1$xpert~Imputown1.1$score),(Imputown1.1$xpert), alpha = 1, lambda = lambda_min_mean.own.simple, family="binomial")
lasso_best_predict <- predict(lasso_best_regression, newx=x_test.simple, type="response")
cbind(lasso_best_predict, external$xpert)
val.prob.ci.2(lasso_best_predict, external$xpert, logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black")
val.prob.ci.2(lasso_best_predict*(106/387/(765/1614)), external$xpert, logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black")
# fit1 <- with(Imput, glm(xpert~scoremean, family=binomial()))$analyses
# pred1 <- fit1 %>% predict(external, type="response")

### Calibration
pred1.unlist <- unlist(pred1)
pred1.matrix <- matrix(pred1.unlist, nrow=387, ncol=15)
pred1.score <- rowMeans(pred1.matrix)
val.prob.ci.2(pred1.score, external$xpert, logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black")

### Calibration when both settings have the equal sampling fraction 
val.prob.ci.2(pred1.score*(32280*106/(15409*387)), external$xpert, logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black")

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

# ### Own model evaluation
# pred <- predict(lasso_best, s = best_lam, newx = x_vars)
# cbind(y_var, pred)
#   
#   ### By ROC
#   roc(y_var, pred, ci=T, plot=T) # 0.8219 (0.8015 - 0.8422)
#   
#   ### By R-square
#   actual <- y_var
#   preds <- pred
#   rss <- sum((preds - actual) ^ 2)
#   tss <- sum((actual - mean(actual)) ^ 2)
#   rsq <- 1 - rss/tss
#   rsq # 0.30
# 
#   ### By Calibration
#   pred1 <- ifelse(pred>=1,0.999999,pred)
#   pred1 <- ifelse(pred1<0, 0, pred1)
#   val.prob.ci.2(pred1[,1], data1_1$xpert, logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
#                 ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
#                 lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black") 
#   
#   ### By Recalibration
#  fit <- with(Imput, glm(xpert~n_tbsymp+symp_2wks+agecat8+sexcat+hivcat1+dbcat+n_other_sympcat+edu8+pasttb+eversmoke+lungcat, family=binomial()))$analyses
#  summary(pool(fit))
#  pred.re <- fit %>% predict(data1_1, type="response")
#  val.prob.ci.2(pred.re[[2]], data1_1$xpert, logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
#               ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
#               lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black") 

