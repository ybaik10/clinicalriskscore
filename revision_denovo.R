### Data setup
external.denovo <- external %>% dplyr::select(n_tbsymp, symp_2wks, agecat4, sexcat, hivarv,  
                                              n_other_sympcat, edu8, pasttb, eversmoke, 
                                              coughsint_1, depress_1, mealssk, hhexposed, xpert) #stc_1

### Bootstrapping internal validation
resample <- lapply(1:20, function(i) sample(nrow(external.denovo), replace=T))
bootstrap <- external.denovo[unlist(resample),]
set.seed(2020)
n <- nrow(bootstrap)
sample <- sample(seq(n), size = n * (2/3), replace = FALSE)
train <- bootstrap[sample,][,-12]
test <- bootstrap[-sample,][,-12]

### Lasso regression 
train[] <- lapply(train, function(x){return(as.factor(x))})
x_vars <- model.matrix(xpert~. , train)[,-1]
y_var <- train$xpert
lambda <- cv.glmnet(x_vars, y_var, nfold=10, alpha = 1, family="binomial")$lambda.min
lasso_best <- glmnet(x_vars, y_var, alpha = 1, lambda = lambda, family="binomial") 
coef(lasso_best)
median(c(1.4079784, 1.4714193, 1.9959521, 1.8500085))
round(coef(lasso_best)/1.660714)

test[] <- lapply(test, function(x){return(as.factor(x))})
x_vars_new <- model.matrix(xpert~. , test)[,-1]
y_var_new <- test$xpert

roc(y_var, as.numeric(predict(lasso_best, x_vars, type = "response")), ci=T) #0.85(0.83-0.86), model derivation coefficient
roc(y_var_new, as.numeric(predict(lasso_best, x_vars_new, type = "response")), ci=T) #0.83(0.81-0.85), internal validation coefficient

# # If not divided the populations into derivation vs. internal validation
# external.denovo.1 <- lapply(external.denovo, function(x){return(as.factor(x))})
# x_vars <- model.matrix(xpert~. , external.denovo.1)[,-1]
# y_var <- external.denovo.1$xpert
# lambda <- cv.glmnet(x_vars, y_var, nfold=10, alpha = 1, family="binomial")$lambda.min
# lasso_best <- glmnet(x_vars, y_var, alpha = 1, lambda = lambda, family="binomial") 
# coef(lasso_best)
median(1.29791827, 1.45101827, 1.06287066, 1.96908950, 1.80646820) #1.297918
round(coef(lasso_best)/1.297918)

# Actual coefficient scoring system  ----> BELOW
# (Intercept)      -4.9409781   n_tbsymp2         0.5390470   n_tbsymp3         1.4079784
# n_tbsymp4         2.1076794   symp_2wks1        1.4714193   agecat41          1.1441589
# agecat42          1.1314662   agecat43          1.0674356   sexcat1           0.8620178
# hivarv1           0.1465374   hivarv2           1.9959521
# n_other_sympcat1  .           edu81             0.2822039   pasttb1           0.7112967
# eversmoke1        0.8657362   coughsint_11      1.8500085   depress_11       -0.4934497
# mealssk1         -0.5856827   stc_11           -0.3752417   hhexposed1       -0.2106405


# Simple scoring system 
# (Intercept)      -4   n_tbsymp2         0   n_tbsymp3         1   n_tbsymp4         1 (2)
# symp_2wks1        1   agecat41          1   agecat42          1   agecat43          1
# sexcat1           1   hivarv1           0   hivarv2           1 (2)
# n_other_sympcat1  .   edu81             0   pasttb1           1   eversmoke1        1
# coughsint_11      1   depress_11        0   mealssk1          0   stc_11            0   hhexposed1        0


### Make simple score and coefficient score in train and test data
# Based on the simple scoring, try a couple different scoring systems
train$score1 <- ifelse(train$agecat4==1|train$agecat4==2|train$agecat4==3,1,0) + ifelse(train$sexcat==1,1,0) + ifelse(train$hivarv==2,1,0) + ifelse(train$n_tbsymp==3|train$n_tbsymp==4,1,0) + ifelse(train$symp_2wks==1,1,0) +
                ifelse(train$pasttb==1,1,0) + ifelse(train$eversmoke==1,1,0) + ifelse(train$coughsint==1,1,0)
train$score1.1.1 <- ifelse(train$agecat4==1|train$agecat4==2|train$agecat4==3,1,0) + ifelse(train$sexcat==1,1,0) + ifelse(train$hivarv==2,1,0) + as.numeric(as.character(train$n_tbsymp)) + ifelse(train$symp_2wks==1,1,0) +
                ifelse(train$pasttb==1,1,0) + ifelse(train$eversmoke==1,1,0) + ifelse(train$coughsint==1,1,0)
roc(xpert ~ score1, data=train, ci=T) #0.81 (0.79-0.82)

train$score1.1 <- ifelse(train$agecat4==1|train$agecat4==2|train$agecat4==3,1,0) + ifelse(train$sexcat==1,1,0) + ifelse(train$hivarv==2,1,0) + ifelse(train$n_tbsymp==3|train$n_tbsymp==4,1,0) + ifelse(train$symp_2wks==1,1,0) +
                  ifelse(train$pasttb==1,1,0) + ifelse(train$eversmoke==1,1,0)
roc(xpert ~ score1.1, data=train, ci=T) # without coughs interview, 0.77 (0.75-0.78)

# internal validation
test$score1 <-  ifelse(test$agecat4==1|test$agecat4==2|test$agecat4==3,1,0) + ifelse(test$sexcat==1,1,0) + ifelse(test$hivarv==2,1,0) + ifelse(test$n_tbsymp==3|test$n_tbsymp==4,1,0) + ifelse(test$symp_2wks==1,1,0) +
                ifelse(test$pasttb==1,1,0) + ifelse(test$eversmoke==1,1,0) + ifelse(test$coughsint==1,1,0)
test$score1.1.1 <-  ifelse(test$agecat4==1|test$agecat4==2|test$agecat4==3,1,0) + ifelse(test$sexcat==1,1,0) + ifelse(test$hivarv==2,1,0) + as.numeric(as.character(test$n_tbsymp)) + ifelse(test$symp_2wks==1,1,0) +
                    ifelse(test$pasttb==1,1,0) + ifelse(test$eversmoke==1,1,0) + ifelse(test$coughsint==1,1,0)
roc(xpert ~ score1, data=test, ci=T) # 0.80 (0.78-0.82)

# train$score2 <- ifelse(train$agecat4==1|train$agecat4==2|train$agecat4==3,1,0) + ifelse(train$sexcat==1,1,0) + ifelse(train$hivarv==1|train$hivarv==2,1,0) + ifelse(train$n_tbsymp==3|train$n_tbsymp==4,1,0) + ifelse(train$symp_2wks==1,1,0) +
#                 ifelse(train$pasttb==1,1,0) + ifelse(train$eversmoke==1,1,0) 
# roc(xpert ~ score2, data=train, ci=T) #0.75 (0.74-0.77)
# 
# test$score2 <-  ifelse(test$agecat4==1|test$agecat4==2|test$agecat4==3,1,0) + ifelse(test$sexcat==1,1,0) + ifelse(test$hivarv==1|test$hivarv==2,1,0) + ifelse(test$n_tbsymp==3|test$n_tbsymp==4,1,0) + ifelse(test$symp_2wks==1,1,0) +
#                 ifelse(test$pasttb==1,1,0) + ifelse(test$eversmoke==1,1,0) 
# roc(xpert ~ score2, data=test, ci=T) # 0.75 (0.74-0.77)




### External validation data (Kharitode) additional data management # newfd_xpert_completecase
newfd_xpert_completecase$agecat4[newfd_xpert_completecase$agecat3=="0"|newfd_xpert_completecase$agecat3=="4"] <-0
newfd_xpert_completecase$agecat4[newfd_xpert_completecase$agecat3=="1"] <-1
newfd_xpert_completecase$agecat4[newfd_xpert_completecase$agecat3=="2"] <-2
newfd_xpert_completecase$agecat4[newfd_xpert_completecase$agecat3=="3"] <-3

newfd_xpert_completecase$hivarv[newfd_xpert_completecase$hivcat1==0] <-0
newfd_xpert_completecase$hivarv[newfd_xpert_completecase$hivcat1==1 & newfd_xpert_completecase$on_arvs_fac==1] <-1
newfd_xpert_completecase$hivarv[newfd_xpert_completecase$hivcat1==1 & (newfd_xpert_completecase$on_arvs_fac==2|newfd_xpert_completecase$on_arvs_fac==77)] <-2

newfd_xpert_completecase$stc_1[newfd_xpert_completecase$seek_care_days_fac<=14| newfd_xpert_completecase$seek_care_wks_fac<=2] <-0
newfd_xpert_completecase$stc_1[newfd_xpert_completecase$seek_care_days_fac>14| newfd_xpert_completecase$seek_care_wks_fac>2| newfd_xpert_completecase$seek_care_mth_fac| newfd_xpert_completecase$seek_care_yr_fac] <-1
newfd_xpert_completecase$stc_1[newfd_xpert_completecase$seek_care_days_fac==999] <-NA

kharitode.denovo <- newfd_xpert_completecase %>% dplyr::select(n_tbsymp, symp_2wks, agecat4, sexcat, dbcat, 
                                                               n_other_sympcat, edu8, pasttb, eversmoke, lungcat, hivarv, stc_1, xpert) 
kharitode.denovo[] <- lapply(kharitode.denovo, function(x){return(as.factor(x))})
Imput.kharitode.denovo <- mice(kharitode.denovo,10,pri=F)
imput.denovo <- complete(Imput.kharitode.denovo, action="long", include=T)

# imput.denovo$score <- ifelse(data.imputed.own.new$agecat3=="2",1,0) + ifelse(data.imputed.own.new$sexcat==1,1,0) + ifelse(data.imputed.own.new$hivcat1==1,2,0) + as.numeric(as.character(data.imputed.own.new$n_tbsymp)) + ifelse(data.imputed.own.new$symp_2wks==1,1,0) + ifelse(data.imputed.own.new$dbcat==1,1,0) +
#                       0*ifelse(data.imputed.own.new$n_other_sympcat==1,0,0) + 0*ifelse(data.imputed.own.new$edu8==1,0,0) + 0*ifelse(data.imputed.own.new$pasttb==1,0,0) + 0*ifelse(data.imputed.own.new$eversmoke==1,0,0) + 0*ifelse(data.imputed.own.new$lungcat==1,0,0)

imput.denovo$score.c<-ifelse(imput.denovo$n_tbsymp==2,0.5390470,0) + ifelse(imput.denovo$n_tbsymp==3,1.4079784,0) + ifelse(imput.denovo$n_tbsymp==4,2.1076794,0) + 
                      ifelse(imput.denovo$symp_2wks==1,1.4714193,0) + ifelse(imput.denovo$agecat4=="1",1.1441589,0) + ifelse(imput.denovo$agecat4=="2",1.1314662,0) + ifelse(imput.denovo$agecat4=="3",1.0674356,0) +
                      ifelse(imput.denovo$sexcat==1, 0.8620178,0) + ifelse(imput.denovo$edu8==1,0.2822039,0) + ifelse(imput.denovo$pasttb==1,0.7112967,0) + 
                      ifelse(imput.denovo$eversmoke==1,0.8657362,0) + ifelse(imput.denovo$hivarv==1, 0.1465374, 0) + ifelse(imput.denovo$hivarv==2, 1.9959521,0) + ifelse(imput.denovo$stc_1==1,-0.3752416,0)

# score1 can't exist because no coughs during the interview variable
imput.denovo$score1.1 <- ifelse(imput.denovo$agecat4==1|imput.denovo$agecat4==2|imput.denovo$agecat4==3,1,0) + ifelse(imput.denovo$sexcat==1,1,0) + ifelse(imput.denovo$hivarv==2,1,0) + ifelse(imput.denovo$n_tbsymp==3|imput.denovo$n_tbsymp==4,1,0) + ifelse(imput.denovo$symp_2wks==1,1,0) +
                         ifelse(imput.denovo$pasttb==1,1,0) + ifelse(imput.denovo$eversmoke==1,1,0)

imput.denovo$score1.1.1 <- ifelse(imput.denovo$agecat4==1|imput.denovo$agecat4==2|imput.denovo$agecat4==3,1,0) + ifelse(imput.denovo$sexcat==1,1,0) + ifelse(imput.denovo$hivarv==2,1,0) + as.numeric(as.character(imput.denovo$n_tbsymp)) + ifelse(imput.denovo$symp_2wks==1,1,0) +
                           ifelse(imput.denovo$pasttb==1,1,0) + ifelse(imput.denovo$eversmoke==1,1,0)
# 
# imput.denovo$score2 <- ifelse(imput.denovo$agecat4==1|imput.denovo$agecat4==2|imput.denovo$agecat4==3,1,0) + ifelse(imput.denovo$sexcat==1,1,0) + ifelse(imput.denovo$hivarv==1|imput.denovo$hivarv==2,1,0) + ifelse(imput.denovo$n_tbsymp==3|imput.denovo$n_tbsymp==4,1,0) + ifelse(imput.denovo$symp_2wks==1,1,0) +
#                        ifelse(imput.denovo$pasttb==1,1,0) + ifelse(imput.denovo$eversmoke==1,1,0) 

imput.denovo.back <- as.mids(imput.denovo)




### ROC
with(imput.denovo.back, roc(xpert~ score1.1.1, ci=T))$analyses
me = mean(unlist(with(imput.denovo.back, roc(xpert~ score1.1.1, ci=T)$auc)$analyses)) #0.77 (score2), 0.74 (score1)
m = 10
Vb = var(unlist(with(imput.denovo.back, roc(xpert~ score1.1.1, ci=T)$auc)$analyses)); for(i in 1:10){Vw0[[i]] <- (with(imput.denovo.back, roc(xpert~ score1.1.1, ci=T)$auc)$analyses[[i]]-me)^2}; Vw = (1/(m-1))*sum(Vw0)
SEp = sqrt(sum(Vb, Vw, Vb/m))
t = 2.042 # lambda = sum(Vb, Vb/m)/sum(Vb, Vw, Vb/m); dfo = (m-1)/(lambda)^2; dfb = ((1634-15)+1)/((1634-15)+3)*(1634-15)*(1-lambda); dfa = dfo*dfb/(dfo+dfb)
l95ci = me - t*SEp; h95ci = me + t*SEp; l95ci;h95ci


# simple score
png(filename = "Denovo simple score ROC.png")
par(pty='s')
roc(xpert ~ score1.1.1, data=train, ci=T, plot=T, col=2, lty=1)
roc(xpert ~ score1.1.1, data=test, ci=T, plot=T, add=T, col=2, lty=2)
roc <- with(imput.denovo.back, roc(xpert~ score1.1.1))$analyses
plot(roc[[1]], add=T, col=1, lty=1)
for(i in 2:10){plot(roc[[i]], add=T, col=1, lty=1)}
par(pty='m')
legend("bottomright", legend=c("Derivation data, c-statistic=0.81(0.80-0.83)", "Internal validation, c-statistic=0.81(0.79-0.83)", "External validation, c-statistic=0.78(0.77-0.79)"),
       col=c(2,2,1), lty=c(1,2,1), cex=0.8, box.lty=0, bty = "n")
dev.off()



# Simple score with risk score 8+
train %>% count(score1.1.1.810)
train$score1.1.1.810 <- train$score1.1.1
train$score1.1.1.810[train$score1.1.1==10|train$score1.1.1==9]<-8

test %>% count(score1.1.1.810)
test$score1.1.1.810 <- test$score1.1.1
test$score1.1.1.810[test$score1.1.1==10|test$score1.1.1==9]<-8

complete(imput.denovo.back,1) %>% count(score1.1.1)
imput.denovo$score1.1.1.810 <- imput.denovo$score1.1.1
imput.denovo$score1.1.1.810[imput.denovo$score1.1.1==9]<-8
imput.denovo.back <- as.mids(imput.denovo)

roc.train.810<-roc(xpert ~ score1.1.1.810, data=train, ci=T, col=2, lty=1, cex.lab=1.5, cex.axis=1.3, main="A")
roc.test.810<-roc(xpert ~ score1.1.1.810, data=test, ci=T, col=4, lty=2)

png(filename = "Denovo simple score ROC_score810.png")
par(pty='s')
plot.roc(roc.train.810, col=2, type="o",print.thres=c(1,2,3,4,5,6,7,8), print.thres.pattern = (c('1','2','3','4','5','6','7','8+')), print.thres.col = 'red', print.thres.adj=c(1,-0.5), cex.axis=1.2, cex.lab=1.5, main="A\n")
roc(xpert ~ score1.1.1.810, data=test, ci=T, plot=T, add=T, col=4, lty=1)
roc.denovo.810 <- with(imput.denovo.back, roc(xpert~ score1.1.1.810))$analyses
plot.roc(roc.denovo.810[[1]], add=T, col=1, type="o", lty=1, print.thres=c(1,2,3,4,5,6,7,8), print.thres.pattern = (c('1','2','3','4','5','6','7','8+')), print.thres.adj=c(-0.5,1))
for(i in 2:10){plot(roc.denovo.810[[i]], add=T, col=1, lty=1)}
par(pty='m')
legend("bottomright", legend=c("Derivation data, c-statistic=0.81(0.80-0.83)", "Internal validation, c-statistic=0.81(0.79-0.83)", "External validation, c-statistic=0.78(0.77-0.79)"),
       col=c(2,4,1), lty=c(1,1,1), cex=1.1, box.lty=0, bty = "n")
dev.off()

plot.roc(roc.denovo.810[[1]], col=1, type="o", lty=1, print.thres=c(1,2,3,4,5,6,7,8))
plot.roc(roc.train.810, col=2, type="o",print.thres=c(1,2,3,4,5,6,7,8))






# coefficient score
png(filename = "Denovo coefficient score ROC_cex.png")
par(pty='s')
roc(y_var, as.numeric(predict(lasso_best, x_vars, type = "response")), ci=T, plot=T, col=2, lty=1, cex.lab=1.5, cex.axis=1.3, main="B") #0.85(0.83-0.86), model derivation coefficient
roc(y_var_new, as.numeric(predict(lasso_best, x_vars_new, type = "response")), ci=T, plot=T, col=4, lty=1, add=T) #0.83(0.81-0.85), internal validation coefficient
roc(y_var, as.numeric(predict(lasso_best, x_vars, type = "response")), ci=T, plot=T, col=2, lty=1, add=T) #0.85(0.83-0.86), model derivation coefficient
roc <- with(imput.denovo.back, roc(xpert~ score.c))$analyses
plot(roc[[1]], add=T, col=1, lty=1)
for(i in 2:10){plot(roc[[i]], add=T, col=1, lty=1)}
par(pty='m')
legend("bottomright", legend=c("Derivation data, c-statistic=0.85(0.83-0.86)", "Internal validation, c-statistic=0.83(0.81-0.85)", "External validation, c-statistic=0.78(0.77-0.78)"),
       col=c(2,4,1), lty=c(1,1,1), cex=1.1, box.lty=0, bty = "n")
dev.off()





### Calibration
# internal, no adjustment required
fit.denovo <- glm(xpert ~ score1.1.1, data=train, family = binomial())
pred.internal <- fit.denovo %>% predict(test, type="response")

png(filename = "Denovo calibration in the internal validation (no adjustment needed)_cex.png")
val.prob.ci.2(pred.internal, as.numeric(as.character(test$xpert)), logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black", cex.leg = 1.1, cex.d01 = 1.1, cex = 1.1, cex.axis=1.2, cex.lab=1.4, main="A")
dev.off()


# external, without adjustment first
fit.denovo.1 <- glm(xpert ~ score1.1.1, data=train, family=binomial())

pred.external<-list()
p<-matrix(nrow=1409)
for(i in 1:10){
d <- complete(imput.denovo.back, i)
pred.external[[i]] <- fit.denovo.1 %>% predict(d, type="response")
p <- cbind(p,pred.external[[i]])
}
p<-p[,-1]

png(filename = "Denovo calibration in the external validation without adj.png")
val.prob.ci.2(rowMeans(p), as.numeric(as.character(d$xpert)), logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black", cex.leg = 1.1, cex.d01 = 1.1, cex = 1.1, cex.axis=1.2, cex.lab=1.4) #xpert will be same in any multiple imputation datasets, so use one of the multiple imputation datasets
dev.off()


# external, with adjustment
pre_tb = 707/(707+702)
post_tb = 1433/(1433+3727)
fit.denovo.1.adj <- glm(xpert ~ score1.1.1, data=train, family=binomial())
fit.denovo.1.adj$coefficients[['(Intercept)']] <- fit.denovo.1.adj$coefficients[['(Intercept)']] + log((pre_tb/(1-pre_tb))/(post_tb/(1-post_tb)))

pred.external.adj<-list()
p.adj<-matrix(nrow=1409)
for(i in 1:10){
  d <- complete(imput.denovo.back, i)
  pred.external.adj[[i]] <- fit.denovo.1.adj %>% predict(d, type="response")
  p.adj <- cbind(p.adj,pred.external.adj[[i]])
}
p.adj<-p.adj[,-1]

png(filename= "Denovo calibration in the external validation with adj_cex.png")
val.prob.ci.2(rowMeans(p.adj), as.numeric(as.character(d$xpert)), logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black", cex.leg = 1.1, cex.d01 = 1.1, cex = 1.1, cex.axis=1.2, cex.lab=1.4, main="B")
dev.off()








# REMOVE STC_1, lungcat, and mealssk --> everything remained almost same
# (Intercept)      -4.90523159
# n_tbsymp2         0.49068430
# n_tbsymp3         1.29791827
# n_tbsymp4         2.02816386
# symp_2wks1        1.45101827
# agecat41          1.00133226
# agecat42          1.06287066
# agecat43          0.98934955
# sexcat1           0.88942699
# hivarv1           0.01382272
# hivarv2           1.96908950
# n_other_sympcat1  .         
# edu81             0.21346356
# pasttb1           0.67229627
# eversmoke1        0.70677327
# coughsint_11      1.80646820
# depress_11       -0.61393860
# hhexposed1       -0.17121863

