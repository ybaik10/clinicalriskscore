## Bring combined data and newly made additional management
newfd <- fddata
# In the score, add symptom and use dichotomized number of symptoms -- I dichotomized at number of symptoms = 3 
newfd$n_tbsymp_cat <- cut(newfd$n_tbsymp, breaks=c(0,3,5), right=F)
newfd$n_tbsymp_cat1[newfd$n_tbsymp_cat=="[0,3)"]<-0
newfd$n_tbsymp_cat1[newfd$n_tbsymp_cat=="[3,5)"]<-1
# Age shouldn't be dichotomized -- how to assign the score? Based on distribution and univariable regression, from oldest 1 to youngest higher score?
logistic.display(glm(xpert ~ as.factor(agecat1), data=newfd, family=binomial))
chisq.test(table(newfd$hivcat1, as.factor(newfd$agecat1)))

newfd$agecat2[newfd$agecat1=="[55,99)"|newfd$agecat1=="[15,25)"]<-0
newfd$agecat2[newfd$agecat1=="[45,55)"]<-1
newfd$agecat2[newfd$agecat1=="[25,35)"|newfd$agecat1=="[35,45)"]<-2

newfd %>% count(employ_fac)
newfd %>% filter(n_tbsymp>0) %>% group_by(xpert) %>% count(employ_fac) %>% view()

newfd %>% filter(n_tbsymp>0, !is.na(xpert), income_employ_fac==1) %>% group_by(xpert) %>% count(income_employ_fac)
newfd %>% filter(n_tbsymp>0, !is.na(xpert), income_casual_fac==1) %>% group_by(xpert) %>% count(income_casual_fac)
newfd %>% filter(n_tbsymp>0, !is.na(xpert), income_grant_fac==1) %>% group_by(xpert) %>% count(income_grant_fac)
newfd %>% filter(n_tbsymp>0, !is.na(xpert), income_other_fac==1) %>% group_by(xpert) %>% count(income_other_fac)

newfd$income_employ_amt_fac_1 <- newfd$income_employ_amt_fac
newfd$income_casual_amt_fac_1 <- newfd$income_casual_amt_fac
newfd$income_grant_amt_fac_1 <- newfd$income_grant_amt_fac
newfd$income_other_amt_fac_1 <- newfd$income_other_amt_fac
newfd$income_employ_amt_fac_1[newfd$income_employ_amt_fac==999]<-NA
newfd$income_casual_amt_fac_1[newfd$income_casual_amt_fac==999]<-NA
newfd$income_grant_amt_fac_1[newfd$income_grant_amt_fac==999]<-NA
newfd$income_other_amt_fac_1[newfd$income_other_amt_fac==999]<-NA

newfd$income <- newfd$income_employ_amt_fac_1 + newfd$income_casual_amt_fac_1 + newfd$income_grant_amt_fac_1 + newfd$income_other_amt_fac_1
newfd %>% filter(n_tbsymp>0, !is.na(xpert), (income_employ_fac==1|income_casual_fac==1|income_grant_fac==1|income_other_fac==1)) %>% group_by(xpert) %>% summarise(median(income, na.rm=T), quantile(income, 0.25, na.rm=T), quantile(income, 0.75, na.rm=T))

table1::table1(~ agecat1 + factor(sexcat) + factor(hivcat1) + factor(hivarv) + 
                 factor(n_tbsymp) + factor(symp_fac___1cat) + factor(symp_fac___2cat) + factor(symp_fac___3cat) + factor(symp_fac___4cat) + 
                 factor(symp_2wks) + factor(n_other_sympcat) + factor(dbcat) + factor(lungcat) + factor(edu8) + factor(pasttb) + factor(eversmoke) + factor(lungcat) + 
                 factor(n_other_sympcat) | factor(xpert), data=newfd_xpert_completecase[!is.na(newfd_xpert_completecase$n_tbsymp),])

newfd %>% filter(n_tbsymp>0) %>% group_by(xpert) %>% count(agecat1)
newfd %>% filter(n_tbsymp>0) %>% group_by(xpert) %>% count(sexcat)
newfd %>% filter(n_tbsymp>0) %>% group_by(xpert) %>% count(hivarv)
newfd %>% filter(is.na(income)) %>% count


## After running all these codes (??), save newfd
write.csv(newfd, "newfd.csv")

library(tidyverse); library(pROC); library(epiDisplay); library(mice)
#########################################################################################
##### ROC revision based on the variable selection done by Hannah ##### 
##### WITHOUT Multiple Imputation #####
#########################################################################################
## simple scoring
# final model
newfd$simpleoriginal <- newfd$agecat2 + newfd$sexcat + newfd$hivcat1 + newfd$dbcat + newfd$n_tbsymp + newfd$n_other_sympcat + newfd$symp_2wks
fit <- glm(xpert ~ simpleoriginal, family=binomial(), data=newfd)
# new final model (with actual symptoms)
newfd$simplenew <- newfd$agecat2 + newfd$sexcat + newfd$hivcat1 + newfd$dbcat + newfd$symp_fac___1cat + newfd$symp_fac___2cat + newfd$symp_fac___3cat + newfd$symp_fac___4cat + newfd$n_tbsymp_cat1 + newfd$n_other_sympcat + newfd$symp_2wks
# full model
newfd$simpleoriginalf <- newfd$agecat2 + newfd$sexcat + newfd$hivcat1 + newfd$dbcat + newfd$n_tbsymp + newfd$n_other_sympcat + newfd$symp_2wks + newfd$edu8 + newfd$pasttb + newfd$eversmoke + newfd$hivarv + newfd$lungcat 
# new full model (with actual symptoms)
newfd$simplenewf <- newfd$agecat2 + newfd$sexcat + newfd$hivcat1 + newfd$dbcat + newfd$symp_fac___1cat + newfd$symp_fac___2cat + newfd$symp_fac___3cat + newfd$symp_fac___4cat + newfd$n_tbsymp_cat1 + newfd$n_other_sympcat + newfd$symp_2wks + newfd$edu8 + newfd$pasttb + newfd$eversmoke + newfd$hivarv + newfd$lungcat

## whole number scoring
fit.simpleoriginal <- glm(xpert~ agecat2+sexcat+hivcat1+dbcat+n_tbsymp+n_other_sympcat+symp_2wks, family=binomial(), data=newfd); fit.simpleoriginal$coefficients
newfd$roundoriginal <- 0.2*newfd$agecat2 + 0.9*newfd$sexcat + 1.2*newfd$hivcat1 + 0.6*newfd$dbcat + 0.6*newfd$n_tbsymp + 0.5*newfd$n_other_sympcat + 0.8*newfd$symp_2wks
fit.simplenew <- glm(xpert~ agecat2+sexcat+hivcat1+dbcat+ newfd$symp_fac___1cat + newfd$symp_fac___2cat + newfd$symp_fac___3cat + newfd$symp_fac___4cat + newfd$n_tbsymp_cat1+n_other_sympcat+symp_2wks, family=binomial(), data=newfd); fit.simplenew$coefficients
newfd$roundnew <- 0.2*newfd$agecat2 + 0.8*newfd$sexcat + 1.0*newfd$hivcat1 + 0.5*newfd$dbcat -0.9*newfd$symp_fac___1cat + 0.05*newfd$symp_fac___2cat + 1.6*newfd$symp_fac___3cat + 0.4*newfd$symp_fac___4cat + 0.4*newfd$n_tbsymp_cat1 + 0.3*newfd$n_other_sympcat + 0.6*newfd$symp_2wks
fit.simpleoriginalf <- glm(xpert~ agecat2+sexcat+hivcat1+dbcat+n_tbsymp+n_other_sympcat+symp_2wks+ newfd$edu8 + newfd$pasttb + newfd$eversmoke + newfd$hivarv + newfd$lungcat, family=binomial(), data=newfd); fit.simpleoriginalf$coefficients
newfd$roundoriginalf <-0.2* newfd$agecat2 + 1.2*newfd$sexcat + 1.0*newfd$hivcat1 + 0.6*newfd$dbcat + 0.6*newfd$n_tbsymp + 0.6*newfd$n_other_sympcat + 0.8*newfd$symp_2wks -0.1*newfd$edu8 + 0.1*newfd$pasttb -0.5*newfd$eversmoke + 0.6*newfd$hivarv + 0.08*newfd$lungcat
fit.simplenewf <- glm(xpert~ agecat2+sexcat+hivcat1+dbcat+newfd$symp_fac___1cat + newfd$symp_fac___2cat + newfd$symp_fac___3cat + newfd$symp_fac___4cat + newfd$n_tbsymp_cat1+n_other_sympcat+symp_2wks+ newfd$edu8 + newfd$pasttb + newfd$eversmoke + newfd$hivarv + newfd$lungcat, family=binomial(), data=newfd); fit.simplenewf$coefficients
newfd$roundnewf <- 0.2*newfd$agecat2 + 1.1*newfd$sexcat + 0.8*newfd$hivcat1 + 0.5*newfd$dbcat -0.9*newfd$symp_fac___1cat + 0.1*newfd$symp_fac___2cat + 1.6*newfd$symp_fac___3cat + 0.4*newfd$symp_fac___4cat + 0.3*newfd$n_tbsymp_cat1 + 0.3*newfd$n_other_sympcat + 0.6*newfd$symp_2wks -0.08*newfd$edu8 + 0.2*newfd$pasttb -0.5*newfd$eversmoke + 0.8*newfd$hivarv - 0.1*newfd$lungcat

## roc without imputation
# simple scoring
roc(xpert~simpleoriginal, data=newfd, ci=T, plot=T, col=2) #0.79 (0.77-0.81)
roc(xpert~simplenew, data=newfd, ci=T, plot=T)
roc(xpert~simpleoriginalf, data=newfd, ci=T, plot=T)
roc(xpert~simplenewf, data=newfd, ci=T, plot=T)

# coefficient scoring
# roc.simpleoriginal<-lroc(glm(xpert~ agecat2+sexcat+hivcat1+dbcat+n_tbsymp+n_other_sympcat+symp_2wks, family=binomial(), data=newfd))
roc.simpleoriginal <- roc(newfd[complete.cases(newfd$simpleoriginal)&complete.cases(newfd$xpert),]$xpert~ fit.simpleoriginal$fitted.values, ci=T, plot=T, col=2)
roc.simplenew <- roc(newfd[complete.cases(newfd$simplenew)&complete.cases(newfd$xpert),]$xpert~ fit.simplenew$fitted.values, ci=T, plot=T)
roc.simpleoriginalf <- roc(newfd[complete.cases(newfd$simpleoriginalf)&complete.cases(newfd$xpert),]$xpert~ fit.simpleoriginalf$fitted.values, ci=T, plot=T)
roc.simplenewf <- roc(newfd[complete.cases(newfd$simplenewf)&complete.cases(newfd$xpert),]$xpert~ fit.simplenewf$fitted.values, ci=T, plot=T)

# whole number scoring
roc(xpert~roundoriginal, data=newfd, ci=T, plot=T, col=1)
roc(xpert~roundnew, data=newfd, ci=T, plot=T, col=2)
roc(xpert~roundoriginalf, data=newfd, ci=T, plot=T, col=3)
roc(xpert~roundnewf, data=newfd, ci=T, plot=T, col=4)


###############################################################################################
##### SINGLE Imputation #####
##### MODE (replace with the most prevalent values) #####
###############################################################################################
# newfd$agecat2_rep <- ifelse(is.na(newfd$agecat),0,newfd$agecat2)
# newfd$sexcat_rep <- ifelse(is.na(newfd$sexcat),1,newfd$sexcat)
newfd$hivcat1_rep <- ifelse(is.na(newfd$hivcat1),0,newfd$hivcat1)
newfd$dbcat_rep <- ifelse(is.na(newfd$dbcat),0,newfd$dbcat)
newfd$symp_fac___1cat_rep <-ifelse(is.na(newfd$symp_fac___1cat),1,newfd$symp_fac___1cat)
newfd$symp_fac___2cat_rep <-ifelse(is.na(newfd$symp_fac___2cat),0,newfd$symp_fac___2cat)
newfd$symp_fac___3cat_rep <-ifelse(is.na(newfd$symp_fac___3cat),0,newfd$symp_fac___3cat)
newfd$symp_fac___4cat_rep <-ifelse(is.na(newfd$symp_fac___4cat),0,newfd$symp_fac___4cat)
newfd$n_other_sympcat_rep <-ifelse(is.na(newfd$n_other_sympcat),1,newfd$n_other_sympcat)
newfd$symp_2wks_rep <-ifelse(is.na(newfd$symp_2wks),1,newfd$symp_2wks)
newfd$edu8_rep <- ifelse(is.na(newfd$edu8),0,newfd$edu8)
newfd$pasttb_rep <- ifelse(is.na(newfd$pasttb),0,newfd$pasttb)
newfd$eversmoke_rep <- ifelse(is.na(newfd$eversmoke),0,newfd$eversmoke)
newfd$lungcat_rep <- ifelse(is.na(newfd$lungcat),0,newfd$lungcat)
newfd$n_tbsymp_rep <- newfd$symp_fac___1cat_rep + newfd$symp_fac___2cat_rep + newfd$symp_fac___3cat_rep + newfd$symp_fac___4cat_rep
newfd$n_tbsymp_cat_rep <- cut(newfd$n_tbsymp_rep, breaks=c(0,3,5), right=F)
newfd$n_tbsymp_cat1_rep[newfd$n_tbsymp_cat_rep=="[0,3)"]<-0
newfd$n_tbsymp_cat1_rep[newfd$n_tbsymp_cat_rep=="[3,5)"]<-1

# simple scoring
modeoriginal <- newfd$agecat2 + newfd$sexcat + newfd$hivcat1_rep + newfd$dbcat_rep + newfd$n_tbsymp_rep + newfd$n_other_sympcat_rep + newfd$symp_2wks_rep
modenew <- newfd$agecat2 + newfd$sexcat + newfd$hivcat1_rep + newfd$dbcat_rep + newfd$symp_fac___1cat_rep + newfd$symp_fac___2cat_rep + newfd$symp_fac___3cat_rep + newfd$symp_fac___4cat_rep + newfd$n_tbsymp_cat1_rep + newfd$n_other_sympcat_rep + newfd$symp_2wks_rep
modeoriginalf <- newfd$agecat2 + newfd$sexcat + newfd$hivcat1_rep + newfd$dbcat_rep + newfd$n_tbsymp_rep + newfd$n_other_sympcat_rep + newfd$symp_2wks_rep + newfd$edu8_rep + newfd$pasttb_rep + newfd$eversmoke_rep + newfd$lungcat_rep
modenewf <- newfd$agecat2 + newfd$sexcat + newfd$hivcat1_rep + newfd$dbcat_rep + newfd$symp_fac___1cat_rep + newfd$symp_fac___2cat_rep + newfd$symp_fac___3cat_rep + newfd$symp_fac___4cat_rep + newfd$n_tbsymp_cat1_rep + newfd$n_other_sympcat_rep + newfd$symp_2wks_rep + newfd$edu8_rep + newfd$pasttb_rep + newfd$eversmoke_rep + newfd$lungcat_rep

roc(xpert~modeoriginal, data=newfd, ci=T, plot=T) # 0.80 (0.78-0.82)
roc(xpert~modenew, data=newfd, ci=T, plot=T) # 0.80 (0.78 - 0.82)
roc(xpert~modeoriginalf, data=newfd, ci=T, plot=T) # 0.78 (0.76-0.80)
roc(xpert~modenewf, data=newfd, ci=T, plot=T) # 0.79 (0.77-0.81)

# whole number scoring
fit.modeoriginal <- glm(newfd$xpert~ newfd$agecat2 + newfd$sexcat + newfd$hivcat1_rep + newfd$dbcat_rep + newfd$n_tbsymp_rep + newfd$n_other_sympcat_rep + newfd$symp_2wks_rep, family=binomial()); fit.modeoriginal$coefficients
fit.modenew <- glm(newfd$xpert~ newfd$agecat2 + newfd$sexcat + newfd$hivcat1_rep + newfd$dbcat_rep + newfd$symp_fac___1cat_rep + newfd$symp_fac___2cat_rep + newfd$symp_fac___3cat_rep + newfd$symp_fac___4cat_rep + newfd$n_tbsymp_cat1_rep + newfd$n_other_sympcat_rep + newfd$symp_2wks_rep, family=binomial()); fit.modenew$coefficients
fit.modeoriginalf <- glm(newfd$xpert~ newfd$agecat2 + newfd$sexcat + newfd$hivcat1_rep + newfd$dbcat_rep + newfd$n_tbsymp_rep + newfd$n_other_sympcat_rep + newfd$symp_2wks_rep + newfd$edu8_rep + newfd$pasttb_rep + newfd$eversmoke_rep + newfd$lungcat_rep, family=binomial()); fit.modeoriginalf$coefficients
fit.modenewf <- glm(newfd$xpert~ newfd$agecat2 + newfd$sexcat + newfd$hivcat1_rep + newfd$dbcat_rep + newfd$symp_fac___1cat_rep + newfd$symp_fac___2cat_rep + newfd$symp_fac___3cat_rep + newfd$symp_fac___4cat_rep + newfd$n_tbsymp_cat1_rep + newfd$n_other_sympcat_rep + newfd$symp_2wks_rep + newfd$edu8_rep + newfd$pasttb_rep + newfd$eversmoke_rep + newfd$lungcat_rep, family=binomial()); fit.modenewf$coefficients

roc(newfd[complete.cases(newfd$xpert),]$xpert~ round(fit.modeoriginal$fitted.values, digits=1), ci=T, plot=T, col=2) # 0.81(0.79-0.83)
roc(newfd[complete.cases(newfd$xpert),]$xpert~ round(fit.modenew$fitted.values, digits=1), ci=T, plot=T) #0.83(0.81-0.85)
roc(newfd[complete.cases(newfd$xpert),]$xpert~ round(fit.modeoriginalf$fitted.values, digits=1), ci=T, plot=T, col=2) #0.81 (0.79-0.83)
roc(newfd[complete.cases(newfd$xpert),]$xpert~ round(fit.modenewf$fitted.values, digits=1), ci=T, plot=T) #0.83(0.81-0.85)


# coefficient scoring
roc.modecoeforiginal <- roc(newfd[complete.cases(newfd$xpert),]$xpert~ fit.modeoriginal$fitted.values, ci=T, plot=T, col=2) # 0.81(0.79-0.83)
roc.modecoefnew <- roc(newfd[complete.cases(newfd$xpert),]$xpert~ fit.modenew$fitted.values, ci=T, plot=T) #0.83(0.81-0.85)
roc.modecoeforiginalf <- roc(newfd[complete.cases(newfd$xpert),]$xpert~ fit.modeoriginalf$fitted.values, ci=T, plot=T, col=2) #0.82 (0.80-0.84)
roc.modecoefnewf <- roc(newfd[complete.cases(newfd$xpert),]$xpert~ fit.modenewf$fitted.values, ci=T, plot=T) #0.84(0.82-0.85)



###############################################################################################
##### MULTIPLE imputation #####
###############################################################################################
newfd.original <- newfd %>% dplyr::select(simpleoriginal, n_tbsymp, symp_2wks, agecat2, sexcat, hivcat1, dbcat, symp_fac___1cat, symp_fac___2cat, symp_fac___3cat, symp_fac___4cat, n_other_sympcat, edu8, pasttb, eversmoke, lungcat, xpert)
newfd.original[] <- lapply(newfd.original, function(x){return(as.factor(x))})
newfd.new <- newfd %>% dplyr::select(simplenew, symp_2wks, agecat2, sexcat, hivcat1, dbcat, symp_fac___1cat, symp_fac___2cat, symp_fac___3cat, symp_fac___4cat, n_tbsymp_cat1, n_other_sympcat,edu8, pasttb, eversmoke, lungcat, xpert)
newfd.new[] <- lapply(newfd.new, function(x){return(as.factor(x))})
newfd.originalf <- newfd %>% dplyr::select(simpleoriginalf, n_tbsymp, symp_2wks, agecat2, sexcat, hivcat1, dbcat, symp_fac___1cat, symp_fac___2cat, symp_fac___3cat, symp_fac___4cat, n_other_sympcat, edu8, pasttb, eversmoke, lungcat, xpert)
newfd.originalf[] <- lapply(newfd.originalf, function(x){return(as.factor(x))})
newfd.newf <- newfd %>% dplyr::select(simplenewf, symp_2wks, agecat2, sexcat, hivcat1, dbcat, symp_fac___1cat, symp_fac___2cat, symp_fac___3cat, symp_fac___4cat, n_tbsymp_cat1, n_other_sympcat,edu8, pasttb, eversmoke, lungcat, xpert)
newfd.newf[] <- lapply(newfd.newf, function(x){return(as.factor(x))})

imputed.original<- mice(newfd.original, m=15, maxit=10, method=c(rep('pmm',2),'logreg','polyreg',rep('logreg',13)))
summary(imputed.original)
imputed.new<- mice(newfd.new, m=15, maxit=10, method=c('pmm','logreg','polyreg',rep('logreg',14)))
summary(imputed.new)
imputed.originalf <- mice(newfd.originalf, m=15, maxit=10, method=c(rep('pmm',2),'logreg','polyreg',rep('logreg',13)))
summary(imputed.originalf)
imputed.newf <- mice(newfd.newf, m=5, maxit=5, method=c('pmm','logreg','polyreg',rep('logreg',14)))
summary(imputed.newf)

# simple score *** primary outcome ***
original.roc <- with(imputed.original, roc(xpert~ glm(xpert~as.numeric(simpleoriginal), family=binomial())$fitted.values, plot=T, col=2, main="Figure 1. (b) Discrimination",)) #ci=T
original.auc <- with(imputed.original, roc(xpert~ glm(xpert~as.numeric(simpleoriginal), family=binomial())$fitted.values)$auc)
me = mean(unlist(original.auc$analyses))
m = 15
Vb = var(unlist(original.auc$analyses))
Vw0 <- NA
for(i in 1:15){
  Vw0[[i]] <- (original.auc$analyses[[i]]-me)^2
}
Vw = (1/(m-1))*sum(Vw0)
SEp = sqrt(sum(Vb, Vw, Vb/m))
t = 1.98
# lambda = sum(Vb, Vb/m)/sum(Vb, Vw, Vb/m)
# dfo = (m-1)/(lambda)^2
# dfb = ((1634-15)+1)/((1634-15)+3)*(1634-15)*(1-lambda)
# dfa = dfo*dfb/(dfo+dfb)
# l95ci = me - qnorm(.975)*SEp/sqrt(m)
# h95ci = me + qnorm(.975)*SEp/sqrt(m)
l95ci = me - t*SEp
h95ci = me + t*SEp
me; l95ci; h95ci #0.80 (0.79-0.80)

# *** validation dataset primary outcome ***
roc.stomp.xpert <- roc(st.xp$xpertcc, st.xp$preds, smoothed=TRUE, plot=TRUE, legacy.axes=TRUE, ci=T)
plot(roc.stomp.xpert, col=1, lty=1,  add=T)
par(pty='m')
legend("bottomright",legend=c("Derivation data, c-statistic=0.798 (0.792-0.803)", "Validation data, c-statistic = 0.761 (0.703-0.818)"), col=c(2,1), lty=1:1, cex=1.0, box.lty=0, bty = "n")

# *** Different if the negative group is multiplied? ***
newfd10<-rbind(newfd, newfd %>% filter(xpert==0), newfd %>% filter(xpert==0), newfd %>% filter(xpert==0), newfd %>% filter(xpert==0), newfd %>% filter(xpert==0), newfd %>% filter(xpert==0), newfd %>% filter(xpert==0))
newfd10.original <- newfd10 %>% dplyr::select(simpleoriginal, n_tbsymp, symp_2wks, agecat2, sexcat, hivcat1, dbcat, symp_fac___1cat, symp_fac___2cat, symp_fac___3cat, symp_fac___4cat, n_other_sympcat, edu8, pasttb, eversmoke, lungcat, xpert)
newfd10.original[] <- lapply(newfd10.original, function(x){return(as.factor(x))})
imputed10.original<- mice(newfd10.original, m=15, maxit=10, method=c(rep('pmm',2),'logreg','polyreg',rep('logreg',13)))
summary(imputed10.original)
original10.roc <- with(imputed10.original, roc(xpert~ glm(xpert~as.numeric(simpleoriginal), family=binomial())$fitted.values, plot=T, col=2, main="Figure 1. (b) Discrimination",)) #ci=T
original10.auc <- with(imputed10.original, roc(xpert~ glm(xpert~as.numeric(simpleoriginal), family=binomial())$fitted.values)$auc)
me = mean(unlist(original10.auc$analyses)) #0.7987593


simplenew.roc <- with(imputed.new, roc(xpert~ glm(xpert~as.numeric(simplenew), family=binomial())$fitted.values, plot=T))
simplenew.auc <- with(imputed.new, roc(xpert~ glm(xpert~as.numeric(simplenew), family=binomial())$fitted.values)$auc)
me = mean(unlist(simplenew.auc$analyses))
m = 15
Vb = var(unlist(simplenew.auc$analyses))
Vw0 <- NA
for(i in 1:15){
  Vw0[[i]] <- (simplenew.auc$analyses[[i]]-me)^2
}
Vw = (1/(m-1))*sum(Vw0)
SEp = sqrt(sum(Vb, Vw, Vb/m))
t = 1.98
l95ci = me - t*SEp
h95ci = me + t*SEp
me; l95ci; h95ci #0.80 (0.80-0.80)

originalf.roc <- with(imputed.originalf, roc(xpert~ glm(xpert~as.numeric(simpleoriginalf), family=binomial())$fitted.values, plot=T)) #ci=T
originalf.auc <- with(imputed.originalf, roc(xpert~ glm(xpert~as.numeric(simpleoriginalf), family=binomial())$fitted.values)$auc)
me = mean(unlist(originalf.auc$analyses))
m = 15
Vb = var(unlist(originalf.auc$analyses))
Vw0 <- NA
for(i in 1:15){
  Vw0[[i]] <- (originalf.auc$analyses[[i]]-me)^2
}
Vw = (1/(m-1))*sum(Vw0)
SEp = sqrt(sum(Vb, Vw, Vb/m))
t = 1.98
l95ci = me - t*SEp
h95ci = me + t*SEp
me; l95ci; h95ci #0.78 (0.78-0.79)

simplenewf.roc <- with(imputed.newf, roc(xpert~ glm(xpert~as.numeric(simplenewf), family=binomial())$fitted.values, plot=T))
simplenewf.auc <- with(imputed.newf, roc(xpert~ glm(xpert~as.numeric(simplenewf), family=binomial())$fitted.values)$auc)
me = mean(unlist(simplenewf.auc$analyses))
m = 5
Vb = var(unlist(simplenewf.auc$analyses))
Vw0 <- NA
for(i in 1:5){
  Vw0[[i]] <- (simplenewf.auc$analyses[[i]]-me)^2
}
Vw = (1/(m-1))*sum(Vw0)
SEp = sqrt(sum(Vb, Vw, Vb/m))
t = 1.98
l95ci = me - t*SEp
h95ci = me + t*SEp
me; l95ci; h95ci #0.79 (0.79-0.80)


# coefficient scoring
multipleimputationfinalfit <- with(imputed.original, glm(xpert~as.numeric(as.character(n_tbsymp))+symp_2wks+agecat2+sexcat+hivcat1+dbcat+n_other_sympcat, family=binomial()))
summary(pool(multipleimputationfinalfit))
coeforiginal.roc <- with(imputed.original, roc(xpert~ glm(xpert~n_tbsymp+symp_2wks+agecat2+sexcat+hivcat1+dbcat+n_other_sympcat, family=binomial())$fitted.values, plot=T)) #ci=T
coeforiginal.auc <- with(imputed.original, roc(xpert~ glm(xpert~n_tbsymp+symp_2wks+agecat2+sexcat+hivcat1+dbcat+n_other_sympcat, family=binomial())$fitted.values)$auc)
me = mean(unlist(coeforiginal.auc$analyses))
m = 15
Vb = var(unlist(coeforiginal.auc$analyses))
Vw0 <- NA
for(i in 1:15){
  Vw0[[i]] <- (coeforiginal.auc$analyses[[i]]-me)^2
}
Vw = (1/(m-1))*sum(Vw0)
SEp = sqrt(sum(Vb, Vw, Vb/m))
t = 1.98
l95ci = me - t*SEp
h95ci = me + t*SEp
me; l95ci; h95ci #0.82 (0.81-0.82)


multipleimputationfinalnewfit <- with(imputed.new, glm(xpert~symp_2wks+agecat2+sexcat+hivcat1+dbcat+n_other_sympcat+symp_fac___1cat +symp_fac___2cat +symp_fac___3cat +symp_fac___4cat +n_tbsymp_cat1, family=binomial()))
summary(pool(multipleimputationfinalnewfit))
coefnew.roc <- with(imputed.new, roc(xpert~ glm(xpert~agecat2+sexcat+hivcat1+dbcat+symp_fac___1cat +symp_fac___2cat +symp_fac___3cat +symp_fac___4cat +n_tbsymp_cat1+n_other_sympcat+symp_2wks, family=binomial())$fitted.values, plot=T))
coefnew.auc <- with(imputed.new, roc(xpert~ glm(xpert~agecat2+sexcat+hivcat1+dbcat+symp_fac___1cat +symp_fac___2cat +symp_fac___3cat +symp_fac___4cat +n_tbsymp_cat1+n_other_sympcat+symp_2wks, family=binomial())$fitted.values)$auc)
me = mean(unlist(coefnew.auc$analyses))
m = 15
Vb = var(unlist(coefnew.auc$analyses))
Vw0 <- NA
for(i in 1:15){
  Vw0[[i]] <- (coefnew.auc$analyses[[i]]-me)^2
}
Vw = (1/(m-1))*sum(Vw0)
SEp = sqrt(sum(Vb, Vw, Vb/m))
t = 1.98
l95ci = me - t*SEp
h95ci = me + t*SEp
me; l95ci; h95ci #0.83 (0.83-0.84)

coeforiginalf.roc <- with(imputed.originalf, roc(xpert~ glm(xpert~n_tbsymp+symp_2wks+agecat2+sexcat+hivcat1+dbcat+n_other_sympcat+edu8+pasttb+eversmoke+lungcat, family=binomial())$fitted.values, plot=T)) #ci=T
coeforiginalf.auc <- with(imputed.originalf, roc(xpert~ glm(xpert~n_tbsymp+symp_2wks+agecat2+sexcat+hivcat1+dbcat+n_other_sympcat+edu8+pasttb+eversmoke+lungcat, family=binomial())$fitted.values)$auc)
me = mean(unlist(coeforiginalf.auc$analyses))
m = 15
Vb = var(unlist(coeforiginalf.auc$analyses))
Vw0 <- NA
for(i in 1:15){
  Vw0[[i]] <- (coeforiginalf.auc$analyses[[i]]-me)^2
}
Vw = (1/(m-1))*sum(Vw0)
SEp = sqrt(sum(Vb, Vw, Vb/m))
t = 1.98
l95ci = me - t*SEp
h95ci = me + t*SEp
me; l95ci; h95ci #0.82 (0.81-0.82)

coefnewf.roc <- with(imputed.newf, roc(xpert~ glm(xpert~symp_fac___1cat+symp_fac___2cat+symp_fac___3cat+symp_fac___4cat+n_tbsymp_cat1+symp_2wks+agecat2+sexcat+hivcat1+dbcat+n_other_sympcat+edu8+pasttb+eversmoke+lungcat, family=binomial())$fitted.values, plot=T))
coefnewf.auc <- with(imputed.newf, roc(xpert~ glm(xpert~symp_fac___1cat+symp_fac___2cat+symp_fac___3cat+symp_fac___4cat+n_tbsymp_cat1+symp_2wks+agecat2+sexcat+hivcat1+dbcat+n_other_sympcat+edu8+pasttb+eversmoke+lungcat, family=binomial())$fitted.values)$auc)
me = mean(unlist(coefnewf.auc$analyses))
m = 5
Vb = var(unlist(coefnewf.auc$analyses))
Vw0 <- NA
for(i in 1:5){
  Vw0[[i]] <- (coefnewf.auc$analyses[[i]]-me)^2
}
Vw = (1/(m-1))*sum(Vw0)
SEp = sqrt(sum(Vb, Vw, Vb/m))
t = 1.98
l95ci = me - t*SEp
h95ci = me + t*SEp
me; l95ci; h95ci #0.84 (0.83-0.84)


# whole number scoring
miroundoriginal.roc <- with(imputed.original, roc(xpert~ round(glm(xpert~n_tbsymp+symp_2wks+agecat2+sexcat+hivcat1+dbcat+n_other_sympcat, family=binomial())$fitted.values, digits=1), plot=T)) #ci=T
miroundoriginal.auc <- with(imputed.original, roc(xpert~ round(glm(xpert~n_tbsymp+symp_2wks+agecat2+sexcat+hivcat1+dbcat+n_other_sympcat, family=binomial())$fitted.values, digits=1))$auc)
me = mean(unlist(miroundoriginal.auc$analyses))
m = 15
Vb = var(unlist(miroundoriginal.auc$analyses))
Vw0 <- NA
for(i in 1:15){
  Vw0[[i]] <- (miroundoriginal.auc$analyses[[i]]-me)^2
}
Vw = (1/(m-1))*sum(Vw0)
SEp = sqrt(sum(Vb, Vw, Vb/m))
t = 1.98
l95ci = me - t*SEp
h95ci = me + t*SEp
me; l95ci; h95ci #0.82 (0.81-0.82)

miroundnew.roc <- with(imputed.new, roc(xpert~ round(glm(xpert~agecat2+sexcat+hivcat1+dbcat+symp_fac___1cat +symp_fac___2cat +symp_fac___3cat +symp_fac___4cat +n_tbsymp_cat1+n_other_sympcat+symp_2wks, family=binomial())$fitted.values, digits=1), plot=T))
miroundnew.auc <- with(imputed.new, roc(xpert~ round(glm(xpert~agecat2+sexcat+hivcat1+dbcat+symp_fac___1cat +symp_fac___2cat +symp_fac___3cat +symp_fac___4cat +n_tbsymp_cat1+n_other_sympcat+symp_2wks, family=binomial())$fitted.values, digits=1))$auc)
me = mean(unlist(miroundnew.auc$analyses))
m = 15
Vb = var(unlist(miroundnew.auc$analyses))
Vw0 <- NA
for(i in 1:15){
  Vw0[[i]] <- (miroundnew.auc$analyses[[i]]-me)^2
}
Vw = (1/(m-1))*sum(Vw0)
SEp = sqrt(sum(Vb, Vw, Vb/m))
t = 1.98
l95ci = me - t*SEp
h95ci = me + t*SEp
me; l95ci; h95ci #0.83 (0.83-0.84)

miroundoriginalf.roc <- with(imputed.originalf, roc(xpert~ round(glm(xpert~n_tbsymp+symp_2wks+agecat2+sexcat+hivcat1+dbcat+n_other_sympcat+edu8+pasttb+eversmoke+lungcat, family=binomial())$fitted.values, digits=1), plot=T)) #ci=T
miroundoriginalf.auc <- with(imputed.originalf, roc(xpert~ round(glm(xpert~n_tbsymp+symp_2wks+agecat2+sexcat+hivcat1+dbcat+n_other_sympcat+edu8+pasttb+eversmoke+lungcat, family=binomial())$fitted.values, digits=1))$auc)
me = mean(unlist(miroundoriginalf.auc$analyses))
m = 15
Vb = var(unlist(miroundoriginalf.auc$analyses))
Vw0 <- NA
for(i in 1:15){
  Vw0[[i]] <- (miroundoriginalf.auc$analyses[[i]]-me)^2
}
Vw = (1/(m-1))*sum(Vw0)
SEp = sqrt(sum(Vb, Vw, Vb/m))
t = 1.98
l95ci = me - t*SEp
h95ci = me + t*SEp
me; l95ci; h95ci #0.81 (0.81-0.82)

miroundnewf.roc <- with(imputed.newf, roc(xpert~ round(glm(xpert~symp_fac___1cat+symp_fac___2cat+symp_fac___3cat+symp_fac___4cat+n_tbsymp_cat1+symp_2wks+agecat2+sexcat+hivcat1+dbcat+n_other_sympcat+edu8+pasttb+eversmoke+lungcat, family=binomial())$fitted.values, digits=1), plot=T))
miroundnewf.auc <- with(imputed.newf, roc(xpert~ round(glm(xpert~symp_fac___1cat+symp_fac___2cat+symp_fac___3cat+symp_fac___4cat+n_tbsymp_cat1+symp_2wks+agecat2+sexcat+hivcat1+dbcat+n_other_sympcat+edu8+pasttb+eversmoke+lungcat, family=binomial())$fitted.values, digits=1))$auc)
me = mean(unlist(miroundnewf.auc$analyses))
m = 5
Vb = var(unlist(miroundnewf.auc$analyses))
Vw0 <- NA
for(i in 1:5){
  Vw0[[i]] <- (miroundnewf.auc$analyses[[i]]-me)^2
}
Vw = (1/(m-1))*sum(Vw0)
SEp = sqrt(sum(Vb, Vw, Vb/m))
t = 1.98
l95ci = me - t*SEp
h95ci = me + t*SEp
me; l95ci; h95ci #0.83 (0.83-0.84)






# ### run a model
# ## final model with a single score
# #original
# 
# fit.s <- with(imputed, (glm(xpert~simpleoriginal, family=binomial())))
# p <- summary(pool(fit.s))
# fit.s.r <- with(imputed, lroc(p))
# 
# #actual symptom
# fit.n <- with(imputed1, logistic.display(glm(xpert~as.numeric(simpleoriginalnew), family=binomial())))
# fit.n.r <- with(imputed1, lroc(glm(xpert~as.numeric(simpleoriginalnew), family=binomial())))
# 
# fit.n.r.roc <- with(imputed1, roc(xpert~ glm(xpert~as.numeric(simpleoriginalnew), family=binomial())$fitted.values, ci=T))
# fit.n.r.auc <- with(imputed1, roc(xpert~ glm(xpert~as.numeric(simpleoriginalnew), family=binomial())$fitted.values)$auc)
# fit.n.r.ci <- with(imputed1, roc(xpert~ glm(xpert~as.numeric(simpleoriginalnew), family=binomial())$fitted.values, ci=T)$ci)
# with(imputed, tapply(xpert, simpleoriginal, var))
# mean(unlist(fit.n.r.auc$analyses)) - me
# SE = sqrt(Vb + Vw + Vb/m)
# m = 3
# Vb = var(unlist(fit.n.r.auc$analyses))
# for(i in 1:3){
#   Vw[[i]] <- (fit.n.r.auc$analyses[[i]]-mean(unlist(fit.n.r.auc$analyses)))^2
# }
# Vw1 = (1/(m-1))*sum(Vw)
# Vw0 = (1/(m-1))*((mean(unlist(fit.n.r.auc$analyses)) - fit.n.r.auc$analyses[[1]])^2 + (mean(unlist(fit.n.r.auc$analyses)) - fit.n.r.auc$analyses[[2]])^2 + (mean(unlist(fit.n.r.auc$analyses)) - fit.n.r.auc$analyses[[3]])^2)
# fit.n.r.ci$analyses[[1]]
# me = qnorm(.975)*SE/sqrt(m)
# 
# ##final model with coefficients
# #original
# fit.c <- with(imputed, (glm(xpert~ n_tbsymp+symp_2wks+agecat2+sexcat+hivcat1+dbcat+n_other_sympcat, family=binomial())))
# fit.c.r <- with(imputed, lroc(glm(xpert~ n_tbsymp+symp_2wks+agecat2+sexcat+hivcat1+dbcat+n_other_sympcat, family=binomial())))
# summary(pool(fit.c.r))
# p_fit.c <- summary(pool(fit.c))
# linearMod <- glm(xpert~ simpleoriginal, family=binomial(), data=newfd)
# logistic.display(glm(xpert~ simpleoriginal, family=binomial(), data=newfd))
# print(linearMod)
# summary(linearMod)
# 
# aucs <- with(imputed, lroc(glm(xpert~ n_tbsymp+symp_2wks+agecat2+sexcat+hivcat1+dbcat+n_other_sympcat, family=binomial()))$auc)
# mean(unlist(aucs$analyses))
# sd(unlist(aucs$analyses))
# var(unlist(aucs$analyses))
# summary(lroc(glm(xpert~ n_tbsymp+symp_2wks+agecat2+sexcat+hivcat1+dbcat+n_other_sympcat, family=binomial(), data=newfd)))
# 
# #actual symptom
# fit.cn <- with(imputed1, logistic.display(glm(xpert~ symp_fac___1cat+symp_fac___2cat+symp_fac___3cat+symp_fac___4cat+n_tbsymp_cat1+symp_2wks+agecat2+sexcat+hivcat1+dbcat+n_other_sympcat, family=binomial())))
# fit.cn.r <- with(imputed1, lroc(glm(xpert~ symp_fac___1cat+symp_fac___2cat+symp_fac___3cat+symp_fac___4cat+n_tbsymp_cat1+symp_2wks+agecat2+sexcat+hivcat1+dbcat+n_other_sympcat, family=binomial())))
# 
# ## full model
# fit.fso <- with(imputed3, lroc(glm(xpert~simpleoriginalf, family=binomial())))
# fit.fsn <- with(imputed4, lroc(glm(xpert~simpleoriginalnewf, family=binomial())))
# fit.fco <- with(imputed1, lroc(glm(xpert~n_tbsymp+symp_2wks+agecat2+sexcat+hivcat1+dbcat+n_other_sympcat+edu8+pasttb+eversmoke, family=binomial())))
# fit.fcn <- with(imputed1, lroc(glm(xpert~symp_fac___1cat+symp_fac___2cat+symp_fac___3cat+symp_fac___4cat+n_tbsymp_cat1+symp_2wks+agecat2+sexcat+hivcat1+dbcat+n_other_sympcat+edu8+pasttb+eversmoke, family=binomial())))
# 
# 
# # fitroc <- with(imputed, roc(as.numeric(newfd1$xpert)~as.numeric(newfd1$simpleoriginal), ci=T, plot=T))
# # library(olsrr)
# # fit <- with(imputed, lm(as.numeric(xpert) ~ n_tbsymp+symp_2wks+agecat2+sexcat+hivcat1+dbcat+ 
# #                         symp_fac___1cat+symp_fac___2cat+symp_fac___3cat+symp_fac___4cat+
# #                         n_other_sympcat+edu8+pasttb+eversmoke))
# # ols_step_best_subset(fit)
# 
# # run ROC
# fitroc <- with(imputed, roc(as.numeric(xpert)~as.numeric(simpleoriginal), ci=T, plot=T, col=1))
# fitroc.f <- 
# 
# # run DCA
# dca<-decision_curve(xpert~simpleoriginal, data=newfd, family=binomial, fitted.risk = FALSE,
#                thresholds = seq(0, 1, by = 0.01), study.design = 'case-control', confidence.intervals = 0.95, bootstraps = 500)
# plot_decision_curve(list(dca), curve.names = ("Kharitode no imputation"),
#                     col = ("red"), 
#                     confidence.intervals = TRUE, cost.benefit.axis = TRUE, legend.position = "topright") 
# 
# midca <- with(imputed.original, decision_curve(as.numeric(xpert)~as.numeric(simpleoriginal), family=binomial, fitted.risk = FALSE,
#                                        thresholds = seq(0, 1, by = 0.01), study.design = 'case-control', confidence.intervals = 0.95, bootstraps = 500))
# # summary(pool(log(fitroc)))
# # 
# # library("psfmi")
# plot(fitdca)
# plot_decision_curve(list(fitdca), curve.names = ("South Africa population"),
#                     col = ("blue"), 
#                     confidence.intervals = TRUE, cost.benefit.axis = TRUE, legend.position = "topright") 

