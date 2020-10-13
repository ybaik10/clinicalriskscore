######################################################################################################################################################################################################
################################### Xpert TB case definition #########################################################################################################################################
st.xp <- read.csv('/Users/ys/Documents/Stomp/Stomp_finaldata_Xpertcasedefinition_fromR_10202019.csv', header = TRUE)
st.xp %>% count(xpertcc)

## age variable management ##
st.xp$agecat5[st.xp$agecat=="(54,68]"|st.xp$agecat=="(14,24]"]<-0
st.xp$agecat5[st.xp$agecat=="(44,54]"]<-1
st.xp$agecat5[st.xp$agecat=="(24,34]"|st.xp$agecat=="(34,44]"]<-2

## random sampling ##
case <- st.xp[which(st.xp$xpertcc==1),]
cont <- st.xp[which(st.xp$xpertcc==0),]
case.rs <- case[sample(nrow(case), 68),]
cont.match <- cont[((cont$id3_cc %in% case.rs$id3_cc)|(cont$id3_com %in% case.rs$id3_com)),] 
st.xp.rs <- rbind(case.rs, cont.match) #Used in building a model

case.rs.exc <- case[!(case$record_id %in% case.rs$record_id),] 
cont.match.exc <- cont[!(cont$record_id %in%  cont.match$record_id),] 
st.xp.rs.exc <- rbind(case.rs.exc, cont.match.exc) #Used in internal validation

## random samples SHOULD be saved to replicate the numbers ##
write.csv(st.xp.rs, "Sample_xpert_model.csv")
write.csv(st.xp.rs.exc, "Sample_xpert_valid.csv")
st.xp.1 <- rbind(st.xp.rs, st.xp.rs.exc)

## 10% prevalence scenario ##
st.xp.rs.10 <- rbind(st.xp.rs, st.xp.rs %>% filter(xpertcc==0), st.xp.rs %>% filter(xpertcc==0), st.xp.rs %>% filter(xpertcc==0), st.xp.rs %>% filter(xpertcc==0))
st.xp.rs.exc.10 <- rbind(st.xp.rs.exc, st.xp.rs.exc %>% filter(xpertcc==0), st.xp.rs.exc %>% filter(xpertcc==0), st.xp.rs.exc %>% filter(xpertcc==0), st.xp.rs.exc %>% filter(xpertcc==0))
st.xp.10 <- rbind(st.xp, st.xp %>% filter(xpertcc==0), st.xp %>% filter(xpertcc==0), st.xp %>% filter(xpertcc==0), st.xp %>% filter(xpertcc==0))

################################### All TB case definition ########################################################################################################################
st.all <- read.csv('/Users/ys/Documents/Stomp/Stomp_finaldata_Allcasedefinition_fromR_10202019.csv', header = TRUE)
st.all %>% count(casepr)

## random sampling already done ##
dt.rs <- read.csv("/Users/ys/Documents/Stomp/Sample.pop.model.csv", header = TRUE) 
dt.rs.exc <- read.csv("/Users/ys/Documents/Stomp/Sample.pop.valid.csv", header = TRUE) 
dt.rs %>% count(casepr)
dt.rs.exc %>% count(casepr)

## age variable management ##
dt.rs$agecat5[dt.rs$agecat=="(54,68]"|dt.rs$agecat=="(14,24]"]<-0
dt.rs$agecat5[dt.rs$agecat=="(44,54]"]<-1
dt.rs$agecat5[dt.rs$agecat=="(24,34]"|dt.rs$agecat=="(34,44]"]<-2

dt.rs$agecat6[dt.rs$agecat=="(44,54]"|dt.rs$agecat=="(54,68]"]<-0
dt.rs$agecat6[dt.rs$agecat=="(14,24]"]<-1
dt.rs$agecat6[dt.rs$agecat=="(24,34]"]<-2
dt.rs$agecat6[dt.rs$agecat=="(34,44]"]<-3

dt.rs.exc$agecat5[dt.rs.exc$agecat=="(54,68]"|dt.rs.exc$agecat=="(14,24]"]<-0
dt.rs.exc$agecat5[dt.rs.exc$agecat=="(44,54]"]<-1
dt.rs.exc$agecat5[dt.rs.exc$agecat=="(24,34]"|dt.rs.exc$agecat=="(34,44]"]<-2

dt.rs.exc$agecat6[dt.rs.exc$agecat=="(44,54]"|dt.rs.exc$agecat=="(54,68]"]<-0
dt.rs.exc$agecat6[dt.rs.exc$agecat=="(14,24]"]<-1
dt.rs.exc$agecat6[dt.rs.exc$agecat=="(24,34]"]<-2
dt.rs.exc$agecat6[dt.rs.exc$agecat=="(34,44]"]<-3

## 10% prevalence scenario ##
dt.rs10<-rbind(dt.rs, dt.rs %>% filter(casepr==0), dt.rs %>% filter(casepr==0), dt.rs %>% filter(casepr==0), dt.rs %>% filter(casepr==0))
dt.rs.exc10<-rbind(dt.rs.exc, dt.rs.exc %>% filter(casepr==0), dt.rs.exc %>% filter(casepr==0), dt.rs.exc %>% filter(casepr==0), dt.rs.exc %>% filter(casepr==0))

###########################STOMP de novo external validation on Kharitode multiple imputation dataset###############################
newfd <- read.csv("/Users/ys/kharitode/newfd.csv", header=T)
newfd$simple.external <- newfd$agecat2 + newfd$sexcat + newfd$hivcat1 + newfd$n_tbsymp + newfd$symp_2wks + newfd$eversmoke
newfd.simple.external <- newfd %>% dplyr::select(simple.external, n_tbsymp, symp_2wks, agecat2, sexcat, hivcat1, dbcat, symp_fac___1cat, symp_fac___2cat, symp_fac___3cat, symp_fac___4cat, n_other_sympcat, edu8, pasttb, eversmoke, lungcat, xpert)
newfd.simple.external[] <- lapply(newfd.simple.external, function(x){return(as.factor(x))})
imputed.simple.external<- mice(newfd.simple.external, m=15, maxit=10, method=c(rep('pmm',2),'logreg','polyreg',rep('logreg',13)))
summary(imputed.simple.external)

newfd.10 <- rbind(newfd, newfd%>%filter(xpert==0), newfd%>%filter(xpert==0), newfd%>%filter(xpert==0), newfd%>%filter(xpert==0), newfd%>%filter(xpert==0), newfd%>%filter(xpert==0), newfd%>%filter(xpert==0))
newfd10.simple.external <- newfd.10 %>% dplyr::select(simple.external, n_tbsymp, symp_2wks, agecat2, sexcat, hivcat1, dbcat, symp_fac___1cat, symp_fac___2cat, symp_fac___3cat, symp_fac___4cat, n_other_sympcat, edu8, pasttb, eversmoke, lungcat, xpert)
newfd10.simple.external[] <- lapply(newfd10.simple.external, function(x){return(as.factor(x))})
imputed10.simple.external<- mice(newfd10.simple.external, m=15, maxit=10, method=c(rep('pmm',2),'logreg','polyreg',rep('logreg',13)))
summary(imputed10.simple.external)
######################################################################################################################################################################################################
######################################################################################################################################################################################################


#### Make a risk score
### Use 'all' case definition
## A simple risk score
# model-derived
dt.rs$simple <- dt.rs$agecat5 + dt.rs$sex_female1 + dt.rs$hivarv + dt.rs$n_tb_symps + dt.rs$symps_weeks_cat2 + dt.rs$eversmoke  + dt.rs$coughsint_1
# internal validation
dt.rs.exc$simple <- dt.rs.exc$agecat5 + dt.rs.exc$sex_female1 + dt.rs.exc$hivarv + dt.rs.exc$n_tb_symps + dt.rs.exc$symps_weeks_cat2 + dt.rs.exc$eversmoke  + dt.rs.exc$coughsint_1
# 10% model-derived
dt.rs10$simple <- dt.rs10$agecat5 + dt.rs10$sex_female1 + dt.rs10$hivarv + dt.rs10$n_tb_symps + dt.rs10$symps_weeks_cat2 + dt.rs10$eversmoke  + dt.rs10$coughsint_1
# 10% internal validation
dt.rs.exc10$simple <- dt.rs.exc10$agecat5 + dt.rs.exc10$sex_female1 + dt.rs.exc10$hivarv + dt.rs.exc10$n_tb_symps + dt.rs.exc10$symps_weeks_cat2 + dt.rs.exc10$eversmoke  + dt.rs.exc10$coughsint_1

## A coefficient risk score
glm(casepr ~ as.factor(agecat) + sex_female1 + hivarv + n_tb_symps + symps_weeks_cat2 + eversmoke  + coughsint_1, family=binomial(link="logit"), data=dt.rs)
exp<-glm(casepr ~ as.factor(agecat6) + sex_female1 + hivarv + n_tb_symps + symps_weeks_cat2 + eversmoke  + coughsint_1, family=binomial(link="logit"), data=dt.rs)
exp.c<-glm(casepr ~ as.factor(agecat6) + sex_female1 + hivarv + n_tb_symps + symps_weeks_cat2 + eversmoke  + coughsint_1, family=binomial(), data=dt.rs.exc)


# model-derived
derived.coeff <- glm(casepr ~ as.factor(agecat5) + sex_female1 + hivarv + n_tb_symps + symps_weeks_cat2 + eversmoke  + coughsint_1, family=binomial(link="logit"), data=dt.rs)
# internal validation
internal.coeff <- glm(casepr ~ agecat5 + sex_female1 + hivarv + n_tb_symps + symps_weeks_cat2 + eversmoke  + coughsint_1, family=binomial(), data=dt.rs.exc)
# 10% model-derived
derived10.coeff <- glm(casepr ~ dt.rs10$agecat5 + dt.rs10$sex_female1 + dt.rs10$hivarv + dt.rs10$n_tb_symps + dt.rs10$symps_weeks_cat2 + dt.rs10$eversmoke  + dt.rs10$coughsint_1, family=binomial, data=dt.rs10)
# 10% internal validation
internal10.coeff <- glm(casepr ~ dt.rs.exc10$agecat5 + dt.rs.exc10$sex_female1 + dt.rs.exc10$hivarv + dt.rs.exc10$n_tb_symps + dt.rs.exc10$symps_weeks_cat2 + dt.rs.exc10$eversmoke  + dt.rs.exc10$coughsint_1, family=binomial, data=dt.rs.exc10)

### Use TB case definition
## A simple risk score
# model-derived
st.xp.rs$simple <- st.xp.rs$agecat5 + st.xp.rs$sex_female1 + st.xp.rs$hivarv + st.xp.rs$n_tb_symps + st.xp.rs$symps_weeks_cat2 + st.xp.rs$eversmoke  + st.xp.rs$coughsint_1
# internal validation
st.xp.rs.exc$simple <- st.xp.rs.exc$agecat5 + st.xp.rs.exc$sex_female1 + st.xp.rs.exc$hivarv + st.xp.rs.exc$n_tb_symps + st.xp.rs.exc$symps_weeks_cat2 + st.xp.rs.exc$eversmoke  + st.xp.rs.exc$coughsint_1
# overall
st.xp$simple <- st.xp$agecat5 + st.xp$sex_female1 + st.xp$hivarv + st.xp$n_tb_symps + st.xp$symps_weeks_cat2 + st.xp$eversmoke  + st.xp$coughsint_1

# 10% model-derived
st.xp.rs.10$simple <- st.xp.rs.10$agecat5 + st.xp.rs.10$sex_female1 + st.xp.rs.10$hivarv + st.xp.rs.10$n_tb_symps + st.xp.rs.10$symps_weeks_cat2 + st.xp.rs.10$eversmoke  + st.xp.rs.10$coughsint_1
# 10% internal validation
st.xp.rs.exc.10$simple <- st.xp.rs.exc.10$agecat5 + st.xp.rs.exc.10$sex_female1 + st.xp.rs.exc.10$hivarv + st.xp.rs.exc.10$n_tb_symps + st.xp.rs.exc.10$symps_weeks_cat2 + st.xp.rs.exc.10$eversmoke  + st.xp.rs.exc.10$coughsint_1
# 10% overall


## A coefficient risk score
# model-derived
xderived.coeff <- glm(xpertcc ~ agecat5 + sex_female1 + hivarv + n_tb_symps + symps_weeks_cat2 + eversmoke  + coughsint_1, family=binomial(link="logit"), data=st.xp.rs)
# internal validation
xinternal.coeff <- glm(xpertcc ~ agecat5 + sex_female1 + hivarv + n_tb_symps + symps_weeks_cat2 + eversmoke  + coughsint_1, family=binomial(), data=st.xp.rs.exc)
# overall
xoverall.coeff <- glm(xpertcc ~ agecat5 + sex_female1 + hivarv + n_tb_symps + symps_weeks_cat2 + eversmoke  + coughsint_1, family=binomial(), data=st.xp.1)
# 10% model-derived
xderived10.coeff <- glm(xpertcc ~ agecat5 + sex_female1 + hivarv + n_tb_symps + symps_weeks_cat2 + eversmoke  + coughsint_1, family=binomial, data=st.xp.rs.10)
# 10% internal validation
xinternal10.coeff <- glm(xpertcc ~ agecat5 + sex_female1 + hivarv + n_tb_symps + symps_weeks_cat2 + eversmoke  + coughsint_1, family=binomial, data=st.xp.rs.exc.10)
# 10% overall
xoverall10.coeff <- glm(xpertcc ~ agecat5 + sex_female1 + hivarv + n_tb_symps + symps_weeks_cat2 + eversmoke  + coughsint_1, family=binomial(), data=st.xp.10)





###### ROC ######
par("mar")
par(mar=c(1,1,1,1))
# par(pty='s')

### All case & Xpert case definition, simple scoring own data (derivation, internal, and external)
original.roc <- with(imputed.simple.external, roc(xpert~ glm(xpert~as.numeric(simple.external), family=binomial())$fitted.values, plot=T, col=4, main="Discrimination in a simple scoring system \nwith the original data", bty='n'))
###################################### Kharitode multiple imputation ROC mean and 95%CI ######################################
# original.auc <- with(imputed.simple.external, roc(xpert~ glm(xpert~as.numeric(simple.external), family=binomial())$fitted.values)$auc)
# me = mean(unlist(original.auc$analyses))
# m = 15
# Vb = var(unlist(original.auc$analyses))
# Vw0 <- NA
# for(i in 1:15){
#   Vw0[[i]] <- (original.auc$analyses[[i]]-me)^2
# }
# Vw = (1/(m-1))*sum(Vw0)
# SEp = sqrt(sum(Vb, Vw, Vb/m))
# t = 2.021
# # lambda = sum(Vb, Vb/m)/sum(Vb, Vw, Vb/m)
# # dfo = (m-1)/(lambda)^2
# # dfb = ((1634-15)+1)/((1634-15)+3)*(1634-15)*(1-lambda)
# # dfa = dfo*dfb/(dfo+dfb)
# # l95ci = me - qnorm(.975)*SEp/sqrt(m)
# # h95ci = me + qnorm(.975)*SEp/sqrt(m)
# l95ci = me - t*SEp
# h95ci = me + t*SEp
# me; l95ci; h95ci
##################################################################################################################
roc(dt.rs$casepr ~ dt.rs$simple, data=dt.rs, ci=T, plot=T, col=1, lty=1, add=T)
roc(casepr ~ simple, data=dt.rs.exc, ci=T, plot=T, col=2, lty=1, add=T)
# DON'T NEED TO SEPARATE XPERTCC because model was not derived by Xpert case definition
# roc(st.xp.rs$xpertcc ~ st.xp.rs$simple, ci=T, plot=T, col=1, lty=2, add=T)
# roc(xpertcc ~ simple, data=st.xp.rs.exc, ci=T, plot=T, col=2, lty=2, add=T)
roc(st.xp.1$xpertcc ~ st.xp.1$simple, ci=T, plot=T, col=3, lty=1, add=T)
# legend("bottomright",
#        legend=c("Derivation population (All), \nc-statistic=0.793 (0.737-0.849)", "Internal validation (All), \nc-statistic=0.812 (0.730-0.893)",
#                 "Derivation population (Xpert), \nc-statistic=0.766 (0.694-0.839)", "Internal validation (Xpert), \nc-statistic=0.852 (0.773-0.931)",
#                 "External validation, \nc-statistic =0.781 (0.776-0.785)"), 
#        col=c(1,2,1,2,4), lty=c(1,1,2,2,1), cex=0.7, box.lty=0, bty = "n")
legend("right",
       legend=c("Derivation population, \nc-statistic=0.793 (0.737-0.849)\n", "Internal validation, \nc-statistic=0.812 (0.730-0.893)\n",
                "Xpert case definition, \nc-statistic=0.795 (0.740-0.850)\n", "External validation, \nc-statistic =0.781 (0.776-0.785)\n"), 
       col=c(1,2,3,4), lty=c(1,1,1,1), cex=0.7, box.lty=0, bty = "n")





### All case & Xpert case definition, coeff scoring own data (derivation, internal, and external)coeforiginal.roc <- with(imputed.original, roc(xpert~ glm(xpert~n_tbsymp+symp_2wks+agecat2+sexcat+hivcat1+dbcat+n_other_sympcat, family=binomial())$fitted.values, plot=T)) #ci=T
coeforiginal.roc <- with(imputed.simple.external, roc(xpert~ glm(xpert~n_tbsymp+symp_2wks+agecat2+sexcat+hivcat1+dbcat+n_other_sympcat, family=binomial())$fitted.values, plot=T, col=4, main="Discrimination in a coefficient scoring system \nwith the original data", bty='n')) #ci=T
###################################### Kharitode multiple imputation ROC mean and 95%CI ######################################
# coeforiginal.auc <- with(imputed.simple.external, roc(xpert~ glm(xpert~n_tbsymp+symp_2wks+agecat2+sexcat+hivcat1+dbcat+n_other_sympcat, family=binomial())$fitted.values)$auc)
# me = mean(unlist(coeforiginal.auc$analyses))
# m = 15
# Vb = var(unlist(coeforiginal.auc$analyses))
# Vw0 <- NA
# for(i in 1:15){
#   Vw0[[i]] <- (coeforiginal.auc$analyses[[i]]-me)^2
# }
# Vw = (1/(m-1))*sum(Vw0)
# SEp = sqrt(sum(Vb, Vw, Vb/m))
# t = 2.021
# # lambda = sum(Vb, Vb/m)/sum(Vb, Vw, Vb/m)
# # dfo = (m-1)/(lambda)^2
# # dfb = ((1634-15)+1)/((1634-15)+3)*(1634-15)*(1-lambda)
# # dfa = dfo*dfb/(dfo+dfb)
# l95ci = me - t*SEp
# h95ci = me + t*SEp
# me; l95ci; h95ci
##################################################################################################################
roc(casepr ~ derived.coeff$fitted.values, data=dt.rs, ci=T, plot=T, col=1, lty=1, add=T)
roc(dt.rs.exc[-c(39),]$casepr ~ internal.coeff$fitted.values, data=dt.rs.exc, ci=T, plot=T, col=2, lty=1, add=T)
# roc(xpertcc ~ xderived.coeff$fitted.values, data=st.xp.rs, ci=T, plot=T, col=1, lty=2, add=T)
# roc(xpertcc ~ xinternal.coeff$fitted.values, data=st.xp.rs.exc, ci=T, plot=T, col=2, lty=2, add=T)
roc(st.xp.1[-c(224),]$xpertcc ~ xoverall.coeff$fitted.values, ci=T, plot=T, col=3, lty=1, add=T)
legend("right",
       legend=c("Derivation population, \nc-statistic=0.827 (0.777-0.877)\n", "Internal validation, \nc-statistic=0.837 (0.755-0.920)\n",
                "Xpert case definition, \nc-statistic=0.825 (0.773-0.876)\n", "External validation, \nc-statistic =0.818 (0.814-0.823)\n"), 
       col=c(1,2,3,4), lty=c(1,1,1,1), cex=0.7, box.lty=0, bty = "n")


roc(casepr ~ exp$fitted.values, data=dt.rs, ci=T)
roc(dt.rs.exc[-c(39),]$casepr ~ exp.c$fitted.values, data=dt.rs.exc, ci=T)
roc(dt.rs.exc[-c(39),]$casepr ~ internal.coeff$fitted.values, data=dt.rs.exc, ci=T)


### All case & Xpert case definition, simple scoring 10% prevalence (derivation, internal, and external)
original10.roc <- with(imputed10.simple.external, roc(xpert~ glm(xpert~as.numeric(simple.external), family=binomial())$fitted.values, plot=T, col=4, main="Discrimination in a simple scoring system \nunder 10% TB prevalence", bty='n'))
# original10.auc <- with(imputed10.simple.external, roc(xpert~ glm(xpert~as.numeric(simple.external), family=binomial())$fitted.values)$auc)
# mean(unlist(original10.auc$analyses))
roc(dt.rs10$casepr ~ dt.rs10$simple, data=dt.rs10, plot=T, col=1, lty=1, add=T)
roc(casepr ~ simple, data=dt.rs.exc10, plot=T, col=2, lty=1, add=T)
roc(st.xp.10$xpertcc ~ st.xp.10$simple, plot=T, col=3, lty=1, add=T)
legend("right",
       legend=c("Derivation population, c-statistic=0.793", "Internal validation, c-statistic=0.812",
                "Xpert case definition, c-statistic=0.795", "External validation, c-statistic =0.780"), 
       col=c(1,2,3,4), lty=c(1,1,1,1), cex=0.7, box.lty=0, bty = "n")




### All case & Xpert case definition, coeff scoring 10% prevalence (derivation, internal, and external)coeforiginal.roc <- with(imputed.original, roc(xpert~ glm(xpert~n_tbsymp+symp_2wks+agecat2+sexcat+hivcat1+dbcat+n_other_sympcat, family=binomial())$fitted.values, plot=T)) #ci=T
coeforiginal10.roc <- with(imputed10.simple.external, roc(xpert~ glm(xpert~n_tbsymp+symp_2wks+agecat2+sexcat+hivcat1+dbcat+n_other_sympcat, family=binomial())$fitted.values, plot=T, col=4, main="Discrimination in a coefficient scoring system \nunder 10% TB prevalence", bty='n')) #ci=T
# coeforiginal10.auc <- with(imputed10.simple.external, roc(xpert~ glm(xpert~n_tbsymp+symp_2wks+agecat2+sexcat+hivcat1+dbcat+n_other_sympcat, family=binomial())$fitted.values)$auc)
# mean(unlist(coeforiginal10.auc$analyses))
roc(casepr ~ derived10.coeff$fitted.values, data=dt.rs10, plot=T, col=1, lty=1, add=T)
roc(dt.rs.exc10[-c(39),]$casepr ~ internal10.coeff$fitted.values, data=dt.rs.exc10, plot=T, col=2, lty=1, add=T)
roc(st.xp.10[-c(224),]$xpertcc ~ xoverall10.coeff$fitted.values, plot=T, col=3, lty=1, add=T)
legend("right",
       legend=c("Derivation population, c-statistic=0.827", "Internal validation, c-statistic=0.836",
                "Xpert case definition, c-statistic=0.828", "External validation, c-statistic =0.817"), 
       col=c(1,2,3,4), lty=c(1,1,1,1), cex=0.7, box.lty=0, bty = "n")





###### Calibration ######
st.xp.1$xpert <- st.xp.1$xpertcc
st.xp.1$simple.external <- st.xp.1$simple

st.xp.10$xpert <- st.xp.10$xpertcc
st.xp.10$simple.external <- st.xp.10$simple

# simple scoring
fit <- glm(xpert ~ simple.external, family=binomial(logit), data=st.xp.1)
fit10 <- glm(xpert ~ simple.external, family=binomial(logit), data=st.xp.10)

# example -- repeat in each dataset and compute mean and variance and plot manually
data1 <- mice::complete(imputed.simple.external,1); data1$simple.external <- as.numeric(as.character(data1$simple.external))
prob1 <- fit %>% predict(data1, type="response")
prob1.1 <- prob1[!is.na(prob1)]
prob1.2 <- prob1.1*(289/102)*(772/1634)
prob1.3 <- ifelse(prob1.2>=1, 0.999, prob1.2)
prob1.4 <- ifelse(prob1.2>=1, NA, prob1.2)
prob1.5 <- prob1.4[!is.na(prob1.4)]
data1$prob1.5 <- prob1.4
val1 <- val.prob.ci.2(prob1.5, as.numeric(as.character(data1[complete.cases(data1$prob1.5),]$xpert)), logit, logistic.cal = T)
val1<- val1[c(12,13)]

data101 <- mice::complete(imputed10.simple.external,1); data101$simple.external <- as.numeric(as.character(data101$simple.external))
prob101 <- fit10 %>% predict(data101, type="response")
prob101.1 <- prob101[!is.na(prob101)]
val101 <- val.prob.ci.2(prob101.1, as.numeric(as.character(data101$xpert)), logit, logistic.cal = T)
val101<- val101[c(12,13)]

# repeat in each dataset and compute mean and variance and plot manually
datax <- mice::complete(imputed.simple.external,15); datax$simple.external <- as.numeric(as.character(datax$simple.external))
probx <- fit %>% predict(datax, type="response")
probx.1 <- probx[!is.na(probx)]
probx.2 <- probx.1*(289/102)*(772/1634)
probx.3 <- ifelse(probx.2>=1, 0.999, probx.2)
valx <- val.prob.ci.2(probx.3, as.numeric(as.character(datax$xpert)), plot=F, logit, logistic.cal = T)
val15<-valx[c(12,13)]

valrbind <- as.data.frame(rbind(val1,val2,val3,val4,val5,val6,val7,val8,val9,val10,val11,val12,val13,val14,val15))
# m=15
# me=mean(valrbind$Intercept)
# vb=var(valrbind$Intercept)
# vw=(valrbind$Intercept-mean(valrbind$Intercept))^2 * (1/(m-1))
# SEp = sqrt(sum(vb, vw, vb/m))
# # # lambda = sum(vb, vb/m)/sum(vb, vw, vb/m)
# # # dfo = (m-1)/(lambda)^2
# # # dfb = ((1634-15)+1)/((1634-15)+3)*(1634-15)*(1-lambda)
# # # dfa = dfo*dfb/(dfo+dfb)
# t = 2.021
# l95ci = me - t*SEp
# h95ci = me + t*SEp
me; l95ci; h95ci # 0.68 (0.66 - 0.71)

# m=15
# me=mean(valrbind$Slope)
# vb=var(valrbind$Slope)
# vw=(valrbind$Slope-mean(valrbind$Slope))^2 * (1/(m-1))
# SEp = sqrt(sum(vb, vw, vb/m))
# # # lambda = sum(vb, vb/m)/sum(vb, vw, vb/m)
# # # dfo = (m-1)/(lambda)^2
# # # dfb = ((1634-15)+1)/((1634-15)+3)*(1634-15)*(1-lambda)
# # # dfa = dfo*dfb/(dfo+dfb)
# t = 2.021
# l95ci = me - t*SEp
# h95ci = me + t*SEp
me; l95ci; h95ci # 0.62 (0.60-0.64)

