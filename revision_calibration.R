library(pROC); library(rms); library(givitiR); library(rmda); library(tidyverse); library(gridExtra); library(grid); library(ggplot2); library(lattice)
library(ResourceSelection); library(CalibrationCurves)

## Checking with the model-derived data
## Cox recalibration frame
# simple scoring
fit <- glm(xpert ~ simpleoriginal, family=binomial(logit), data=newfd) # simple scoring
prob <- fit %>% predict(st.xp, type="response")
# prob1 <- fit %>% predict(st.xp, type="response", na.rm=F)
prob2 <- prob[!is.na(prob)]
# coeff scoring
probabilities <- fit.simpleoriginal %>% predict(st.xp, type="response") # coefficient scoring
probabilities2 <- probabilities[!is.na(probabilities)]

# multiple imputation with simple scoring
fit.mi.simple <- with(imputed.original, glm(xpert ~ as.numeric(simpleoriginal), family=binomial()))$analyses
prob.mi <- fit.mi.simple %>% predict(st.xp, type="response")
prob.mi2 <- unlist(prob.mi)[!is.na(unlist(prob.mi))]
prob.mi2.matrix <- matrix(prob.mi2, nrow=288, ncol=15)
prob.mi3 <- rowMeans(prob.mi2.matrix)
# odds.mi3<-prob.mi3/(1-prob.mi3)

fit <- with(Imput.own, glm(xpert~ifelse(agecat1=="[25,35)",1,0) + 1*sexcat + 1*hivcat1 + 1*n_tbsymp + 1*symp_2wks + 1*dbcat + 
                             0*n_other_sympcat + 0*edu8 + 0*pasttb + 0*eversmoke + 0*lungcat, family=binomial()))$analyses
prob <- fit %>% predict(external.subset, type="response")
probmean <- rowMeans(matrix(unlist(prob),nrow=387, ncol=15))
val.prob.ci.2(probmean, external.subset$xpert, logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black")

fit <- glm(xpert ~ score, family=binomial(), data=Imputown1.1)
fit$coefficients[['(Intercept)']] <- fit$coefficients[['(Intercept)']] + log((0.27/(1-0.27))/(0.49/(1-0.49)))
prob <- fit %>% predict(external.subset.simple, type="response")
val.prob.ci.2(prob, external.subset.simple$xpert, logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black")



score <- with(Imput.own, score<-ifelse(agecat1=="[25,35)",1,0) + ifelse(sexcat==1,1,0) + ifelse(hivcat1==1,2,0) + as.numeric(as.character(n_tbsymp)) + ifelse(symp_2wks==1,1,0) + ifelse(dbcat==1,1,0) +
                0*ifelse(n_other_sympcat==1,1,0) + 0*ifelse(edu8==1,1,0) + 0*ifelse(pasttb==1,1,0) + 0*ifelse(eversmoke==1,1,0) + 0*ifelse(lungcat==1,1,0))$analyses
Imputown1 <- complete(Imput.own, 1)
Imputown1$score <- score[[1]]
Imputown1.1 <- Imputown1[,c("score","xpert")]
Imputown1.1$score <- as.factor(Imputown1.1$score)
# Imputown1.1$score = relevel(Imputown1.1$score, ref=5)
fit <- glm(xpert ~ score, family=binomial(), data=Imputown1.1)
fit$coefficients[['(Intercept)']] <- (0.045007953 + log((0.2764858/(1-0.2764858))/(0.4739777/(1-0.4739777))))/0.1373901# log((0.2764858/(1-0.2764858))/(0.5000432/(1-0.5000432)))/0.1373901

external.subset.simple <- external[,c("score","xpert")]
prob <- fit %>% predict(external.subset.simple, type="response")

val.prob.ci.2(prob, external.subset.simple$xpert, logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black")



Imputown1 <- complete(Imput.own, 1)
Imputown1 <- data.imputed.Boot.own[[1]]
# fit <- round(glm(xpert ~ agecat1 + sexcat + hivcat1 + dbcat + n_tbsymp + symp_2wks, family=binomial(), data=Imputown1)/0.1373901)
fit <- glm(xpert ~ agecat1 + sexcat + hivcat1 + dbcat + n_tbsymp + symp_2wks + n_other_sympcat + edu8 + pasttb + eversmoke + lungcat, family=binomial(), data=Imputown1)
fit$coefficients[['(Intercept)']] <- -5 + round(log((0.2764858/(1-0.2764858))/(0.4739777/(1-0.4739777))))
fit$coefficients[['agecat1[25,35)']] <-1
fit$coefficients[['agecat1[35,45)']] <-0
fit$coefficients[['agecat1[45,55)']] <-0
fit$coefficients[['agecat1[55,99)']] <-0
fit$coefficients[['sexcat1']] <- 1
fit$coefficients[['hivcat11']] <- 2
fit$coefficients[['dbcat1']] <- 1
fit$coefficients[['symp_2wks1']] <- 1
fit$coefficients[['n_tbsymp1']] <-1; fit$coefficients[['n_tbsymp2']] <-2; fit$coefficients[['n_tbsymp3']] <-3; fit$coefficients[['n_tbsymp4']] <-4
fit$coefficients[['n_other_sympcat1']] <-0
fit$coefficients[['edu81']] <-0
fit$coefficients[['pasttb1']] <-0
fit$coefficients[['eversmoke1']] <-0
fit$coefficients[['lungcat1']] <-0

external.subset <- external[,c("agecat1","sexcat","hivcat1","n_tbsymp","symp_2wks","dbcat","n_other_sympcat","pasttb","edu8","eversmoke","lungcat","xpert")]
prob <- fit %>% predict(external.subset, type="response")
val.prob.ci.2(prob, external.subset.simple$xpert, logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black")






resamples <- lapply(1:20, function(i) sample(nrow(external.subset.simple), replace=T))
external.subset.simple.bootstrap<- external.subset.simple[unlist(resamples),]

fit$coefficients[['(Intercept)']] <- fit$coefficients[['(Intercept)']] + log((0.27/(1-0.27))/(0.34/(1-0.34)))
prob.bootstrap <- fit %>% predict(external.subset.simple.bootstrap, type='response')
val.prob.ci.2(prob.bootstrap, external.subset.simple.bootstrap$xpert, logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black")


val.prob.ci.2(prob*(106/387)/(765/1614), external.subset.simple$xpert, logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black")
glm(xpert ~ offset(prob), family=binomial, data=external.subset.simple)
glm(xpert ~ prob, family=binomial, data=external.subset.simple)


# 0. Build linear model 
fit <- glm(xpert ~ as.factor(score), family=binomial(), data=Imputown1.1)
# 1. Add predictions 
prob <- fit %>% predict(external.subset.simple, type="response", interval="prediction")
mydata <- cbind(external.subset.simple, prob)
pred <- predict(fit, newdata = external.subset.simple, interval = "confidence")
# 2. Regression line + confidence intervals
library("ggplot2")
p <- ggplot(mydata, aes(prob, xpert)) +
  geom_point() +
  stat_smooth(method = glm)
# 3. Add prediction intervals
p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")
p + geom_abline(y=x)


fit <-NA


# multiple imputation with coefficient scoring
fit.mi.coef<-with(imputed.original, glm(xpert~n_tbsymp+symp_2wks+sexcat+hivcat1+dbcat+n_other_sympcat, family=binomial()))$analyses
prob.mi.coef <- fit.mi.coef %>% predict(st.xp, type="response", na.action=na.omit)


with(imputed.original, val.prob(unlist(glm(xpert ~ as.numeric(simpleoriginal), family=binomial())%>%predict(st.xp, type='response')), st.xp$xpert, logit, pl=T, logistic.cal = T,  xlab="Predicted Probability of TB", 
                                     ylab="Actual Xpert TB"))

val.prob(prob, st.xp$xpert, logit, pl=T, smooth=T, logistic.cal = T,  xlab="Predicted Probability of TB", 
                   ylab="Actual Xpert TB", lim=c(0, 1), emax.lim=c(0,1),
         legendloc=lim[1] + c(0.55 * diff(lim), 0.40 * diff(lim)))#statloc = T

val.prob(probabilities, st.xp$xpert, logit, pl=T, smooth=T, logistic.cal = T,  xlab="Predicted Probability of TB", 
         ylab="Actual Xpert TB", lim=c(0, 1), emax.lim=c(0,1),
         legendloc=lim[1] + c(0.55 * diff(lim), 0.40 * diff(lim)))#statloc = T

val.prob.ci.2(prob2, st.xp[-c(224),]$xpert, logit, pl=T,  logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black") #CL.mooth=T

val.prob.ci.2(probabilities2, st.xp[-c(224),]$xpert, logit, pl=T, logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black")

val.prob.ci.2(prob.mi3, st.xp[-c(224),]$xpert, logit, pl=T, logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black")

val.prob.ci.2(prob.mi3*(102/289)*(1634/765), st.xp[-c(224),]$xpert, logit, pl=T, logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level = 0.95, xlim = c(0, 1),ylim = c(0,1),
              lty.smooth=2,col.smooth="blue", lty.log=1,col.log="black")




CalibrationCurves(prob, st.xp$xpert, logit, pl=T, smooth=T, logistic.cal = T,  xlab="Predicted Probability of TB", 
                  ylab="Actual Xpert TB", cl.level=0.95)
val.prob.ci.2(prob, st.xp$xpert, logit, pl=T, CL.smooth=T, logistic.cal = T,  xlab="Predicted Probability of TB", 
              ylab="Actual Xpert TB", cl.level=0.95)
val.prob.ci.2(prob, st.xp$xpert, logit)
st.xp$preds <- st.xp$agecat5 + st.xp$sex_female1 + st.xp$hiv + st.xp$db + st.xp$cough + st.xp$fever + st.xp$nightsweats + st.xp$weight_loss +
  st.xp$n_nontb_symps_cat + st.xp$symps_weeks_cat2
st.xp$simpleoriginal <- st.xp$preds
st.xp$xpert <- st.xp$xpertcc
st.xp$agecat2 <- st.xp$agecat5
st.xp$sexcat <- st.xp$sex_female1
st.xp$hivcat1 <- st.xp$hiv
st.xp$dbcat <- st.xp$db
st.xp$n_tbsymp <- st.xp$cough + st.xp$fever + st.xp$nightsweats + st.xp$weight_loss
st.xp$n_other_sympcat <- st.xp$n_nontb_symps_cat
st.xp$symp_2wks <- st.xp$symps_weeks_cat2

#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3933449/
# fit0 <- glm(xpert~ agecat2+sexcat+hivcat1+dbcat+n_tbsymp+n_other_sympcat+symp_2wks, family=binomial(), data=newfd)
# p <- predict(fit0, newdata=st.xp)
# group <- cut(p, c(-Inf, quantile(p,(1:9)/10, na.rm=T), Inf))
# fit1 <- glm(xpert ~ offset(p), family=binomial, data=st.xp)
# fit2 <- glm(xpert ~ p, family=binomial, data=st.xp)
# fit3 <- glm(xpert ~ -1 + group + offset(p), family=binomial, data=st.xp)
# plot(fit3)
fit <- glm(xpert ~ simpleoriginal, family=binomial(), data=newfd) # simple scoring
pp <- predict(fit, newdata=st.xp)
glm(xpert ~ offset(pp), family=binomial, data=st.xp)
glm(xpert ~ pp, family=binomial, data=st.xp)


## Hosmer-lemeshow goodness of fit
mod1 <- glm(xpert ~ simpleoriginal, data=newfd, family = binomial)
hoslem.test(newfd[complete.cases(newfd$simpleoriginal)&complete.cases(newfd$xpert),]$xpert, fitted(mod1), g=10) #X-squared = 7.1015, df = 8, p-value = 0.5257 --> no evidence of poor fit
hoslem.test(st.xp[complete.cases(st.xp$simpleoriginal),]$xpert, predict(mod1, newdata=st.xp[complete.cases(st.xp$simpleoriginal),]), g=10) #X-squared = -472.95, df = 8, p-value = 1

# mimod <- pool(with(imputed.original, glm(xpert~as.numeric(simpleoriginal), family=binomial())))
# hoslem.test(newfd$xpert, fitted(mimod), g=10)


## Calibration to be reported (with the validation data, Xpert case definition)
# age shouldn't be dichotomized
table(is.na(st.xp$age_years)); summary(st.xp$age_years)
st.xp$agecat4 <- cut(st.xp$age_years, breaks = c(15,25,35,45,55,99), right=F)
st.xp %>% group_by(xpertcc) %>% count(agecat4)
logistic.display(glm(xpertcc ~ agecat4, family=binomial(), data=st.xp))
st.xp$agecat5[st.xp$agecat4=="[55,99)"|st.xp$agecat4=="[15,25)"]<-0
st.xp$agecat5[st.xp$agecat4=="[45,55)"]<-1
st.xp$agecat5[st.xp$agecat4=="[25,35)"|st.xp$agecat4=="[35,45)"]<-2

st.xp$preds <- st.xp$agecat5 + st.xp$sex_female1 + st.xp$hiv + st.xp$db + st.xp$cough + st.xp$fever + st.xp$nightsweats + st.xp$weight_loss +
  st.xp$n_nontb_symps_cat + st.xp$symps_weeks_cat2


# The calibration plot 
m1 <- glm(xpert_status_fac_1 ~ agecat + sexcat + hivcat + dbcat + 
            n_tbsymp + n_other_sympcat + symp_2wks, data=fd, family = binomial)
fd.comp <- fd[!is.na(fd$preds),]
fd.comp$m1_pred <- predict(m1, type = "response")
val_m1 <- val.prob(fd.comp$m1_pred, as.numeric(fd.comp$xpert_status_fac_1) - 1, pl = FALSE) %>% round(3)

# Function to produce the calibration plots
fd.comp$xpert_status_fac_2 <- factor(fd.comp$xpert_status_fac_1)
# The calibration plot        
g1 <- mutate(fd.comp, bin = ntile(get("m1_pred"), 10)) %>% 
  # Bin prediction into 10ths
  group_by(bin) %>%
  mutate(n = n(), # Get ests and CIs
         bin_pred = mean(get("m1_pred")), 
         bin_prob = mean(as.numeric(xpert_status_fac_2) - 1), 
         se = sqrt((bin_prob * (1 - bin_prob)) / n), 
         ul = bin_prob + 1.96 * se, 
         ll = bin_prob - 1.96 * se) %>%
  ungroup() %>%
  ggplot(aes(x = bin_pred, y = bin_prob, ymin = ll, ymax = ul)) +
  geom_pointrange(size = 0.5, color = "black") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  geom_abline() + # 45 degree line indicating perfect calibration
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", 
              color = "black", formula = y~-1 + x) + 
  # straight line fit through estimates
  geom_smooth(aes(x = get("m1_pred"), y = as.numeric(xpert_status_fac_2) - 1), 
              color = "red", se = FALSE, method = "loess") + 
  # loess fit through estimates
  xlab("") +
  ylab("Observed Probability") +
  theme_minimal()+
  theme_title("Hosmer-lemeshow calibration plot")

# The distribution plot        
g2 <- ggplot(fd.comp, aes(x = get("m1_pred"))) +
  geom_histogram(fill = "black", bins = 200) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  xlab("Predicted Probability") +
  ylab("") +
  theme_minimal() +
  scale_y_continuous(breaks = c(0, 40)) +
  theme(panel.grid.minor = element_blank())

# Combine them    
g <- arrangeGrob(g1, g2, respect = TRUE, heights = c(1, 0.25), ncol = 1)
grid.newpage()
grid.draw(g)
return(g[[3]])