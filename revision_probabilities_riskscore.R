##How many at each risk score
# mt<-table(fd$preds, fd$xpert_status_fac)
# md <- as.data.frame.table(mt)

##TB probability in different TB prevalence settings
##1. sensitivity & specificity (at point) from Kharitode setting
# mytable<-prop.table(table(fd$preds, fd$xpert_status_fac),2)
# mydata <- as.data.frame.table(mytable)
# sens <- as.data.frame(mydata[1:9,3])
# spec <- as.data.frame(mydata[10:18,3]) #this is (1-spec)

# https://stackoverflow.com/questions/37102702/contingency-tables-after-multiply-imputated-data-with-mice-in-r
table <- with(imputed.original, prop.table(table(simpleoriginal, xpert),2))
nl <- length(table$analyses)
nr <- nrow(table$analyses[[1]])
nc <- ncol(table$analyses[[1]])

rnames <- rownames(table$analyses[[1]])
cnames <- colnames(table$analyses[[1]])

array <- array(unlist(table$analyses), dim=c(nr,nc,nl), dimnames=list(rnames,cnames))
array1 <- apply(array, 1:2, mean)
tabledata <- as.data.frame.array(array1)

sens <- as.data.frame(tabledata[1:11,2])
spec_1 <- as.data.frame(tabledata[1:11,1])

table.vector <- unlist(table$analyses)
table.matrix <- matrix(table.vector,nrow=11, ncol=30)
table.data <- as.data.frame.matrix(table.matrix)
sens1<-rowMeans(table.data[c(1:11),c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30)])
spec1_1 <- rowMeans(table.data[c(1:11),c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)])
spec1 <- 1-spec1_1

# ##2. probability in Kharitode setting
# TBprob.gold <- prop.table(table(fd$preds, fd$xpert_status_fac),1)
# prev <- 641/1253 # this is not a real prev.
# TBprob <- (prev*sens1 / (prev*sens1 + (1-prev)*spec1))*100


##3. probability in 5%,10%,20% TB prevalence setting
prev <- 0.01
TBprob.01 <- (prev*sens1) / (prev*sens1 + (1-prev)*(1-spec1))*100 #same equation as ppv but sens and spec in ppv are "higher than cutoff", here is "at cutoff"


# 20%
# 1            2.773777
# 2            5.395202
# 3            6.910880
# 4           10.872045
# 5           18.089945
# 6           34.558183
# 7           51.785026
# 8           70.134303
# 9           64.647713
# 10          71.590217
# 11         100.000000

# 10%
# 1            1.252084
# 2            2.471960
# 3            3.194137
# 4            5.142637
# 5            8.938269
# 6           19.008664
# 7           32.311375
# 8           51.069117
# 9           44.834976
# 10          52.829341
# 11         100.000000

# 5%
# 1            0.597027
# 2            1.186364
# 3            1.538883
# 4            2.503754
# 5            4.442928
# 6           10.005067
# 7           18.441557
# 8           33.082785
# 9           27.796970
# 10          34.662228
# 11         100.000000


# ##1. sensitivity & specificity (at higher-than point) from Kharitode setting
# mytable1 <- table(fd$preds, fd$xpert_status_fac)
# mydata1 <- as.data.frame.table(mytable1)
# mydata1.sub.sens <- mydata1[mydata1$Var2==1,]
# mydata1.sub.sens$Var1 <- as.numeric(mydata1.sub.sens$Var1)
# mydata1.sub.sens<-mydata1.sub.sens[order(-mydata1.sub.sens$Var1),]
# mydata1.sub.sens$cum <- cumsum(mydata1.sub.sens$Freq)
# sens2 <- as.data.frame(mydata1.sub.sens$cum/641)
# 
# mydata1.sub.spec <- mydata1[mydata1$Var2==2,]
# mydata1.sub.spec$Var1 <- as.numeric(mydata1.sub.spec$Var1)
# mydata1.sub.spec<-mydata1.sub.spec[order(-mydata1.sub.spec$Var1),]
# mydata1.sub.spec$cum <- cumsum(mydata1.sub.spec$Freq)
# spec2 <- as.data.frame(mydata1.sub.spec$cum/612)
# 
# 
# tablec <- with(imputed.original, table(simpleoriginal, xpert))
# nl <- length(tablec$analyses)
# nr <- nrow(tablec$analyses[[1]])
# nc <- ncol(tablec$analyses[[1]])
# 
# rnames <- rownames(tablec$analyses[[1]])
# cnames <- colnames(tablec$analyses[[1]])
# 
# arrayc <- array(unlist(tablec$analyses), dim=c(nr,nc,nl), dimnames=list(rnames,cnames))
# arrayc1 <- apply(arrayc, 1:2, mean)
# tabledatac <- as.data.frame.array(arrayc1)
# sensc <- cumsum(tabledatac[,2]/774.8)
# specc_1 <- cumsum(tabledatac[,1]/859.2)
# specc <- 1-cumsum(tabledatac[,1]/859.2)
# colSums(tabledatac)
# sensc
# sensc[order(-sensc)] # rev(sensc)
# specc
# rev(specc)

## sens, spec, PPV, NPV graph
original.roc <- with(imputed.original, roc(xpert~ glm(xpert~as.numeric(simpleoriginal), family=binomial())$fitted.values, plot=T, col=2, main="Figure 1. (b) Discrimination",)) #ci=T
roc.sens <- with(imputed.original, pROC::roc(xpert~ glm(xpert~as.numeric(simpleoriginal), family=binomial())$fitted.values)$sensitivities)
roc.spec <- with(imputed.original, pROC::roc(xpert~glm(xpert~as.numeric(simpleoriginal), family=binomial())$fitted.values)$specificities)

roc.sens.vector <- unlist(roc.sens$analyses)
roc.sens.matrix <- matrix(roc.sens.vector,nrow=12, ncol=15)
roc.sens.data <- as.data.frame.matrix(roc.sens.matrix)

roc.spec.data <- as.data.frame.matrix(matrix(unlist(roc.spec$analyses), nrow=12, ncol=15))

sensitivity<-rowMeans(roc.sens.data)
specificity<-rowMeans(roc.spec.data)

prevalence = 0.05
ppv05 = (sensitivity*prevalence)/((sensitivity*prevalence)+(1-specificity)*(1-prevalence))
npv05 = (specificity*(1-prevalence))/(((1-sensitivity)*prevalence)+(specificity*(1-prevalence)))

cutoff <- c(1,2,3,4,5,6,7,8,9,10,11,12)

# curve <- matrix(c(cutoff, rev(sensc), rev(specc), ppv20, npv20, ppv10, npv10, ppv05, npv05), nrow=11, ncol=9)
# curvedt <- as.data.frame(curve, colnames=c("cutoff","sensitivity","specificity","positive predictive value", "negative predictive value"))

curvedt <- as.data.frame(matrix(c(cutoff, sensitivity, specificity, ppv20, npv20, ppv10, npv10, ppv05, npv05), nrow=12, ncol=9))
plot(100*curvedt$V2~curvedt$V1, col=1, type="o", xlab="Risk score cutoff", ylab="(%)", xlim=c(0,11),
     frame=F, pch=18, main="Prediction tool evaluation")
xtick<-seq(0,12, by=1)
axis(side=1, at=xtick)
lines(100*curvedt$V3, col=2, type="o", pch=18)
lines(100*curvedt$V4, col=3, lty=1, type="o", pch=18)
lines(100*curvedt$V5, col=4, lty=1, type="o", pch=18)
lines(100*curvedt$V6, col=3, lty=2, type="o", pch=18)
lines(100*curvedt$V7, col=4, lty=2, type="o", pch=18)
lines(100*curvedt$V8, col=3, lty=3, type="o", pch=18)
lines(100*curvedt$V9, col=4, lty=3, type="o", pch=18)
legend("left", legend=c("Sensitivity","Specificity","PPV(prev=20%)","NPV(prev=20%)","PPV(prev=10%)","NPV(prev=10%)","PPV(prev=5%)","NPV(prev=5%)"),
       col=c(1,2,3,4,3,4,3,4), lty=c(1,1,1,1,2,2,3,3), cex=0.5)
# 
# g1<-ggplot(data=curvedt, aes(x=V1, y=V2, group=1))+
#   geom_line()+
#   geom_point()
# g2<-ggplot(data=curvedt, aes(x=V1, y=V3, group=1))+
#   geom_line()+
#   geom_point()
# g1+g2
# ggplot(curvedt, aes(x=V1, y=V2, group=1))+
#   geom_line()+
#   geom_line(data=curvedt, aes(x=V1, y=V2, group=1))
  


# library(AUC)
# accuracy(glm(newfd[complete.cases(newfd$xpert)&complete.cases(newfd$simpleoriginal),]$xpert~newfd[complete.cases(newfd$xpert)&complete.cases(newfd$simpleoriginal),]$simpleoriginal, family=binomial()), newfd$xpert)
# accuracy(newfd$simpleoriginal, newfd$xpert, perc.rank=T)
# plot(sensitivity(newfd$simpleoriginal, newfd$xpert))

# library(Epi)
# with(imputed.original, ROC(form= xpert~ glm(xpert~as.numeric(simpleoriginal), family=binomial()), plot="sp"))
# ROC(form= newfd$xpert~ glm(newfd$xpert~newfd$simpleoriginal), family=binomial()), plot="sp")
# library(ROCit)
# roc_binormal <- with(imputed.original, rocit(as.numeric(simpleoriginal), xpert, method="binormal"))
# roc_measure <- with(imputed.original, measureit(as.numeric(simpleoriginal), xpert, method="binormal"))
# 
# library(riskyr)
# plot_curve()
# plot_curve()
# plot_curve(prev=0.10, sens=c(0.009465557, 0.026160072, 0.071153034, 0.114180902, 0.151003976, 0.180778743, 0.177680509, 0.159613501, 0.082859497, 0.025813544, 0.001290666),
#            spec = c(0.9170535, 0.8853209, 0.7603933, 0.7659891, 0.8290660, 0.9144162, 0.9586422, 0.9830077, 0.9886722, 0.9974390, 1.0000000))
# 



### Kharitode predicted probabilities by score
prevalence <- 0.10
d<-data.imputed.Boot.own[[1]]
# sens <- pROC::roc(d$xpert ~ glm(xpert ~ score, data= d, family=binomial())$fitted.values)$sensitivities
# spec <- pROC::roc(d$xpert ~ glm(xpert ~ score, data= d, family=binomial())$fitted.values)$specificities
# predicted.probability = (sens*prevalence)/((sens*prevalence)+(1-spec)*(1-prevalence)); predicted.probability

table <- prop.table(table(d$score, d$xpert),2)
sens <- as.data.frame(table[1:10,2])
spec_1 <- as.data.frame(table[1:10,1])
spec <- 1-spec_1

predicted.probability <- (prevalence*sens) / (prevalence*sens + (1-prevalence)*(1-spec)) ; predicted.probability
