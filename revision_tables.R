### Table 1 for Kharitode 1409 population vs. STOMP 387 population
table1::table1(~ agecat1 + factor(sexcat) + factor(hivcat1) + factor(hivarv) + 
         factor(n_tbsymp) + factor(symp_fac___1cat) + factor(symp_fac___2cat) + factor(symp_fac___3cat) + factor(symp_fac___4cat) + 
         factor(symp_2wks) + factor(n_other_sympcat) + factor(dbcat) + factor(lungcat) + factor(edu8) + factor(pasttb) + factor(eversmoke) + factor(lungcat) + 
         factor(n_other_sympcat) | factor(xpert), data=newfd_xpert_completecase)

table1::table1(~ agecat1 + factor(sexcat) + factor(hivcat1) + factor(hivarv) + 
                factor(n_tbsymp) + factor(cough) + factor(fever) + factor(weight_loss) + factor(nightsweats) +
                factor(symp_2wks) + factor(n_other_sympcat) + factor(dbcat) + factor(lungcat) + factor(edu8) + factor(pasttb) + factor(eversmoke) + factor(lungcat) + 
                factor(n_other_sympcat) | factor(xpert), data=external)

### Supplement -- Table 1 for STOMP 387 population (derivation vs. internal population) vs. Kharitode
table1::table1(~ factor(coughsint_1) + factor(depress_1) + factor(mealssk) + factor(hhexposed) | factor(xpert), data=train)
table1::table1(~ factor(coughsint_1) + factor(depress_1) + factor(mealssk) + factor(hhexposed) | factor(xpert), data=test)



### Table 2 for univariable, multivariable, and Lasso regression OR in Kharitode
outcome <- factor(newfd_xpert_completecase$xpert)
exposure <- factor(newfd_xpert_completecase$pasttb)
uniglm <- glm(outcome ~ exposure, family=binomial())
logistic.display(uniglm, alpha=0.05)[2]

multiglm <- glm(xpert ~ pasttb + eversmoke + agecat1 + sexcat + hivcat1, data=newfd_xpert_completecase, family=binomial())
logistic.display(multiglm)

multiglm <- glm(xpert ~ agecat3 + sexcat + hivcat1 + factor(n_tbsymp) + symp_2wks + n_other_sympcat + dbcat + pasttb + eversmoke, data=newfd_xpert_completecase, family=binomial())
logistic.display(multiglm, alpha=0.05)[2]


### Supplement -- Table 2 for univariable, multivariable, and Lasso regression OR in STOMP
outcome <- factor(external$xpert)
exposure <- factor(external$hhexposed)
uniglm <- glm(outcome ~ exposure, family=binomial())
logistic.display(uniglm, alpha=0.05)[2]

multiglm <- glm(xpert ~ factor(agecat4) + factor(sexcat) + factor(hivarv) + factor(n_tbsymp) + factor(symp_2wks) + factor(n_other_sympcat) + factor(edu8) + factor(pasttb) + factor(eversmoke) + factor(coughsint_1) + factor(depress_1) + factor(hhexposed), data=external, family=binomial())
logistic.display(multiglm, alpha=0.05)[2]

