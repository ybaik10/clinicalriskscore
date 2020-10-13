data<-read.csv("STOMPTBMainCasesCont_DATA_2020-01-13_1333.csv", header=T) 
main<-data
# source('All cases cleaning code.R')

main$xcase[((main$case.control=="CC phase enrolled case"|main$case.control=="CC phase reclassified case")|
            ((main$case.control=="COM phase enrolled case"|main$case.control=="COM phase reclassified case")&main$com_screening_method_case==1))&
             (main$why_a_case_xpert.factor=="Yes, positive Xpert"|
                main$com_why_a_case_xpert.factor=="Yes, positive Xpert")]<-1

main$xcase[(main$case.control=="CC phase control"|
            (main$case.control=="COM phase control"&main$com_ctrl_type==1))&
             (main$why_a_ctrl_xpert.factor=="Yes, negative Xpert"|
                main$com_why_a_ctrl_xpert.factor=="Yes, negative Xpert")]<-0

## refusal rate?
# from CC pahse enrolled case
main %>% filter(phase=="1a" & redcap_event_name=="tb_case_enrollment_arm_1" & ineligiblecase_screening___0.factor=="Checked" & why_a_case_xpert.factor=="Yes, positive Xpert") %>% count(caseagreed_screening.factor)
# from CC phase reclassified case
# from COM phase enrolled case
main %>% filter(phase=="1b" & redcap_event_name=="tb_case_enrollment_arm_3" & com_ineligcase_screening___0.factor=="Checked" & com_why_a_case_xpert.factor=="Yes, positive Xpert") %>% count(com_caseagreed_screening.factor)
#main$studyid=="COM-2-11125-1"
# from COM phase reclassified case
# from CC phase control
main %>% filter(phase=="1a" & redcap_event_name=="control_enrollment_arm_2" & ineligible_screening_ctrl___0.factor=="Checked" & !(main$not_really_a_control) & why_a_ctrl_xpert.factor=="Yes, negative Xpert") %>% count(agreed_screening_ctrl.factor)
# from COM phase control
main %>% filter(phase=="1b" & redcap_event_name=="control_enrollment_arm_4" & com_inelig_screening_ctrl___0.factor=="Checked" & !(main$not_really_a_control) & com_why_a_ctrl_xpert.factor=="Yes, negative Xpert") %>% count(com_agreed_screening_ctrl.factor)
# total = 3+72+1+34+9+210+1+71; (401-387)/401


external <- main %>% filter(xcase==1 | xcase==0)

# Xpert
external$xpert <- external$xcase
# Age
external$agecat1 <- ifelse(external$age_years>=25 & external$age_years<35,1,0)
external$agecat1[external$age_years>=15 & external$age_years<25]<-"[15,25)"
external$agecat1[external$age_years>=25 & external$age_years<35]<-"[25,35)"
external$agecat1[external$age_years>=35 & external$age_years<45]<-"[35,45)"
external$agecat1[external$age_years>=45 & external$age_years<55]<-"[45,55)"
external$agecat1[external$age_years>=55]<-"[55,99)"
# external$agecat2 <- external$agecat1
external$agecat3[external$agecat1=="[15,25)"] <- "1"
external$agecat3[external$agecat1=="[25,35)"] <- "2"
external$agecat3[external$agecat1=="[35,45)"] <- "3"
external$agecat3[external$agecat1=="[45,55)"] <- "4"
external$agecat3[external$agecat1=="[55,99)"] <- "0"
external$agecat4[external$agecat3==0 | external$agecat3==4] <- 0
external$agecat4[external$agecat3==1]<-1
external$agecat4[external$agecat3==2]<-2
external$agecat4[external$agecat3==3]<-3
# Sex
external$sexcat <- ifelse(external$sex_female.factor=="Male",1,0)
# HIV
# external$hiv <- ifelse(!is.na(external$labhiv.factor),external$labhiv.factor,
#                  ifelse(!is.na(external$hiv_test_result.factor),external$hiv_test_result.factor,
#                         ifelse(!is.na(external$pretbhiv.factor),external$pretbhiv.factor,
#                                ifelse(!is.na(external$medical_conditions___5),external$medical_conditions___5,NA))))
# external %>% count(labhiv.factor, hiv_test_result.factor, pretbhiv.factor, medical_conditions___5, hiv)
external$hivcat1 <- ifelse(external$hiv==1,1,0)
# Diabetes
# external$db <- ifelse(external$medical_conditions___8==0,
#                       ifelse(is.na(external$hba1c_informed.factor),external$medical_conditions___3,external$hba1c_informed.factor), NA)
# external %>% count(medical_conditions___8, medical_conditions___3, hba1c_informed.factor, db)
external$dbcat1 <- external$dbcat <- ifelse(external$db==1,1,0)
# TB symptoms
# external$cough <- ifelse(external$symptoms_past_week___9==0, ifelse((external$symptoms_past_week___1==1|external$symptoms_past_week___2==1),1,0), NA)
# external$fever <- ifelse(external$symptoms_past_week___9==0, ifelse(external$symptoms_past_week___3==1, 1, 0), NA)
# external$weight_loss
# external$nightsweats <- ifelse(external$symptoms_past_week___9==0, ifelse(external$symptoms_past_week___5==1, 1, 0), NA)
external$n_tbsymp <- external$n_symp <- external$cough + external$fever + external$weight_loss + external$nightsweats
# external$n_tbsymp1 <- external$n_tbsymp <- cut(external$n_symp, breaks=c(0,1,2,3,4,5), right=F, labels = c(0,1,2,3,4))
# Duration of TB symptoms (including hemoptysis and taking longer symptom period is different from Kharitode question)
# external$symps_weeks <- pmax(external$cough_weeks, external$hemoptysis_weeks, external$weightloss_weeks, external$nightsweats_weeks, external$feverchills_weeks, na.rm=TRUE)
external$symp_2wks[external$symps_weeks <=2] <-0
external$symp_2wks[external$symps_weeks >2] <-1
# Non-TB symptoms
# external$chestpain <- ifelse(external$symptoms_past_week___9==0, ifelse(external$symptoms_past_week___7==1, 1, 0), NA)
# external$fatigue <- ifelse(external$symptoms_past_week___9==0, ifelse(external$symptoms_past_week___4==1, 1, 0), NA)
# external$shortbreath <- ifelse(external$symptoms_past_week___9==0, ifelse(external$symptoms_past_week___6==1, 1, 0), NA)
# external$others <- ifelse(external$symptoms_past_week___9==0, ifelse(external$symptoms_past_week___8==1, 1, 0), NA)
# external$n_nontb_symps <- external$chestpain + external$fatigue + external$shortbreath + external$others
external$n_other_sympcat <- ifelse(external$n_nontb_symps==0,0,1)
# Education (after primary before middle school vs. middle school and above)
external$edu8[external$highest_grade_education==1|external$highest_grade_education==2] <-1
external$edu8[external$highest_grade_education==3|external$highest_grade_education==4|external$highest_grade_education==5|external$highest_grade_education==7|external$highest_grade_education==9] <-0
# Self-report previous TB (question is sepcifically about past treatment, which is different from Kharitode question)
external$pasttb <- external$treated_past
# Ever smoking
external$eversmoke[external$smoked_tobacco_in_past==1 | external$smoke_tobacco==1]<-1
external$eversmoke[external$smoked_tobacco_in_past==0 & external$smoke_tobacco==0]<-0
# Any lung disease (COPD)
external$lungcat <- ifelse(external$medical_conditions___8==0,
                           ifelse(external$medical_conditions___2==1,1,0),NA)

external$score <- ifelse(external$agecat1=="[25,35)"|external$agecat1=="[35,45)",1,0) + external$sexcat + 2*external$hivcat1 + external$dbcat + as.numeric(as.character(external$n_tbsymp)) + as.numeric(as.character(external$symp_2wks)) 
external$score[external$score>=8]<-"8+"
external$score <- as.factor(external$score)

external$score.c <- ifelse(external$n_tbsymp==2,0.66786770,0) + ifelse(external$n_tbsymp==3,1.68024123,0) + ifelse(external$n_tbsymp==4,2.27979890,0) + 
  ifelse(external$symp_2wks==1,0.83973696,0) + ifelse(external$agecat3=="1",0.28886628,0) + ifelse(external$agecat3=="2",0.81594505,0) + ifelse(external$agecat3=="3",0.41984499,0) + ifelse(external$agecat3=="4",0.14742784,0) +
  ifelse(external$sexcat==1,0.92502417,0) + ifelse(external$hivcat1==1,1.20714530,0) + ifelse(external$dbcat==1,0.70729174,0) +
  ifelse(external$n_other_sympcat==1,0.25531847,0) + ifelse(external$edu8==1,-0.07751068,0) + ifelse(external$pasttb==1,0.12307903,0) + 
  ifelse(external$eversmoke==1,-0.19609719,0) + ifelse(external$lungcat==1,-0.14438743,0)

external$score.c.rep <- ifelse(external$n_tbsymp==2,0.73394762,0) + ifelse(external$n_tbsymp==3,1.75329639,0) + ifelse(external$n_tbsymp==4,2.37461251,0) + 
  ifelse(external$symp_2wks==1,0.81263463,0) + ifelse(external$agecat3=="1",0.35380844,0) + ifelse(external$agecat3=="2",0.88368297,0) + ifelse(external$agecat3=="3",0.48474498,0) + ifelse(external$agecat3=="4",0.20626423,0) +
  ifelse(external$sexcat==1,0.96635333,0) + ifelse(external$hivcat1==1,1.21848394,0) + ifelse(external$dbcat==1,0.73883474,0) +
  ifelse(external$n_other_sympcat==1,0.24823684,0) + ifelse(external$edu8==1,-0.05952317,0) + ifelse(external$pasttb==1,0.14853784,0) + 
  ifelse(external$eversmoke==1,-0.24226227,0) + ifelse(external$lungcat==1,-0.19100971,0)

external$score810 <- external$score
external$score810[external$score810==8]<-10
external$score810[external$score810==9]<-10

external.subset <- external %>% select(xpert, score)
external.subset810 <- external %>% select(xpert, score810)

external$scoren7 <- -1*external$cough + 0*external$fever + 2*external$weight_loss + 1*external$nightsweats + external$sexcat + external$hivcat1 + external$dbcat +  as.numeric(as.character(external$symp_2wks)) +
  1*ifelse(external$agecat1=="[15,25)",1,0) + 1*ifelse(external$agecat1=="[25,35)",1,0) + 1*ifelse(external$agecat1=="[35,45)",1,0) + 1*ifelse(external$agecat1=="[45,55)",1,0) + 1*ifelse(external$agecat1=="[55,99)",1,0) 
# external.subset[]<-lapply(external.subset, function(x){return(as.factor(x))})

### More data management
# Drinking
# external$binge[external$six_or_more_alchl_past_yr==6]<-NA
# external$binge[external$six_or_more_alchl_past_yr==1]<-0
# external$binge[external$six_or_more_alchl_past_yr==2|external$six_or_more_alchl_past_yr==3]<-1
# external$binge[external$six_or_more_alchl_past_yr==4|external$six_or_more_alchl_past_yr==5]<-2
external$binge_1[external$binge==0]<-0
external$binge_1[external$binge==1|external$binge==2]<-1
# Coughs in interview
# external$coughsint[external$coughs_in_interview==1|external$coughs_in_interview==2]<-0
# external$coughsint[external$coughs_in_interview==3|external$coughs_in_interview==4]<-1
# external$coughsint[external$coughs_in_interview==5] <-2
external$coughsint_1[external$coughsint==0]<-0
external$coughsint_1[external$coughsint==1|external$coughsint==2]<-1
# Depression
# external$depress[external$feeling_down_depressed==5]<-NA
# external$depress[external$feeling_down_depressed==1]<-0
# external$depress[external$feeling_down_depressed==2|external$feeling_down_depressed==3]<-1
# external$depress[external$feeling_down_depressed==4]<-2
external$depress_1[external$depress==0]<-0
external$depress_1[external$depress==1|external$depress==2]<-1
# Household poverty
external$mealssk[external$meals_skipped==4]<-NA
external$mealssk[external$meals_skipped==1]<-0
external$mealssk[external$meals_skipped==2|external$meals_skipped==3]<-1
# Household exposed
# table(external$ever_lived_w_tb_contact.factor)
# table(external$same_room_wth_tb_case.factor)
external$hhexposed <- ifelse(external$ever_lived_w_tb_contact.factor=="Yes"|external$same_room_wth_tb_case.factor=="Yes",1,0)
external$hhexposed[is.na(external$hhexposed)]<-0
# Time to seekcare
# external$stc[external$time_to_seeking_care<=7] <-1
# external$stc[external$time_to_seeking_care>7 & external$time_to_seeking_care<=14] <-2
# external$stc[external$time_to_seeking_care>14 & external$time_to_seeking_care<=21] <-3
# external$stc[external$time_to_seeking_care>21] <-4
external$stc_1[external$time_to_seeking_care<=14]<-0
external$stc_1[external$time_to_seeking_care>14]<-1
#hiv+arv, arv
external$hivarv[external$hivcat1==0]<-0
external$hivarv[external$hivcat1==1 & external$currently_taking_arvs==1]<-1
external$hivarv[external$hivcat1==1 & external$currently_taking_arvs==0]<-2
external$arv[external$hivarv==0]<-NA
external$arv[external$hivarv==1]<-0 # yes on art
external$arv[external$hivarv==2]<-1 # no not on art

external %>% group_by(xpert) %>% count(occupation.factor)
external %>% count(income_regular_emplyment, income_casual_work, income_grant_pensions, income_other_sources) %>% view()
external$income <- external$income_regular_emplyment + external$income_casual_work + external$income_grant_pensions + external$income_other_sources
external %>% group_by(xpert) %>% summarise(median(income, na.rm=T),quantile(income,0.25,na.rm=T),quantile(income,0.75,na.rm = T))

external %>% filter(symptoms_past_week___1==1) %>% count(symptoms_past_week___2)


# # To put in lasso matrix after multiple imputation of Kharitode
# external$xpert <- as.factor(external$xpert)
# external$symp_2wks <- as.factor(external$symp_2wks)
# external$sexcat <- as.factor(external$sexcat)
# external$hivcat1 <- as.factor(external$hivcat1)
# external$dbcat <- as.factor(external$dbcat)
# external$n_tbsymp <- as.factor(external$n_tbsymp); external$n_tbsymp[]<-0
# external$n_other_sympcat <- as.factor(external$n_other_sympcat)
# external$edu8 <- as.factor(external$edu8)
# external$pasttb <- as.factor(external$pasttb)
# external$eversmoke <- as.factor(external$eversmoke)
# external$lungcat <- as.factor(external$lungcat)





# external$score <- external$agecat3 + external$sexcat + 2*external$hivcat1 + external$dbcat + as.numeric(as.character(external$n_tbsymp)) + as.numeric(as.character(external$symp_2wks)) 
# external$score <- as.factor(external$score)
# external %>% count(score)
# external$score[external$score==8]<-7
# class(external$symp_2wks)
# external %>% count(as.numeric(as.character(symp_2wks)))

