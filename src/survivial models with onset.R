coxme.cla0 <- coxme(Surv(DaysToCLA, FirstCLA)~AvgKeto20_100 + MaxKetoDIM60 + KetoOnset20 + ad_Lactation + PeakDFC + AvgMY60 + CalvingSeason + cut(CowNo, c(0, 100, 180, 1000)) + (1|FarmNo), data = survival)
coxme.cla0

# categorized AvgKeto20_100
coxme.cla1 <- coxme(Surv(DaysToCLA, FirstCLA)~factor(KetoStatus20) + MaxKetoDIM60 + KetoOnset20 + ad_Lactation + PeakDFC + AvgMY60 + CalvingSeason + cut(CowNo, c(0, 100, 180, 1000)) + (1|FarmNo), data = survival)
coxme.cla1

anova(coxme.cla0, coxme.cla1)

# Remove AvgMY60
coxme.cla0.1 <- coxme(Surv(DaysToCLA, FirstCLA)~AvgKeto20_100 + MaxKetoDIM60 + KetoOnset20 + ad_Lactation + PeakDFC + CalvingSeason + cut(CowNo, c(0, 100, 180, 1000)) + (1|FarmNo), data = survival)
coxme.cla0.1

anova(coxme.cla0, coxme.cla0.1) # loglik is the same, model coxme.cla0.1 is simpler

coxme.cla1.1 <- coxme(Surv(DaysToCLA, FirstCLA)~factor(KetoStatus20) + MaxKetoDIM60 + KetoOnset20 + ad_Lactation + PeakDFC + CalvingSeason + cut(CowNo, c(0, 100, 180, 1000)) + (1|FarmNo), data = survival)
coxme.cla1.1

anova(coxme.cla0.1, coxme.cla1.1) # model coxme.cla0.1 is better

# Remove MaxKetoDIM60
coxme.cla0.2 <- coxme(Surv(DaysToCLA, FirstCLA)~AvgKeto20_100 + KetoOnset20 + ad_Lactation + PeakDFC + CalvingSeason + cut(CowNo, c(0, 100, 180, 1000)) + (1|FarmNo), data = survival)
coxme.cla0.2

anova(coxme.cla0.1, coxme.cla0.2) # similar loglik, model coxme.cla0.2 is simpler

coxme.cla1.2 <- coxme(Surv(DaysToCLA, FirstCLA)~factor(KetoStatus20) + ad_Lactation + KetoOnset20 + PeakDFC + CalvingSeason + cut(CowNo, c(0, 100, 180, 1000)) + (1|FarmNo), data = survival)
coxme.cla1.2

# Remove farm size
coxme.cla0.3 <- coxme(Surv(DaysToCLA, FirstCLA)~AvgKeto20_100 + KetoOnset20 + ad_Lactation + PeakDFC + CalvingSeason + (1|FarmNo), data = survival)
coxme.cla0.3

anova(coxme.cla0.2, coxme.cla0.3) # model coxme.cla0.2 has higher loglik

AIC(coxme.cla0.2) # 58791.65
AIC(coxme.cla0.3) # 58793.48
AIC(coxme.cla0.3) - AIC(coxme.cla0.2) # AIC difference < 2, model without farm size is better

coxme.cla1.3 <- coxme(Surv(DaysToCLA, FirstCLA)~factor(KetoStatus20) + ad_Lactation + PeakDFC + CalvingSeason + (1|FarmNo), data = survival)

anova(coxme.cla1.2, coxme.cla1.3) # model coxme.cla1.2 has higher loglik

AIC(coxme.cla1.2) # 58800.21
AIC(coxme.cla1.3) # 58801.9

# remove PeakDFC
coxme.cla0.4 <- coxme(Surv(DaysToCLA, FirstCLA)~AvgKeto20_100 + ad_Lactation + CalvingSeason + (1|FarmNo), data = survival)
coxme.cla0.4
AIC(coxme.cla0.4) # 58803.3, coxme.cla0.3 is better

# coxme.cal0.3 remove CalvingSeason
coxme.cla0.5 <- coxme(Surv(DaysToCLA, FirstCLA)~AvgKeto20_100 + ad_Lactation + PeakDFC + (1|FarmNo), data = survival)
AIC(coxme.cla0.5) # 58829.13, coxme.cla0.3 is better

########################
##     FINAL MODEL    ##
########################
coxme.cla0.3 <- coxme(Surv(DaysToCLA, FirstCLA)~AvgKeto20_100 + KetoOnset20 + ad_Lactation + I(PeakDFC/7) + CalvingSeason + (1|FarmNo), data = survival)
coxme.cla0.3
AIC(coxme.cla0.3) # 58785.4



#############################
#############################
##  First Heat
coxph1 <- coxph(Surv(DaysToFirstHeat, FirstHeat) ~ AvgKeto20_100 + MaxKetoDIM60 + ad_Lactation + PeakDFC + AvgMY60 + CalvingSeason + frailty(FarmNo), data=survival)
coxph1
coxph1.1 <- coxph(Surv(DaysToFirstHeat, FirstHeat) ~ AvgKeto20_100 + MaxKetoDIM60 + KetoOnset20 +ad_Lactation + PeakDFC + AvgMY60 + CalvingSeason + frailty(FarmNo), data=survival)
coxph1.1


coxme1 <- coxme(Surv(DaysToFirstHeat, FirstHeat) ~ AvgKeto20_100 + MaxKetoDIM60 + ad_Lactation + PeakDFC + AvgMY60 + CalvingSeason + (1|FarmNo), data=survival)
coxme1

anova(coxph1, coxme1)

# drop AvgMY60
coxph2 <- coxph(Surv(DaysToFirstHeat, FirstHeat) ~ AvgKeto20_100 + MaxKetoDIM60 + ad_Lactation + PeakDFC + CalvingSeason + frailty(FarmNo), data=survival)
coxph2

coxph2.1 <- coxph(Surv(DaysToFirstHeat, FirstHeat) ~ AvgKeto20_100 + MaxKetoDIM60 + KetoOnset20 +  ad_Lactation + PeakDFC + CalvingSeason + frailty(FarmNo), data=survival)
coxph2.1

coxph.fix2 <- coxph(Surv(DaysToFirstHeat, FirstHeat) ~ AvgKeto20_100 + MaxKetoDIM60 + KetoOnset20+ad_Lactation + PeakDFC + CalvingSeason, data=survival)
coxph.fix2

anova(coxph2.1, coxph.fix2)

# Both MaxKetoDIM60 and KetoOnset20
coxme1 <- coxme(Surv(DaysToFirstHeat, FirstHeat) ~ AvgKeto20_100 + MaxKetoDIM60 + KetoOnset20 + ad_Lactation + PeakDFC + CalvingSeason + (1|FarmNo), data=survival)
coxme1

## only MaxKetoDIM60
coxme2 <- coxme(Surv(DaysToFirstHeat, FirstHeat) ~ AvgKeto20_100 + MaxKetoDIM60 + ad_Lactation + PeakDFC + CalvingSeason + (1|FarmNo), data=survival)
coxme2

## only KetoOnset20
coxme3 <- coxme(Surv(DaysToFirstHeat, FirstHeat) ~ AvgKeto20_100 + KetoOnset20 + ad_Lactation + PeakDFC + CalvingSeason + (1|FarmNo), data=survival)
coxme3

anova(coxme2, coxme3)
anova(coxme1, coxme3)

anova(coxph2, coxme2)
anova(coxme2, coxph.fix2)

AIC(coxph.fix2) # 58895.74
AIC(coxph2) # 58802.06
AIC(coxme2) # 58801.06

coxph.firstheat <- coxph(Surv(DaysToFirstHeat, FirstHeat)~AvgKeto20_100 + MaxKetoDIM60 + KetoOnset20 + factor(ad_Lactation) + I(PeakDFC/7) + CalvingSeason + frailty(FarmNo), data=survival)
coxph.firstheat
AIC(coxph.firstheat) # 58795.17

# Remove MaxKetoDIM60
coxph.firstheat1 <- coxph(Surv(DaysToFirstHeat, FirstHeat)~AvgKeto20_100 + KetoOnset20 + factor(ad_Lactation) + I(PeakDFC/7) + CalvingSeason + frailty(FarmNo), data=survival)
coxph.firstheat1
AIC(coxph.firstheat1) # 58800.98



##############################
##############################
##  AI


coxme8 <- coxme(Surv(DaysToFirstIns, FirstAI)~AvgKeto20_100 + ad_Lactation + PeakDFC:ad_Lactation + PeakDFC + AvgMY60 + (1|FarmNo), data=survival)
coxme8
coxme8.1 <- coxme(Surv(DaysToFirstIns, FirstAI)~AvgKeto20_100 + KetoOnset20 +ad_Lactation + PeakDFC:ad_Lactation + PeakDFC + AvgMY60 + (1|FarmNo), data=survival)
coxme8.1

coxme6 <- coxme(Surv(DaysToFirstIns, FirstAI)~AvgKeto20_100 + I(PeakDFC/7) + AvgMY60 + (1|FarmNo), data=survival)
coxme6
AIC(coxme6) #56673.82
coxme6.1 <- coxme(Surv(DaysToFirstIns, FirstAI)~AvgKeto20_100 + KetoOnset20 + I(PeakDFC/7) + AvgMY60 + (1|FarmNo), data=survival)
coxme6.1
AIC(coxme6.1) #56667.89

