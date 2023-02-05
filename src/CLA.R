rm(list=ls(pattern="milk"))
plot(survival$DaysToCLA, survival$AvgKeto20_100,
     xlab = "Time from calving to resupmtion of cyclicity",
     ylab = "Average milk BHB*100 (mmol/L)")
boxplot(survival$DaysToFirstHeat~survival$KetoStatus20,
        xlab = "Average milk BHB < 0.08: 0; >=0.08: 1",
        ylab = "Time from calving to first cyclicity")


Three <- read_excel("../data/raw/Three.xlsx")

survival <- Three %>%
  mutate(FirstCLA = ifelse(is.na(Three$DaysToCLA), 0, 1)) %>%
  mutate(FirstHeat = ifelse(is.na(Three$DaysToFirstHeat), 0, 1)) %>%
  mutate(FirstAI = ifelse(is.na(Three$DaysToFirstIns), 0, 1)) %>%
  mutate(KetoStatus20 = ifelse(Three$AvgKeto20_atleast10 >= 0.08, 1, 0)) %>%
  mutate(KetoGroup = ifelse(AvgKeto20_100 >= 0 & AvgKeto20_100 < 5.5, "1",
                            ifelse(AvgKeto20_100 >= 5.5 & AvgKeto20_100 <6.1, "2",
                                   ifelse(AvgKeto20_100 >= 6.1 & AvgKeto20_100 < 7.3, "3",
                                          ifelse(AvgKeto20_100 >= 7.3 & AvgKeto20_100 <8.0, "4",
                                                 ifelse(AvgKeto20_100 >= 8, "5", NA)))))) %>%
  mutate(KetoQua = ifelse(Three$AvgKeto20_atleast10 >= 0.076, 4,
                          ifelse(Three$AvgKeto20_atleast10 >= 0.061 &
                                   Three$AvgKeto20_atleast10 < 0.076, 3,
                                 ifelse(Three$AvgKeto20_atleast10 >= 0.055 &
                                          Three$AvgKeto20_atleast10 < 0.061, 2,
                                        ifelse(Three$AvgKeto20_atleast10 < 0.055, 1, NA))))) %>%
  mutate(KetoMaxDay = ifelse(Three$MaxKetoDIM20 >= 17, 4,
                             ifelse(Three$MaxKetoDIM20 >= 12 & Three$MaxKetoDIM20 < 17, 3,
                                    ifelse(Three$MaxKetoDIM20 >= 8 & Three$MaxKetoDIM20 < 12, 2,
                                           ifelse(Three$MaxKetoDIM20 <8, 1, NA))))) %>%
  mutate(KetoMaxDay2 = ifelse(Three$MaxKetoDIM20 >= 12, 1, 0)) %>%
  mutate(KetoMaxDay3 = ifelse(Three$MaxKetoDIM60 <= 14, "first2wk",
                              ifelse(Three$MaxKetoDIM60 > 14 & Three$MaxKetoDIM60 <= 30, "firstmth",
                                     ifelse(Three$MaxKetoDIM60 > 30, "secondmth", NA)))) %>%
  mutate(KetoOnset20 = ifelse(Three$onset20 > 20, "0", #0 means the cow never had milk BHB >= 0.08 within 20 DIM
                              ifelse(Three$onset20 > 14 & Three$onset20 <= 20, "3",
                                     ifelse(Three$onset20 > 7 & Three$onset20 <= 14, "2",
                                            ifelse(Three$onset20 <= 7, "1", NA))))) %>%
  mutate(KetoOnset20.1 = ifelse(Three$onset20 > 20, "0", #0 means the cow never had milk BHB >= 0.08 within 20 DIM
                                ifelse(Three$onset20 > 7 & Three$onset20 <= 20, "2and3",
                                       ifelse(Three$onset20 <= 7, "1", NA)))) %>%
  mutate(Onset20 = ifelse(Three$onset20 < 20, 1, 0))

# CLA vs AvgKeto20_100
fit <- survfit(Surv(DaysToCLA, FirstCLA) ~ cut(AvgKeto20_100, c(0, 5.5, 6.1, 7.3, 8, 42)) , data=survival)
plot(fit,
     lty = c(1, 2, 3, 4, 5),
     col = c("yellow", "blue", "green", "red", "black"),
     xlab = "HNDaysToCLA (DIM)",
     ylab = "Survival probability",
     main = "Kaplan-Meier curve of AvgKeto20 to HNDaysToCLA")
legend("topright",
       inset = 0.005,
       c("Avg mBHB 0.03-0.055 mmol/L",
         "Avg mBHB 0.055-0.061 mmol/L",
         "Avg mBHB 0.061-0.073 mmol/L",
         "Avg mBHB 0.073-0.08 mmol/L",
         "Avg mBHB 0.08-0.41 mmol/L"),
       lty=c(1, 2, 3, 4, 5),
       col = c("yellow", "blue", "green", "red"))

# CLA vs KetoOnset20
fit1 <- survfit(Surv(DaysToCLA, FirstCLA)~KetoOnset20, data=survival)
plot(fit1,
     lty = c(1, 2, 3, 4),
     col = c("yellow", "blue", "green", "red"),
     xlab = "Onset of ketosis with 20 d (DIM)",
     ylab = "Survival probability",
     main = "Kaplan-Meier curve of onset of ketosis to HNDaysToCLA")
legend("topright",
       inset = 0.005,
       c("Never had ketosis",
         "onset in 1st wk",
         "onset in 2nd wk",
         "onset in 3rd wk"),
       lty=c(1, 2, 3, 4),
       col = c("yellow", "blue", "green", "red"))

# Onset20
fit2 <- survfit(Surv(onset20, Onset20)~1, survival)
plot(fit2, xlab = "Onset of ketosis within 20 d (DIM)",
     ylab = "Survival probability",
     main = "Kaplan-Meier curve of onset of ketosis")

# Onset20 and KetoGroup
fit3 <- survfit(Surv(onset20, Onset20)~KetoGroup, survival)
plot(fit3, xlab = "Onset of ketosis within 20 d (DIM)",
     lty = c(1, 2, 3, 4, 5),
     col = c("yellow", "blue", "green", "red", "black"))
legend("topright",
       inset = 0.005,
       c("Avg mBHB 0.03-0.055 mmol/L",
         "Avg mBHB 0.055-0.061 mmol/L",
         "Avg mBHB 0.061-0.073 mmol/L",
         "Avg mBHB 0.073-0.08 mmol/L",
         "Avg mBHB 0.08-0.41 mmol/L"),
       lty=c(1, 2, 3, 4, 5),
       col = c("yellow", "blue", "green", "red", "black"))
df2 <- survival %>%
  select(FarmNo, CowId, onset20, Onset20, KetoGroup)


#########
#  CLA  #
#########

df.20keto.repro <- Three %>%
  select(FarmNo, CowId, ad_Lactation, DaysToCLA, DaysToFirstHeat, DaysToFirstIns,
         AvgKeto20_100, onset20, MaxKeto20, MaxKetoDIM20, nHighKeto20, KetoDur20) %>%
  mutate(KetoCow.avg20 = ifelse(AvgKeto20_100 >= 8, "Yes", "No"))
df1 <- df.20keto.repro %>%
  filter(KetoCow.avg20 == "Yes")

# plot days to CLA with onset of ketosis within 20 d
plot(df1$DaysToCLA~df1$onset20)
ggplot(data = df.20keto.repro, aes(x = factor(onset20), y = DaysToCLA, fill = KetoCow.avg20)) +
  geom_boxplot() +
  xlab("Onset of ketosis within 20 DIM (DIM)") +
  ylab("Days from calving to resumption of cyclicity") +
  theme_classic() +
  labs(fill = "KetoCow")

# DaysToCLA
ggplot(data=survival, aes(x=DaysToCLA))+
  geom_histogram(breaks=seq(0, 250, by=5),
                 fill="lightblue")+
  #facet_wrap(Three$FarmNo) +
  ggtitle("Time from Calving to Resumption of Cyclicity")+
  xlab("day")+
  ylab("#cows")+
  theme_classic()
summary(survival$DaysToCLA)
var(survival$DaysToCLA)
sd(survival$DaysToCLA)

# DaysToCLA - survival model
m.cla0 <- coxph(Surv(DaysToCLA, FirstCLA)~AvgKeto20_100 +
                      MaxKetoDIM20 +
                      ad_Lactation +
                      PeakDFC +
                      AvgMY60 +
                      CalvingSeason +
                      cut(CowNo, c(0, 100, 180, 1000)) +
                      frailty(FarmNo), data = survival)
m.cla0

m.cla0.1 <- coxph(Surv(DaysToCLA, FirstCLA)~AvgKeto20_100 +
                   KetoOnset20+
                   ad_Lactation +
                   PeakDFC +
                   AvgMY60 +
                   CalvingSeason +
                   cut(CowNo, c(0, 100, 180, 1000)) +
                   frailty(FarmNo), data = survival)
m.cla0.1
anova(m.cla0, m.cla0.1) #model with onset is better than maxketodim20

m.cla0.1.1 <- coxph(Surv(DaysToCLA, FirstCLA)~AvgKeto20_100 +
                    KetoOnset20.1+
                    ad_Lactation +
                    PeakDFC +
                    AvgMY60 +
                    CalvingSeason +
                    cut(CowNo, c(0, 100, 180, 1000)) +
                    frailty(FarmNo), data = survival)
m.cla0.1.1
anova(m.cla0.1, m.cla0.1.1) # model m.cla0.1 is better (categorize onset into 4 groups)

## remove AvgMY60
m.cla1 <- coxph(Surv(DaysToCLA, FirstCLA)~AvgKeto20_100 +
                  KetoOnset20 +
                  ad_Lactation +
                  PeakDFC +
                  CalvingSeason +
                  cut(CowNo, c(0, 100, 180, 1000)) +
                  frailty(FarmNo), data = survival)
m.cla1

summary(survival$AvgKeto20_100)
## categorize avgketo20
m.cla1.1 <- coxph(Surv(DaysToCLA, FirstCLA)~cut(AvgKeto20_100, c(0, 5.5, 6.1, 7.3, 8.0, 42)) +
                    KetoOnset20 +
                    ad_Lactation +
                    PeakDFC +
                    CalvingSeason +
                    cut(CowNo, c(0, 100, 180, 1000)) +
                    frailty(FarmNo), data = survival)
m.cla1.1 # onset of ketosis become not significantly different among groups


anova(m.cla1, m.cla1.1)

m.cla1.2 <- coxph(Surv(DaysToCLA, FirstCLA)~cut(AvgKeto20_100, c(0, 5.5, 6.1, 7.3, 8.0, 42)) +
                    MaxKetoDIM20+
                    ad_Lactation +
                    PeakDFC +
                    CalvingSeason +
                    cut(CowNo, c(0, 100, 180, 1000)) +
                    frailty(FarmNo), data = survival)
m.cla1.2

m.cla1.3 <- coxph(Surv(DaysToCLA, FirstCLA)~cut(AvgKeto20_100, c(0, 5.5, 6.1, 7.3, 8.0, 42)) +
                    onset20+
                    ad_Lactation +
                    PeakDFC +
                    CalvingSeason +
                    cut(CowNo, c(0, 100, 180, 1000)) +
                    frailty(FarmNo), data = survival)
m.cla1.3

# remove onset
m.cla2 <- coxph(Surv(DaysToCLA, FirstCLA)~cut(AvgKeto20_100, c(0, 5.5, 6.1, 7.3, 8.0, 42)) +
                    ad_Lactation +
                    PeakDFC +
                    CalvingSeason +
                    cut(CowNo, c(0, 100, 180, 1000)) +
                    frailty(FarmNo), data = survival)
m.cla2

# remove farmsize (CowNo)
m.cla3 <- coxph(Surv(DaysToCLA, FirstCLA)~cut(AvgKeto20_100, c(0, 5.5, 6.1, 7.3, 8.0, 42)) +
                  ad_Lactation +
                  PeakDFC +
                  CalvingSeason +
                  frailty(FarmNo), data = survival)
m.cla3
anova(m.cla2, m.cla3) #same loglikelihood and m.cla3 is simpler

# remove calving season
m.cla4 <- coxph(Surv(DaysToCLA, FirstCLA)~cut(AvgKeto20_100, c(0, 5.5, 6.1, 7.3, 8.0, 42)) +
                  ad_Lactation +
                  PeakDFC +
                  frailty(FarmNo), data = survival)
m.cla4
anova(m.cla3, m.cla4) # m.cla3 is significantly better than model cla 4

# check interaction
m.cla5 <- coxph(Surv(DaysToCLA, FirstCLA)~cut(AvgKeto20_100, c(0, 5.5, 6.1, 7.3, 8.0, 42)) +
                            cut(AvgKeto20_100, c(0, 5.5, 6.1, 7.3, 8.0, 42))*KetoOnset20 +
                            ad_Lactation +
                            PeakDFC +
                            CalvingSeason +
                            frailty(FarmNo), data = survival)
m.cla5


############################
# code data.frame survival #
############################

CLA <- survival %>%
  mutate(KetoGroup = ifelse(AvgKeto20_100 >= 0 & AvgKeto20_100 < 5.5, "1",
                            ifelse(AvgKeto20_100 >= 5.5 & AvgKeto20_100 <6.1, "2",
                                   ifelse(AvgKeto20_100 >= 6.1 & AvgKeto20_100 < 7.3, "3",
                                          ifelse(AvgKeto20_100 >= 7.3 & AvgKeto20_100 <8.0, "4",
                                                 ifelse(AvgKeto20_100 >= 8, "5", NA))))))
summary(CLA$KetoGroup)
str(CLA$KetoGroup)

m.cla5 <- coxph(Surv(DaysToCLA, FirstCLA)~KetoGroup +
                  KetoGroup*KetoOnset20 +
                  ad_Lactation +
                  PeakDFC +
                  CalvingSeason +
                  frailty(FarmNo), data = CLA)
m.cla5
anova(m.cla3, m.cla5) #m.cla5 is significantly better than m.cla3

# use categorized AvgKeto (KetoGroup)
## with MaxKetoDIM20
cla1 <- coxph(Surv(DaysToCLA, FirstCLA)~KetoGroup +
                MaxKetoDIM20 +
                ad_Lactation +
                PeakDFC +
                CalvingSeason +
                frailty(FarmNo), data = CLA)
cla1

## with onset20
cla2 <- coxph(Surv(DaysToCLA, FirstCLA)~KetoGroup +
                onset20 +
                ad_Lactation +
                PeakDFC +
                CalvingSeason +
                frailty(FarmNo), data = CLA)
cla2

## without MaxKetoDIM20 nor onset20
cla3 <- coxph(Surv(DaysToCLA, FirstCLA)~KetoGroup +
                ad_Lactation +
                PeakDFC +
                CalvingSeason +
                frailty(FarmNo), data = CLA)
cla3

# AvgKeto as a continuous variable
cla4 <- coxph(Surv(DaysToCLA, FirstCLA)~AvgKeto20_100 +
                ad_Lactation +
                PeakDFC +
                CalvingSeason +
                frailty(FarmNo), data = CLA)
cla4

anova(cla3, cla4) # cla3 is significantly better than cla4

# AvgKeto + KetoOnset20
cla5 <- coxph(Surv(DaysToCLA, FirstCLA)~AvgKeto20_100 +
                KetoOnset20 +
                ad_Lactation +
                PeakDFC +
                CalvingSeason +
                frailty(FarmNo), data = CLA)
cla5
anova(cla4, cla5) # cla5 is better than cla4
anova(cla3, cla5) # cla3 and cla5 are similar models

# use KetoGroup + KetoOnset20
cla6 <- coxph(Surv(DaysToCLA, FirstCLA)~KetoGroup +
                KetoOnset20 +
                ad_Lactation +
                PeakDFC +
                CalvingSeason +
                frailty(FarmNo), data = CLA)
cla6
anova(cla3, cla6) # no difference and cla6 is more complicated than cla3. cla3 is a better model

# lactation as a categorical variable
cla7 <- coxph(Surv(DaysToCLA, FirstCLA)~KetoGroup +
                factor(ad_Lactation) +
                PeakDFC +
                CalvingSeason +
                frailty(FarmNo), data = CLA)
cla7
anova(cla3, cla7)

# onset20 as a continuous variable
cla8 <- coxph(Surv(DaysToCLA, FirstCLA)~KetoGroup +
                onset20 +
                factor(ad_Lactation) +
                PeakDFC +
                CalvingSeason +
                frailty(FarmNo), data = CLA)
cla8
anova(cla7, cla8) # same, but cla7 is simpler

## Kaplan-Meier curves for each explanatory variables
# CLA vs AvgKeto20_100
fit <- survfit(Surv(DaysToCLA, FirstCLA) ~ cut(AvgKeto20_100, c(3, 6.1, 8, 15, 45)) , data=survival)
plot(fit,
     lty = c(1, 2, 3, 4),
     col = c("yellow", "blue", "green", "red"),
     xlab = "HNDaysToCLA (DIM)",
     ylab = "Survival probability",
     main = "Kaplan-Meier curve of AvgKeto20 to HNDaysToCLA")
legend("topright",
       inset = 0.005,
       c("BHB 0.03-0.061 mmol/L", "BHB 0.062-0.08 mmol/L", "BHB 0.08-0.15 mmol/L", "BHB 0.15-0.41 mmol/L"),
       lty=c(1, 2, 3, 4, 5),
       col = c("yellow", "blue", "green", "red"))
