rm(list=ls(pattern="milk"))
plot(survival$DaysToCLA, survival$AvgKeto20_100,
     xlab = "Time from calving to resupmtion of cyclicity",
     ylab = "Average milk BHB*100 (mmol/L)")
boxplot(survival$DaysToFirstHeat~survival$KetoStatus20,
        xlab = "Average milk BHB < 0.08: 0; >=0.08: 1",
        ylab = "Time from calving to first cyclicity")


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

# code data.frame survival
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
