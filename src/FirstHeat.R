Three <- read_excel("../data/raw/Three.xlsx")

survival <- Three %>%
  mutate(FirstCLA = ifelse(is.na(Three$DaysToCLA), 0, 1)) %>%
  mutate(FirstHeat = ifelse(is.na(Three$DaysToFirstHeat), 0, 1)) %>%
  mutate(FirstAI = ifelse(is.na(Three$DaysToFirstIns), 0, 1)) %>%
  mutate(KetoStatus20 = ifelse(Three$AvgKeto20_atleast10 >= 0.08, 1, 0)) %>%
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
                                       ifelse(Three$onset20 <= 7, "1", NA))))
Heat <- survival %>%
  mutate(KetoGroup = ifelse(AvgKeto20_100 >= 0 & AvgKeto20_100 < 5.5, "1",
                            ifelse(AvgKeto20_100 >= 5.5 & AvgKeto20_100 <6.1, "2",
                                   ifelse(AvgKeto20_100 >= 6.1 & AvgKeto20_100 < 7.3, "3",
                                          ifelse(AvgKeto20_100 >= 7.3 & AvgKeto20_100 <8.0, "4",
                                                 ifelse(AvgKeto20_100 >= 8, "5", NA))))))


#############
# FirstHeat #
#############

# plot onset
plot(survival$DaysToFirstHeat~survival$onset20)
ggplot(data = survival, aes(x = factor(onset20), y = DaysToFirstHeat)) +
  geom_boxplot() +
  xlab("Onset of ketosis within 20 DIM (DIM)") +
  ylab("Days from calving to resumption of cyclicity") +
  theme_classic() +
  labs(fill = "KetoCow")

heat <- coxph(Surv(DaysToFirstHeat, FirstHeat)~AvgKeto20_100 +
                onset20 +
                MaxKetoDIM20 +
                ad_Lactation +
                PeakDFC +
                AvgMY60 +
                CalvingSeason +
                cut(CowNo, c(0, 100, 180, 1000)) +
                frailty(FarmNo), data = Heat)
heat

# remove AvgMY60

heat0 <- coxph(Surv(DaysToFirstHeat, FirstHeat)~AvgKeto20_100 +
                 onset20 +
                 MaxKetoDIM20 +
                 ad_Lactation +
                 PeakDFC +
                 CalvingSeason +
                 cut(CowNo, c(0, 100, 180, 1000)) +
                 frailty(FarmNo), data = Heat)
heat0

heat1 <- coxph(Surv(DaysToFirstHeat, FirstHeat)~AvgKeto20_100 +
                 MaxKetoDIM20 +
                 ad_Lactation +
                 PeakDFC +
                 CalvingSeason +
                 cut(CowNo, c(0, 100, 180, 1000)) +
                 frailty(FarmNo), data = Heat)
heat1

# with onset
heat2 <- coxph(Surv(DaysToFirstHeat, FirstHeat)~AvgKeto20_100 +
                 onset20 +
                 ad_Lactation +
                 PeakDFC +
                 CalvingSeason +
                 cut(CowNo, c(0, 100, 180, 1000)) +
                 frailty(FarmNo), data = Heat)
heat2
anova(heat1, heat2) # model heat2 is better than heat1

# categorized onset20
heat3 <- coxph(Surv(DaysToFirstHeat, FirstHeat)~AvgKeto20_100 +
                          KetoOnset20 +
                          ad_Lactation +
                          PeakDFC +
                          CalvingSeason +
                          cut(CowNo, c(0, 100, 180, 1000)) +
                          frailty(FarmNo), data = Heat)
heat3

# categorized Avgketo20_100
heat4 <- coxph(Surv(DaysToFirstHeat, FirstHeat)~KetoGroup +
                 KetoOnset20 +
                 ad_Lactation +
                 PeakDFC +
                 CalvingSeason +
                 cut(CowNo, c(0, 100, 180, 1000)) +
                 frailty(FarmNo), data = Heat)
heat4 #KetoOnset20 become less significant
anova(heat3, heat4) #model heat3 and heat4 are similar

# remove KetoOnset20
heat5 <- coxph(Surv(DaysToFirstHeat, FirstHeat)~KetoGroup +
                 ad_Lactation +
                 PeakDFC +
                 CalvingSeason +
                 cut(CowNo, c(0, 100, 180, 1000)) +
                 frailty(FarmNo), data = Heat)
heat5
anova(heat4, heat5) #model heat4 is better

# remove farmsize
heat6 <- coxph(Surv(DaysToFirstHeat, FirstHeat)~KetoGroup +
          KetoOnset20 +
          ad_Lactation +
          PeakDFC +
          CalvingSeason +
          frailty(FarmNo), data = Heat)
heat6
anova(heat5, heat6) #similar models but heat6 is simpler
# KetoOnset20 is not significant at all


# add onset20 as a continuous variable
heat8 <-coxph(Surv(DaysToFirstHeat, FirstHeat)~KetoGroup +
                onset20 +
                ad_Lactation +
                PeakDFC +
                CalvingSeason +
                frailty(FarmNo), data = Heat)
heat8
anova(heat6, heat8) #heat6 and heat8 are similar

# categorize lactation
heat9 <- coxph(Surv(DaysToFirstHeat, FirstHeat)~KetoGroup +
                   onset20 +
                   factor(ad_Lactation) +
                   PeakDFC +
                   CalvingSeason +
                   frailty(FarmNo), data = Heat)
heat9
anova(heat8, heat9) # heat8 and heat9 are similar

# categorize lactation and remove onset20
heat10 <- coxph(Surv(DaysToFirstHeat, FirstHeat)~KetoGroup +
                 factor(ad_Lactation) +
                 PeakDFC +
                 CalvingSeason +
                 frailty(FarmNo), data = Heat)
heat10
anova(heat9, heat10) # model heat9 is better
anova(heat8, heat10) # heat8 and heat 10 are similar

# categorized avgketo, lactation, onset
heat10.1 <- coxph(Surv(DaysToFirstHeat, FirstHeat)~KetoGroup +
                    KetoOnset20 +
                    factor(ad_Lactation) +
                    PeakDFC +
                    CalvingSeason +
                    frailty(FarmNo), data = Heat)
heat10.1
anova(heat10, heat10.1) #heat10.1 is a better model (?)
anova(heat9, heat10.1)  #heat9 and heat10.1 are similar models

### model heat9 and model10.1 seems to be a nice model -> Categorized onset20 is not significant, yet continous onset20 is significant
# are there interaction between average ketosis and onset of ketosis?

#############################
# consider only KetoOnset20 #
#############################

heat11 <- coxph(Surv(DaysToFirstHeat, FirstHeat)~KetoOnset20 +
                  factor(ad_Lactation) +
                  PeakDFC +
                  CalvingSeason +
                  frailty(FarmNo), data = Heat)
heat11
anova(heat5, heat11) #heat5 is a better model


######################################
# interaction: KetoGroup*KetoOnset20 #
######################################

heat12 <-coxph(Surv(DaysToFirstHeat, FirstHeat)~KetoGroup*KetoOnset20 +
                 factor(ad_Lactation) +
                 PeakDFC +
                 CalvingSeason +
                 frailty(FarmNo), data = Heat)
heat12
anova(heat9, heat12)
anova(heat10.1, heat12)
