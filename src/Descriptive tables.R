summary(Three$AvgKeto20_atleast10)
summary(Three$nHighKeto20)
summary(Three$MaxKeto20)
summary(Three$MaxKetoDIM20)
summary(Three$MaxKetoDIM60)

df.keto <- Three %>%
  select(FarmNo, CowId, AvgKeto20_atleast10, nHighKeto20, MaxKeto20, MaxKetoDIM20, MaxKetoDIM60, DaysToCLA, DaysToFirstHeat, DaysToFirstIns)

### ketosis dynamics WITHIN 20 DIM
# Group1: categorize ketosis by AvgKeto20_atleast10 = 1st Qu < 0.054; median: 0.054-0.061; 3rd Qu: 0.061-0.075; over 3rd Qu: >0.075
# Group2: categorize ketosis by nHighKeto20 = range 1: 0 (never 0.08); 2: 1-5; 3: >5
# Group3: categorize ketosis by MaxKeto20 = range 1: MaxKeto20 <0.07; 2: 0.07-0.09; 3: 0.09-0.12; 4: >=0.12
# Group4: categorize ketosis by MaxKetoDFC20 = early: MaxKetoDFC20 < 5; middle: MaxKetoDFC 5-15; late: > 15
keto <- df.keto %>%
  mutate(KetoGroup1 = ifelse(df.keto$AvgKeto20_atleast10 < 0.054, "1st Qu",
                                 ifelse(df.keto$AvgKeto20_atleast10 >= 0.054 & df.keto$AvgKeto20_atleast10 < 0.061, "median",
                                        ifelse(df.keto$AvgKeto20_atleast10 >= 0.061 & df.keto$AvgKeto20_atleast10 < 0.075, "3rd Qu",
                                               ifelse(df.keto$AvgKeto20_atleast10 >=0.075, "over 3rd Qu", df.keto$AvgKeto20_atleast10)))), .after="AvgKeto20_atleast10") %>%
  mutate(KetoGroup2 = ifelse(df.keto$nHighKeto20 == 0, "never",
                                 ifelse(df.keto$nHighKeto20 >= 1 & df.keto$nHighKeto20 <= 5, "seldom",
                                        ifelse(df.keto$nHighKeto20 > 5 & df.keto$nHighKeto20 < 11, "sometimes",
                                               ifelse(df.keto$nHighKeto20 >= 11, "usually", df.keto$nHighKeto20)))), .after="nHighKeto20") %>%
  mutate(KetoGroup3 = ifelse(df.keto$MaxKeto20 < 0.07, "low",
                                 ifelse(df.keto$MaxKeto20 >= 0.07 & df.keto$MaxKeto20 < 0.09, "middle",
                                        ifelse(df.keto$MaxKeto20 >= 0.09 & df.keto$MaxKeto20 <0.12, "higher",
                                               ifelse(df.keto$MaxKeto20 >=0.12, "very high", df.keto$MaxKeto20)))), .after="MaxKeto20") %>%
  mutate(KetoGroup4 = ifelse(df.keto$MaxKetoDIM20 < 8, "first week",
                                 ifelse(df.keto$MaxKetoDIM20 >= 8 & df.keto$MaxKetoDIM20 <= 14, "second week",
                                        ifelse(df.keto$MaxKetoDIM20 >14, "late", df.keto$MaxKetoDIM20))), .after="MaxKetoDIM20") %>%
  mutate(KetoGroup5 = ifelse(df.keto$MaxKetoDIM60 < 15, "first 2 weeks",
                             ifelse(df.keto$MaxKetoDIM60 >= 15 & df.keto$MaxKetoDIM60 <= 29, "3rd & 4th weeks",
                                    ifelse(df.keto$MaxKetoDIM60 >29, "over a month", df.keto$MaxKetoDIM60))), .after="MaxKetoDIM60")

df.group1 <- keto %>%
  group_by(KetoGroup1) %>%
  summarize(meanCLA = mean(DaysToCLA),
            sdCLA = sd(DaysToCLA),
            count = n())
df.group1.firstheat <- keto %>%
  group_by(KetoGroup1) %>%
  summarize(meanFirstHeat = mean(DaysToFirstHeat),
            sdFirstHeat = sd(DaysToFirstHeat),
            count = n())
df.group1.firstai <- keto %>%
  group_by(KetoGroup1) %>%
  summarize(meanFirstAI = mean(DaysToFirstIns, na.rm = TRUE),
            sdFirstAI = sd(DaysToFirstIns, na.rm = TRUE),
            count = n())
# ANOVA test for KetoGroup1 and DaysToCLA
boxplot(keto$DaysToCLA~keto$KetoGroup1)
df.fit1 <- lm(keto$DaysToCLA~keto$KetoGroup1)
anova(df.fit1)
# post hoc test
df.fit1.post <- aov(keto$DaysToCLA~keto$KetoGroup1)
TukeyHSD(df.fit1.post)
# ANOVA test for KetoGroup1 and DaysToFirstHeat
boxplot(keto$DaysToFirstHeat~keto$KetoGroup1)
df.fit1 <- lm(keto$DaysToFirstHeat~keto$KetoGroup1)
anova(df.fit1)
# post hoc test
df.fit1.post <- aov(keto$DaysToFirstHeat~keto$KetoGroup1)
TukeyHSD(df.fit1.post)
# ANOVA test for KetoGroup1 and DaysToFirstIns
boxplot(keto$DaysToFirstIns~keto$KetoGroup1)
df.fit1 <- lm(keto$DaysToFirstIns~keto$KetoGroup1)
anova(df.fit1)
# post hoc test
df.fit1.post <- aov(keto$DaysToFirstIns~keto$KetoGroup1)
TukeyHSD(df.fit1.post)

df.group2 <- keto %>%
  group_by(KetoGroup2) %>%
  summarize(meanCLA = mean(DaysToCLA),
            sdCLA = sd(DaysToCLA),
            count = n())
df.group2.firstheat <- keto %>%
  group_by(KetoGroup2) %>%
  summarize(meanFirstHeat = mean(DaysToFirstHeat),
            sdFirstHeat = sd(DaysToFirstHeat),
            count = n())
df.group2.firstai <- keto %>%
  group_by(KetoGroup2) %>%
  summarize(meanFirstAI = mean(DaysToFirstIns, na.rm = TRUE),
            sdFirstAI = sd(DaysToFirstIns, na.rm = TRUE),
            count = n())
# ANOVA test for KetoGroup2 and DaysToCLA
boxplot(keto$DaysToCLA~keto$KetoGroup2)
df.fit2 <- lm(keto$DaysToCLA~keto$KetoGroup2)
anova(df.fit2)
# post hoc test
df.fit2.post <- aov(keto$DaysToCLA~keto$KetoGroup2)
TukeyHSD(df.fit2.post)
# ANOVA test for KetoGroup2 and DaysToFirstHeat
boxplot(keto$DaysToFirstHeat~keto$KetoGroup2)
df.fit2 <- lm(keto$DaysToFirstHeat~keto$KetoGroup2)
anova(df.fit2)
# post hoc test
df.fit2.post <- aov(keto$DaysToFirstHeat~keto$KetoGroup2)
TukeyHSD(df.fit2.post)
# ANOVA test for KetoGroup2 and DaysToFirstIns
boxplot(keto$DaysToFirstIns~keto$KetoGroup2)
df.fit2 <- lm(keto$DaysToFirstIns~keto$KetoGroup2)
anova(df.fit2)
# post hoc test
df.fit2.post <- aov(keto$DaysToFirstIns~keto$KetoGroup2)
TukeyHSD(df.fit2.post)

df.group3 <- keto %>%
  group_by(KetoGroup3) %>%
  summarize(meanCLA = mean(DaysToCLA),
            sdCLA = sd(DaysToCLA),
            count = n())
df.group3.firstheat <- keto %>%
  group_by(KetoGroup3) %>%
  summarize(meanFirstHeat = mean(DaysToFirstHeat),
            sdFirstHeat = sd(DaysToFirstHeat),
            count = n())
df.group3.firstai <- keto %>%
  group_by(KetoGroup3) %>%
  summarize(meanFirstAI = mean(DaysToFirstIns, na.rm = TRUE),
            sdFirstAI = sd(DaysToFirstIns, na.rm = TRUE),
            count = n())
# ANOVA test for KetoGroup3 and DaysToCLA
boxplot(keto$DaysToCLA~keto$KetoGroup3)
df.fit3 <- lm(keto$DaysToCLA~keto$KetoGroup3)
anova(df.fit3)
# post hoc test
df.fit3.post <- aov(keto$DaysToCLA~keto$KetoGroup3)
TukeyHSD(df.fit3.post)
# ANOVA test for KetoGroup3 and DaysToFirstHeat
boxplot(keto$DaysToFirstHeat~keto$KetoGroup3)
df.fit3 <- lm(keto$DaysToFirstHeat~keto$KetoGroup3)
anova(df.fit3)
# post hoc test
df.fit3.post <- aov(keto$DaysToFirstHeat~keto$KetoGroup3)
TukeyHSD(df.fit3.post)
# ANOVA test for KetoGroup3 and DaysToFirstIns
boxplot(keto$DaysToFirstIns~keto$KetoGroup3)
df.fit3 <- lm(keto$DaysToFirstIns~keto$KetoGroup3)
anova(df.fit3)
# post hoc test
df.fit3.post <- aov(keto$DaysToFirstIns~keto$KetoGroup3)
TukeyHSD(df.fit3.post)

df.group4 <- keto %>%
  group_by(KetoGroup4) %>%
  summarize(meanCLA = mean(DaysToCLA),
            sdCLA = sd(DaysToCLA),
            count = n())
df.group4.firstheat <- keto %>%
  group_by(KetoGroup4) %>%
  summarize(meanFirstHeat = mean(DaysToFirstHeat),
            sdFirstHeat = sd(DaysToFirstHeat),
            count = n())
df.group4.firstai <- keto %>%
  group_by(KetoGroup4) %>%
  summarize(meanFirstAI = mean(DaysToFirstIns, na.rm = TRUE),
            sdFirstAI = sd(DaysToFirstIns, na.rm = TRUE),
            count = n())
# ANOVA test for KetoGroup4 and DaysToCLA
boxplot(keto$DaysToCLA~keto$KetoGroup4)
df.fit4 <- lm(keto$DaysToCLA~keto$KetoGroup4)
anova(df.fit4)
# post hoc test
df.fit4.post <- aov(keto$DaysToCLA~keto$KetoGroup4)
TukeyHSD(df.fit4.post)
# ANOVA test for KetoGroup4 and DaysToFirstHeat
boxplot(keto$DaysToFirstHeat~keto$KetoGroup4)
df.fit4 <- lm(keto$DaysToFirstHeat~keto$KetoGroup4)
anova(df.fit4)
# post hoc test
df.fit4.post <- aov(keto$DaysToFirstHeat~keto$KetoGroup4)
TukeyHSD(df.fit4.post)
# ANOVA test for KetoGroup4 and DaysToFirstIns
boxplot(keto$DaysToFirstIns~keto$KetoGroup4)
df.fit4 <- lm(keto$DaysToFirstIns~keto$KetoGroup4)
anova(df.fit4)
# post hoc test
df.fit4.post <- aov(keto$DaysToFirstIns~keto$KetoGroup4)
TukeyHSD(df.fit4.post)

df.group5 <- keto %>%
  group_by(KetoGroup5) %>%
  summarize(meanCLA = mean(DaysToCLA),
            sdCLA = sd(DaysToCLA),
            count = n())
df.group5.firstheat <- keto %>%
  group_by(KetoGroup5) %>%
  summarize(meanFirstHeat = mean(DaysToFirstHeat),
            sdFirstHeat = sd(DaysToFirstHeat),
            count = n())
df.group5.firstai <- keto %>%
  group_by(KetoGroup5) %>%
  summarize(meanFirstAI = mean(DaysToFirstIns, na.rm = TRUE),
            sdFirstAI = sd(DaysToFirstIns, na.rm = TRUE),
            count = n())
# ANOVA test for KetoGroup5 and DaysToCLA
boxplot(keto$DaysToCLA~keto$KetoGroup5)
df.fit5 <- lm(keto$DaysToCLA~keto$KetoGroup5)
anova(df.fit5)
# post hoc test
df.fit5.post <- aov(keto$DaysToCLA~keto$KetoGroup5)
TukeyHSD(df.fit5.post)
# ANOVA test for KetoGroup5 and DaysToFirstHeat
boxplot(keto$DaysToFirstHeat~keto$KetoGroup5)
df.fit5 <- lm(keto$DaysToFirstHeat~keto$KetoGroup5)
anova(df.fit5)
# post hoc test
df.fit5.post <- aov(keto$DaysToFirstHeat~keto$KetoGroup5)
TukeyHSD(df.fit5.post)
# ANOVA test for KetoGroup5 and DaysToFirstIns
boxplot(keto$DaysToFirstIns~keto$KetoGroup5)
df.fit5 <- lm(keto$DaysToFirstIns~keto$KetoGroup5)
anova(df.fit5)
# post hoc test
df.fit5.post <- aov(keto$DaysToFirstIns~keto$KetoGroup5)
TukeyHSD(df.fit5.post)

