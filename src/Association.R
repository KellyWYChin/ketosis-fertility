###### Merge keto, reproevents, milk ###########################################
df.merge1 <- merge(repro, keto, by=c('FarmNo', 'CowId', 'Lactation'), all.x=TRUE, sort=F) %>%
  select(FarmNo, CowId, Lactation, CalvingDate, Season, InsNumber_N, PregStatus_1, DFC_CLA, DFCFirstHeat, DaysToFirstIns, ECC,`AvgKeto_1-20`, KetoCow1, AvgKeto10, KetoGroup1, KetoGroup1.1, KetoCow10, AvgKeto20, KetoDays, KetoCow2, MaxKetoDFC20, KetoGroup4, MaxKeto20, KetoGroup3, nHighKeto20, KetoGroup2, nHighKeto60, KetoGroup5, MaxKetoDFC60, KetoGroup7, MaxKeto60, KetoGroup6)
summary(df.merge1)
df.merge2 <- merge(df.merge1, milk_all, by=c('FarmNo', 'CowId', 'Lactation', 'CalvingDate'), all.x=TRUE, sort=F)
summary(df.merge2)
#exclude cows without AvgMilk20
df.merge3 <- df.merge2[!is.na(df.merge2$AvgMilk20), ]
#exclude cows without AvgKeto10
df.merge3 <- df.merge3[!is.na(df.merge3$AvgKeto10), ]
summary(df.merge3)

attach(df.merge1)
boxplot(AvgKeto10~Season, main="average milk BHB values within 20 DIM")
boxplot(AvgKeto10~PregStatus_1, xlab="pregnancy status after 1st AI", 
        names=c("non-pregnant", "pregnant"))
boxplot(AvgKeto10~PregStatus, xlab="pregnancy status within the cohort period", 
        names=c("non-pregnant", "pregnant"))
boxplot(AvgKeto10~Culled, names=c("alive", "culled"), 
        main="Culling after 150 DIM and average milk BHB values within 20 DIM")
t.test(log(AvgKeto10)~Culled)
by(df.merge1$AvgKeto10, df.merge1$Culled, describe)
by(df.merge1$AvgKeto10, df.merge1$PregStatus_1, describe)
by(df.merge1$AvgKeto10, df.merge1$Season, describe)

################################################################################
###### ANOVA test for KetoGroup 1.1 and DFC_CLA ################################
boxplot(df.merge3$DFC_CLA~df.merge3$KetoGroup1.1)
df.fit1 <- lm(df.merge3$DFC_CLA~factor(df.merge3$KetoGroup1.1))
anova(df.fit1)
# post hoc test
df.fit2 <- aov(df.merge3$DFC_CLA~factor(df.merge3$KetoGroup1.1))
TukeyHSD(df.fit2)
# log-transformation 
df.merge3 <- df.merge3 %>%
  add_column(logDFC_CLA = log(df.merge3$DFC_CLA), .after="DFC_CLA")
boxplot(df.merge3$logDFC_CLA~df.merge3$KetoGroup1.1)
df.fit3 <- lm(df.merge3$logDFC_CLA~factor(df.merge3$KetoGroup1.1))
anova(df.fit3)
df.fit4 <- aov(df.merge3$logDFC_CLA~factor(df.merge3$KetoGroup1.1))
TukeyHSD(df.fit4)
# check ANOVA assumption
e <- residuals(df.fit4)
boxplot(e)
qqnorm(e)
abline(mean(e), sd(e))
###### ANOVA test for KetoGroup 1.1 and DFCFirstHeat ###########################
# log-transformation 
df.merge3 <- df.merge3 %>%
  add_column(logDFCFirstHeat = log(df.merge3$DFCFirstHeat), .after="DFCFirstHeat")
boxplot(df.merge3$logDFCFirstHeat~df.merge3$KetoGroup1.1)
df.fit3 <- lm(df.merge3$logDFCFirstHeat~factor(df.merge3$KetoGroup1.1))
anova(df.fit3)
df.fit4 <- aov(df.merge3$logDFCFirstHeat~factor(df.merge3$KetoGroup1.1))
TukeyHSD(df.fit4)
# check ANOVA assumption
e <- residuals(df.fit4)
boxplot(e)
qqnorm(e)
abline(mean(e), sd(e))
###### ANOVA test for KetoGroup 1.1 and DaysToFirstIns #########################
# log-transformation 
df.merge3 <- df.merge3 %>%
  add_column(logDaysToFirstIns = log(df.merge3$DaysToFirstIns), .after="DaysToFirstIns")
boxplot(df.merge3$logDaysToFirstIns~df.merge3$KetoGroup1.1)
df.fit3 <- lm(df.merge3$logDaysToFirstIns~factor(df.merge3$KetoGroup1.1))
anova(df.fit3)
df.fit4 <- aov(df.merge3$logDaysToFirstIns~factor(df.merge3$KetoGroup1.1))
TukeyHSD(df.fit4)
# check ANOVA assumption
e <- residuals(df.fit4)
boxplot(e)
qqnorm(e)
abline(mean(e), sd(e))

# how many cows were ketotic cows and their culling status?
df.data <- as.data.frame(table(df.merge1$KetoCow10, df.merge1$Culled))
df.data1 <- df.merge1 %>%
  select(FarmNo, CowId, Lactation, AvgKeto10, KetoCow10, Culled) %>%
  filter(KetoCow10 == 0 & Culled == 1) %>%
  summarize(meanavgketo10 = mean(AvgKeto10),
            sdavgketo10 = sd(AvgKeto10), 
            count=n())
chisq.test(matrix(c(185, 515, 735, 2782), nrow=2, byrow=TRUE))

# how many cows were ketotic cows and their pregnant status after 1st AI?
df.data <- as.data.frame(table(df.merge3$KetoCow10, df.merge3$PregStatus_1))
df.data1 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, AvgKeto10, KetoCow10, PregStatus_1) %>%
  filter(KetoCow10 == 1 & PregStatus_1 == 0) %>%
  summarize(meanavgketo10 = mean(AvgKeto10),
            sdavgketo10 = sd(AvgKeto10), 
            count=n())
chisq.test(matrix(c(303, 1269, 529, 1864), nrow=2, byrow=TRUE))

#how many cows were ketotic cows and if they are early conceived cows?
df.data <- as.data.frame(table(df.merge2$KetoCow10, df.merge2$ECC))
df.data1 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, AvgKeto10, KetoCow10, ECC) %>%
  filter(KetoCow10 == 0 & ECC == 0) %>%
  summarize(meanavgketo10 = mean(AvgKeto10),
            sdavgketo10 = sd(AvgKeto10), 
            count=n())
chisq.test(matrix(c(132, 683, 700, 2450), nrow=2, byrow=TRUE))

# how many cows were categroized with KetoGroup 1 and their DFC_CLA?
summary(df.merge3$AvgKeto10)
df.data1 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, AvgKeto10, KetoGroup1, DFC_CLA) %>%
  group_by(KetoGroup1) %>%
  summarize(meanDFC_CAL = mean(DFC_CLA),
            sdDFC_CLA = sd(DFC_CLA), 
            count=n())
# how many cows were categroized with KetoGroup 1.1 and their DFC_CLA?
df.data1 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, AvgKeto10, KetoGroup1.1, DFC_CLA) %>%
  group_by(KetoGroup1.1) %>%
  summarize(meanDFC_CAL = mean(DFC_CLA),
            sdDFC_CLA = sd(DFC_CLA), 
            count=n())

# how many cows were categroized with KetoGroup 2 and their DFC_CLA?
# Group2: categorize ketosis by nHighKeto20 = range 1: 0 (never 0.08); 2: 1-5; 3: >5
summary(df.merge3$nHighKeto20)
df.data2 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, AvgKeto10, KetoGroup2, DFC_CLA) %>%
  group_by(KetoGroup2) %>%
  summarize(meanDFC_CAL = mean(DFC_CLA),
            sdDFC_CLA = sd(DFC_CLA), 
            count=n())

# how many cows were categroized with KetoGroup 3 and their DFC_CLA?
# Group3: categorize ketosis by MaxKeto20 = range 1: MaxKeto20 < 0.07; 2: 0.07-0.08; 3: 0.08-0.12; 4: >=0.12
summary(df.merge3$MaxKeto20)
df.data3 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, AvgKeto10, KetoGroup3, DFC_CLA) %>%
  group_by(KetoGroup3) %>%
  summarize(meanDFC_CAL = mean(DFC_CLA, na.rm=TRUE),
            sdDFC_CLA = sd(DFC_CLA, na.rm=TRUE), 
            count=n())

# how many cows were categroized with KetoGroup 4 and their DFC_CLA?
# Group4: categorize ketosis by MaxKetoDFC20 = early: MaxKetoDFC20 < 5; middle: MaxKetoDFC 5-15; late: > 15
summary(df.merge3$MaxKetoDFC20)
df.data4 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, AvgKeto10, KetoGroup4, DFC_CLA) %>%
  group_by(KetoGroup4) %>%
  summarize(meanDFC_CAL = mean(DFC_CLA, na.rm=TRUE),
            sdDFC_CLA = sd(DFC_CLA, na.rm=TRUE), 
            count=n())

#how many cows were categorized with KetoGroup 5 and their DFC_CLA?
# Group5: categorize ketosis by nHighKeto60 = range 1: 0 (never 0.08); seldom: 1-=5; sometimes: 5-=10; usually:>10
summary(df.merge3$nHighKeto60)
df.data5 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, AvgKeto10, KetoGroup5, DFC_CLA) %>%
  group_by(KetoGroup5) %>%
  summarize(meanDFC_CAL = mean(DFC_CLA, na.rm=TRUE),
            sdDFC_CLA = sd(DFC_CLA, na.rm=TRUE), 
            count=n())

#how many cows were categorized with KetoGroup 6 and their DFC_CLA?
# Group6: categorize ketosis by MaxKeto60 = range 1: MaxKeto20 <0.07; 2: 0.07-=0.08; 3: 0.08-0.12; 4: >=0.12
summary(df.merge3$MaxKeto60)
df.data6 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, AvgKeto10, KetoGroup6, DFC_CLA) %>%
  group_by(KetoGroup6) %>%
  summarize(meanDFC_CAL = mean(DFC_CLA, na.rm=TRUE),
            sdDFC_CLA = sd(DFC_CLA, na.rm=TRUE), 
            count=n())

#how many cows were categorized with KetoGroup 7 and their DFC_CLA?
# Group7: categorize ketosis by MaxKetoDFC60 = first week: MaxKetoDFC20 <= 7; first 2 weeks: MaxKetoDFC 7-=14; 1st month: 14-=30; 2nd month: >30
summary(df.merge3$MaxKetoDFC60)
df.data7 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, AvgKeto10, KetoGroup7, DFC_CLA) %>%
  group_by(KetoGroup7) %>%
  summarize(meanDFC_CAL = mean(DFC_CLA, na.rm=TRUE),
            sdDFC_CLA = sd(DFC_CLA, na.rm=TRUE), 
            count=n())

# how many cows were categroized with KetoGroup 1 and their DFCFirstHeat?
df.data1 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, AvgKeto10, KetoGroup1, DFCFirstHeat) %>%
  group_by(KetoGroup1) %>%
  summarize(meanDFCFirstHeat = mean(DFCFirstHeat, na.rm=TRUE),
            sdDFCFirstHeat = sd(DFCFirstHeat, na.rm=TRUE), 
            count=n())
# KetoGroup1.1 and DFCFirstHeat?
df.data1 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, AvgKeto10, KetoGroup1.1, DFCFirstHeat) %>%
  group_by(KetoGroup1.1) %>%
  summarize(meanDFCFirstHeat = mean(DFCFirstHeat, na.rm=TRUE),
            sdDFCFirstHeat = sd(DFCFirstHeat, na.rm=TRUE), 
            count=n())
# KetoGroup2 and DFCFirstHeat?
summary(df.merge1$nHighKeto20)
df.data2 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, AvgKeto10, KetoGroup2, DFCFirstHeat) %>%
  group_by(KetoGroup2) %>%
  summarize(meanDFCFirstHeat = mean(DFCFirstHeat, na.rm=TRUE),
            sdDFCFirstHeat = sd(DFCFirstHeat, na.rm=TRUE), 
            count=n())
# KetoGroup3 and DFCFirstHeat?
df.data3 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, AvgKeto10, KetoGroup3, DFCFirstHeat) %>%
  group_by(KetoGroup3) %>%
  summarize(meanDFCFirstHeat = mean(DFCFirstHeat, na.rm=TRUE),
            sdDFCFirstHeat = sd(DFCFirstHeat, na.rm=TRUE), 
            count=n())
# KetoGroup4 and DFCFirstHeat?
df.data4 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, AvgKeto10, KetoGroup4, DFCFirstHeat) %>%
  group_by(KetoGroup4) %>%
  summarize(meanDFCFirstHeat = mean(DFCFirstHeat, na.rm=TRUE),
            sdDFCFirstHeat = sd(DFCFirstHeat, na.rm=TRUE), 
            count=n())
# KetoGroup5 and DFCFirstHeat?
summary(df.merge3$nHighKeto60)
df.data5 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, AvgKeto10, KetoGroup5, DFCFirstHeat) %>%
  group_by(KetoGroup5) %>%
  summarize(meanDFCFirstHeat = mean(DFCFirstHeat, na.rm=TRUE),
            sdDFCFirstHeat = sd(DFCFirstHeat, na.rm=TRUE), 
            count=n())
# KetoGroup6 and DFCFirstHeat?
df.data6 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, AvgKeto10, KetoGroup6, DFCFirstHeat) %>%
  group_by(KetoGroup6) %>%
  summarize(meanDFCFirstHeat = mean(DFCFirstHeat, na.rm=TRUE),
            sdDFCFirstHeat = sd(DFCFirstHeat, na.rm=TRUE), 
            count=n())
# KetoGroup7 and DFCFirstHeat?
summary(df.merge3$MaxKetoDFC60)
df.data7 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, AvgKeto10, KetoGroup7, DFCFirstHeat) %>%
  group_by(KetoGroup7) %>%
  summarize(meanDFCFirstHeat = mean(DFCFirstHeat, na.rm=TRUE),
            sdDFCFirstHeat = sd(DFCFirstHeat, na.rm=TRUE), 
            count=n())


# how many cows were categroized with KetoGroup 1 and their DaysToFirstIns?
df.data1 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, AvgKeto10, KetoGroup1, DaysToFirstIns) %>%
  group_by(KetoGroup1) %>%
  summarize(meanDaysToFirstIns = mean(DaysToFirstIns, na.rm=TRUE),
            sdDaysToFirstIns = sd(DaysToFirstIns, na.rm=TRUE), 
            count=n())
# KetoGroup1.1 and DaysToFirstIns?
df.data1 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, AvgKeto10, KetoGroup1.1, DaysToFirstIns) %>%
  group_by(KetoGroup1.1) %>%
  summarize(meanDaysToFirstIns = mean(DaysToFirstIns, na.rm=TRUE),
            sdDaysToFirstIns = sd(DaysToFirstIns, na.rm=TRUE), 
            count=n())
# KetoGroup2 and DaysToFirstIns?
summary(df.merge3$nHighKeto20)
df.data2 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, AvgKeto10, KetoGroup2, DaysToFirstIns) %>%
  group_by(KetoGroup2) %>%
  summarize(meanDaysToFirstIns = mean(DaysToFirstIns, na.rm=TRUE),
            sdDaysToFirstIns = sd(DaysToFirstIns, na.rm=TRUE), 
            count=n())
# KetoGroup3 and DaysToFirstIns?
df.data3 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, AvgKeto10, KetoGroup3, DaysToFirstIns) %>%
  group_by(KetoGroup3) %>%
  summarize(meanDaysToFirstIns = mean(DaysToFirstIns, na.rm=TRUE),
            sdDaysToFirstIns = sd(DaysToFirstIns, na.rm=TRUE), 
            count=n())
# KetoGroup4 and DaysToFirstIns?
df.data4 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, AvgKeto10, KetoGroup4, DaysToFirstIns) %>%
  group_by(KetoGroup4) %>%
  summarize(meanDaysToFirstIns = mean(DaysToFirstIns, na.rm=TRUE),
            sdDaysToFirstIns = sd(DaysToFirstIns, na.rm=TRUE), 
            count=n())
# KetoGroup5 and DaysToFirstIns?
summary(df.merge3$nHighKeto60)
df.data5 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, AvgKeto10, KetoGroup5, DaysToFirstIns) %>%
  group_by(KetoGroup5) %>%
  summarize(meanDaysToFirstIns = mean(DaysToFirstIns, na.rm=TRUE),
            sdDaysToFirstIns = sd(DaysToFirstIns, na.rm=TRUE), 
            count=n())
# KetoGroup6 and DaysToFirstIns?
df.data6 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, AvgKeto10, KetoGroup6, DaysToFirstIns) %>%
  group_by(KetoGroup6) %>%
  summarize(meanDaysToFirstIns = mean(DaysToFirstIns, na.rm=TRUE),
            sdDaysToFirstIns = sd(DaysToFirstIns, na.rm=TRUE), 
            count=n())
# KetoGroup7 and DaysToFristIns?
summary(df.merge3$MaxKetoDFC60)
df.data7 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, AvgKeto10, KetoGroup7, DaysToFirstIns) %>%
  group_by(KetoGroup7) %>%
  summarize(meanDaysToFirstInS = mean(DaysToFirstIns, na.rm=TRUE),
            sdDaysToFirstIns = sd(DaysToFirstIns, na.rm=TRUE), 
            count=n())

###### Categorize milk data ####################################################
summary(df.merge3$PeakDim)
summary(df.merge3$PeakYield)
summary(df.merge3$AvgMilk20)
summary(df.merge3$sumMY20)
summary(df.merge3$AvgMilk60)
summary(df.merge3$sumMY60)
# MilkGroup1: categorized by PeakDim based on quantiles: 1st Qu <=35; 2nd Qu: >35 & <= 47, 3rd Qu: >47 & <= 67; 4th Qu: >67
# MilkGroup2: categorized by PeakYield based on quantiles: 1st Qu <=36; 2nd Qu: >36 & <= 43, 3rd Qu: >43 & <= 50; 4th Qu: >50
# MilkGroup3: categorized by AvgMilk20 based on quantiles: 1st Qu =< 25.6; 2nd Qu: >25.6 & <=32.8; 3rd Qu: >32.8 & <=39.3; 4th Qu: >39.3
# MilkGroup4: categorized by sumMY20 based on quantiles: 1st Qu <=432; 2nd Qu: >432 & <=556; 3rd Qu: >556 & <= 667; 4th Qu: >667
# MilkGroup5: categorized by AvgMilk60 based on quantiles: 1st Qu =< 30; 2nd Qu: >30 & <=37; 3rd Qu: >37 & <=43; 4th Qu: >43
# MilkGroup6: categorized by sumMY60 based on quantiles: 1st Qu <=1684; 2nd Qu: >1684 & <=2089; 3rd Qu: >2089 & <= 2457; 4th Qu: >2457
df.merge3 <- df.merge3 %>%
  add_column(MilkGroup1 = ifelse(df.merge3$PeakDim <= 35, "1st Qu",
             ifelse(df.merge3$PeakDim >35 & df.merge3$PeakDim <=47, "2nd Qu", 
                    ifelse(df.merge3$PeakDim >47 & df.merge3$PeakDim <= 67, "3rd Qu", 
                           ifelse(df.merge3$PeakDim >67, "4th Qu", df.merge3$PeakDim)))), .after="PeakDim") %>%
  add_column(MilkGroup2 = ifelse(df.merge3$PeakYield <= 36, "1st Qu",
                                 ifelse(df.merge3$PeakYield >36 & df.merge3$PeakYield <=43, "2nd Qu", 
                                        ifelse(df.merge3$PeakYield >43 & df.merge3$PeakYield <= 50, "3rd Qu", 
                                               ifelse(df.merge3$PeakYield >50, "4th Qu", df.merge3$PeakYield)))), .after="PeakYield") %>%
  add_column(MilkGroup3 = ifelse(df.merge3$AvgMilk20 <= 25.6, "1st Qu",
                                 ifelse(df.merge3$AvgMilk20 >25.6 & df.merge3$AvgMilk20 <=32.8, "2nd Qu", 
                                        ifelse(df.merge3$AvgMilk20 >32.8 & df.merge3$AvgMilk20 <= 39.3, "3rd Qu", 
                                               ifelse(df.merge3$AvgMilk20 >39.3, "4th Qu", df.merge3$AvgMilk20)))), .after="AvgMilk20") %>%
  add_column(MilkGroup4 = ifelse(df.merge3$sumMY20 <= 432, "1st Qu",
                                 ifelse(df.merge3$sumMY20 >432 & df.merge3$sumMY20 <= 556, "2nd Qu",
                                        ifelse(df.merge3$sumMY20 > 556 & df.merge3$sumMY20 <= 667, "3rd Qu",
                                               ifelse(df.merge3$sumMY20 > 667, "4th Qu", df.merge3$sumMY20)))), .after="sumMY20") %>%
  add_column(MilkGroup5 = ifelse(df.merge3$AvgMilk60 <= 30, "1st Qu",
                                 ifelse(df.merge3$AvgMilk60 >30 & df.merge3$AvgMilk60 <= 37, "2nd Qu",
                                        ifelse(df.merge3$AvgMilk60 > 37 & df.merge3$AvgMilk60 <= 43, "3rd Qu",
                                               ifelse(df.merge3$AvgMilk60 > 43, "4th Qu", df.merge3$AvgMilk60)))), .after="AvgMilk60") %>%
  add_column(MilkGroup6 = ifelse(df.merge3$sumMY60 <= 1684, "1st Qu",
                                 ifelse(df.merge3$sumMY60 >1684 & df.merge3$sumMY60 <= 2089, "2nd Qu",
                                        ifelse(df.merge3$sumMY60 > 2089 & df.merge3$sumMY60 <= 2457, "3rd Qu",
                                               ifelse(df.merge3$sumMY60 > 2457, "4th Qu", df.merge3$sumMY60)))), .after="sumMY60")
df.merge3$MilkGroup1 <- NULL
df.merge3$MilkGroup2 <- NULL
df.merge3$MilkGroup3 <- NULL
df.merge3$MilkGroup4 <- NULL
df.merge3$MilkGroup5 <- NULL
df.merge3$MilkGroup6 <- NULL

# how many cows were categroized with MilkGroup 1 and their DFC_CLA?
df.data1 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, PeakDim, MilkGroup1, DFC_CLA) %>%
  group_by(MilkGroup1) %>%
  summarize(meanDFC_CAL = mean(DFC_CLA),
            sdDFC_CLA = sd(DFC_CLA), 
            count=n())
# MilkGroup1 and DFCFirstHeat?
df.data1 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, PeakDim, MilkGroup1, DFCFirstHeat) %>%
  group_by(MilkGroup1) %>%
  summarize(meanDFCFirstHeat = mean(DFCFirstHeat, na.rm=TRUE),
            sdDFCFirstHeat = sd(DFCFirstHeat, na.rm=TRUE), 
            count=n())
# MilkGroup1 and DaysToFirstIns?
df.data1 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, PeakDim, MilkGroup1, DaysToFirstIns) %>%
  group_by(MilkGroup1) %>%
  summarize(meanDaysToFirstIns = mean(DaysToFirstIns, na.rm=TRUE),
            sdDaysToFirstIns = sd(DaysToFirstIns, na.rm=TRUE), 
            count=n())
View(df.data1)
# how many cows were categorized with MilkGroup 2 and their DFC_CLA?
df.data2 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, PeakYield, MilkGroup2, DFC_CLA) %>%
  group_by(MilkGroup2) %>%
  summarize(meanDFC_CAL = mean(DFC_CLA),
            sdDFC_CLA = sd(DFC_CLA), 
            count=n())
# MilkGroup2 and DFCFirstHeat?
df.data2 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, PeakYield, MilkGroup2, DFCFirstHeat) %>%
  group_by(MilkGroup2) %>%
  summarize(meanDFCFirstHeat = mean(DFCFirstHeat, na.rm=TRUE),
            sdDFCFirstHeat = sd(DFCFirstHeat, na.rm=TRUE), 
            count=n())
# MilkGroup2 and DaysToFirstIns?
df.data2 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, PeakYield, MilkGroup2, DaysToFirstIns) %>%
  group_by(MilkGroup2) %>%
  summarize(meanDaysToFirstIns = mean(DaysToFirstIns, na.rm=TRUE),
            sdDaysToFirstIns = sd(DaysToFirstIns, na.rm=TRUE), 
            count=n())
View(df.data2)
# how many cows were categorized with MilkGroup 3 and their DFC_CLA?
df.data3 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, AvgMilk20, MilkGroup3, DFC_CLA) %>%
  group_by(MilkGroup3) %>%
  summarize(meanDFC_CAL = mean(DFC_CLA),
            sdDFC_CLA = sd(DFC_CLA), 
            count=n())
# MilkGroup3 and DFCFirstHeat?
df.data3 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, AvgMilk20, MilkGroup3, DFCFirstHeat) %>%
  group_by(MilkGroup3) %>%
  summarize(meanDFCFirstHeat = mean(DFCFirstHeat, na.rm=TRUE),
            sdDFCFirstHeat = sd(DFCFirstHeat, na.rm=TRUE), 
            count=n())
# MilkGroup3 and DaysToFirstIns?
df.data3 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, AvgMilk20, MilkGroup3, DaysToFirstIns) %>%
  group_by(MilkGroup3) %>%
  summarize(meanDaysToFirstIns = mean(DaysToFirstIns, na.rm=TRUE),
            sdDaysToFirstIns = sd(DaysToFirstIns, na.rm=TRUE), 
            count=n())
View(df.data3)
# how many cows were categorized with MilkGroup 4 and their DFC_CLA?
df.data4 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, sumMY20, MilkGroup4, DFC_CLA) %>%
  group_by(MilkGroup4) %>%
  summarize(meanDFC_CAL = mean(DFC_CLA),
            sdDFC_CLA = sd(DFC_CLA), 
            count=n())
# MilkGroup4 and DFCFirstHeat?
df.data4 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, sumMY20, MilkGroup4, DFCFirstHeat) %>%
  group_by(MilkGroup4) %>%
  summarize(meanDFCFirstHeat = mean(DFCFirstHeat, na.rm=TRUE),
            sdDFCFirstHeat = sd(DFCFirstHeat, na.rm=TRUE), 
            count=n())
# MilkGroup4 and DaysToFirstIns?
df.data4 <- df.merge3 %>%
  select(FarmNo, CowId, Lactation, sumMY20, MilkGroup4, DaysToFirstIns) %>%
  group_by(MilkGroup4) %>%
  summarize(meanDaysToFirstIns = mean(DaysToFirstIns, na.rm=TRUE),
            sdDaysToFirstIns = sd(DaysToFirstIns, na.rm=TRUE), 
            count=n())
View(df.data4)





# log-transfusion
df.table <- data.frame()
df.table <- df.merge1 %>%
  select(FarmNo, CowId, Lactation, Season, DFC_CLA, DFCFirstHeat, DaysToFirstIns,  Culled, PregStatus, PregStatus_1, AvgKeto10,ECC, KetoCow10, KetoDays) %>%
  add_column(log.DFC_CLA = log(DFC_CLA),
             log.DFCFirstHeat = log(DFCFirstHeat),
             log.DaysToFirstIns = log(DaysToFirstIns),
             log.AvgKeto10 = log(AvgKeto10),
             log.KetoDays = log(KetoDays))
hist(df.table$log.AvgKeto10)
boxplot(log.AvgKeto10~Culled, data=df.table, names=c("alive", "culled"), ylab="log-transformed AvgKeto10", 
        main="Culling after 150 DIM and average milk BHB values within 20 DIM")
boxplot(log.AvgKeto10~PregStatus, data=df.table)
boxplot(log.AvgKeto10~PregStatus_1, data=df.table, 
        names=c("non-prengant", "pregnant"), xlab = "pregnancy status after 1st AI")
boxplot(log.DFC_CLA~Culled, data=df.table)
t.test(df.table$log.AvgKeto10~df.table$Culled)
t.test(df.table$log.DFC_CLA~df.table$Culled)
t.test(df.table$log.AvgKeto10~df.table$PregStatus_1)
t.test(df.table$log.AvgKeto10~df.table$PregStatus)
by(df.table$log.AvgKeto10, df.table$Culled, describe)
by(df.table$log.AvgKeto10, df.table$PregStatus_1, describe)
by(df.table$log.AvgKeto10, df.table$Season, describe)

p <- lm(log.AvgKeto10~factor(Season), data=df.table)
anova(p)
p1 <- lm(AvgKeto10~factor(Season), data=df.merge1)
anova(p1)
summary(p)
p2 <- aov(log.AvgKeto10~factor(Culled), data=df.table)
e2 <- residuals(p2)
boxplot(e2)
qqnorm(e2)
abline(mean(e2), sd(e2))








##Plot the points for the people with Parkinson's, using red. Add the regression line:
  plot(d.unhealthy$age, d.unhealthy$speed, 
       main="Scatterplot Parkinson Data Full", 
       ylab="Speed", xlab="Age", pch=19, col="red")
abline(lm(d.unhealthy$speed~d.unhealthy$age),
       col="red") # regression line (y~x)  

##Add the points for the people without Parkinson's and their the regression line to the above plot using green:
  points(d.healthy$age, d.healthy$speed, pch=19, col="green")
abline(lm(d.healthy$speed~ d.healthy$age), 
       col="green") # regression line (y~x)

##Now add a legend to the graph:
  legend(40, 1.9, 
         c("Parksinson","Healthy"), 
         lty=c(1,1), 
         lwd=c(2.5,2.5),col=c("red","green"))
  
  
#################################################################################
###### survival analysis for knowing the onset of BHBraw over 0.08 mmol/L ######
library(survival)
df.avg20d$KetoCow <- ifelse(df.avg20d$BHBraw >= 0.08, 1, 0)
df.ketomilk <- merge(df.avg20d, milk_all, by = c('FarmNo', 'CowId'), all.x=TRUE, sort=F)
df.farmsize <- df.ketomilk %>%
  select(FarmNo, CowId) %>%
  group_by(FarmNo, CowId) %>%
  summarize(count=n()) %>%
  group_by(FarmNo) %>%
  summarize(count=n())
summary(df.farmsize)
df.ketomilk1 <- merge(df.ketomilk, df.farmsize, by = c('FarmNo'), all.x=TRUE, sort=F)
df.ketomilk1 <- df.ketomilk1 %>%
  add_column(FarmSize = ifelse(df.ketomilk1$count <90, "Small", 
                               ifelse(df.ketomilk1$count >= 90 & df.ketomilk1$count <114, "Median", 
                                      ifelse(df.ketomilk1$count >= 114 & df.ketomilk1$count <154, "Large", 
                                             ifelse(df.ketomilk1$count >=154, "Extreme", NA)))), .after="FarmNo")

df.surv1 <- survfit(Surv(rDFC, KetoCow)~ad_Lactation, data=df.ketomilk)
summary(df.surv1)
plot(df.surv1, main="Cumulative Survival Function", lty=c(1, 2, 3, 4), xlab="Day after calving (day)", ylab="Probability")
legend(x=0, y=0.6, legend=c("Primiparous", "2nd Lactation", "3rd Lactation", "4th+ Lactation"), lty=c(1, 2, 3, 4))

df.surv2 <- survfit(Surv(rDFC, KetoCow)~FarmSize, data=df.ketomilk1)
summary(df.surv2)
plot(df.surv2, main="Cumulative Survival Function", 
     lty=c(1, 2, 3, 4), xlab="Day after calving (day)", ylab="Probability")
legend(x=0, y=0.6, legend=c("Extra Large", "Large", "Median", "Small"), lty=c(1, 2, 3, 4))

df.surv3 <- coxph(Surv(rDFC, KetoCow) ~ ad_Lactation, data=df.ketomilk1)
summary(df.surv3)
plot(survfit(df.surv3), fun = function(S) log(-log(S)), log = "x")
df.surv4 <- coxph(Surv(rDFC, KetoCow)~ad_Lactation + FarmSize, data=df.ketomilk1)
summary(df.surv4)



plot(survfit(df.surv2), fun = function(S) log(-log(S)), log="x")

leuk<-read.table("leukaemia.txt", header = TRUE)
attach(leuk)
leuksurv<-survfit(Surv(TIME,STATUS)~GROUP)
summary(leuksurv)
plot(leuksurv , main="Cumulative Survival Function", lty=c(1,2))
legend(x=25,y=1.0, legend = c("control", "treatment"), lty=c(1,2))

fit.1 <- coxph( Surv(TIME, STATUS) ~ GROUP)
summary(fit.1)
fit.3 <- coxph( Surv(TIME, STATUS) ~ GROUP + logWBCC)
summary(fit.3)
fit.st<-coxph(Surv(TIME, STATUS)~strata(GROUP))
plot(survfit(fit.st), fun = function(S) log(-log(S)), log = "x")

### survival analysis of reproduction events ###################################
# define farm size
df.farmsize <- df.merge3 %>%
  select(FarmNo, CowId) %>%
  group_by(FarmNo, CowId) %>%
  summarize(count=n()) %>%
  group_by(FarmNo) %>%
  summarize(count=n())
summary(df.farmsize)
# categorize farm size into 4 groups: 1. small farm (0-73); 2. middle (73-96); 3. large (96-128); 4. extra large (>128)
df.farmsize <- df.farmsize %>%
  add_column(FarmSize = ifelse(df.farmsize$count <= 73, "Small", 
                               ifelse(df.farmsize$count > 73 & df.farmsize$count <= 96, "Middle", 
                                      ifelse(df.farmsize$count > 96  & df.farmsize$count <= 128, "Large", 
                                             ifelse(df.farmsize$count > 128, "Extreme", NA)))), .after="count")
# merge the analysis data frame with the farm size
df.merge4 <- merge(df.merge3, df.farmsize, by=('FarmNo'), keep_all=TRUE)
df.merge3 <- df.merge4
# add column of reproduction events    
df.merge3 <- df.merge3 %>%
  add_column(SurvDFC_CLA = ifelse(df.merge3$DFC_CLA > 0, 1, df.merge3$DFC_CLA), .after="DFC_CLA") %>%
  add_column(SurvDFCFirstHeat = ifelse(df.merge3$DFCFirstHeat > 0, 1, df.merge3$DFCFirstHeat), .after="DFCFirstHeat") %>%
  add_column(SurvDaysToFirstIns = ifelse(df.merge3$DaysToFirstIns > 0, 1, df.merge3$DaysToFirstIns), .after="DaysToFirstIns")
df.fit <- survfit(Surv(DFC_CLA, )~Keto)