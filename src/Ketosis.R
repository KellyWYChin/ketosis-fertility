df.keto01 <- read_excel("1001, All Data, Proc, June 2020.xlsx", sheet = "Ketosis")
summary(df.keto01)
plot(df.keto01$DFC)

# BHB measurements did not start from the same day (mostly 3 or 4 DFC)
# some cows have a more frequent measurement of BHB (more than once per day)

all_keto_raw <- data.frame()
for(i in 1001:1038){
  filename <- paste("C:/Data/",i,", All Data, Proc, June 2020.xlsx", sep = "")
  df.keto <- read_excel(filename, sheet = "Ketosis")
  
  all_keto_raw <- bind_rows(all_keto_raw, df.keto) %>%
    group_by(FarmNo, CowId) %>%
    arrange(DFC, .by_group = TRUE)
} 
summary(all_keto_raw)
rm(list=ls(pattern="df"))

all_keto_raw$rDFC <- round(all_keto_raw$DFC)
summary(all_keto_raw)

#filter out all the measurements after 20 DIM
df.avg20d.all <- all_keto_raw %>%
  group_by(FarmNo, CowId) %>%
  select(Parity, rDFC, BHBraw) %>%
  filter(rDFC <=20) %>%
  mutate(AvgKeto20 = mean(BHBraw)) %>%
  distinct(FarmNo, CowId, Parity, AvgKeto20)

#list all cows and their maximum DFC of measurements
df.avg20d <- all_keto_raw %>%
  group_by(FarmNo, CowId) %>%
  select(Parity, rDFC, BHBraw) %>%
  filter(rDFC <=20) %>%
  mutate(AvgKeto20 = mean(BHBraw), MaxDFC = max(rDFC))

################################################################################
####### find out the duration of ketosis within first 20 DFC ###################
################################################################################
df.dur1_20 <- data.frame()
df.dur1_20 <- all_keto_raw %>%
  group_by(FarmNo, CowId) %>%
  select(Parity, DFC, rDFC, BHBraw) %>%
  filter(rDFC <= 20) %>%
  summarise(days_DFC = ifelse(BHBraw >= 0.08, DFC - lag(DFC), 0)
            , DFC, rDFC, Parity, BHBraw)
df.dur1_20 <- df.dur1_20 %>%
  mutate(KetoDays = sum(days_DFC, na.rm=TRUE)) %>%
  distinct(FarmNo, CowId, Parity, KetoDays)

#select all cows regardless numbers of time they were sampled
df.avgall20.ind <- df.avg20d %>%
  distinct(FarmNo, CowId, Parity, AvgKeto20)
names(df.avgall20.ind)[names(df.avgall20.ind) == "AvgKeto20"] <- "AvgKeto_1-20"
#select all cows that have the maxDFC above 10
df.avg10d.comp <- subset(df.avg20d, MaxDFC > "10")
df.avg10d.ind <- df.avg10d.comp %>%
  distinct(FarmNo, CowId, Parity, AvgKeto20)
names(df.avg10d.ind)[names(df.avg10d.ind) == "AvgKeto20"] <- "AvgKeto10"

#only select cows that have the maxDFC = 20 
df.avg20d.comp  <- subset(df.avg20d, MaxDFC == "20")
df.avg20d.ind <- df.avg20d.comp %>%
  distinct(FarmNo, CowId, Parity, AvgKeto20)
summary(df.avg20d.ind)
which.max(df.avg20d.ind$AvgKeto20)
hist(df.avg20d.ind$AvgKeto20, main = "Histogram of mean milk BHBraw of each cows", xlab = "mean milk BHBraw (mmol/L)")

df.keto1 <- merge(df.avgall20.ind, df.avg10d.ind, by=c('FarmNo', 'CowId', 'Parity'), all.x=TRUE, all.y=TRUE)
df.keto2 <- merge(df.keto1, df.avg20d.ind, by=c('FarmNo', 'CowId', 'Parity'), all.x=TRUE, all.y=TRUE)
df.keto3 <- merge(df.keto2, df.dur1_20, by=c('FarmNo', 'CowId', 'Parity'), sort=F, all.x=TRUE, all.y=TRUE)

df.max20d.all <- all_keto_raw %>%
  group_by(FarmNo, CowId) %>%
  select(Parity, rDFC, BHBraw) %>%
  filter(rDFC <= 20) %>%
  filter(BHBraw == max(BHBraw))
names(df.max20d.all)[names(df.max20d.all) == "BHBraw"] <- "MaxKeto20"
names(df.max20d.all)[names(df.max20d.all) == "rDFC"] <- "MaxKetoDFC20"

df.keto4 <- merge(df.keto3, df.max20d.all, by=c('FarmNo', 'CowId', 'Parity'), all.x=TRUE, all.y=TRUE)

#how many times BHB>0.08 within 20 DFC?
df.highketo.20d <- df.avg20d %>%
  filter(BHBraw >= 0.08) %>%
  mutate(nHighKeto20 = n()) %>%
  distinct(FarmNo, CowId, Parity, nHighKeto20)

df.keto5 <- merge(df.keto4, df.highketo.20d, by=c('FarmNo', 'CowId', 'Parity'), all.x=TRUE, all.y=TRUE)

#how many times BHB>0.08 within 60 DFC?
df.highketo.60d <- all_keto_raw %>%
  group_by(FarmNo, CowId) %>%
  select(Parity, rDFC, BHBraw) %>%
  filter(rDFC <=60) %>%
  filter(BHBraw >= 0.08) %>%
  mutate(nHighKeto60 = n()) %>%
  distinct(FarmNo, CowId, Parity, nHighKeto60)

df.keto6 <- merge(df.keto5, df.highketo.60d,  by=c('FarmNo', 'CowId', 'Parity'), all.x=TRUE, all.y=TRUE)

summary(df.keto6)
summary(df.keto5)
#maximum BHBraw within 60 DFC?
df.max60d.all <- all_keto_raw %>%
  group_by(FarmNo, CowId) %>%
  select(Parity, rDFC, BHBraw) %>%
  filter(rDFC <= 60) %>%
  filter(BHBraw == max(BHBraw))
names(df.max60d.all)[names(df.max60d.all) == "BHBraw"] <- "MaxKeto60"
names(df.max60d.all)[names(df.max60d.all) == "rDFC"] <- "MaxKetoDFC60"
  
keto <- merge(df.keto6, df.max60d.all, c=by('FarmNo', 'CowId'), all.x=TRUE, all.y=TRUE)
summary(keto)
rm(list=ls(pattern="df"))

#if the cow does not have any value over 0.08 within 20 DFC, the number of High BHB measure = 0
keto$nHighKeto20 <- ifelse((!is.na(keto$MaxKeto20) & (keto$MaxKeto20 < 0.08)), 0, keto$nHighKeto20)
summary(keto)
#if the cow does not have any value over 0.08 within 60 DFC, the number of High BHB measure = 0
keto$nHighKeto60 <- ifelse((!is.na(keto$MaxKeto60) & (keto$MaxKeto60 < 0.08)), 0, keto$nHighKeto60)
summary(keto)

#####Exclude all the duplicated cows due to data inconsistency##################
test1 <- keto %>%
  distinct(FarmNo, CowId, .keep_all=TRUE)
test2 <- keto %>%  
  distinct(FarmNo, CowId, Parity)
test2$farmcow <- paste(test2$FarmNo, test2$CowId, sep="") #stick FarmNo and CowId together
test3 <- test2 %>%
  mutate(farmcow.dup=ifelse(duplicated(farmcow)|
                              duplicated(farmcow, fromLast=TRUE), 1, 0))
test3 <- test3[(test3$farmcow.dup == 0), ]

keto$farmcow <- paste(keto$FarmNo, keto$CowId, sep="")
keto <- keto %>%
  mutate(farmcow.dup = ifelse(duplicated(farmcow)|
                                duplicated(farmcow, fromLast=TRUE), 1, 0))
keto <- keto[(keto$farmcow.dup == 0), ]
keto$farmcow.dup <- NULL
keto$farmcow <- NULL
rm(list=ls(pattern="test"))
rm(list=ls(pattern="df."))
summary(keto)

keto <- keto %>%
  add_column(KetoCow10 = 
               ifelse(keto$AvgKeto10 >= 0.08, "1", "0"), .after="AvgKeto10")
names(keto)[names(keto) == "Parity"] <- "Lactation"

################################################################################
############ Ketosis definitions ################################################
################################################################################
# How many % of cows are ketosis in a farm??
## definition 1: AvgKeto1_20 >= 0.08; definition 2: KetoDays >= 1
df.keto <- keto %>%
  select(FarmNo, CowId, `AvgKeto_1-20`, KetoDays)
df.keto <- df.keto %>%
  add_column(KetoCow1 = ifelse(df.keto$'AvgKeto_1-20' >= 0.08, 1, 0), .after="AvgKeto_1-20") %>%
  add_column(KetoCow2 = ifelse(df.keto$KetoDays >= 1, 1, 0), .after="KetoDays")
summary(df.keto$KetoCow1)
summary(df.keto$KetoCow2)
table(df.keto$KetoCow1) #how many cows are categorized as ketosis under definition 1
table(df.keto$KetoCow2) #how many cows are categorized as ketosis under definition 2
keto <- keto%>%
  add_column(KetoCow2 = ifelse(keto$KetoDays >= 1, 1, 0), .after="KetoDays") %>%
  add_column(KetoCow1 = ifelse(keto$`AvgKeto_1-20` >= 0.08, 1, 0), .after="AvgKeto_1-20")

summary(keto$AvgKeto10)
summary(keto$nHighKeto20)
summary(keto$MaxKeto20)
summary(keto$MaxKetoDFC20)
### ketosis dynamics WITHIN 20 DIM
# Group1: categorize ketosis by AvgKeto10 = range 1: AvgKeto10 <0.05; 2: 0.05-0.08; 3: >0.08
# Group1.1: categorize ketosis by AvgKeto10 = 1st Qu < 0.054; median: 0.054-0.061; 3rd Qu: 0.061-0.076; over 3rd Qu: >0.076 
# Group2: categorize ketosis by nHighKeto20 = range 1: 0 (never 0.08); 2: 1-5; 3: >5
# Group3: categorize ketosis by MaxKeto20 = range 1: MaxKeto20 <0.07; 2: 0.07-0.08; 3: 0.08-0.12; 4: >=0.12
# Group4: categorize ketosis by MaxKetoDFC20 = early: MaxKetoDFC20 < 5; middle: MaxKetoDFC 5-15; late: > 15
keto <- keto %>%
  add_column(KetoGroup1 = ifelse(keto$AvgKeto10 < 0.05, "low",
                                 ifelse(keto$AvgKeto10 >= 0.05 & keto$AvgKeto10 < 0.08, "middle", 
                                        ifelse(keto$AvgKeto10 >= 0.08, "high", keto$AvgKeto10))), .after="AvgKeto10") %>%
  add_column(KetoGroup1.1 = ifelse(keto$AvgKeto10 < 0.054, "1st Qu", 
                                   ifelse(keto$AvgKeto10 >= 0.054 & keto$AvgKeto10 < 0.061, "median",
                                          ifelse(keto$AvgKeto10 >= 0.061 & keto$AvgKeto10 < 0.076, "3rd Qu", 
                                                 ifelse(keto$AvgKeto10 >=0.076, "over 3rd Qu", keto$AvgKeto10)))), .after="KetoGroup1") %>%
  add_column(KetoGroup2 = ifelse(keto$nHighKeto20 == 0, "never",
                                 ifelse(keto$nHighKeto20 >= 1 & keto$nHighKeto20 <= 5, "seldom",
                                        ifelse(keto$nHighKeto20 > 5 & keto$nHighKeto20 < 11, "sometimes", 
                                               ifelse(keto$nHighKeto20 >= 11, "usually", keto$nHighKeto20)))), .after="nHighKeto20") %>%
  add_column(KetoGroup3 = ifelse(keto$MaxKeto20 < 0.07, "low", 
                                 ifelse(keto$MaxKeto20 >= 0.07 & keto$MaxKeto20 < 0.08, "middle", 
                                        ifelse(keto$MaxKeto20 >= 0.08 & keto$MaxKeto20 <0.12, "higher", 
                                               ifelse(keto$MaxKeto20 >=0.12, "very high", keto$MaxKeto20)))), .after="MaxKeto20") %>%
  add_column(KetoGroup4 = ifelse(keto$MaxKetoDFC20 < 5, "early",
                                 ifelse(keto$MaxKetoDFC20 >= 5 & keto$MaxKetoDFC20 <= 15, "middle",
                                        ifelse(keto$MaxKetoDFC20 >15, "late", keto$MaxKetoDFC20))), .after="MaxKetoDFC20")
keto$KetoGroup1 <- NULL
keto$KetoGroup1.1 <- NULL
keto$KetoGroup2 <- NULL
keto$KetoGroup3 <- NULL
keto$KetoGroup4 <- NULL

summary(keto$nHighKeto60)
summary(keto$MaxKeto60)
summary(keto$MaxKetoDFC60)
### ketosis dynamics WITHIN 60 DIM
# Group5: categorize ketosis by nHighKeto60 = range 1: 0 (never 0.08); seldom: 1-=5; sometimes: 5-=10; usually:>10
# Group6: categorize ketosis by MaxKeto60 = range 1: MaxKeto20 <0.07; 2: 0.07-=0.08; 3: 0.08-0.12; 4: >=0.12
# Group7: categorize ketosis by MaxKetoDFC60 = first week: MaxKetoDFC20 <= 7; first 2 weeks: MaxKetoDFC 7-=14; 1st month: 14-=30; 2nd month: >30
keto <- keto %>%
  add_column(KetoGroup5 = ifelse(keto$nHighKeto60 == 0, "never",
                                 ifelse(keto$nHighKeto60 >= 1 & keto$nHighKeto60 <= 5, "seldom",
                                        ifelse(keto$nHighKeto60 > 5 & keto$nHighKeto60 < 11, "sometimes", 
                                               ifelse(keto$nHighKeto60 >= 11, "usually", keto$nHighKeto60)))), .after="nHighKeto60") %>%
  add_column(KetoGroup6 = ifelse(keto$MaxKeto60 <= 0.07, "low", 
                                 ifelse(keto$MaxKeto60 > 0.07 & keto$MaxKeto60 <= 0.08, "middle", 
                                        ifelse(keto$MaxKeto60 > 0.08 & keto$MaxKeto60 <= 0.12, "higher", 
                                               ifelse(keto$MaxKeto60 > 0.12, "very high", keto$MaxKeto60)))), .after="MaxKeto60") %>%
  add_column(KetoGroup7 = ifelse(keto$MaxKetoDFC60 <= 7, "first week",
                                 ifelse(keto$MaxKetoDFC60 > 7 & keto$MaxKetoDFC60 <= 14, "2nd week", 
                                        ifelse(keto$MaxKetoDFC60 > 14 & keto$MaxKetoDFC60 <=30, "first month",
                                               ifelse(keto$MaxKetoDFC60 > 30, "2nd month", keto$MaxKetoDFC60)))), .after="MaxKetoDFC60")

keto$KetoGroup5 <- NULL
keto$KetoGroup6 <- NULL
keto$KetoGroup7 <- NULL

##########################################################################################################
############# Descriptive Statistics #####################################################################
##########################################################################################################
summary(keto$KetoDays)
ggplot(data=keto, aes(x=KetoDays)) +
  geom_histogram(col="red", fill="green", alpha=0.2) +
  scale_x_continuous(breaks = seq(0, 20, by=5)) +
  ggtitle("Days of milk BHB over 0.08 mmol/L within 20 DFC in 38 farms") 
h1 <- hist(keto$KetoDays, xlim = c(0, 20), ylim = c(0, 3000), main="Days of milk BHB over 0.08 mmol/L within 20 DIM")
text(h1$mids, h1$counts, labels=h1$counts, adj=c(0.5, -0.5))
summary(keto)
#Herd size
counts <- table(keto$FarmNo)
ggplot(data=keto, aes(x=factor(FarmNo))) +
  geom_bar(stat="count", fill="orange") +
  stat_count(geom="text", size = 4, aes(label=..count..)) 
summary(keto_FarmLevel$CowNo)
sum(keto_FarmLevel$CowNo)
#Lactation distribution
ggplot(data=keto, aes(x=factor(Lactation))) +
  geom_bar(stat = "count", fill="orange")+
  stat_count(geom = "text", size= 4, aes(label=..count..)) +
  ggtitle("Parity distribution") + xlab("Parity") 
summary(keto$`AvgKeto_1-20`)
count(keto %>%
  filter(keto$`AvgKeto_1-20` >= 0.08))
summary(keto$AvgKeto10)
count(keto %>%
        filter(keto$AvgKeto10 >= 0.08))
table(keto$KetoCow10)
summary(keto$AvgKeto20)
count(keto %>%
        filter(keto$AvgKeto20 >= 0.08))
#DIM where cows reached the highest milk BHB within 20 DIM 
summary(keto$MaxKetoDFC20)
ggplot(data=keto, aes(x=MaxKetoDFC20)) +
  geom_bar(stat = "count", fill="orange")+
  stat_count(geom = "text", size= 4, aes(label=..count..)) +
  ggtitle("DIM where cows reached the highest milk BHB within 20 DIM") + xlab("DIM")

#separate with MAX BHB value (over 0.08 or not) with colors
df.table <- as.data.frame(table(keto$MaxKetoDFC20, keto$MaxKetoKeto1))
ggplot(data=df.table, aes(x=Var1, y=Freq, fill=Var2)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Freq), position = position_stack(vjust=0.5)) +
  ggtitle("DIM where cows reached the highest milk BHB within 20 DIM") +
  xlab("DIM") +
  ylab("Numbers of cows") +
  scale_fill_manual(values = c("light blue", "pink"), name="BHB over 0.08 mmol/L")

#DIM where cows reached the highest milk BHB within 60 DIM
summary(keto$MaxKetoDFC60)
ggplot(data=keto, aes(x=MaxKetoDFC60)) +
  geom_bar(stat = "count", fill="orange")+
  stat_count(geom = "text", size= 3, aes(label=..count..)) +
  ggtitle("DIM where cows reached the highest milk BHB within 60 DIM") + xlab("DIM")
#separate with max BHB over 0.08 or not with colors
df.table2 <- as.data.frame(table(keto$MaxKetoDFC60, keto$MaxKetoKeto2))
ggplot(data=df.table2, aes(x=Var1, y=Freq, fill=Var2)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Freq), position = position_stack(vjust=0.7)) +
  ggtitle("DIM where cows reached the highest milk BHB within 60 DIM") +
  xlab("DIM") +
  ylab("Numbers of cows") +
  scale_fill_manual(values = c("light blue", "pink"), name="BHB over 0.08 mmol/L")

## whether the maximum BHB level is over 0.08 mmol/L?
keto <- keto %>%
  add_column(MaxKetoKeto1 = ifelse(keto$MaxKeto20 >= 0.08, 1, 0), .after="MaxKeto20") %>%
  add_column(MaxKetoKeto2 = ifelse(keto$MaxKeto60 >= 0.08, 1, 0), .after="MaxKeto60")

ggplot(data = keto, aes(x = MaxKetoDFC20, fill = MaxKetoKeto1)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  ggtitle("DIM where cows reached the highest milk BHB within 20 DIM") + xlab("DIM")

#number of measurements that milk BHB value over 0.08 mmol/L within 20 DIM
ggplot(data=keto, aes(x=nHighKeto20)) +
  geom_bar(stat = "count", fill="orange")+
  stat_count(geom = "text", size= 3, aes(label=..count..)) +
  ggtitle("Number of measurements that milk BHB value over 0.08 mmol/L within 20 DIM") + 
  xlab("Number of measurements") +
  ylab("Number of cows")
summary(keto$nHighKeto20)

ggplot(data=keto, aes(x=nHighKeto60)) +
  geom_bar(stat = "count", fill="orange")+
  stat_count(geom = "text", size= 3, aes(label=..count..)) +
  ggtitle("Number of measurements that milk BHB value over 0.08 mmol/L within 60 DIM") + 
  xlab("Number of measurements") +
  ylab("Number of cows")
summary(keto$nHighKeto60)

#if MaxKeto60 = MaxKeto20, highest BHB value happened within 20 DIM = 0; Highest BHB value happened between 20-60 DIM = 1
keto <- keto %>%
  add_column(DiffHighKeto = ifelse((keto$MaxKetoDFC60-keto$MaxKetoDFC20) == 0, 0, 1))
table(keto$DiffHighKeto) 
sum(keto$DiffHighKeto, na.rm=T)
summary(keto$DiffHighKeto)


########## Farm Level ###################################################################
keto_summary <- data.frame()
keto_summary <- keto %>%
  group_by(FarmNo) %>%
  summarize(Mean_Parity = mean(Lactation),
            sd_Parity = sd(Lactation),
            Mean_AvgKeto1_20 = mean(`AvgKeto_1-20`, na.rm=TRUE),
            sd_AvgKeto1_20 = sd(`AvgKeto_1-20`, na.rm=TRUE),
            Mean_AvgKeto20 = mean(AvgKeto20, na.rm=TRUE),
            sd_AvgKeto20 = sd(AvgKeto20, na.rm=TRUE),
            Mean_KetoDays = mean(KetoDays, na.rm=TRUE),
            sd_KetoDays = sd(KetoDays, na.rm=TRUE),
            Mean_MaxKetoDFC20 = mean(MaxKetoDFC20, na.rm=TRUE),
            Mean_MaxKeto20 = mean(MaxKeto20, na.rm=TRUE), 
            Mean_nHighKeto20 = mean(nHighKeto20, na.rm=TRUE), 
            Mean_nHighKeto60 = mean(nHighKeto60, na.rm=TRUE))
keto_summary <- keto_summary %>%
  mutate_if(is.numeric, round, digits=3)

### Farm level #################################################################
df.keto1 <- df.keto %>%
  group_by(FarmNo) %>%
  summarize(CowNo = n(), 
            SumKeto1 = sum(KetoCow1, na.rm=TRUE),
            SumKeto2 = sum(KetoCow2, na.rm=TRUE))
df.keto1 <- df.keto1 %>%
  summarize(FarmNo, CowNo, 
            percent1 = (SumKeto1/CowNo)*100,
            percent2 = (SumKeto2/CowNo)*100, 
            SumKeto1, SumKeto2)
keto_FarmLevel <- merge(df.keto1, keto_summary, by=c('FarmNo'), all.x=TRUE, all.y=TRUE)
write.csv(keto_FarmLevel, "FarmLevel_Ketosis.csv")
summary(keto_FarmLevel)
  
summary(keto)
summary(keto_summary)







repro_milk_keto <- merge(repro_milk, keto, c = by('FarmNo', 'CowId'), all.x = TRUE, all.y = TRUE)                            
repro_milk_keto$Parity <- NULL
summary(repro_milk_keto)
# exclude the cows without CalvingDate records
repro_milk_keto <- repro_milk_keto[!is.na(repro_milk_keto$CalvingDate), ]
summary(repro_milk_keto)

ggplot(data=repro_milk_keto, aes(x=AvgKeto20)) +
  geom_histogram(col="red", fill="green", alpha=0.2) +
  ggtitle("First 20d Average Milk BHB in 38 farms") +
  facet_wrap(repro_milk_keto$FarmNo)
ggplot(data=repro_milk_keto, aes(x=AvgKeto20)) +
  geom_histogram(col="red", fill="green", alpha=0.2) +
  facet_wrap(repro_milk_keto$ad_Lactation) +
  ggtitle("First 20d Average Milk BHB in different parity")

hist(repro_milk_keto$MaxKeto20, main = "Histogram of maximum BHB level within 20 DFC", xlab = "milk BHB (mmol/L)")
summary(repro_milk_keto$MaxKeto20)
ggplot(data=repro_milk_keto, aes(x=MaxKeto20)) +
  geom_histogram(col="red", fill="green", alpha=0.2) +
  ggtitle("Maximum Milk BHBraw within 20 DFC")
ggplot(data=repro_milk_keto, aes(x=MaxKeto20)) +
  geom_histogram(col="red", fill="green", alpha=0.2) +
  facet_wrap(repro_milk_keto$FarmNo) +
  ggtitle("Maximum Milk BHBraw within 20 DFC in 38 farms")
ggplot(data=repro_milk_keto, aes(x=MaxKeto20)) +
  geom_histogram(col="red", fill="green", alpha=0.2) +
  facet_wrap(repro_milk_keto$ad_Lactation) +
  ggtitle("Maximum Milk BHBraw within 20 DFC in Different Parity")

scatter.hist(repro_milk_keto$ad_Lactation, repro_milk_keto$AvgKeto20, xlab="Lactation", ylab="Average 20d Milk BHB")  
scatter.hist(repro_milk_keto$ad_Lactation, repro_milk_keto$MaxKeto20, xlab="Lactation", ylab="Max Milk BHB within 20d")
scatter.hist(repro_milk_keto$ad_Lactation, repro_milk_keto$AvgMilk305, xlab="Lactation", ylab="Average 305d Milk Yield")
scatter.hist(repro_milk_keto$AvgMilk20, repro_milk_keto$AvgKeto20, xlab="Average Milk Yield in first 20d", ylab="Average Milk BHB in first 20d ")
##########################################################################################################
##########################################################################################################

###### survival analysis for knowing the onset of BHBraw over 0.08 mmol/L ######
#categroise parity
df.avg20d$ad_Lactation[df.avg20d$Parity >= 4] <- 4
df.avg20d$ad_Lactation[df.avg20d$Parity == 3] <- 3
df.avg20d$ad_Lactation[df.avg20d$Parity == 2] <- 2
df.avg20d$ad_Lactation[df.avg20d$Parity == 1] <- 1

df.avg20d <- df.avg20d %>%
  add_column(KetoCow=
               ifelse(df.avg20d$BHBraw >= 0.08, "1", "0"), .after="BHBraw")
df.avg20d$KetoCow <- as.numeric(df.avg20d$KetoCow)

library(survival)
df.surv1 <- survfit(Surv(rDFC, KetoCow)~ad_Lactation, data=df.avg20d)
summary(df.surv1)
plot(df.surv1, main="Cumulative Survival Function", lty=c(1, 2, 3, 4), xlab="Day after calving (day)", ylab="Probability")
legend(x=0, y=0.6, legend=c("Primiparous", "2nd Lactation", "3rd Lactation", "4th+ Lactation"), lty=c(1, 2, 3, 4))
summary(df.surv1)


