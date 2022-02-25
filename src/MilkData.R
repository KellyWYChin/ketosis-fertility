df.milk01 <- data.frame()
df.milk01 <- read_excel("1001, All Data, Proc, June 2020.xlsx", sheet = "MilkData") %>%
  arrange(CowId) #reorder rows by CowId in ascending order
summary(df.milk01)
str(df.milk01)
which.min(df.milk01$DayYield)

df.calving01 <- read_excel("1001, All Data, Proc, June 2020.xlsx", sheet = "Calving")
df.calvingdate01 <- df.calving01 %>%
  select(CowId, CalvingDate)

df.master01 <- merge(df.calvingdate01, df.milk01, by = "CowId")
df.cowinfo38 <- read_excel("1038, All Data, Proc, June 2020.xlsx", sheet = "CowInfo")
summary(df.cowinfo38)
df.cowinfo01 <- read_excel("1001, All Data, Proc, June 2020.xlsx", sheet = "CowInfo")
summary(df.cowinfo01)

################################################################################
################################################################################

df.lact <- all_farm %>%
  select(CowId, FarmNo, CalvingDate, Lactation, PeakYield, PeakDim) %>%
  distinct(CowId, FarmNo, Lactation, .keep_all=TRUE)

all_milk_raw <- data.frame()
for(i in 1001:1038){
  filename <- paste("C:/Data/",i,", All Data, Proc, June 2020.xlsx", sep = "")
  df.milk <- read_excel(filename, sheet = "MilkData")
  df.cowinfo <- read_excel(filename, sheet = "CowInfo")
  # change the column name of cowinfo (CowID -> CowId)
  colnames(df.cowinfo)[colnames(df.cowinfo)=="CowID"] <- "CowId"
  df.cowinfo$BirthDate <- as.character(df.cowinfo$BirthDate)
  
  df.milkfarm <- merge(df.cowinfo, df.milk, by = "CowId", all.x= TRUE, all.y=TRUE) %>%
    group_by(FarmNo, CowId) %>%
    arrange(DFC, .by_group = TRUE)

  all_milk_raw <- bind_rows(all_milk_raw, df.milkfarm)
}
summary(all_milk_raw)
all_milk_raw$CowNo <- NULL
all_milk_raw$BirthDate <- NULL
all_milk_raw$Inclusion <- NULL
all_milk_raw$HasMilkLactCowID <- NULL
all_milk <- all_milk_raw %>%
  distinct(FarmNo, CowId)

#create a milk dataframe including all milk information from 38 farms #
df.merge <- data.frame()
df.merge <- merge(df.lact, all_milk_raw, by=c('FarmNo', 'CowId'), all.x=TRUE, all.y=TRUE) %>%
  group_by(FarmNo, CowId) %>%
  arrange(DFC, .by_group=TRUE)
summary(df.merge)
# Exclude cows without FarmNo
df.merge <- df.merge[!is.na(df.merge$FarmNo), ]
# Exclude DIM 1-3 milk yield
df.merge1 <- df.merge %>%
  filter(DFC > 3)
# if daily milk yield <1 kg, we name it as NA; others = original daily milk yield
df.merge1$DayYield <- ifelse(df.merge1$DayYield < 1, NA, df.merge1$DayYield)
df.merge <- df.merge1
df.merge2 <- df.merge1 %>%
  distinct(FarmNo, CowId)

##### Calculate first 20 DIM, mean milk yield, cumulative milk yield
df.first20d <- df.merge %>%
  group_by(FarmNo, CowId) %>%
  select(Lactation, CalvingDate, DayYield, DFC) %>%
  filter(DFC <= 20) %>%
  mutate(AvgMilk20 = mean(DayYield), 
         MinDFC = min(DFC), 
         MaxDFC = max(DFC), 
         sumMY20 = sum(DayYield))
summary(df.first20d)

# ONLY when Min DFC = 4 and  Max DFC = 20 the cows can be included into the dataset #
df.first20d.comp <- subset(df.first20d, 
                           MinDFC == "4" & MaxDFC == "20")
summary(df.first20d.comp)
# All cows 
df.20dmilk <- df.first20d %>%
  select(FarmNo, CowId, Lactation, CalvingDate, AvgMilk20, sumMY20) %>%
  distinct(FarmNo, CowId, Lactation, AvgMilk20, sumMY20)
# Only the cows have completed 4-20 DIM milk records
df.20d_ind <- df.first20d.comp %>%
  select(FarmNo, CowId, Lactation, CalvingDate, AvgMilk20, sumMY20) %>%
  distinct(FarmNo, CowId, Lactation, AvgMilk20, sumMY20)

##### Calculate first 60 d mean milk yield, cumulative milk yield
df.first60d <- df.merge %>%
  group_by(FarmNo, CowId) %>%
  select(Lactation, CalvingDate, DayYield, DFC) %>%
  filter(DFC <= 60) %>%
  mutate(AvgMilk60 = mean(DayYield), 
         MinDFC = min(DFC), 
         MaxDFC = max(DFC), 
         sumMY60 = sum(DayYield))
summary(df.first60d)
# Only the cows have completed 4-60 DIM milk records
df.first60d.comp <- subset(df.first60d, 
                           MinDFC == "4" & MaxDFC == "60")
summary(df.first60d.comp)
df.60dmilk <- df.first60d %>%
  select(FarmNo, CowId, Lactation, CalvingDate, AvgMilk60, sumMY60) %>%
  distinct(FarmNo, CowId, Lactation, CalvingDate, AvgMilk60, sumMY60)

df.60d_ind <- df.first60d.comp %>%
  select(FarmNo, CowId, Lactation, CalvingDate, AvgMilk60, sumMY60) %>%
  distinct(FarmNo, CowId, Lactation, AvgMilk60, sumMY60)

df.master1 <- merge(df.20d_ind, df.60d_ind, 
                    by = c('FarmNo', 'CowId', 'Lactation'), all.x=TRUE, all.y=TRUE)
df.master5 <- merge(df.20dmilk, df.60dmilk, 
                    by= c('FarmNo', 'CowId', 'Lactation'), all.x=TRUE, all.y=TRUE)
summary(df.master1)

df.first80d <- df.merge %>%
  group_by(FarmNo, CowId) %>%
  select(Lactation, CalvingDate, DayYield, DFC) %>%
  filter(DFC <= 80) %>%
  mutate(AvgMilk80 = mean(DayYield), 
         MinDFC = min(DFC), 
         MaxDFC = max(DFC), 
         sumMY80 = sum(DayYield))
summary(df.first80d)
# Only the cows have completed 4-80 DIM milk records
df.first80d.comp <- subset(df.first80d, 
                           MinDFC == "4" & MaxDFC == "80")
summary(df.first80d.comp)
df.80dmilk <- df.first80d %>%
  select(FarmNo, CowId, Lactation, CalvingDate, AvgMilk80, sumMY80) %>%
  distinct(FarmNo, CowId, Lactation, AvgMilk80, sumMY80)
df.80d_ind <- df.first80d.comp %>%
  select(FarmNo, CowId, Lactation, CalvingDate, AvgMilk80, sumMY80) %>%
  distinct(FarmNo, CowId, Lactation, AvgMilk80, sumMY80)

df.master2 <- merge(df.master1, df.80d_ind, 
                    by = c('FarmNo', 'CowId', 'Lactation'), all.x=TRUE, all.y=TRUE)
df.master6 <- merge(df.master5, df.80dmilk, 
                    by = c('FarmNo', 'CowId', 'Lactation'), all.x=TRUE, all.y=TRUE)
summary(df.master2)

df.first100d <- df.merge %>%
  group_by(FarmNo, CowId) %>%
  select(Lactation, CalvingDate, DayYield, DFC) %>%
  filter(DFC <= 100) %>%
  mutate(AvgMilk100 = mean(DayYield), MinDFC = min(DFC), MaxDFC = max(DFC), sumMY100 = sum(DayYield))
summary(df.first100d)

df.first100d.comp <- subset(df.first100d, MinDFC == "4" & MaxDFC == "100")
summary(df.first100d.comp)

df.100d_ind <- df.first100d.comp %>%
  select(FarmNo, CowId, Lactation, CalvingDate, AvgMilk100, sumMY100) %>%
  distinct(FarmNo, CowId, Lactation, AvgMilk100, sumMY100)

df.master3 <- merge(df.master2, df.100d_ind, by = c('FarmNo', 'CowId', 'Lactation'), all.x=TRUE, all.y=TRUE)

df.first305d <- df.merge %>%
  group_by(FarmNo, CowId) %>%
  select(Lactation, CalvingDate, DayYield, DFC) %>%
  filter(DFC <= 305) %>%
  mutate(AvgMilk305 = mean(DayYield), 
         MinDFC = min(DFC), 
         MaxDFC = max(DFC), 
         sumMY305 = sum(DayYield))
summary(df.first305d)

df.305dmilk <- df.first305d %>%
  select(FarmNo, CowId, Lactation, CalvingDate, AvgMilk305, sumMY305) %>%
  distinct(FarmNo, CowId, Lactation, AvgMilk305, sumMY305)

df.first305d.comp <- subset(df.first305d, MinDFC == "4" & MaxDFC == "305")

summary(df.first305d.comp)
df.305d_ind <- df.first305d.comp %>%
  select(FarmNo, CowId, Lactation, CalvingDate, AvgMilk305, sumMY305) %>%
  distinct(FarmNo, CowId, Lactation, AvgMilk305, sumMY305)
summary(df.305d_ind)

df.master4 <- merge(df.master3, df.305d_ind, by = c('FarmNo', 'CowId', 'Lactation'), all.x = TRUE, all.y = TRUE)
summary(df.master4)
df.master7 <-merge(df.master6, df.305dmilk, 
                   by=c('FarmNo', 'CowId', 'Lactation'), all.x=TRUE, all.y=TRUE)
milk <- merge(df.lact, df.master4, by=c('FarmNo', 'CowId', 'Lactation'), all.x=TRUE, all.y=TRUE)
summary(milk)
milk <- milk[!is.na(milk$FarmNo), ]

milk_all <- merge(df.lact, df.master7, 
                  by=c('FarmNo', 'CowId', 'Lactation', 'CalvingDate'), all.x=TRUE, all.y=TRUE)
milk_all <- milk_all[!is.na(milk_all$FarmNo), ]
milk_all <- milk_all[!is.na(milk_all$CalvingDate), ]
rm(list=ls(pattern="df"))
rm(all_milk)

# re-categroize Lactation into 4 groups: 1, 2, 3, and 4+
milk$ad_Lactation[milk$Lactation >= 4] <- 4
milk$ad_Lactation[milk$Lactation == 3] <- 3
milk$ad_Lactation[milk$Lactation == 2] <- 2
milk$ad_Lactation[milk$Lactation == 1] <- 1
summary(milk)
milk_all$ad_Lactation[milk_all$Lactation >= 4] <- 4
milk_all$ad_Lactation[milk_all$Lactation == 3] <- 3
milk_all$ad_Lactation[milk_all$Lactation == 2] <- 2
milk_all$ad_Lactation[milk_all$Lactation == 1] <- 1
summary(milk_all)


################################################################################
####################### Discriptive statistics #################################
################################################################################

#Lactation distribution
ggplot(data=milk, aes(x=factor(Lactation))) +
  geom_bar(stat = "count", fill="orange")+
  stat_count(geom = "text", size= 4, aes(label=..count..)) +
  ggtitle("Parity distribution") + xlab("Parity") 
#adjusted Lactation distribution
ggplot(data=milk, aes(x=factor(ad_Lactation))) +
  geom_bar(stat = "count", fill="orange")+
  stat_count(geom = "text", size= 4, aes(label=..count..)) +
  ggtitle("Parity distribution") + xlab("Parity") 

hist(milk$AvgMilk20, main = "Histogram of average 20 d Milk Yield", xlab = "Average 20 d Milk Yield (kg)")
summary(milk$AvgMilk20)
summary(milk_all$AvgMilk20)
summary(milk$sumMY20)
summary(milk_all$sumMY20)

ggplot(data=milk, aes(x=AvgMilk20)) +
  geom_histogram(breaks=seq(0, 60, by=3),
                 col="red", fill="green", alpha=0.2) +
  facet_wrap(milk$FarmNo) +
  ggtitle("First 20d Average Milk Yield in 38 Farms")

ggplot(data=milk, aes(x=AvgMilk20)) +
  geom_histogram(breaks=seq(0, 60, by=3),
                 col="red", fill="green", alpha=0.2) +
  facet_wrap(milk$ad_Lactation) +
  ggtitle("First 20d Average Milk Yield in Different Parities") +
  xlab("Mean Milk Yield in 20 DIM") +
  ylab("No. of Cows")

df.table1 <- milk %>%
  select(FarmNo, CowId, ad_Lactation, AvgMilk20) %>%
  filter(ad_Lactation == "1") 
summary(df.table1)

df.table2 <- milk %>%
  select(FarmNo, CowId, ad_Lactation, AvgMilk20) %>%
  filter(ad_Lactation == "2") 
summary(df.table2)

df.table3 <- milk %>%
  select(FarmNo, CowId, ad_Lactation, AvgMilk20) %>%
  filter(ad_Lactation == "3") 
summary(df.table3)

df.table4 <- milk %>%
  select(FarmNo, CowId, ad_Lactation, AvgMilk20) %>%
  filter(ad_Lactation == "4") 
summary(df.table4)
rm(list=ls(pattern="df"))

hist(milk$AvgMilk60, main = "Histogram of average 60 d Milk Yield", xlab = "Average 60 d Milk Yield (kg)")
summary(milk$AvgMilk60)
summary(milk_all$AvgMilk60)
summary(milk$sumMY60)
summary(milk_all$sumMY60)

ggplot(data=milk, aes(x=AvgMilk60)) +
  geom_histogram(breaks=seq(0, 60, by=3),
                 col="red", fill="green", alpha=0.2) +
  facet_wrap(milk$FarmNo) +
  ggtitle("First 60d Average Milk Yield in 38 Farms")

ggplot(data=milk, aes(x=AvgMilk60)) +
  geom_histogram(breaks=seq(0, 70, by=3),
                 col="red", fill="green", alpha=0.2) +
  facet_wrap(milk$ad_Lactation) +
  ggtitle("First 60d Average Milk Yield in Different Parities") +
  xlab("Mean Milk Yield in 60 DIM") +
  ylab("No. of Cows")

df.table1 <- milk %>%
  select(FarmNo, CowId, ad_Lactation, AvgMilk60) %>%
  filter(ad_Lactation == "1") 
summary(df.table1)

df.table2 <- milk %>%
  select(FarmNo, CowId, ad_Lactation, AvgMilk60) %>%
  filter(ad_Lactation == "2") 
summary(df.table2)

df.table3 <- milk %>%
  select(FarmNo, CowId, ad_Lactation, AvgMilk60) %>%
  filter(ad_Lactation == "3") 
summary(df.table3)

df.table4 <- milk %>%
  select(FarmNo, CowId, ad_Lactation, AvgMilk60) %>%
  filter(ad_Lactation == "4") 
summary(df.table4)
rm(list=ls(pattern="df"))

hist(milk$AvgMilk80, main = "Histogram of average 80 d Milk Yield", xlab = "Average 80 d Milk Yield (kg)")
summary(milk$AvgMilk80)
summary(milk$sumMY80)
summary(milk_all$AvgMilk80)
summary(milk_all$sumMY80)

ggplot(data=milk, aes(x=AvgMilk80)) +
  geom_histogram(breaks=seq(0, 70, by=3),
                 col="red", fill="green", alpha=0.2) +
  facet_wrap(milk$ad_Lactation) +
  ggtitle("First 80d Average Milk Yield in Different Parities")+
  xlab("Mean Milk Yield in 80 DIM") +
  ylab("No. of Cows")

ggplot(data=milk, aes(x=AvgMilk80)) +
  geom_histogram(breaks=seq(0, 70, by=3),
                 col="red", fill="green", alpha=0.2) +
  facet_wrap(milk$FarmNo) +
  ggtitle("First 80d Average Milk Yield in 38 Farms") +
  xlab("Mean Milk Yield in 80 DIM") +
  ylab("No. of Cows")

df.table1 <- milk %>%
  select(FarmNo, CowId, ad_Lactation, AvgMilk80) %>%
  filter(ad_Lactation == "1") 
summary(df.table1)

df.table2 <- milk %>%
  select(FarmNo, CowId, ad_Lactation, AvgMilk80) %>%
  filter(ad_Lactation == "2") 
summary(df.table2)

df.table3 <- milk %>%
  select(FarmNo, CowId, ad_Lactation, AvgMilk80) %>%
  filter(ad_Lactation == "3") 
summary(df.table3)

df.table4 <- milk %>%
  select(FarmNo, CowId, ad_Lactation, AvgMilk80) %>%
  filter(ad_Lactation == "4") 
summary(df.table4)
rm(list=ls(pattern="df"))

hist(milk$AvgMilk100, main = "Histogram of average 100 d Milk Yield", xlab = "Average 100 d Milk Yield (kg)")
summary(milk$AvgMilk100)
ggplot(data=milk, aes(x=AvgMilk100)) +
  geom_histogram(breaks=seq(0, 70, by=3),
                 col="red", fill="green", alpha=0.2) +
  facet_wrap(milk$FarmNo) +
  ggtitle("First 100d Average Milk Yield in 38 Farms")
ggplot(data=milk, aes(x=AvgMilk100)) +
  geom_histogram(breaks=seq(0, 70, by=3),
                 col="red", fill="green", alpha=0.2) +
  facet_wrap(milk$ad_Lactation) +
  ggtitle("First 100d Average Milk Yield in Different Parities")

hist(milk$AvgMilk305, main = "Histogram of average 305 d Milk Yield", xlab = "Average 305 d Milk Yield (kg)")
summary(milk$AvgMilk305)
summary(milk$sumMY305)
summary(milk_all$AvgMilk305)
summary(milk_all$sumMY305)

ggplot(data=milk, aes(x=AvgMilk305)) +
  geom_histogram(breaks=seq(0, 70, by=3),
                 col="red", fill="green", alpha=0.2) +
  facet_wrap(milk$FarmNo) +
  ggtitle("First 305d Average Milk Yield in 38 Farms")
ggplot(data=milk, aes(x=AvgMilk305)) +
  geom_histogram(breaks=seq(0, 70, by=3),
                 col="red", fill="green", alpha=0.2) +
  facet_wrap(milk$ad_Lactation) +
  ggtitle("First 305d Average Milk Yield in Different Parities") +
  xlab("Mean Milk Yield in 305 DIM") +
  ylab("No. of Cows")

df.table1 <- milk %>%
  select(FarmNo, CowId, ad_Lactation, AvgMilk305) %>%
  filter(ad_Lactation == "1") 
summary(df.table1)

df.table2 <- milk %>%
  select(FarmNo, CowId, ad_Lactation, AvgMilk305) %>%
  filter(ad_Lactation == "2") 
summary(df.table2)

df.table3 <- milk %>%
  select(FarmNo, CowId, ad_Lactation, AvgMilk305) %>%
  filter(ad_Lactation == "3") 
summary(df.table3)

df.table4 <- milk %>%
  select(FarmNo, CowId, ad_Lactation, AvgMilk305) %>%
  filter(ad_Lactation == "4") 
summary(df.table4)
rm(list=ls(pattern="df"))

#Peak DIM
ggplot(data=milk, aes(x=PeakDim)) +
geom_histogram(breaks=seq(0, 650, by=20),
               col="red", fill="green", alpha=0.2) +
  ggtitle("Peak DIM, where the highest milk yield achieved")+
  xlab("DIM")
summary(milk$PeakDim)

#Peak Yield
ggplot(data=milk, aes(x=PeakYield)) +
  geom_histogram(breaks=seq(0, 90, by=3),
                 col="red", fill="green", alpha=0.2) +
  ggtitle("The highest 7 days rolling average milk yield")+
  xlab("Milk Yield (kg)")
summary(milk$PeakYield)

#sumMY20
ggplot(data=milk, aes(x=sumMY20)) +
  geom_histogram(breaks=seq(0, 2000, by=3),
                 col="red", fill="green", alpha=0.2) +
  ggtitle("First 20d Culmulative Milk Yield") 
summary(milk$sumMY20)

ggplot(data=milk, aes(x=sumMY20)) +
  geom_histogram(breaks=seq(0, 2000, by=3),
                 col="red", fill="green", alpha=0.2) +
  ggtitle("First 20d Culmulative Milk Yield in different parities") +
  facet_wrap(milk$ad_Lactation)

#sumMY60
summary(milk$sumMY60)
ggplot(data=milk, aes(x=sumMY60)) +
  geom_histogram(breaks=seq(0, 6500, by=3),
                 col="red", fill="green", alpha=0.2) +
  ggtitle("First 60d Culmulative Milk Yield")

ggplot(data=milk, aes(x=sumMY60)) +
  geom_histogram(breaks=seq(0, 6500, by=3),
                 col="red", fill="green", alpha=0.2) +
  ggtitle("First 60d Culmulative Milk Yield in different parities") +
  facet_wrap(milk$ad_Lactation)

#sumMY80
summary(milk$sumMY80)
ggplot(data=milk, aes(x=sumMY80)) +
  geom_histogram(breaks=seq(0, 9000, by=3),
                 col="red", fill="green", alpha=0.2) +
  ggtitle("First 80d Culmulative Milk Yield")

ggplot(data=milk, aes(x=sumMY80)) +
  geom_histogram(breaks=seq(0, 9000, by=3),
                 col="red", fill="green", alpha=0.2) +
  ggtitle("First 80d Culmulative Milk Yield in different parities") +
  facet_wrap(milk$ad_Lactation)

#sumMY305
summary(milk$sumMY305)
ggplot(data=milk, aes(x=sumMY305)) +
  geom_histogram(breaks=seq(0, 30000, by=50),
                 col="red", fill="green", alpha=0.2) +
  ggtitle("First 305d Culmulative Milk Yield")
ggplot(data=milk, aes(x=sumMY305)) +
  geom_histogram(breaks=seq(0, 30000, by=50),
                 col="red", fill="green", alpha=0.2) +
  facet_wrap(milk$ad_Lactation) +
  ggtitle("First 305d Culmulative Milk Yield in different parities")


# merge repro and the avg milk yield #
repro_milk <- merge(repro, milk, by = c('FarmNo', 'CowId', 'Lactation', 'CalvingDate'), all.x = TRUE, all.y = TRUE, sort = F)
repro_milk$ad_Lactation <- NULL
summary(repro_milk)
repro_milk <- repro_milk[!is.na(repro_milk$FarmNo), ]

ggplot(data=repro_milk, aes(x=Lactation)) +
  geom_bar(stat = "count", fill = "orange") +
  stat_count(geom = "text", size= 3, aes(label=..count..))

scatter.hist(x = repro_milk$ad_Lactation, y = repro_milk$AvgMilk305, xlab = "Lactation", ylab = "Average 305d Milk Yield")
?scatter.hist

df.avgmilk60 <- repro_milk %>%
  select(FarmNo, Lactation, AvgMilk60) %>%
  group_by(FarmNo) %>%
  mutate(FarmAvgMY = mean(AvgMilk60, na.rm=TRUE), sd = sd(AvgMilk60, na.rm=TRUE))

ggplot(data=df.avgmilk60, aes(x=AvgFarmMY))