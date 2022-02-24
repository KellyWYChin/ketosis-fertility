library(readxl)
remove.packages("rlang")
install.packages("rlang")
install.packages("conjurer")
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(psych)
library(skimr)
library(reader)
library(lubridate)
df.calving <- read_excel("1001, All Data, Proc, June 2020.xlsx", sheet = "Calving")
df.cowinfo <- read_excel("1001, All Data, Proc, June 2020.xlsx", sheet = "CowInfo")
df.culldata <- read_excel("1001, All Data, Proc, June 2020.xlsx", sheet = "CullData")
df.calving <- read_excel("1001, All Data, Proc, June 2020.xlsx", sheet = "Calving")
df.insemination <- read_excel("1001, All Data, Proc, June 2020.xlsx", sheet = "Insemination")
df.pregnancy <- read_excel("1001, All Data, Proc, June 2020.xlsx", sheet = "Pregnancy")

df.milk <- read_excel("1001, All Data, Proc, June 2020.xlsx", sheet = "MilkData")
df.repro <- read_excel("1001, All Data, Proc, June 2020.xlsx", sheet = "ReproData")
warning()
df.keto <- read_excel("1001, All Data, Proc, June 2020.xlsx", sheet = "Ketosis")

colnames(df.cowinfo)[colnames(df.cowinfo)=="CowID"] <- "CowId"

#farm 1001-merged file
df.master01 <- merge(df.cowinfo, df.calving, by = "CowId") %>%
  merge(df.culldata, by = "CowId", all.x = TRUE) %>%
  merge(df.insemination, by = "CowId") %>%
  merge(df.pregnancy, by = "CowId")
df.masterdaily01 <- merge(df.milk, df.keto, by = "CowId")
##### can not add df.repro into the merged data frame?
summary(df.masterdaily01)

df.masterdaily01 <- data.frame()
all_farm_daily <- data.frame()

#merge daily measurement of milkdata and ketosis into one data frame
for(i in 1001:1038){
  # create the excel file name base on the input i
  filename <- paste("C:/Data/",i,", All Data, Proc, June 2020.xlsx", sep = "")
  # load the sheets within the same excel file (the same farm)
  df.milk <- read_excel(filename, sheet = "MilkData")
  df.keto <- read_excel(filename, sheet = "Ketosis")
  # merge the data of different sheets within the same farm
  df.farmdaily <- merge(df.milk, df.keto, by = "CowId")
  # append all the merged data into the master final data table
  all_farm_daily <- bind_rows(all_farm_daily, df.farmdaily)
}
all_farm_daily <- data.frame()

df.farmdaily <-
  df.milk %>%
  full_join(df.keto, by="CowId")

summary(df.all_farmdaily)

#number of cows in farm 1001
ncow <- df.master01 %>%
  group_by(CowId) %>%
  summarise(cow_count=n()) %>%
  arrange(desc(cow_count))
#number of cows in farm 1038
ncow38 <- df.master38 %>%
  group_by(CowId) %>%
  summarise(cow_count=n()) %>%
  arrange(desc(cow_count))
rlang::last_error()

#farm 1038-merged file
df.calving38 <- read_excel("1038, All Data, Proc, June 2020.xlsx", sheet = "Calving")
df.cowinfo38 <- read_excel("1038, All Data, Proc, June 2020.xlsx", sheet = "CowInfo")
#Change BirthDate into character
df.cowinfo38$BirthDate <- as.character(df.cowinfo38$BirthDate)
df.culldata38 <- read_excel("1038, All Data, Proc, June 2020.xlsx", sheet = "CullData")
df.calving38 <- read_excel("1038, All Data, Proc, June 2020.xlsx", sheet = "Calving")
df.insemination38 <- read_excel("1038, All Data, Proc, June 2020.xlsx", sheet = "Insemination")
df.pregnancy38 <- read_excel("1038, All Data, Proc, June 2020.xlsx", sheet = "Pregnancy")
#change the CowID in sheet cowinfo into CowId
colnames(df.cowinfo38)[colnames(df.cowinfo38)=="CowID"] <- "CowId"
df.master38 <- merge(df.cowinfo38, df.calving38, by = "CowId") %>%
  merge(df.culldata38, by = "CowId", all.x = TRUE) %>%
  merge(df.insemination38, by = "CowId") %>%
  merge(df.pregnancy38, by = "CowId")
df.master38 <- data.frame()
str(df.master38)
str(df.master01)
df.cowinfo$BirthDate <- as.POSIXct("1038, All Data, Proc, June 2020. xlsx", format="%d-%b-%Y %H:%M:%OS")
df.cowinfo$BirthDate <- as.character(df.cowinfo$BirthDate)

##### Create an excel file for merged data #####
write.csv(df.all_farm, "All_data.csv")
# Delete the data that starts with "df.calving"
rm(list=ls(pattern="df.calving"))

#######################################################################
########################## Reproduction ###############################
#######################################################################
# empty the data frame first
all_farm_raw <- data.frame()

for(i in 1001:1038){
  # create the excel file name base on the input i
  filename <- paste("C:/Data/",i,", All Data, Proc, June 2020.xlsx", sep = "")

  # load the sheets within the same excel file (the same farm)
  df.calving <- read_excel(filename, sheet = "Calving")
  df.cowinfo <- read_excel(filename, sheet = "CowInfo")
  # change the column name of cowinfo (CowID -> CowId)
  colnames(df.cowinfo)[colnames(df.cowinfo)=="CowID"] <- "CowId"
  # change the column BirthDate into character
  df.cowinfo$BirthDate <- as.character(df.cowinfo$BirthDate)
  df.culldata <- read_excel(filename, sheet = "CullData")
  df.calving <- read_excel(filename, sheet = "Calving")
  df.insemination <- read_excel(filename, sheet = "Insemination")
  df.pregnancy <- read_excel(filename, sheet = "Pregnancy")

  # merge the data of different sheets within the same farm
  df.farm <- merge(df.cowinfo, df.calving, by = "CowId", all.x=TRUE, all.y=TRUE) %>%
    merge(df.culldata, by = "CowId", all.x = TRUE, all.y=TRUE) %>%
    merge(df.insemination, by = "CowId", all.x=TRUE, all.y=TRUE) %>%
    merge(df.pregnancy, by = "CowId", all.x=TRUE, all.y=TRUE)

  # append all the merged data into the master final data table
  all_farm_raw <- bind_rows(all_farm_raw, df.farm)
}
rm(list=ls(pattern="df"))
summary(all_farm_raw)
# number of cows in total (at the very beginning)
df.cow <- all_farm_raw %>%
  distinct(FarmNo, CowId)
# Delete all the rows that without calving date
all_farm <- subset(all_farm_raw, !is.na(all_farm_raw$CalvingDate))
df.cow <- all_farm %>%
  distinct(FarmNo, CowId)
rm(all_farm_raw)
nrow(all_farm)
str(all_farm)
summary(all_farm)
all_farm$BirthDate <- as.Date(all_farm$BirthDate)

str(all_farm$NextIns)
# change weird date signs to NAs #
all_farm$EEDDate <- as.Date(all_farm$NextIns)
all_farm$EEDDate[all_farm$EEDDate == "1899-12-31"] <- NA
all_farm$NextIns <- as.Date(all_farm$NextIns)
all_farm$NextIns[all_farm$NextIns == "1899-12-31 00:00:00"] <- NA
all_farm$PregDate.x <- as.Date(all_farm$PregDate.x)
all_farm$PregDate.x[all_farm$PregDate.x == "1899-12-31 00:00:00"] <- NA
all_farm$AbortionDate <- as.Date(all_farm$AbortionDate)
all_farm$AbortionDate[all_farm$AbortionDate == "1899-12-31 00:00:00"] <- NA
summary(all_farm)
summary(all_farm$CohortExitDate)



# select the columns that are going to use #
work <- all_farm%>%
  select(FarmNo, CowId, BirthDate, CalvingDate, Lactation, DFC_CLA, DFCFirstHeat, DaysToFirstIns, DaysToLastIns, DaysBtwFirstAndLastIns, InsDate, InsNumber, PregDate.x, PregDate.y, AbortionDate, DaysToAbort) %>%
  distinct(FarmNo, CowId, Lactation, InsNumber, PregDate.x, .keep_all=TRUE)
df.work.ind <- work %>%
  distinct(FarmNo, CowId, .keep_all = TRUE)
df.work.lact <- work %>%
  distinct(FarmNo, CowId, Lactation, .keep_all=TRUE)
# list the first insemination, one cow in only one row #
df.ins1 <- all_farm %>%
  group_by(FarmNo, CowId, Lactation) %>%
  summarize(Ins_1=min(InsNumber), PregDate_1=min(PregDate.x), InsDate_1=min(InsDate), AbortionDate_1=min(AbortionDate))
# list the last insemination, one cow in only one row #
df.insN <- all_farm %>%
  group_by(FarmNo, CowId, Lactation) %>%
  summarize(Ins_N=max(InsNumber), PregDate_N=max(PregDate.x), InsDate_N=max(InsDate), AbortionDate_N=max(AbortionDate))
df.insN$Ins_N[df.insN$Ins_N == 1] <- NA
df.insN$PregDate_N <- ifelse(is.na(df.insN$Ins_N), NA, as.character(df.insN$PregDate_N))
df.insN$InsDate_N <- ifelse(is.na(df.insN$Ins_N), NA, as.character(df.insN$InsDate_N))
df.insN$AbortionDate_N <- as.Date(df.insN$AbortionDate_N)
which.max(df.insN$Gap)
# check data consistency, whether the time differences between Abortion date and insemination date are positive #
df.insN$PregDate_N <- as.Date(df.insN$PregDate_N)
df.insN$InsDate_N <- as.Date(df.insN$InsDate_N)
df.insN$Gap <- difftime(df.insN$AbortionDate_N, df.insN$InsDate_N, units="days")
df.insN$Gap <- as.numeric(df.insN$Gap)
summary(df.insN$Gap)
df.insN$Gap[which(df.insN$Gap > 0)]
df.insN$Gap <- NULL
df.ins1$InsDate_1 <- as.Date(df.ins1$InsDate_1)
df.ins1$PregDate_1 <- as.Date(df.ins1$PregDate_1)
df.ins1$AbortionDate_1 <- as.Date(df.ins1$AbortionDate_1)
df.ins1$Gap <- difftime(df.ins1$AbortionDate_1, df.ins1$InsDate_1, units="days")
df.ins1$Gap <- as.numeric(df.ins1$Gap)
summary(df.ins1$Gap)
df.ins1$Gap <- NULL

df.ins.all <- data.frame()
df.ins.all <- merge(df.ins1, df.insN, by = c('FarmNo', 'CowId', 'Lactation'), all.y=TRUE, sort=F)
df.work_repro <- data.frame()
df.work_repro <- merge(work, df.ins.all,
                       by = c('FarmNo', 'CowId', 'Lactation'), all.y = TRUE, sort=F) %>%
  distinct(FarmNo, CowId, Lactation, .keep_all=TRUE)
# delete columns #
df.work_repro$PregDate.x <- NULL
df.work_repro$PregDate.y <- NULL
df.work_repro$InsDate <- NULL
df.work_repro$InsNumber <- NULL
repro <- df.work_repro
rm(list=ls(pattern="df"))

# create a new column, PregStatus #
repro$PregStatus <- NA
repro[is.na(repro$PregDate_1) & is.na(repro$PregDate_N) & is.na(repro$AbortionDate_1) & is.na(repro$AbortionDate_N), ]$PregStatus<- FALSE
repro[!is.na(repro$PregDate_1) | !is.na(repro$PregDate_N), ]$PregStatus<- TRUE
repro[is.na(repro$PregDate_1) & is.na(repro$PregDate_N) & is.na(repro$Ins_1) & is.na(repro$Ins_N), ]$PregStatus<- NA
repro[!is.na(repro$InsDate_N > repro$AbortionDate_N), ]$PregStatus <- FALSE
################################################################################
####### how about abortion happened after the last insemination ??? ############
################################################################################
repro$PregStatus[repro$PregStatus == TRUE] <- 1

summary(repro)
which.max(repro$DFC_CLA)
which.min(repro$DFC_CLA)
which.min(repro$DFCFirstHeat)
summary(work)

# open days calculation #
repro$endtime <- NA

#listed farm ID and how many data within a farm
table(FarmNo)
#same:
counts <- data.frame(table(FarmNo))
counts

#number of farms
length(unique(FarmNo))
#number of cows?
#number of cows in all farms
ncow.all <- all_farm_raw %>%
  group_by(FarmNo, CowId) %>%
  dplyr::summarise(cow_count=n()) %>%
  arrange(desc(cow_count))
#how many cows in each farm?
factor(ncow.all$FarmNo) #to see what is contained in FarmNo (Level)
w=table(ncow.all$FarmNo)
w
as.data.frame(w)
summary(lact.master)
library(plyr)

count(ncow.all, 'FarmNo')
#barplot in R to show how many cows in each farm
counts <- table(ncow.all$FarmNo)
barplot(counts, main="No. of cows in each farm", xlab="Farm No.", ylab="No. of Cows", ylim=c(0, 400))
#average herd size
mean(counts)

test <- data.frame()
test <- all_farm_raw %>% distinct(FarmNo, CowId, BirthDate, CalvingDate, PregDate.x, InsDate, AbortionDate, .keep_all=TRUE)
summary(test)
test$PregDate.x[test$PregDate.x == "1899-12-31 00:00:00"] <- NA

######## select columns & Lactation ########
lact.all <- select(all_farm_raw, CowId, FarmNo, BirthDate, CalvingDate, Lactation)
#remove duplicate rows based on FarmNo, CowId, and Lactation (dplyr package)
lact.master <- lact.all %>% distinct(FarmNo, CowId, .keep_all=TRUE)
rm(lact.all)
rm(h2.fh)

lact.master %>%
  group_by(FarmNo, CowId) %>%
  summarise(avg_lact = mean(Lactation))
#barplot of lactation
counts.lact <- table(lact.master$Lactation)
counts.lact
barplot(counts.lact, main="Lactation Distribution", xlab="Lactation", ylab="No. of Cows", ylim=c(0, 1500))
#how many cows in each lactation?
w.lact = table(lact.master$Lactation)
w.lact
#lactation distribution of each farm?
p.lact <- ggplot(data=lact.master, aes(x=Lactation, y=counts.lact, group=FarmNo))
#lactation distribution of each farm
ggplot(data=lact.master, aes(x=factor(Lactation))) +
  geom_bar() +
  facet_wrap(lact.master$FarmNo)
#lactation distribution of 4435 cows from 38 farms
ggplot(data=lact.master, aes(x=factor(Lactation))) +
  geom_bar(stat = "count", fill="orange")+
  stat_count(geom = "text", size= 5, aes(label=..count..))
### make a new column age: Age of each cow (round down) ###
lact.master$Age <- difftime(lact.master$CalvingDate, lact.master$BirthDate, units="weeks")/52
lact.master$Diff <- round(lact.master$Age-lact.master$Lactation)
table(lact.master$Diff)

#categorize lactation into 3 groups (1, 2, 3+)
lact.master$Lactation[lact.master$Lactation==1] = 1
lact.master$Lactation[lact.master$Lactation==2] = 2
lact.master$Lactation[lact.master$Lactation >2] = 3

##### DFC_CLA: time from calving to resumption of cyclicity #####
dfc.master <- select(all_farm_raw, FarmNo, CowId, CalvingDate, Lactation, DFC_CLA) %>%
  distinct(FarmNo, CowId, .keep_all=TRUE)
summary(dfc.master$DFC_CLA)

#find the row with the max DFC_CLA
which.max(dfc.master$DFC_CLA)
dfc.master[711, ]
which.max(all_farm_raw$DFC_CLA)
all_farm_raw[7060, ]
#find the row with the min DFC_CLA
which.min(all_farm_raw$DFC_CLA)
all_farm_raw[29732, ]

#find the na's of DFC_CLA
which(is.na(dfc.master$DFC_CLA))
dfc.master[3012, ]
dfc.master[3380, ]

#DFC_CLA distribution of 4435 cows from 38 farms
hist(dfc.master$DFC_CLA)
h1.dfc <- hist(DFC_CLA, main="Days from calving to resupmtion of cyclicity", xlab="Days in milk", xlim=c(0, 400), breaks=20, ylim=c(0, 32000))
text(h1.dfc$mids, h1.dfc$counts, labels=h1.dfc$counts, adj=c(0.5, -0.5))

ggplot(data=dfc.master, aes(x=DFC_CLA))+
  geom_histogram(breaks=seq(0, 400, by=20),
                 col="red", fill="green",
                 alpha=0.2)+
  labs(title="Histogream for Time from calving to resumption of cyclicity", x="Days in milk", y="Count")

ggplot(data=dfc.master, aes(x=DFC_CLA))+
  geom_histogram()+
  facet_wrap(dfc.master$FarmNo)

##### DFCFirstHeat: time from calving to the first recognised heat alarm #####
fh.master <- select(all_farm_raw, FarmNo, CowId, CalvingDate, Lactation, DFC_CLA, DFCFirstHeat) %>%
  distinct(FarmNo, CowId, .keep_all=TRUE)
summary(fh.master$DFCFirstHeat)
which.max(all_farm_raw$DFCFirstHeat)
all_farm_raw[33812, ]
which(is.na(fh.master$DFCFirstHeat))

hist(fh.master$DFCFirstHeat)
h2.fh <- hist(fh.master$DFCFirstHeat, main="Time from calving to 1st recognised heat alarm", xlab="Days in milk", xlim=c(0, 350), breaks=20, ylim=c(0, 2000))
text(h2.fh$mids, h2.fh$counts, labels=h2.fh$counts, adj=c(0.5, -0.5))

str(all_farm_raw)
hist(all_farm_raw$CalvingDate)
plot(all_farm_raw$CowId~as.Date(all_farm_raw$CalvingDate, "%Y/%m/%d"), xlab="Date", ylab="#Cows")
all_farm_raw$CalvingDate <- as.Date(all_farm_raw$CalvingDate, "%Y/%m/%d")
lact.master$CalvingDate <- as.Date(lact.master$CalvingDate, "%m/%d/%Y")
plot(CowId ~ CalvingDate, lact.master, xaxt="n")
axis(1, lact.master$CalvingDate, format(lact.master$CalvingDate, "%b %d", cex.axis= .7))
axis(1, at=NULL, labels=F)
text(x=lact.master$CalvingDate, par("usr")[3]*.97, labels=paste(lact.master$CalvingDate,' '), srt=45, pos = 1, xpd = TRUE, cex=.5)

require(ggplot2)
ggplot(data=lact.master, aes(CalvingDate, CowId)) + geom_point()
table(lact.master$CalvingDate)

summary(lact.master)
ggplot(data=df.all_farmdaily, aes(SamplingTime, CowId)) + geom_point()
ggplot(data=lact.master, aes(CalvingDate)) + geom_histogram(position="dodge")

##### number of inseminations for each cow in every farm #####
N.D.Insemination <- data.frame()

for(i in 1001:1038){
  filename <- paste("C:/Data/",i,", All Data, Proc, June 2020.xlsx", sep = "")
  #open only ReproData sheet from each farm
  df.repro <- read_excel(filename, sheet = "ReproData")

  #create a new column which code the cows which were inseminated as 1, not-inseminated as 0
  df.repro$Ins <- ifelse(df.repro$Inseminate == "TRUE", 1, 0)
  #times of insemination per cow
  df.n_insem <- df.repro %>%
    group_by(CowId, FarmNo) %>%
    summarise(n.Ins=sum(Ins))

  df.fd_insem <- df.repro %>%
    group_by(CowId, FarmNo) %>%
    select(CowId, FarmNo, Inseminate, SamplingTime, DFC) %>%
    filter(Inseminate==TRUE) %>%
    slice_max(SamplingTime)

  df.n_insem <- merge(df.n_insem, df.fd_insem, by=c('FarmNo', 'CowId'), all.x=TRUE, all.y=TRUE)
  N.D.Insemination <- bind_rows(N.D.Insemination, df.n_insem)
}
# change the inseminate=NA to 0 #
N.D.Insemination$Inseminate <- as.character(N.D.Insemination$Inseminate)
N.D.Insemination$Inseminate <- ifelse(is.na(N.D.Insemination$Inseminate), "0", N.D.Insemination$Inseminate)

# change the name of the columns #
names(N.D.Insemination)[names(N.D.Insemination) == "DFC"] <- "DFC_last_Ins"
names(N.D.Insemination)[names(N.D.Insemination) == "Inseminate"] <- "Ins"

##### merge all farms with reproduction events and number of inseminations #####
all_farm_repro <- data.frame()
all_farm_repro <- merge(all_farm_raw, N.D.Insemination, by=c('FarmNo', 'CowId'), all.y=TRUE, all.x=TRUE)

##### milk BHB average#####
df.keto01 <- read_excel("1001, All Data, Proc, June 2020.xlsx", sheet = "Ketosis")
df.BHB.avg01 <- df.keto01 %>%
  group_by(CowId, FarmNo) %>%
  summarise(BHB.avg=mean(BHBraw))

df.all.BHBavg <- data.frame()
for(i in 1001:1038){
  filename <- paste("C:/Data/",i,", All Data, Proc, June 2020.xlsx", sep = "")
  df.keto <- read_excel(filename, sheet = "Ketosis")
  df.BHB.avg<- df.keto %>%
    group_by(CowId, FarmNo) %>%
    summarise(mean_BHB=mean(BHBraw))
  df.all.BHBavg <- bind_rows(df.all.BHBavg, df.BHB.avg)
}
summary(df.all.BHBavg)
table(df.all.BHBavg$FarmNo)
all_farm_keto_repro <- merge(all_farm_repro, df.all.BHBavg, by=c('CowId', 'FarmNo'), all.y=TRUE, all.x=TRUE)

##### merge the ketotic Cows and the earliest DFC where BHB > 0.08 #####
all_farm_keto <- data.frame()
for(i in 1001:1038){
  filename <- paste("C:/Data/",i,", All Data, Proc, June 2020.xlsx", sep = "")
  df.keto <- read_excel(filename, sheet = "Ketosis")
  ##### how many days that BHB>0.08 mmol/L ######
  df.keto$ketoCow <- ifelse(df.keto$BHBraw >= 0.08, 1, 0)
  df.n.ketoCow <- df.keto %>%
    group_by(CowId, FarmNo) %>%
    summarise(n.ketoCow=sum(ketoCow))
  ##### the earliest DFC when BHB>0.08 mmol/L together with the BHB value #####
  df.table <- df.keto %>%
    group_by(CowId) %>%
    select(CowId, FarmNo, ketoCow, BHBraw, DFC) %>%
    filter(ketoCow==1) %>%
    slice_min(DFC)
  df.n.ketoCow <- merge(df.n.ketoCow, df.table, by=c('CowId', 'FarmNo'), all.x=TRUE)
  all_farm_keto <- bind_rows(all_farm_keto, df.n.ketoCow)
}
all_farm_keto$ketoCow <- as.character(all_farm_keto$ketoCow)
all_farm_keto$ketoCow <- ifelse(is.na(all_farm_keto$ketoCow), "0", all_farm_keto$ketoCow)

# rename the BHBraw to "the first BHB>0.08" and DFC to "first DFC when BHB>0.08"
names(all_farm_keto)[names(all_farm_keto) == "BHBraw"] <- "first_ketoBHB"
names(all_farm_keto)[names(all_farm_keto) == "DFC"] <- "DFC_first_ketoBHB"

all_farm <- data.frame()
all_farm <- merge(all_farm_keto_repro, all_farm_keto, by=c('FarmNo', 'CowId'), all.y=TRUE, all.x=TRUE)
################################################################################
############### merge reproevents, keto ########################################
################################################################################
df.all <- merge(reproevents, keto, by=c('FarmNo', 'CowId', 'Lactation'), all.x=TRUE, all.y=TRUE)
summary(df.all)
df.all <- df.all[!is.na(df.all$CalvingDate), ]
table(df.all$ECC, df.all$KetoCow10)
hist(keto$nHighKeto20)
table(keto$nHighKeto20)
