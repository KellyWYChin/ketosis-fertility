library(readxl)
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(psych)
library(reader)
library(lubridate)

##### MERGE EXCEL SHEETS #######################################################
################################################################################
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
# number of cows 
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

# select pregnancy events of interests
df.preg <- all_farm %>%
  select(FarmNo, CowId, BirthDate, CalvingDate, Lactation, CullDate, InsDate, InsNumber, NextIns, PregDate.x, PregDate.y, DeterminationType, AbortionDate, DaysToAbort)
summary(df.preg)
#Remove rows with FarmNo = NA
df.preg1 <- df.preg[!is.na(df.preg$FarmNo), ]
summary(df.preg1)

# number of cows 
df.cow <- df.preg1 %>%
  distinct(FarmNo, CowId)

### Check data consistency (PregDate - Calving Date) & (CullDate - Calving Date) 
################################################################################
# create new variables concerning time from calving to culling
df.preg1$CalvingDate <- as.Date(df.preg1$CalvingDate)
df.preg1$PregDate.y <- as.Date(df.preg1$PregDate.y)
df.preg1$CullDate <- as.Date(df.preg1$CullDate)
#date that CullDate is before 2018-06-01 should be nonsense in this dataset
df.preg1$CullDate[df.preg1$CullDate <= "2018-05-31"] <- NA
df.preg1$preg.calving <- difftime(df.preg1$PregDate.y, df.preg1$CalvingDate, units="days")
df.preg1$cull.calving <- difftime(df.preg1$CullDate, df.preg1$CalvingDate, units="days")
df.preg1$Culled <- ifelse(!is.na(df.preg1$cull.calving), 1, 0)
summary(df.preg1)
#exclude rows that have been culled within 150 DIM
df.preg1 <- df.preg1[-which(df.preg1$cull.calving < 150), ]

# number of cows 
df.cow <- df.preg1 %>%
  distinct(FarmNo, CowId)

#Remove column PregDate.x
df.preg1$PregDate.x <- NULL

### Check (AbortionDate - PregDate) ############################################
df.preg1$AbortionDate <- as.Date(df.preg1$AbortionDate)
df.preg1$abort.preg <- difftime(df.preg1$AbortionDate, df.preg1$PregDate.y, units = "days")
df.preg1$abort.preg <- as.numeric(df.preg1$abort.preg)

### Check data consistency (PregDate - Insemination Date) ######################
df.preg1$InsDate <- as.Date(df.preg1$InsDate)
df.preg1$preg.ins <- difftime(df.preg1$PregDate.y, df.preg1$InsDate, units="days")
df.preg1$preg.ins <- as.numeric(df.preg1$preg.ins)

# Select the pregnancy determined by HN system (DeterminationType = 2) and NAs
df.preg1 <- df.preg1[(df.preg1$DeterminationType == 2 | is.na(df.preg1$DeterminationType)), ]
# number of cows 
df.cow <- df.preg1 %>%
  distinct(FarmNo, CowId)

## Exclude inconsistent cows
#exclude rows that have negative values of time between pregnancy and calving
df.preg1 <- df.preg1[(df.preg1$preg.calving >=0 | is.na(df.preg1$preg.calving)), ]
# number of cows 
df.cow <- df.preg1 %>%
  distinct(FarmNo, CowId)
#exclude rows that have negative values of time between prengancy and insemination
df.preg1 <- df.preg1[(df.preg1$preg.ins >=0 | is.na(df.preg1$preg.ins)), ]
# number of cows 
df.cow <- df.preg1 %>%
  distinct(FarmNo, CowId)
#exclude rows with negative DaysToAbort
df.preg1 <- df.preg1[(df.preg1$DaysToAbort >=0 | is.na(df.preg1$DaysToAbort)), ]
# number of cows 
df.cow <- df.preg1 %>%
  distinct(FarmNo, CowId)
################################################################################
########### list the first insemination, one cow in only one row ###############
df.ins1 <- df.preg1 %>%
  group_by(FarmNo, CowId, Lactation, CalvingDate) %>%
  summarize(InsNumber_1 = min(InsNumber), 
            PregDate_1 = min(PregDate.y), 
            InsDate_1=min(InsDate), 
            AbortionDate_1 = min(AbortionDate))

#list the last insemination, one cow in only one row
df.insN <- df.preg1 %>%
  group_by(FarmNo, CowId, Lactation, CalvingDate) %>%
  summarize(InsNumber_N = max(InsNumber), 
            PregDate_N = max(PregDate.y), 
            InsDate_N = max(InsDate), 
            AbortionDate_N = max(AbortionDate))
summary(df.insN)
table(df.insN$InsNumber_N) #How many times does a cow be inseminated?
#number of cows in Number of AI
df.insN$InsNumber_N <- as.factor(df.insN$InsNumber_N)
ggplot(data=df.insN, aes(x=factor(InsNumber_N))) +
  geom_bar(stat="count", fill="lightblue") +
  stat_count(geom="text", size=3, aes(label=..count..)) +
  ggtitle("Number of Cows and Number of AI") + xlab("Number of AI") 
table(df.ins1$InsNumber_1)
str(df.insN)
df.data <- merge(df.ins1, df.insN, 
                 by=c('FarmNo', 'CowId', 'Lactation', 'CalvingDate'), all.x=TRUE, all.y=TRUE, sort=F)
df.merge1 <- merge(df.preg1, df.data, 
                   by=c('FarmNo', 'CowId', 'Lactation', 'CalvingDate'), all.X =TRUE, sort=F)
#remove unnecessary x columns
df.merge1$BirthDate <- NULL
df.merge1$InsDate <- NULL
df.merge1$InsNumber <- NULL
df.merge1$AbortionDate <- NULL
df.merge1$NextIns <- NULL

df.preg1 <- df.merge1 %>%
  distinct(FarmNo, CowId, Lactation, CalvingDate, .keep_all=TRUE)
summary(df.preg1)
### Select useful reproduction events ###########################################
df.repro <- all_farm %>%
  select(FarmNo, CowId, CalvingDate, Lactation, DFC_CLA, DFCFirstHeat, DaysToFirstIns, DaysToLastIns, DaysBtwFirstAndLastIns) %>%
  distinct(FarmNo, CowId, Lactation, .keep_all=TRUE)
summary(df.repro)

df.preg <- merge(df.preg1, df.repro, 
                   by=c('FarmNo', 'CowId', 'Lactation', 'CalvingDate'), all.x=TRUE, sort=F)
summary(df.preg)

# time from first AI to first detected cyclic
df.preg$Ins.Calving <- as.numeric(difftime(df.preg$InsDate_1, df.preg$CalvingDate, units="days"))
# time between first AI to DFC_CLA
df.preg$FirstAItoFirstCLA <- as.numeric(df.preg$Ins.Calving-df.preg$DFC_CLA)
summary(df.preg)
# time between first AI to first detected heat (DFCFirstHeat)
df.preg$FirstAItoFirstHeat <- as.numeric(df.preg$Ins.Calving-df.preg$DFCFirstHeat)
summary(df.preg)

##### Exclude duplicated cows ##################################################
df.preg$farmcow <- paste(df.preg$FarmNo, df.preg$CowId, sep="")
repro <- df.preg %>%
  mutate(farmcow.dup = ifelse(duplicated(farmcow)|
                                duplicated(farmcow, fromLast=TRUE), 1, 0))
repro <- repro[(repro$farmcow.dup == 0), ]
repro$farmcow.dup <- NULL
repro$farmcow <- NULL
summary(repro)

####### Data exclusion #########################################################
### Exclude cows without DFC_CLA ###############################################
repro <- repro[!is.na(repro$DFC_CLA), ]
### Exclude cows with DFC_CLA less than 20 and more than 200
repro <- repro[-which(repro$DFC_CLA < 20 | repro$DFC_CLA > 200), ]
summary(repro)
### Exclude cows DaysToFirstIns <20 DIM and >250 DIM
repro <- repro[-which(repro$DaysToFirstIns < 20 | repro$DaysToFirstIns >250), ]
### Exclude cows DFCFirstHeat <20 DIM and >200 DIM
repro <- repro[-which(repro$DFCFirstHeat < 20 | repro$DFCFirstHeat > 200), ]
summary(repro)

# Check data consistency
summary(repro$Ins.Calving)
summary(repro$FirstAItoFirstCLA)
summary(repro$FirstAItoFirstHeat)

# Exclude inconsistent cows
repro <- repro[-which(repro$FirstAItoFirstCLA < 0), ]
summary(repro$FirstAItoFirstCLA)
summary(repro$FirstAItoFirstHeat)

repro <- repro[-which(repro$FirstAItoFirstHeat < -1), ]
summary(repro$FirstAItoFirstHeat)
summary(repro)
rm(list=ls(pattern="df"))

##### Define pregnancy status after the last AI ################################
repro <- repro %>%
  add_column(PregStatus = ifelse((repro$PregDate_N==TRUE), NA, "1"), .after="PregDate_N")
repro$PregStatus <- ifelse((is.na(repro$PregDate_N) & !is.na(repro$InsDate_N)), "0", repro$PregStatus)
table(repro$PregStatus)

#################### Define endtime ############################################
#calculate the time difference between insemination date and pregnancy date
repro$PregDate_N <- as.Date(repro$PregDate_N)
repro$InsDate_N <- as.Date(repro$InsDate_N)
repro$ins.preg <- as.numeric(difftime(repro$PregDate_N, repro$InsDate_N, units="days"))
summary(repro$ins.preg1)
repro$ins.preg <- ifelse((repro$InsNumber_N==1), difftime(repro$PregDate_1, repro$InsDate_1, units="days"), 
                        difftime(repro$PregDate_N, repro$InsDate_N, units="days"))
repro$ins.preg <- as.numeric(repro$ins.preg)
summary(repro$ins.preg)
repro$ins.preg1 <- ifelse((repro$ins.preg > 300), difftime(repro$PregDate_1, repro$InsDate_N, units="days"),
                         repro$ins.preg)

#calculate the time difference between last insemination date and calving date
repro$Endtime <- as.numeric(difftime(repro$InsDate_N, repro$CalvingDate, units="days"))
summary(repro$Endtime)
hist(repro$Endtime)

repro$Endtime1 <- ifelse((repro$InsNumber_N==1), difftime(repro$InsDate_1, repro$CalvingDate, units="days"), 
                        difftime(repro$InsDate_N, repro$CalvingDate, units="days"))
repro$diffEndtime <- NULL
repro$Endtime1 <- NULL

################# considering abortion date ####################################
#cows that abort after 1st insemination
repro$ins.abo1 <- as.numeric(difftime(repro$AbortionDate_1, repro$InsDate_1, units="days"))
#cows that abort after last insemination
repro$ins.aboN <- as.numeric(difftime(repro$AbortionDate_N, repro$InsDate_N, units="days"))

################ whether the cow gets pregnant in the first AI?#################
repro$FirstAI_DFC <- as.numeric(difftime(repro$InsDate_1, repro$CalvingDate, units="days"))
repro <- repro %>%
  add_column(PregStatus_1 = ifelse(!(repro$InsNumber_N==1), 0, 1), .after="PregDate_1")
#################### Define seasons ############################################
getSeason <- function(DATES){
  WS <- as.Date("2012-1-1", format="%Y-%m-%d")
  SE <- as.Date("2012-4-1", format="%Y-%m-%d")
  SS <- as.Date("2012-7-1", format="%Y-%m-%d")
  FE <- as.Date("2012-10-1", format="%Y-%m-%d")
  
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  ifelse(d >= WS & d < SE, "Winter", 
         ifelse(d >= SE & d < SS, "Spring", 
                ifelse(d >= SS & d < FE, "Summer", "Fall")))
}
repro <- repro %>%
  add_column(Season = 
               getSeason(repro$CalvingDate), .after="CalvingDate")
repro$`Season <- getSeason(repro$CalvingDate)` <- NULL
repro$Season.1 <- NULL
################################################################################
#########create a new variable ECC (early/late conceived cows)##################
repro$ECC <- ifelse((repro$Ins.Calving <= 80 & repro$PregStatus_1 == "1"), 1, 0)
