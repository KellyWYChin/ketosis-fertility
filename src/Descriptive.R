summary(all_farm$DeterminationType)
hist(all_farm$DeterminationType)

#number of cows in a farm
attach(repro_milk_keto)
table(FarmNo)
duplicated(CowId)
threeinone <- repro_milk_keto %>%
  distinct(FarmNo, CowId, Lactation, .keep_all=TRUE)
detach(repro_milk_keto)
attach(threeinone)
table(FarmNo)g
summary(threeinone)
threeinone$season <- NA
str(CalvingDate)
CalvingDate <- as.Date(CalvingDate)

#Define seasons
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

threeinone$season <- getSeason(threeinone$CalvingDate)
str(threeinone$season)
summary(threeinone$season)
detach(threeinone)
attach(threeinone)
threeinone$season[threeinone$season=="Spring"] <- 1
threeinone$season[threeinone$season=="Summer"] <- 2
threeinone$season[threeinone$season=="Fall"] <- 3
threeinone$season[threeinone$season=="Winter"] <- 4
threeinone$season <- as.numeric(threeinone$season)

ggplot(data=threeinone, aes(x=AvgMilk20))+
  geom_histogram(breaks=seq(0,70, by=3),
                 col="red", fill="green", alpha=0.2)+
  facet_wrap(~threeinone$season) +
  ggtitle("Average Milk Yield in first 20d in four seasons")
ggplot(data=threeinone, aes(x=AvgMilk60))+
  geom_histogram(breaks=seq(0,70, by=3),
                 col="red", fill="green", alpha=0.2)+
  facet_wrap(~threeinone$season)+
  ggtitle("Average Milk Yield in first 60d in four seasons")
ggplot(data=threeinone, aes(x=AvgMilk100))+
  geom_histogram(breaks=seq(0,70, by=3),
                 col="red", fill="green", alpha=0.2)+
  facet_wrap(~threeinone$season)+
  ggtitle("Average Milk Yield in first 100d in four seasons")
ggplot(data=threeinone, aes(x=AvgMilk305))+
  geom_histogram(breaks=seq(0,70, by=3),
                 col="red", fill="green", alpha=0.2)+
  facet_wrap(~threeinone$season)+
  ggtitle("Average Milk Yield in first 305d in four seasons")

#(1st AI date)-(Calving date) should > 0. If not, there must be something wrong with the data
#Remove duplicated rows and inconsistant data
threeinone$AI_Calving <- difftime(threeinone$InsDate_1, threeinone$CalvingDate, units="days")
summary(threeinone$AI_Calving)
threeinone$AI_Calving <- as.numeric(threeinone$AI_Calving)
which(threeinone$AI_Calving > 365)

################################################################################
###  Exclude all the duplicated cows due to data inconsistency  ################
################################################################################
test1 <- threeinone %>%
  distinct(FarmNo, CowId, .keep_all=TRUE)
test2 <- threeinone %>%
  distinct(FarmNo, CowId, Lactation)
test2$farmcow <- paste(test2$FarmNo, test2$CowId, sep="")
test3 <- test2 %>%
  mutate(farmcow.dup=ifelse(duplicated(farmcow)|
                              duplicated(farmcow, fromLast=TRUE), 1, 0))
test3 <- test3[(test3$farmcow.dup == 0), ]

threeinone$farmcow <- paste(threeinone$FarmNo, threeinone$CowId, sep="")
threeinone <- threeinone %>%
  mutate(farmcow.dup = ifelse(duplicated(farmcow)|
                                duplicated(farmcow, fromLast=TRUE), 1, 0))
threeinone <- threeinone[(threeinone$farmcow.dup == 0), ]
threeinone$farmcow.dup <- NULL
threeinone$farmcow <- NULL
rm(list=ls(pattern="test"))

#Exclude cows without AvgMilk20
fix1 <- threeinone[!is.na(threeinone$AvgMilk20), ]
summary(fix1)
#Exclude cows without AvgKeto20
fix2 <- fix1[!is.na(fix1$AvgKeto20), ]
summary(fix2)

#number of cows in each farm
fix2$FarmNo <- as.factor(fix2$FarmNo)
ggplot(data=fix2, aes(x=fct_rev(fct_infreq(FarmNo)))) +
  geom_bar(stat="count", fill="orange") +
  stat_count(geom="text", size=3, aes(label=..count..)) +
  ggtitle("Number of Cows in Each Farm")

#numbers of cows in each season
ggplot(data=fix2, aes(x=factor(season))) +
  geom_bar(stat="count", fill="orange") +
  stat_count(geom="text", size=5, aes(label=..count..)) +
  ggtitle("Number of Cows in Calving Seasons")

#numbers of cows in 4 lactation categories
fix2$ad_Lactation <- as.factor(fix2$ad_Lactation)
ggplot(data=fix2, aes(x=factor(ad_Lactation)))+
  geom_bar(stat="count", fill="orange") +
  stat_count(geom="text", size=5, aes(label=..count..)) +
  ggtitle("Number of Cows in 4 Lactation Categories")

summary(fix2)
ggplot(data=fix2, aes(x=factor(Lactation)))+
  geom_bar(stat="count", fill="orange") +
  stat_count(geom="text", size=4, aes(label=..count..))+
  ggtitle("Number of Cows in each Lactation")
fix2$AI_Calving <- as.numeric(fix2$AI_Calving)
summary(fix2)
which.max(fix2$AI_Calving)

K
h4 <- hist(fix2$DaysToLastIns, xlab="Days to last insemination", ylim=c(0, 1500))
text(h4$mids, h4$counts, labels=h4$counts, adj=c(0.5, -0.5))
summary(fix2$DaysToLastIns)
rm(list=ls(pattern="h"))

###################################################################################
hist(fix2$AvgMilk20, main = "Histogram of average 20 d Milk Yield", xlab = "Average 20 d Milk Yield (kg)")
summary(fix2$AvgMilk20)
ggplot(data=fix2, aes(x=AvgMilk20)) +
  geom_histogram(breaks=seq(0, 60, by=3),
                 col="red", fill="green", alpha=0.2) +
  facet_wrap(fix2$FarmNo) +
  ggtitle("First 20d Average Milk Yield in 38 Farms")
ggplot(data=fix2, aes(x=AvgMilk20)) +
  geom_histogram(breaks=seq(0, 60, by=3),
                 col="red", fill="green", alpha=0.2) +
  facet_wrap(fix2$ad_Lactation) +
  ggtitle("First 20d Average Milk Yield in Different Parities")

hist(fix2$AvgMilk60, main = "Histogram of average 60 d Milk Yield", xlab = "Average 60 d Milk Yield (kg)")
summary(fix2$AvgMilk60)
ggplot(data=fix2, aes(x=AvgMilk60)) +
  geom_histogram(breaks=seq(0, 60, by=3),
                 col="red", fill="green", alpha=0.2) +
  facet_wrap(fix2$FarmNo) +
  ggtitle("First 60d Average Milk Yield in 38 Farms")
ggplot(data=fix2, aes(x=AvgMilk60)) +
  geom_histogram(breaks=seq(0, 70, by=3),
                 col="red", fill="green", alpha=0.2) +
  facet_wrap(fix2$ad_Lactation) +
  ggtitle("First 60d Average Milk Yield in Different Parities")
