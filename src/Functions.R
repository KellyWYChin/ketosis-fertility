# Cumulative milk yield, mean milk yield
cumulativeMY <- function(D){
  df.merge %>%
    group_by(FarmNo, CowId, CalvingDate) %>%
    select(Lactation, DayYield, DFC) %>%
    filter(DFC <= D) %>%
    mutate(!!paste0('AvgMY', D) := mean(DayYield, na.rm = TRUE),
           MinDFC = min(DFC),
           MaxDFC = max(DFC),
           !!paste0('SumMY', D) := sum(DayYield, na.rm = TRUE))
}

# Define seasons
CalvingSeason <- function(DATES){
  WS <- as.Date("2012-1-1", format="%Y-%m-%d")
  SE <- as.Date("2012-4-1", format="%Y-%m-%d")
  SS <- as.Date("2012-7-1", format="%Y-%m-%d")
  FE <- as.Date("2012-10-1", format="%Y-%m-%d")

  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  ifelse(d >= WS & d < SE, "Winter",
         ifelse(d >= SE & d < SS, "Spring",
                ifelse(d >= SS & d < FE, "Summer", "Fall")))
}

# Exercise for computational statistics
res <- data.frame()
for(i in 101:110){
  res[i] <- i*2
}
res

x <- 101:110
res <- rep(NA, length(x))
res
for(i in 1:length(x)){
  res[i] <- x[i]*2               #to specify the position of x
}
res

x <- 101:110
res <- rep(NA, length(x))
res
for(i in 1:length(x)){
  x <- 101:110
  res <- rep(NA, length(x))
  res[i] <- x[i]*2               #to specify the position of x
}
res

# Simple example function
posfun <- function(number){  #number can be values or vectors
  number[number < 0] <- -1*number[number <0]
  number
}
posfun(-10:10)
