# Cumulative milk yield, mean milk yield
cumulativeMY <- function(D){
  df.merge %>%
    group_by(FarmNo, CowId, CalvingDate) %>%
    select(Lactation, DayYield, DFC) %>%
    filter(DFC <= D) %>%
    mutate(!!paste0('AvgMY', D) := mean(DayYield),
           MinDFC = min(DFC),
           MaxDFC = max(DFC),
           !!paste0('SumMY', D) := sum(DayYield))
}

# Define seasons
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
