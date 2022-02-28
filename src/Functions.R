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
