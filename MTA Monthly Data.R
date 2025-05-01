library(fpp3)
library(tidyverse)


MTA <- readr::read_csv("/Users/jderege/Documents/2025/Spring 2025/MTH 260/Project/CSVs/MTA_Monthly_Ridership___Traffic_Data__Beginning_January_2008.csv")
MTAts <- MTA |> 
  mutate(Month = yearmonth(Month)) |> 
  as_tsibble(key = Agency, index = Month)
#MTAts |> filter(Agency == "Subway") |>   select(Month,Ridership) -> subway
MTAts |> filter(Agency == "Subway") ->subway
subway |> autoplot(Ridership) + labs(y = "Ridership")
subway |> ggplot(aes(x = Month, y = Ridership)) + geom_point()
subway |> ggplot(aes(x = Month, y = Ridership)) + geom_line()
subway |> gg_season(Ridership)
subway |> gg_subseries(Ridership)
#subway |> filter(Ridership != 98729329)
print(subway, n = 100)

subway |> gg_lag(Ridership,lags = 1:81,period = "1y", geom = "point")
subway |> ACF(Ridership,lag_max = 85)|>
  autoplot()
