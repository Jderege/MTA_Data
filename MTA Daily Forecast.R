library(fpp3)
library(tidyverse)
library(lubridate)

MTAdaily <- readr::read_csv("MTA_Daily_Ridership_and_Traffic__Beginning_2020_20250430.csv")

MTAdaily |>
  mutate(Date = mdy(Date))|>
  filter(Date < mdy("04/28/2025"))|>
  as_tsibble(key = Mode, index = Date) -> MTADailyts
MTADailyts |> distinct(Mode)
MTADailyts |> filter(Mode == "Subway") -> subwayDailyts
MTADailyts |> filter(Mode == "CRZ Entries") -> CRZ_data_entries
MTADailyts |> filter(Mode == "Subway") |> filter(year(Date) > 2023) -> CRZ_data_subway

CRZ_data_subway |> gg_season()
CRZ_data_entries |> autoplot()

subwayDailyts |> 
  model(
    STL(Count ~ trend(window = 100) + season(window = 13),
        robust = FALSE)) |> components() |>
  autoplot()

subway_fit <- subwayDailyts |> 
  model(trend_model = TSLM(Count ~ trend()))

subway_fit |> forecast(h = "2m") |>
  autoplot(filter(subwayDailyts,year(Date) > 2023))

STLsubwayDaily <- subwayDailyts |>
  model(stl = STL(Count))

STLsubwayDailyts <- STLsubwayDaily |> components() |> as_tsibble()


subway_fit |> forecast(h = "2m") |>
  autoplot() + 
  geom_line(aes(x = Date,y = Count),subwayDailyts,color = "grey") +
  geom_line(aes(y = trend),STLsubwayDailyts,color = "red")

subway_train <- subwayDailyts |> 
  filter_index("2020" ~ "2024")
subway_fit_simple <- subway_train |>
  model(
    Mean = MEAN(Count),
    Naive = NAIVE(Count),
    Seasonal_Naive = SNAIVE(Count)
  )

subway_fc <- subway_fit_simple |> forecast(h = "5m")

subway_fc |> autoplot(filter_index(subway_train, "2024" ~ .), level = NULL) +
  autolayer(
    filter_index(subwayDailyts, "2025" ~ .), color = "grey")


augment(subway_fit) |>
  autoplot(.innov)
subway_fit |> augment() |> features(.innov, ljung_box, lag = 365)
