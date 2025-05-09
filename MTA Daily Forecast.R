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
CRZ_data_entries |> gg_season(period = "1w")
CRZ_data_subway |> gg_season(period = "1w")

subwayDailyts |>
  autoplot() + 
  labs(x = "Date", y = "Ridership")

subwayDailyts |> 
  model(
    STL(Count ~ trend(window = 99) + season(window = 13),
        robust = FALSE)) |> components() -> subwayDailytsSTLModel

ggplot(subwayDailytsSTLModel, aes(x = Date, y = Count)) +
  geom_line(aes(y = Count), color = "grey") + 
  geom_line(aes(y = trend), color = "red") +
  labs(x = "Date",y = "Ridership")

subwayDailyts |> 
  model(
    STL(Count ~ trend(window = 99) + season(window = 13),
        robust = FALSE)) |> components() |>
  autoplot()

subwayDailyts |> 
  model(
    STL(Count ~ trend(window = 99) + season(window = 13),
        robust = FALSE)) |> components() |>
  filter_index("2024" ~ .) |>
  autoplot(season_week)

subwayDailyts |> 
  model(
    STL(Count ~ trend(window = 99) + season(window = 13),
        robust = FALSE)) |> components() |>
  #filter_index("2024" ~ .) |>
  autoplot(season_year)

subwayDailyts |>
  gg_season()

subwayDailyts |>
  gg_season(period = "1week")

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

subway_fit |> forecast(h = 10) |> autoplot(filter_index(subwayDailyts, "2024-03-01" ~ .))

subway_sim <- subway_fit |> generate (h = 60, times = 2, bootstrap = TRUE)

subwayDailyts |> 
  filter_index("2024-03-01" ~ .) |>
  ggplot(aes(x = Date)) +
  geom_line(aes(y = Count)) + 
  geom_line(aes(y = .sim, color = as.factor(.rep)), data = subway_sim)

subway_fc |> autoplot(subwayDailyts) + ylim(0,1e+07) 

subway_dcmp <- subwayDailyts |>
  model(STL(Count ~ trend(window = 100), robust = TRUE)) |>
  components() |>
  select(-.model)

subway_dcmp |>
  model(SNAIVE(season_adjust)) |>
  forecast() |>
  autoplot(filter_index(subway_dcmp,"2024" ~ .))

subway_fit_dcmp <- subwayDailyts |>
  model(stlf = decomposition_model(
    STL(Count ~ trend(window = 100), robust = TRUE),
    NAIVE(season_adjust)
  ))

subway_fit_dcmp |>
  forecast() |>
  autoplot(filter_index(subwayDailyts, "2024" ~ .))

subway_fit_dcmp |> gg_tsresiduals()
