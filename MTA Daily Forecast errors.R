library(fpp3)
library(tidyverse)
library(lubridate)

MTAdaily <- readr::read_csv("/Users/jderege/tmp/MTA_Daily_Ridership_and_Traffic__Beginning_2020_20250430.csv")

MTAdaily |>
  mutate(Date = mdy(Date))|>
  filter(Date < mdy("04/28/2025"))|>
  as_tsibble(key = Mode, index = Date) -> MTADailyts

subway_daily <- MTADailyts |> filter(Mode == "Subway") |> filter_index("2020-04-01" ~ .)

subway_train <- subway_daily |> filter_index(. ~ "2024")

subway_test <- subway_daily |> filter_index("2025")

subway_fit <- subway_train |> 
  model(
    Mean = MEAN(Count),
    Naive = NAIVE(Count),
    SNaive = SNAIVE(Count),
    Drift = RW(Count ~ drift()),
    trend_model = TSLM(Count ~ trend())
  )
subway_fc <- subway_fit|>
  forecast(h = "4m")

subway_fc |>
  autoplot() +
  geom_line(aes(x = Date, y = Count),subway_daily, color = "grey")

subway_fc |>
  autoplot() +
  geom_line(aes(x = Date, y = Count),filter_index(subway_daily, "2024" ~ .))

subway_train |>
  ggplot(aes(x = Date, y = Count)) +
  geom_line(aes(x = Date, y = Count),filter(subway_test),color = "grey") +
  geom_line(aes(x = Date, y = .mean),filter(subway_fc,.model == "SNaive"),color = "green") +
  geom_line(aes(x = Date, y = Count), filter_index(subway_train, "2024" ~ .)) + 
  geom_line(aes(x = Date, y = .mean),filter(subway_fc,.model == "Mean"),color = "red") +
  geom_line(aes(x = Date, y = .mean),filter(subway_fc,.model == "Naive"),color = "blue") +
  geom_line(aes(x = Date, y = .mean),filter(subway_fc,.model == "Drift"),color = "orange") +
  guides(color = guide_legend(title = "Forecasts"))

subway_train |>
  ggplot(aes(x = Date, y = Count)) +
  geom_line(aes(x = Date, y = Count),filter(subway_test),color = "grey") +
  geom_line(aes(x = Date, y = .mean),filter(subway_fc,.model == "trend_model"),color = "green") +
  geom_line(aes(x = Date, y = Count), filter_index(subway_train, "2024" ~ .)) 

subway_fc |>
  filter(.model == "trend_model") |>
  autoplot() +
  autolayer(subway_test, color = "grey") +
  geom_line(aes(x = Date, y = Count),filter_index(subway_train,"2024" ~ .))

accuracy(subway_fc, subway_daily)

subway_fc |>
  filter(.model == "trend_model") |>
  autoplot(bind_rows(subway_train,subway_test))

subway_fc |> 
  filter(.model == "trend_model", Date == "2025-02-01") |>
  accuracy(subway_daily, list(qs = quantile_score), probs = 0.90)

subway_fc |> 
  filter(.model == "trend_model", Date == "2025-02-01") |>
  accuracy(subway_daily, list(winkler = winkler_score), level = 80)

subway_fc |>
  accuracy(subway_daily, list(crps = CRPS))

subway_fc |>
  accuracy(subway_daily, list(skill = skill_score(CRPS)))

