library(arrow)
library(dplyr)
library(lubridate)
library(tidyr)

locations <- read.csv("https://code.earthengine.google.com/?asset=projects/carbon-cities-493002/assets/tropomi-356wkt-monthly") # from earthengine 

# Process TROPOMI NO2 data
locations_select <- locations |>
  select(
    target_name = "Target.Name",  
    wkt_center=".geo",
    wkt_shape="Site.Shape.WKT",
    latitude = "lat",
    longitude = "lon",
    city,
    country,
    population,
    gdp_bil_usd,
    coastal_km,
    is_coastal,
    no2 = "mean",
    date
  )|>
  mutate(
    date_parsed = parse_date_time(date, orders = c("ymd", "mdy", "dmy", "ym", "my")),
    year = year(date_parsed),
    month = month(date_parsed)
  )|>
  group_by(target_name, year, month) |>
  summarise(
    city        = first(city),
    country     = first(country),
    population  = first(population),
    gdp_bil_usd = first(gdp_bil_usd),
    coastal_km  = first(coastal_km),
    is_coastal  = first(is_coastal),
    no2_median  = median(no2, na.rm = TRUE),
    .groups     = "drop"
  )

# Process CO2 (xco2 only)
co2_monthly <- read_parquet("LOCAL FILE to oco3_parquet/co2_sam.parquet") |>
  mutate(
    datetime = as.POSIXct(datetime),
    year     = year(datetime),
    month    = month(datetime)
  ) |>
  group_by(target_name, year, month) |>
  summarise(
    latitude    = first(latitude),
    xco2_median = median(xco2, na.rm = TRUE),
    xco2_n      = sum(!is.na(xco2)),
    .groups     = "drop"
  )

# Process SIF (Daily_SIF_757nm only)
sif_monthly <- read_parquet("LOCAL FILE to sif_sam.parquet") |>
  mutate(
    datetime = as.POSIXct(datetime),
    year     = year(datetime),
    month    = month(datetime)
  ) |>
  group_by(target_name, year, month) |>
  summarise(
    sif_median  = median(Daily_SIF_757nm, na.rm = TRUE),
    sif_n       = sum(!is.na(Daily_SIF_757nm)),
    .groups     = "drop"
  )

# Merge CO2 + SIF (both have target_name, year, month)
oco3_monthly <- co2_monthly |>
  left_join(sif_monthly, by = c("target_name", "year", "month"))

# Merge with TROPOMI + locations
oco3_full <- oco3_monthly |>
  left_join(locations_select, by = c("target_name", "year", "month")) |>
  filter(!is.na(population))

# Add season
oco3_full <- oco3_full |>
  mutate(
    season = case_when(
      latitude >= -20 & latitude <= 20 & month %in% 5:10      ~ "Tropical Wet",
      latitude >= -20 & latitude <= 20                         ~ "Tropical Dry",
      latitude >  20  & latitude <= 60 & month %in% c(12,1,2) ~ "Winter (North)",
      latitude >  20  & latitude <= 60 & month %in% 3:5       ~ "Spring (North)",
      latitude >  20  & latitude <= 60 & month %in% 6:8       ~ "Summer (North)",
      latitude >  20  & latitude <= 60 & month %in% 9:11      ~ "Autumn (North)",
      latitude >= -60 & latitude < -20 & month %in% c(12,1,2) ~ "Summer (South)",
      latitude >= -60 & latitude < -20 & month %in% 3:5       ~ "Autumn (South)",
      latitude >= -60 & latitude < -20 & month %in% 6:8       ~ "Winter (South)",
      latitude >= -60 & latitude < -20 & month %in% 9:11      ~ "Spring (South)",
      TRUE ~ "Other"
    )
  )

# IQR normalization function
iqr_norm <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  (x - q1) / (q3 - q1)
}

# Final aggregation by season
regression_df <- oco3_full |>
  group_by(target_name, season) |>
  summarise(
    city            = first(city),
    country         = first(country),
    population      = first(population),
    gdp_bil_usd     = first(gdp_bil_usd),
    coastal_km      = first(coastal_km),
    is_coastal      = first(is_coastal),
    xco2_median     = median(xco2_median, na.rm = TRUE),
    xco2_std        = sd(xco2_median, na.rm = TRUE),
    sif_median      = median(sif_median,  na.rm = TRUE),
    sif_std         = sd(sif_median,  na.rm = TRUE),
    no2_median      = median(no2_median, na.rm = TRUE),
    no2_std         = sd(no2_median, na.rm = TRUE),
    .groups         = "drop"
  ) |>
  filter(!is.na(population)) |>
  mutate(
    xco2_iqr = iqr_norm(xco2_median),
    sif_iqr = iqr_norm(sif_median),
    pop_iqr = iqr_norm(population),
    gdp_iqr = iqr_norm(gdp_bil_usd),
    coast_iqr = iqr_norm(coastal_km),
    no2_iqr = iqr_norm(no2_median), 
    season = as.factor(season)
  )

# Run regressions by season
seasons_list <- unique(regression_df$season)

for (s in seasons_list) {
  cat("\n=== Season:", s, "===\n")
  
  model_s_m <- lm(xco2_median ~ sif_median + pop_iqr + gdp_iqr + coast_iqr + no2_median,
                  data = regression_df |> filter(season == s))
  
  print(summary(model_s_m))
}
