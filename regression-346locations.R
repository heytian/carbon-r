library(arrow)
library(dplyr)
library(lubridate)
library(tidyr)
locations <- read.csv("clasp_report_356sams-pop-gdp-coast.csv")

# replace parquet paths with local paths
oco3co2 <- read_parquet("/Users/heyutian/Downloads/oco3_parquet/co2_sam.parquet")
oco3sif <- read_parquet("/Users/heyutian/Downloads/oco3_parquet/sif_sam.parquet")
oco3co2sif <- bind_rows(oco3co2, oco3sif)

# # diagnostic: check for overlaps between target names in the 2 files
# target_name_csv <- unique(locations$Target.Name)
# target_name_parquet <- unique(oco3co2sif$target_name)
# missing_in_csv <- setdiff(target_name_parquet, target_name_csv)
# missing_in_parquet <- setdiff(target_name_csv, target_name_parquet)
# cat("Parquet entries NOT in CSV:\n")
# print(missing_in_csv)
# cat("\nCSV entries NOT in parquet:\n")
# print(missing_in_parquet)

# # check column names
# colnames(locations)
# colnames(oco3co2sif)

locations_select <- locations |>
  select(
    target_name = "Target.Name",  
    wkt_center="Site.Center.WKT",
    wkt_shape="Site.Shape.WKT",
    city,
    country,
    population,
    gdp_bil_usd,
    coastal_km,
    is_coastal
  )

oco3_full <- oco3co2sif |>
  select(-any_of(c("city","country","population"))) |>
  left_join(locations_select, by = "target_name")

# print(colnames(oco3_full))
# print(sum(!is.na(oco3_full$country)))

oco3_full <- oco3_full |>
  mutate(
    datetime = as.POSIXct(datetime),
    year = year(datetime),
    month = month(datetime),
    day_of_year = yday(datetime),
    season = case_when(
      latitude >= -20 & latitude <= 20 & month >= 5 & month <= 10 ~ "Tropical Wet",
      latitude >= -20 & latitude <= 20 ~ "Tropical Dry",
      
      latitude > 20 & latitude <= 60 & month %in% c(12, 1, 2) ~ "Winter (North)",
      latitude > 20 & latitude <= 60 & month %in% c(3, 4, 5) ~ "Spring (North)",
      latitude > 20 & latitude <= 60 & month %in% c(6, 7, 8) ~ "Summer (North)",
      latitude > 20 & latitude <= 60 & month %in% c(9, 10, 11) ~ "Autumn (North)",
      
      latitude >= -60 & latitude < -20 & month %in% c(12, 1, 2) ~ "Summer (South)",
      latitude >= -60 & latitude < -20 & month %in% c(3, 4, 5) ~ "Autumn (South)",
      latitude >= -60 & latitude < -20 & month %in% c(6, 7, 8) ~ "Winter (South)",
      latitude >= -60 & latitude < -20 & month %in% c(9, 10, 11) ~ "Spring (South)",
      
      TRUE ~ "Other"
    ),
    hour = hour(local_time),
    time_of_day = case_when(
      hour < 6  ~ "Night",
      hour < 12 ~ "Morning",
      hour < 18 ~ "Afternoon",
      TRUE ~ "Evening"
    )
  )

iqr_norm <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  (x - q1) / (q3 - q1)
}

regression_df <- oco3_full |>
  group_by(target_name, country, population, gdp_bil_usd, coastal_km, season, time_of_day) |>
  summarise(
    city = first(city),
    country = first(country),
    population = first(population),
    gdp_bil_usd = first(gdp_bil_usd),
    coastal_km = first(coastal_km),
    xco2_median = median(xco2, na.rm = TRUE),
    xco2_std = sd(xco2, na.rm = TRUE),
    xco2_n = n(),
    sif_median = median(Daily_SIF_757nm, na.rm = TRUE),
    sif_std = sd(Daily_SIF_757nm, na.rm = TRUE),
    sif_n = n(),
    .groups = "drop"
  ) |>
  filter(!is.na(population)) |>
  mutate(
    xco2_iqr = iqr_norm(xco2_median),
    sif_iqr = iqr_norm(sif_median),
    pop_iqr = iqr_norm(population),
    xco2_numsams_iqr = iqr_norm(xco2_n),
    sif_numsams_iqr = iqr_norm(sif_n),
    gdp_iqr = iqr_norm(gdp_bil_usd),
    coast_iqr = iqr_norm(coastal_km),
    season = as.factor(season),
    time_of_day = as.factor(time_of_day)
  ) 

# model interactions
model_interactions <- lm(xco2_iqr ~ (sif_iqr + pop_iqr + xco2_numsams_iqr + gdp_iqr + coast_iqr) * season, 
                         data = regression_df)
summary(model_interactions)

# run models by seasons
seasons_list <- unique(regression_df$season)

# use xco2 iqr
for (s in seasons_list) {
  cat("\n=== Season:", s, "===\n")

  model_s <- lm(xco2_iqr ~ sif_iqr + pop_iqr + xco2_numsams_iqr + gdp_iqr + coast_iqr,
                data = regression_df %>% filter(season == s))

  print(summary(model_s))
}

# use xco2 median
for (s in seasons_list) {
  cat("\n=== Season:", s, "===\n")
  
  model_seasons_median <- lm(xco2_iqr ~ sif_iqr + pop_iqr + xco2_numsams_iqr + gdp_iqr + coast_iqr,
                data = regression_df |> filter(season == s))
  
  print(summary(model_seasons_median))
}

# # check for median xco2 for summer north - strongest case 
# summary(lm(xco2_median ~ sif_median + pop_iqr + gdp_iqr + coast_iqr,
#            data = regression_df %>% filter(season == "Summer (North)")))
