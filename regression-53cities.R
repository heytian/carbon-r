# reference: https://towardsdatascience.com/7-steps-to-run-a-linear-regression-analysis-using-r-d5897a66b835/

# install.packages("tidyverse")
library(tidyverse)


df <- read.csv("c40cities-co2-em-conc-wkt-coast.csv") # outcome of https://github.com/heytian/d2d-oco3-tools/blob/main/20260405_04_wkt-coastal.py
df <- df |> drop_na(population)

# normalize independent variables using interquartile range 
iqr_norm <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  (x - q1) / (q3 - q1)
}


norm_df <- df |> 
  mutate(
    co2_conc_iqr = iqr_norm(xco2_ppm),
    co2_em_iqr = iqr_norm(annual_co2_em_mtco2),
    sif_iqr = iqr_norm(sif_757nm),
    pop_iqr = iqr_norm(population),
    xco2numsams_iqr = iqr_norm(xco2_numsams),
    sifnumsams_iqr = iqr_norm(sif_numsams),
    gdp_iqr = iqr_norm(gdp_bil_usd),
    coast_iqr = iqr_norm(coastal_km),
    impact_iqr = co2_conc_iqr - co2_em_iqr
  )
    

lm_iqr_impact<- lm(impact_iqr ~ sif_iqr + pop_iqr + xco2numsams_iqr + sifnumsams_iqr + gdp_iqr + coast_iqr, 
                  data = norm_df)
summary(lm_iqr_impact)





  
